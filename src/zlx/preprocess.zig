const std = @import("std");
const abi = @import("abi.zig");
const host_mod = @import("host.zig");

pub const Error = error{
    UnterminatedBlock,
    HandlerFailed,
    OutOfMemory,
};

pub const SourceMapEntry = struct {
    generated_offset: usize,
    original_line: u32,
    original_column: u32,
};

pub const ExpandReport = struct {
    expansions: usize = 0,
    source_map: std.ArrayList(SourceMapEntry) = .empty,

    pub fn deinit(self: *ExpandReport, alloc: std.mem.Allocator) void {
        self.source_map.deinit(alloc);
    }
};

pub fn expandExtensionBlocks(
    alloc: std.mem.Allocator,
    host: *host_mod.Host,
    file_label: []const u8,
    input: []const u8,
) Error!struct { source: []u8, report: ExpandReport } {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(alloc);

    var report: ExpandReport = .{};

    if (host.syntax_blocks.items.len == 0) {
        try out.appendSlice(alloc, input);
        return .{ .source = try out.toOwnedSlice(alloc), .report = report };
    }

    var line: u32 = 1;
    var column: u32 = 1;
    var i: usize = 0;
    while (i < input.len) {
        const c = input[i];
        if (c == '?' and i + 1 < input.len and input[i + 1] == '?') {
            const nl = std.mem.indexOfScalarPos(u8, input, i, '\n') orelse input.len;
            try out.appendSlice(alloc, input[i..nl]);
            i = nl;
            column = 1;
            continue;
        }
        if (c == '"') {
            const start_str = i;
            var k = i + 1;
            while (k < input.len) : (k += 1) {
                if (input[k] == '\\' and k + 1 < input.len) {
                    k += 1;
                    continue;
                }
                if (input[k] == '"') {
                    k += 1;
                    break;
                }
            }
            try out.appendSlice(alloc, input[start_str..k]);
            advanceLineCol(input[start_str..k], &line, &column);
            i = k;
            continue;
        }
        if (isIdentStart(c)) {
            const start = i;
            var j = i + 1;
            while (j < input.len and isIdentCont(input[j])) : (j += 1) {}
            const ident = input[start..j];

            var matched_block: ?*const host_mod.SyntaxRegistration = null;
            for (host.syntax_blocks.items) |*block| {
                if (std.mem.eql(u8, block.name, ident)) {
                    matched_block = block;
                    break;
                }
            }

            if (matched_block) |block_ptr| brace_path: {
                if (!atStatementStart(input, start)) break :brace_path;
                var k = j;
                while (k < input.len and (input[k] == ' ' or input[k] == '\t' or input[k] == '\r' or input[k] == '\n')) : (k += 1) {}
                if (k < input.len and input[k] == '(') break :brace_path;
                if (k >= input.len or input[k] != '{') {
                    const stmt_start = j;
                    var stmt_end = stmt_start;
                    var nested_depth: usize = 0;
                    while (stmt_end < input.len) : (stmt_end += 1) {
                        const stmt_ch = input[stmt_end];
                        if (stmt_ch == ';' and nested_depth == 0) break;
                        if (stmt_ch == '}' and nested_depth == 0) break;
                        if (stmt_ch == '{' or stmt_ch == '(' or stmt_ch == '[') {
                            nested_depth += 1;
                        } else if ((stmt_ch == '}' or stmt_ch == ')' or stmt_ch == ']') and nested_depth != 0) {
                            nested_depth -= 1;
                        } else if (stmt_ch == '"') {
                            stmt_end += 1;
                            while (stmt_end < input.len) : (stmt_end += 1) {
                                if (input[stmt_end] == '\\' and stmt_end + 1 < input.len) {
                                    stmt_end += 1;
                                    continue;
                                }
                                if (input[stmt_end] == '"') break;
                            }
                        }
                    }
                    const raw = std.mem.trim(u8, input[stmt_start..stmt_end], " \t\r\n");
                    if (raw.len == 0) break :brace_path;

                    const file_z = try alloc.dupeZ(u8, file_label);
                    defer alloc.free(file_z);
                    const input_struct = abi.BlockInput{
                        .file = file_z.ptr,
                        .line = line,
                        .column = column,
                        .raw_source = raw.ptr,
                        .raw_source_len = @intCast(raw.len),
                    };
                    var output_struct: abi.BlockOutput = .{
                        .generated_zlang_source = @as([*]const u8, @ptrCast(&[_]u8{})),
                        .generated_zlang_source_len = 0,
                        .source_map = null,
                        .source_map_len = 0,
                    };

                    const handler = block_ptr.handler orelse break :brace_path;
                    const rc = handler(&host.api, &input_struct, &output_struct);
                    if (rc != 0) return error.HandlerFailed;

                    const generated_len: usize = @intCast(output_struct.generated_zlang_source_len);
                    if (generated_len != 0) {
                        const base_offset = out.items.len;
                        const gen = output_struct.generated_zlang_source[0..generated_len];
                        try out.appendSlice(alloc, gen);
                        try appendSourceMap(alloc, &report, base_offset, &output_struct, generated_len);
                    }

                    const consumed_end = if (stmt_end < input.len and input[stmt_end] == ';') stmt_end + 1 else stmt_end;
                    advanceLineCol(input[i..consumed_end], &line, &column);
                    i = consumed_end;
                    report.expansions += 1;
                    continue;
                }

                const block_start = k + 1;
                var depth: usize = 1;
                var end: usize = block_start;
                while (end < input.len) : (end += 1) {
                    const ch = input[end];
                    if (ch == '{') depth += 1;
                    if (ch == '}') {
                        depth -= 1;
                        if (depth == 0) break;
                    }
                }
                if (depth != 0) return error.UnterminatedBlock;

                const raw = input[block_start..end];

                const file_z = try alloc.dupeZ(u8, file_label);
                defer alloc.free(file_z);
                const input_struct = abi.BlockInput{
                    .file = file_z.ptr,
                    .line = line,
                    .column = column,
                    .raw_source = if (raw.len == 0) @as([*]const u8, @ptrCast(&[_]u8{})) else raw.ptr,
                    .raw_source_len = @intCast(raw.len),
                };
                var output_struct: abi.BlockOutput = .{
                    .generated_zlang_source = @as([*]const u8, @ptrCast(&[_]u8{})),
                    .generated_zlang_source_len = 0,
                    .source_map = null,
                    .source_map_len = 0,
                };

                const syntax = abi.BlockSyntax{
                    .mode = block_ptr.mode,
                    .terminator = if (block_ptr.terminator) |t| @ptrCast(t.ptr) else null,
                };
                _ = syntax;

                const handler = block_ptr.handler orelse break :brace_path;
                const rc = handler(&host.api, &input_struct, &output_struct);
                if (rc != 0) return error.HandlerFailed;

                const generated_len: usize = @intCast(output_struct.generated_zlang_source_len);
                if (generated_len != 0) {
                    const base_offset = out.items.len;
                    const gen = output_struct.generated_zlang_source[0..generated_len];
                    try out.appendSlice(alloc, gen);
                    try appendSourceMap(alloc, &report, base_offset, &output_struct, generated_len);
                }

                var consumed_end = end + 1;
                if (consumed_end < input.len and input[consumed_end] == ';') consumed_end += 1;
                advanceLineCol(input[i..consumed_end], &line, &column);
                i = consumed_end;
                report.expansions += 1;
                continue;
            }

            try out.appendSlice(alloc, ident);
            advanceLineCol(ident, &line, &column);
            i = j;
            continue;
        }

        try out.append(alloc, c);
        if (c == '\n') {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
        i += 1;
    }

    return .{ .source = try out.toOwnedSlice(alloc), .report = report };
}

fn appendSourceMap(
    alloc: std.mem.Allocator,
    report: *ExpandReport,
    base_offset: usize,
    output: *const abi.BlockOutput,
    generated_len: usize,
) !void {
    const entries = output.source_map orelse return;
    const len: usize = @intCast(output.source_map_len);
    for (entries[0..len]) |entry| {
        if (entry.generated_offset > generated_len) continue;
        try report.source_map.append(alloc, .{
            .generated_offset = base_offset + @as(usize, @intCast(entry.generated_offset)),
            .original_line = entry.original_line,
            .original_column = entry.original_column,
        });
    }
}

fn advanceLineCol(text: []const u8, line: *u32, column: *u32) void {
    for (text) |ch| {
        if (ch == '\n') {
            line.* += 1;
            column.* = 1;
        } else {
            column.* += 1;
        }
    }
}

fn isIdentStart(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}

fn isIdentCont(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
}

fn atStatementStart(input: []const u8, start: usize) bool {
    var p = start;
    while (p > 0) {
        p -= 1;
        switch (input[p]) {
            ' ', '\t', '\r' => continue,
            '\n', ';', '{' => return true,
            else => return false,
        }
    }
    return true;
}

const testing = std.testing;

fn testMarkerHandler(host: *abi.HostApi, input: *const abi.BlockInput, output: *abi.BlockOutput) callconv(.c) c_int {
    _ = host;
    _ = input;
    const marker: []const u8 = "MARK";
    output.* = .{
        .generated_zlang_source = marker.ptr,
        .generated_zlang_source_len = @intCast(marker.len),
        .source_map = null,
        .source_map_len = 0,
    };
    return 0;
}

fn expandForTest(alloc: std.mem.Allocator, host: *host_mod.Host, input: []const u8) ![]u8 {
    var res = try expandExtensionBlocks(alloc, host, "test.zl", input);
    res.report.deinit(alloc);
    return res.source;
}

fn hostWithBlock(host: *host_mod.Host) void {
    const syntax = abi.BlockSyntax{ .mode = .brace_counting, .terminator = null };
    _ = host.api.register_syntax_block(&host.api, "blk", &syntax, testMarkerHandler);
}

test "expand: brace and statement forms invoke the handler" {
    const alloc = testing.allocator;
    var host = host_mod.Host.init(alloc);
    defer host.deinit();
    hostWithBlock(&host);

    const brace = try expandForTest(alloc, &host, "blk { body }");
    defer alloc.free(brace);
    try testing.expect(std.mem.indexOf(u8, brace, "MARK") != null);

    const stmt = try expandForTest(alloc, &host, "blk foo;");
    defer alloc.free(stmt);
    try testing.expect(std.mem.indexOf(u8, stmt, "MARK") != null);
}

test "expand: function-call form is not hijacked by an extension keyword" {
    const alloc = testing.allocator;
    var host = host_mod.Host.init(alloc);
    defer host.deinit();
    hostWithBlock(&host);

    const call = try expandForTest(alloc, &host, "blk(x);");
    defer alloc.free(call);
    try testing.expect(std.mem.indexOf(u8, call, "MARK") == null);
    try testing.expect(std.mem.indexOf(u8, call, "blk(x);") != null);

    const spaced = try expandForTest(alloc, &host, "blk (x);");
    defer alloc.free(spaced);
    try testing.expect(std.mem.indexOf(u8, spaced, "MARK") == null);
}

test "expand: keywords inside strings and comments are left alone" {
    const alloc = testing.allocator;
    var host = host_mod.Host.init(alloc);
    defer host.deinit();
    hostWithBlock(&host);

    const in_str = try expandForTest(alloc, &host, "\"blk { x }\"");
    defer alloc.free(in_str);
    try testing.expect(std.mem.indexOf(u8, in_str, "MARK") == null);

    const in_comment = try expandForTest(alloc, &host, "?? blk { x }\n");
    defer alloc.free(in_comment);
    try testing.expect(std.mem.indexOf(u8, in_comment, "MARK") == null);
}

test "expand: input is unchanged when no blocks are registered" {
    const alloc = testing.allocator;
    var host = host_mod.Host.init(alloc);
    defer host.deinit();
    const src = "fun main() >> i32 { return 0; }";
    const out = try expandForTest(alloc, &host, src);
    defer alloc.free(out);
    try testing.expectEqualStrings(src, out);
}
