const std = @import("std");
const abi = @import("abi.zig");
const host_mod = @import("host.zig");

pub const Error = error{
    UnterminatedBlock,
    HandlerFailed,
    OutOfMemory,
};

pub const ExpandReport = struct {
    expansions: usize = 0,
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
                var k = j;
                while (k < input.len and (input[k] == ' ' or input[k] == '\t' or input[k] == '\r' or input[k] == '\n')) : (k += 1) {}
                if (k >= input.len or input[k] != '{') break :brace_path;

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
                    const gen = output_struct.generated_zlang_source[0..generated_len];
                    try out.appendSlice(alloc, gen);
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

