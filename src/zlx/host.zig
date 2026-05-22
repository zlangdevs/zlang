const std = @import("std");
const abi = @import("abi.zig");

pub const SyntaxRegistration = struct {
    name: []u8,
    mode: abi.DelimiterMode,
    terminator: ?[]u8,
    owner: ?[]u8,
    handler: ?abi.BlockHandler,
};

pub const ModuleRegistration = struct {
    name: []u8,
    path: []u8,
    owner: ?[]u8,
};

pub const CliFlagRegistration = struct {
    name: []u8,
    help: ?[]u8,
    mandatory: bool,
    owner: ?[]u8,
};

pub const HelpSection = struct {
    section_id: []u8,
    text: []u8,
    owner: ?[]u8,
};

pub const LinkFlagRegistration = struct {
    flag: []u8,
    owner: ?[]u8,
};

pub const Diagnostic = struct {
    level: abi.DiagnosticLevel,
    file: ?[]u8,
    line: u32,
    column: u32,
    message: []u8,
    hint: ?[]u8,
};

pub const Counts = struct {
    syntax_blocks: usize = 0,
    modules: usize = 0,
    cli_flags: usize = 0,
    link_flags: usize = 0,
    help_sections: usize = 0,
    duplicate_syntax_blocks: usize = 0,
    duplicate_modules: usize = 0,
    duplicate_cli_flags: usize = 0,
    duplicate_link_flags: usize = 0,
    duplicate_help_sections: usize = 0,
    diagnostics: usize = 0,
};

pub const Host = struct {
    api: abi.HostApi,
    alloc: std.mem.Allocator,
    syntax_blocks: std.ArrayList(SyntaxRegistration) = .empty,
    modules: std.ArrayList(ModuleRegistration) = .empty,
    cli_flags: std.ArrayList(CliFlagRegistration) = .empty,
    link_flags: std.ArrayList(LinkFlagRegistration) = .empty,
    help_sections: std.ArrayList(HelpSection) = .empty,
    diagnostics: std.ArrayList(Diagnostic) = .empty,
    counts: Counts = .{},
    current_owner: ?[]u8 = null,

    pub fn setCurrentOwner(self: *Host, name: []const u8) !void {
        self.clearCurrentOwner();
        self.current_owner = try self.alloc.dupe(u8, name);
    }

    pub fn clearCurrentOwner(self: *Host) void {
        if (self.current_owner) |o| self.alloc.free(o);
        self.current_owner = null;
    }

    fn dupeOwner(self: *Host) ?[]u8 {
        const owner = self.current_owner orelse return null;
        return self.alloc.dupe(u8, owner) catch null;
    }

    pub fn init(alloc: std.mem.Allocator) Host {
        return .{
            .api = .{
                .api_version = abi.api_version,
                .register_syntax_block = registerSyntaxBlock,
                .register_help_section = registerHelpSection,
                .register_cli_flag = registerCliFlag,
                .register_module = registerModule,
                .register_link_flag = registerLinkFlag,
                .diagnostic = recordDiagnostic,
                .resolve_type_size = resolveTypeSize,
            },
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *Host) void {
        for (self.syntax_blocks.items) |item| {
            self.alloc.free(item.name);
            if (item.terminator) |t| self.alloc.free(t);
            if (item.owner) |o| self.alloc.free(o);
        }
        self.syntax_blocks.deinit(self.alloc);
        for (self.modules.items) |item| {
            self.alloc.free(item.name);
            self.alloc.free(item.path);
            if (item.owner) |o| self.alloc.free(o);
        }
        self.modules.deinit(self.alloc);
        for (self.cli_flags.items) |item| {
            self.alloc.free(item.name);
            if (item.help) |h| self.alloc.free(h);
            if (item.owner) |o| self.alloc.free(o);
        }
        self.cli_flags.deinit(self.alloc);
        for (self.link_flags.items) |item| {
            self.alloc.free(item.flag);
            if (item.owner) |o| self.alloc.free(o);
        }
        self.link_flags.deinit(self.alloc);
        for (self.help_sections.items) |item| {
            self.alloc.free(item.section_id);
            self.alloc.free(item.text);
            if (item.owner) |o| self.alloc.free(o);
        }
        self.help_sections.deinit(self.alloc);
        for (self.diagnostics.items) |item| {
            if (item.file) |f| self.alloc.free(f);
            self.alloc.free(item.message);
            if (item.hint) |h| self.alloc.free(h);
        }
        self.diagnostics.deinit(self.alloc);
        self.clearCurrentOwner();
    }

    fn fromApi(api: *abi.HostApi) *Host {
        return @fieldParentPtr("api", api);
    }
};

fn dupeC(alloc: std.mem.Allocator, ptr: [*:0]const u8) ![]u8 {
    return alloc.dupe(u8, std.mem.span(ptr));
}

fn dupeCOpt(alloc: std.mem.Allocator, ptr: ?[*:0]const u8) !?[]u8 {
    return if (ptr) |p| try alloc.dupe(u8, std.mem.span(p)) else null;
}

fn registerSyntaxBlock(
    host_api: *abi.HostApi,
    name: [*:0]const u8,
    syntax: *const abi.BlockSyntax,
    handler: abi.BlockHandler,
) callconv(.c) c_int {
    const host = Host.fromApi(host_api);
    const name_slice = std.mem.span(name);
    for (host.syntax_blocks.items) |item| {
        if (std.mem.eql(u8, item.name, name_slice)) {
            host.counts.duplicate_syntax_blocks += 1;
            return @intFromEnum(abi.RegisterResult.duplicate);
        }
    }
    const name_owned = host.alloc.dupe(u8, name_slice) catch return @intFromEnum(abi.RegisterResult.invalid);
    const term_owned: ?[]u8 = if (syntax.terminator) |t| host.alloc.dupe(u8, std.mem.span(t)) catch return @intFromEnum(abi.RegisterResult.invalid) else null;
    host.syntax_blocks.append(host.alloc, .{
        .name = name_owned,
        .mode = syntax.mode,
        .terminator = term_owned,
        .owner = host.dupeOwner(),
        .handler = handler,
    }) catch return @intFromEnum(abi.RegisterResult.invalid);
    host.counts.syntax_blocks += 1;
    return @intFromEnum(abi.RegisterResult.ok);
}

fn registerHelpSection(host_api: *abi.HostApi, section_id: [*:0]const u8, text: [*:0]const u8) callconv(.c) c_int {
    const host = Host.fromApi(host_api);
    const id_slice = std.mem.span(section_id);
    for (host.help_sections.items) |item| {
        if (std.mem.eql(u8, item.section_id, id_slice)) {
            host.counts.duplicate_help_sections += 1;
            return @intFromEnum(abi.RegisterResult.duplicate);
        }
    }
    const id_owned = dupeC(host.alloc, section_id) catch return @intFromEnum(abi.RegisterResult.invalid);
    const text_owned = dupeC(host.alloc, text) catch return @intFromEnum(abi.RegisterResult.invalid);
    host.help_sections.append(host.alloc, .{ .section_id = id_owned, .text = text_owned, .owner = host.dupeOwner() }) catch return @intFromEnum(abi.RegisterResult.invalid);
    host.counts.help_sections += 1;
    return @intFromEnum(abi.RegisterResult.ok);
}

fn registerCliFlag(host_api: *abi.HostApi, flag_name: [*:0]const u8, help_text: ?[*:0]const u8, mandatory: c_int) callconv(.c) c_int {
    const host = Host.fromApi(host_api);
    const name_slice = std.mem.span(flag_name);
    for (host.cli_flags.items) |item| {
        if (std.mem.eql(u8, item.name, name_slice)) {
            host.counts.duplicate_cli_flags += 1;
            return @intFromEnum(abi.RegisterResult.duplicate);
        }
    }
    const name_owned = dupeC(host.alloc, flag_name) catch return @intFromEnum(abi.RegisterResult.invalid);
    const help_owned = dupeCOpt(host.alloc, help_text) catch return @intFromEnum(abi.RegisterResult.invalid);
    host.cli_flags.append(host.alloc, .{ .name = name_owned, .help = help_owned, .mandatory = mandatory != 0, .owner = host.dupeOwner() }) catch return @intFromEnum(abi.RegisterResult.invalid);
    host.counts.cli_flags += 1;
    return @intFromEnum(abi.RegisterResult.ok);
}

fn registerModule(host_api: *abi.HostApi, module_name: [*:0]const u8, package_relative_path: [*:0]const u8) callconv(.c) c_int {
    const host = Host.fromApi(host_api);
    const name_slice = std.mem.span(module_name);
    for (host.modules.items) |item| {
        if (std.mem.eql(u8, item.name, name_slice)) {
            host.counts.duplicate_modules += 1;
            return @intFromEnum(abi.RegisterResult.duplicate);
        }
    }
    const name_owned = dupeC(host.alloc, module_name) catch return @intFromEnum(abi.RegisterResult.invalid);
    const path_owned = dupeC(host.alloc, package_relative_path) catch return @intFromEnum(abi.RegisterResult.invalid);
    host.modules.append(host.alloc, .{ .name = name_owned, .path = path_owned, .owner = host.dupeOwner() }) catch return @intFromEnum(abi.RegisterResult.invalid);
    host.counts.modules += 1;
    return @intFromEnum(abi.RegisterResult.ok);
}

fn registerLinkFlag(host_api: *abi.HostApi, flag: [*:0]const u8) callconv(.c) c_int {
    const host = Host.fromApi(host_api);
    const slice = std.mem.span(flag);
    for (host.link_flags.items) |item| {
        if (std.mem.eql(u8, item.flag, slice)) {
            host.counts.duplicate_link_flags += 1;
            return @intFromEnum(abi.RegisterResult.duplicate);
        }
    }
    const owned = dupeC(host.alloc, flag) catch return @intFromEnum(abi.RegisterResult.invalid);
    host.link_flags.append(host.alloc, .{ .flag = owned, .owner = host.dupeOwner() }) catch return @intFromEnum(abi.RegisterResult.invalid);
    host.counts.link_flags += 1;
    return @intFromEnum(abi.RegisterResult.ok);
}

fn recordDiagnostic(
    host_api: *abi.HostApi,
    level: abi.DiagnosticLevel,
    file: ?[*:0]const u8,
    line: u32,
    column: u32,
    message: [*:0]const u8,
    hint: ?[*:0]const u8,
) callconv(.c) void {
    const host = Host.fromApi(host_api);
    const file_owned = dupeCOpt(host.alloc, file) catch return;
    const message_owned = dupeC(host.alloc, message) catch return;
    const hint_owned = dupeCOpt(host.alloc, hint) catch return;
    host.diagnostics.append(host.alloc, .{
        .level = level,
        .file = file_owned,
        .line = line,
        .column = column,
        .message = message_owned,
        .hint = hint_owned,
    }) catch return;
    host.counts.diagnostics += 1;
}

fn primitiveSize(t: []const u8) i32 {
    if (std.mem.eql(u8, t, "bool") or std.mem.eql(u8, t, "i8") or std.mem.eql(u8, t, "u8")) return 1;
    if (std.mem.eql(u8, t, "i16") or std.mem.eql(u8, t, "u16") or std.mem.eql(u8, t, "f16")) return 2;
    if (std.mem.eql(u8, t, "i32") or std.mem.eql(u8, t, "u32") or std.mem.eql(u8, t, "f32")) return 4;
    if (std.mem.eql(u8, t, "i64") or std.mem.eql(u8, t, "u64") or std.mem.eql(u8, t, "f64")) return 8;
    if (std.mem.startsWith(u8, t, "ptr<")) return 8;
    return -1;
}

fn typeAlign(size: i32) i32 {
    if (size >= 8) return 8;
    if (size >= 4) return 4;
    if (size >= 2) return 2;
    return 1;
}

fn alignTo(v: i32, a: i32) i32 {
    return @divTrunc(v + a - 1, a) * a;
}

fn splitTopComma(s: []const u8) ?usize {
    var depth: i32 = 0;
    for (s, 0..) |c, i| {
        if (c == '<' or c == '(' or c == '[') depth += 1
        else if (c == '>' or c == ')' or c == ']') { if (depth > 0) depth -= 1; }
        else if (c == ',' and depth == 0) return i;
    }
    return null;
}

fn computeArrSize(alloc: std.mem.Allocator, src: []const u8, type_str: []const u8) i32 {
    if (!std.mem.startsWith(u8, type_str, "arr<") or !std.mem.endsWith(u8, type_str, ">")) return -1;
    const inner = type_str[4 .. type_str.len - 1];
    const comma = splitTopComma(inner) orelse return -1;
    const elem = std.mem.trim(u8, inner[0..comma], " \t");
    const cnt_s = std.mem.trim(u8, inner[comma + 1 ..], " \t");
    const cnt = std.fmt.parseInt(i32, cnt_s, 10) catch return -1;
    if (cnt <= 0) return -1;
    const es = resolveSize(alloc, src, elem);
    if (es <= 0) return -1;
    return es * cnt;
}

fn computeStructSize(alloc: std.mem.Allocator, src: []const u8, name: []const u8) i32 {
    var needle_buf: [256]u8 = undefined;
    const needle = std.fmt.bufPrint(&needle_buf, "struct {s}", .{name}) catch return -1;
    var pos: usize = 0;
    const found_at = std.mem.indexOf(u8, src, needle) orelse return -1;
    pos = found_at + needle.len;
    if (pos < src.len) {
        const c = src[pos];
        if (std.ascii.isAlphanumeric(c) or c == '_') return -1;
    }
    const open_rel = std.mem.indexOfScalar(u8, src[pos..], '{') orelse return -1;
    const open = pos + open_rel;
    var depth: i32 = 0;
    var close: usize = open;
    while (close < src.len) : (close += 1) {
        if (src[close] == '{') depth += 1
        else if (src[close] == '}') { depth -= 1; if (depth == 0) break; }
    }
    if (close >= src.len) return -1;
    var body = alloc.dupe(u8, src[open + 1 .. close]) catch return -1;
    defer alloc.free(body);
    // strip ?? comments
    var i: usize = 0;
    while (i + 1 < body.len) : (i += 1) {
        if (body[i] == '?' and body[i + 1] == '?') {
            while (i < body.len and body[i] != '\n') : (i += 1) body[i] = ' ';
        }
    }
    var total: i32 = 0;
    var max_align: i32 = 1;
    var it = std.mem.tokenizeAny(u8, body, ",\n");
    while (it.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \t\r");
        if (line.len == 0) continue;
        var sp_idx: usize = 0;
        while (sp_idx < line.len and !std.ascii.isWhitespace(line[sp_idx])) : (sp_idx += 1) {}
        if (sp_idx >= line.len) continue;
        const field_type = std.mem.trim(u8, line[sp_idx..], " \t\r");
        const fs = resolveSize(alloc, src, field_type);
        if (fs <= 0) return -1;
        const a = typeAlign(fs);
        if (a > max_align) max_align = a;
        total = alignTo(total, a) + fs;
    }
    return alignTo(total, max_align);
}

fn resolveSize(alloc: std.mem.Allocator, src: []const u8, type_str_raw: []const u8) i32 {
    const t = std.mem.trim(u8, type_str_raw, " \t\r");
    const prim = primitiveSize(t);
    if (prim >= 0) return prim;
    if (std.mem.startsWith(u8, t, "arr<")) return computeArrSize(alloc, src, t);
    return computeStructSize(alloc, src, t);
}

const libc = struct {
    extern fn fopen(path: [*:0]const u8, mode: [*:0]const u8) ?*anyopaque;
    extern fn fclose(f: ?*anyopaque) c_int;
    extern fn fseek(f: ?*anyopaque, off: c_long, whence: c_int) c_int;
    extern fn ftell(f: ?*anyopaque) c_long;
    extern fn fread(ptr: [*]u8, size: usize, n: usize, f: ?*anyopaque) usize;
};

fn resolveTypeSize(host_api: *abi.HostApi, file: [*:0]const u8, type_name: [*:0]const u8) callconv(.c) i32 {
    const host = Host.fromApi(host_api);
    const type_s = std.mem.span(type_name);
    const f = libc.fopen(file, "rb") orelse return -1;
    defer _ = libc.fclose(f);
    if (libc.fseek(f, 0, 2) != 0) return -1;
    const n = libc.ftell(f);
    if (n <= 0 or n > 64 * 1024 * 1024) return -1;
    if (libc.fseek(f, 0, 0) != 0) return -1;
    const sz: usize = @intCast(n);
    const buf = host.alloc.alloc(u8, sz) catch return -1;
    defer host.alloc.free(buf);
    const got = libc.fread(buf.ptr, 1, sz, f);
    return resolveSize(host.alloc, buf[0..got], type_s);
}

const manifest_mod = @import("manifest.zig");

pub fn simulateFromManifest(host: *Host, m: manifest_mod.Manifest) !void {
    for (m.syntax_blocks) |block| {
        const name_z = try host.alloc.dupeZ(u8, block.name);
        defer host.alloc.free(name_z);
        const syntax = abi.BlockSyntax{
            .mode = switch (block.delimiter_mode) {
                .brace_counting => .brace_counting,
                .custom_terminator => .custom_terminator,
            },
            .terminator = null,
        };
        _ = host.api.register_syntax_block(&host.api, name_z.ptr, &syntax, noopHandler);
    }
    for (m.modules) |module| {
        const name_z = try host.alloc.dupeZ(u8, module.name);
        defer host.alloc.free(name_z);
        const path_z = try host.alloc.dupeZ(u8, module.path);
        defer host.alloc.free(path_z);
        _ = host.api.register_module(&host.api, name_z.ptr, path_z.ptr);
    }
    for (m.cli_flags) |flag| {
        const name_z = try host.alloc.dupeZ(u8, flag.name);
        defer host.alloc.free(name_z);
        const help_z: ?[:0]u8 = if (flag.value) |v| try host.alloc.dupeZ(u8, v) else null;
        defer if (help_z) |h| host.alloc.free(h);
        const help_ptr: ?[*:0]const u8 = if (help_z) |h| h.ptr else null;
        _ = host.api.register_cli_flag(&host.api, name_z.ptr, help_ptr, @intFromBool(flag.mandatory));
    }
    for (m.link_flags) |lf| {
        const flag_z = try host.alloc.dupeZ(u8, lf);
        defer host.alloc.free(flag_z);
        _ = host.api.register_link_flag(&host.api, flag_z.ptr);
    }
}

fn noopHandler(host: *abi.HostApi, input: *const abi.BlockInput, output: *abi.BlockOutput) callconv(.c) c_int {
    _ = host;
    _ = input;
    const empty: []const u8 = &[_]u8{};
    output.* = .{
        .generated_zlang_source = empty.ptr,
        .generated_zlang_source_len = 0,
        .source_map = null,
        .source_map_len = 0,
    };
    return 0;
}

const testHandler: abi.BlockHandler = noopHandler;

test "host records registrations and detects duplicates" {
    const alloc = std.testing.allocator;
    var host = Host.init(alloc);
    defer host.deinit();
    const syntax = abi.BlockSyntax{ .mode = .brace_counting, .terminator = null };
    const r1 = host.api.register_syntax_block(&host.api, "brainfuck", &syntax, testHandler);
    const r2 = host.api.register_syntax_block(&host.api, "brainfuck", &syntax, testHandler);
    try std.testing.expectEqual(@as(c_int, @intFromEnum(abi.RegisterResult.ok)), r1);
    try std.testing.expectEqual(@as(c_int, @intFromEnum(abi.RegisterResult.duplicate)), r2);
    try std.testing.expectEqual(@as(usize, 1), host.counts.syntax_blocks);
    try std.testing.expectEqual(@as(usize, 1), host.counts.duplicate_syntax_blocks);
}
