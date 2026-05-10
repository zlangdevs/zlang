const std = @import("std");
const abi = @import("abi.zig");

pub const SyntaxRegistration = struct {
    name: []u8,
    mode: abi.DelimiterMode,
    terminator: ?[]u8,
};

pub const ModuleRegistration = struct {
    name: []u8,
    path: []u8,
};

pub const CliFlagRegistration = struct {
    name: []u8,
    help: ?[]u8,
    mandatory: bool,
};

pub const HelpSection = struct {
    section_id: []u8,
    text: []u8,
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
    link_flags: std.ArrayList([]u8) = .empty,
    help_sections: std.ArrayList(HelpSection) = .empty,
    diagnostics: std.ArrayList(Diagnostic) = .empty,
    counts: Counts = .{},

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
            },
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *Host) void {
        for (self.syntax_blocks.items) |item| {
            self.alloc.free(item.name);
            if (item.terminator) |t| self.alloc.free(t);
        }
        self.syntax_blocks.deinit(self.alloc);
        for (self.modules.items) |item| {
            self.alloc.free(item.name);
            self.alloc.free(item.path);
        }
        self.modules.deinit(self.alloc);
        for (self.cli_flags.items) |item| {
            self.alloc.free(item.name);
            if (item.help) |h| self.alloc.free(h);
        }
        self.cli_flags.deinit(self.alloc);
        for (self.link_flags.items) |item| self.alloc.free(item);
        self.link_flags.deinit(self.alloc);
        for (self.help_sections.items) |item| {
            self.alloc.free(item.section_id);
            self.alloc.free(item.text);
        }
        self.help_sections.deinit(self.alloc);
        for (self.diagnostics.items) |item| {
            if (item.file) |f| self.alloc.free(f);
            self.alloc.free(item.message);
            if (item.hint) |h| self.alloc.free(h);
        }
        self.diagnostics.deinit(self.alloc);
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
    _ = handler;
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
    host.help_sections.append(host.alloc, .{ .section_id = id_owned, .text = text_owned }) catch return @intFromEnum(abi.RegisterResult.invalid);
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
    host.cli_flags.append(host.alloc, .{ .name = name_owned, .help = help_owned, .mandatory = mandatory != 0 }) catch return @intFromEnum(abi.RegisterResult.invalid);
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
    host.modules.append(host.alloc, .{ .name = name_owned, .path = path_owned }) catch return @intFromEnum(abi.RegisterResult.invalid);
    host.counts.modules += 1;
    return @intFromEnum(abi.RegisterResult.ok);
}

fn registerLinkFlag(host_api: *abi.HostApi, flag: [*:0]const u8) callconv(.c) c_int {
    const host = Host.fromApi(host_api);
    const slice = std.mem.span(flag);
    for (host.link_flags.items) |item| {
        if (std.mem.eql(u8, item, slice)) {
            host.counts.duplicate_link_flags += 1;
            return @intFromEnum(abi.RegisterResult.duplicate);
        }
    }
    const owned = dupeC(host.alloc, flag) catch return @intFromEnum(abi.RegisterResult.invalid);
    host.link_flags.append(host.alloc, owned) catch return @intFromEnum(abi.RegisterResult.invalid);
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
    output.* = .{ .generated_zlang_source = "", .generated_zlang_source_len = 0 };
    return 0;
}

test "host records registrations and detects duplicates" {
    const alloc = std.testing.allocator;
    var host = Host.init(alloc);
    defer host.deinit();
    const syntax = abi.BlockSyntax{ .mode = .brace_counting, .terminator = null };
    const r1 = host.api.register_syntax_block(&host.api, "brainfuck", &syntax, noopHandler);
    const r2 = host.api.register_syntax_block(&host.api, "brainfuck", &syntax, noopHandler);
    try std.testing.expectEqual(@as(c_int, @intFromEnum(abi.RegisterResult.ok)), r1);
    try std.testing.expectEqual(@as(c_int, @intFromEnum(abi.RegisterResult.duplicate)), r2);
    try std.testing.expectEqual(@as(usize, 1), host.counts.syntax_blocks);
    try std.testing.expectEqual(@as(usize, 1), host.counts.duplicate_syntax_blocks);
}
