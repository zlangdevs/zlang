const std = @import("std");
const builtin = @import("builtin");

pub const Manifest = struct {
    format_version: u32 = 1,
    name: []const u8,
    version: []const u8,
    api_min: u32 = 1,
    api_max: u32 = 1,
    entry: ?[]const u8 = null,
    provides: []const []const u8 = &.{},
    targets: []const []const u8 = &.{},
    dependencies: []const []const u8 = &.{},
    modules: []const Module = &.{},
    syntax_blocks: []const SyntaxBlock = &.{},
    cli_flags: []const CliFlag = &.{},
    native_libs: []const []const u8 = &.{},
    link_flags: []const []const u8 = &.{},
    expose: Expose = .{},
};

pub const Module = struct {
    name: []const u8,
    path: []const u8,
};

pub const Expose = struct {
    global_syntax: bool = false,
    global_modules: bool = false,
};

pub const SyntaxBlock = struct {
    name: []const u8,
    delimiter_mode: DelimiterMode = .brace_counting,
    mandatory: bool = true,
};

pub const DelimiterMode = enum {
    brace_counting,
    custom_terminator,
};

pub const CliFlag = struct {
    name: []const u8,
    value: ?[]const u8 = null,
    mandatory: bool = false,
};

pub const Error = error{
    InvalidName,
    UnsupportedFormat,
    UnsupportedApiRange,
    ParseZon,
    OutOfMemory,
};

pub fn parse(alloc: std.mem.Allocator, source: []const u8) Error!Manifest {
    const source_z = try alloc.dupeZ(u8, source);
    defer alloc.free(source_z);

    var diag: std.zon.parse.Diagnostics = .{};
    defer diag.deinit(alloc);

    const parsed = std.zon.parse.fromSliceAlloc(Manifest, alloc, source_z, &diag, .{}) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseZon => return error.ParseZon,
    };
    errdefer std.zon.parse.free(alloc, parsed);

    try validate(parsed);
    return parsed;
}

pub fn free(alloc: std.mem.Allocator, value: Manifest) void {
    std.zon.parse.free(alloc, value);
}

pub fn validate(manifest: Manifest) Error!void {
    if (manifest.format_version != 1) return error.UnsupportedFormat;
    if (manifest.api_min == 0 or manifest.api_max == 0 or manifest.api_min > manifest.api_max) return error.UnsupportedApiRange;
    if (!isValidName(manifest.name)) return error.InvalidName;
    for (manifest.modules) |module| {
        if (!isValidModuleName(module.name)) return error.InvalidName;
        if (module.path.len == 0) return error.InvalidName;
    }
    for (manifest.syntax_blocks) |block| {
        if (!isValidName(block.name)) return error.InvalidName;
    }
    for (manifest.cli_flags) |flag| {
        if (!isValidCliFlag(flag.name)) return error.InvalidName;
    }
}

pub fn supportsCurrentTarget(manifest: Manifest) bool {
    if (manifest.targets.len == 0) return true;
    const current = currentTargetName();
    for (manifest.targets) |target| {
        if (std.mem.eql(u8, target, current)) return true;
    }
    return false;
}

pub fn currentTargetName() []const u8 {
    return comptime @tagName(builtin.os.tag) ++ "-" ++ @tagName(builtin.cpu.arch);
}

pub fn isValidName(name: []const u8) bool {
    if (name.len == 0) return false;
    for (name) |ch| {
        if (std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '-') continue;
        return false;
    }
    return true;
}

fn isValidModuleName(name: []const u8) bool {
    if (name.len == 0) return false;
    for (name) |ch| {
        if (std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '-' or ch == '.') continue;
        return false;
    }
    return true;
}

fn isValidCliFlag(name: []const u8) bool {
    if (name.len < 2 or name[0] != '-') return false;
    for (name[1..]) |ch| {
        if (std.ascii.isAlphanumeric(ch) or ch == '-' or ch == '_') continue;
        return false;
    }
    return true;
}
