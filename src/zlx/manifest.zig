const std = @import("std");

pub const Manifest = struct {
    format_version: u32 = 1,
    name: []const u8,
    version: []const u8,
    api_min: u32 = 1,
    api_max: u32 = 1,
    provides: []const []const u8 = &.{},
    targets: []const []const u8 = &.{},
    dependencies: []const []const u8 = &.{},
    native_libs: []const []const u8 = &.{},
    link_flags: []const []const u8 = &.{},
    expose: []const []const u8 = &.{},
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
}

pub fn isValidName(name: []const u8) bool {
    if (name.len == 0) return false;
    for (name) |ch| {
        if (std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '-') continue;
        return false;
    }
    return true;
}
