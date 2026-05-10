const std = @import("std");
const manifest_mod = @import("manifest.zig");

pub const max_package_size = 16 * 1024 * 1024;

pub const Layout = enum {
    single_manifest,
};

pub const Package = struct {
    layout: Layout,
    manifest: manifest_mod.Manifest,
    raw: []u8,

    pub fn deinit(self: *Package, alloc: std.mem.Allocator) void {
        manifest_mod.free(alloc, self.manifest);
        alloc.free(self.raw);
    }

    pub fn manifestSource(self: Package) []const u8 {
        return switch (self.layout) {
            .single_manifest => self.raw,
        };
    }
};

pub const Error = manifest_mod.Error || error{
    UnreadablePackage,
    UnknownLayout,
};

pub fn detectLayout(bytes: []const u8) Error!Layout {
    if (bytes.len == 0) return error.UnknownLayout;
    var i: usize = 0;
    while (i < bytes.len and (bytes[i] == ' ' or bytes[i] == '\t' or bytes[i] == '\n' or bytes[i] == '\r')) : (i += 1) {}
    if (i >= bytes.len) return error.UnknownLayout;
    if (bytes[i] == '.') return .single_manifest;
    return error.UnknownLayout;
}

pub fn open(alloc: std.mem.Allocator, io: std.Io, path: []const u8) !Package {
    const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, alloc, .limited(max_package_size)) catch {
        return error.UnreadablePackage;
    };
    errdefer alloc.free(bytes);

    const layout = try detectLayout(bytes);
    const manifest_bytes = switch (layout) {
        .single_manifest => bytes,
    };
    const parsed = try manifest_mod.parse(alloc, manifest_bytes);
    return .{ .layout = layout, .manifest = parsed, .raw = bytes };
}

pub fn layoutName(layout: Layout) []const u8 {
    return @tagName(layout);
}
