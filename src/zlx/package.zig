const std = @import("std");
const manifest_mod = @import("manifest.zig");

pub const max_package_size = 16 * 1024 * 1024;

pub const Layout = enum {
    single_manifest,
    archive,
};

pub const ArchiveEntry = struct {
    path: []const u8,
    data: []const u8,
};

pub const archive_magic = "ZLX1";

pub const Package = struct {
    layout: Layout,
    manifest: manifest_mod.Manifest,
    raw: []u8,
    manifest_bytes: []const u8,
    entries: []ArchiveEntry,

    pub fn deinit(self: *Package, alloc: std.mem.Allocator) void {
        manifest_mod.free(alloc, self.manifest);
        alloc.free(self.entries);
        alloc.free(self.raw);
    }

    pub fn manifestSource(self: Package) []const u8 {
        return self.manifest_bytes;
    }

    pub fn findEntry(self: Package, path: []const u8) ?ArchiveEntry {
        for (self.entries) |e| {
            if (std.mem.eql(u8, e.path, path)) return e;
        }
        return null;
    }
};

pub const Error = manifest_mod.Error || error{
    UnreadablePackage,
    UnknownLayout,
    InvalidArchive,
};

pub fn detectLayout(bytes: []const u8) Error!Layout {
    if (bytes.len >= 4 and std.mem.eql(u8, bytes[0..4], archive_magic)) return .archive;
    if (bytes.len == 0) return error.UnknownLayout;
    var i: usize = 0;
    while (i < bytes.len and (bytes[i] == ' ' or bytes[i] == '\t' or bytes[i] == '\n' or bytes[i] == '\r')) : (i += 1) {}
    if (i >= bytes.len) return error.UnknownLayout;
    if (bytes[i] == '.') return .single_manifest;
    return error.UnknownLayout;
}

fn readU32(bytes: []const u8, off: *usize) Error!u32 {
    if (off.* + 4 > bytes.len) return error.InvalidArchive;
    const v = std.mem.readInt(u32, bytes[off.*..][0..4], .little);
    off.* += 4;
    return v;
}

fn readU64(bytes: []const u8, off: *usize) Error!u64 {
    if (off.* + 8 > bytes.len) return error.InvalidArchive;
    const v = std.mem.readInt(u64, bytes[off.*..][0..8], .little);
    off.* += 8;
    return v;
}

fn parseArchive(alloc: std.mem.Allocator, bytes: []const u8) Error![]ArchiveEntry {
    if (bytes.len < 8) return error.InvalidArchive;
    var off: usize = 4;
    const count = try readU32(bytes, &off);
    if (count > 1024) return error.InvalidArchive;
    var list = alloc.alloc(ArchiveEntry, count) catch return error.OutOfMemory;
    var i: usize = 0;
    while (i < count) : (i += 1) {
        const path_len = try readU32(bytes, &off);
        if (off + path_len > bytes.len) {
            alloc.free(list);
            return error.InvalidArchive;
        }
        const path = bytes[off..][0..path_len];
        off += path_len;
        const data_len = try readU64(bytes, &off);
        if (off + data_len > bytes.len) {
            alloc.free(list);
            return error.InvalidArchive;
        }
        const data = bytes[off..][0..data_len];
        off += data_len;
        list[i] = .{ .path = path, .data = data };
    }
    return list;
}

pub fn open(alloc: std.mem.Allocator, io: std.Io, path: []const u8) !Package {
    const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, alloc, .limited(max_package_size)) catch {
        return error.UnreadablePackage;
    };
    errdefer alloc.free(bytes);

    const layout = try detectLayout(bytes);
    var entries: []ArchiveEntry = &.{};
    var manifest_bytes: []const u8 = undefined;

    switch (layout) {
        .single_manifest => {
            manifest_bytes = bytes;
            entries = alloc.alloc(ArchiveEntry, 0) catch return error.OutOfMemory;
        },
        .archive => {
            entries = try parseArchive(alloc, bytes);
            errdefer alloc.free(entries);
            var found: ?[]const u8 = null;
            for (entries) |e| {
                if (std.mem.eql(u8, e.path, "manifest.zon")) {
                    found = e.data;
                    break;
                }
            }
            manifest_bytes = found orelse return error.InvalidArchive;
        },
    }
    errdefer alloc.free(entries);
    const parsed = try manifest_mod.parse(alloc, manifest_bytes);
    return .{ .layout = layout, .manifest = parsed, .raw = bytes, .manifest_bytes = manifest_bytes, .entries = entries };
}

pub fn layoutName(layout: Layout) []const u8 {
    return @tagName(layout);
}

pub fn writeArchive(alloc: std.mem.Allocator, entries: []const ArchiveEntry) ![]u8 {
    var total: usize = 4 + 4;
    for (entries) |e| total += 4 + e.path.len + 8 + e.data.len;
    var out = try alloc.alloc(u8, total);
    @memcpy(out[0..4], archive_magic);
    std.mem.writeInt(u32, out[4..8], @intCast(entries.len), .little);
    var off: usize = 8;
    for (entries) |e| {
        std.mem.writeInt(u32, out[off..][0..4], @intCast(e.path.len), .little);
        off += 4;
        @memcpy(out[off..][0..e.path.len], e.path);
        off += e.path.len;
        std.mem.writeInt(u64, out[off..][0..8], @intCast(e.data.len), .little);
        off += 8;
        @memcpy(out[off..][0..e.data.len], e.data);
        off += e.data.len;
    }
    return out;
}
