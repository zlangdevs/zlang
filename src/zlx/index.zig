const std = @import("std");
const manifest_mod = @import("manifest.zig");
const store_mod = @import("store.zig");

const max_index_size = 1024 * 1024;
const max_package_size = 16 * 1024 * 1024;

pub const Status = enum {
    installed,
    missing,
    broken,
};

pub const Entry = struct {
    name: []const u8,
    version: []const u8,
    path: []const u8,
    api_min: u32,
    api_max: u32,
    status: Status = .installed,
};

pub const Index = struct {
    format_version: u32 = 1,
    modules: []const Entry = &.{},
};

pub const Loaded = struct {
    value: Index,
    parsed: bool,

    pub fn deinit(self: Loaded, alloc: std.mem.Allocator) void {
        if (self.parsed) std.zon.parse.free(alloc, self.value);
    }
};

pub fn load(alloc: std.mem.Allocator, io: std.Io, store: store_mod.Store) !Loaded {
    const path = try store.indexPath(alloc);
    defer alloc.free(path);

    const source = std.Io.Dir.cwd().readFileAlloc(io, path, alloc, .limited(max_index_size)) catch |err| switch (err) {
        error.FileNotFound => return .{ .value = .{}, .parsed = false },
        else => return err,
    };
    defer alloc.free(source);

    const source_z = try alloc.dupeZ(u8, source);
    defer alloc.free(source_z);

    var diag: std.zon.parse.Diagnostics = .{};
    defer diag.deinit(alloc);
    const parsed = try std.zon.parse.fromSliceAlloc(Index, alloc, source_z, &diag, .{});
    return .{ .value = parsed, .parsed = true };
}

pub fn write(alloc: std.mem.Allocator, io: std.Io, store: store_mod.Store, entries: []const Entry) !void {
    try store.ensure(io);
    const path = try store.indexPath(alloc);
    defer alloc.free(path);

    var file = try std.Io.Dir.cwd().createFile(io, path, .{ .truncate = true });
    defer file.close(io);
    var buffer: [4096]u8 = undefined;
    var writer = file.writer(io, &buffer);

    try writer.interface.writeAll(".{\n    .format_version = 1,\n    .modules = .{\n");
    for (entries) |entry| {
        try writer.interface.writeAll("        .{ .name = ");
        try writeString(&writer.interface, entry.name);
        try writer.interface.writeAll(", .version = ");
        try writeString(&writer.interface, entry.version);
        try writer.interface.writeAll(", .path = ");
        try writeString(&writer.interface, entry.path);
        try writer.interface.print(", .api_min = {d}, .api_max = {d}, .status = .{s} }},\n", .{ entry.api_min, entry.api_max, @tagName(entry.status) });
    }
    try writer.interface.writeAll("    },\n}\n");
    try writer.interface.flush();
}

pub fn rebuild(alloc: std.mem.Allocator, io: std.Io, store: store_mod.Store) !std.ArrayList(Entry) {
    var entries: std.ArrayList(Entry) = .empty;
    errdefer deinitEntries(&entries, alloc);

    var dir = std.Io.Dir.openDirAbsolute(io, store.root, .{ .iterate = true }) catch |err| switch (err) {
        error.FileNotFound => return entries,
        else => return err,
    };
    defer dir.close(io);

    var it = dir.iterate();
    while (try it.next(io)) |dir_entry| {
        if (dir_entry.kind != .file or !std.mem.endsWith(u8, dir_entry.name, ".zlx")) continue;
        const package_path = try std.fmt.allocPrint(alloc, "{s}/{s}", .{ store.root, dir_entry.name });
        errdefer alloc.free(package_path);
        const package = std.Io.Dir.cwd().readFileAlloc(io, package_path, alloc, .limited(max_package_size)) catch {
            alloc.free(package_path);
            continue;
        };
        defer alloc.free(package);
        const parsed = manifest_mod.parse(alloc, package) catch {
            alloc.free(package_path);
            continue;
        };
        defer manifest_mod.free(alloc, parsed);
        try entries.append(alloc, .{
            .name = try alloc.dupe(u8, parsed.name),
            .version = try alloc.dupe(u8, parsed.version),
            .path = package_path,
            .api_min = parsed.api_min,
            .api_max = parsed.api_max,
            .status = .installed,
        });
    }

    try write(alloc, io, store, entries.items);
    return entries;
}

pub fn deinitEntries(entries: *std.ArrayList(Entry), alloc: std.mem.Allocator) void {
    for (entries.items) |entry| {
        alloc.free(entry.name);
        alloc.free(entry.version);
        alloc.free(entry.path);
    }
    entries.deinit(alloc);
}

fn writeString(writer: *std.Io.Writer, value: []const u8) !void {
    try writer.writeByte('"');
    for (value) |ch| switch (ch) {
        '\\' => try writer.writeAll("\\\\"),
        '"' => try writer.writeAll("\\\""),
        '\n' => try writer.writeAll("\\n"),
        '\r' => try writer.writeAll("\\r"),
        '\t' => try writer.writeAll("\\t"),
        else => try writer.writeByte(ch),
    };
    try writer.writeByte('"');
}
