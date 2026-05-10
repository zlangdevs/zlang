const std = @import("std");
const manifest_mod = @import("manifest.zig");
const store_mod = @import("store.zig");

const max_index_size = 1024 * 1024;
const max_package_size = 16 * 1024 * 1024;

pub const Status = enum {
    installed,
    incompatible,
    missing,
    broken,
};

pub const Entry = struct {
    name: []const u8,
    version: []const u8,
    path: []const u8,
    api_min: u32,
    api_max: u32,
    dependencies: []const []const u8 = &.{},
    status: Status = .installed,
    status_reason: ?[]const u8 = null,
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
        try writer.interface.print(", .api_min = {d}, .api_max = {d}, .dependencies = .{{", .{ entry.api_min, entry.api_max });
        for (entry.dependencies, 0..) |dep, i| {
            if (i != 0) try writer.interface.writeAll(", ");
            try writeString(&writer.interface, dep);
        }
        try writer.interface.print("}}, .status = .{s}", .{@tagName(entry.status)});
        if (entry.status_reason) |reason| {
            try writer.interface.writeAll(", .status_reason = ");
            try writeString(&writer.interface, reason);
        }
        try writer.interface.writeAll(" },\n");
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
            .dependencies = try dupeStringList(alloc, parsed.dependencies),
            .status = if (manifest_mod.supportsCurrentTarget(parsed)) .installed else .incompatible,
            .status_reason = if (manifest_mod.supportsCurrentTarget(parsed)) null else "unsupported target",
        });
    }

    try applyDependencyStatuses(alloc, entries.items);
    try write(alloc, io, store, entries.items);
    return entries;
}

pub fn applyDependencyStatuses(alloc: std.mem.Allocator, entries: []Entry) !void {
    applyMissingDependencyStatuses(entries);
    try applyCycleStatuses(alloc, entries);
    applyMissingDependencyStatuses(entries);
}

fn applyMissingDependencyStatuses(entries: []Entry) void {
    var changed = true;
    while (changed) {
        changed = false;
        for (entries) |*entry| {
            if (entry.status == .incompatible and entry.status_reason != null and !std.mem.eql(u8, entry.status_reason.?, "missing dependency")) continue;
            const old_status = entry.status;
            const old_reason = entry.status_reason;
            for (entry.dependencies) |dep| {
                if (!hasInstalledDependency(entries, dep)) {
                    entry.status = .incompatible;
                    entry.status_reason = "missing dependency";
                    break;
                }
            } else if (entry.status_reason != null and std.mem.eql(u8, entry.status_reason.?, "missing dependency")) {
                entry.status = .installed;
                entry.status_reason = null;
            }
            if (entry.status != old_status or reasonChanged(old_reason, entry.status_reason)) changed = true;
        }
    }
}

pub fn loadOrder(alloc: std.mem.Allocator, entries: []const Entry) !std.ArrayList(usize) {
    var order: std.ArrayList(usize) = .empty;
    errdefer order.deinit(alloc);
    const state = try alloc.alloc(u8, entries.len);
    defer alloc.free(state);
    @memset(state, 0);

    for (entries, 0..) |entry, i| {
        if (entry.status != .installed) continue;
        try visitLoadOrder(alloc, entries, i, state, &order);
    }
    return order;
}

fn visitLoadOrder(alloc: std.mem.Allocator, entries: []const Entry, index: usize, state: []u8, order: *std.ArrayList(usize)) !void {
    if (state[index] == 2) return;
    if (state[index] == 1) return error.DependencyCycle;
    state[index] = 1;
    for (entries[index].dependencies) |dep| {
        const dep_index = findEntry(entries, dep) orelse continue;
        if (entries[dep_index].status == .installed) try visitLoadOrder(alloc, entries, dep_index, state, order);
    }
    state[index] = 2;
    try order.append(alloc, index);
}

fn applyCycleStatuses(alloc: std.mem.Allocator, entries: []Entry) !void {
    const state = try alloc.alloc(u8, entries.len);
    defer alloc.free(state);
    for (entries, 0..) |entry, i| {
        if (entry.status != .installed) continue;
        @memset(state, 0);
        if (hasCycleFrom(entries, i, state)) markCycle(entries, i);
    }
}

fn hasCycleFrom(entries: []const Entry, index: usize, state: []u8) bool {
    if (state[index] == 1) return true;
    if (state[index] == 2) return false;
    state[index] = 1;
    for (entries[index].dependencies) |dep| {
        const dep_index = findEntry(entries, dep) orelse continue;
        if (entries[dep_index].status != .installed) continue;
        if (hasCycleFrom(entries, dep_index, state)) return true;
    }
    state[index] = 2;
    return false;
}

fn markCycle(entries: []Entry, index: usize) void {
    if (entries[index].status == .incompatible and entries[index].status_reason != null and !std.mem.eql(u8, entries[index].status_reason.?, "dependency cycle")) return;
    entries[index].status = .incompatible;
    entries[index].status_reason = "dependency cycle";
}

fn findEntry(entries: []const Entry, name: []const u8) ?usize {
    for (entries, 0..) |entry, i| {
        if (std.mem.eql(u8, entry.name, name)) return i;
    }
    return null;
}

fn reasonChanged(a: ?[]const u8, b: ?[]const u8) bool {
    if (a == null and b == null) return false;
    if (a == null or b == null) return true;
    return !std.mem.eql(u8, a.?, b.?);
}

fn hasInstalledDependency(entries: []const Entry, name: []const u8) bool {
    for (entries) |entry| {
        if (std.mem.eql(u8, entry.name, name) and entry.status == .installed) return true;
    }
    return false;
}

pub fn deinitEntries(entries: *std.ArrayList(Entry), alloc: std.mem.Allocator) void {
    for (entries.items) |entry| {
        alloc.free(entry.name);
        alloc.free(entry.version);
        alloc.free(entry.path);
        freeStringList(alloc, entry.dependencies);
    }
    entries.deinit(alloc);
}

fn dupeStringList(alloc: std.mem.Allocator, values: []const []const u8) ![]const []const u8 {
    const out = try alloc.alloc([]const u8, values.len);
    errdefer alloc.free(out);
    for (values, 0..) |value, i| {
        out[i] = try alloc.dupe(u8, value);
    }
    return out;
}

fn freeStringList(alloc: std.mem.Allocator, values: []const []const u8) void {
    for (values) |value| alloc.free(value);
    alloc.free(values);
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
