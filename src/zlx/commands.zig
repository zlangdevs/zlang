const std = @import("std");
const manifest = @import("manifest.zig");
const store_mod = @import("store.zig");

const max_package_size = 16 * 1024 * 1024;

pub fn handle(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !?u8 {
    if (args.len < 2) return null;
    if (std.mem.eql(u8, args[1], "install")) return try install(args, alloc, io);
    if (std.mem.eql(u8, args[1], "list-modules")) return try listModules(args, alloc, io);
    if (std.mem.eql(u8, args[1], "del-module")) return try delModule(args, alloc, io);
    return null;
}

fn install(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    if (args.len != 3) {
        std.debug.print("Usage: zlang install <file.zlx>\n", .{});
        return 1;
    }

    const source_path = args[2];
    if (!std.mem.endsWith(u8, source_path, ".zlx")) {
        std.debug.print("zlx: expected a .zlx package\n", .{});
        return 1;
    }

    const package = std.Io.Dir.cwd().readFileAlloc(io, source_path, alloc, .limited(max_package_size)) catch |err| {
        std.debug.print("zlx: could not read {s}: {}\n", .{ source_path, err });
        return 1;
    };
    defer alloc.free(package);

    const parsed = manifest.parse(alloc, package) catch |err| {
        std.debug.print("zlx: invalid manifest in {s}: {}\n", .{ source_path, err });
        return 1;
    };
    defer manifest.free(alloc, parsed);

    var store = store_mod.Store.init(alloc) catch |err| {
        std.debug.print("zlx: could not locate module store: {}\n", .{err});
        return 1;
    };
    defer store.deinit(alloc);
    store.ensure(io) catch |err| {
        std.debug.print("zlx: could not create module store {s}: {}\n", .{ store.root, err });
        return 1;
    };

    const dest_path = store.modulePath(alloc, parsed.name) catch |err| {
        std.debug.print("zlx: could not build destination path: {}\n", .{err});
        return 1;
    };
    defer alloc.free(dest_path);

    var out_file = std.Io.Dir.cwd().createFile(io, dest_path, .{ .truncate = true }) catch |err| {
        std.debug.print("zlx: could not write {s}: {}\n", .{ dest_path, err });
        return 1;
    };
    defer out_file.close(io);
    var out_buffer: [4096]u8 = undefined;
    var out_writer = out_file.writer(io, &out_buffer);
    try out_writer.interface.writeAll(package);
    try out_writer.interface.flush();

    std.debug.print("Installed {s} {s}\n", .{ parsed.name, parsed.version });
    return 0;
}

fn listModules(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    if (args.len != 2) {
        std.debug.print("Usage: zlang list-modules\n", .{});
        return 1;
    }

    var store = store_mod.Store.init(alloc) catch |err| {
        std.debug.print("zlx: could not locate module store: {}\n", .{err});
        return 1;
    };
    defer store.deinit(alloc);

    var dir = std.Io.Dir.openDirAbsolute(io, store.root, .{ .iterate = true }) catch |err| switch (err) {
        error.FileNotFound => {
            std.debug.print("No modules installed\n", .{});
            return 0;
        },
        else => {
            std.debug.print("zlx: could not open module store {s}: {}\n", .{ store.root, err });
            return 1;
        },
    };
    defer dir.close(io);

    var count: usize = 0;
    var it = dir.iterate();
    while (try it.next(io)) |entry| {
        if (entry.kind != .file or !std.mem.endsWith(u8, entry.name, ".zlx")) continue;
        const path = try std.fmt.allocPrint(alloc, "{s}/{s}", .{ store.root, entry.name });
        defer alloc.free(path);
        const package = std.Io.Dir.cwd().readFileAlloc(io, path, alloc, .limited(max_package_size)) catch continue;
        defer alloc.free(package);
        const parsed = manifest.parse(alloc, package) catch continue;
        defer manifest.free(alloc, parsed);
        std.debug.print("{s} {s} (api {d}-{d})\n", .{ parsed.name, parsed.version, parsed.api_min, parsed.api_max });
        count += 1;
    }

    if (count == 0) std.debug.print("No modules installed\n", .{});
    return 0;
}

fn delModule(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    if (args.len != 3) {
        std.debug.print("Usage: zlang del-module <name>\n", .{});
        return 1;
    }
    if (!manifest.isValidName(args[2])) {
        std.debug.print("zlx: invalid module name\n", .{});
        return 1;
    }

    var store = store_mod.Store.init(alloc) catch |err| {
        std.debug.print("zlx: could not locate module store: {}\n", .{err});
        return 1;
    };
    defer store.deinit(alloc);

    const path = store.modulePath(alloc, args[2]) catch |err| {
        std.debug.print("zlx: could not build module path: {}\n", .{err});
        return 1;
    };
    defer alloc.free(path);

    std.Io.Dir.deleteFileAbsolute(io, path) catch |err| switch (err) {
        error.FileNotFound => {
            std.debug.print("zlx: module not installed: {s}\n", .{args[2]});
            return 1;
        },
        else => {
            std.debug.print("zlx: could not delete {s}: {}\n", .{ path, err });
            return 1;
        },
    };

    std.debug.print("Deleted {s}\n", .{args[2]});
    return 0;
}
