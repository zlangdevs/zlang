const std = @import("std");
const host_mod = @import("host.zig");
const index_mod = @import("index.zig");
const loader_mod = @import("loader.zig");
const store_mod = @import("store.zig");

pub const LoadAllReport = struct {
    loaded: usize = 0,
    skipped: usize = 0,
    failed: usize = 0,
};

pub fn loadAllInstalled(alloc: std.mem.Allocator, io: std.Io, host: *host_mod.Host) !LoadAllReport {
    var store = store_mod.Store.init(alloc) catch return .{};
    defer store.deinit(alloc);

    const loaded_index = index_mod.load(alloc, io, store) catch return .{};
    defer loaded_index.deinit(alloc);

    var rebuilt: ?std.ArrayList(index_mod.Entry) = null;
    defer if (rebuilt) |*r| index_mod.deinitEntries(r, alloc);
    const modules = if (!loaded_index.parsed) blk: {
        rebuilt = index_mod.rebuild(alloc, io, store) catch return .{};
        break :blk rebuilt.?.items;
    } else loaded_index.value.modules;

    var order = index_mod.loadOrder(alloc, modules) catch return .{};
    defer order.deinit(alloc);

    var report: LoadAllReport = .{};
    for (order.items) |idx| {
        const entry = modules[idx];
        const sidecar = store.pluginPath(alloc, entry.name) catch {
            report.failed += 1;
            continue;
        };
        defer alloc.free(sidecar);
        std.Io.Dir.cwd().access(io, sidecar, .{}) catch {
            report.skipped += 1;
            continue;
        };
        const info = loader_mod.loadAndRegister(alloc, host, sidecar) catch {
            report.failed += 1;
            continue;
        };
        info.deinit(alloc);
        report.loaded += 1;
    }
    return report;
}

const manifest_mod = @import("manifest.zig");
const package_mod = @import("package.zig");

pub fn loadManifestModulesOnly(alloc: std.mem.Allocator, io: std.Io, host: *host_mod.Host) !void {
    var store = store_mod.Store.init(alloc) catch return;
    defer store.deinit(alloc);

    const loaded_index = index_mod.load(alloc, io, store) catch return;
    defer loaded_index.deinit(alloc);

    var rebuilt: ?std.ArrayList(index_mod.Entry) = null;
    defer if (rebuilt) |*r| index_mod.deinitEntries(r, alloc);
    const modules = if (!loaded_index.parsed) blk: {
        rebuilt = index_mod.rebuild(alloc, io, store) catch return;
        break :blk rebuilt.?.items;
    } else loaded_index.value.modules;

    for (modules) |entry| {
        if (entry.status != .installed) continue;
        var pkg = package_mod.open(alloc, io, entry.path) catch continue;
        defer pkg.deinit(alloc);
        host.setCurrentOwner(pkg.manifest.name) catch continue;
        defer host.clearCurrentOwner();
        for (pkg.manifest.modules) |mod| {
            const name_z = alloc.dupeZ(u8, mod.name) catch continue;
            defer alloc.free(name_z);
            const path_z = alloc.dupeZ(u8, mod.path) catch continue;
            defer alloc.free(path_z);
            _ = host.api.register_module(&host.api, name_z.ptr, path_z.ptr);
        }
    }
}

pub fn scanUseImports(alloc: std.mem.Allocator, io: std.Io, paths: []const []const u8) !std.StringHashMap(void) {
    var result: std.StringHashMap(void) = .init(alloc);
    errdefer {
        var it = result.keyIterator();
        while (it.next()) |k| alloc.free(k.*);
        result.deinit();
    }

    for (paths) |path| {
        const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, alloc, .limited(16 * 1024 * 1024)) catch continue;
        defer alloc.free(bytes);

        var line_it = std.mem.splitScalar(u8, bytes, '\n');
        while (line_it.next()) |raw_line| {
            const line = std.mem.trim(u8, raw_line, " \t\r");
            if (!std.mem.startsWith(u8, line, "use ") and !std.mem.startsWith(u8, line, "use\t")) continue;
            var rest = std.mem.trim(u8, line[3..], " \t");
            if (std.mem.indexOfAny(u8, rest, "/*")) |c| rest = rest[0..c];
            rest = std.mem.trim(u8, rest, " \t;");
            if (rest.len == 0) continue;
            const dot = std.mem.indexOfScalar(u8, rest, '.') orelse rest.len;
            const head = rest[0..dot];
            if (head.len == 0) continue;
            if (result.contains(head)) continue;
            const owned = try alloc.dupe(u8, head);
            try result.put(owned, {});
        }
    }
    return result;
}

pub fn freeUseImports(alloc: std.mem.Allocator, set: *std.StringHashMap(void)) void {
    var it = set.keyIterator();
    while (it.next()) |k| alloc.free(k.*);
    set.deinit();
}

pub fn printPluginExtensions(host: *const host_mod.Host) void {
    const has_any = host.cli_flags.items.len != 0 or
        host.syntax_blocks.items.len != 0 or
        host.modules.items.len != 0 or
        host.help_sections.items.len != 0;
    if (!has_any) return;

    std.debug.print("\nPlugin extensions:\n", .{});
    if (host.cli_flags.items.len != 0) {
        std.debug.print("  CLI flags:\n", .{});
        for (host.cli_flags.items) |flag| {
            if (flag.help) |h| {
                std.debug.print("    {s}  {s}\n", .{ flag.name, h });
            } else {
                std.debug.print("    {s}\n", .{flag.name});
            }
        }
    }
    if (host.syntax_blocks.items.len != 0) {
        std.debug.print("  Syntax blocks:\n", .{});
        for (host.syntax_blocks.items) |block| {
            std.debug.print("    {s} ({s})\n", .{ block.name, @tagName(block.mode) });
        }
    }
    if (host.modules.items.len != 0) {
        std.debug.print("  Modules:\n", .{});
        for (host.modules.items) |module| {
            std.debug.print("    {s} -> {s}\n", .{ module.name, module.path });
        }
    }
    if (host.help_sections.items.len != 0) {
        std.debug.print("  Help sections:\n", .{});
        for (host.help_sections.items) |section| {
            std.debug.print("    [{s}] {s}\n", .{ section.section_id, section.text });
        }
    }
}
