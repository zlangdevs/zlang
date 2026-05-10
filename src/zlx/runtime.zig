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
