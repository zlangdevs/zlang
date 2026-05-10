const std = @import("std");
const abi = @import("abi.zig");
const host_mod = @import("host.zig");
const loader_mod = @import("loader.zig");
const index_mod = @import("index.zig");
const manifest = @import("manifest.zig");
const package_mod = @import("package.zig");
const registry = @import("registry.zig");
const store_mod = @import("store.zig");

pub fn handle(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !?u8 {
    if (args.len < 2) return null;
    if (std.mem.eql(u8, args[1], "install")) return try install(args, alloc, io);
    if (std.mem.eql(u8, args[1], "list-modules")) return try listModules(args, alloc, io);
    if (std.mem.eql(u8, args[1], "module-load-order")) return try moduleLoadOrder(args, alloc, io);
    if (std.mem.eql(u8, args[1], "del-module")) return try delModule(args, alloc, io);
    if (std.mem.eql(u8, args[1], "validate-module")) return try validateModule(args, alloc, io);
    if (std.mem.eql(u8, args[1], "module-info")) return try moduleInfo(args, alloc, io);
    if (std.mem.eql(u8, args[1], "module-abi")) return try moduleAbi(args);
    if (std.mem.eql(u8, args[1], "module-dryrun")) return try moduleDryrun(args, alloc, io);
    if (std.mem.eql(u8, args[1], "module-load")) return try moduleLoad(args, alloc);
    if (std.mem.eql(u8, args[1], "module-loadall")) return try moduleLoadAll(args, alloc, io);
    return null;
}

fn moduleLoadAll(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    if (args.len != 2) {
        std.debug.print("Usage: zlang module-loadall\n", .{});
        return 1;
    }

    var store = store_mod.Store.init(alloc) catch |err| {
        std.debug.print("zlx: could not locate module store: {}\n", .{err});
        return 1;
    };
    defer store.deinit(alloc);

    const loaded_index = index_mod.load(alloc, io, store) catch |err| {
        std.debug.print("zlx: could not read module index: {}\n", .{err});
        return 1;
    };
    defer loaded_index.deinit(alloc);

    var rebuilt: ?std.ArrayList(index_mod.Entry) = null;
    defer if (rebuilt) |*r| index_mod.deinitEntries(r, alloc);
    const modules = if (!loaded_index.parsed) blk: {
        rebuilt = index_mod.rebuild(alloc, io, store) catch |err| {
            std.debug.print("zlx: could not rebuild module index: {}\n", .{err});
            return 1;
        };
        break :blk rebuilt.?.items;
    } else loaded_index.value.modules;

    var order = index_mod.loadOrder(alloc, modules) catch |err| {
        std.debug.print("zlx: could not compute load order: {}\n", .{err});
        return 1;
    };
    defer order.deinit(alloc);

    if (order.items.len == 0) {
        std.debug.print("No loadable modules\n", .{});
        return 0;
    }

    var host = host_mod.Host.init(alloc);
    defer host.deinit();

    var loaded: usize = 0;
    var skipped: usize = 0;
    var failed: usize = 0;
    for (order.items) |idx| {
        const entry = modules[idx];
        const sidecar = store.pluginPath(alloc, entry.name) catch |err| {
            std.debug.print("  {s}: path error: {}\n", .{ entry.name, err });
            failed += 1;
            continue;
        };
        defer alloc.free(sidecar);

        std.Io.Dir.cwd().access(io, sidecar, .{}) catch |err| switch (err) {
            error.FileNotFound => {
                std.debug.print("  {s}: skipped (no plugin sidecar)\n", .{entry.name});
                skipped += 1;
                continue;
            },
            else => {
                std.debug.print("  {s}: access error: {}\n", .{ entry.name, err });
                failed += 1;
                continue;
            },
        };

        const info = loader_mod.loadAndRegister(alloc, &host, sidecar) catch |err| {
            std.debug.print("  {s}: load failed: {s}\n", .{ entry.name, @errorName(err) });
            failed += 1;
            continue;
        };
        defer info.deinit(alloc);
        std.debug.print("  {s}: loaded {s} {s} (api {d}-{d})\n", .{ entry.name, info.name, info.version, info.api_min, info.api_max });
        loaded += 1;
    }

    const c = host.counts;
    std.debug.print("totals: loaded={d} skipped={d} failed={d}\n", .{ loaded, skipped, failed });
    std.debug.print("  syntax_blocks: {d} (duplicates {d})\n", .{ c.syntax_blocks, c.duplicate_syntax_blocks });
    std.debug.print("  modules:       {d} (duplicates {d})\n", .{ c.modules, c.duplicate_modules });
    std.debug.print("  cli_flags:     {d} (duplicates {d})\n", .{ c.cli_flags, c.duplicate_cli_flags });
    std.debug.print("  link_flags:    {d} (duplicates {d})\n", .{ c.link_flags, c.duplicate_link_flags });
    std.debug.print("  diagnostics:   {d}\n", .{c.diagnostics});
    if (failed != 0) return 1;
    return 0;
}

fn moduleLoad(args: []const [:0]u8, alloc: std.mem.Allocator) !u8 {
    if (args.len != 3) {
        std.debug.print("Usage: zlang module-load <file.so>\n", .{});
        return 1;
    }
    var host = host_mod.Host.init(alloc);
    defer host.deinit();
    const info = loader_mod.loadAndRegister(alloc, &host, args[2]) catch |err| {
        std.debug.print("zlx: module-load failed: {s}\n", .{@errorName(err)});
        return 1;
    };
    defer info.deinit(alloc);
    const c = host.counts;
    std.debug.print("loaded {s} {s} (api {d}-{d})\n", .{ info.name, info.version, info.api_min, info.api_max });
    std.debug.print("  syntax_blocks: {d} (duplicates {d})\n", .{ c.syntax_blocks, c.duplicate_syntax_blocks });
    std.debug.print("  modules:       {d} (duplicates {d})\n", .{ c.modules, c.duplicate_modules });
    std.debug.print("  cli_flags:     {d} (duplicates {d})\n", .{ c.cli_flags, c.duplicate_cli_flags });
    std.debug.print("  link_flags:    {d} (duplicates {d})\n", .{ c.link_flags, c.duplicate_link_flags });
    std.debug.print("  help_sections: {d}\n", .{c.help_sections});
    std.debug.print("  diagnostics:   {d}\n", .{c.diagnostics});
    return 0;
}

fn moduleDryrun(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    var pkg = (try openPackage(args, alloc, io, "zlang module-dryrun <file.zlx>")) orelse return 1;
    defer pkg.deinit(alloc);

    switch (abi.checkApiRange(pkg.manifest.api_min, pkg.manifest.api_max)) {
        .compatible => {},
        .api_too_old => {
            std.debug.print("zlx: package api {d}-{d} is older than host api {d}-{d}\n", .{ pkg.manifest.api_min, pkg.manifest.api_max, abi.api_min_supported, abi.api_max_supported });
            return 1;
        },
        .api_too_new => {
            std.debug.print("zlx: package api {d}-{d} is newer than host api {d}-{d}\n", .{ pkg.manifest.api_min, pkg.manifest.api_max, abi.api_min_supported, abi.api_max_supported });
            return 1;
        },
    }

    var host = host_mod.Host.init(alloc);
    defer host.deinit();
    host_mod.simulateFromManifest(&host, pkg.manifest) catch |err| {
        std.debug.print("zlx: dryrun failed: {}\n", .{err});
        return 1;
    };

    const c = host.counts;
    std.debug.print("dryrun {s} {s}:\n", .{ pkg.manifest.name, pkg.manifest.version });
    std.debug.print("  syntax_blocks: {d} (duplicates {d})\n", .{ c.syntax_blocks, c.duplicate_syntax_blocks });
    std.debug.print("  modules:       {d} (duplicates {d})\n", .{ c.modules, c.duplicate_modules });
    std.debug.print("  cli_flags:     {d} (duplicates {d})\n", .{ c.cli_flags, c.duplicate_cli_flags });
    std.debug.print("  link_flags:    {d} (duplicates {d})\n", .{ c.link_flags, c.duplicate_link_flags });
    std.debug.print("  diagnostics:   {d}\n", .{c.diagnostics});

    const dups = c.duplicate_syntax_blocks + c.duplicate_modules + c.duplicate_cli_flags + c.duplicate_link_flags + c.duplicate_help_sections;
    if (dups != 0) return 1;
    return 0;
}

fn moduleAbi(args: []const [:0]u8) !u8 {
    if (args.len != 2) {
        std.debug.print("Usage: zlang module-abi\n", .{});
        return 1;
    }
    std.debug.print("host_api_version: {d}\n", .{abi.api_version});
    std.debug.print("supported_range: {d}-{d}\n", .{ abi.api_min_supported, abi.api_max_supported });
    std.debug.print("probe_symbol: {s}\n", .{abi.probe_symbol});
    std.debug.print("init_symbol: {s}\n", .{abi.init_symbol});
    std.debug.print("header: include/zlang_plugin_api_v1.h\n", .{});
    return 0;
}

fn openPackage(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io, usage: []const u8) !?package_mod.Package {
    if (args.len != 3) {
        std.debug.print("Usage: {s}\n", .{usage});
        return null;
    }

    const source_path = args[2];
    if (!std.mem.endsWith(u8, source_path, ".zlx")) {
        std.debug.print("zlx: expected a .zlx package\n", .{});
        return null;
    }

    return package_mod.open(alloc, io, source_path) catch |err| {
        std.debug.print("zlx: could not open package {s}: {}\n", .{ source_path, err });
        return null;
    };
}

fn install(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    if (args.len != 3) {
        std.debug.print("Usage: zlang install <file.zlx>\n", .{});
        return 1;
    }
    if (!std.mem.endsWith(u8, args[2], ".zlx")) {
        std.debug.print("zlx: expected a .zlx package\n", .{});
        return 1;
    }

    var pkg = package_mod.open(alloc, io, args[2]) catch |err| {
        std.debug.print("zlx: could not open package {s}: {}\n", .{ args[2], err });
        return 1;
    };
    defer pkg.deinit(alloc);
    const parsed = pkg.manifest;
    const package = pkg.manifestSource();
    const registration_diagnostics = registry.validateManifest(alloc, parsed) catch |err| {
        std.debug.print("zlx: could not validate extension registrations: {}\n", .{err});
        return 1;
    };
    if (registration_diagnostics.conflicts != 0) {
        std.debug.print("zlx: manifest has {d} duplicate registration(s)\n", .{registration_diagnostics.conflicts});
        return 1;
    }

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
    const status: index_mod.Status = if (manifest.supportsCurrentTarget(parsed)) .installed else .incompatible;
    const status_reason: ?[]const u8 = if (status == .incompatible) "unsupported target" else null;

    var out_file = std.Io.Dir.cwd().createFile(io, dest_path, .{ .truncate = true }) catch |err| {
        std.debug.print("zlx: could not write {s}: {}\n", .{ dest_path, err });
        return 1;
    };
    defer out_file.close(io);
    var out_buffer: [4096]u8 = undefined;
    var out_writer = out_file.writer(io, &out_buffer);
    try out_writer.interface.writeAll(package);
    try out_writer.interface.flush();

    const sidecar_installed = installSidecar(alloc, io, store, args[2], parsed.name) catch |err| {
        std.debug.print("zlx: could not install plugin sidecar: {}\n", .{err});
        return 1;
    };

    const loaded_index = index_mod.load(alloc, io, store) catch |err| {
        std.debug.print("zlx: could not read module index: {}\n", .{err});
        return 1;
    };
    defer loaded_index.deinit(alloc);
    var next_entries: std.ArrayList(index_mod.Entry) = .empty;
    defer next_entries.deinit(alloc);
    var replaced = false;
    for (loaded_index.value.modules) |entry| {
        if (std.mem.eql(u8, entry.name, parsed.name)) {
            try next_entries.append(alloc, .{
                .name = parsed.name,
                .version = parsed.version,
                .path = dest_path,
                .api_min = parsed.api_min,
                .api_max = parsed.api_max,
                .dependencies = parsed.dependencies,
                .status = status,
                .status_reason = status_reason,
            });
            replaced = true;
        } else {
            try next_entries.append(alloc, entry);
        }
    }
    if (!replaced) {
        try next_entries.append(alloc, .{
            .name = parsed.name,
            .version = parsed.version,
            .path = dest_path,
            .api_min = parsed.api_min,
            .api_max = parsed.api_max,
            .dependencies = parsed.dependencies,
            .status = status,
            .status_reason = status_reason,
        });
    }
    index_mod.applyDependencyStatuses(alloc, next_entries.items) catch |err| {
        std.debug.print("zlx: could not validate dependency graph: {}\n", .{err});
        return 1;
    };
    index_mod.write(alloc, io, store, next_entries.items) catch |err| {
        std.debug.print("zlx: could not update module index: {}\n", .{err});
        return 1;
    };

    if (status == .incompatible) {
        std.debug.print("Installed {s} {s} as incompatible with current target {s}\n", .{ parsed.name, parsed.version, manifest.currentTargetName() });
    } else if (sidecar_installed) {
        std.debug.print("Installed {s} {s} (with plugin sidecar)\n", .{ parsed.name, parsed.version });
    } else {
        std.debug.print("Installed {s} {s}\n", .{ parsed.name, parsed.version });
    }
    return 0;
}

fn installSidecar(alloc: std.mem.Allocator, io: std.Io, store: store_mod.Store, source_zlx: []const u8, name: []const u8) !bool {
    if (!std.mem.endsWith(u8, source_zlx, ".zlx")) return false;
    const stem = source_zlx[0 .. source_zlx.len - 4];
    const sidecar_src = try std.fmt.allocPrint(alloc, "{s}.so", .{stem});
    defer alloc.free(sidecar_src);

    const bytes = std.Io.Dir.cwd().readFileAlloc(io, sidecar_src, alloc, .limited(64 * 1024 * 1024)) catch |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    };
    defer alloc.free(bytes);

    const dest = try store.pluginPath(alloc, name);
    defer alloc.free(dest);
    var out = try std.Io.Dir.cwd().createFile(io, dest, .{ .truncate = true });
    defer out.close(io);
    var buf: [4096]u8 = undefined;
    var writer = out.writer(io, &buf);
    try writer.interface.writeAll(bytes);
    try writer.interface.flush();
    return true;
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

    const loaded_index = index_mod.load(alloc, io, store) catch |err| {
        std.debug.print("zlx: could not read module index: {}\n", .{err});
        return 1;
    };
    defer loaded_index.deinit(alloc);

    var rebuilt_entries: ?std.ArrayList(index_mod.Entry) = null;
    defer if (rebuilt_entries) |*entries| index_mod.deinitEntries(entries, alloc);
    const modules = if (!loaded_index.parsed) blk: {
        rebuilt_entries = index_mod.rebuild(alloc, io, store) catch |err| {
            std.debug.print("zlx: could not rebuild module index: {}\n", .{err});
            return 1;
        };
        break :blk rebuilt_entries.?.items;
    } else loaded_index.value.modules;

    for (modules) |entry| {
        if (entry.status_reason) |reason| {
            std.debug.print("{s} {s} (api {d}-{d}, {s}: {s})\n", .{ entry.name, entry.version, entry.api_min, entry.api_max, @tagName(entry.status), reason });
        } else {
            std.debug.print("{s} {s} (api {d}-{d}, {s})\n", .{ entry.name, entry.version, entry.api_min, entry.api_max, @tagName(entry.status) });
        }
    }

    if (modules.len == 0) std.debug.print("No modules installed\n", .{});
    return 0;
}

fn moduleLoadOrder(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    if (args.len != 2) {
        std.debug.print("Usage: zlang module-load-order\n", .{});
        return 1;
    }

    var store = store_mod.Store.init(alloc) catch |err| {
        std.debug.print("zlx: could not locate module store: {}\n", .{err});
        return 1;
    };
    defer store.deinit(alloc);

    const loaded_index = index_mod.load(alloc, io, store) catch |err| {
        std.debug.print("zlx: could not read module index: {}\n", .{err});
        return 1;
    };
    defer loaded_index.deinit(alloc);

    var rebuilt_entries: ?std.ArrayList(index_mod.Entry) = null;
    defer if (rebuilt_entries) |*entries| index_mod.deinitEntries(entries, alloc);
    const modules = if (!loaded_index.parsed) blk: {
        rebuilt_entries = index_mod.rebuild(alloc, io, store) catch |err| {
            std.debug.print("zlx: could not rebuild module index: {}\n", .{err});
            return 1;
        };
        break :blk rebuilt_entries.?.items;
    } else loaded_index.value.modules;

    var order = index_mod.loadOrder(alloc, modules) catch |err| {
        std.debug.print("zlx: could not compute module load order: {}\n", .{err});
        return 1;
    };
    defer order.deinit(alloc);
    if (order.items.len == 0) {
        std.debug.print("No loadable modules\n", .{});
        return 0;
    }
    for (order.items) |module_index| {
        std.debug.print("{s}\n", .{modules[module_index].name});
    }
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

    const sidecar = store.pluginPath(alloc, args[2]) catch |err| {
        std.debug.print("zlx: could not build plugin path: {}\n", .{err});
        return 1;
    };
    defer alloc.free(sidecar);
    std.Io.Dir.deleteFileAbsolute(io, sidecar) catch |err| switch (err) {
        error.FileNotFound => {},
        else => {
            std.debug.print("zlx: could not delete {s}: {}\n", .{ sidecar, err });
            return 1;
        },
    };

    const loaded_index = index_mod.load(alloc, io, store) catch |err| {
        std.debug.print("zlx: could not read module index: {}\n", .{err});
        return 1;
    };
    defer loaded_index.deinit(alloc);
    var next_entries: std.ArrayList(index_mod.Entry) = .empty;
    defer next_entries.deinit(alloc);
    for (loaded_index.value.modules) |entry| {
        if (!std.mem.eql(u8, entry.name, args[2])) try next_entries.append(alloc, entry);
    }
    index_mod.applyDependencyStatuses(alloc, next_entries.items) catch |err| {
        std.debug.print("zlx: could not validate dependency graph: {}\n", .{err});
        return 1;
    };
    index_mod.write(alloc, io, store, next_entries.items) catch |err| {
        std.debug.print("zlx: could not update module index: {}\n", .{err});
        return 1;
    };

    std.debug.print("Deleted {s}\n", .{args[2]});
    return 0;
}

fn validateModule(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    var pkg = (try openPackage(args, alloc, io, "zlang validate-module <file.zlx>")) orelse return 1;
    defer pkg.deinit(alloc);
    const parsed = pkg.manifest;
    const registration_diagnostics = registry.validateManifest(alloc, parsed) catch |err| {
        std.debug.print("zlx: could not validate extension registrations: {}\n", .{err});
        return 1;
    };
    if (registration_diagnostics.conflicts != 0) {
        std.debug.print("zlx: manifest has {d} duplicate registration(s)\n", .{registration_diagnostics.conflicts});
        return 1;
    }
    std.debug.print("Valid module: {s} {s}\n", .{ parsed.name, parsed.version });
    return 0;
}

fn moduleInfo(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    var pkg = (try openPackage(args, alloc, io, "zlang module-info <file.zlx>")) orelse return 1;
    defer pkg.deinit(alloc);
    const parsed = pkg.manifest;

    std.debug.print("name: {s}\n", .{parsed.name});
    std.debug.print("version: {s}\n", .{parsed.version});
    std.debug.print("format: {d}\n", .{parsed.format_version});
    std.debug.print("layout: {s}\n", .{package_mod.layoutName(pkg.layout)});
    std.debug.print("api: {d}-{d}\n", .{ parsed.api_min, parsed.api_max });
    std.debug.print("current_target: {s}\n", .{manifest.currentTargetName()});
    std.debug.print("target_compatible: {}\n", .{manifest.supportsCurrentTarget(parsed)});
    const registration_diagnostics = try registry.validateManifest(alloc, parsed);
    std.debug.print("registrations: modules={d}, syntax_blocks={d}, cli_flags={d}, conflicts={d}\n", .{
        registration_diagnostics.modules,
        registration_diagnostics.syntax_blocks,
        registration_diagnostics.cli_flags,
        registration_diagnostics.conflicts,
    });
    if (parsed.entry) |entry| {
        std.debug.print("entry: {s}\n", .{entry});
    } else {
        std.debug.print("entry: none\n", .{});
    }
    printStringList("provides", parsed.provides);
    printStringList("targets", parsed.targets);
    printStringList("dependencies", parsed.dependencies);
    printModules(parsed.modules);
    printSyntaxBlocks(parsed.syntax_blocks);
    printCliFlags(parsed.cli_flags);
    printStringList("native_libs", parsed.native_libs);
    printStringList("link_flags", parsed.link_flags);
    std.debug.print("expose.global_syntax: {}\n", .{parsed.expose.global_syntax});
    std.debug.print("expose.global_modules: {}\n", .{parsed.expose.global_modules});
    return 0;
}

fn printModules(modules: []const manifest.Module) void {
    std.debug.print("modules:", .{});
    if (modules.len == 0) {
        std.debug.print(" none\n", .{});
        return;
    }
    std.debug.print("\n", .{});
    for (modules) |module| {
        std.debug.print("  - {s}: {s}\n", .{ module.name, module.path });
    }
}

fn printSyntaxBlocks(blocks: []const manifest.SyntaxBlock) void {
    std.debug.print("syntax_blocks:", .{});
    if (blocks.len == 0) {
        std.debug.print(" none\n", .{});
        return;
    }
    std.debug.print("\n", .{});
    for (blocks) |block| {
        std.debug.print("  - {s}: {s}, mandatory={}\n", .{ block.name, @tagName(block.delimiter_mode), block.mandatory });
    }
}

fn printCliFlags(flags: []const manifest.CliFlag) void {
    std.debug.print("cli_flags:", .{});
    if (flags.len == 0) {
        std.debug.print(" none\n", .{});
        return;
    }
    std.debug.print("\n", .{});
    for (flags) |flag| {
        if (flag.value) |value| {
            std.debug.print("  - {s}={s}, mandatory={}\n", .{ flag.name, value, flag.mandatory });
        } else {
            std.debug.print("  - {s}, mandatory={}\n", .{ flag.name, flag.mandatory });
        }
    }
}

fn printStringList(label: []const u8, values: []const []const u8) void {
    std.debug.print("{s}:", .{label});
    if (values.len == 0) {
        std.debug.print(" none\n", .{});
        return;
    }
    std.debug.print("\n", .{});
    for (values) |value| {
        std.debug.print("  - {s}\n", .{value});
    }
}
