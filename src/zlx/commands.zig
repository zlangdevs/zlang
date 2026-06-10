const std = @import("std");
const abi = @import("abi.zig");
const host_mod = @import("host.zig");
const loader_mod = @import("loader.zig");
const index_mod = @import("index.zig");
const manifest = @import("manifest.zig");
const package_mod = @import("package.zig");
const registry = @import("registry.zig");
const store_mod = @import("store.zig");
const sdk_embed = @import("sdk_embed.zig");

extern fn inotify_init1(flags: c_int) c_int;
extern fn inotify_add_watch(fd: c_int, pathname: [*:0]const u8, mask: u32) c_int;
extern fn close(fd: c_int) c_int;
extern fn usleep(usec: c_uint) c_int;

// Linux inotify constants used by `zlang module dev`.
const IN_MODIFY: u32 = 0x00000002;
const IN_CLOSE_WRITE: u32 = 0x00000008;
const IN_MOVED_FROM: u32 = 0x00000040;
const IN_MOVED_TO: u32 = 0x00000080;
const IN_CREATE: u32 = 0x00000100;
const IN_DELETE: u32 = 0x00000200;
const IN_ISDIR: u32 = 0x40000000;
const IN_NONBLOCK: c_int = 0x800;
const dev_watch_mask: u32 = IN_MODIFY | IN_CLOSE_WRITE | IN_MOVED_FROM | IN_MOVED_TO | IN_CREATE | IN_DELETE;

var dev_stop_requested = false;

fn handleDevSigint(_: std.posix.SIG) callconv(.c) void {
    dev_stop_requested = true;
}

pub fn handle(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !?u8 {
    if (args.len < 2) return null;
    if (!std.mem.eql(u8, args[1], "module")) return null;
    if (args.len < 3) {
        printModuleHelp();
        return 0;
    }
    const sub_args = args[1..];
    const cmd = args[2];
    if (std.mem.eql(u8, cmd, "help")) {
        printModuleHelp();
        return 0;
    }
    if (std.mem.eql(u8, cmd, "install")) return try install(sub_args, alloc, io);
    if (std.mem.eql(u8, cmd, "list")) return try listModules(sub_args, alloc, io);
    if (std.mem.eql(u8, cmd, "load-order")) return try moduleLoadOrder(sub_args, alloc, io);
    if (std.mem.eql(u8, cmd, "delete")) return try delModule(sub_args, alloc, io);
    if (std.mem.eql(u8, cmd, "validate")) return try validateModule(sub_args, alloc, io);
    if (std.mem.eql(u8, cmd, "info")) return try moduleInfo(sub_args, alloc, io);
    if (std.mem.eql(u8, cmd, "abi")) return try moduleAbi(sub_args);
    if (std.mem.eql(u8, cmd, "dryrun")) return try moduleDryrun(sub_args, alloc, io);
    if (std.mem.eql(u8, cmd, "load")) return try moduleLoad(sub_args, alloc);
    if (std.mem.eql(u8, cmd, "loadall")) return try moduleLoadAll(sub_args, alloc, io);
    if (std.mem.eql(u8, cmd, "doctor")) return try doctorModules(sub_args, alloc, io);
    if (std.mem.eql(u8, cmd, "pack")) return try packModule(sub_args, alloc, io);
    if (std.mem.eql(u8, cmd, "build")) return try buildModule(sub_args, alloc, io);
    if (std.mem.eql(u8, cmd, "sdk")) return try sdkCommand(sub_args, alloc, io);
    if (std.mem.eql(u8, cmd, "dev")) return try devModule(sub_args, alloc, io);
    std.debug.print("zlx: unknown module subcommand '{s}'. Try 'zlang module help'.\n", .{cmd});
    return 1;
}

fn printModuleHelp() void {
    std.debug.print(
        \\Usage: zlang module <subcommand> [args]
        \\
        \\Subcommands:
        \\  install <file.zlx>    Install a .zlx extension package
        \\  delete <name>         Remove an installed extension
        \\  list                  List installed extensions and their status
        \\  info <file.zlx>       Print metadata of a .zlx package
        \\  validate <file.zlx>   Validate a .zlx package's manifest
        \\  abi                   Print host plugin ABI version and entry symbols
        \\  load <file.so>        Load a single plugin .so against the host
        \\  loadall               Load every installed plugin in topological order
        \\  dryrun <file.zlx>     Simulate plugin registrations from manifest
        \\  load-order            Print topological load order of installed modules
        \\  doctor                Diagnose installed extensions (status, api, hash, sidecar)
        \\  pack <dir> -o <out>   Pack <dir>/manifest.zon + declared files into <out>.zlx
        \\  build <dir>           Compile src/plugin.zig or src/plugin.c against the bundled SDK and pack
        \\  sdk [--zig|--include] [dir]  Materialize the bundled plugin SDK and print its path
        \\  dev <dir>             Watch, rebuild, and install an extension package
        \\  help                  Show this help
        \\
    , .{});
}

fn doctorModules(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    if (args.len != 2) {
        std.debug.print("Usage: zlang module doctor\n", .{});
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

    std.debug.print("host_api: {d} (supported {d}-{d})\n", .{ abi.api_version, abi.api_min_supported, abi.api_max_supported });
    std.debug.print("store: {s}\n", .{store.root});
    std.debug.print("modules: {d}\n", .{modules.len});

    var problems: usize = 0;
    for (modules) |entry| {
        var ok = true;
        var notes: std.ArrayList(u8) = .empty;
        defer notes.deinit(alloc);

        if (entry.status != .installed) {
            ok = false;
            try notes.appendSlice(alloc, "status=");
            try notes.appendSlice(alloc, @tagName(entry.status));
            if (entry.status_reason) |r| {
                try notes.appendSlice(alloc, " (");
                try notes.appendSlice(alloc, r);
                try notes.appendSlice(alloc, ")");
            }
            try notes.appendSlice(alloc, "; ");
        }

        switch (abi.checkApiRange(entry.api_min, entry.api_max)) {
            .compatible => {},
            .api_too_old, .api_too_new => {
                ok = false;
                try notes.appendSlice(alloc, "api out of host range; ");
            },
        }

        const sidecar = store.pluginPath(alloc, entry.name) catch null;
        defer if (sidecar) |s| alloc.free(s);
        if (sidecar) |s| {
            std.Io.Dir.cwd().access(io, s, .{}) catch {
                try notes.appendSlice(alloc, "no sidecar .so; ");
            };
        }

        if (entry.manifest_sha256) |recorded| {
            const bytes = std.Io.Dir.cwd().readFileAlloc(io, entry.path, alloc, .limited(16 * 1024 * 1024)) catch null;
            if (bytes) |b| {
                defer alloc.free(b);
                const fresh = index_mod.hashHex(alloc, b) catch null;
                if (fresh) |f| {
                    defer alloc.free(f);
                    if (!std.mem.eql(u8, f, recorded)) {
                        try notes.appendSlice(alloc, "manifest hash mismatch; ");
                    }
                }
            } else {
                try notes.appendSlice(alloc, "manifest unreadable; ");
            }
        } else {
            try notes.appendSlice(alloc, "no manifest hash recorded; ");
        }

        if (ok and notes.items.len == 0) {
            std.debug.print("  OK   {s} {s} (api {d}-{d})\n", .{ entry.name, entry.version, entry.api_min, entry.api_max });
        } else {
            problems += 1;
            std.debug.print("  WARN {s} {s}: {s}\n", .{ entry.name, entry.version, notes.items });
        }
    }

    if (problems != 0) {
        std.debug.print("doctor: {d} problem(s)\n", .{problems});
        return 1;
    }
    std.debug.print("doctor: clean\n", .{});
    return 0;
}

fn moduleLoadAll(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    if (args.len != 2) {
        std.debug.print("Usage: zlang module loadall\n", .{});
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
        std.debug.print("Usage: zlang module load <file.so>\n", .{});
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
    var pkg = (try openPackage(args, alloc, io, "zlang module dryrun <file.zlx>")) orelse return 1;
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
        std.debug.print("Usage: zlang module abi\n", .{});
        return 1;
    }
    std.debug.print("host_api_version: {d}\n", .{abi.api_version});
    std.debug.print("supported_range: {d}-{d}\n", .{ abi.api_min_supported, abi.api_max_supported });
    std.debug.print("probe_symbol: {s}\n", .{abi.probe_symbol});
    std.debug.print("init_symbol: {s}\n", .{abi.init_symbol});
    std.debug.print("sdk: run 'zlang module sdk' to materialize headers and the Zig SDK\n", .{});
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
        std.debug.print("Usage: zlang module install <file.zlx>\n", .{});
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

    const sidecar_installed = installSidecar(alloc, io, store, args[2], parsed.name, pkg) catch |err| {
        std.debug.print("zlx: could not install plugin sidecar: {}\n", .{err});
        return 1;
    };

    const modules_installed = installModuleFiles(alloc, io, store, args[2], parsed, pkg) catch |err| {
        std.debug.print("zlx: could not install plugin modules: {}\n", .{err});
        return 1;
    };
    _ = modules_installed;

    const native_libs_installed = installNativeLibs(alloc, io, store, args[2], parsed, pkg) catch |err| {
        std.debug.print("zlx: could not install native libraries: {}\n", .{err});
        return 1;
    };
    _ = native_libs_installed;

    const loaded_index = index_mod.load(alloc, io, store) catch |err| {
        std.debug.print("zlx: could not read module index: {}\n", .{err});
        return 1;
    };
    defer loaded_index.deinit(alloc);
    const manifest_hash = index_mod.hashHex(alloc, package) catch null;
    defer if (manifest_hash) |h| alloc.free(h);
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
                .manifest_sha256 = manifest_hash,
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
            .manifest_sha256 = manifest_hash,
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

fn packModule(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    var src_dir: ?[]const u8 = null;
    var out_path: ?[]const u8 = null;
    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "-o")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("zlx: -o requires a path\n", .{});
                return 1;
            }
            out_path = args[i];
        } else if (src_dir == null) {
            src_dir = args[i];
        }
    }
    const dir = src_dir orelse {
        std.debug.print("Usage: zlang module pack <dir> -o <out.zlx>\n", .{});
        return 1;
    };
    const out = out_path orelse {
        std.debug.print("Usage: zlang module pack <dir> -o <out.zlx>\n", .{});
        return 1;
    };

    const manifest_path = try std.fs.path.join(alloc, &.{ dir, "manifest.zon" });
    defer alloc.free(manifest_path);
    const manifest_bytes = std.Io.Dir.cwd().readFileAlloc(io, manifest_path, alloc, .limited(1024 * 1024)) catch |err| {
        std.debug.print("zlx: could not read {s}: {}\n", .{ manifest_path, err });
        return 1;
    };
    defer alloc.free(manifest_bytes);

    const parsed = manifest.parse(alloc, manifest_bytes) catch |err| {
        std.debug.print("zlx: invalid manifest in {s}: {}\n", .{ manifest_path, err });
        return 1;
    };
    defer manifest.free(alloc, parsed);

    var entries: std.ArrayList(package_mod.ArchiveEntry) = .empty;
    defer entries.deinit(alloc);
    var owned_bufs: std.ArrayList([]u8) = .empty;
    defer {
        for (owned_bufs.items) |b| alloc.free(b);
        owned_bufs.deinit(alloc);
    }

    try entries.append(alloc, .{ .path = "manifest.zon", .data = manifest_bytes });

    if (parsed.entry) |entry_path| {
        const full = try std.fs.path.join(alloc, &.{ dir, entry_path });
        defer alloc.free(full);
        const bytes = std.Io.Dir.cwd().readFileAlloc(io, full, alloc, .limited(64 * 1024 * 1024)) catch |err| {
            std.debug.print("zlx: could not read plugin entry {s}: {}\n", .{ full, err });
            return 1;
        };
        try owned_bufs.append(alloc, bytes);
        try entries.append(alloc, .{ .path = "plugin.so", .data = bytes });
    }

    for (parsed.modules) |module| {
        const full = try std.fs.path.join(alloc, &.{ dir, module.path });
        defer alloc.free(full);
        const bytes = std.Io.Dir.cwd().readFileAlloc(io, full, alloc, .limited(16 * 1024 * 1024)) catch |err| {
            std.debug.print("zlx: could not read module file {s}: {}\n", .{ full, err });
            return 1;
        };
        try owned_bufs.append(alloc, bytes);
        try entries.append(alloc, .{ .path = module.path, .data = bytes });
    }

    for (parsed.native_libs) |native_lib| {
        const full = try std.fs.path.join(alloc, &.{ dir, native_lib });
        defer alloc.free(full);
        const bytes = std.Io.Dir.cwd().readFileAlloc(io, full, alloc, .limited(64 * 1024 * 1024)) catch |err| {
            std.debug.print("zlx: could not read native library {s}: {}\n", .{ full, err });
            return 1;
        };
        try owned_bufs.append(alloc, bytes);
        try entries.append(alloc, .{ .path = native_lib, .data = bytes });
    }

    const archive = package_mod.writeArchive(alloc, entries.items) catch |err| {
        std.debug.print("zlx: could not build archive: {}\n", .{err});
        return 1;
    };
    defer alloc.free(archive);

    writeFileBytes(io, out, archive) catch |err| {
        std.debug.print("zlx: could not write {s}: {}\n", .{ out, err });
        return 1;
    };

    std.debug.print("Packed {d} entry/entries into {s}\n", .{ entries.items.len, out });
    return 0;
}

fn sdkDir(alloc: std.mem.Allocator) ![]u8 {
    const home = std.c.getenv("HOME") orelse return error.HomeNotSet;
    return std.fmt.allocPrint(alloc, "{s}/.zlang/sdk", .{std.mem.span(home)});
}

fn writeSdkInto(dir: []const u8, alloc: std.mem.Allocator, io: std.Io) !void {
    try std.Io.Dir.cwd().createDirPath(io, dir);
    for (sdk_embed.files) |file| {
        const path = try std.fs.path.join(alloc, &.{ dir, file.name });
        defer alloc.free(path);
        try writeFileBytes(io, path, file.bytes);
    }
}

fn ensureSdk(alloc: std.mem.Allocator, io: std.Io) ![]u8 {
    const dir = try sdkDir(alloc);
    errdefer alloc.free(dir);
    try writeSdkInto(dir, alloc, io);
    return dir;
}

fn sdkCommand(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    var want: enum { dir, zig, include } = .dir;
    var target: ?[]const u8 = null;
    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        const a = args[i];
        if (std.mem.eql(u8, a, "--zig")) {
            want = .zig;
        } else if (std.mem.eql(u8, a, "--include")) {
            want = .include;
        } else if (std.mem.eql(u8, a, "--dir")) {
            want = .dir;
        } else if (target == null) {
            target = a;
        }
    }

    const dir = if (target) |t| blk: {
        try writeSdkInto(t, alloc, io);
        break :blk try alloc.dupe(u8, t);
    } else try ensureSdk(alloc, io);
    defer alloc.free(dir);

    switch (want) {
        .dir, .include => try printStdoutLine(io, dir),
        .zig => {
            const p = try std.fs.path.join(alloc, &.{ dir, "sdk.zig" });
            defer alloc.free(p);
            try printStdoutLine(io, p);
        },
    }
    return 0;
}

fn printStdoutLine(io: std.Io, text: []const u8) !void {
    var buffer: [4096]u8 = undefined;
    var writer = std.Io.File.stdout().writer(io, &buffer);
    try writer.interface.writeAll(text);
    try writer.interface.writeAll("\n");
    try writer.interface.flush();
}

fn buildModule(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    var src_dir: ?[]const u8 = null;
    var out_path: ?[]const u8 = null;
    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "-o")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("zlx: -o requires a path\n", .{});
                return 1;
            }
            out_path = args[i];
        } else if (src_dir == null) {
            src_dir = args[i];
        }
    }
    const dir = src_dir orelse {
        std.debug.print("Usage: zlang module build <dir> [-o out.zlx]\n", .{});
        return 1;
    };

    const manifest_path = try std.fs.path.join(alloc, &.{ dir, "manifest.zon" });
    defer alloc.free(manifest_path);
    const manifest_bytes = std.Io.Dir.cwd().readFileAlloc(io, manifest_path, alloc, .limited(1024 * 1024)) catch |err| {
        std.debug.print("zlx: could not read {s}: {}\n", .{ manifest_path, err });
        return 1;
    };
    defer alloc.free(manifest_bytes);
    const parsed = manifest.parse(alloc, manifest_bytes) catch |err| {
        std.debug.print("zlx: invalid manifest in {s}: {}\n", .{ manifest_path, err });
        return 1;
    };
    defer manifest.free(alloc, parsed);

    const sdk_dir = try ensureSdk(alloc, io);
    defer alloc.free(sdk_dir);

    const entry = parsed.entry orelse "plugin.so";
    const out_so = try std.fs.path.join(alloc, &.{ dir, entry });
    defer alloc.free(out_so);
    if (std.fs.path.dirname(out_so)) |parent| try std.Io.Dir.cwd().createDirPath(io, parent);

    const zig_src = try std.fs.path.join(alloc, &.{ dir, "src", "plugin.zig" });
    defer alloc.free(zig_src);
    const c_src = try std.fs.path.join(alloc, &.{ dir, "src", "plugin.c" });
    defer alloc.free(c_src);

    const has_zig = blk: {
        std.Io.Dir.cwd().access(io, zig_src, .{}) catch break :blk false;
        break :blk true;
    };
    const has_c = blk: {
        std.Io.Dir.cwd().access(io, c_src, .{}) catch break :blk false;
        break :blk true;
    };

    if (has_zig) {
        const sdk_zig_path = try std.fs.path.join(alloc, &.{ sdk_dir, "sdk.zig" });
        defer alloc.free(sdk_zig_path);
        const mroot = try std.fmt.allocPrint(alloc, "-Mroot={s}", .{zig_src});
        defer alloc.free(mroot);
        const mzlx = try std.fmt.allocPrint(alloc, "-Mzlx={s}", .{sdk_zig_path});
        defer alloc.free(mzlx);
        const emit = try std.fmt.allocPrint(alloc, "-femit-bin={s}", .{out_so});
        defer alloc.free(emit);
        runCommand(io, &.{
            "zig", "build-lib", "-dynamic", "-OReleaseSafe", "-fPIC",
            "-lc", "--dep",     "zlx",      mroot,           mzlx,
            emit,
        }, null) catch |err| {
            std.debug.print("zlx: zig build-lib failed: {}\n", .{err});
            return 1;
        };
    } else if (has_c) {
        const include = try std.fmt.allocPrint(alloc, "-I{s}", .{sdk_dir});
        defer alloc.free(include);
        runCommand(io, &.{
            "clang", "-shared", "-fPIC", "-O2", include, "-o", out_so, c_src,
        }, null) catch |err| {
            std.debug.print("zlx: clang failed: {}\n", .{err});
            return 1;
        };
    } else {
        std.debug.print("zlx: no src/plugin.zig or src/plugin.c found in {s}\n", .{dir});
        return 1;
    }

    const default_zlx = try std.fmt.allocPrint(alloc, "{s}.zlx", .{parsed.name});
    defer alloc.free(default_zlx);
    const out_zlx = out_path orelse try std.fs.path.join(alloc, &.{ dir, default_zlx });
    const out_zlx_owned = out_path == null;
    defer if (out_zlx_owned) alloc.free(out_zlx);

    const module_z = try alloc.dupeZ(u8, "module");
    defer alloc.free(module_z);
    const pack_z = try alloc.dupeZ(u8, "pack");
    defer alloc.free(pack_z);
    const dir_z = try alloc.dupeZ(u8, dir);
    defer alloc.free(dir_z);
    const dash_o_z = try alloc.dupeZ(u8, "-o");
    defer alloc.free(dash_o_z);
    const out_zlx_z = try alloc.dupeZ(u8, out_zlx);
    defer alloc.free(out_zlx_z);
    const pack_args = [_][:0]u8{ module_z, pack_z, dir_z, dash_o_z, out_zlx_z };
    return try packModule(&pack_args, alloc, io);
}

fn devModule(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    if (args.len != 3) {
        std.debug.print("Usage: zlang module dev <dir>\n", .{});
        return 1;
    }
    const dir = args[2];

    std.debug.print("zlx: watching {s}\n", .{dir});
    _ = try buildAndInstallDevModule(dir, alloc, io);

    dev_stop_requested = false;
    const sigint_action: std.posix.Sigaction = .{
        .handler = .{ .handler = handleDevSigint },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    std.posix.sigaction(.INT, &sigint_action, null);

    const fd = inotify_init1(IN_NONBLOCK);
    if (fd < 0) {
        std.debug.print("zlx: inotify_init1 failed\n", .{});
        return 1;
    }
    defer _ = close(fd);

    if ((try addWatchRecursive(fd, dir, dev_watch_mask, alloc, io)) == 0) {
        std.debug.print("zlx: inotify_add_watch failed under {s}\n", .{dir});
        return 1;
    }

    var buf: [8192]u8 align(@alignOf(usize)) = undefined;
    while (!dev_stop_requested) {
        const n = std.posix.read(fd, &buf) catch |err| {
            if (err == error.WouldBlock) {
                _ = usleep(100_000);
                continue;
            }
            std.debug.print("zlx: inotify read failed: {}\n", .{err});
            return 1;
        };
        if (n == 0) continue;
        if (!hasRelevantDevEvent(buf[0..n])) continue;
        _ = usleep(150_000);
        drainInotify(fd);
        _ = buildAndInstallDevModule(dir, alloc, io) catch |err| {
            std.debug.print("zlx: dev rebuild failed: {}\n", .{err});
            continue;
        };
        drainInotify(fd);
    }
    std.debug.print("zlx: stopped watching {s}\n", .{dir});
    return 0;
}

fn drainInotify(fd: c_int) void {
    var buf: [8192]u8 align(@alignOf(usize)) = undefined;
    while (true) {
        const n = std.posix.read(fd, &buf) catch return;
        if (n == 0) return;
    }
}

fn hasRelevantDevEvent(events: []const u8) bool {
    var offset: usize = 0;
    while (offset + 16 <= events.len) {
        const mask = std.mem.readInt(u32, events[offset + 4 ..][0..4], .little);
        const name_len = std.mem.readInt(u32, events[offset + 12 ..][0..4], .little);
        const name_start = offset + 16;
        const name_end = @min(name_start + @as(usize, @intCast(name_len)), events.len);
        const raw_name = events[name_start..name_end];
        const nul = std.mem.indexOfScalar(u8, raw_name, 0) orelse raw_name.len;
        const name = raw_name[0..nul];
        if ((mask & IN_ISDIR) == 0 and !isDevBuildOutput(name)) return true;
        offset = name_start + @as(usize, @intCast(name_len));
    }
    return false;
}

fn isDevBuildOutput(name: []const u8) bool {
    return std.mem.endsWith(u8, name, ".so") or
        std.mem.endsWith(u8, name, ".a") or
        std.mem.endsWith(u8, name, ".o") or
        std.mem.endsWith(u8, name, ".zlx");
}

fn addWatchRecursive(fd: c_int, dir: []const u8, mask: u32, alloc: std.mem.Allocator, io: std.Io) !usize {
    const dir_z = try alloc.dupeZ(u8, dir);
    defer alloc.free(dir_z);
    var count: usize = if (inotify_add_watch(fd, dir_z.ptr, mask) >= 0) 1 else 0;

    var opened = std.Io.Dir.cwd().openDir(io, dir, .{ .iterate = true }) catch return count;
    defer opened.close(io);
    var it = opened.iterate();
    while (try it.next(io)) |entry| {
        if (entry.kind != .directory) continue;
        if (std.mem.eql(u8, entry.name, ".git") or std.mem.eql(u8, entry.name, "build")) continue;
        const child = try std.fs.path.join(alloc, &.{ dir, entry.name });
        defer alloc.free(child);
        count += try addWatchRecursive(fd, child, mask, alloc, io);
    }
    return count;
}

fn buildAndInstallDevModule(dir: []const u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    const start = std.Io.Timestamp.now(io, .real).toNanoseconds();
    const manifest_path = try std.fs.path.join(alloc, &.{ dir, "manifest.zon" });
    defer alloc.free(manifest_path);
    const manifest_bytes = try std.Io.Dir.cwd().readFileAlloc(io, manifest_path, alloc, .limited(1024 * 1024));
    defer alloc.free(manifest_bytes);
    const parsed = try manifest.parse(alloc, manifest_bytes);
    defer manifest.free(alloc, parsed);

    const build_sh = try std.fs.path.join(alloc, &.{ dir, "build.sh" });
    defer alloc.free(build_sh);
    const has_build_sh = blk: {
        std.Io.Dir.cwd().access(io, build_sh, .{}) catch break :blk false;
        break :blk true;
    };

    if (has_build_sh) {
        try runCommand(io, &.{ "sh", "build.sh" }, dir);
    } else {
        const entry = parsed.entry orelse "plugin.so";
        const out_so = try std.fs.path.join(alloc, &.{ dir, entry });
        defer alloc.free(out_so);
        if (std.fs.path.dirname(out_so)) |parent| try std.Io.Dir.cwd().createDirPath(io, parent);
        const src_plugin_c = try std.fs.path.join(alloc, &.{ dir, "src/plugin.c" });
        defer alloc.free(src_plugin_c);
        const fallback_plugin_c = try std.fs.path.join(alloc, &.{ dir, "plugin.c" });
        defer alloc.free(fallback_plugin_c);
        const plugin_c = blk: {
            std.Io.Dir.cwd().access(io, src_plugin_c, .{}) catch break :blk fallback_plugin_c;
            break :blk src_plugin_c;
        };
        const sdk_dir = try ensureSdk(alloc, io);
        defer alloc.free(sdk_dir);
        const include = try std.fmt.allocPrint(alloc, "-I{s}", .{sdk_dir});
        defer alloc.free(include);
        try runCommand(io, &.{ "clang", "-shared", "-fPIC", include, "-o", out_so, plugin_c }, null);
    }

    const zlx_name = try std.fmt.allocPrint(alloc, "{s}.zlx", .{parsed.name});
    defer alloc.free(zlx_name);
    const out_zlx = try std.fs.path.join(alloc, &.{ dir, zlx_name });
    defer alloc.free(out_zlx);

    if (!has_build_sh) {
        const module_z = try alloc.dupeZ(u8, "module");
        defer alloc.free(module_z);
        const pack_z = try alloc.dupeZ(u8, "pack");
        defer alloc.free(pack_z);
        const dash_o_z = try alloc.dupeZ(u8, "-o");
        defer alloc.free(dash_o_z);
        const out_zlx_z = try alloc.dupeZ(u8, out_zlx);
        defer alloc.free(out_zlx_z);
        const dir_z = try alloc.dupeZ(u8, dir);
        defer alloc.free(dir_z);
        const pack_args = [_][:0]u8{ module_z, pack_z, dir_z, dash_o_z, out_zlx_z };
        if (try packModule(&pack_args, alloc, io) != 0) return 1;
    }

    const module_z = try alloc.dupeZ(u8, "module");
    defer alloc.free(module_z);
    const install_cmd_z = try alloc.dupeZ(u8, "install");
    defer alloc.free(install_cmd_z);
    const install_z = try alloc.dupeZ(u8, out_zlx);
    defer alloc.free(install_z);
    const install_args = [_][:0]u8{ module_z, install_cmd_z, install_z };
    const rc = try install(&install_args, alloc, io);
    const elapsed_ms = @as(f64, @floatFromInt(std.Io.Timestamp.now(io, .real).toNanoseconds() - start)) / 1_000_000.0;
    std.debug.print("zlx: dev rebuild installed {s} in {d:.2} ms\n", .{ out_zlx, elapsed_ms });
    return rc;
}

fn runCommand(io: std.Io, argv: []const []const u8, cwd: ?[]const u8) !void {
    const child_cwd: std.process.Child.Cwd = if (cwd) |path| .{ .path = path } else .inherit;
    var child = try std.process.spawn(io, .{ .argv = argv, .cwd = child_cwd });
    const term = try child.wait(io);
    switch (term) {
        .exited => |code| if (code != 0) return error.CommandFailed,
        else => return error.CommandFailed,
    }
}

fn writeFileBytes(io: std.Io, path: []const u8, data: []const u8) !void {
    if (std.fs.path.dirname(path)) |parent| {
        try std.Io.Dir.cwd().createDirPath(io, parent);
    }
    var out = try std.Io.Dir.cwd().createFile(io, path, .{ .truncate = true });
    defer out.close(io);
    var buf: [4096]u8 = undefined;
    var writer = out.writer(io, &buf);
    try writer.interface.writeAll(data);
    try writer.interface.flush();
}

fn installSidecar(alloc: std.mem.Allocator, io: std.Io, store: store_mod.Store, source_zlx: []const u8, name: []const u8, pkg: package_mod.Package) !bool {
    if (pkg.layout == .archive) {
        const entry = pkg.findEntry("plugin.so") orelse return false;
        const dest = try store.pluginPath(alloc, name);
        defer alloc.free(dest);
        try writeFileBytes(io, dest, entry.data);
        return true;
    }
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
    try writeFileBytes(io, dest, bytes);
    return true;
}

fn installModuleFiles(alloc: std.mem.Allocator, io: std.Io, store: store_mod.Store, source_zlx: []const u8, parsed: manifest.Manifest, pkg: package_mod.Package) !usize {
    if (parsed.modules.len == 0) return 0;
    const modules_root = try store.pluginModulesDir(alloc, parsed.name);
    defer alloc.free(modules_root);

    var copied: usize = 0;
    for (parsed.modules) |module| {
        const dest_path = try std.fs.path.join(alloc, &.{ modules_root, module.path });
        defer alloc.free(dest_path);

        if (pkg.layout == .archive) {
            const entry = pkg.findEntry(module.path) orelse continue;
            try writeFileBytes(io, dest_path, entry.data);
            copied += 1;
            continue;
        }

        const src_dir = std.fs.path.dirname(source_zlx) orelse ".";
        const src_path = try std.fs.path.join(alloc, &.{ src_dir, module.path });
        defer alloc.free(src_path);
        const bytes = std.Io.Dir.cwd().readFileAlloc(io, src_path, alloc, .limited(16 * 1024 * 1024)) catch |err| switch (err) {
            error.FileNotFound => continue,
            else => return err,
        };
        defer alloc.free(bytes);
        try writeFileBytes(io, dest_path, bytes);
        copied += 1;
    }
    return copied;
}

fn installNativeLibs(alloc: std.mem.Allocator, io: std.Io, store: store_mod.Store, source_zlx: []const u8, parsed: manifest.Manifest, pkg: package_mod.Package) !usize {
    if (parsed.native_libs.len == 0) return 0;
    const native_root = try store.pluginNativeDir(alloc, parsed.name);
    defer alloc.free(native_root);

    var copied: usize = 0;
    for (parsed.native_libs) |native_lib| {
        const dest_path = try std.fs.path.join(alloc, &.{ native_root, native_lib });
        defer alloc.free(dest_path);

        if (pkg.layout == .archive) {
            const entry = pkg.findEntry(native_lib) orelse continue;
            try writeFileBytes(io, dest_path, entry.data);
            copied += 1;
            continue;
        }

        const src_dir = std.fs.path.dirname(source_zlx) orelse ".";
        const src_path = try std.fs.path.join(alloc, &.{ src_dir, native_lib });
        defer alloc.free(src_path);
        const bytes = std.Io.Dir.cwd().readFileAlloc(io, src_path, alloc, .limited(64 * 1024 * 1024)) catch |err| switch (err) {
            error.FileNotFound => continue,
            else => return err,
        };
        defer alloc.free(bytes);
        try writeFileBytes(io, dest_path, bytes);
        copied += 1;
    }
    return copied;
}

fn deleteModulesDir(alloc: std.mem.Allocator, io: std.Io, store: store_mod.Store, name: []const u8) !void {
    const dir = try store.pluginModulesDir(alloc, name);
    defer alloc.free(dir);
    std.Io.Dir.cwd().deleteTree(io, dir) catch |err| return err;
}

fn deleteNativeDir(alloc: std.mem.Allocator, io: std.Io, store: store_mod.Store, name: []const u8) !void {
    const dir = try store.pluginNativeDir(alloc, name);
    defer alloc.free(dir);
    std.Io.Dir.cwd().deleteTree(io, dir) catch |err| return err;
}

fn listModules(args: []const [:0]u8, alloc: std.mem.Allocator, io: std.Io) !u8 {
    if (args.len != 2) {
        std.debug.print("Usage: zlang module list\n", .{});
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
        std.debug.print("Usage: zlang module load-order\n", .{});
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
        std.debug.print("Usage: zlang module delete <name>\n", .{});
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
    deleteModulesDir(alloc, io, store, args[2]) catch |err| {
        std.debug.print("zlx: could not delete plugin modules dir: {}\n", .{err});
        return 1;
    };
    deleteNativeDir(alloc, io, store, args[2]) catch |err| {
        std.debug.print("zlx: could not delete plugin native dir: {}\n", .{err});
        return 1;
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
    var pkg = (try openPackage(args, alloc, io, "zlang module validate <file.zlx>")) orelse return 1;
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
    var pkg = (try openPackage(args, alloc, io, "zlang module info <file.zlx>")) orelse return 1;
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
