const std = @import("std");
const manifest_mod = @import("manifest.zig");

pub const Diagnostics = struct {
    conflicts: usize = 0,
    modules: usize = 0,
    syntax_blocks: usize = 0,
    cli_flags: usize = 0,
};

pub fn validateManifest(alloc: std.mem.Allocator, manifest: manifest_mod.Manifest) !Diagnostics {
    var seen_modules = std.StringHashMap(void).init(alloc);
    defer seen_modules.deinit();
    var seen_blocks = std.StringHashMap(void).init(alloc);
    defer seen_blocks.deinit();
    var seen_flags = std.StringHashMap(void).init(alloc);
    defer seen_flags.deinit();

    var diagnostics = Diagnostics{};
    for (manifest.modules) |module| {
        diagnostics.modules += 1;
        if (seen_modules.contains(module.name)) {
            diagnostics.conflicts += 1;
        } else {
            try seen_modules.put(module.name, {});
        }
    }
    for (manifest.syntax_blocks) |block| {
        diagnostics.syntax_blocks += 1;
        if (seen_blocks.contains(block.name)) {
            diagnostics.conflicts += 1;
        } else {
            try seen_blocks.put(block.name, {});
        }
    }
    for (manifest.cli_flags) |flag| {
        diagnostics.cli_flags += 1;
        if (seen_flags.contains(flag.name)) {
            diagnostics.conflicts += 1;
        } else {
            try seen_flags.put(flag.name, {});
        }
    }
    return diagnostics;
}
