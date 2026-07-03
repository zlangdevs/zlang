const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const consts = @import("consts.zig");
const codegen = @import("codegen/llvm.zig");
const llvm_tools = @import("llvm_tools.zig");

const allocator = std.heap.page_allocator;

pub const CompilationStats = struct {
    const LinkBackend = codegen.CodeGenerator.BackendTiming.LinkBackend;

    parse_time_ns: u64,
    parse_load_time_ns: u64,
    parse_std_time_ns: u64,
    parse_resolve_time_ns: u64,
    parse_rewrite_time_ns: u64,
    parse_merge_time_ns: u64,
    codegen_time_ns: u64,
    backend_time_ns: u64,
    backend_prepare_time_ns: u64,
    backend_split_time_ns: u64,
    backend_wall_time_ns: u64,
    backend_cpu_sum_time_ns: u64,
    backend_units: usize,
    backend_unit_max_time_ns: u64,
    backend_unit_min_time_ns: u64,
    link_time_ns: u64,
    total_time_ns: u64,
    lines_of_code: usize,
    memory_peak_kb: usize,
    llvm_link_version_major: u32,
    llc_version_major: i16,
    opt_version_major: i16,
    clang_version_major: i16,
    lld_version_major: i16,
    link_backend: LinkBackend,
};

pub fn printStats(stats: CompilationStats) void {
    const parse_ms = @as(f64, @floatFromInt(stats.parse_time_ns)) / 1_000_000.0;
    const codegen_ms = @as(f64, @floatFromInt(stats.codegen_time_ns)) / 1_000_000.0;
    const backend_ms = @as(f64, @floatFromInt(stats.backend_time_ns)) / 1_000_000.0;
    const backend_prepare_ms = @as(f64, @floatFromInt(stats.backend_prepare_time_ns)) / 1_000_000.0;
    const backend_split_ms = @as(f64, @floatFromInt(stats.backend_split_time_ns)) / 1_000_000.0;
    const backend_wall_ms = @as(f64, @floatFromInt(stats.backend_wall_time_ns)) / 1_000_000.0;
    const backend_cpu_sum_ms = @as(f64, @floatFromInt(stats.backend_cpu_sum_time_ns)) / 1_000_000.0;
    const link_ms = @as(f64, @floatFromInt(stats.link_time_ns)) / 1_000_000.0;
    const total_ms = @as(f64, @floatFromInt(stats.total_time_ns)) / 1_000_000.0;

    const link_backend_text = switch (stats.link_backend) {
        .unknown => "unknown",
        .zig_cc => "zig cc",
        .lld => "ld.lld (pure LLVM toolkit)",
        .clang => "clang driver",
    };

    std.debug.print("Compilation Statistics:\n", .{});
    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});
    std.debug.print("  Parse time:      {d:.2} ms\n", .{parse_ms});
    std.debug.print("  Parse breakdown: load={d:.2} std={d:.2} resolve={d:.2} rewrite={d:.2} merge={d:.2} ms\n", .{
        @as(f64, @floatFromInt(stats.parse_load_time_ns)) / 1_000_000.0,
        @as(f64, @floatFromInt(stats.parse_std_time_ns)) / 1_000_000.0,
        @as(f64, @floatFromInt(stats.parse_resolve_time_ns)) / 1_000_000.0,
        @as(f64, @floatFromInt(stats.parse_rewrite_time_ns)) / 1_000_000.0,
        @as(f64, @floatFromInt(stats.parse_merge_time_ns)) / 1_000_000.0,
    });
    std.debug.print("  Codegen time:    {d:.2} ms\n", .{codegen_ms});
    std.debug.print("  Backend time:    {d:.2} ms\n", .{backend_ms});
    std.debug.print("  Backend prepare: {d:.2} ms\n", .{backend_prepare_ms});
    std.debug.print("  Backend split:   {d:.2} ms\n", .{backend_split_ms});
    std.debug.print("  Backend wall:    {d:.2} ms\n", .{backend_wall_ms});
    std.debug.print("  Backend cpu-sum: {d:.2} ms\n", .{backend_cpu_sum_ms});
    std.debug.print("  Backend units:   {d} (unit min/max: {d:.2}/{d:.2} ms)\n", .{
        stats.backend_units,
        @as(f64, @floatFromInt(stats.backend_unit_min_time_ns)) / 1_000_000.0,
        @as(f64, @floatFromInt(stats.backend_unit_max_time_ns)) / 1_000_000.0,
    });
    std.debug.print("  Link time:       {d:.2} ms\n", .{link_ms});
    std.debug.print("  Link backend:    {s}\n", .{link_backend_text});
    std.debug.print("  LLVM linked:     {d}\n", .{stats.llvm_link_version_major});
    std.debug.print("  LLVM versions:   llc={d} opt={d} lld={d} clang={d}\n", .{ stats.llc_version_major, stats.opt_version_major, stats.lld_version_major, stats.clang_version_major });
    std.debug.print("  Total time:      {d:.2} ms\n", .{total_ms});
    std.debug.print("  Lines of code:   {d}\n", .{stats.lines_of_code});
    std.debug.print("  Memory peak:     {d} KB\n", .{stats.memory_peak_kb});
    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});
}

fn parseProcStatusKbValue(content: []const u8, key: []const u8) ?usize {
    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        if (!std.mem.startsWith(u8, line, key)) continue;
        const rest = std.mem.trimStart(u8, line[key.len..], " \t:");
        var i: usize = 0;
        while (i < rest.len and (rest[i] < '0' or rest[i] > '9')) : (i += 1) {}
        var j = i;
        while (j < rest.len and rest[j] >= '0' and rest[j] <= '9') : (j += 1) {}
        if (j <= i) return null;
        return std.fmt.parseInt(usize, rest[i..j], 10) catch null;
    }
    return null;
}

pub fn readMemoryPeakKb() usize {
    if (builtin.os.tag != .linux) return 0;
    const file = std.c.fopen("/proc/self/status", "rb") orelse return 0;
    defer _ = std.c.fclose(file);

    var buffer: [16384]u8 = undefined;
    const bytes_read = std.c.fread(&buffer, 1, buffer.len, file);
    const content = buffer[0..bytes_read];

    return parseProcStatusKbValue(content, "VmHWM") orelse
        parseProcStatusKbValue(content, "VmPeak") orelse 0;
}

fn printToolVersion(alloc: std.mem.Allocator, label: []const u8, tool: llvm_tools.LLVMTool) void {
    const maybe_path = llvm_tools.getLLVMToolPath(alloc, tool) catch null;
    if (maybe_path) |path| {
        const major = llvm_tools.detectToolVersionMajor(alloc, path) catch null;
        if (major) |version| {
            std.debug.print("  {s}: {s} ({d})\n", .{ label, path, version });
        } else {
            std.debug.print("  {s}: {s}\n", .{ label, path });
        }
    } else {
        std.debug.print("  {s}: not found\n", .{label});
    }
}

pub fn printVersionInfo() void {
    std.debug.print("{s} {s}\n", .{ consts.NAME, consts.VERSION });
    std.debug.print("  Zig: {s}\n", .{@import("builtin").zig_version_string});
    if (build_options.llvm_version_major != 0) {
        std.debug.print("  LLVM linked: {d}\n", .{build_options.llvm_version_major});
    } else {
        std.debug.print("  LLVM linked: unknown\n", .{});
    }
    std.debug.print("LLVM tools:\n", .{});
    printToolVersion(allocator, "llc", .llc);
    printToolVersion(allocator, "opt", .opt);
    printToolVersion(allocator, "ld.lld", .ld_lld);
    printToolVersion(allocator, "clang", .clang);
    printToolVersion(allocator, "lli", .lli);
}
