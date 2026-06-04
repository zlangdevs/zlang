const std = @import("std");
const llvm_tools = @import("llvm_tools.zig");

pub fn runWithLLI(allocator: std.mem.Allocator, ll_file: []const u8, program_args: []const []const u8) !u8 {
    const lli_tool = try llvm_tools.getLLVMToolPath(allocator, .lli);
    if (lli_tool == null) {
        std.debug.print("Error: lli (LLVM interpreter) not found.\n", .{});
        std.debug.print("Tried: lli-20, lli-19, lli-18, ..., lli\n", .{});
        std.debug.print("Please install LLVM tools.\n", .{});
        return error.LLINotFound;
    }
    defer if (lli_tool) |tool| {
        if (!std.mem.eql(u8, tool, "lli")) {
            allocator.free(tool);
        }
    };

    var argv: std.ArrayList([]const u8) = .empty;
    defer argv.deinit(allocator);

    try argv.append(allocator, lli_tool.?);
    try argv.append(allocator, ll_file);

    for (program_args) |arg| {
        try argv.append(allocator, arg);
    }

    var threaded: std.Io.Threaded = .init(allocator, .{});
    defer threaded.deinit();
    var child = try std.process.spawn(threaded.io(), .{ .argv = argv.items });
    const term = try child.wait(threaded.io());

    return switch (term) {
        .exited => |code| code,
        .signal => 1,
        .stopped => 1,
        .unknown => 1,
    };
}

pub fn checkLLIAvailable(allocator: std.mem.Allocator) bool {
    const lli_tool = llvm_tools.getLLVMToolPath(allocator, .lli) catch return false;
    defer if (lli_tool) |tool| {
        if (!std.mem.eql(u8, tool, "lli")) {
            allocator.free(tool);
        }
    };

    if (lli_tool == null) {
        return false;
    }

    var threaded: std.Io.Threaded = .init(allocator, .{});
    defer threaded.deinit();
    const run = std.process.run(allocator, threaded.io(), .{ .argv = &[_][]const u8{ lli_tool.?, "--version" } }) catch return false;
    defer allocator.free(run.stdout);
    defer allocator.free(run.stderr);

    return switch (run.term) {
        .exited => |code| code == 0,
        else => false,
    };
}
