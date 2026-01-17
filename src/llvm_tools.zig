const std = @import("std");
const utils = @import("codegen/utils.zig");

pub const LLVMTool = enum {
    llc,
    lli,
    opt,
    clang,
    pub fn baseName(self: LLVMTool) []const u8 {
        return switch (self) {
            .llc => "llc",
            .lli => "lli",
            .opt => "opt",
            .clang => "clang",
        };
    }
};
pub const ToolPath = struct {
    path: []const u8,
    owned: bool,
    pub fn deinit(self: ToolPath, allocator: std.mem.Allocator) void {
        if (self.owned) {
            allocator.free(self.path);
        }
    }
};

pub fn findLLVMTool(allocator: std.mem.Allocator, tool: LLVMTool) !?ToolPath {
    const base_name = tool.baseName();
    const version_suffixes = [_][]const u8{
        "-20", "-19", "-18", "-17", "-16", "-15", "-14", "",
    };
    for (version_suffixes) |suffix| {
        const tool_name = if (suffix.len > 0)
            try std.fmt.allocPrint(allocator, "{s}{s}", .{ base_name, suffix })
        else
            base_name;
        defer if (suffix.len > 0) allocator.free(tool_name);
        if (try isToolAvailable(allocator, tool_name)) {
            if (suffix.len > 0) {
                return ToolPath{
                    .path = utils.dupe(u8, allocator, tool_name),
                    .owned = true,
                };
            } else {
                return ToolPath{
                    .path = base_name,
                    .owned = false,
                };
            }
        }
    }

    return null;
}

fn isToolAvailable(allocator: std.mem.Allocator, tool_name: []const u8) !bool {
    var child = std.process.Child.init(&[_][]const u8{ tool_name, "--version" }, allocator);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Ignore;
    child.stderr_behavior = .Ignore;
    const term = child.spawnAndWait() catch return false;
    return switch (term) {
        .Exited => |code| code == 0,
        else => false,
    };
}

pub const ToolCache = struct {
    llc: ?ToolPath = null,
    lli: ?ToolPath = null,
    opt: ?ToolPath = null,
    clang: ?ToolPath = null,
    allocator: std.mem.Allocator,
    pub fn init(allocator: std.mem.Allocator) ToolCache {
        return .{
            .allocator = allocator,
        };
    }
    pub fn deinit(self: *ToolCache) void {
        if (self.llc) |tool| tool.deinit(self.allocator);
        if (self.lli) |tool| tool.deinit(self.allocator);
        if (self.opt) |tool| tool.deinit(self.allocator);
        if (self.clang) |tool| tool.deinit(self.allocator);
    }
    pub fn get(self: *ToolCache, tool: LLVMTool) !?[]const u8 {
        const cached = switch (tool) {
            .llc => &self.llc,
            .lli => &self.lli,
            .opt => &self.opt,
            .clang => &self.clang,
        };
        if (cached.*) |tool_path| {
            return tool_path.path;
        }
        if (try findLLVMTool(self.allocator, tool)) |tool_path| {
            cached.* = tool_path;
            return tool_path.path;
        }

        return null;
    }
};

pub fn getLLVMToolPath(allocator: std.mem.Allocator, tool: LLVMTool) !?[]const u8 {
    if (try findLLVMTool(allocator, tool)) |tool_path| {
        defer tool_path.deinit(allocator);
        if (tool_path.owned) {
            return utils.dupe(u8, allocator, tool_path.path);
        } else {
            return tool_path.path;
        }
    }
    return null;
}
