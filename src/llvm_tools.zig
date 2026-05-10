const std = @import("std");
const build_options = @import("build_options");
const utils = @import("codegen/utils.zig");

pub const LLVMTool = enum {
    llc,
    lli,
    opt,
    llvm_split,
    clang,
    ld_lld,
    zig,
    pub fn baseName(self: LLVMTool) []const u8 {
        return switch (self) {
            .llc => "llc",
            .lli => "lli",
            .opt => "opt",
            .llvm_split => "llvm-split",
            .clang => "clang",
            .ld_lld => "ld.lld",
            .zig => "zig",
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

fn getEnvVarOwned(allocator: std.mem.Allocator, name: [:0]const u8) !?[]u8 {
    const value = std.c.getenv(name) orelse return null;
    return try allocator.dupe(u8, std.mem.span(value));
}

pub fn findLLVMTool(allocator: std.mem.Allocator, tool: LLVMTool) !?ToolPath {
    const base_name = tool.baseName();
    if (tool == .zig) {
        if (try isToolAvailable(allocator, base_name)) {
            return ToolPath{ .path = base_name, .owned = false };
        }
        return null;
    }

    if ((try getEnvVarOwned(allocator, "ZLANG_LLVM_BIN"))) |llvm_bin| {
        defer allocator.free(llvm_bin);
        if (llvm_bin.len != 0) {
            const full = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ llvm_bin, base_name });
            defer allocator.free(full);
            if (try isToolAvailable(allocator, full)) {
                return ToolPath{ .path = utils.dupe(u8, allocator, full), .owned = true };
            }
        }
    }

    if (build_options.llvm_version_major != 0) {
        if (try findVersionedTool(allocator, base_name, build_options.llvm_version_major)) |tool_path| {
            return tool_path;
        }
    }

    if (try isToolAvailable(allocator, base_name)) {
        return ToolPath{
            .path = base_name,
            .owned = false,
        };
    }

    var version: u32 = 40;
    while (version >= 14) : (version -= 1) {
        if (version == build_options.llvm_version_major) continue;
        if (try findVersionedTool(allocator, base_name, version)) |tool_path| {
            return tool_path;
        }
    }

    version = 40;
    while (version >= 14) : (version -= 1) {
        const suffixes = [_][]const u8{
            try std.fmt.allocPrint(allocator, "/usr/lib/llvm-{d}/bin", .{version}),
            try std.fmt.allocPrint(allocator, "/usr/lib64/llvm{d}/bin", .{version}),
            try std.fmt.allocPrint(allocator, "/opt/homebrew/opt/llvm@{d}/bin", .{version}),
        };
        defer allocator.free(suffixes[0]);
        defer allocator.free(suffixes[1]);
        defer allocator.free(suffixes[2]);
        for (suffixes) |prefix| {
            const full = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ prefix, base_name });
            defer allocator.free(full);
            if (try isToolAvailable(allocator, full)) {
                return ToolPath{ .path = utils.dupe(u8, allocator, full), .owned = true };
            }
        }
    }

    const generic_prefixes = [_][]const u8{"/opt/llvm/bin"};
    for (generic_prefixes) |prefix| {
        const full = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ prefix, base_name });
        defer allocator.free(full);
        if (try isToolAvailable(allocator, full)) {
            return ToolPath{ .path = utils.dupe(u8, allocator, full), .owned = true };
        }
    }

    return null;
}

fn findVersionedTool(allocator: std.mem.Allocator, base_name: []const u8, version: u32) !?ToolPath {
    const suffixes = [_][]const u8{
        try std.fmt.allocPrint(allocator, "-{d}", .{version}),
        try std.fmt.allocPrint(allocator, "{d}", .{version}),
    };
    defer allocator.free(suffixes[0]);
    defer allocator.free(suffixes[1]);
    for (suffixes) |suffix| {
        const tool_name = try std.fmt.allocPrint(allocator, "{s}{s}", .{ base_name, suffix });
        defer allocator.free(tool_name);
        if (try isToolAvailable(allocator, tool_name)) {
            return ToolPath{
                .path = utils.dupe(u8, allocator, tool_name),
                .owned = true,
            };
        }
    }
    return null;
}

fn parseTrailingVersion(name: []const u8) ?u8 {
    if (name.len == 0) return null;
    var end = name.len;
    while (end > 0 and name[end - 1] >= '0' and name[end - 1] <= '9') : (end -= 1) {}
    if (end == name.len) return null;
    return std.fmt.parseInt(u8, name[end..], 10) catch null;
}

fn parseFirstVersionToken(text: []const u8) ?u8 {
    var i: usize = 0;
    while (i < text.len) : (i += 1) {
        if (text[i] < '0' or text[i] > '9') continue;
        var j = i;
        while (j < text.len and text[j] >= '0' and text[j] <= '9') : (j += 1) {}
        if (j > i) return std.fmt.parseInt(u8, text[i..j], 10) catch null;
    }
    return null;
}

pub fn detectToolVersionMajor(allocator: std.mem.Allocator, command_path: []const u8) !?u8 {
    const base = std.fs.path.basename(command_path);
    if (parseTrailingVersion(base)) |v| return v;

    var threaded: std.Io.Threaded = .init(allocator, .{});
    defer threaded.deinit();
    const run = std.process.run(allocator, threaded.io(), .{
        .argv = &[_][]const u8{ command_path, "--version" },
    }) catch return null;
    defer allocator.free(run.stdout);
    defer allocator.free(run.stderr);

    if (parseFirstVersionToken(run.stdout)) |v| return v;
    if (parseFirstVersionToken(run.stderr)) |v| return v;
    return null;
}

fn isToolAvailable(allocator: std.mem.Allocator, tool_name: []const u8) !bool {
    const probe_args = [_][]const []const u8{
        &[_][]const u8{ tool_name, "--version" },
        &[_][]const u8{ tool_name, "version" },
    };

    for (probe_args) |argv| {
        var threaded: std.Io.Threaded = .init(allocator, .{});
        defer threaded.deinit();
        const run = std.process.run(allocator, threaded.io(), .{ .argv = argv }) catch continue;
        defer allocator.free(run.stdout);
        defer allocator.free(run.stderr);
        const ok = switch (run.term) {
            .exited => |code| code == 0,
            else => false,
        };
        if (ok) return true;
    }
    return false;
}

pub const ToolCache = struct {
    llc: ?ToolPath = null,
    lli: ?ToolPath = null,
    opt: ?ToolPath = null,
    llvm_split: ?ToolPath = null,
    clang: ?ToolPath = null,
    ld_lld: ?ToolPath = null,
    zig: ?ToolPath = null,
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
        if (self.llvm_split) |tool| tool.deinit(self.allocator);
        if (self.clang) |tool| tool.deinit(self.allocator);
        if (self.ld_lld) |tool| tool.deinit(self.allocator);
        if (self.zig) |tool| tool.deinit(self.allocator);
    }
    pub fn get(self: *ToolCache, tool: LLVMTool) !?[]const u8 {
        const cached = switch (tool) {
            .llc => &self.llc,
            .lli => &self.lli,
            .opt => &self.opt,
            .llvm_split => &self.llvm_split,
            .clang => &self.clang,
            .ld_lld => &self.ld_lld,
            .zig => &self.zig,
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
