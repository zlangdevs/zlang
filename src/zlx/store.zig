const std = @import("std");

pub const Store = struct {
    root: []const u8,

    pub fn init(alloc: std.mem.Allocator) !Store {
        const home = std.c.getenv("HOME") orelse return error.HomeNotSet;
        const root = try std.fmt.allocPrint(alloc, "{s}/.zlang/modules", .{std.mem.span(home)});
        return .{ .root = root };
    }

    pub fn deinit(self: Store, alloc: std.mem.Allocator) void {
        alloc.free(self.root);
    }

    pub fn ensure(self: Store, io: std.Io) !void {
        try std.Io.Dir.cwd().createDirPath(io, self.root);
    }

    pub fn modulePath(self: Store, alloc: std.mem.Allocator, name: []const u8) ![]u8 {
        return try std.fmt.allocPrint(alloc, "{s}/{s}.zlx", .{ self.root, name });
    }

    pub fn pluginPath(self: Store, alloc: std.mem.Allocator, name: []const u8) ![]u8 {
        return try std.fmt.allocPrint(alloc, "{s}/{s}.so", .{ self.root, name });
    }

    pub fn pluginModulesDir(self: Store, alloc: std.mem.Allocator, name: []const u8) ![]u8 {
        return try std.fmt.allocPrint(alloc, "{s}/{s}.modules", .{ self.root, name });
    }

    pub fn indexPath(self: Store, alloc: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(alloc, "{s}/index.zon", .{self.root});
    }
};
