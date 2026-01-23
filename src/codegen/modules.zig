const std = @import("std");
const utils = @import("utils.zig");

pub const ModuleManager = struct {
    allocator: std.mem.Allocator,
    function_to_module: std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    module_dependencies: std.HashMap([]const u8, std.ArrayList([]const u8), std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    module_paths: std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    current_module_name: []const u8,
    pub fn init(allocator: std.mem.Allocator) ModuleManager {
        return .{
            .allocator = allocator,
            .function_to_module = std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .module_dependencies = std.HashMap([]const u8, std.ArrayList([]const u8), std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .module_paths = std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .current_module_name = "",
        };
    }
    pub fn deinit(self: *ModuleManager) void {
        var deps_it = self.module_dependencies.iterator();
        while (deps_it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.module_dependencies.deinit();
        var paths_it = self.module_paths.iterator();
        while (paths_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.module_paths.deinit();
        self.function_to_module.deinit();
    }
    pub fn registerModule(self: *ModuleManager, module_name: []const u8, full_path: []const u8, deps: []const []const u8) !void {
        var list = std.ArrayList([]const u8){};
        for (deps) |d| {
            try list.append(self.allocator, utils.dupe(u8, self.allocator, d));
        }
        try self.module_dependencies.put(utils.dupe(u8, self.allocator, module_name), list);
        try self.module_paths.put(utils.dupe(u8, self.allocator, module_name), utils.dupe(u8, self.allocator, full_path));
    }
    pub fn registerFunctionModule(self: *ModuleManager, func_name: []const u8, module_name: []const u8) !void {
        try self.function_to_module.put(utils.dupe(u8, self.allocator, func_name), utils.dupe(u8, self.allocator, module_name));
    }
    pub fn setCurrentModuleByFunction(self: *ModuleManager, func_name: []const u8) void {
        if (self.function_to_module.get(func_name)) |m| {
            self.current_module_name = m;
        } else {
            self.current_module_name = "";
        }
    }
    pub fn canAccess(self: *ModuleManager, from_module: []const u8, target_module: []const u8) bool {
        if (from_module.len == 0 or std.mem.eql(u8, from_module, target_module)) {
            return true;
        }
        if (self.module_dependencies.get(from_module)) |list| {
            for (list.items) |dep| {
                if (std.mem.eql(u8, dep, target_module)) {
                    return true;
                }
            }
        }   
        return false;
    }
    pub fn getCurrentModulePath(self: *ModuleManager) []const u8 {
        if (self.module_paths.get(self.current_module_name)) |p| {
            return p;
        }
        return "unknown";
    }
    pub fn getModulePath(self: *ModuleManager, module_name: []const u8) ?[]const u8 {
        return self.module_paths.get(module_name);
    }
};
