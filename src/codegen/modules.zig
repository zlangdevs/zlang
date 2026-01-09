const std = @import("std");
const utils = @import("utils.zig");

/// Module system for managing multi-file compilation and dependencies
/// Tracks module dependencies, function-to-module mappings, and access control
pub const ModuleManager = struct {
    allocator: std.mem.Allocator,
    
    /// Maps function names to their declaring module
    function_to_module: std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    
    /// Maps module names to their list of dependencies
    module_dependencies: std.HashMap([]const u8, std.ArrayList([]const u8), std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    
    /// Maps module names to their full file paths (for error reporting)
    module_paths: std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    
    /// Name of the current module being compiled
    current_module_name: []const u8,

    /// Initialize a new module manager
    pub fn init(allocator: std.mem.Allocator) ModuleManager {
        return .{
            .allocator = allocator,
            .function_to_module = std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .module_dependencies = std.HashMap([]const u8, std.ArrayList([]const u8), std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .module_paths = std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .current_module_name = "",
        };
    }

    /// Clean up all allocated resources
    pub fn deinit(self: *ModuleManager) void {
        // Free all dependency lists
        var deps_it = self.module_dependencies.iterator();
        while (deps_it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.module_dependencies.deinit();

        // Free all module paths
        var paths_it = self.module_paths.iterator();
        while (paths_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.module_paths.deinit();

        self.function_to_module.deinit();
    }

    /// Register a module with its dependencies
    /// 
    /// Args:
    ///   - module_name: Name of the module to register
    ///   - full_path: Full file path of the module (for error reporting)
    ///   - deps: List of module names this module depends on
    pub fn registerModule(self: *ModuleManager, module_name: []const u8, full_path: []const u8, deps: []const []const u8) !void {
        var list = std.ArrayList([]const u8){};
        for (deps) |d| {
            try list.append(self.allocator, utils.dupe(u8, self.allocator, d));
        }
        try self.module_dependencies.put(utils.dupe(u8, self.allocator, module_name), list);
        try self.module_paths.put(utils.dupe(u8, self.allocator, module_name), utils.dupe(u8, self.allocator, full_path));
    }

    /// Register which module a function belongs to
    /// 
    /// Args:
    ///   - func_name: Name of the function
    ///   - module_name: Name of the module that declares this function
    pub fn registerFunctionModule(self: *ModuleManager, func_name: []const u8, module_name: []const u8) !void {
        try self.function_to_module.put(utils.dupe(u8, self.allocator, func_name), utils.dupe(u8, self.allocator, module_name));
    }

    /// Set the current module context based on a function name
    /// Used during code generation to track which module is being compiled
    /// 
    /// Args:
    ///   - func_name: Name of the function to look up
    pub fn setCurrentModuleByFunction(self: *ModuleManager, func_name: []const u8) void {
        if (self.function_to_module.get(func_name)) |m| {
            self.current_module_name = m;
        } else {
            self.current_module_name = "";
        }
    }

    /// Check if one module can access another module
    /// A module can access itself and any of its declared dependencies
    /// 
    /// Args:
    ///   - from_module: Name of the module attempting access
    ///   - target_module: Name of the module being accessed
    /// 
    /// Returns: true if access is allowed, false otherwise
    pub fn canAccess(self: *ModuleManager, from_module: []const u8, target_module: []const u8) bool {
        // Empty module name or same module always has access
        if (from_module.len == 0 or std.mem.eql(u8, from_module, target_module)) {
            return true;
        }
        
        // Check if target_module is in the dependency list of from_module
        if (self.module_dependencies.get(from_module)) |list| {
            for (list.items) |dep| {
                if (std.mem.eql(u8, dep, target_module)) {
                    return true;
                }
            }
        }
        
        return false;
    }

    /// Get the file path for the current module (for error reporting)
    /// 
    /// Returns: The file path or "unknown" if not found
    pub fn getCurrentModulePath(self: *ModuleManager) []const u8 {
        if (self.module_paths.get(self.current_module_name)) |p| {
            return p;
        }
        return "unknown";
    }

    /// Get the file path for a specific module
    /// 
    /// Args:
    ///   - module_name: Name of the module
    /// 
    /// Returns: The file path or null if not found
    pub fn getModulePath(self: *ModuleManager, module_name: []const u8) ?[]const u8 {
        return self.module_paths.get(module_name);
    }
};
