const std = @import("std");
const errors = @import("../errors.zig");
const structs = @import("structs.zig");
const llvm = @import("llvm.zig");

pub fn pushScope(cg: *llvm.CodeGenerator) !void {
    const new_scope = std.HashMap([]const u8, structs.VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(cg.allocator);
    try cg.variable_scopes.append(new_scope);
}

pub fn popScope(cg: *llvm.CodeGenerator) void {
    if (cg.variable_scopes.items.len > 1) {
        var scope = cg.variable_scopes.pop().?;
        scope.deinit();
    }
}

pub fn clearCurrentFunctionScopes(cg: *llvm.CodeGenerator) void {
    while (cg.variable_scopes.items.len > 1) {
        var scope = cg.variable_scopes.pop().?;
        scope.deinit();
    }
}

pub fn getVariable(cg: *llvm.CodeGenerator, name: []const u8) ?structs.VariableInfo {
    var i = cg.variable_scopes.items.len;
    while (i > 0) {
        i -= 1;
        if (cg.variable_scopes.items[i].get(name)) |var_info| {
            return var_info;
        }
    }
    if (cg.variables.get(name)) |var_info| {
        return var_info;
    }
    return null;
}

pub fn putVariable(cg: *llvm.CodeGenerator, name: []const u8, var_info: structs.VariableInfo) !void {
    const current_scope = &cg.variable_scopes.items[cg.variable_scopes.items.len - 1];
    if (current_scope.contains(name)) {
        return errors.CodegenError.RedeclaredVariable;
    }

    try current_scope.put(name, var_info);
}

pub fn variableExistsInCurrentScope(cg: *llvm.CodeGenerator, name: []const u8) bool {
    if (cg.variable_scopes.items.len == 0) return false;
    const current_scope = &cg.variable_scopes.items[cg.variable_scopes.items.len - 1];
    return current_scope.contains(name);
}
