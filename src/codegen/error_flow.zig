const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
const structs = @import("structs.zig");
const functions = @import("functions.zig");
const llvm = @import("llvm.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

pub const SolicitCaptureBinding = struct {
    name: []const u8,
    type_ref: c.LLVMTypeRef,
    type_name: []const u8,
    mutated: bool,
    slot: c.LLVMValueRef,
};

pub fn ensureLastErrorGlobal(cg: *llvm.CodeGenerator) c.LLVMValueRef {
    if (cg.last_error_global) |g| return g;

    const name_z = utils.dupeZ(cg.allocator, "__zlang_last_error");
    defer cg.allocator.free(name_z);
    const i32_ty = c.LLVMInt32TypeInContext(cg.context);
    const global_var = c.LLVMAddGlobal(cg.module, i32_ty, name_z.ptr);
    c.LLVMSetInitializer(global_var, c.LLVMConstInt(i32_ty, 0, 0));
    c.LLVMSetLinkage(global_var, c.LLVMInternalLinkage);
    cg.last_error_global = global_var;
    return global_var;
}

pub fn ensureLastErrorModeGlobal(cg: *llvm.CodeGenerator) c.LLVMValueRef {
    if (cg.last_error_mode_global) |g| return g;

    const name_z = utils.dupeZ(cg.allocator, "__zlang_last_error_mode");
    defer cg.allocator.free(name_z);
    const i32_ty = c.LLVMInt32TypeInContext(cg.context);
    const global_var = c.LLVMAddGlobal(cg.module, i32_ty, name_z.ptr);
    c.LLVMSetInitializer(global_var, c.LLVMConstInt(i32_ty, 0, 0));
    c.LLVMSetLinkage(global_var, c.LLVMInternalLinkage);
    cg.last_error_mode_global = global_var;
    return global_var;
}

pub fn ensureSolicitCallbackGlobal(cg: *llvm.CodeGenerator) c.LLVMValueRef {
    if (cg.solicit_callback_global) |g| return g;

    const name_z = utils.dupeZ(cg.allocator, "__zlang_solicit_callback");
    defer cg.allocator.free(name_z);
    const i8_ptr = c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0);
    const global_var = c.LLVMAddGlobal(cg.module, i8_ptr, name_z.ptr);
    c.LLVMSetInitializer(global_var, c.LLVMConstNull(i8_ptr));
    c.LLVMSetLinkage(global_var, c.LLVMInternalLinkage);
    cg.solicit_callback_global = global_var;
    return global_var;
}

pub fn ensureSolicitVarSlot(cg: *llvm.CodeGenerator, function_name: []const u8, var_name: []const u8) errors.CodegenError!c.LLVMValueRef {
    const key = try std.fmt.allocPrint(cg.allocator, "{s}::{s}", .{ function_name, var_name });
    defer cg.allocator.free(key);

    if (cg.solicit_var_slots.get(key)) |slot| return slot;

    const global_name = try std.fmt.allocPrint(cg.allocator, "__zlang_solicit_var_{s}_{s}", .{ function_name, var_name });
    defer cg.allocator.free(global_name);
    const global_name_z = utils.dupeZ(cg.allocator, global_name);
    defer cg.allocator.free(global_name_z);

    const i8_ptr = c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0);
    const global_var = c.LLVMAddGlobal(cg.module, i8_ptr, global_name_z.ptr);
    c.LLVMSetInitializer(global_var, c.LLVMConstNull(i8_ptr));
    c.LLVMSetLinkage(global_var, c.LLVMInternalLinkage);

    try cg.solicit_var_slots.put(utils.dupe(u8, cg.allocator, key), global_var);
    return global_var;
}

pub fn ensureSolicitCaptureSlot(cg: *llvm.CodeGenerator, callback_id: usize, var_name: []const u8) errors.CodegenError!c.LLVMValueRef {
    const key = try std.fmt.allocPrint(cg.allocator, "{d}::{s}", .{ callback_id, var_name });
    defer cg.allocator.free(key);

    if (cg.solicit_capture_slots.get(key)) |slot| return slot;

    const global_name = try std.fmt.allocPrint(cg.allocator, "__zlang_solicit_capture_{d}_{s}", .{ callback_id, var_name });
    defer cg.allocator.free(global_name);
    const global_name_z = utils.dupeZ(cg.allocator, global_name);
    defer cg.allocator.free(global_name_z);

    const i8_ptr = c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0);
    const global_var = c.LLVMAddGlobal(cg.module, i8_ptr, global_name_z.ptr);
    c.LLVMSetInitializer(global_var, c.LLVMConstNull(i8_ptr));
    c.LLVMSetLinkage(global_var, c.LLVMInternalLinkage);

    try cg.solicit_capture_slots.put(utils.dupe(u8, cg.allocator, key), global_var);
    return global_var;
}

pub fn nextFreeErrorCode(cg: *llvm.CodeGenerator) i32 {
    var code = cg.next_error_code;
    while (true) : (code += 1) {
        var used = false;
        var it = cg.error_codes.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* == code) {
                used = true;
                break;
            }
        }
        if (!used) {
            cg.next_error_code = code + 1;
            return code;
        }
    }
}

pub fn registerBuiltinErrorGuard(cg: *llvm.CodeGenerator) !void {
    if (cg.error_codes.contains("ErrorGuard")) return;
    const code = nextFreeErrorCode(cg);
    try cg.error_codes.put("ErrorGuard", code);
}

pub fn registerErrorDeclaration(cg: *llvm.CodeGenerator, error_decl: ast.ErrorDecl) errors.CodegenError!void {
    if (cg.error_codes.contains(error_decl.name)) return;

    var code: i32 = 0;
    switch (error_decl.code_kind) {
        .explicit => {
            code = error_decl.explicit_code;
        },
        .alias => {
            const alias_name = error_decl.alias_name orelse return errors.CodegenError.TypeMismatch;
            code = cg.error_codes.get(alias_name) orelse return errors.CodegenError.TypeMismatch;
        },
        .auto => {
            code = nextFreeErrorCode(cg);
        },
    }
    try cg.error_codes.put(error_decl.name, code);
}

pub fn getScopedVariable(cg: *llvm.CodeGenerator, name: []const u8) ?structs.VariableInfo {
    var i = cg.variable_scopes.items.len;
    while (i > 0) {
        i -= 1;
        if (cg.variable_scopes.items[i].get(name)) |var_info| {
            return var_info;
        }
    }
    return null;
}

pub fn getOrCreateStackSaveIntrinsic(cg: *llvm.CodeGenerator) c.LLVMValueRef {
    var f = c.LLVMGetNamedFunction(cg.module, "llvm.stacksave.p0");
    if (f == null) {
        const i8_ptr = c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0);
        const fn_ty = c.LLVMFunctionType(i8_ptr, null, 0, 0);
        f = c.LLVMAddFunction(cg.module, "llvm.stacksave.p0", fn_ty);
    }
    return f;
}

pub fn getOrCreateStackRestoreIntrinsic(cg: *llvm.CodeGenerator) c.LLVMValueRef {
    var f = c.LLVMGetNamedFunction(cg.module, "llvm.stackrestore.p0");
    if (f == null) {
        const i8_ptr = c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0);
        var params = [_]c.LLVMTypeRef{i8_ptr};
        const fn_ty = c.LLVMFunctionType(c.LLVMVoidTypeInContext(cg.context), &params, 1, 0);
        f = c.LLVMAddFunction(cg.module, "llvm.stackrestore.p0", fn_ty);
    }
    return f;
}

pub fn appendUniqueCaptureName(cg: *llvm.CodeGenerator, captures: *std.ArrayList([]const u8), name: []const u8, locals: *std.StringHashMap(void)) !void {
    if (std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false")) return;
    if (locals.contains(name)) return;
    for (captures.items) |existing| {
        if (std.mem.eql(u8, existing, name)) return;
    }
    try captures.append(cg.allocator, name);
}

pub fn appendUniqueName(cg: *llvm.CodeGenerator, names: *std.ArrayList([]const u8), name: []const u8) !void {
    for (names.items) |existing| {
        if (std.mem.eql(u8, existing, name)) return;
    }
    try names.append(cg.allocator, name);
}

pub fn collectHandlerLocalDecls(cg: *llvm.CodeGenerator, statements: []const *ast.Node, locals: *std.StringHashMap(void)) errors.CodegenError!void {
    for (statements) |stmt| {
        switch (stmt.data) {
            .var_decl => |decl| {
                try locals.put(decl.name, {});
            },
            .if_stmt => |if_stmt| {
                try collectHandlerLocalDecls(cg, if_stmt.then_body.items, locals);
                if (if_stmt.else_body) |else_body| {
                    try collectHandlerLocalDecls(cg, else_body.items, locals);
                }
            },
            .for_stmt => |for_stmt| {
                try collectHandlerLocalDecls(cg, for_stmt.body.items, locals);
            },
            .c_for_stmt => |c_for| {
                if (c_for.init) |init_node| {
                    var one = [_]*ast.Node{init_node};
                    try collectHandlerLocalDecls(cg, one[0..], locals);
                }
                try collectHandlerLocalDecls(cg, c_for.body.items, locals);
            },
            .match_stmt => |match_stmt| {
                for (match_stmt.cases.items) |case| {
                    try collectHandlerLocalDecls(cg, case.body.items, locals);
                }
            },
            .expression_block => |block| {
                try collectHandlerLocalDecls(cg, block.statements.items, locals);
            },
            .handled_call_stmt => |handled| {
                for (handled.handlers.items) |nested_handler| {
                    try collectHandlerLocalDecls(cg, nested_handler.body.items, locals);
                }
            },
            else => {},
        }
    }
}

pub fn collectHandlerCapturesFromExpression(cg: *llvm.CodeGenerator, expr: *ast.Node, captures: *std.ArrayList([]const u8), locals: *std.StringHashMap(void)) errors.CodegenError!void {
    switch (expr.data) {
        .identifier => |ident| {
            try appendUniqueCaptureName(cg, captures, ident.name, locals);
        },
        .qualified_identifier => |qual| {
            try collectHandlerCapturesFromExpression(cg, qual.base, captures, locals);
        },
        .function_call => |call| {
            for (call.args.items) |arg| {
                try collectHandlerCapturesFromExpression(cg, arg, captures, locals);
            }
        },
        .method_call => |method| {
            try collectHandlerCapturesFromExpression(cg, method.object, captures, locals);
            for (method.args.items) |arg| {
                try collectHandlerCapturesFromExpression(cg, arg, captures, locals);
            }
        },
        .unary_op => |un| {
            try collectHandlerCapturesFromExpression(cg, un.operand, captures, locals);
        },
        .binary_op => |bin| {
            try collectHandlerCapturesFromExpression(cg, bin.lhs, captures, locals);
            try collectHandlerCapturesFromExpression(cg, bin.rhs, captures, locals);
        },
        .comparison => |cmp| {
            try collectHandlerCapturesFromExpression(cg, cmp.lhs, captures, locals);
            try collectHandlerCapturesFromExpression(cg, cmp.rhs, captures, locals);
        },
        .array_initializer => |arr| {
            for (arr.elements.items) |elem| {
                try collectHandlerCapturesFromExpression(cg, elem, captures, locals);
            }
        },
        .array_index => |arr_idx| {
            try collectHandlerCapturesFromExpression(cg, arr_idx.array, captures, locals);
            try collectHandlerCapturesFromExpression(cg, arr_idx.index, captures, locals);
        },
        .simd_initializer => |simd_init| {
            for (simd_init.elements.items) |elem| {
                try collectHandlerCapturesFromExpression(cg, elem, captures, locals);
            }
        },
        .simd_index => |simd_idx| {
            try collectHandlerCapturesFromExpression(cg, simd_idx.simd, captures, locals);
            try collectHandlerCapturesFromExpression(cg, simd_idx.index, captures, locals);
        },
        .simd_method_call => |simd_method| {
            try collectHandlerCapturesFromExpression(cg, simd_method.simd, captures, locals);
            for (simd_method.args.items) |arg| {
                try collectHandlerCapturesFromExpression(cg, arg, captures, locals);
            }
        },
        .struct_initializer => |struct_init| {
            for (struct_init.field_values.items) |field| {
                try collectHandlerCapturesFromExpression(cg, field.value, captures, locals);
            }
        },
        .cast => |cast_expr| {
            try collectHandlerCapturesFromExpression(cg, cast_expr.expr, captures, locals);
        },
        .expression_block => |block| {
            try collectHandlerCapturesFromStatements(cg, block.statements.items, captures, locals);
            try collectHandlerCapturesFromExpression(cg, block.result, captures, locals);
        },
        else => {},
    }
}

pub fn collectHandlerCapturesFromStatements(cg: *llvm.CodeGenerator, statements: []const *ast.Node, captures: *std.ArrayList([]const u8), locals: *std.StringHashMap(void)) errors.CodegenError!void {
    for (statements) |stmt| {
        switch (stmt.data) {
            .assignment => |as| {
                try collectHandlerCapturesFromExpression(cg, as.target, captures, locals);
                try collectHandlerCapturesFromExpression(cg, as.value, captures, locals);
            },
            .compound_assignment => |as| {
                try collectHandlerCapturesFromExpression(cg, as.target, captures, locals);
                try collectHandlerCapturesFromExpression(cg, as.value, captures, locals);
            },
            .array_assignment => |arr| {
                try collectHandlerCapturesFromExpression(cg, arr.array, captures, locals);
                try collectHandlerCapturesFromExpression(cg, arr.index, captures, locals);
                try collectHandlerCapturesFromExpression(cg, arr.value, captures, locals);
            },
            .array_compound_assignment => |arr| {
                try collectHandlerCapturesFromExpression(cg, arr.array, captures, locals);
                try collectHandlerCapturesFromExpression(cg, arr.index, captures, locals);
                try collectHandlerCapturesFromExpression(cg, arr.value, captures, locals);
            },
            .simd_assignment => |simd_ass| {
                try collectHandlerCapturesFromExpression(cg, simd_ass.simd, captures, locals);
                try collectHandlerCapturesFromExpression(cg, simd_ass.index, captures, locals);
                try collectHandlerCapturesFromExpression(cg, simd_ass.value, captures, locals);
            },
            .simd_compound_assignment => |simd_ass| {
                try collectHandlerCapturesFromExpression(cg, simd_ass.simd, captures, locals);
                try collectHandlerCapturesFromExpression(cg, simd_ass.index, captures, locals);
                try collectHandlerCapturesFromExpression(cg, simd_ass.value, captures, locals);
            },
            .var_decl => |decl| {
                if (decl.initializer) |init_expr| {
                    try collectHandlerCapturesFromExpression(cg, init_expr, captures, locals);
                }
            },
            .function_call, .method_call, .unary_op, .binary_op, .comparison, .array_index, .array_initializer, .identifier, .qualified_identifier, .struct_initializer, .cast, .expression_block, .simd_initializer, .simd_index, .simd_method_call => {
                try collectHandlerCapturesFromExpression(cg, stmt, captures, locals);
            },
            .return_stmt => |ret| {
                if (ret.expression) |expr| {
                    try collectHandlerCapturesFromExpression(cg, expr, captures, locals);
                }
            },
            .if_stmt => |if_stmt| {
                try collectHandlerCapturesFromExpression(cg, if_stmt.condition, captures, locals);
                try collectHandlerCapturesFromStatements(cg, if_stmt.then_body.items, captures, locals);
                if (if_stmt.else_body) |else_body| {
                    try collectHandlerCapturesFromStatements(cg, else_body.items, captures, locals);
                }
            },
            .for_stmt => |for_stmt| {
                if (for_stmt.condition) |cond| {
                    try collectHandlerCapturesFromExpression(cg, cond, captures, locals);
                }
                try collectHandlerCapturesFromStatements(cg, for_stmt.body.items, captures, locals);
            },
            .c_for_stmt => |c_for| {
                if (c_for.init) |init_node| {
                    try collectHandlerCapturesFromStatements(cg, &[_]*ast.Node{init_node}, captures, locals);
                }
                if (c_for.condition) |cond| {
                    try collectHandlerCapturesFromExpression(cg, cond, captures, locals);
                }
                if (c_for.increment) |inc| {
                    try collectHandlerCapturesFromStatements(cg, &[_]*ast.Node{inc}, captures, locals);
                }
                try collectHandlerCapturesFromStatements(cg, c_for.body.items, captures, locals);
            },
            .match_stmt => |match_stmt| {
                try collectHandlerCapturesFromExpression(cg, match_stmt.condition, captures, locals);
                for (match_stmt.cases.items) |case| {
                    for (case.values.items) |val| {
                        try collectHandlerCapturesFromExpression(cg, val, captures, locals);
                    }
                    try collectHandlerCapturesFromStatements(cg, case.body.items, captures, locals);
                }
            },
            .handled_call_stmt => |handled| {
                try collectHandlerCapturesFromExpression(cg, handled.call, captures, locals);
                for (handled.handlers.items) |nested_handler| {
                    try collectHandlerCapturesFromStatements(cg, nested_handler.body.items, captures, locals);
                }
            },
            else => {},
        }
    }
}

pub fn collectMutatedCaptureFromTarget(cg: *llvm.CodeGenerator, target: *ast.Node, mutated: *std.ArrayList([]const u8), locals: *std.StringHashMap(void)) errors.CodegenError!void {
    switch (target.data) {
        .identifier => |ident| {
            if (std.mem.eql(u8, ident.name, "true") or std.mem.eql(u8, ident.name, "false")) return;
            if (locals.contains(ident.name)) return;
            try appendUniqueName(cg, mutated, ident.name);
        },
        .qualified_identifier => |qual| {
            try collectMutatedCaptureFromTarget(cg, qual.base, mutated, locals);
        },
        .array_index => |arr_idx| {
            try collectMutatedCaptureFromTarget(cg, arr_idx.array, mutated, locals);
        },
        .simd_index => |simd_idx| {
            try collectMutatedCaptureFromTarget(cg, simd_idx.simd, mutated, locals);
        },
        else => {},
    }
}

pub fn collectHandlerMutatedCaptures(cg: *llvm.CodeGenerator, statements: []const *ast.Node, mutated: *std.ArrayList([]const u8), locals: *std.StringHashMap(void)) errors.CodegenError!void {
    for (statements) |stmt| {
        switch (stmt.data) {
            .assignment => |as| {
                try collectMutatedCaptureFromTarget(cg, as.target, mutated, locals);
            },
            .compound_assignment => |as| {
                try collectMutatedCaptureFromTarget(cg, as.target, mutated, locals);
            },
            .array_assignment => |arr| {
                try collectMutatedCaptureFromTarget(cg, arr.array, mutated, locals);
            },
            .array_compound_assignment => |arr| {
                try collectMutatedCaptureFromTarget(cg, arr.array, mutated, locals);
            },
            .simd_assignment => |simd_ass| {
                try collectMutatedCaptureFromTarget(cg, simd_ass.simd, mutated, locals);
            },
            .simd_compound_assignment => |simd_ass| {
                try collectMutatedCaptureFromTarget(cg, simd_ass.simd, mutated, locals);
            },
            .if_stmt => |if_stmt| {
                try collectHandlerMutatedCaptures(cg, if_stmt.then_body.items, mutated, locals);
                if (if_stmt.else_body) |else_body| {
                    try collectHandlerMutatedCaptures(cg, else_body.items, mutated, locals);
                }
            },
            .for_stmt => |for_stmt| {
                try collectHandlerMutatedCaptures(cg, for_stmt.body.items, mutated, locals);
            },
            .c_for_stmt => |c_for| {
                if (c_for.init) |init_node| {
                    try collectHandlerMutatedCaptures(cg, &[_]*ast.Node{init_node}, mutated, locals);
                }
                if (c_for.increment) |inc_node| {
                    try collectHandlerMutatedCaptures(cg, &[_]*ast.Node{inc_node}, mutated, locals);
                }
                try collectHandlerMutatedCaptures(cg, c_for.body.items, mutated, locals);
            },
            .match_stmt => |match_stmt| {
                for (match_stmt.cases.items) |case| {
                    try collectHandlerMutatedCaptures(cg, case.body.items, mutated, locals);
                }
            },
            .expression_block => |block| {
                try collectHandlerMutatedCaptures(cg, block.statements.items, mutated, locals);
            },
            .handled_call_stmt => |handled| {
                for (handled.handlers.items) |nested_handler| {
                    try collectHandlerMutatedCaptures(cg, nested_handler.body.items, mutated, locals);
                }
            },
            else => {},
        }
    }
}

pub fn nameInList(names: []const []const u8, name: []const u8) bool {
    for (names) |entry| {
        if (std.mem.eql(u8, entry, name)) return true;
    }
    return false;
}

pub fn materializeHandlerCaptures(cg: *llvm.CodeGenerator, statements: []const *ast.Node) errors.CodegenError!void {
    var locals = std.StringHashMap(void).init(cg.allocator);
    defer locals.deinit();
    try collectHandlerLocalDecls(cg, statements, &locals);

    var captures: std.ArrayList([]const u8) = .empty;
    defer captures.deinit(cg.allocator);
    try collectHandlerCapturesFromStatements(cg, statements, &captures, &locals);

    var mutated: std.ArrayList([]const u8) = .empty;
    defer mutated.deinit(cg.allocator);
    try collectHandlerMutatedCaptures(cg, statements, &mutated, &locals);

    for (captures.items) |name| {
        if (llvm.CodeGenerator.variableExistsInCurrentScope(cg, name)) continue;
        const outer_var = getScopedVariable(cg, name) orelse continue;
        if (outer_var.value == null) continue;

        if (nameInList(mutated.items, name)) {
            try llvm.CodeGenerator.putVariable(cg, name, structs.VariableInfo{
                .value = outer_var.value,
                .type_ref = outer_var.type_ref,
                .type_name = outer_var.type_name,
                .is_byval_param = outer_var.is_byval_param,
                .is_const = outer_var.is_const,
            });
            continue;
        }

        const slot = c.LLVMBuildAlloca(cg.builder, outer_var.type_ref, name.ptr);
        const loaded = c.LLVMBuildLoad2(cg.builder, outer_var.type_ref, outer_var.value, "capture.load");
        _ = c.LLVMBuildStore(cg.builder, loaded, slot);

        try llvm.CodeGenerator.putVariable(cg, name, structs.VariableInfo{
            .value = slot,
            .type_ref = outer_var.type_ref,
            .type_name = outer_var.type_name,
            .is_byval_param = outer_var.is_byval_param,
            .is_const = outer_var.is_const,
        });
    }
}

pub fn collectFunctionVarTypesFromStatements(cg: *llvm.CodeGenerator, statements: []const *ast.Node, out: *std.StringHashMap([]const u8)) errors.CodegenError!void {
    for (statements) |stmt| {
        switch (stmt.data) {
            .var_decl => |decl| {
                if (!out.contains(decl.name)) {
                    try out.put(decl.name, decl.type_name);
                }
            },
            .if_stmt => |if_stmt| {
                try collectFunctionVarTypesFromStatements(cg, if_stmt.then_body.items, out);
                if (if_stmt.else_body) |else_body| {
                    try collectFunctionVarTypesFromStatements(cg, else_body.items, out);
                }
            },
            .for_stmt => |for_stmt| {
                try collectFunctionVarTypesFromStatements(cg, for_stmt.body.items, out);
            },
            .c_for_stmt => |c_for| {
                if (c_for.init) |init_node| {
                    try collectFunctionVarTypesFromStatements(cg, &[_]*ast.Node{init_node}, out);
                }
                try collectFunctionVarTypesFromStatements(cg, c_for.body.items, out);
            },
            .match_stmt => |match_stmt| {
                for (match_stmt.cases.items) |case| {
                    try collectFunctionVarTypesFromStatements(cg, case.body.items, out);
                }
            },
            .expression_block => |block| {
                try collectFunctionVarTypesFromStatements(cg, block.statements.items, out);
            },
            .handled_call_stmt => |handled| {
                for (handled.handlers.items) |handler| {
                    try collectFunctionVarTypesFromStatements(cg, handler.body.items, out);
                }
            },
            else => {},
        }
    }
}

pub fn collectFunctionVarTypes(cg: *llvm.CodeGenerator, function_name: []const u8, out: *std.StringHashMap([]const u8)) errors.CodegenError!void {
    if (cg.function_asts.get(function_name)) |func| {
        for (func.parameters.items) |param| {
            if (!out.contains(param.name)) {
                try out.put(param.name, param.type_name);
            }
        }
        try collectFunctionVarTypesFromStatements(cg, func.body.items, out);
    }
}

pub fn collectSolicitHandlerCaptureSets(cg: *llvm.CodeGenerator, handled: ast.HandledCallStmt, captures: *std.ArrayList([]const u8), mutated: *std.ArrayList([]const u8)) errors.CodegenError!void {
    for (handled.handlers.items) |handler| {
        if (handler.kind != .solicit) continue;

        var locals = std.StringHashMap(void).init(cg.allocator);
        defer locals.deinit();
        try collectHandlerLocalDecls(cg, handler.body.items, &locals);

        var handler_caps: std.ArrayList([]const u8) = .empty;
        defer handler_caps.deinit(cg.allocator);
        try collectHandlerCapturesFromStatements(cg, handler.body.items, &handler_caps, &locals);
        for (handler_caps.items) |name| {
            try appendUniqueName(cg, captures, name);
        }

        var handler_mut: std.ArrayList([]const u8) = .empty;
        defer handler_mut.deinit(cg.allocator);
        try collectHandlerMutatedCaptures(cg, handler.body.items, &handler_mut, &locals);
        for (handler_mut.items) |name| {
            try appendUniqueName(cg, mutated, name);
        }
    }
}

pub fn exposeCurrentFunctionVarsForSolicit(cg: *llvm.CodeGenerator) errors.CodegenError!void {
    const source_name = cg.current_source_function_name orelse return;

    var seen = std.StringHashMap(void).init(cg.allocator);
    defer seen.deinit();

    var scope_index = cg.variable_scopes.items.len;
    while (scope_index > 0) {
        scope_index -= 1;
        var it = cg.variable_scopes.items[scope_index].iterator();
        while (it.next()) |entry| {
            const var_name = entry.key_ptr.*;
            if (seen.contains(var_name)) continue;
            try seen.put(var_name, {});

            const slot = try ensureSolicitVarSlot(cg, source_name, var_name);
            const i8_ptr = c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0);
            const raw_ptr = c.LLVMBuildBitCast(cg.builder, entry.value_ptr.value, i8_ptr, "solicit.var.ptr");
            _ = c.LLVMBuildStore(cg.builder, raw_ptr, slot);
        }
    }
}

pub fn generateSolicitCallback(cg: *llvm.CodeGenerator, callback_id: usize, handled: ast.HandledCallStmt, caller_bindings: []const SolicitCaptureBinding, callee_bindings: []const SolicitCaptureBinding) errors.CodegenError!c.LLVMValueRef {
    const i32_ty = c.LLVMInt32TypeInContext(cg.context);
    var param_types = [_]c.LLVMTypeRef{i32_ty};
    const callback_fn_ty = c.LLVMFunctionType(c.LLVMVoidTypeInContext(cg.context), &param_types, 1, 0);

    const callback_name = try std.fmt.allocPrint(cg.allocator, "__zlang_solicit_cb_{d}", .{callback_id});
    defer cg.allocator.free(callback_name);
    const callback_name_z = utils.dupeZ(cg.allocator, callback_name);
    defer cg.allocator.free(callback_name_z);

    const callback_fn = c.LLVMAddFunction(cg.module, callback_name_z.ptr, callback_fn_ty);
    const entry_bb = c.LLVMAppendBasicBlockInContext(cg.context, callback_fn, "entry");

    const saved_bb = c.LLVMGetInsertBlock(cg.builder);
    const saved_current_function = cg.current_function;
    const saved_return_type = cg.current_function_return_type;
    const saved_source_name = cg.current_source_function_name;
    const saved_scopes = cg.variable_scopes;

    cg.variable_scopes = .empty;
    defer {
        for (cg.variable_scopes.items) |*scope| {
            scope.deinit();
        }
        cg.variable_scopes.deinit(cg.allocator);
        cg.variable_scopes = saved_scopes;
        cg.current_function = saved_current_function;
        cg.current_function_return_type = saved_return_type;
        cg.current_source_function_name = saved_source_name;
        if (saved_bb != null) {
            c.LLVMPositionBuilderAtEnd(cg.builder, saved_bb);
        }
    }

    try cg.variable_scopes.append(cg.allocator, std.HashMap([]const u8, structs.VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(cg.allocator));
    cg.current_function = callback_fn;
    cg.current_function_return_type = "void";
    cg.current_source_function_name = null;
    c.LLVMPositionBuilderAtEnd(cg.builder, entry_bb);

    const i8_ptr = c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0);
    const all_bindings = [_][]const SolicitCaptureBinding{ caller_bindings, callee_bindings };
    for (all_bindings) |bindings| {
        for (bindings) |binding| {
            const raw_ptr = c.LLVMBuildLoad2(cg.builder, i8_ptr, binding.slot, "solicit.cap.raw");
            const typed_ptr_ty = c.LLVMPointerType(binding.type_ref, 0);
            const typed_ptr = c.LLVMBuildBitCast(cg.builder, raw_ptr, typed_ptr_ty, "solicit.cap.ptr");

            if (binding.mutated) {
                try llvm.CodeGenerator.putVariable(cg, binding.name, structs.VariableInfo{
                    .value = typed_ptr,
                    .type_ref = binding.type_ref,
                    .type_name = binding.type_name,
                });
            } else {
                const copy_slot = c.LLVMBuildAlloca(cg.builder, binding.type_ref, binding.name.ptr);
                const loaded = c.LLVMBuildLoad2(cg.builder, binding.type_ref, typed_ptr, "solicit.cap.load");
                _ = c.LLVMBuildStore(cg.builder, loaded, copy_slot);
                try llvm.CodeGenerator.putVariable(cg, binding.name, structs.VariableInfo{
                    .value = copy_slot,
                    .type_ref = binding.type_ref,
                    .type_name = binding.type_name,
                });
            }
        }
    }

    const err_code_param = c.LLVMGetParam(callback_fn, 0);
    for (handled.handlers.items) |handler| {
        if (handler.kind != .solicit) continue;

        const current_bb = c.LLVMGetInsertBlock(cg.builder);
        if (c.LLVMGetBasicBlockTerminator(current_bb) != null) break;

        const handler_bb = c.LLVMAppendBasicBlockInContext(cg.context, callback_fn, "solicit.handler");
        const next_bb = c.LLVMAppendBasicBlockInContext(cg.context, callback_fn, "solicit.next");

        const cond = if (handler.error_name) |error_name| blk: {
            const code = cg.error_codes.get(error_name) orelse return errors.CodegenError.TypeMismatch;
            const code_bits: c_ulonglong = @bitCast(@as(i64, code));
            const code_val = c.LLVMConstInt(i32_ty, code_bits, 1);
            break :blk c.LLVMBuildICmp(cg.builder, c.LLVMIntEQ, err_code_param, code_val, "solicit.match");
        } else if (handler.error_code) |error_code| blk: {
            const code_bits: c_ulonglong = @bitCast(@as(i64, error_code));
            const code_val = c.LLVMConstInt(i32_ty, code_bits, 1);
            break :blk c.LLVMBuildICmp(cg.builder, c.LLVMIntEQ, err_code_param, code_val, "solicit.match.code");
        } else blk: {
            const zero = c.LLVMConstInt(i32_ty, 0, 0);
            break :blk c.LLVMBuildICmp(cg.builder, c.LLVMIntNE, err_code_param, zero, "solicit.any");
        };

        _ = c.LLVMBuildCondBr(cg.builder, cond, handler_bb, next_bb);
        c.LLVMPositionBuilderAtEnd(cg.builder, handler_bb);
        for (handler.body.items) |handler_stmt| {
            try cg.generateStatement(handler_stmt);
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(cg.builder)) != null) break;
        }
        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(cg.builder)) == null) {
            _ = c.LLVMBuildBr(cg.builder, next_bb);
        }
        c.LLVMPositionBuilderAtEnd(cg.builder, next_bb);
    }

    if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(cg.builder)) == null) {
        _ = c.LLVMBuildRetVoid(cg.builder);
    }

    return callback_fn;
}

// Handler safety contract (draft for docs):
// - Handlers run synchronously in caller context.
// - Captured values are non-escaping: no storing of temporary capture references beyond handler execution.
// - By-ref captures are only for variables proven mutated by handler body; read-only captures are copied by value.
// - Handler stack temporaries are bounded to handler lifetime via stacksave/stackrestore.
// - `send` and `solicit` are tracked as separate flow modes; each `on` handler kind matches only its mode.
// - Runtime split: `send` handlers execute after the call returns; `solicit` handlers execute immediately
//   via a synchronous callback trampoline while callee locals are still live.
pub fn generateHandledCall(cg: *llvm.CodeGenerator, handled: ast.HandledCallStmt, expected_type: ?[]const u8) errors.CodegenError!c.LLVMValueRef {
    const err_global = ensureLastErrorGlobal(cg);
    const mode_global = ensureLastErrorModeGlobal(cg);
    const callback_global = ensureSolicitCallbackGlobal(cg);
    const i32_ty = c.LLVMInt32TypeInContext(cg.context);
    const i8_ptr = c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0);
    _ = c.LLVMBuildStore(cg.builder, c.LLVMConstInt(i32_ty, 0, 0), err_global);
    _ = c.LLVMBuildStore(cg.builder, c.LLVMConstInt(i32_ty, 0, 0), mode_global);
    _ = c.LLVMBuildStore(cg.builder, c.LLVMConstNull(i8_ptr), callback_global);

    var has_solicit_handlers = false;
    for (handled.handlers.items) |handler| {
        if (handler.kind == .solicit) {
            has_solicit_handlers = true;
            break;
        }
    }

    if (has_solicit_handlers and handled.call.data == .function_call) {
        const call_name = handled.call.data.function_call.name;
        var captures: std.ArrayList([]const u8) = .empty;
        defer captures.deinit(cg.allocator);
        var mutated: std.ArrayList([]const u8) = .empty;
        defer mutated.deinit(cg.allocator);
        try collectSolicitHandlerCaptureSets(cg, handled, &captures, &mutated);

        var callee_var_types = std.StringHashMap([]const u8).init(cg.allocator);
        defer callee_var_types.deinit();
        try collectFunctionVarTypes(cg, call_name, &callee_var_types);

        var caller_bindings: std.ArrayList(SolicitCaptureBinding) = .empty;
        defer caller_bindings.deinit(cg.allocator);
        var callee_bindings: std.ArrayList(SolicitCaptureBinding) = .empty;
        defer callee_bindings.deinit(cg.allocator);

        const callback_id = cg.next_solicit_callback_id;
        cg.next_solicit_callback_id += 1;

        for (captures.items) |name| {
            const is_mutated = nameInList(mutated.items, name);
            if (getScopedVariable(cg, name)) |outer_var| {
                const slot = try ensureSolicitCaptureSlot(cg, callback_id, name);
                const raw_ptr = c.LLVMBuildBitCast(cg.builder, outer_var.value, i8_ptr, "solicit.capture.ptr");
                _ = c.LLVMBuildStore(cg.builder, raw_ptr, slot);
                try caller_bindings.append(cg.allocator, .{
                    .name = name,
                    .type_ref = outer_var.type_ref,
                    .type_name = outer_var.type_name,
                    .mutated = is_mutated,
                    .slot = slot,
                });
                continue;
            }

            if (callee_var_types.get(name)) |type_name| {
                const type_ref = try cg.getLLVMType(type_name);
                const slot = try ensureSolicitVarSlot(cg, call_name, name);
                try callee_bindings.append(cg.allocator, .{
                    .name = name,
                    .type_ref = type_ref,
                    .type_name = type_name,
                    .mutated = is_mutated,
                    .slot = slot,
                });
            }
        }

        const callback_fn = try generateSolicitCallback(cg, callback_id, handled, caller_bindings.items, callee_bindings.items);
        const callback_ptr = c.LLVMBuildBitCast(cg.builder, callback_fn, i8_ptr, "solicit.callback.ptr");
        _ = c.LLVMBuildStore(cg.builder, callback_ptr, callback_global);
    }

    var call_result: c.LLVMValueRef = undefined;
    if (handled.call.data == .function_call) {
        call_result = try functions.generateFunctionCall(cg, handled.call.data.function_call, expected_type);
    } else {
        call_result = try cg.generateExpressionWithContext(handled.call, expected_type);
    }

    _ = c.LLVMBuildStore(cg.builder, c.LLVMConstNull(i8_ptr), callback_global);

    for (handled.handlers.items) |handler| {
        if (handler.kind == .solicit) continue;
        const current_bb = c.LLVMGetInsertBlock(cg.builder);
        if (c.LLVMGetBasicBlockTerminator(current_bb) != null) break;

        const current_fn = cg.current_function orelse return errors.CodegenError.TypeMismatch;
        const handler_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_fn, "err.handler");
        const next_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_fn, "err.next");
        const err_now = c.LLVMBuildLoad2(cg.builder, i32_ty, err_global, "err.code");
        const mode_now = c.LLVMBuildLoad2(cg.builder, i32_ty, mode_global, "err.mode");
        const expected_mode = switch (handler.kind) {
            .send => c.LLVMConstInt(i32_ty, 1, 0),
            .solicit => c.LLVMConstInt(i32_ty, 2, 0),
        };
        const mode_match = c.LLVMBuildICmp(cg.builder, c.LLVMIntEQ, mode_now, expected_mode, "err.mode.match");

        const error_cond = if (handler.error_name) |error_name| blk: {
            const code = cg.error_codes.get(error_name) orelse return errors.CodegenError.TypeMismatch;
            const code_bits: c_ulonglong = @bitCast(@as(i64, code));
            const code_val = c.LLVMConstInt(i32_ty, code_bits, 1);
            break :blk c.LLVMBuildICmp(cg.builder, c.LLVMIntEQ, err_now, code_val, "err.match");
        } else if (handler.error_code) |error_code| blk: {
            const code_bits: c_ulonglong = @bitCast(@as(i64, error_code));
            const code_val = c.LLVMConstInt(i32_ty, code_bits, 1);
            break :blk c.LLVMBuildICmp(cg.builder, c.LLVMIntEQ, err_now, code_val, "err.match.code");
        } else blk: {
            const zero = c.LLVMConstInt(i32_ty, 0, 0);
            break :blk c.LLVMBuildICmp(cg.builder, c.LLVMIntNE, err_now, zero, "err.any");
        };
        const cond = c.LLVMBuildAnd(cg.builder, mode_match, error_cond, "err.cond");

        _ = c.LLVMBuildCondBr(cg.builder, cond, handler_bb, next_bb);

        c.LLVMPositionBuilderAtEnd(cg.builder, handler_bb);

        const stacksave_fn = getOrCreateStackSaveIntrinsic(cg);
        const stacksave_ty = c.LLVMGlobalGetValueType(stacksave_fn);
        const stack_ptr = c.LLVMBuildCall2(cg.builder, stacksave_ty, stacksave_fn, null, 0, "handler.stack");

        try llvm.CodeGenerator.pushScope(cg);
        const handler_marker = cg.currentDeferMarker();
        errdefer llvm.CodeGenerator.popScope(cg);
        try materializeHandlerCaptures(cg, handler.body.items);
        for (handler.body.items) |handler_stmt| {
            try cg.generateStatement(handler_stmt);
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(cg.builder)) != null) break;
        }
        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(cg.builder)) == null) {
            try cg.runDeferredActionsFrom(handler_marker);
        }
        llvm.CodeGenerator.popScope(cg);

        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(cg.builder)) == null) {
            const stackrestore_fn = getOrCreateStackRestoreIntrinsic(cg);
            const stackrestore_ty = c.LLVMGlobalGetValueType(stackrestore_fn);
            var restore_args = [_]c.LLVMValueRef{stack_ptr};
            _ = c.LLVMBuildCall2(cg.builder, stackrestore_ty, stackrestore_fn, &restore_args, 1, "");
            _ = c.LLVMBuildBr(cg.builder, next_bb);
        }

        c.LLVMPositionBuilderAtEnd(cg.builder, next_bb);
    }

    return call_result;
}
