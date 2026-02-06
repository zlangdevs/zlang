const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
const llvm = @import("llvm.zig");
const variables = @import("variables.zig");
const structs = @import("structs.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

pub fn declareLibcFunction(cg: *llvm.CodeGenerator, func_name: []const u8) !c.LLVMValueRef {
    if (cg.functions.get(func_name)) |existing| {
        return @ptrCast(existing);
    }
    if (utils.LIBC_FUNCTIONS.get(func_name)) |signature| {
        const func = try createFunctionFromSignature(cg, func_name, signature);
        try cg.functions.put(utils.dupe(u8, cg.allocator, func_name), @ptrCast(func));
        return func;
    }
    const func = try createGenericLibcFunction(cg, func_name);
    try cg.functions.put(utils.dupe(u8, cg.allocator, func_name), @ptrCast(func));
    return func;
}

fn createFunctionFromSignature(cg: *llvm.CodeGenerator, func_name: []const u8, signature: utils.LibcFunctionSignature) !c.LLVMValueRef {
    const return_type = libcTypeToLLVM(cg, signature.return_type);

    var param_types = std.ArrayList(c.LLVMTypeRef){};
    defer param_types.deinit(cg.allocator);

    for (signature.param_types) |param_type| {
        try param_types.append(cg.allocator, libcTypeToLLVM(cg, param_type));
    }

    const function_type = if (param_types.items.len > 0)
        c.LLVMFunctionType(return_type, param_types.items.ptr, @intCast(param_types.items.len), if (signature.is_varargs) 1 else 0)
    else
        c.LLVMFunctionType(return_type, null, 0, if (signature.is_varargs) 1 else 0);

    const func_name_z = utils.dupeZ(cg.allocator, func_name);
    defer cg.allocator.free(func_name_z);

    const llvm_func = c.LLVMAddFunction(@ptrCast(cg.module), func_name_z.ptr, function_type);
    try cg.functions.put(utils.dupe(u8, cg.allocator, func_name), @ptrCast(llvm_func));

    return llvm_func;
}

fn createGenericLibcFunction(cg: *llvm.CodeGenerator, func_name: []const u8) !c.LLVMValueRef {
    const i8_ptr_type = c.LLVMPointerType(c.LLVMInt8TypeInContext(@ptrCast(cg.context)), 0);
    var generic_args = [_]c.LLVMTypeRef{i8_ptr_type};
    const function_type = c.LLVMFunctionType(c.LLVMInt32TypeInContext(@ptrCast(cg.context)), &generic_args[0], 1, 1);

    const func_name_z = utils.dupeZ(cg.allocator, func_name);
    defer cg.allocator.free(func_name_z);

    const llvm_func = c.LLVMAddFunction(@ptrCast(cg.module), func_name_z.ptr, function_type);
    try cg.functions.put(utils.dupe(u8, cg.allocator, func_name), @ptrCast(llvm_func));

    return llvm_func;
}

fn isTemplate(func: ast.Function) bool {
    // Check if the return type has generic parameters like [T]>>T
    if (std.mem.startsWith(u8, func.return_type, "[")) return true;
    for (func.parameters.items) |param| {
        // [T] is used for generic types in parameters
        if (std.mem.indexOf(u8, param.type_name, "[") != null) return true;
    }
    return false;
}

fn getActualReturnType(raw: []const u8) []const u8 {
    if (std.mem.startsWith(u8, raw, "[")) {
        if (std.mem.indexOf(u8, raw, ">>")) |idx| {
            return raw[idx + 2 ..];
        }
    }
    // Remove >> if it's there at the start (though parser usually handles this, sometimes the raw string might have it)
    if (std.mem.startsWith(u8, raw, ">>")) {
        return raw[2..];
    }
    return raw;
}

pub fn getMangledName(allocator: std.mem.Allocator, name: []const u8, param_type_names: []const []const u8, _: *llvm.CodeGenerator) ![]const u8 {
    if (std.mem.eql(u8, name, "main")) return utils.dupe(u8, allocator, name);

    var mangled = std.ArrayList(u8){};

    try mangled.appendSlice(allocator, name);
    try mangled.appendSlice(allocator, "__");

    for (param_type_names, 0..) |type_name, i| {
        if (i > 0) try mangled.appendSlice(allocator, "_");
        for (type_name) |char| {
            switch (char) {
                ' ', '<', '>', ',', '*', '[', ']', '(', ')' => try mangled.append(allocator, '_'),
                else => try mangled.append(allocator, char),
            }
        }
    }

    return mangled.toOwnedSlice(allocator);
}

pub fn declareFunction(cg: *llvm.CodeGenerator, func: ast.Function) errors.CodegenError!void {
    const is_temp = isTemplate(func);

    const overload = structs.FunctionOverload{
        .func_node = func,
        .is_template = is_temp,
    };

    if (cg.function_overloads.getPtr(func.name)) |list_ptr| {
        try list_ptr.append(cg.allocator, overload);
    } else {
        var list = std.ArrayList(structs.FunctionOverload){};
        try list.append(cg.allocator, overload);
        try cg.function_overloads.put(utils.dupe(u8, cg.allocator, func.name), list);
    }

    if (is_temp and cg.template_substitutions == null) return;

    var param_types = std.ArrayList(c.LLVMTypeRef){};
    defer param_types.deinit(cg.allocator);

    var is_varargs = false;
    var is_typed_vararg = false;
    var vararg_element_type: ?[]const u8 = null;

    for (func.parameters.items, 0..) |param, i| {
        if (utils.isVarArgType(param.type_name)) {
            if (i != func.parameters.items.len - 1) {
                return errors.CodegenError.TypeMismatch;
            }
            if (utils.getVarArgType(param.type_name)) |var_type| {
                is_typed_vararg = true;
                vararg_element_type = var_type;
                const element_llvm_type = try cg.getLLVMType(var_type);
                try param_types.append(cg.allocator, c.LLVMPointerType(element_llvm_type, 0));
                try param_types.append(cg.allocator, c.LLVMInt32TypeInContext(cg.context));
            } else {
                is_varargs = true;
            }
            continue;
        }
        const param_type = try cg.getLLVMType(param.type_name);
        if (c.LLVMGetTypeKind(@ptrCast(param_type)) == c.LLVMStructTypeKind and utils.shouldUseByVal(cg, @ptrCast(param_type))) {
            try param_types.append(cg.allocator, c.LLVMPointerType(@ptrCast(param_type), 0));
        } else {
            try param_types.append(cg.allocator, @ptrCast(param_type));
        }
    }

    const return_type = try cg.getLLVMType(getActualReturnType(func.return_type));
    const function_type = if (param_types.items.len > 0)
        c.LLVMFunctionType(@ptrCast(return_type), param_types.items.ptr, @intCast(param_types.items.len), if (is_varargs) 1 else 0)
    else
        c.LLVMFunctionType(@ptrCast(return_type), null, 0, if (is_varargs) 1 else 0);

    var final_func_name: []const u8 = undefined;
    var raw_param_types = std.ArrayList(c.LLVMTypeRef){};
    defer raw_param_types.deinit(cg.allocator);
    for (func.parameters.items) |param| {
        if (utils.isVarArgType(param.type_name)) continue;
        try raw_param_types.append(cg.allocator, try cg.getLLVMType(param.type_name));
    }

    var type_names = std.ArrayList([]const u8){};
    defer type_names.deinit(cg.allocator);
    for (func.parameters.items) |p| {
        if (utils.isVarArgType(p.type_name)) continue;
        if (cg.template_substitutions) |subs| {
            if (subs.get(p.type_name)) |resolved| {
                try type_names.append(cg.allocator, resolved);
                continue;
            }
        }
        try type_names.append(cg.allocator, p.type_name);
    }
    final_func_name = try getMangledName(cg.allocator, func.name, type_names.items, cg);

    const func_name_z = utils.dupeZ(cg.allocator, final_func_name);
    defer cg.allocator.free(func_name_z);

    if (cg.functions.get(final_func_name)) |_| {
        return;
    }

    const llvm_func = c.LLVMAddFunction(@ptrCast(cg.module), func_name_z.ptr, function_type);

    var param_idx: usize = 0;
    for (func.parameters.items) |param| {
        if (utils.isVarArgType(param.type_name)) {
            if (is_typed_vararg) {
                param_idx += 2;
            }
            continue;
        }
        const param_type = try cg.getLLVMType(param.type_name);
        if (c.LLVMGetTypeKind(@ptrCast(param_type)) == c.LLVMStructTypeKind and utils.shouldUseByVal(cg, @ptrCast(param_type))) {
            const byval_attr = c.LLVMCreateTypeAttribute(cg.context, c.LLVMGetEnumAttributeKindForName("byval", 5), @ptrCast(param_type));
            c.LLVMAddAttributeAtIndex(llvm_func, @intCast(param_idx + 1), byval_attr);
            const align_attr = c.LLVMCreateEnumAttribute(cg.context, c.LLVMGetEnumAttributeKindForName("align", 5), cg.getAlignmentForType(@ptrCast(param_type)));
            c.LLVMAddAttributeAtIndex(llvm_func, @intCast(param_idx + 1), align_attr);
        }
        param_idx += 1;
    }

    try cg.functions.put(final_func_name, @ptrCast(llvm_func));
    try cg.regular_functions.put(final_func_name, true);
    try cg.function_return_types.put(final_func_name, func.return_type);
    if (is_varargs or is_typed_vararg) {
        const last_param = func.parameters.items[func.parameters.items.len - 1];
        if (is_typed_vararg and vararg_element_type != null) {
            try cg.function_vararg_types.put(final_func_name, vararg_element_type.?);
            const typed_info = structs.TypedVarargInfo{
                .param_name = utils.dupe(u8, cg.allocator, last_param.name),
                .ptr_var = try std.fmt.allocPrint(cg.allocator, "__vararg_ptr_{s}", .{last_param.name}),
                .count_var = try std.fmt.allocPrint(cg.allocator, "__vararg_count_{s}", .{last_param.name}),
                .element_type = utils.dupe(u8, cg.allocator, vararg_element_type.?),
            };
            try cg.typed_vararg_info.put(final_func_name, typed_info);
        } else if (utils.getVarArgType(last_param.type_name)) |var_type| {
            try cg.function_vararg_types.put(final_func_name, var_type);
        } else {
            try cg.function_vararg_types.put(final_func_name, "_");
        }
    }
}

pub fn generateFunctionBody(cg: *llvm.CodeGenerator, func: ast.Function) errors.CodegenError!void {
    if (isTemplate(func) and cg.template_substitutions == null) return;
    var type_names = std.ArrayList([]const u8){};
    defer type_names.deinit(cg.allocator);
    for (func.parameters.items) |param| {
        if (utils.isVarArgType(param.type_name)) continue;
        if (cg.template_substitutions) |subs| {
            if (subs.get(param.type_name)) |resolved| {
                try type_names.append(cg.allocator, resolved);
                continue;
            }
        }
        try type_names.append(cg.allocator, param.type_name);
    }
    const mangled_name = try getMangledName(cg.allocator, func.name, type_names.items, cg);
    defer if (!std.mem.eql(u8, func.name, "main")) cg.allocator.free(mangled_name);
    const llvm_func = cg.functions.get(mangled_name) orelse return errors.CodegenError.UndefinedFunction;
    cg.setCurrentModuleByFunction(mangled_name);
    cg.current_function = llvm_func;
    cg.current_function_return_type = getActualReturnType(func.return_type);
    const entry_block = c.LLVMAppendBasicBlockInContext(@ptrCast(cg.context), @ptrCast(llvm_func), "entry");
    c.LLVMPositionBuilderAtEnd(@ptrCast(cg.builder), entry_block);
    cg.label_blocks.clearRetainingCapacity();
    for (cg.pending_gotos.items) |pending| {
        cg.allocator.free(pending.label);
    }
    cg.pending_gotos.clearRetainingCapacity();

    variables.clearCurrentFunctionScopes(cg);

    try variables.pushScope(cg);

    var param_idx: usize = 0;
    for (func.parameters.items) |param| {
        if (utils.isVarArgType(param.type_name)) {
            if (cg.typed_vararg_info.get(mangled_name)) |typed_info| {
                const ptr_param = c.LLVMGetParam(@ptrCast(llvm_func), @intCast(param_idx));
                const count_param = c.LLVMGetParam(@ptrCast(llvm_func), @intCast(param_idx + 1));

                const element_type = try cg.getLLVMType(typed_info.element_type);
                const ptr_type = c.LLVMPointerType(element_type, 0);
                const count_type = c.LLVMInt32TypeInContext(cg.context);

                const ptr_alloca = c.LLVMBuildAlloca(@ptrCast(cg.builder), ptr_type, typed_info.ptr_var.ptr);
                _ = c.LLVMBuildStore(@ptrCast(cg.builder), ptr_param, ptr_alloca);
                const ptr_type_name = try std.fmt.allocPrint(cg.allocator, "ptr<{s}>", .{typed_info.element_type});
                try variables.putVariable(cg, typed_info.ptr_var, structs.VariableInfo{
                    .value = @ptrCast(ptr_alloca),
                    .type_ref = ptr_type,
                    .type_name = ptr_type_name,
                });

                const count_alloca = c.LLVMBuildAlloca(@ptrCast(cg.builder), count_type, typed_info.count_var.ptr);
                _ = c.LLVMBuildStore(@ptrCast(cg.builder), count_param, count_alloca);
                try variables.putVariable(cg, typed_info.count_var, structs.VariableInfo{
                    .value = @ptrCast(count_alloca),
                    .type_ref = count_type,
                    .type_name = "i32",
                });

                param_idx += 2;
            }
            continue;
        }
        const param_value = c.LLVMGetParam(@ptrCast(llvm_func), @intCast(param_idx));
        const param_type = try cg.getLLVMType(param.type_name);

        if (c.LLVMGetTypeKind(@ptrCast(param_type)) == c.LLVMStructTypeKind and utils.shouldUseByVal(cg, @ptrCast(param_type))) {
            try variables.putVariable(cg, param.name, structs.VariableInfo{
                .value = @ptrCast(param_value),
                .type_ref = @ptrCast(param_type),
                .type_name = param.type_name,
                .is_byval_param = true,
            });
        } else {
            const alloca = c.LLVMBuildAlloca(@ptrCast(cg.builder), @ptrCast(param_type), param.name.ptr);
            _ = c.LLVMBuildStore(@ptrCast(cg.builder), @ptrCast(param_value), @ptrCast(alloca));
            try variables.putVariable(cg, param.name, structs.VariableInfo{
                .value = @ptrCast(alloca),
                .type_ref = @ptrCast(param_type),
                .type_name = param.type_name,
            });
        }
        param_idx += 1;
    }

    const valid_control_flow = try hasValidControlFlow(cg, func);

    for (func.body.items) |stmt| {
        try cg.generateStatement(stmt);
    }
    if (!valid_control_flow and !std.mem.eql(u8, func.return_type, "void")) {
        if (func.body.items.len == 0 or !utils.isReturnStatement(func.body.items[func.body.items.len - 1])) {
            return errors.CodegenError.TypeMismatch;
        }
    }
    if (valid_control_flow) {
        const last_is_return = if (func.body.items.len > 0)
            utils.isReturnStatement(func.body.items[func.body.items.len - 1])
        else
            false;
        if (std.mem.eql(u8, func.return_type, "void")) {
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(@ptrCast(cg.builder))) == null) {
                _ = c.LLVMBuildRetVoid(@ptrCast(cg.builder));
            }
        } else {
            if (!last_is_return) {
                const default_value = utils.getDefaultValueForType(cg, func.return_type);
                _ = c.LLVMBuildRet(@ptrCast(cg.builder), @ptrCast(default_value));
            } else {
                if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(@ptrCast(cg.builder))) == null) {
                    const default_value = utils.getDefaultValueForType(cg, func.return_type);
                    _ = c.LLVMBuildRet(@ptrCast(cg.builder), @ptrCast(default_value));
                }
            }
        }
    }
}

fn hasValidControlFlow(cg: *llvm.CodeGenerator, func: ast.Function) errors.CodegenError!bool {
    if (std.mem.eql(u8, func.return_type, "void")) {
        return true;
    }
    return cg.control_flow_analyzer.analyzeStatementListEnhanced(func.body.items);
}

pub fn generateCFunctionDeclaration(cg: *llvm.CodeGenerator, c_func: ast.CFunctionDecl) errors.CodegenError!void {
    const func_name_z = utils.dupeZ(cg.allocator, c_func.name);
    defer cg.allocator.free(func_name_z);
    if (c.LLVMGetNamedFunction(@ptrCast(cg.module), func_name_z.ptr)) |existing_func| {
        try cg.functions.put(utils.dupe(u8, cg.allocator, c_func.name), @ptrCast(existing_func));
        return;
    }

    var param_types = std.ArrayList(c.LLVMTypeRef){};
    defer param_types.deinit(cg.allocator);
    const return_type = try cg.getLLVMType(c_func.return_type);
    const uses_sret = c.LLVMGetTypeKind(@ptrCast(return_type)) == c.LLVMStructTypeKind and utils.shouldUseByVal(cg, @ptrCast(return_type));

    if (uses_sret) {
        try param_types.append(cg.allocator, c.LLVMPointerType(@ptrCast(return_type), 0));
    }

    var is_varargs = false;
    for (c_func.parameters.items, 0..) |param, i| {
        if (utils.isVarArgType(param.type_name)) {
            if (i != c_func.parameters.items.len - 1) {
                return errors.CodegenError.TypeMismatch;
            }
            is_varargs = true;
            continue;
        }
        const param_type = try cg.getLLVMType(param.type_name);
        if (c.LLVMGetTypeKind(@ptrCast(param_type)) == c.LLVMStructTypeKind) {
            if (utils.shouldSplitAsVector(cg, @ptrCast(param_type))) {
                const float_type = c.LLVMFloatTypeInContext(@ptrCast(cg.context));
                const vec2_type = c.LLVMVectorType(float_type, 2);
                try param_types.append(cg.allocator, vec2_type);
                try param_types.append(cg.allocator, float_type);
            } else if (utils.shouldUseByVal(cg, @ptrCast(param_type))) {
                try param_types.append(cg.allocator, c.LLVMPointerType(@ptrCast(param_type), 0));
            } else {
                try param_types.append(cg.allocator, @ptrCast(param_type));
            }
        } else {
            try param_types.append(cg.allocator, @ptrCast(param_type));
        }
    }

    const final_return_type = if (uses_sret) c.LLVMVoidTypeInContext(@ptrCast(cg.context)) else return_type;
    const function_type = if (param_types.items.len > 0)
        c.LLVMFunctionType(@ptrCast(final_return_type), param_types.items.ptr, @intCast(param_types.items.len), if (is_varargs) 1 else 0)
    else
        c.LLVMFunctionType(@ptrCast(final_return_type), null, 0, if (is_varargs) 1 else 0);

    if (cg.functions.get(c_func.name) == null) {
        const llvm_func = c.LLVMAddFunction(@ptrCast(cg.module), func_name_z.ptr, function_type);

        var attr_idx: u32 = 1;
        if (uses_sret) {
            const sret_attr = c.LLVMCreateTypeAttribute(cg.context, c.LLVMGetEnumAttributeKindForName("sret", 4), @ptrCast(return_type));
            c.LLVMAddAttributeAtIndex(llvm_func, attr_idx, sret_attr);
            const align_attr = c.LLVMCreateEnumAttribute(cg.context, c.LLVMGetEnumAttributeKindForName("align", 5), cg.getAlignmentForType(@ptrCast(return_type)));
            c.LLVMAddAttributeAtIndex(llvm_func, attr_idx, align_attr);
            try cg.sret_functions.put(utils.dupe(u8, cg.allocator, c_func.name), @ptrCast(return_type));
            attr_idx += 1;
        }

        var actual_param_idx: u32 = attr_idx;
        for (c_func.parameters.items) |param| {
            if (utils.isVarArgType(param.type_name)) continue;
            const param_type = try cg.getLLVMType(param.type_name);
            if (c.LLVMGetTypeKind(@ptrCast(param_type)) == c.LLVMStructTypeKind) {
                if (utils.shouldSplitAsVector(cg, @ptrCast(param_type))) {
                    actual_param_idx += 2;
                } else if (utils.shouldUseByVal(cg, @ptrCast(param_type))) {
                    const byval_attr = c.LLVMCreateTypeAttribute(cg.context, c.LLVMGetEnumAttributeKindForName("byval", 5), @ptrCast(param_type));
                    c.LLVMAddAttributeAtIndex(llvm_func, actual_param_idx, byval_attr);
                    const align_attr = c.LLVMCreateEnumAttribute(cg.context, c.LLVMGetEnumAttributeKindForName("align", 5), cg.getAlignmentForType(@ptrCast(param_type)));
                    c.LLVMAddAttributeAtIndex(llvm_func, actual_param_idx, align_attr);
                    actual_param_idx += 1;
                } else {
                    actual_param_idx += 1;
                }
            } else {
                actual_param_idx += 1;
            }
        }

        try cg.functions.put(utils.dupe(u8, cg.allocator, c_func.name), @ptrCast(llvm_func));
        c.LLVMSetLinkage(llvm_func, c.LLVMExternalLinkage);
    }
    try cg.c_function_declarations.put(utils.dupe(u8, cg.allocator, c_func.name), c_func.is_wrapped);
    try cg.function_return_types.put(utils.dupe(u8, cg.allocator, c_func.name), c_func.return_type);
    if (is_varargs) {
        const last_param = c_func.parameters.items[c_func.parameters.items.len - 1];
        if (utils.getVarArgType(last_param.type_name)) |var_type| {
            try cg.function_vararg_types.put(utils.dupe(u8, cg.allocator, c_func.name), var_type);
        } else {
            try cg.function_vararg_types.put(utils.dupe(u8, cg.allocator, c_func.name), "_");
        }
    }
}

pub fn generateFunctionCall(cg: *llvm.CodeGenerator, call: ast.FunctionCall, expected_return_type: ?[]const u8) errors.CodegenError!c.LLVMValueRef {
    if (call.is_libc and std.mem.eql(u8, call.name, "va_arg")) {
        if (call.args.items.len != 2) return errors.CodegenError.TypeMismatch;
        const vl_ptr = try cg.generateExpression(call.args.items[0]);

        const type_arg = call.args.items[1];
        if (type_arg.data != .string_literal) return errors.CodegenError.TypeMismatch;
        const type_name = type_arg.data.string_literal.value;
        const target_type = try cg.getLLVMType(type_name);

        const i32_ty = c.LLVMInt32TypeInContext(cg.context);
        const i8_ptr_ty = c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0);
        var struct_elems = [_]c.LLVMTypeRef{ i32_ty, i32_ty, i8_ptr_ty, i8_ptr_ty };
        const va_list_ty = c.LLVMStructTypeInContext(cg.context, &struct_elems, 4, 0);
        const va_list_ptr_ty = c.LLVMPointerType(va_list_ty, 0);

        const vl = c.LLVMBuildBitCast(cg.builder, vl_ptr, va_list_ptr_ty, "vl_cast");

        const type_kind = c.LLVMGetTypeKind(target_type);
        const is_float = type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind;

        const current_block = c.LLVMGetInsertBlock(cg.builder);
        const func = c.LLVMGetBasicBlockParent(current_block);
        const in_reg_bb = c.LLVMAppendBasicBlockInContext(cg.context, func, "va_arg_in_reg");
        const in_mem_bb = c.LLVMAppendBasicBlockInContext(cg.context, func, "va_arg_in_mem");
        const cont_bb = c.LLVMAppendBasicBlockInContext(cg.context, func, "va_arg_cont");

        const offset_idx: u32 = if (is_float) 1 else 0;
        const offset_ptr = c.LLVMBuildStructGEP2(cg.builder, va_list_ty, vl, offset_idx, "offset_ptr");
        const offset = c.LLVMBuildLoad2(cg.builder, i32_ty, offset_ptr, "offset");

        const limit = c.LLVMConstInt(i32_ty, if (is_float) 176 else 48, 0);
        const fits_in_reg = c.LLVMBuildICmp(cg.builder, c.LLVMIntULT, offset, limit, "fits_in_reg");
        _ = c.LLVMBuildCondBr(cg.builder, fits_in_reg, in_reg_bb, in_mem_bb);

        c.LLVMPositionBuilderAtEnd(cg.builder, in_reg_bb);
        const reg_save_area_ptr = c.LLVMBuildStructGEP2(cg.builder, va_list_ty, vl, 3, "reg_save_area_ptr");
        const reg_save_area = c.LLVMBuildLoad2(cg.builder, i8_ptr_ty, reg_save_area_ptr, "reg_save_area");
        var idx_list = [_]c.LLVMValueRef{offset};
        const reg_addr = c.LLVMBuildGEP2(cg.builder, c.LLVMInt8TypeInContext(cg.context), reg_save_area, &idx_list, 1, "reg_addr");

        const reg_val_ptr = c.LLVMBuildBitCast(cg.builder, reg_addr, c.LLVMPointerType(target_type, 0), "reg_val_ptr");
        const reg_val = c.LLVMBuildLoad2(cg.builder, target_type, reg_val_ptr, "reg_val");

        const next_offset = c.LLVMBuildAdd(cg.builder, offset, c.LLVMConstInt(i32_ty, if (is_float) 16 else 8, 0), "next_offset");
        _ = c.LLVMBuildStore(cg.builder, next_offset, offset_ptr);
        _ = c.LLVMBuildBr(cg.builder, cont_bb);

        c.LLVMPositionBuilderAtEnd(cg.builder, in_mem_bb);
        const overflow_arg_area_ptr = c.LLVMBuildStructGEP2(cg.builder, va_list_ty, vl, 2, "overflow_arg_area_ptr");
        const overflow_arg_area = c.LLVMBuildLoad2(cg.builder, i8_ptr_ty, overflow_arg_area_ptr, "overflow_arg_area");

        const mem_val_ptr = c.LLVMBuildBitCast(cg.builder, overflow_arg_area, c.LLVMPointerType(target_type, 0), "mem_val_ptr");
        const mem_val = c.LLVMBuildLoad2(cg.builder, target_type, mem_val_ptr, "mem_val");

        const step = c.LLVMConstInt(c.LLVMInt64TypeInContext(cg.context), 8, 0);
        var idx_step = [_]c.LLVMValueRef{step};
        const next_overflow = c.LLVMBuildGEP2(cg.builder, c.LLVMInt8TypeInContext(cg.context), overflow_arg_area, &idx_step, 1, "next_overflow");
        _ = c.LLVMBuildStore(cg.builder, next_overflow, overflow_arg_area_ptr);
        _ = c.LLVMBuildBr(cg.builder, cont_bb);

        c.LLVMPositionBuilderAtEnd(cg.builder, cont_bb);
        const phi = c.LLVMBuildPhi(cg.builder, target_type, "va_arg_result");
        var phi_vals = [_]c.LLVMValueRef{ reg_val, mem_val };
        var phi_blocks = [_]c.LLVMBasicBlockRef{ in_reg_bb, in_mem_bb };
        c.LLVMAddIncoming(phi, &phi_vals, &phi_blocks, 2);

        return phi;
    }

    if (call.is_libc and (std.mem.eql(u8, call.name, "va_start") or std.mem.eql(u8, call.name, "va_end"))) {
        if (call.args.items.len != 1) return errors.CodegenError.TypeMismatch;
        const arg_val = try cg.generateExpression(call.args.items[0]);
        const i8_ptr_ty = c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0);
        const casted_arg = c.LLVMBuildBitCast(cg.builder, arg_val, i8_ptr_ty, "va_list_cast");

        const void_ty = c.LLVMVoidTypeInContext(cg.context);
        var param_types = [_]c.LLVMTypeRef{i8_ptr_ty};
        const fn_ty = c.LLVMFunctionType(void_ty, &param_types, 1, 0);

        const intrinsic_name = if (std.mem.eql(u8, call.name, "va_start")) "llvm.va_start" else "llvm.va_end";
        var intrinsic = c.LLVMGetNamedFunction(cg.module, intrinsic_name);
        if (intrinsic == null) {
            intrinsic = c.LLVMAddFunction(cg.module, intrinsic_name, fn_ty);
        }

        var args = [_]c.LLVMValueRef{casted_arg};
        _ = c.LLVMBuildCall2(cg.builder, fn_ty, intrinsic, &args, 1, "");
        return c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0);
    }

    if (call.is_libc and std.mem.eql(u8, call.name, "vararg_len")) {
        if (call.args.items.len != 1) return errors.CodegenError.TypeMismatch;
        const arg = call.args.items[0];
        if (arg.data != .identifier) return errors.CodegenError.TypeMismatch;

        const param_name = arg.data.identifier.name;
        const count_var_name = try std.fmt.allocPrint(cg.allocator, "__vararg_count_{s}", .{param_name});
        defer cg.allocator.free(count_var_name);

        if (variables.getVariable(cg, count_var_name)) |count_info| {
            return c.LLVMBuildLoad2(cg.builder, count_info.type_ref, count_info.value, "vararg_len");
        }
        return errors.CodegenError.UndefinedVariable;
    }

    if (call.is_libc and std.mem.eql(u8, call.name, "vararg_get")) {
        if (call.args.items.len != 2) return errors.CodegenError.TypeMismatch;
        const arg = call.args.items[0];
        if (arg.data != .identifier) return errors.CodegenError.TypeMismatch;

        const param_name = arg.data.identifier.name;
        const index_expr = try cg.generateExpression(call.args.items[1]);

        const ptr_var_name = try std.fmt.allocPrint(cg.allocator, "__vararg_ptr_{s}", .{param_name});
        defer cg.allocator.free(ptr_var_name);

        if (variables.getVariable(cg, ptr_var_name)) |ptr_info| {
            if (std.mem.startsWith(u8, ptr_info.type_name, "ptr<") and std.mem.endsWith(u8, ptr_info.type_name, ">")) {
                const elem_type_name = ptr_info.type_name[4 .. ptr_info.type_name.len - 1];
                const element_type = try cg.getLLVMType(elem_type_name);
                const ptr_val = c.LLVMBuildLoad2(cg.builder, ptr_info.type_ref, ptr_info.value, "vararg_ptr");
                var indices = [_]c.LLVMValueRef{index_expr};
                const element_ptr = c.LLVMBuildGEP2(cg.builder, element_type, ptr_val, &indices, 1, "vararg_element_ptr");
                return c.LLVMBuildLoad2(cg.builder, element_type, element_ptr, "vararg_element");
            }
        }
        return errors.CodegenError.UndefinedVariable;
    }

    var func: c.LLVMValueRef = undefined;
    var resolved_name: []const u8 = undefined;

    var arg_values = std.ArrayList(c.LLVMValueRef){};
    defer arg_values.deinit(cg.allocator);
    var arg_types = std.ArrayList(c.LLVMTypeRef){};
    defer arg_types.deinit(cg.allocator);

    for (call.args.items) |arg| {
        const val = try cg.generateExpression(arg);
        try arg_values.append(cg.allocator, val);
        try arg_types.append(cg.allocator, c.LLVMTypeOf(val));
    }

    var final_best_match: ?structs.FunctionOverload = null;
    var final_best_substitutions: ?std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage) = null;
    var found_overload = false;
    if (cg.function_overloads.get(call.name)) |overloads| {
        var best_score: i32 = -1;

        for (overloads.items) |candidate| {
            const param_count = candidate.func_node.parameters.items.len;
            var is_varargs = false;
            if (candidate.func_node.parameters.items.len > 0) {
                const last = candidate.func_node.parameters.items[candidate.func_node.parameters.items.len - 1];
                if (utils.isVarArgType(last.type_name)) {
                    is_varargs = true;
                }
            }

            if (is_varargs) {
                if (arg_values.items.len < param_count - 1) continue;
            } else {
                if (arg_values.items.len != param_count) continue;
            }

            var score: i32 = 0;
            var substitutions = std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(cg.allocator);
            var mismatch = false;

            if (candidate.is_template and expected_return_type != null) {
                const actual_ret_type_name = getActualReturnType(candidate.func_node.return_type);
                if (utils.getLLVMTypeSilent(cg, actual_ret_type_name)) |_| {} else |_| {
                    try substitutions.put(utils.dupe(u8, cg.allocator, actual_ret_type_name), expected_return_type.?);
                    score += 2;
                }
            }

            for (candidate.func_node.parameters.items, 0..) |param, i| {
                if (utils.isVarArgType(param.type_name)) break;
                if (i >= arg_types.items.len) break;

                const arg_type = arg_types.items[i];
                const type_name = param.type_name;

                if (std.mem.startsWith(u8, type_name, "[") and std.mem.endsWith(u8, type_name, "]")) {
                    const inner = type_name[1 .. type_name.len - 1];
                    if (substitutions.get(inner)) |prev_type_name| {
                        const expected_type = try cg.getLLVMType(prev_type_name);
                        if (arg_type == expected_type) {
                            score += 10;
                        } else {
                            mismatch = true;
                            break;
                        }
                    } else {
                        var it = std.mem.tokenizeAny(u8, inner, ", ");
                        var matched_one = false;
                        while (it.next()) |opt_raw| {
                            const opt = std.mem.trim(u8, opt_raw, " \t");
                            if (cg.getLLVMType(opt)) |opt_ty| {
                                if (opt_ty == arg_type) {
                                    matched_one = true;
                                    try substitutions.put(param.name, utils.getTypeNameFromLLVMType(cg, arg_type));
                                    score += 5;
                                    break;
                                }
                            } else |_| {
                                if (substitutions.get(opt)) |ref_type_name| {
                                    const ref_ty = try cg.getLLVMType(ref_type_name);
                                    if (ref_ty == arg_type) {
                                        matched_one = true;
                                        try substitutions.put(param.name, ref_type_name);
                                        score += 10;
                                        break;
                                    }
                                }
                            }
                        }
                        if (!matched_one) {
                            mismatch = true;
                            break;
                        }
                    }
                } else if (std.mem.startsWith(u8, type_name, ">>")) {} else {
                    var resolved_param_ty: ?c.LLVMTypeRef = if (utils.getLLVMTypeSilent(cg, type_name)) |ty| ty else |_| null;
                    if (resolved_param_ty == null) {
                        if (substitutions.get(type_name)) |sub_name| {
                            resolved_param_ty = if (utils.getLLVMTypeSilent(cg, sub_name)) |ty| ty else |_| null;
                        }
                    }

                    if (resolved_param_ty) |param_ty| {
                        if (param_ty == arg_type) {
                            score += 5;
                        } else {
                            const arg_kind = c.LLVMGetTypeKind(arg_type);
                            const param_kind = c.LLVMGetTypeKind(param_ty);
                            const arg_node = call.args.items[i];

                            const is_number_literal = arg_node.data == .number_literal;
                            const is_float_literal = arg_node.data == .float_literal;

                            if (is_float_literal and (param_kind == c.LLVMFloatTypeKind or param_kind == c.LLVMDoubleTypeKind or param_kind == c.LLVMHalfTypeKind)) {
                                score += 4;
                            } else if (is_number_literal and param_kind == c.LLVMIntegerTypeKind) {
                                score += 4;
                            } else if (is_number_literal and (param_kind == c.LLVMFloatTypeKind or param_kind == c.LLVMDoubleTypeKind)) {
                                score += 3;
                            } else if ((arg_kind == c.LLVMFloatTypeKind or arg_kind == c.LLVMHalfTypeKind) and param_kind == c.LLVMDoubleTypeKind) {
                                score += 3;
                            } else if (arg_kind == c.LLVMDoubleTypeKind and param_kind == c.LLVMFloatTypeKind) {
                                score += 2;
                            } else if (arg_kind == c.LLVMIntegerTypeKind and param_kind == c.LLVMIntegerTypeKind) {
                                score += 2;
                            } else if (arg_kind == c.LLVMIntegerTypeKind and (param_kind == c.LLVMFloatTypeKind or param_kind == c.LLVMDoubleTypeKind)) {
                                score += 3;
                            } else if ((arg_kind == c.LLVMFloatTypeKind or arg_kind == c.LLVMDoubleTypeKind) and param_kind == c.LLVMIntegerTypeKind) {
                                mismatch = true;
                                break;
                            } else {
                                mismatch = true;
                                break;
                            }
                        }
                    } else {
                        if (candidate.is_template) {
                            const concrete_type_name = utils.getTypeNameFromLLVMType(cg, arg_type);
                            try substitutions.put(utils.dupe(u8, cg.allocator, type_name), concrete_type_name);
                            score += 5;
                        } else {
                            mismatch = true;
                            break;
                        }
                    }
                    try substitutions.put(param.name, utils.getTypeNameFromLLVMType(cg, arg_type));
                }
            }

            if (!mismatch) {
                if (score > best_score) {
                    best_score = score;
                    final_best_match = candidate;
                    if (final_best_substitutions) |*map| map.deinit();
                    final_best_substitutions = substitutions;
                } else {
                    substitutions.deinit();
                }
            } else {
                substitutions.deinit();
            }
        }

        if (final_best_match) |match| {
            found_overload = true;
            if (match.is_template) {
                const old_subs = cg.template_substitutions;
                cg.template_substitutions = final_best_substitutions.?;

                var resolved_param_type_names = std.ArrayList([]const u8){};
                defer resolved_param_type_names.deinit(cg.allocator);
                for (match.func_node.parameters.items) |p| {
                    if (utils.isVarArgType(p.type_name)) continue;
                    if (final_best_substitutions.?.get(p.type_name)) |resolved| {
                        try resolved_param_type_names.append(cg.allocator, resolved);
                    } else {
                        try resolved_param_type_names.append(cg.allocator, p.type_name);
                    }
                }

                const mangled = try getMangledName(cg.allocator, match.func_node.name, resolved_param_type_names.items, cg);

                if (cg.functions.get(mangled)) |existing| {
                    func = existing;
                    resolved_name = mangled;
                    cg.template_substitutions = old_subs;
                    if (final_best_substitutions) |*map| map.deinit();
                } else {
                    try cg.declareFunction(match.func_node);
                    try cg.pending_template_instantiations.append(cg.allocator, structs.TemplateInstantiation{
                        .func_node = match.func_node,
                        .substitutions = final_best_substitutions.?,
                    });

                    final_best_substitutions = null;

                    cg.template_substitutions = old_subs;

                    func = cg.functions.get(mangled) orelse return errors.CodegenError.UndefinedFunction;
                    resolved_name = mangled;
                }
            } else {
                var cand_param_type_names = std.ArrayList([]const u8){};
                defer cand_param_type_names.deinit(cg.allocator);
                for (match.func_node.parameters.items) |p| {
                    if (!utils.isVarArgType(p.type_name)) try cand_param_type_names.append(cg.allocator, p.type_name);
                }
                const mangled = try getMangledName(cg.allocator, match.func_node.name, cand_param_type_names.items, cg);
                func = cg.functions.get(mangled) orelse return errors.CodegenError.UndefinedFunction;
                resolved_name = mangled;
                if (final_best_substitutions) |*map| map.deinit();
            }
        } else {
            if (final_best_substitutions) |*map| map.deinit();
        }
    }

    var func_type_set: ?c.LLVMTypeRef = null;
    if (!found_overload) {
        resolved_name = call.name;
        if (call.is_libc) {
            var found_ext = false;
            var func_iter = c.LLVMGetFirstFunction(cg.module);
            while (func_iter != null) : (func_iter = c.LLVMGetNextFunction(func_iter)) {
                if (c.LLVMGetValueName(func_iter)) |name_ptr| {
                    if (std.mem.eql(u8, std.mem.span(name_ptr), call.name)) {
                        func = func_iter;
                        found_ext = true;
                        break;
                    }
                }
            }
            if (!found_ext) {
                if (utils.LIBC_FUNCTIONS.get(call.name)) |sig| {
                    func = try createFunctionFromSignature(cg, call.name, sig);
                } else {
                    func = try createGenericLibcFunction(cg, call.name);
                }
            }
        } else {
            if (cg.functions.get(call.name)) |declared_func| {
                func = declared_func;
            } else if (llvm.CodeGenerator.getVariable(cg, call.name)) |var_info| {
                func = c.LLVMBuildLoad2(cg.builder, var_info.type_ref, var_info.value, "load_func_ptr");
                if (std.mem.startsWith(u8, var_info.type_name, "ptr<") and std.mem.endsWith(u8, var_info.type_name, ">")) {
                    const sig = var_info.type_name[4 .. var_info.type_name.len - 1];
                    func_type_set = try utils.getLLVMFunctionType(cg, sig);
                } else {
                    return errors.CodegenError.UndefinedFunction;
                }
            } else {
                if (cg.function_overloads.get(call.name)) |overloads| {
                    if (overloads.items.len == 1) {
                        const match = overloads.items[0];
                        if (!match.is_template) {
                            var cand_param_type_names = std.ArrayList([]const u8){};
                            defer cand_param_type_names.deinit(cg.allocator);
                            for (match.func_node.parameters.items) |p| {
                                if (!utils.isVarArgType(p.type_name)) try cand_param_type_names.append(cg.allocator, p.type_name);
                            }
                            const mangled = try getMangledName(cg.allocator, match.func_node.name, cand_param_type_names.items, cg);
                            defer cg.allocator.free(mangled);
                            if (cg.functions.get(mangled)) |f| {
                                func = f;
                                found_overload = true;
                                final_best_match = match;
                            }
                        }
                    }
                }
                if (!found_overload) return errors.CodegenError.UndefinedFunction;
            }
        }
    }
    const func_type = if (func_type_set) |ft| ft else c.LLVMGlobalGetValueType(func);
    const param_count_llvm = c.LLVMCountParamTypes(func_type);
    var func_param_types = std.ArrayList(c.LLVMTypeRef){};
    defer func_param_types.deinit(cg.allocator);
    try func_param_types.resize(cg.allocator, param_count_llvm);
    if (param_count_llvm > 0) c.LLVMGetParamTypes(func_type, func_param_types.items.ptr);
    var final_args = std.ArrayList(c.LLVMValueRef){};
    defer final_args.deinit(cg.allocator);
    const uses_sret = cg.sret_functions.get(resolved_name) != null;
    var sret_alloca: ?c.LLVMValueRef = null;
    if (uses_sret) {
        if (cg.sret_functions.get(resolved_name)) |pointee_type| {
            sret_alloca = c.LLVMBuildAlloca(cg.builder, pointee_type, "sret_temp");
            try final_args.append(cg.allocator, sret_alloca.?);
        }
    }
    var arg_idx: usize = 0;
    var param_idx: usize = if (uses_sret) 1 else 0;

    var sig_param_it: ?std.mem.TokenIterator(u8, .any) = null;
    if (func_type_set != null) {
        if (llvm.CodeGenerator.getVariable(cg, call.name)) |var_info| {
            if (std.mem.startsWith(u8, var_info.type_name, "ptr<") and std.mem.endsWith(u8, var_info.type_name, ">")) {
                const sig = var_info.type_name[4 .. var_info.type_name.len - 1];
                if (std.mem.indexOfScalar(u8, sig, '(')) |lp| {
                    if (std.mem.lastIndexOfScalar(u8, sig, ')')) |rp| {
                        sig_param_it = std.mem.tokenizeAny(u8, sig[lp + 1 .. rp], ",");
                    }
                }
            }
        }
    }

    const ByValInfo = struct { idx: usize, ty: c.LLVMTypeRef };
    var byval_args = std.ArrayList(ByValInfo){};
    defer byval_args.deinit(cg.allocator);

    for (arg_values.items) |val| {
        var param_type_name: ?[]const u8 = null;
        if (final_best_match) |match| {
            if (arg_idx < match.func_node.parameters.items.len) {
                param_type_name = match.func_node.parameters.items[arg_idx].type_name;
            }
        } else if (sig_param_it) |*it| {
            if (it.next()) |tn_raw| {
                param_type_name = std.mem.trim(u8, tn_raw, " \t");
            }
        }

        if (param_idx < func_param_types.items.len) {
            const expected = func_param_types.items[param_idx];
            var final_arg = val;
            var processed_byval = false;

            if (param_type_name) |tn| {
                if (utils.getVarArgType(tn)) |et| {
                    const elem_ty = try cg.getLLVMType(et);
                    const bundle_count = arg_values.items.len - arg_idx;
                    const array_ty = c.LLVMArrayType(elem_ty, @intCast(bundle_count));
                    const array_alloca = c.LLVMBuildAlloca(@ptrCast(cg.builder), array_ty, "typed_vararg_array");

                    for (arg_idx..arg_values.items.len) |i| {
                        const arg_val = arg_values.items[i];
                        const casted = try cg.castWithRules(arg_val, elem_ty, call.args.items[i]);
                        var elem_indices = [_]c.LLVMValueRef{
                            c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0),
                            c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), @intCast(i - arg_idx), 0),
                        };
                        const elem_ptr = c.LLVMBuildGEP2(@ptrCast(cg.builder), array_ty, array_alloca, &elem_indices, 2, "vararg_elem_ptr");
                        _ = c.LLVMBuildStore(@ptrCast(cg.builder), casted, elem_ptr);
                    }

                    var head_indices = [_]c.LLVMValueRef{
                        c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0),
                        c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0),
                    };
                    const ptr_to_array = c.LLVMBuildGEP2(@ptrCast(cg.builder), array_ty, array_alloca, &head_indices, 2, "vararg_ptr");

                    try final_args.append(cg.allocator, ptr_to_array);
                    try final_args.append(cg.allocator, c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), @intCast(bundle_count), 0));
                    break;
                }

                if (utils.isByValType(cg, tn)) {
                    processed_byval = true;
                    const struct_ty = try cg.getLLVMType(tn);
                    if (call.args.items[arg_idx].data == .identifier) {
                        const ident_name = call.args.items[arg_idx].data.identifier.name;
                        if (llvm.CodeGenerator.getVariable(cg, ident_name)) |var_info| {
                            final_arg = @ptrCast(var_info.value);
                        } else {
                            const temp = c.LLVMBuildAlloca(@ptrCast(cg.builder), @ptrCast(struct_ty), "byval_tmp");
                            _ = c.LLVMBuildStore(@ptrCast(cg.builder), val, temp);
                            final_arg = temp;
                        }
                    } else {
                        const temp = c.LLVMBuildAlloca(@ptrCast(cg.builder), @ptrCast(struct_ty), "byval_tmp");
                        _ = c.LLVMBuildStore(@ptrCast(cg.builder), val, temp);
                        final_arg = temp;
                    }
                    try byval_args.append(cg.allocator, .{ .idx = param_idx, .ty = @ptrCast(struct_ty) });
                }
            }

            if (!processed_byval) {
                final_arg = try cg.castWithRules(val, expected, call.args.items[arg_idx]);
            }
            try final_args.append(cg.allocator, final_arg);
            param_idx += 1;
        } else {
            const ty = c.LLVMTypeOf(val);
            const kind = c.LLVMGetTypeKind(ty);
            if (kind == c.LLVMFloatTypeKind) {
                const f64_type = c.LLVMDoubleTypeInContext(cg.context);
                const promoted = c.LLVMBuildFPExt(cg.builder, val, f64_type, "vararg_promotion");
                try final_args.append(cg.allocator, promoted);
            } else if (kind == c.LLVMDoubleTypeKind) {
                try final_args.append(cg.allocator, val);
            } else if (kind == c.LLVMIntegerTypeKind) {
                const bits = c.LLVMGetIntTypeWidth(ty);
                if (bits < 32) {
                    const arg_node = call.args.items[arg_idx];
                    const type_name = try cg.inferType(arg_node);
                    const i32_type = c.LLVMInt32TypeInContext(cg.context);
                    const promoted = if (utils.isUnsignedType(type_name))
                        c.LLVMBuildZExt(cg.builder, val, i32_type, "vararg_promotion")
                    else
                        c.LLVMBuildSExt(cg.builder, val, i32_type, "vararg_promotion");
                    try final_args.append(cg.allocator, promoted);
                } else {
                    try final_args.append(cg.allocator, val);
                }
            } else {
                try final_args.append(cg.allocator, val);
            }
        }
        arg_idx += 1;
    }
    const call_inst = if (final_args.items.len > 0)
        c.LLVMBuildCall2(cg.builder, func_type, func, final_args.items.ptr, @intCast(final_args.items.len), "")
    else
        c.LLVMBuildCall2(cg.builder, func_type, func, null, 0, "");

    for (byval_args.items) |info| {
        const byval_attr = c.LLVMCreateTypeAttribute(cg.context, c.LLVMGetEnumAttributeKindForName("byval", 5), info.ty);
        c.LLVMAddAttributeAtIndex(call_inst, @intCast(info.idx + 1), byval_attr);
    }

    if (uses_sret and sret_alloca != null) {
        if (cg.sret_functions.get(resolved_name)) |pt| {
            return c.LLVMBuildLoad2(cg.builder, pt, sret_alloca.?, "sret_res");
        }
    }
    return call_inst;
}

pub fn libcTypeToLLVM(cg: *llvm.CodeGenerator, libc_type: utils.LibcType) c.LLVMTypeRef {
    return switch (libc_type) {
        .void_type => c.LLVMVoidTypeInContext(@ptrCast(cg.context)),
        .int_type => c.LLVMInt32TypeInContext(@ptrCast(cg.context)),
        .char_ptr_type => c.LLVMPointerType(c.LLVMInt8TypeInContext(@ptrCast(cg.context)), 0),
        .size_t_type => c.LLVMInt64TypeInContext(@ptrCast(cg.context)),
        .file_ptr_type => c.LLVMPointerType(c.LLVMInt8TypeInContext(@ptrCast(cg.context)), 0),
        .long_type => c.LLVMInt64TypeInContext(@ptrCast(cg.context)),
        .double_type => c.LLVMDoubleTypeInContext(@ptrCast(cg.context)),
    };
}

fn prepareArgumentForLibcCall(cg: *llvm.CodeGenerator, arg_value: c.LLVMValueRef, func_name: []const u8, arg_index: usize) c.LLVMValueRef {
    if (std.mem.eql(u8, func_name, "printf") or
        std.mem.eql(u8, func_name, "sprintf") or
        std.mem.eql(u8, func_name, "snprintf") or
        std.mem.eql(u8, func_name, "fprintf"))
    {
        if (arg_index > 0) {
            const arg_type = c.LLVMTypeOf(arg_value);
            const arg_kind = c.LLVMGetTypeKind(arg_type);

            if (arg_kind == c.LLVMIntegerTypeKind) {
                const width = c.LLVMGetIntTypeWidth(arg_type);
                if (width == 64) {
                    return arg_value;
                } else {
                    const i32_type = c.LLVMInt32TypeInContext(cg.context);
                    return cg.castToType(arg_value, i32_type);
                }
            } else if (arg_kind == c.LLVMFloatTypeKind or arg_kind == c.LLVMHalfTypeKind) {
                const double_type = c.LLVMDoubleTypeInContext(cg.context);
                return c.LLVMBuildFPExt(cg.builder, arg_value, double_type, "fpext");
            }
        }
    }

    return arg_value;
}
