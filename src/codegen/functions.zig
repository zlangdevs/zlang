const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
const llvm = @import("llvm.zig");
const variables = @import("variables.zig");

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

pub fn declareFunction(cg: *llvm.CodeGenerator, func: ast.Function) errors.CodegenError!void {
    var param_types = std.ArrayList(c.LLVMTypeRef){};
    defer param_types.deinit(cg.allocator);

    for (func.parameters.items) |param| {
        const param_type = try cg.getLLVMType(param.type_name);
        if (c.LLVMGetTypeKind(@ptrCast(param_type)) == c.LLVMStructTypeKind and utils.shouldUseByVal(cg, @ptrCast(param_type))) {
            try param_types.append(cg.allocator, c.LLVMPointerType(@ptrCast(param_type), 0));
        } else {
            try param_types.append(cg.allocator, @ptrCast(param_type));
        }
    }

    const return_type = try cg.getLLVMType(func.return_type);
    const function_type = if (param_types.items.len > 0)
        c.LLVMFunctionType(@ptrCast(return_type), param_types.items.ptr, @intCast(param_types.items.len), 0)
    else
        c.LLVMFunctionType(@ptrCast(return_type), null, 0, 0);

    const func_name_z = utils.dupeZ(cg.allocator, func.name);
    defer cg.allocator.free(func_name_z);

    const llvm_func = c.LLVMAddFunction(@ptrCast(cg.module), func_name_z.ptr, function_type);

    for (func.parameters.items, 0..) |param, i| {
        const param_type = try cg.getLLVMType(param.type_name);
        if (c.LLVMGetTypeKind(@ptrCast(param_type)) == c.LLVMStructTypeKind and utils.shouldUseByVal(cg, @ptrCast(param_type))) {
            const byval_attr = c.LLVMCreateTypeAttribute(cg.context, c.LLVMGetEnumAttributeKindForName("byval", 5), @ptrCast(param_type));
            c.LLVMAddAttributeAtIndex(llvm_func, @intCast(i + 1), byval_attr);
            const align_attr = c.LLVMCreateEnumAttribute(cg.context, c.LLVMGetEnumAttributeKindForName("align", 5), cg.getAlignmentForType(@ptrCast(param_type)));
            c.LLVMAddAttributeAtIndex(llvm_func, @intCast(i + 1), align_attr);
        }
    }

    try cg.functions.put(func.name, @ptrCast(llvm_func));
    try cg.regular_functions.put(func.name, true);
}

pub fn generateFunctionBody(cg: *llvm.CodeGenerator, func: ast.Function) errors.CodegenError!void {
    const llvm_func = cg.functions.get(func.name) orelse return errors.CodegenError.UndefinedFunction;
    cg.setCurrentModuleByFunction(func.name);
    cg.current_function = llvm_func;
    cg.current_function_return_type = func.return_type;
    const entry_block = c.LLVMAppendBasicBlockInContext(@ptrCast(cg.context), @ptrCast(llvm_func), "entry");
    c.LLVMPositionBuilderAtEnd(@ptrCast(cg.builder), entry_block);
    cg.label_blocks.clearRetainingCapacity();
    for (cg.pending_gotos.items) |pending| {
        cg.allocator.free(pending.label);
    }
    cg.pending_gotos.clearRetainingCapacity();

    // Reset scopes for new function (keep global scope)
    variables.clearCurrentFunctionScopes(cg);
    // Create a new function-level scope for this function
    try variables.pushScope(cg);
    // Add parameters to the function-level scope
    for (func.parameters.items, 0..) |param, i| {
        const param_value = c.LLVMGetParam(@ptrCast(llvm_func), @intCast(i));
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

    for (c_func.parameters.items) |param| {
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
        c.LLVMFunctionType(@ptrCast(final_return_type), param_types.items.ptr, @intCast(param_types.items.len), 0)
    else
        c.LLVMFunctionType(@ptrCast(final_return_type), null, 0, 0);

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
}

pub fn generateFunctionCall(cg: *llvm.CodeGenerator, call: ast.FunctionCall) errors.CodegenError!c.LLVMValueRef {
    var func: c.LLVMValueRef = undefined;
    if (call.is_libc) {
        var found_external = false;
        var func_iter = c.LLVMGetFirstFunction(cg.module);
        while (func_iter != null) : (func_iter = c.LLVMGetNextFunction(func_iter)) {
            const func_name_ptr = c.LLVMGetValueName(func_iter);
            if (func_name_ptr != null) {
                const func_name_slice = std.mem.span(func_name_ptr);
                if (std.mem.eql(u8, func_name_slice, call.name) and c.LLVMIsDeclaration(func_iter) != 0) {
                    func = func_iter;
                    found_external = true;
                    break;
                }
            }
        }
        if (!found_external) {
            if (utils.LIBC_FUNCTIONS.get(call.name)) |signature| {
                func = try createFunctionFromSignature(cg, call.name, signature);
            } else {
                func = try createGenericLibcFunction(cg, call.name);
            }
        }
    } else {
        var found_external = false;
        var func_iter = c.LLVMGetFirstFunction(cg.module);
        while (func_iter != null) : (func_iter = c.LLVMGetNextFunction(func_iter)) {
            const func_name_ptr = c.LLVMGetValueName(func_iter);
            if (func_name_ptr != null) {
                const func_name_slice = std.mem.span(func_name_ptr);
                if (std.mem.eql(u8, func_name_slice, call.name) and c.LLVMIsDeclaration(func_iter) != 0) {
                    if (cg.functions.get(call.name)) |declared_func| {
                        func = declared_func;
                    } else {
                        func = func_iter;
                        found_external = true;
                    }
                    break;
                }
            }
        }
        if (!found_external) {
            if (cg.functions.get(call.name)) |declared_func| {
                if (cg.regular_functions.get(call.name) == null) {
                    if (cg.c_function_declarations.get(call.name)) |is_wrapped| {
                        if (!is_wrapped) {
                            // Pure C function declared with "fun @name" cannot be called without @
                            return errors.CodegenError.UndefinedFunction;
                        }
                    }
                }
                // visibility check for wrapper defined in another module
                if (cg.function_to_module.get(call.name)) |target_mod| {
                    if (!cg.canAccess(cg.current_module_name, target_mod)) {
                        return errors.CodegenError.UndefinedFunction;
                    }
                }
                func = declared_func;
            } else {
                if (llvm.CodeGenerator.getVariable(cg, call.name)) |var_info| {
                    const callee_ptr = c.LLVMBuildLoad2(@ptrCast(cg.builder), @ptrCast(var_info.type_ref), @ptrCast(var_info.value), "load_fnptr");
                    var fn_ty: c.LLVMTypeRef = undefined;
                    var param_types = std.ArrayList(c.LLVMTypeRef){};
                    defer param_types.deinit(cg.allocator);
                    if (std.mem.startsWith(u8, var_info.type_name, "ptr<") and std.mem.endsWith(u8, var_info.type_name, ">")) {
                        const inner = var_info.type_name[4 .. var_info.type_name.len - 1];
                        if (std.mem.indexOfScalar(u8, inner, '(')) |lp| {
                            if (std.mem.lastIndexOfScalar(u8, inner, ')')) |rp| {
                                if (rp > lp) {
                                    const ret_part = std.mem.trim(u8, inner[0..lp], " \t");
                                    const args_part_full = inner[lp + 1 .. rp];
                                    var it = std.mem.tokenizeAny(u8, args_part_full, ",");
                                    while (it.next()) |arg_raw| {
                                        const arg_trim = std.mem.trim(u8, arg_raw, " \t");
                                        if (arg_trim.len == 0) continue;
                                        const aty = try cg.getLLVMType(arg_trim);
                                        param_types.append(cg.allocator, aty) catch unreachable;
                                    }
                                    const ret_ty = try cg.getLLVMType(ret_part);
                                    fn_ty = if (param_types.items.len > 0)
                                        c.LLVMFunctionType(@ptrCast(ret_ty), param_types.items.ptr, @intCast(param_types.items.len), 0)
                                    else
                                        c.LLVMFunctionType(@ptrCast(ret_ty), null, 0, 0);
                                } else return errors.CodegenError.TypeMismatch;
                            } else return errors.CodegenError.TypeMismatch;
                        } else return errors.CodegenError.TypeMismatch;
                    } else return errors.CodegenError.TypeMismatch;

                    var args = std.ArrayList(c.LLVMValueRef){};
                    defer args.deinit(cg.allocator);
                    for (call.args.items, 0..) |arg, i| {
                        var v = try cg.generateExpression(arg);
                        if (arg.data == .cast and arg.data.cast.auto) {
                            if (i < param_types.items.len) {
                                const expected_type = param_types.items[i];
                                v = cg.castToType(v, expected_type);
                            }
                        }

                        if (i < param_types.items.len) {
                            v = try cg.castWithRules(v, param_types.items[i], arg);
                        }
                        try args.append(cg.allocator, v);
                    }
                    const res = if (args.items.len > 0)
                        c.LLVMBuildCall2(@ptrCast(cg.builder), fn_ty, @ptrCast(callee_ptr), args.items.ptr, @intCast(args.items.len), "call")
                    else
                        c.LLVMBuildCall2(@ptrCast(cg.builder), fn_ty, @ptrCast(callee_ptr), null, 0, "call");
                    return res;
                }
                // If there is an extern with this name in another module, require visibility
                if (cg.function_to_module.get(call.name)) |target_mod2| {
                    if (!cg.canAccess(cg.current_module_name, target_mod2)) {
                        return errors.CodegenError.UndefinedFunction;
                    }
                }
                return errors.CodegenError.UndefinedFunction;
            }
        }
    }

    const func_type = c.LLVMGlobalGetValueType(func);
    const return_type = c.LLVMGetReturnType(func_type);
    const is_void_return = c.LLVMGetTypeKind(return_type) == c.LLVMVoidTypeKind;

    const param_count = c.LLVMCountParamTypes(func_type);
    var func_param_types = std.ArrayList(c.LLVMTypeRef){};
    defer func_param_types.deinit(cg.allocator);
    if (param_count > 0) {
        try func_param_types.resize(cg.allocator, @as(usize, @intCast(param_count)));
        c.LLVMGetParamTypes(func_type, func_param_types.items.ptr);
    }

    const uses_sret = cg.sret_functions.get(call.name) != null;

    var args = std.ArrayList(c.LLVMValueRef){};
    defer args.deinit(cg.allocator);

    var sret_alloca: ?c.LLVMValueRef = null;
    var sret_type: ?c.LLVMTypeRef = null;
    if (uses_sret) {
        if (cg.sret_functions.get(call.name)) |pointee_type| {
            sret_type = pointee_type;
            sret_alloca = c.LLVMBuildAlloca(cg.builder, pointee_type, "sret_temp");
            try args.append(cg.allocator, sret_alloca.?);
        }
    }

    var param_offset: usize = if (uses_sret) 1 else 0;
    for (call.args.items, 0..) |arg, i| {
        const param_idx = param_offset;
        const should_split_vector = blk: {
            if (param_idx + 1 < @as(usize, @intCast(param_count))) {
                const expected_type = func_param_types.items[param_idx];
                const next_expected_type = func_param_types.items[param_idx + 1];
                if (c.LLVMGetTypeKind(expected_type) == c.LLVMVectorTypeKind and
                    c.LLVMGetTypeKind(next_expected_type) == c.LLVMFloatTypeKind)
                {
                    if (c.LLVMGetVectorSize(expected_type) == 2) {
                        const vec_elem_type = c.LLVMGetElementType(expected_type);
                        if (c.LLVMGetTypeKind(vec_elem_type) == c.LLVMFloatTypeKind) {
                            break :blk true;
                        }
                    }
                }
            }
            break :blk false;
        };

        const should_pass_byval_direct = blk: {
            if (param_idx < @as(usize, @intCast(param_count))) {
                const expected_type = func_param_types.items[param_idx];
                if (c.LLVMGetTypeKind(expected_type) == c.LLVMPointerTypeKind) {
                    if (arg.data == .identifier) {
                        if (llvm.CodeGenerator.getVariable(cg, arg.data.identifier.name)) |var_info| {
                            if (var_info.is_byval_param) {
                                const struct_size = utils.getStructSizeBytes(cg, var_info.type_ref);
                                if (struct_size > 16) break :blk true;
                            }
                        }
                    }
                }
            }
            break :blk false;
        };

        var arg_value: c.LLVMValueRef = undefined;
        if (should_pass_byval_direct) {
            if (arg.data == .identifier) {
                if (llvm.CodeGenerator.getVariable(cg, arg.data.identifier.name)) |var_info| {
                    arg_value = var_info.value;
                }
            }
        } else {
            arg_value = try cg.generateExpression(arg);
            if (arg.data == .cast and arg.data.cast.auto) {
                if (param_idx < @as(usize, @intCast(param_count))) {
                    const expected_type = func_param_types.items[param_idx];
                    arg_value = cg.castToType(arg_value, expected_type);
                }
            }
        }

        if (should_split_vector) {
            const arg_type = c.LLVMTypeOf(arg_value);
            if (c.LLVMGetTypeKind(arg_type) == c.LLVMStructTypeKind) {
                const field0 = c.LLVMBuildExtractValue(cg.builder, arg_value, 0, "v3_x");
                const field1 = c.LLVMBuildExtractValue(cg.builder, arg_value, 1, "v3_y");
                const field2 = c.LLVMBuildExtractValue(cg.builder, arg_value, 2, "v3_z");
                const float_type = c.LLVMFloatTypeInContext(cg.context);
                const vec2_type = c.LLVMVectorType(float_type, 2);
                const undef_vec = c.LLVMGetUndef(vec2_type);
                const vec_with_x = c.LLVMBuildInsertElement(cg.builder, undef_vec, field0, c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0), "");
                const vec2 = c.LLVMBuildInsertElement(cg.builder, vec_with_x, field1, c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 1, 0), "");
                try args.append(cg.allocator, vec2);
                try args.append(cg.allocator, field2);
                param_offset += 2;
                continue;
            }
        }

        if (call.is_libc) {
            if (utils.LIBC_FUNCTIONS.get(call.name)) |signature| {
                if (i < signature.param_types.len) {
                    const expected_type = libcTypeToLLVM(cg, signature.param_types[i]);
                    arg_value = cg.castToType(arg_value, expected_type);
                }
            }
        } else {
            if (param_idx < @as(usize, @intCast(param_count))) {
                if (!should_pass_byval_direct) {
                    const expected_type = func_param_types.items[param_idx];
                    const arg_type = c.LLVMTypeOf(arg_value);

                    if (c.LLVMGetTypeKind(expected_type) == c.LLVMPointerTypeKind) {
                        if (c.LLVMGetTypeKind(arg_type) == c.LLVMStructTypeKind) {
                            const struct_size = utils.getStructSizeBytes(cg, arg_type);
                            if (struct_size > 16) {
                                const temp_alloca = c.LLVMBuildAlloca(cg.builder, arg_type, "byval_temp");
                                _ = c.LLVMBuildStore(cg.builder, arg_value, temp_alloca);
                                arg_value = temp_alloca;
                            }
                        }
                    } else if (c.LLVMGetTypeKind(expected_type) == c.LLVMStructTypeKind) {
                        if (c.LLVMGetTypeKind(arg_type) == c.LLVMPointerTypeKind) {
                            if (arg.data == .struct_initializer) {
                                arg_value = c.LLVMBuildLoad2(cg.builder, expected_type, arg_value, "load_struct");
                            }
                        } else {
                            arg_value = try cg.castWithRules(arg_value, expected_type, arg);
                        }
                    } else {
                        arg_value = try cg.castWithRules(arg_value, expected_type, arg);
                    }
                }
            }
        }

        if (call.is_libc) {
            arg_value = prepareArgumentForLibcCall(cg, arg_value, call.name, i);
        }

        try args.append(cg.allocator, arg_value);
        param_offset += 1;
    }

    const needs_result_name = !is_void_return or uses_sret;
    const call_name = if (needs_result_name) call.name else "";
    const call_name_z = utils.dupeZ(cg.allocator, call_name);
    defer cg.allocator.free(call_name_z);

    const call_result = if (args.items.len > 0)
        c.LLVMBuildCall2(cg.builder, func_type, func, args.items.ptr, @as(c_uint, @intCast(args.items.len)), if (uses_sret) "" else call_name_z.ptr)
    else
        c.LLVMBuildCall2(cg.builder, func_type, func, null, 0, if (uses_sret) "" else call_name_z.ptr);

    if (uses_sret and sret_alloca != null and sret_type != null) {
        const result = c.LLVMBuildLoad2(cg.builder, sret_type.?, sret_alloca.?, call_name_z.ptr);
        return result;
    }

    return call_result;
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

const structs = @import("structs.zig");
