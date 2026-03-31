const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
const structs = @import("structs.zig");
const variables = @import("variables.zig");
const strings = @import("strings.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

const CodeGenerator = @import("llvm.zig").CodeGenerator;

pub fn containsArrayIndex(self: *CodeGenerator, node: *ast.Node) !bool {
    switch (node.data) {
        .array_index => return true,
        .qualified_identifier => |qual_id| return try containsArrayIndex(self, qual_id.base),
        else => return false,
    }
}

pub fn extractArrayIndex(self: *CodeGenerator, node: *ast.Node) !ast.ArrayIndex {
    switch (node.data) {
        .array_index => |arr_idx| return arr_idx,
        .qualified_identifier => |qual_id| return try extractArrayIndex(self, qual_id.base),
        else => return errors.CodegenError.TypeMismatch,
    }
}

pub fn collectArrayIndices(self: *CodeGenerator, node: *ast.Node, indices: *std.ArrayList(c.LLVMValueRef)) !*ast.Node {
    switch (node.data) {
        .array_index => |arr_idx| {
            const base = try collectArrayIndices(self, arr_idx.array, indices);
            var index_value = try self.generateExpression(arr_idx.index);
            index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
            try indices.append(self.allocator, index_value);
            return base;
        },
        else => return node,
    }
}

pub fn generateArrayElementFieldAssignment(self: *CodeGenerator, array_index: ast.ArrayIndex, field_path: []const u8, value_expr: *ast.Node) errors.CodegenError!void {
    const array_name = try self.getBaseIdentifierName(array_index.array);
    defer self.allocator.free(array_name);
    const array_var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;
    var index_value = try self.generateExpression(array_index.index);

    var element_type: c.LLVMTypeRef = undefined;
    var array_element_ptr: c.LLVMValueRef = undefined;

    if (std.mem.startsWith(u8, array_var_info.type_name, "ptr<") and std.mem.endsWith(u8, array_var_info.type_name, ">")) {
        const element_type_name = array_var_info.type_name[4 .. array_var_info.type_name.len - 1];
        element_type = try self.getLLVMType(element_type_name);
        const index64 = self.castToType(index_value, c.LLVMInt64TypeInContext(self.context));
        const loaded_ptr = c.LLVMBuildLoad2(self.builder, array_var_info.type_ref, array_var_info.value, "load_ptr_for_field_assign");
        var ptr_indices = [_]c.LLVMValueRef{index64};
        array_element_ptr = c.LLVMBuildGEP2(self.builder, element_type, loaded_ptr, &ptr_indices[0], 1, "ptr_struct_element_ptr");
    } else {
        index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
        element_type = c.LLVMGetElementType(@ptrCast(array_var_info.type_ref));
        var array_indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_value };
        array_element_ptr = c.LLVMBuildGEP2(@ptrCast(self.builder), @ptrCast(array_var_info.type_ref), @ptrCast(array_var_info.value), &array_indices[0], 2, "array_element_ptr");
    }

    const element_type_kind = c.LLVMGetTypeKind(element_type);
    if (element_type_kind != c.LLVMStructTypeKind) {
        return errors.CodegenError.TypeMismatch;
    }
    const struct_name = try self.getStructTypeName(@ptrCast(element_type));
    defer self.allocator.free(struct_name);
    try self.generateRecursiveFieldAssignment(array_element_ptr, element_type, struct_name, field_path, value_expr);
}

pub fn generateArrayAssignment(self: *CodeGenerator, arr_ass: ast.ArrayAssignment) errors.CodegenError!void {
    var collected_indices = std.ArrayList(c.LLVMValueRef){};
    defer collected_indices.deinit(self.allocator);

    const base_node = try collectArrayIndices(self, arr_ass.array, &collected_indices);
    var index_value = try self.generateExpression(arr_ass.index);

    var base_ptr: c.LLVMValueRef = undefined;
    var base_type: c.LLVMTypeRef = undefined;
    var base_type_name: []const u8 = "";
    var base_is_const = false;
    var base_ptr_is_direct_value = false;

    if (base_node.data == .qualified_identifier) {
        const pair = try self.getQualifiedFieldPtrAndType(base_node);
        base_ptr = pair.ptr;
        base_type = pair.ty;
        base_type_name = self.getTypeNameFromLLVMType(base_type);

        const root_name = try self.getBaseIdentifierName(base_node);
        defer self.allocator.free(root_name);
        if (CodeGenerator.getVariable(self, root_name)) |root_var| {
            base_is_const = root_var.is_const;
        }
    } else {
        if (base_node.data == .function_call) {
            base_ptr = try self.generateExpression(base_node);
            base_type = c.LLVMTypeOf(base_ptr);
            const call = base_node.data.function_call;
            base_type_name = if (self.function_return_types.get(call.name)) |rt|
                rt
            else if (self.function_asts.get(call.name)) |fn_ast|
                fn_ast.return_type
            else
                try self.inferType(base_node);
            base_is_const = false;
            base_ptr_is_direct_value = true;
        } else {
            const array_name = try self.getBaseIdentifierName(base_node);
            defer self.allocator.free(array_name);
            const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;
            base_ptr = var_info.value;
            base_type = var_info.type_ref;
            base_type_name = var_info.type_name;
            base_is_const = var_info.is_const;
        }
    }

    if (base_is_const) {
        const owner_name = switch (base_node.data) {
            .identifier => |ident| ident.name,
            else => "array",
        };
        self.reportErrorFmt("Cannot modify element of const array '{s}'", .{owner_name}, "Array is declared as const");
        return errors.CodegenError.ConstReassignment;
    }

    const is_pointer_base = std.mem.startsWith(u8, base_type_name, "ptr<") or (base_ptr_is_direct_value and c.LLVMGetTypeKind(base_type) == c.LLVMPointerTypeKind);
    if (is_pointer_base) {
        if (c.LLVMTypeOf(index_value) != c.LLVMInt64TypeInContext(self.context)) {
            index_value = self.castToType(index_value, c.LLVMInt64TypeInContext(self.context));
        }
        try collected_indices.append(self.allocator, index_value);
        const ptr_val = if (base_ptr_is_direct_value)
            base_ptr
        else
            c.LLVMBuildLoad2(self.builder, base_type, base_ptr, "load_ptr_for_assign");
        if (!(std.mem.startsWith(u8, base_type_name, "ptr<") and std.mem.endsWith(u8, base_type_name, ">"))) {
            return errors.CodegenError.TypeMismatch;
        }
        const element_type_name = base_type_name[4 .. base_type_name.len - 1];
        const element_type = try self.getLLVMType(element_type_name);
        const idx = collected_indices.items[collected_indices.items.len - 1];
        var indices = [_]c.LLVMValueRef{idx};
        const element_ptr = c.LLVMBuildGEP2(self.builder, element_type, ptr_val, &indices[0], 1, "ptr_index_assign");
        const expected_ty_name = self.getTypeNameFromLLVMType(element_type);
        const value_raw = try self.generateExpressionWithContext(arr_ass.value, expected_ty_name);
        const final_val = try self.castWithRules(value_raw, element_type, arr_ass.value);
        _ = c.LLVMBuildStore(self.builder, final_val, element_ptr);
        return;
    }

    if (std.mem.startsWith(u8, base_type_name, "[]")) {
        index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
        try collected_indices.append(self.allocator, index_value);

        const element_type_name = base_type_name[2..];
        const element_type = try self.getLLVMType(element_type_name);

        var ptr_indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0) };
        const ptr_in_struct = c.LLVMBuildGEP2(self.builder, base_type, base_ptr, &ptr_indices[0], 2, "slice_ptr_in_struct");
        const arg_ptr_type = c.LLVMPointerType(element_type, 0);
        const loaded_ptr = c.LLVMBuildLoad2(self.builder, arg_ptr_type, ptr_in_struct, "slice_ptr_val");

        const idx = collected_indices.items[collected_indices.items.len - 1];
        var indices = [_]c.LLVMValueRef{idx};
        const element_ptr = c.LLVMBuildGEP2(self.builder, element_type, loaded_ptr, &indices[0], 1, "slice_element_ptr");

        const expected_ty_name = self.getTypeNameFromLLVMType(element_type);
        const value_raw = try self.generateExpressionWithContext(arr_ass.value, expected_ty_name);
        const final_val = try self.castWithRules(value_raw, element_type, arr_ass.value);
        _ = c.LLVMBuildStore(self.builder, final_val, element_ptr);
        return;
    }

    index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
    try collected_indices.append(self.allocator, index_value);
    var final_type = base_type;
    for (0..collected_indices.items.len) |_| {
        final_type = c.LLVMGetElementType(@ptrCast(final_type));
    }

    var all_indices = std.ArrayList(c.LLVMValueRef){};
    defer all_indices.deinit(self.allocator);
    try all_indices.append(self.allocator, c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0));

    var i = collected_indices.items.len;
    while (i > 0) {
        i -= 1;
        try all_indices.append(self.allocator, collected_indices.items[i]);
    }

    const element_ptr = c.LLVMBuildGEP2(self.builder, base_type, base_ptr, all_indices.items.ptr, @intCast(all_indices.items.len), "array_element_ptr");

    const expected_ty_name = self.getTypeNameFromLLVMType(final_type);
    const value_raw = try self.generateExpressionWithContext(arr_ass.value, expected_ty_name);
    const final_val = try self.castWithRules(value_raw, final_type, arr_ass.value);
    _ = c.LLVMBuildStore(self.builder, final_val, element_ptr);
}

pub fn generateArrayReassignment(self: *CodeGenerator, array_name: []const u8, value_expr: *ast.Node) errors.CodegenError!void {
    const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;

    switch (value_expr.data) {
        .array_initializer => |init_list| {
            for (init_list.elements.items, 0..) |element, idx| {
                const element_type = c.LLVMGetElementType(@ptrCast(var_info.type_ref));
                const expected_ty_name = self.getTypeNameFromLLVMType(@ptrCast(element_type));
                const element_value_raw = try self.generateExpressionWithContext(element, expected_ty_name);

                var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(idx)), 0) };
                const element_ptr = c.LLVMBuildGEP2(@ptrCast(self.builder), @ptrCast(var_info.type_ref), @ptrCast(var_info.value), &indices[0], 2, "array_element_ptr");

                const casted_value = try self.castWithRules(element_value_raw, element_type, element);
                _ = c.LLVMBuildStore(self.builder, casted_value, element_ptr);
            }
        },
        .string_literal => |str_lit| {
            const element_type = c.LLVMGetElementType(var_info.type_ref);
            const element_type_kind = c.LLVMGetTypeKind(element_type);
            if (element_type_kind != c.LLVMIntegerTypeKind or c.LLVMGetIntTypeWidth(element_type) != 8) {
                self.reportTypeMismatchStr("arr<u8, N>", var_info.type_ref, "string literal assignment");
                return errors.CodegenError.TypeMismatch;
            }
            const parsed_str = try strings.parseEscape(self.allocator, str_lit.value);
            defer self.allocator.free(parsed_str);
            const str_len = parsed_str.len;
            const array_type = var_info.type_ref;
            const array_length = c.LLVMGetArrayLength(array_type);
            if (str_len > array_length) {
                self.reportErrorFmt("String literal too long: {d} characters exceeds array size of {d}", .{ str_len, array_length }, "Increase array size or shorten the string");
                return errors.CodegenError.TypeMismatch;
            }
            const memcpy_func = try CodeGenerator.declareLibcFunction(self, "memcpy");
            const i8_type = c.LLVMInt8TypeInContext(self.context);
            const i8_ptr_type = c.LLVMPointerType(i8_type, 0);
            const size_t_type = self.libcTypeToLLVM(.size_t_type);
            const array_i8_ptr = c.LLVMBuildBitCast(self.builder, var_info.value, i8_ptr_type, "array_i8_ptr");
            const global_str = c.LLVMBuildGlobalStringPtr(self.builder, parsed_str.ptr, "temp_str_ptr");
            const copy_size = c.LLVMConstInt(size_t_type, @as(c_ulonglong, @intCast(str_len)), 0);
            var memcpy_args = [_]c.LLVMValueRef{ array_i8_ptr, global_str, copy_size };
            _ = c.LLVMBuildCall2(self.builder, c.LLVMGlobalGetValueType(memcpy_func), memcpy_func, &memcpy_args[0], 3, "");
        },
        else => {
            self.reportTypeMismatchGeneric("array initialization", "Array must be initialized with an array literal or string literal");
            return errors.CodegenError.TypeMismatch;
        },
    }
}

pub fn generateArrayDeclaration(self: *CodeGenerator, decl: ast.VarDecl) errors.CodegenError!void {
    const inner = decl.type_name[4 .. decl.type_name.len - 1];
    var comma_pos: ?usize = null;
    var search_idx: usize = inner.len;
    while (search_idx > 0) {
        search_idx -= 1;
        if (inner[search_idx] == ',') {
            comma_pos = search_idx;
            break;
        }
    }

    if (comma_pos) |pos| {
        const element_type_part = inner[0..pos];
        const element_type_name = std.mem.trim(u8, element_type_part, " \t");
        const size_part = inner[pos + 1 ..];
        const size_str = std.mem.trim(u8, size_part, " \t");
        var array_size: usize = 0;
        if (std.mem.eql(u8, size_str, "_")) {
            if (decl.initializer) |initializer| {
                switch (initializer.data) {
                    .array_initializer => |init_list| {
                        array_size = init_list.elements.items.len;
                    },
                    .string_literal => |str_lit| {
                        const parsed_str = try strings.parseEscape(self.allocator, str_lit.value);
                        defer self.allocator.free(parsed_str);
                        array_size = parsed_str.len;
                    },
                    else => {
                        self.reportTypeMismatchGeneric("array size inference", "Array size can only be inferred from array or string literals");
                        return errors.CodegenError.TypeMismatch;
                    },
                }
            } else {
                self.reportTypeMismatchGeneric("array declaration", "Array size must be specified or inferable from initializer");
                return errors.CodegenError.TypeMismatch;
            }
        } else {
            array_size = std.fmt.parseInt(usize, size_str, 10) catch {
                self.reportErrorFmt("Invalid array size: '{s}'", .{size_str}, "Array size must be a valid integer");
                return errors.CodegenError.TypeMismatch;
            };
        }

        const element_type = try self.getLLVMType(element_type_name);
        const array_type = c.LLVMArrayType(@ptrCast(element_type), @intCast(array_size));
        const alloca = self.buildAllocaAtEntry(array_type, decl.name);

        try CodeGenerator.putVariable(self, decl.name, structs.VariableInfo{
            .value = @ptrCast(alloca),
            .type_ref = @ptrCast(array_type),
            .type_name = decl.type_name,
            .is_const = decl.is_const,
        });
        if (decl.initializer) |initializer| {
            switch (initializer.data) {
                .array_initializer => |init_list| {
                    for (init_list.elements.items, 0..) |element, idx| {
                        const expected_ty_name = self.getTypeNameFromLLVMType(element_type);
                        const element_value_raw = try self.generateExpressionWithContext(element, expected_ty_name);
                        var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(idx)), 0) };
                        const element_ptr = c.LLVMBuildGEP2(self.builder, array_type, alloca, &indices[0], 2, "array_element_ptr");
                        const casted_value = try self.castWithRules(element_value_raw, @ptrCast(element_type), element);
                        _ = c.LLVMBuildStore(self.builder, casted_value, element_ptr);
                    }
                },
                .string_literal => |str_lit| {
                    const element_type_kind = c.LLVMGetTypeKind(element_type);
                    if (element_type_kind != c.LLVMIntegerTypeKind or c.LLVMGetIntTypeWidth(element_type) != 8) {
                        return errors.CodegenError.TypeMismatch;
                    }
                    const parsed_str = try strings.parseEscape(self.allocator, str_lit.value);
                    defer self.allocator.free(parsed_str);
                    for (parsed_str, 0..) |byte, idx| {
                        if (idx >= array_size) break;
                        var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(idx)), 0) };
                        const element_ptr = c.LLVMBuildGEP2(self.builder, array_type, alloca, &indices[0], 2, "array_element_ptr");
                        const byte_value = c.LLVMConstInt(c.LLVMInt8TypeInContext(self.context), @as(c_ulonglong, @intCast(byte)), 0);
                        _ = c.LLVMBuildStore(self.builder, byte_value, element_ptr);
                    }
                },
                else => {
                    const init_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                    const casted_init_value = try self.castWithSourceRules(init_value, array_type, initializer);
                    _ = c.LLVMBuildStore(self.builder, casted_init_value, alloca);
                },
            }
        }
    } else {
        return errors.CodegenError.TypeMismatch;
    }
}

pub fn generateArrayIndexExpression(self: *CodeGenerator, arr_idx: ast.ArrayIndex) errors.CodegenError!c.LLVMValueRef {
    var collected_indices = std.ArrayList(c.LLVMValueRef){};
    defer collected_indices.deinit(self.allocator);

    const base_node = try collectArrayIndices(self, arr_idx.array, &collected_indices);
    var index_value = try self.generateExpression(arr_idx.index);

    var base_ptr: c.LLVMValueRef = undefined;
    var base_type: c.LLVMTypeRef = undefined;
    var base_type_name: []const u8 = "";
    var base_ptr_is_direct_value = false;

    if (base_node.data == .qualified_identifier) {
        const pair = try self.getQualifiedFieldPtrAndType(base_node);
        base_ptr = pair.ptr;
        base_type = pair.ty;
        base_type_name = self.getTypeNameFromLLVMType(base_type);
    } else {
        if (base_node.data == .function_call) {
            base_ptr = try self.generateExpression(base_node);
            base_type = c.LLVMTypeOf(base_ptr);
            const call = base_node.data.function_call;
            base_type_name = if (self.function_return_types.get(call.name)) |rt|
                rt
            else if (self.function_asts.get(call.name)) |fn_ast|
                fn_ast.return_type
            else
                try self.inferType(base_node);
            base_ptr_is_direct_value = true;
        } else {
            const array_name = try self.getBaseIdentifierName(base_node);
            defer self.allocator.free(array_name);
            const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;
            base_ptr = var_info.value;
            base_type = var_info.type_ref;
            base_type_name = var_info.type_name;
        }
    }

    if (std.mem.startsWith(u8, base_type_name, "simd<")) {
        index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
        try collected_indices.append(self.allocator, index_value);
        const simd_val = c.LLVMBuildLoad2(self.builder, base_type, base_ptr, "load_simd");
        const simd_index_value = collected_indices.items[collected_indices.items.len - 1];
        return c.LLVMBuildExtractElement(self.builder, simd_val, simd_index_value, "simd_extract");
    }

    const is_pointer_base = std.mem.startsWith(u8, base_type_name, "ptr<") or (base_ptr_is_direct_value and c.LLVMGetTypeKind(base_type) == c.LLVMPointerTypeKind);
    if (is_pointer_base) {
        if (c.LLVMTypeOf(index_value) != c.LLVMInt64TypeInContext(self.context)) {
            index_value = self.castToType(index_value, c.LLVMInt64TypeInContext(self.context));
        }
        try collected_indices.append(self.allocator, index_value);
        const ptr_val = if (base_ptr_is_direct_value)
            base_ptr
        else
            c.LLVMBuildLoad2(self.builder, base_type, base_ptr, "load_ptr");
        if (!(std.mem.startsWith(u8, base_type_name, "ptr<") and std.mem.endsWith(u8, base_type_name, ">"))) {
            return errors.CodegenError.TypeMismatch;
        }
        const element_type_name = base_type_name[4 .. base_type_name.len - 1];
        const element_type = try self.getLLVMType(element_type_name);
        const idx = collected_indices.items[collected_indices.items.len - 1];
        var indices = [_]c.LLVMValueRef{idx};
        const element_ptr = c.LLVMBuildGEP2(self.builder, element_type, ptr_val, &indices[0], 1, "ptr_index");
        return c.LLVMBuildLoad2(self.builder, element_type, element_ptr, "ptr_element");
    }

    if (std.mem.startsWith(u8, base_type_name, "[]")) {
        index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
        try collected_indices.append(self.allocator, index_value);

        const element_type_name = base_type_name[2..];
        const element_type = try self.getLLVMType(element_type_name);

        var ptr_indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0) };
        const ptr_in_struct = c.LLVMBuildGEP2(self.builder, base_type, base_ptr, &ptr_indices[0], 2, "slice_ptr_in_struct");
        const arg_ptr_type = c.LLVMPointerType(element_type, 0);
        const loaded_ptr = c.LLVMBuildLoad2(self.builder, arg_ptr_type, ptr_in_struct, "slice_ptr_val");

        const idx = collected_indices.items[collected_indices.items.len - 1];
        var indices = [_]c.LLVMValueRef{idx};
        const element_ptr = c.LLVMBuildGEP2(self.builder, element_type, loaded_ptr, &indices[0], 1, "slice_element_ptr");
        return c.LLVMBuildLoad2(self.builder, element_type, element_ptr, "slice_element");
    }

    index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
    try collected_indices.append(self.allocator, index_value);

    var final_type = base_type;
    for (0..collected_indices.items.len) |_| {
        final_type = c.LLVMGetElementType(final_type);
    }

    var all_indices = std.ArrayList(c.LLVMValueRef){};
    defer all_indices.deinit(self.allocator);
    try all_indices.append(self.allocator, c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0));

    var i = collected_indices.items.len;
    while (i > 0) {
        i -= 1;
        try all_indices.append(self.allocator, collected_indices.items[i]);
    }

    const element_ptr = c.LLVMBuildGEP2(self.builder, base_type, base_ptr, all_indices.items.ptr, @intCast(all_indices.items.len), "array_element_ptr");

    return c.LLVMBuildLoad2(self.builder, final_type, element_ptr, "array_element");
}
