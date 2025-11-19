const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
const llvm = @import("llvm.zig");
const variables = @import("variables.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

pub const VariableInfo = struct {
    value: c.LLVMValueRef,
    type_ref: c.LLVMTypeRef,
    type_name: []const u8,
    is_byval_param: bool = false,
    is_const: bool = false,
};

pub const LoopContext = struct {
    break_block: c.LLVMBasicBlockRef,
    continue_block: c.LLVMBasicBlockRef,
};

pub const PendingGoto = struct {
    label: []const u8,
    goto_block: c.LLVMBasicBlockRef,
};

const ElementResult = struct {
    element_ptr: c.LLVMValueRef,
    element_type: c.LLVMTypeRef,
};

pub fn getStructTypeName(cg: *llvm.CodeGenerator, struct_type: c.LLVMTypeRef) ![]const u8 {
    const name_ptr = c.LLVMGetStructName(struct_type);
    if (name_ptr != null) {
        return utils.dupe(u8, cg.allocator, std.mem.sliceTo(name_ptr, 0));
    }
    var it = cg.struct_types.iterator();
    while (it.next()) |entry| {
        if (@intFromPtr(entry.value_ptr.*) == @intFromPtr(struct_type)) {
            return utils.dupe(u8, cg.allocator, entry.key_ptr.*);
        }
    }
    return utils.dupe(u8, cg.allocator, "anonymous_struct");
}

pub fn generateRecursiveFieldAssignment(cg: *llvm.CodeGenerator, base_value: c.LLVMValueRef, base_type: c.LLVMTypeRef, base_type_name: []const u8, field_path: []const u8, value_expr: *ast.Node) errors.CodegenError!void {
    var current_value = base_value;
    var current_type = base_type;
    var current_type_name = base_type_name;
    var path = field_path;

    while (path.len > 0) {
        if (std.mem.startsWith(u8, path, "[")) {
            const close_bracket_pos = std.mem.indexOfScalar(u8, path, ']') orelse return errors.CodegenError.TypeMismatch;
            const index_str = path[1..close_bracket_pos];
            const index_expr = if (variables.getVariable(cg, index_str)) |var_info|
                c.LLVMBuildLoad2(@ptrCast(cg.builder), @ptrCast(var_info.type_ref), @ptrCast(var_info.value), "index_var")
            else
                try cg.generateExpressionFromString(index_str);
            const var_type_kind = c.LLVMGetTypeKind(current_type);
            const result = blk: {
                if (var_type_kind == c.LLVMArrayTypeKind) {
                    const element_type = c.LLVMGetElementType(current_type);
                    var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0), index_expr };
                    const element_ptr = c.LLVMBuildGEP2(cg.builder, current_type, current_value, &indices[0], 2, "array_element_ptr");
                    break :blk ElementResult{ .element_ptr = element_ptr, .element_type = element_type };
                } else if (var_type_kind == c.LLVMPointerTypeKind) {
                    // For pointer types, we need to dereference and then index
                    const element_type = c.LLVMGetElementType(current_type);
                    const loaded_ptr = c.LLVMBuildLoad2(cg.builder, current_type, current_value, "loaded_ptr");
                    var indices = [_]c.LLVMValueRef{index_expr};
                    const element_ptr = c.LLVMBuildGEP2(cg.builder, element_type, loaded_ptr, &indices[0], 1, "ptr_element_ptr");
                    break :blk ElementResult{ .element_ptr = element_ptr, .element_type = element_type };
                } else {
                    return errors.CodegenError.TypeMismatch;
                }
            };
            const element_ptr = result.element_ptr;
            const element_type = result.element_type;
            current_value = element_ptr;
            current_type = element_type;
            const elem_type_kind = c.LLVMGetTypeKind(element_type);
            if (elem_type_kind == c.LLVMStructTypeKind) {
                const name_ptr = c.LLVMGetStructName(element_type);
                if (name_ptr != null) {
                    current_type_name = std.mem.sliceTo(name_ptr, 0);
                }
            } else if (var_type_kind == c.LLVMPointerTypeKind and std.mem.startsWith(u8, current_type_name, "ptr<")) {
                // For pointer dereferencing, extract the inner type name
                current_type_name = current_type_name[4 .. current_type_name.len - 1];
            }
            path = path[close_bracket_pos + 1 ..];
            if (std.mem.startsWith(u8, path, ".")) {
                path = path[1..];
            }
        } else {
            const dot_pos = std.mem.indexOfScalar(u8, path, '.');
            const bracket_pos = std.mem.indexOfScalar(u8, path, '[');
            const field_name_len = if (dot_pos != null and bracket_pos != null)
                @min(dot_pos.?, bracket_pos.?)
            else if (dot_pos != null)
                dot_pos.?
            else if (bracket_pos != null)
                bracket_pos.?
            else
                path.len;
            const field_name = path[0..field_name_len];
            const var_type_kind = c.LLVMGetTypeKind(current_type);
            if (var_type_kind != c.LLVMStructTypeKind) {
                if (cg.current_line > 0) {
                    std.debug.print("Error at line {d}: Cannot access field on non-struct type\n", .{cg.current_line});
                } else {
                    std.debug.print("Error: Cannot access field on non-struct type\n", .{});
                }
                return errors.CodegenError.TypeMismatch;
            }
            const struct_type = cg.struct_types.get(current_type_name) orelse {
                if (cg.current_line > 0) {
                    std.debug.print("Error at line {d}: Unknown struct type '{s}'\n", .{ cg.current_line, current_type_name });
                } else {
                    std.debug.print("Error: Unknown struct type '{s}'\n", .{current_type_name});
                }
                return errors.CodegenError.TypeMismatch;
            };
            const field_map = cg.struct_fields.get(current_type_name) orelse return errors.CodegenError.TypeMismatch;
            const field_index = field_map.get(field_name) orelse return errors.CodegenError.UndefinedVariable;

            const field_ptr = try getStructFieldPointer(cg, struct_type, current_value, field_index);

            // Get the actual field type (works for both structs and unions)
            const field_type = try getFieldType(cg, struct_type, field_index);

            if (field_name_len < path.len) {
                // There's more path to process after this field
                const field_type_kind = c.LLVMGetTypeKind(field_type);
                if (field_type_kind == c.LLVMStructTypeKind) {
                    current_value = field_ptr;
                    current_type = field_type;
                    const name_ptr = c.LLVMGetStructName(field_type);
                    if (name_ptr == null) return errors.CodegenError.TypeMismatch;
                    current_type_name = std.mem.sliceTo(name_ptr, 0);
                } else if (field_type_kind == c.LLVMPointerTypeKind) {
                    // For pointer fields followed by array access, load the pointer value
                    const loaded_ptr = c.LLVMBuildLoad2(cg.builder, field_type, field_ptr, "loaded_field_ptr");
                    current_value = loaded_ptr;
                    current_type = field_type;
                    // Get field type name from struct declaration
                    const field_decl = cg.struct_declarations.get(current_type_name) orelse return errors.CodegenError.TypeMismatch;
                    const field_info = field_decl.fields.items[field_index];
                    current_type_name = field_info.type_name;
                } else {
                    return errors.CodegenError.TypeMismatch;
                }
                path = if (dot_pos) |pos| path[pos + 1 ..] else path[field_name_len..];
            } else {
                try assignToField(cg, field_ptr, field_type, value_expr);
                return;
            }
        }
    }
    return errors.CodegenError.TypeMismatch;
}

pub fn generateStructFieldAssignment(cg: *llvm.CodeGenerator, struct_name: []const u8, field_path: []const u8, value_expr: *ast.Node) errors.CodegenError!void {
    const struct_var_info = variables.getVariable(cg, struct_name) orelse return errors.CodegenError.UndefinedVariable;

    if (struct_var_info.is_const) {
        if (cg.current_line > 0) {
            std.debug.print("Error at line {d}: Cannot modify field of const variable '{s}'\n", .{ cg.current_line, struct_name });
        } else {
            std.debug.print("Error: Cannot modify field of const variable '{s}'\n", .{struct_name});
        }
        return errors.CodegenError.ConstReassignment;
    }

    return try generateRecursiveFieldAssignment(cg, struct_var_info.value, struct_var_info.type_ref, struct_var_info.type_name, field_path, value_expr);
}

pub fn getStructFieldPointer(cg: *llvm.CodeGenerator, struct_type: c.LLVMTypeRef, struct_value: c.LLVMValueRef, field_index: c_uint) errors.CodegenError!c.LLVMValueRef {
    // Check if this is a union by examining the struct name
    const struct_name_ptr = c.LLVMGetStructName(struct_type);
    var is_union = false;
    if (struct_name_ptr) |name_ptr| {
        const name = std.mem.sliceTo(name_ptr, 0);
        if (cg.struct_declarations.get(name)) |decl| {
            is_union = decl.is_union;
        }
    }

    if (is_union) {
        // For unions, all fields are at offset 0, just get pointer to the union body at index 0
        var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(@ptrCast(cg.context)), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(@ptrCast(cg.context)), 0, 0) };
        const union_body_ptr = c.LLVMBuildGEP2(@ptrCast(cg.builder), struct_type, struct_value, &indices[0], 2, "union_body_ptr");
        return union_body_ptr;
    } else {
        // For structs, use the actual field index
        var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(@ptrCast(cg.context)), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(@ptrCast(cg.context)), field_index, 0) };
        return c.LLVMBuildGEP2(@ptrCast(cg.builder), struct_type, struct_value, &indices[0], 2, "struct_field_ptr");
    }
}

pub fn getFieldType(cg: *llvm.CodeGenerator, struct_type: c.LLVMTypeRef, field_index: c_uint) !c.LLVMTypeRef {
    const struct_name_ptr = c.LLVMGetStructName(struct_type);
    if (struct_name_ptr) |name_ptr| {
        const name = std.mem.sliceTo(name_ptr, 0);
        if (cg.struct_declarations.get(name)) |decl| {
            if (decl.is_union) {
                // For unions, get the actual field type from the declaration
                if (field_index >= decl.fields.items.len) return errors.CodegenError.TypeMismatch;
                return try utils.getLLVMType(cg, decl.fields.items[field_index].type_name);
            }
        }
    }
    // For regular structs, use LLVM's type info
    return c.LLVMStructGetTypeAtIndex(struct_type, field_index);
}

pub fn assignToField(cg: *llvm.CodeGenerator, field_ptr: c.LLVMValueRef, field_type: c.LLVMTypeRef, value_expr: *ast.Node) errors.CodegenError!void {
    const field_type_kind = c.LLVMGetTypeKind(field_type);
    if (value_expr.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
        try assignStringLiteralToArrayField(cg, field_ptr, field_type, value_expr.data.string_literal);
        return;
    }
    var value_raw = try cg.generateExpression(value_expr);
    if (value_expr.data == .struct_initializer) {
        const value_type = c.LLVMTypeOf(value_raw);
        const value_type_kind = c.LLVMGetTypeKind(value_type);
        if (value_type_kind == c.LLVMPointerTypeKind and field_type_kind == c.LLVMStructTypeKind) {
            value_raw = c.LLVMBuildLoad2(cg.builder, field_type, value_raw, "load_struct_init");
        }
    }

    const final_val = try cg.castWithRules(value_raw, field_type, value_expr);
    const store_instr = c.LLVMBuildStore(cg.builder, final_val, field_ptr);
    const alignment = utils.getAlignmentForType(cg, field_type);
    c.LLVMSetAlignment(store_instr, alignment);
}

pub fn assignStringLiteralToArrayField(cg: *llvm.CodeGenerator, field_ptr: c.LLVMValueRef, field_type: c.LLVMTypeRef, str_lit: ast.StringLiteral) errors.CodegenError!void {
    const element_type = c.LLVMGetElementType(field_type);
    const element_type_kind = c.LLVMGetTypeKind(element_type);
    if (element_type_kind != c.LLVMIntegerTypeKind or c.LLVMGetIntTypeWidth(element_type) != 8) {
        return;
    }
    const parsed_str = try cg.parse_escape(str_lit.value);
    defer cg.allocator.free(parsed_str);
    const str_len = parsed_str.len;
    const array_length = c.LLVMGetArrayLength(field_type);
    if (str_len > array_length) {
        return errors.CodegenError.TypeMismatch;
    }
    const memcpy_func = try cg.declareLibcFunction("memcpy");
    const i8_type = c.LLVMInt8TypeInContext(@ptrCast(cg.context));
    const i8_ptr_type = c.LLVMPointerType(i8_type, 0);
    const size_t_type = utils.libcTypeToLLVM(cg, .size_t_type);
    const field_i8_ptr = c.LLVMBuildBitCast(@ptrCast(cg.builder), field_ptr, i8_ptr_type, "field_i8_ptr");
    const global_str = c.LLVMBuildGlobalStringPtr(@ptrCast(cg.builder), parsed_str.ptr, "temp_str_ptr");
    const copy_size = c.LLVMConstInt(size_t_type, @as(c_ulonglong, @intCast(str_len)), 0);
    var memcpy_args = [_]c.LLVMValueRef{ field_i8_ptr, global_str, copy_size };
    _ = c.LLVMBuildCall2(cg.builder, c.LLVMGlobalGetValueType(memcpy_func), memcpy_func, &memcpy_args[0], 3, "");
}

pub fn generateRecursiveFieldAccess(cg: *llvm.CodeGenerator, base_value: c.LLVMValueRef, base_type: c.LLVMTypeRef, base_type_name: []const u8, field_path: []const u8) errors.CodegenError!c.LLVMValueRef {
    var current_value = base_value;
    var current_type = base_type;
    var current_type_name = base_type_name;
    var path = field_path;
    while (path.len > 0) {
        if (std.mem.startsWith(u8, path, "[")) {
            const close_bracket_pos = std.mem.indexOfScalar(u8, path, ']') orelse return errors.CodegenError.TypeMismatch;
            const index_str = path[1..close_bracket_pos];
            const index_expr = if (variables.getVariable(cg, index_str)) |var_info| blk: {
                break :blk c.LLVMBuildLoad2(cg.builder, var_info.type_ref, var_info.value, "index_var");
            } else blk: {
                break :blk try cg.generateExpressionFromString(index_str);
            };
            const var_type_kind = c.LLVMGetTypeKind(current_type);
            const result = blk: {
                if (var_type_kind == c.LLVMArrayTypeKind) {
                    const element_type = c.LLVMGetElementType(current_type);
                    var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0), index_expr };
                    const element_ptr = c.LLVMBuildGEP2(cg.builder, current_type, current_value, &indices[0], 2, "array_element_ptr");
                    break :blk ElementResult{ .element_ptr = element_ptr, .element_type = element_type };
                } else if (var_type_kind == c.LLVMPointerTypeKind) {
                    // For pointer types, we need to dereference and then index
                    const element_type = c.LLVMGetElementType(current_type);
                    const loaded_ptr = c.LLVMBuildLoad2(cg.builder, current_type, current_value, "loaded_ptr");
                    var indices = [_]c.LLVMValueRef{index_expr};
                    const element_ptr = c.LLVMBuildGEP2(cg.builder, element_type, loaded_ptr, &indices[0], 1, "ptr_element_ptr");
                    break :blk ElementResult{ .element_ptr = element_ptr, .element_type = element_type };
                } else {
                    return errors.CodegenError.TypeMismatch;
                }
            };
            const element_ptr = result.element_ptr;
            const element_type = result.element_type;
            current_value = element_ptr;
            current_type = element_type;
            const elem_type_kind = c.LLVMGetTypeKind(element_type);
            if (elem_type_kind == c.LLVMStructTypeKind) {
                const name_ptr = c.LLVMGetStructName(element_type);
                if (name_ptr != null) {
                    current_type_name = std.mem.sliceTo(name_ptr, 0);
                }
            } else if (var_type_kind == c.LLVMPointerTypeKind and std.mem.startsWith(u8, current_type_name, "ptr<")) {
                // For pointer dereferencing, extract the inner type name
                current_type_name = current_type_name[4 .. current_type_name.len - 1];
            }
            path = path[close_bracket_pos + 1 ..];
            if (std.mem.startsWith(u8, path, ".")) {
                path = path[1..];
            }
        } else {
            const dot_pos = std.mem.indexOfScalar(u8, path, '.');
            const field_name_len = if (dot_pos) |pos| pos else path.len;
            const field_name = path[0..field_name_len];
            const var_type_kind = c.LLVMGetTypeKind(current_type);
            if (var_type_kind != c.LLVMStructTypeKind) {
                if (cg.current_line > 0) {
                    std.debug.print("Error at line {d}: Cannot access field on non-struct type\n", .{cg.current_line});
                } else {
                    std.debug.print("Error: Cannot access field on non-struct type\n", .{});
                }
                return errors.CodegenError.TypeMismatch;
            }
            const struct_type = cg.struct_types.get(current_type_name) orelse {
                if (cg.current_line > 0) {
                    std.debug.print("Error at line {d}: Unknown struct type '{s}'\n", .{ cg.current_line, current_type_name });
                } else {
                    std.debug.print("Error: Unknown struct type '{s}'\n", .{current_type_name});
                }
                return errors.CodegenError.TypeMismatch;
            };
            const field_map = cg.struct_fields.get(current_type_name) orelse return errors.CodegenError.TypeMismatch;
            const field_index = field_map.get(field_name) orelse return errors.CodegenError.UndefinedVariable;

            const field_ptr = try getStructFieldPointer(cg, struct_type, current_value, field_index);

            // Get the actual field type (works for both structs and unions)
            const field_type = try getFieldType(cg, struct_type, field_index);

            if (dot_pos != null) {
                const field_type_kind = c.LLVMGetTypeKind(field_type);
                if (field_type_kind != c.LLVMStructTypeKind) return errors.CodegenError.TypeMismatch;
                current_value = field_ptr;
                current_type = field_type;
                const name_ptr = c.LLVMGetStructName(field_type);
                if (name_ptr == null) return errors.CodegenError.TypeMismatch;
                current_type_name = std.mem.sliceTo(name_ptr, 0);
                path = path[dot_pos.? + 1 ..];
            } else {
                const field_type_kind = c.LLVMGetTypeKind(field_type);
                if (field_type_kind == c.LLVMArrayTypeKind) {
                    return field_ptr;
                }
                const load_instr = c.LLVMBuildLoad2(cg.builder, field_type, field_ptr, "struct_field");
                const alignment = utils.getAlignmentForType(cg, field_type);
                c.LLVMSetAlignment(load_instr, alignment);
                return load_instr;
            }
        }
    }
    return errors.CodegenError.TypeMismatch;
}

pub fn generateStructFieldAccess(cg: *llvm.CodeGenerator, struct_var_info: VariableInfo, field_path: []const u8) errors.CodegenError!c.LLVMValueRef {
    return try generateRecursiveFieldAccess(cg, struct_var_info.value, struct_var_info.type_ref, struct_var_info.type_name, field_path);
}

pub fn generateStructType(cg: *llvm.CodeGenerator, struct_decl: ast.StructDecl) errors.CodegenError!void {
    // Create the named struct type first (opaque)
    const struct_name_z = utils.dupeZ(cg.allocator, struct_decl.name);
    defer cg.allocator.free(struct_name_z);
    const struct_type = c.LLVMStructCreateNamed(@ptrCast(cg.context), struct_name_z.ptr);

    // Register the struct type BEFORE resolving field types (allows self-referential structs)
    try cg.struct_types.put(struct_decl.name, @ptrCast(struct_type));
    try cg.struct_declarations.put(struct_decl.name, struct_decl);

    // Now resolve field types (can reference the struct itself)
    var field_types = std.ArrayList(c.LLVMTypeRef){};
    defer field_types.deinit(cg.allocator);
    var field_map = std.HashMap([]const u8, c_uint, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(cg.allocator);

    if (struct_decl.is_union) {
        // For unions, find the largest field type and use that as the body
        var max_size: c_ulonglong = 0;
        var max_align: c_uint = 1;
        var largest_type: ?c.LLVMTypeRef = null;

        for (struct_decl.fields.items, 0..) |field, i| {
            const field_type = try utils.getLLVMType(cg, field.type_name);
            const size = c.LLVMStoreSizeOfType(c.LLVMGetModuleDataLayout(cg.module), field_type);
            const alignment = utils.getAlignmentForType(cg, field_type);

            if (size > max_size or (size == max_size and alignment > max_align)) {
                max_size = size;
                max_align = alignment;
                largest_type = field_type;
            }

            try field_map.put(field.name, @intCast(i));
        }

        // Union body is a single element of the largest type
        if (largest_type) |lt| {
            var union_body = [_]c.LLVMTypeRef{lt};
            c.LLVMStructSetBody(struct_type, &union_body[0], 1, 0);
        } else {
            // Empty union - use i8
            var union_body = [_]c.LLVMTypeRef{c.LLVMInt8TypeInContext(cg.context)};
            c.LLVMStructSetBody(struct_type, &union_body[0], 1, 0);
        }
    } else {
        // Regular struct - all fields laid out sequentially
        for (struct_decl.fields.items, 0..) |field, i| {
            const field_type = try utils.getLLVMType(cg, field.type_name);
            try field_types.append(cg.allocator, @ptrCast(field_type));
            try field_map.put(field.name, @intCast(i));
        }

        // Set the struct body with resolved field types
        c.LLVMStructSetBody(struct_type, field_types.items.ptr, @intCast(field_types.items.len), 0);
    }

    try cg.struct_fields.put(struct_decl.name, field_map);
}

pub fn generateStructInitializer(cg: *llvm.CodeGenerator, struct_init: ast.StructInitializer) errors.CodegenError!c.LLVMValueRef {
    const struct_type = cg.struct_types.get(struct_init.struct_name) orelse {
        if (cg.current_line > 0) {
            std.debug.print("Error at line {d}: Unknown struct type '{s}'\n", .{ cg.current_line, struct_init.struct_name });
        } else {
            std.debug.print("Error: Unknown struct type '{s}'\n", .{struct_init.struct_name});
        }
        return errors.CodegenError.TypeMismatch;
    };
    const field_map = cg.struct_fields.get(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
    const struct_ptr = c.LLVMBuildAlloca(cg.builder, struct_type, "struct_init");
    const struct_decl = getStructDecl(cg, struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
    for (struct_decl.fields.items, 0..) |field, i| {
        if (field.default_value) |default_val| {
            const field_ptr = try getStructFieldPointer(cg, struct_type, struct_ptr, @intCast(i));
            const field_type = try utils.getLLVMType(cg, field.type_name);
            const field_type_kind = c.LLVMGetTypeKind(field_type);
            if (default_val.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
                try assignStringLiteralToArrayField(cg, field_ptr, field_type, default_val.data.string_literal);
            } else {
                const value = try cg.generateExpression(default_val);
                const casted_value = try cg.castWithRules(value, field_type, default_val);
                _ = c.LLVMBuildStore(cg.builder, casted_value, field_ptr);
            }
        }
    }

    for (struct_init.field_values.items, 0..) |field_val, idx| {
        const field_index = if (field_val.field_name) |field_name|
            field_map.get(field_name) orelse return errors.CodegenError.UndefinedVariable
        else
            @as(c_uint, @intCast(idx));
        const field_ptr = try getStructFieldPointer(cg, struct_type, struct_ptr, field_index);

        // Get the actual field type (works for both structs and unions)
        const field_type = try getFieldType(cg, struct_type, field_index);

        const field_type_kind = c.LLVMGetTypeKind(field_type);
        if (field_val.value.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
            try assignStringLiteralToArrayField(cg, field_ptr, field_type, field_val.value.data.string_literal);
        } else {
            const value = try cg.generateExpression(field_val.value);
            const casted_value = try cg.castWithRules(value, field_type, field_val.value);
            _ = c.LLVMBuildStore(cg.builder, casted_value, field_ptr);
        }
    }
    return struct_ptr;
}

pub fn getStructDecl(cg: *llvm.CodeGenerator, struct_name: []const u8) ?ast.StructDecl {
    return cg.struct_declarations.get(struct_name);
}

pub fn getLLVMStructType(cg: *llvm.CodeGenerator, struct_name: []const u8) !c.LLVMTypeRef {
    if (cg.struct_types.get(struct_name)) |existing| {
        return existing;
    }

    // Look for the struct declaration in the AST
    // This is a simplified approach - in a real implementation we'd need to
    // build a symbol table for struct declarations during code generation
    return errors.CodegenError.TypeMismatch;
}
