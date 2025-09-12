const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
const llvm = @import("llvm.zig");
const variables = @import("variables.zig");

const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/IRReader.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/ExecutionEngine.h");
});

pub const VariableInfo = struct {
    value: c.LLVMValueRef,
    type_ref: c.LLVMTypeRef,
    type_name: []const u8,
};

pub const LoopContext = struct {
    break_block: c.LLVMBasicBlockRef,
    continue_block: c.LLVMBasicBlockRef,
};

pub fn getStructTypeName(cg: *llvm.CodeGenerator, struct_type: c.LLVMTypeRef) ![]const u8 {
    const name_ptr = c.LLVMGetStructName(struct_type);
    if (name_ptr != null) {
        return try cg.allocator.dupe(u8, std.mem.span(name_ptr));
    }
    var it = cg.struct_types.iterator();
    while (it.next()) |entry| {
        if (entry.value_ptr.* == struct_type) {
            return try cg.allocator.dupe(u8, entry.key_ptr.*);
        }
    }
    return try cg.allocator.dupe(u8, "anonymous_struct");
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
            const index_expr = if (variables.getVariable(cg, index_str)) |var_info| blk: {
                break :blk c.LLVMBuildLoad2(cg.builder, var_info.type_ref, var_info.value, "index_var");
            } else blk: {
                break :blk try cg.generateExpressionFromString(index_str);
            };
            const var_type_kind = c.LLVMGetTypeKind(current_type);
            if (var_type_kind != c.LLVMArrayTypeKind) return errors.CodegenError.TypeMismatch;
            const element_type = c.LLVMGetElementType(current_type);
            var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0), index_expr };
            const element_ptr = c.LLVMBuildGEP2(cg.builder, current_type, current_value, &indices[0], 2, "array_element_ptr");
            current_value = element_ptr;
            current_type = element_type;
            const elem_type_kind = c.LLVMGetTypeKind(element_type);
            if (elem_type_kind == c.LLVMStructTypeKind) {
                const name_ptr = c.LLVMGetStructName(element_type);
                if (name_ptr != null) {
                    current_type_name = std.mem.span(name_ptr);
                }
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
            if (var_type_kind != c.LLVMStructTypeKind) return errors.CodegenError.TypeMismatch;
            const struct_type = cg.struct_types.get(current_type_name) orelse return errors.CodegenError.TypeMismatch;
            const field_map = cg.struct_fields.get(current_type_name) orelse return errors.CodegenError.TypeMismatch;
            const field_index = field_map.get(field_name) orelse return errors.CodegenError.UndefinedVariable;
            const field_count = c.LLVMCountStructElementTypes(struct_type);
            if (field_index >= field_count) {
                return errors.CodegenError.TypeMismatch;
            }
            const field_ptr = try getStructFieldPointer(cg, struct_type, current_value, field_index);
            const field_type = c.LLVMStructGetTypeAtIndex(struct_type, field_index);
            if (dot_pos != null) {
                const field_type_kind = c.LLVMGetTypeKind(field_type);
                if (field_type_kind != c.LLVMStructTypeKind) return errors.CodegenError.TypeMismatch;
                current_value = field_ptr;
                current_type = field_type;
                const name_ptr = c.LLVMGetStructName(field_type);
                if (name_ptr == null) return errors.CodegenError.TypeMismatch;
                current_type_name = std.mem.span(name_ptr);
                path = path[dot_pos.? + 1 ..];
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
    return try generateRecursiveFieldAssignment(cg, struct_var_info.value, struct_var_info.type_ref, struct_var_info.type_name, field_path, value_expr);
}

pub fn getStructFieldPointer(cg: *llvm.CodeGenerator, struct_type: c.LLVMTypeRef, struct_value: c.LLVMValueRef, field_index: c_uint) errors.CodegenError!c.LLVMValueRef {
    var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), field_index, 0) };
    return c.LLVMBuildGEP2(cg.builder, struct_type, struct_value, &indices[0], 2, "struct_field_ptr");
}

pub fn assignToField(cg: *llvm.CodeGenerator, field_ptr: c.LLVMValueRef, field_type: c.LLVMTypeRef, value_expr: *ast.Node) errors.CodegenError!void {
    const field_type_kind = c.LLVMGetTypeKind(field_type);
    if (value_expr.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
        try assignStringLiteralToArrayField(cg, field_ptr, field_type, value_expr.data.string_literal);
        return;
    }
    const value = try cg.generateExpression(value_expr);

    const casted_value = cg.castToType(value, field_type);

    const store_instr = c.LLVMBuildStore(cg.builder, casted_value, field_ptr);
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
    const i8_type = c.LLVMInt8TypeInContext(cg.context);
    const i8_ptr_type = c.LLVMPointerType(i8_type, 0);
    const size_t_type = utils.libcTypeToLLVM(cg, .size_t_type);
    const field_i8_ptr = c.LLVMBuildBitCast(cg.builder, field_ptr, i8_ptr_type, "field_i8_ptr");
    const global_str = c.LLVMBuildGlobalStringPtr(cg.builder, parsed_str.ptr, "temp_str_ptr");
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
            if (var_type_kind != c.LLVMArrayTypeKind) return errors.CodegenError.TypeMismatch;
            const element_type = c.LLVMGetElementType(current_type);
            var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0), index_expr };
            const element_ptr = c.LLVMBuildGEP2(cg.builder, current_type, current_value, &indices[0], 2, "array_element_ptr");
            current_value = element_ptr;
            current_type = element_type;
            const elem_type_kind = c.LLVMGetTypeKind(element_type);
            if (elem_type_kind == c.LLVMStructTypeKind) {
                const name_ptr = c.LLVMGetStructName(element_type);
                if (name_ptr != null) {
                    current_type_name = std.mem.span(name_ptr);
                }
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
            if (var_type_kind != c.LLVMStructTypeKind) return errors.CodegenError.TypeMismatch;
            const struct_type = cg.struct_types.get(current_type_name) orelse {
                return errors.CodegenError.TypeMismatch;
            };
            const field_map = cg.struct_fields.get(current_type_name) orelse return errors.CodegenError.TypeMismatch;
            const field_index = field_map.get(field_name) orelse return errors.CodegenError.UndefinedVariable;
            const field_count = c.LLVMCountStructElementTypes(struct_type);
            if (field_index >= field_count) {
                return errors.CodegenError.TypeMismatch;
            }
            const field_ptr = try getStructFieldPointer(cg, struct_type, current_value, field_index);
            const field_type = c.LLVMStructGetTypeAtIndex(struct_type, field_index);
            if (dot_pos != null) {
                const field_type_kind = c.LLVMGetTypeKind(field_type);
                if (field_type_kind != c.LLVMStructTypeKind) return errors.CodegenError.TypeMismatch;
                current_value = field_ptr;
                current_type = field_type;
                const name_ptr = c.LLVMGetStructName(field_type);
                if (name_ptr == null) return errors.CodegenError.TypeMismatch;
                current_type_name = std.mem.span(name_ptr);
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
    var field_types = std.ArrayList(c.LLVMTypeRef).init(cg.allocator);
    defer field_types.deinit();
    var field_map = std.HashMap([]const u8, c_uint, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(cg.allocator);
    for (struct_decl.fields.items, 0..) |field, i| {
        const field_type = utils.getLLVMType(cg, field.type_name);
        try field_types.append(field_type);
        try field_map.put(field.name, @intCast(i));
    }
    const struct_name_z = try cg.allocator.dupeZ(u8, struct_decl.name);
    defer cg.allocator.free(struct_name_z);
    const struct_type = c.LLVMStructCreateNamed(cg.context, struct_name_z.ptr);
    c.LLVMStructSetBody(struct_type, field_types.items.ptr, @intCast(field_types.items.len), 0);
    try cg.struct_types.put(struct_decl.name, struct_type);
    try cg.struct_fields.put(struct_decl.name, field_map);
    try cg.struct_declarations.put(struct_decl.name, struct_decl);
}

pub fn generateStructInitializer(cg: *llvm.CodeGenerator, struct_init: ast.StructInitializer) errors.CodegenError!c.LLVMValueRef {
    const struct_type = cg.struct_types.get(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
    const field_map = cg.struct_fields.get(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
    const struct_ptr = c.LLVMBuildAlloca(cg.builder, struct_type, "struct_init");
    const struct_decl = getStructDecl(cg, struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
    for (struct_decl.fields.items, 0..) |field, i| {
        if (field.default_value) |default_val| {
            const field_ptr = try getStructFieldPointer(cg, struct_type, struct_ptr, @intCast(i));
            const field_type = utils.getLLVMType(cg, field.type_name);
            const field_type_kind = c.LLVMGetTypeKind(field_type);
            if (default_val.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
                try assignStringLiteralToArrayField(cg, field_ptr, field_type, default_val.data.string_literal);
            } else {
                const value = try cg.generateExpression(default_val);
                const casted_value = cg.castToType(value, field_type);
                _ = c.LLVMBuildStore(cg.builder, casted_value, field_ptr);
            }
        }
    }

    for (struct_init.field_values.items) |field_val| {
        const field_index = field_map.get(field_val.field_name) orelse return errors.CodegenError.UndefinedVariable;
        const field_ptr = try getStructFieldPointer(cg, struct_type, struct_ptr, field_index);
        const field_type = c.LLVMStructGetTypeAtIndex(struct_type, field_index);
        const field_type_kind = c.LLVMGetTypeKind(field_type);
        if (field_val.value.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
            try assignStringLiteralToArrayField(cg, field_ptr, field_type, field_val.value.data.string_literal);
        } else {
            const value = try cg.generateExpression(field_val.value);
            const casted_value = cg.castToType(value, field_type);
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
