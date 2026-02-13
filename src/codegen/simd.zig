const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
const structs = @import("structs.zig");
const variables = @import("variables.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

const CodeGenerator = @import("llvm.zig").CodeGenerator;

pub fn generateSimdDeclaration(self: *CodeGenerator, decl: ast.VarDecl) errors.CodegenError!void {
    const inner = decl.type_name[5 .. decl.type_name.len - 1];
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
        const vector_size = std.fmt.parseInt(u32, size_str, 10) catch {
            return errors.CodegenError.TypeMismatch;
        };
        const element_type = try self.getLLVMType(element_type_name);
        const vector_type = c.LLVMVectorType(@ptrCast(element_type), vector_size);
        const alloca = self.buildAllocaAtEntry(vector_type, decl.name);
        try CodeGenerator.putVariable(self, decl.name, structs.VariableInfo{
            .value = @ptrCast(alloca),
            .type_ref = @ptrCast(vector_type),
            .type_name = decl.type_name,
            .is_const = decl.is_const,
        });
        if (decl.initializer) |initializer| {
            switch (initializer.data) {
                .array_initializer => |init_list| {
                    if (init_list.elements.items.len == 0) {
                        return errors.CodegenError.TypeMismatch;
                    }
                    var result = c.LLVMGetUndef(vector_type);
                    for (init_list.elements.items, 0..) |element, i| {
                        const element_value = try self.generateExpression(element);
                        const casted_value = self.castToType(element_value, element_type);
                        const index = c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(i)), 0);
                        result = c.LLVMBuildInsertElement(self.builder, result, casted_value, index, "simd_init");
                    }
                    _ = c.LLVMBuildStore(self.builder, result, alloca);
                },
                .binary_op => {
                    const expr_value = try self.generateExpression(initializer);
                    _ = c.LLVMBuildStore(self.builder, expr_value, alloca);
                },
                .function_call => {
                    const expr_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                    const casted_value = try self.castWithRules(expr_value, vector_type, initializer);
                    _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                },
                .identifier => {
                    const expr_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                    const casted_value = try self.castWithRules(expr_value, vector_type, initializer);
                    _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                },
                .method_call => {
                    const expr_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                    const casted_value = try self.castWithRules(expr_value, vector_type, initializer);
                    _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                },
                else => {
                    return errors.CodegenError.TypeMismatch;
                },
            }
        }
    } else {
        return errors.CodegenError.TypeMismatch;
    }
}

pub fn generateSimdBinaryOp(self: *CodeGenerator, op: u8, lhs: c.LLVMValueRef, rhs: c.LLVMValueRef) errors.CodegenError!c.LLVMValueRef {
    const lhs_type = c.LLVMTypeOf(lhs);
    const element_type = c.LLVMGetElementType(lhs_type);
    const type_kind = c.LLVMGetTypeKind(element_type);
    return switch (op) {
        '+' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
            c.LLVMBuildFAdd(self.builder, lhs, rhs, "simd_fadd")
        else
            c.LLVMBuildAdd(self.builder, lhs, rhs, "simd_add"),
        '-' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
            c.LLVMBuildFSub(self.builder, lhs, rhs, "simd_fsub")
        else
            c.LLVMBuildSub(self.builder, lhs, rhs, "simd_sub"),
        '*' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
            c.LLVMBuildFMul(self.builder, lhs, rhs, "simd_fmul")
        else
            c.LLVMBuildMul(self.builder, lhs, rhs, "simd_mul"),
        '/' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
            c.LLVMBuildFDiv(self.builder, lhs, rhs, "simd_fdiv")
        else
            c.LLVMBuildSDiv(self.builder, lhs, rhs, "simd_sdiv"),
        'A' => c.LLVMBuildAnd(self.builder, lhs, rhs, "simd_and"),
        '$' => c.LLVMBuildOr(self.builder, lhs, rhs, "simd_or"),
        '^' => c.LLVMBuildXor(self.builder, lhs, rhs, "simd_xor"),
        '=' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
            c.LLVMBuildFCmp(self.builder, c.LLVMRealOEQ, lhs, rhs, "simd_fcmp_eq")
        else
            c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, lhs, rhs, "simd_icmp_eq"),
        '!' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
            c.LLVMBuildFCmp(self.builder, c.LLVMRealONE, lhs, rhs, "simd_fcmp_ne")
        else
            c.LLVMBuildICmp(self.builder, c.LLVMIntNE, lhs, rhs, "simd_icmp_ne"),
        'L' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
            c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, lhs, rhs, "simd_fcmp_lt")
        else
            c.LLVMBuildICmp(self.builder, c.LLVMIntSLT, lhs, rhs, "simd_icmp_slt"),
        'G' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
            c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, lhs, rhs, "simd_fcmp_gt")
        else
            c.LLVMBuildICmp(self.builder, c.LLVMIntSGT, lhs, rhs, "simd_icmp_sgt"),
        'l' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
            c.LLVMBuildFCmp(self.builder, c.LLVMRealOLE, lhs, rhs, "simd_fcmp_le")
        else
            c.LLVMBuildICmp(self.builder, c.LLVMIntSLE, lhs, rhs, "simd_icmp_sle"),
        'g' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
            c.LLVMBuildFCmp(self.builder, c.LLVMRealOGE, lhs, rhs, "simd_fcmp_ge")
        else
            c.LLVMBuildICmp(self.builder, c.LLVMIntSGE, lhs, rhs, "simd_icmp_sge"),
        else => errors.CodegenError.UnsupportedOperation,
    };
}

pub fn generateSimdReduction(self: *CodeGenerator, operation: []const u8, simd_value: c.LLVMValueRef) errors.CodegenError!c.LLVMValueRef {
    const simd_type = c.LLVMTypeOf(simd_value);
    const element_type = c.LLVMGetElementType(simd_type);
    const type_kind = c.LLVMGetTypeKind(element_type);
    const vector_size = c.LLVMGetVectorSize(simd_type);
    var result = c.LLVMBuildExtractElement(self.builder, simd_value, c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), "extract_0");
    var i: u32 = 1;
    while (i < vector_size) : (i += 1) {
        const index = c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(i)), 0);
        const element = c.LLVMBuildExtractElement(self.builder, simd_value, index, "extract_elem");
        if (std.mem.eql(u8, operation, "sum")) {
            result = if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                c.LLVMBuildFAdd(self.builder, result, element, "reduce_fadd")
            else
                c.LLVMBuildAdd(self.builder, result, element, "reduce_add");
        } else if (std.mem.eql(u8, operation, "product")) {
            result = if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                c.LLVMBuildFMul(self.builder, result, element, "reduce_fmul")
            else
                c.LLVMBuildMul(self.builder, result, element, "reduce_mul");
        } else if (std.mem.eql(u8, operation, "min")) {
            if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind) {
                const cmp = c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, result, element, "min_cmp");
                result = c.LLVMBuildSelect(self.builder, cmp, result, element, "reduce_fmin");
            } else {
                const cmp = c.LLVMBuildICmp(self.builder, c.LLVMIntSLT, result, element, "min_cmp");
                result = c.LLVMBuildSelect(self.builder, cmp, result, element, "reduce_min");
            }
        } else if (std.mem.eql(u8, operation, "max")) {
            if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind) {
                const cmp = c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, result, element, "max_cmp");
                result = c.LLVMBuildSelect(self.builder, cmp, result, element, "reduce_fmax");
            } else {
                const cmp = c.LLVMBuildICmp(self.builder, c.LLVMIntSGT, result, element, "max_cmp");
                result = c.LLVMBuildSelect(self.builder, cmp, result, element, "reduce_max");
            }
        } else {
            return errors.CodegenError.UnsupportedOperation;
        }
    }
    return result;
}

pub fn handleSimdAssignment(self: *CodeGenerator, arr_ass: ast.ArrayAssignment) errors.CodegenError!bool {
    const array_name = try self.getBaseIdentifierName(arr_ass.array);
    defer self.allocator.free(array_name);
    const var_info = CodeGenerator.getVariable(self, array_name) orelse return false;

    if (!std.mem.startsWith(u8, var_info.type_name, "simd<")) {
        return false;
    }

    const simd_val = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_simd");
    var index_value = try self.generateExpression(arr_ass.index);
    index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
    const element_type = c.LLVMGetElementType(var_info.type_ref);
    const element_value = try self.generateExpression(arr_ass.value);
    const casted_value = try self.castWithRules(element_value, element_type, arr_ass.value);
    const updated_simd = c.LLVMBuildInsertElement(self.builder, simd_val, casted_value, index_value, "simd_insert");
    _ = c.LLVMBuildStore(self.builder, updated_simd, var_info.value);
    return true;
}

pub fn generateSimdCompoundAssignment(self: *CodeGenerator, simd_cass: ast.SimdCompoundAssignment) errors.CodegenError!void {
    const simd_name = try self.getBaseIdentifierName(simd_cass.simd);
    defer self.allocator.free(simd_name);
    const var_info = CodeGenerator.getVariable(self, simd_name) orelse return errors.CodegenError.UndefinedVariable;

    const current_simd = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_simd");

    var index_value = try self.generateExpression(simd_cass.index);
    index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));

    const current_element = c.LLVMBuildExtractElement(self.builder, current_simd, index_value, "extract_elem");

    const element_type = c.LLVMGetElementType(var_info.type_ref);
    const expected_ty_name = self.getTypeNameFromLLVMType(element_type);
    const rhs_raw = try self.generateExpressionWithContext(simd_cass.value, expected_ty_name);
    const rhs_val = try self.castWithRules(rhs_raw, element_type, simd_cass.value);

    const element_kind = c.LLVMGetTypeKind(element_type);
    const new_element = if (element_kind == c.LLVMFloatTypeKind or element_kind == c.LLVMDoubleTypeKind or element_kind == c.LLVMHalfTypeKind) blk: {
        break :blk switch (simd_cass.op) {
            '+' => c.LLVMBuildFAdd(self.builder, current_element, rhs_val, "fadd"),
            '-' => c.LLVMBuildFSub(self.builder, current_element, rhs_val, "fsub"),
            '*' => c.LLVMBuildFMul(self.builder, current_element, rhs_val, "fmul"),
            '/' => c.LLVMBuildFDiv(self.builder, current_element, rhs_val, "fdiv"),
            else => return errors.CodegenError.UnsupportedOperation,
        };
    } else if (element_kind == c.LLVMIntegerTypeKind) blk: {
        break :blk switch (simd_cass.op) {
            '+' => c.LLVMBuildAdd(self.builder, current_element, rhs_val, "add"),
            '-' => c.LLVMBuildSub(self.builder, current_element, rhs_val, "sub"),
            '*' => c.LLVMBuildMul(self.builder, current_element, rhs_val, "mul"),
            '/' => blk2: {
                const is_unsigned = utils.isUnsignedType(expected_ty_name);
                break :blk2 if (is_unsigned)
                    c.LLVMBuildUDiv(self.builder, current_element, rhs_val, "udiv")
                else
                    c.LLVMBuildSDiv(self.builder, current_element, rhs_val, "sdiv");
            },
            '%' => c.LLVMBuildSRem(self.builder, current_element, rhs_val, "srem"),
            else => return errors.CodegenError.UnsupportedOperation,
        };
    } else return errors.CodegenError.UnsupportedOperation;

    const updated_simd = c.LLVMBuildInsertElement(self.builder, current_simd, new_element, index_value, "simd_update");

    _ = c.LLVMBuildStore(self.builder, updated_simd, var_info.value);
}

pub fn handleSimdArrayCompoundAssignment(self: *CodeGenerator, arr_cass: ast.ArrayCompoundAssignment) errors.CodegenError!bool {
    const array_name = try self.getBaseIdentifierName(arr_cass.array);
    defer self.allocator.free(array_name);
    const var_info = CodeGenerator.getVariable(self, array_name) orelse return false;

    if (!std.mem.startsWith(u8, var_info.type_name, "simd<")) {
        return false;
    }

    var index_value = try self.generateExpression(arr_cass.index);
    index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));

    const current_simd = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_simd");
    const current_element = c.LLVMBuildExtractElement(self.builder, current_simd, index_value, "extract_elem");
    const element_type = c.LLVMGetElementType(var_info.type_ref);
    const expected_ty_name = self.getTypeNameFromLLVMType(element_type);
    const rhs_raw = try self.generateExpressionWithContext(arr_cass.value, expected_ty_name);
    const rhs_val = try self.castWithRules(rhs_raw, element_type, arr_cass.value);

    const element_kind = c.LLVMGetTypeKind(element_type);
    const new_element = if (element_kind == c.LLVMFloatTypeKind or element_kind == c.LLVMDoubleTypeKind or element_kind == c.LLVMHalfTypeKind) blk: {
        break :blk switch (arr_cass.op) {
            '+' => c.LLVMBuildFAdd(self.builder, current_element, rhs_val, "fadd"),
            '-' => c.LLVMBuildFSub(self.builder, current_element, rhs_val, "fsub"),
            '*' => c.LLVMBuildFMul(self.builder, current_element, rhs_val, "fmul"),
            '/' => c.LLVMBuildFDiv(self.builder, current_element, rhs_val, "fdiv"),
            else => return errors.CodegenError.UnsupportedOperation,
        };
    } else if (element_kind == c.LLVMIntegerTypeKind) blk: {
        break :blk switch (arr_cass.op) {
            '+' => c.LLVMBuildAdd(self.builder, current_element, rhs_val, "add"),
            '-' => c.LLVMBuildSub(self.builder, current_element, rhs_val, "sub"),
            '*' => c.LLVMBuildMul(self.builder, current_element, rhs_val, "mul"),
            '/' => blk2: {
                const is_unsigned = utils.isUnsignedType(expected_ty_name);
                break :blk2 if (is_unsigned)
                    c.LLVMBuildUDiv(self.builder, current_element, rhs_val, "udiv")
                else
                    c.LLVMBuildSDiv(self.builder, current_element, rhs_val, "sdiv");
            },
            '%' => c.LLVMBuildSRem(self.builder, current_element, rhs_val, "srem"),
            else => return errors.CodegenError.UnsupportedOperation,
        };
    } else return errors.CodegenError.UnsupportedOperation;

    const updated_simd = c.LLVMBuildInsertElement(self.builder, current_simd, new_element, index_value, "simd_update");
    _ = c.LLVMBuildStore(self.builder, updated_simd, var_info.value);
    return true;
}
