const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
const strings = @import("strings.zig");
const structs = @import("structs.zig");
const numeric = @import("numeric.zig");
const simd = @import("simd.zig");
const enums = @import("enums.zig");
const functions = @import("functions.zig");
const array = @import("array.zig");
const error_flow = @import("error_flow.zig");
const llvm = @import("llvm.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

pub fn generateExpressionWithContext(cg: *llvm.CodeGenerator, expr: *ast.Node, expected_type: ?[]const u8) errors.CodegenError!c.LLVMValueRef {
    switch (expr.data) {
        .cast => |cst| {
            if (cst.auto) {
                if (expected_type) |type_name| {
                    const target_ty = try cg.getLLVMType(type_name);
                    const inner = try cg.generateExpression(cst.expr);
                    var source_type_name: ?[]const u8 = null;
                    if (cst.expr.data == .identifier) {
                        const ident = cst.expr.data.identifier;
                        if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                            source_type_name = var_info.type_name;
                        }
                    } else if (cst.expr.data == .qualified_identifier) {
                        const qual_id = cst.expr.data.qualified_identifier;
                        if (qual_id.base.data == .identifier) {
                            const base_name = qual_id.base.data.identifier.name;
                            if (llvm.CodeGenerator.getVariable(cg, base_name)) |base_var| {
                                if (std.mem.startsWith(u8, base_var.type_name, "ptr<") and std.mem.endsWith(u8, base_var.type_name, ">")) {
                                    const inner_type_name = base_var.type_name[4 .. base_var.type_name.len - 1];
                                    const struct_decl = cg.struct_declarations.get(inner_type_name) orelse return errors.CodegenError.TypeMismatch;
                                    const field_map = cg.struct_fields.get(inner_type_name) orelse return errors.CodegenError.TypeMismatch;
                                    const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
                                    source_type_name = struct_decl.fields.items[field_index].type_name;
                                } else {
                                    const struct_decl = cg.struct_declarations.get(base_var.type_name) orelse return errors.CodegenError.TypeMismatch;
                                    const field_map = cg.struct_fields.get(base_var.type_name) orelse return errors.CodegenError.TypeMismatch;
                                    const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
                                    source_type_name = struct_decl.fields.items[field_index].type_name;
                                }
                            }
                        }
                    }
                    return @ptrCast(cg.castToTypeWithSourceInfo(inner, @ptrCast(target_ty), source_type_name));
                } else {
                    return errors.CodegenError.TypeMismatch;
                }
            } else if (cst.type_name) |tn| {
                const target_ty = try cg.getLLVMType(tn);
                const inner = try cg.generateExpression(cst.expr);
                var source_type_name: ?[]const u8 = null;
                if (cst.expr.data == .identifier) {
                    const ident = cst.expr.data.identifier;
                    if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                        source_type_name = var_info.type_name;
                    }
                } else if (cst.expr.data == .function_call) {
                    const call = cst.expr.data.function_call;
                    if (cg.function_return_types.get(call.name)) |ret_type| {
                        source_type_name = ret_type;
                    }
                }
                return cg.castToTypeWithSourceInfo(inner, @ptrCast(target_ty), source_type_name);
            } else {
                return errors.CodegenError.TypeMismatch;
            }
        },
        .float_literal => |float| {
            const float_val = numeric.parseFloatLiteral(float.value) catch 0.0;

            if (expected_type) |type_name| {
                if (std.mem.eql(u8, type_name, "f16")) {
                    return c.LLVMConstReal(c.LLVMHalfTypeInContext(cg.context), float_val);
                } else if (std.mem.eql(u8, type_name, "f32")) {
                    return c.LLVMConstReal(c.LLVMFloatTypeInContext(cg.context), float_val);
                } else if (std.mem.eql(u8, type_name, "f64")) {
                    return c.LLVMConstReal(c.LLVMDoubleTypeInContext(cg.context), float_val);
                }
            }

            return c.LLVMConstReal(c.LLVMDoubleTypeInContext(cg.context), float_val);
        },
        .number_literal => |num| {
            if (std.mem.indexOf(u8, num.value, ".") != null) {
                const float_val = numeric.parseFloatLiteral(num.value) catch 0.0;

                if (expected_type) |type_name| {
                    if (std.mem.eql(u8, type_name, "f16")) {
                        return c.LLVMConstReal(c.LLVMHalfTypeInContext(cg.context), float_val);
                    } else if (std.mem.eql(u8, type_name, "f32")) {
                        return c.LLVMConstReal(c.LLVMFloatTypeInContext(cg.context), float_val);
                    } else if (std.mem.eql(u8, type_name, "f64")) {
                        return c.LLVMConstReal(c.LLVMDoubleTypeInContext(cg.context), float_val);
                    }
                }

                return c.LLVMConstReal(c.LLVMDoubleTypeInContext(cg.context), float_val);
            } else {
                if (expected_type) |type_name| {
                    const llvm_type = try cg.getLLVMType(type_name);
                    const expected_kind = c.LLVMGetTypeKind(llvm_type);
                    if (expected_kind != c.LLVMIntegerTypeKind) {
                        const value = numeric.parseNumericLiteral(num.value) catch 0;
                        return c.LLVMConstInt(c.LLVMInt64TypeInContext(cg.context), @as(c_ulonglong, @intCast(value)), 0);
                    }
                    if (std.mem.startsWith(u8, type_name, "u")) {
                        if (std.mem.startsWith(u8, num.value, "-")) {
                            const sval = numeric.parseNumericLiteral(num.value) catch {
                                return errors.CodegenError.TypeMismatch;
                            };
                            const uval: u64 = @bitCast(sval);
                            return c.LLVMConstInt(llvm_type, @as(c_ulonglong, @intCast(uval)), 0);
                        } else {
                            const value = numeric.parseNumericLiteralUnsigned(num.value) catch {
                                return errors.CodegenError.TypeMismatch;
                            };
                            return c.LLVMConstInt(llvm_type, @as(c_ulonglong, @intCast(value)), 0);
                        }
                    } else {
                        const value = numeric.parseNumericLiteral(num.value) catch {
                            return errors.CodegenError.TypeMismatch;
                        };
                        return c.LLVMConstInt(llvm_type, @as(c_ulonglong, @intCast(value)), 0);
                    }
                } else {
                    const value = numeric.parseNumericLiteral(num.value) catch 0;
                    return c.LLVMConstInt(c.LLVMInt64TypeInContext(cg.context), @as(c_ulonglong, @intCast(value)), 0);
                }
            }
        },
        .char_literal => |char| {
            return c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), @as(c_ulonglong, @intCast(char.value)), 0);
        },
        .identifier => |ident| {
            if (std.mem.eql(u8, ident.name, "true")) {
                return c.LLVMConstInt(c.LLVMInt1TypeInContext(cg.context), 1, 0);
            } else if (std.mem.eql(u8, ident.name, "false")) {
                return c.LLVMConstInt(c.LLVMInt1TypeInContext(cg.context), 0, 0);
            }

            if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                return c.LLVMBuildLoad2(cg.builder, @ptrCast(var_info.type_ref), @ptrCast(var_info.value), "load");
            }
            var func_iter = c.LLVMGetFirstFunction(cg.module);
            while (func_iter != null) : (func_iter = c.LLVMGetNextFunction(func_iter)) {
                const func_name_ptr = c.LLVMGetValueName(func_iter);
                if (func_name_ptr != null) {
                    const func_name_slice = std.mem.span(func_name_ptr);
                    if (std.mem.eql(u8, func_name_slice, ident.name)) {
                        return func_iter;
                    }
                }
            }
            if (cg.functions.get(ident.name)) |declared_func| {
                return declared_func;
            }
            if (cg.function_overloads.get(ident.name)) |overloads| {
                if (overloads.items.len == 1) {
                    const match = overloads.items[0];
                    if (!match.is_template) {
                        var cand_param_type_names: std.ArrayList([]const u8) = .empty;
                        defer cand_param_type_names.deinit(cg.allocator);
                        for (match.func_node.parameters.items) |p| {
                            if (!utils.isVarArgType(p.type_name)) try cand_param_type_names.append(cg.allocator, p.type_name);
                        }
                        const mangled = try functions.getMangledName(cg.allocator, match.func_node.name, cand_param_type_names.items, cg);
                        defer cg.allocator.free(mangled);
                        if (cg.functions.get(mangled)) |f| return f;
                    }
                }
            }
            return errors.CodegenError.UndefinedVariable;
        },
        .null_literal => {
            if (expected_type) |type_name| {
                if (std.mem.startsWith(u8, type_name, "ptr<")) {
                    const llvm_type = try cg.getLLVMType(type_name);
                    return c.LLVMConstNull(llvm_type);
                } else {
                    return errors.CodegenError.NullNotAllowedInNonPointerType;
                }
            } else {
                const void_ptr_type = c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0);
                return c.LLVMConstNull(void_ptr_type);
            }
        },
        .binary_op => |b| {
            if (expected_type) |type_name| {
                const target_ty = try cg.getLLVMType(type_name);
                const kind = c.LLVMGetTypeKind(target_ty);
                if (kind == c.LLVMPointerTypeKind) {
                    return generateBinaryOp(cg, b);
                }

                const lhs_val = try cg.generateExpressionWithContext(b.lhs, expected_type);
                const rhs_val = try cg.generateExpressionWithContext(b.rhs, expected_type);
                if (kind == c.LLVMFloatTypeKind or kind == c.LLVMDoubleTypeKind or kind == c.LLVMHalfTypeKind) {
                    const lhs_casted = try cg.castWithRules(lhs_val, target_ty, b.lhs);
                    const rhs_casted = try cg.castWithRules(rhs_val, target_ty, b.rhs);
                    return switch (b.op) {
                        '+' => c.LLVMBuildFAdd(cg.builder, lhs_casted, rhs_casted, "fadd"),
                        '-' => c.LLVMBuildFSub(cg.builder, lhs_casted, rhs_casted, "fsub"),
                        '*' => c.LLVMBuildFMul(cg.builder, lhs_casted, rhs_casted, "fmul"),
                        '/' => c.LLVMBuildFDiv(cg.builder, lhs_casted, rhs_casted, "fdiv"),
                        '%' => blk: {
                            cg.uses_float_modulo = true;
                            break :blk c.LLVMBuildFRem(cg.builder, lhs_casted, rhs_casted, "frem");
                        },
                        '&', '|', 'A', '$', '^', '<', '>' => try generateBinaryOp(cg, b),
                        else => errors.CodegenError.UnsupportedOperation,
                    };
                } else if (kind == c.LLVMIntegerTypeKind) {
                    if (c.LLVMGetTypeKind(c.LLVMTypeOf(lhs_val)) == c.LLVMPointerTypeKind or
                        c.LLVMGetTypeKind(c.LLVMTypeOf(rhs_val)) == c.LLVMPointerTypeKind)
                    {
                        return generateBinaryOp(cg, b);
                    }
                    var lhs_int = lhs_val;
                    var rhs_int = rhs_val;
                    if (c.LLVMTypeOf(lhs_int) != target_ty) {
                        lhs_int = cg.castToTypeWithSourceInfo(lhs_int, target_ty, try cg.inferType(b.lhs));
                    }
                    if (c.LLVMTypeOf(rhs_int) != target_ty) {
                        rhs_int = cg.castToTypeWithSourceInfo(rhs_int, target_ty, try cg.inferType(b.rhs));
                    }
                    return switch (b.op) {
                        '+' => c.LLVMBuildAdd(cg.builder, lhs_int, rhs_int, "add"),
                        '-' => c.LLVMBuildSub(cg.builder, lhs_int, rhs_int, "sub"),
                        '*' => c.LLVMBuildMul(cg.builder, lhs_int, rhs_int, "mul"),
                        '/' => blk: {
                            const is_unsigned = utils.isUnsignedType(cg.getTypeNameFromLLVMType(target_ty));
                            break :blk if (is_unsigned)
                                c.LLVMBuildUDiv(cg.builder, lhs_int, rhs_int, "udiv")
                            else
                                c.LLVMBuildSDiv(cg.builder, lhs_int, rhs_int, "sdiv");
                        },
                        '%' => blk: {
                            const is_unsigned = utils.isUnsignedType(cg.getTypeNameFromLLVMType(target_ty));
                            break :blk if (is_unsigned)
                                c.LLVMBuildURem(cg.builder, lhs_int, rhs_int, "urem")
                            else
                                c.LLVMBuildSRem(cg.builder, lhs_int, rhs_int, "srem");
                        },
                        '&', '|', 'A', '$', '^', '<', '>' => try generateBinaryOp(cg, b),
                        else => errors.CodegenError.UnsupportedOperation,
                    };
                } else {
                    return errors.CodegenError.UnsupportedOperation;
                }
            }
            return generateBinaryOp(cg, b);
        },
        .unary_op => |un| {
            switch (un.op) {
                '-' => {
                    const operand_val = try cg.generateExpressionWithContext(un.operand, expected_type);
                    const operand_type = c.LLVMTypeOf(operand_val);
                    const type_kind = c.LLVMGetTypeKind(operand_type);
                    return switch (type_kind) {
                        c.LLVMIntegerTypeKind => c.LLVMBuildNeg(cg.builder, operand_val, "neg"),
                        c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => c.LLVMBuildFNeg(cg.builder, operand_val, "fneg"),
                        else => errors.CodegenError.UnsupportedOperation,
                    };
                },
                '+' => {
                    return try cg.generateExpressionWithContext(un.operand, expected_type);
                },
                '~' => {
                    const operand_val = try cg.generateExpressionWithContext(un.operand, expected_type);
                    const operand_type = c.LLVMTypeOf(operand_val);
                    const type_kind = c.LLVMGetTypeKind(operand_type);
                    if (type_kind == c.LLVMIntegerTypeKind) {
                        return c.LLVMBuildNot(cg.builder, operand_val, "bitnot");
                    } else {
                        return errors.CodegenError.UnsupportedOperation;
                    }
                },
                '*' => {
                    var ptr_val: c.LLVMValueRef = undefined;
                    if (expected_type) |tn| {
                        const inner_expected = std.fmt.allocPrint(cg.allocator, "ptr<{s}>", .{tn}) catch return errors.CodegenError.OutOfMemory;
                        defer cg.allocator.free(inner_expected);
                        ptr_val = try cg.generateExpressionWithContext(un.operand, inner_expected);
                    } else {
                        ptr_val = try cg.generateExpression(un.operand);
                    }
                    const ptr_ty = c.LLVMTypeOf(ptr_val);
                    if (c.LLVMGetTypeKind(ptr_ty) != c.LLVMPointerTypeKind) return errors.CodegenError.TypeMismatch;
                    if (expected_type) |tn2| {
                        const load_ty = try cg.getLLVMType(tn2);
                        if (c.LLVMGetTypeKind(load_ty) == c.LLVMFunctionTypeKind) return ptr_val;
                        return c.LLVMBuildLoad2(cg.builder, @ptrCast(load_ty), ptr_val, "deref_ctx");
                    }
                    const elem_ty = c.LLVMGetElementType(ptr_ty);
                    if (c.LLVMGetTypeKind(elem_ty) == c.LLVMFunctionTypeKind) return ptr_val;
                    return c.LLVMBuildLoad2(cg.builder, elem_ty, ptr_val, "deref");
                },
                else => return cg.generateExpression(expr),
            }
        },
        .array_initializer => |init_list| {
            if (expected_type) |type_name| {
                if (std.mem.startsWith(u8, type_name, "simd<")) {
                    const vector_type = try cg.getLLVMType(type_name);
                    const element_type = c.LLVMGetElementType(vector_type);

                    if (init_list.elements.items.len == 0) {
                        return errors.CodegenError.TypeMismatch;
                    }

                    var result = c.LLVMGetUndef(vector_type);
                    for (init_list.elements.items, 0..) |element, i| {
                        const element_value = try cg.generateExpression(element);
                        const casted_value = cg.castToType(element_value, element_type);
                        const index = c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), @as(c_ulonglong, @intCast(i)), 0);
                        result = c.LLVMBuildInsertElement(cg.builder, result, casted_value, index, "simd_init");
                    }
                    return result;
                }

                if (llvm.CodeGenerator.parseFixedArrayType(type_name)) |array_info| {
                    if (init_list.elements.items.len > array_info.array_size) {
                        cg.reportErrorFmt("Array initializer has {d} elements, but target type '{s}' has size {d}", .{ init_list.elements.items.len, type_name, array_info.array_size }, "Reduce elements or increase declared array size");
                        return errors.CodegenError.TypeMismatch;
                    }

                    const array_type = try cg.getLLVMType(type_name);
                    const element_type = c.LLVMGetElementType(array_type);
                    var result = c.LLVMConstNull(array_type);

                    for (init_list.elements.items, 0..) |element, i| {
                        const element_value_raw = try cg.generateExpressionWithContext(element, array_info.element_type_name);
                        const casted_value = try cg.castWithRules(element_value_raw, element_type, element);
                        result = c.LLVMBuildInsertValue(cg.builder, result, casted_value, @intCast(i), "array_init");
                    }

                    return result;
                }
            }
            return errors.CodegenError.TypeMismatch;
        },
        .simd_initializer => |simd_init| {
            if (expected_type) |type_name| {
                if (std.mem.startsWith(u8, type_name, "simd<")) {
                    const vector_type = try cg.getLLVMType(type_name);
                    const element_type = c.LLVMGetElementType(vector_type);
                    if (simd_init.elements.items.len == 0) {
                        return errors.CodegenError.TypeMismatch;
                    }
                    var result = c.LLVMGetUndef(vector_type);
                    for (simd_init.elements.items, 0..) |element, i| {
                        const element_value = try cg.generateExpression(element);
                        const casted_value = cg.castToType(element_value, element_type);
                        const index = c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), @as(c_ulonglong, @intCast(i)), 0);
                        result = c.LLVMBuildInsertElement(cg.builder, result, casted_value, index, "simd_init");
                    }
                    return result;
                }
            }
            return errors.CodegenError.TypeMismatch;
        },
        .function_call => |call| {
            return try llvm.CodeGenerator.generateFunctionCall(cg, call, expected_type);
        },
        .handled_call_stmt => |handled| {
            return try error_flow.generateHandledCall(cg, handled, expected_type);
        },
        .expression_block => |block| {
            return try cg.generateExpressionBlock(block);
        },
        else => return cg.generateExpression(expr),
    }
}

fn mergeTypes(cg: *llvm.CodeGenerator, t1: []const u8, t2: []const u8) []const u8 {
    _ = cg;
    if (std.mem.eql(u8, t1, t2)) return t1;
    if (utils.isFloatType(t1)) return t1;
    if (utils.isFloatType(t2)) return t2;

    const w1 = utils.getIntWidth(t1);
    const w2 = utils.getIntWidth(t2);

    if (w1 > w2) return t1;
    if (w2 > w1) return t2;

    if (utils.isUnsignedType(t1)) return t1;
    if (utils.isUnsignedType(t2)) return t2;

    return t1;
}

fn resolveStructTypeName(cg: *llvm.CodeGenerator, struct_type_name: []const u8) ?[]const u8 {
    if (cg.struct_declarations.get(struct_type_name) != null) {
        return struct_type_name;
    }

    var it = cg.struct_declarations.iterator();
    while (it.next()) |entry| {
        const candidate = entry.key_ptr.*;
        if (std.mem.endsWith(u8, candidate, struct_type_name)) {
            if (candidate.len == struct_type_name.len) {
                return candidate;
            }
            const sep_idx = candidate.len - struct_type_name.len - 1;
            if (candidate[sep_idx] == '.') {
                return candidate;
            }
        }
    }

    return null;
}

fn getStructFieldTypeName(cg: *llvm.CodeGenerator, struct_type_name: []const u8, field_name: []const u8) ?[]const u8 {
    const resolved_name = resolveStructTypeName(cg, struct_type_name) orelse return null;
    const struct_decl = cg.struct_declarations.get(resolved_name) orelse return null;
    const field_map = cg.struct_fields.get(resolved_name) orelse return null;
    const field_index = field_map.get(field_name) orelse return null;
    return struct_decl.fields.items[field_index].type_name;
}

pub fn inferType(cg: *llvm.CodeGenerator, node: *ast.Node) errors.CodegenError![]const u8 {
    switch (node.data) {
        .identifier => |ident| {
            if (std.mem.eql(u8, ident.name, "true") or std.mem.eql(u8, ident.name, "false")) return "bool";
            if (cg.error_codes.contains(ident.name)) return "error";
            if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                return var_info.type_name;
            }
            return "void";
        },
        .number_literal => return "i32",
        .float_literal => return "f64",
        .bool_literal => return "bool",
        .string_literal => return "ptr<u8>",
        .cast => |cst| {
            if (cst.type_name) |tn| return tn;
            return cg.inferType(cst.expr);
        },
        .expression_block => |block| return block.type_name,
        .binary_op => |bin_op| {
            if (bin_op.op == '&' or bin_op.op == '|') return "bool";
            const lhs_type = try cg.inferType(bin_op.lhs);
            const rhs_type = try cg.inferType(bin_op.rhs);
            if ((bin_op.op == '+' or bin_op.op == '-') and
                ((std.mem.startsWith(u8, lhs_type, "ptr<") and std.mem.endsWith(u8, lhs_type, ">")) or
                    (std.mem.startsWith(u8, rhs_type, "ptr<") and std.mem.endsWith(u8, rhs_type, ">"))))
            {
                if (std.mem.startsWith(u8, lhs_type, "ptr<") and std.mem.endsWith(u8, lhs_type, ">")) {
                    return lhs_type;
                }
                return rhs_type;
            }
            return mergeTypes(cg, lhs_type, rhs_type);
        },
        .qualified_identifier => |qual_id| {
            const base_type = try cg.inferType(qual_id.base);
            var struct_type_name = base_type;
            if (std.mem.startsWith(u8, struct_type_name, "ptr<") and std.mem.endsWith(u8, struct_type_name, ">")) {
                struct_type_name = struct_type_name[4 .. struct_type_name.len - 1];
            }

            if (getStructFieldTypeName(cg, struct_type_name, qual_id.field)) |field_type| {
                return field_type;
            }

            if (cg.getQualifiedFieldPtrAndType(node)) |pair| {
                return cg.getTypeNameFromLLVMType(pair.ty);
            } else |_| {}

            return "void";
        },
        .function_call => |call| {
            if (cg.function_return_types.get(call.name)) |ret_type| {
                return ret_type;
            }
            if (utils.LIBC_FUNCTIONS.get(call.name)) |signature| {
                return switch (signature.return_type) {
                    .void_type => "void",
                    .int_type => "i32",
                    .char_ptr_type => "ptr<u8>",
                    .size_t_type => "u64",
                    .file_ptr_type => "ptr<u8>",
                    .long_type => "i64",
                    .double_type => "f64",
                };
            }
            if (cg.c_function_declarations.get(call.name)) |is_wrapped| {
                _ = is_wrapped;
            }
            return "i32";
        },
        .handled_call_stmt => |handled| {
            return cg.inferType(handled.call);
        },
        .array_index => |idx| {
            const arr_type = try cg.inferType(idx.array);
            if (std.mem.startsWith(u8, arr_type, "arr<")) {
                const inner = arr_type[4 .. arr_type.len - 1];
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
                    return std.mem.trim(u8, inner[0..pos], " \t");
                }
            } else if (std.mem.startsWith(u8, arr_type, "ptr<")) {
                const inner = arr_type[4 .. arr_type.len - 1];
                return std.mem.trim(u8, inner, " \t");
            }
            return "void";
        },
        .unary_op => |un| {
            if (un.op == '&') {
                const operand_type = switch (un.operand.data) {
                    .identifier => |ident| blk: {
                        if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                            break :blk var_info.type_name;
                        }
                        break :blk try cg.inferType(un.operand);
                    },
                    .qualified_identifier => blk: {
                        const pair = try cg.getQualifiedFieldPtrAndType(un.operand);
                        break :blk cg.getTypeNameFromLLVMType(pair.ty);
                    },
                    else => try cg.inferType(un.operand),
                };
                return try std.fmt.allocPrint(cg.allocator, "ptr<{s}>", .{operand_type});
            }
            return cg.inferType(un.operand);
        },
        .comparison => return "bool",
        else => return "void",
    }
}

pub fn generateExpression(cg: *llvm.CodeGenerator, expr: *ast.Node) errors.CodegenError!c.LLVMValueRef {
    switch (expr.data) {
        .cast => |cst| {
            if (cst.auto) return try cg.generateExpression(cst.expr);
            if (cst.type_name) |tn| {
                const target_ty = try cg.getLLVMType(tn);
                const inner = try cg.generateExpression(cst.expr);
                const source_type_name = try cg.inferType(cst.expr);
                return cg.castToTypeWithSourceInfo(inner, @ptrCast(target_ty), source_type_name);
            }
            return errors.CodegenError.TypeMismatch;
        },
        .expression_block => |block| {
            return try cg.generateExpressionBlock(block);
        },
        .handled_call_stmt => |handled| {
            return try error_flow.generateHandledCall(cg, handled, null);
        },
        .identifier => |ident| {
            if (std.mem.eql(u8, ident.name, "true")) {
                return c.LLVMConstInt(c.LLVMInt1TypeInContext(cg.context), 1, 0);
            } else if (std.mem.eql(u8, ident.name, "false")) {
                return c.LLVMConstInt(c.LLVMInt1TypeInContext(cg.context), 0, 0);
            }

            if (cg.error_codes.get(ident.name)) |code| {
                const code_bits: c_ulonglong = @bitCast(@as(i64, code));
                return c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), code_bits, 1);
            }

            if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                return c.LLVMBuildLoad2(cg.builder, @ptrCast(var_info.type_ref), @ptrCast(var_info.value), "load");
            }
            var func_iter = c.LLVMGetFirstFunction(cg.module);
            while (func_iter != null) : (func_iter = c.LLVMGetNextFunction(func_iter)) {
                const func_name_ptr = c.LLVMGetValueName(func_iter);
                if (func_name_ptr != null) {
                    const func_name_slice = std.mem.span(func_name_ptr);
                    if (std.mem.eql(u8, func_name_slice, ident.name)) {
                        return func_iter;
                    }
                }
            }
            if (cg.functions.get(ident.name)) |declared_func| {
                return declared_func;
            }
            if (cg.function_overloads.get(ident.name)) |overloads| {
                if (overloads.items.len == 1) {
                    const match = overloads.items[0];
                    if (!match.is_template) {
                        var cand_param_type_names: std.ArrayList([]const u8) = .empty;
                        defer cand_param_type_names.deinit(cg.allocator);
                        for (match.func_node.parameters.items) |p| {
                            if (!utils.isVarArgType(p.type_name)) try cand_param_type_names.append(cg.allocator, p.type_name);
                        }
                        const mangled = try functions.getMangledName(cg.allocator, match.func_node.name, cand_param_type_names.items, cg);
                        defer cg.allocator.free(mangled);
                        if (cg.functions.get(mangled)) |f| return f;
                    }
                }
            }
            return errors.CodegenError.UndefinedVariable;
        },
        .qualified_identifier => {
            const qual_id = expr.data.qualified_identifier;

            if (try enums.tryLoadEnumValue(cg, qual_id)) |enum_value| {
                return enum_value;
            }
            var result_value: c.LLVMValueRef = undefined;
            var result_type: c.LLVMTypeRef = undefined;
            var result_type_name: []const u8 = "";
            switch (qual_id.base.data) {
                .identifier => {
                    const base_name = qual_id.base.data.identifier.name;
                    const var_info = llvm.CodeGenerator.getVariable(cg, base_name) orelse return errors.CodegenError.UndefinedVariable;
                    result_value = @ptrCast(var_info.value);
                    result_type = @ptrCast(var_info.type_ref);
                    result_type_name = var_info.type_name;
                },
                .array_index => {
                    const array_index = qual_id.base.data.array_index;
                    const array_name = try cg.getBaseIdentifierName(qual_id.base);
                    defer cg.allocator.free(array_name);
                    const array_var_info = llvm.CodeGenerator.getVariable(cg, array_name) orelse return errors.CodegenError.UndefinedVariable;
                    var index_value = try cg.generateExpression(array_index.index);
                    if (std.mem.startsWith(u8, array_var_info.type_name, "ptr<") and std.mem.endsWith(u8, array_var_info.type_name, ">")) {
                        const element_type_name = array_var_info.type_name[4 .. array_var_info.type_name.len - 1];
                        const element_type = try cg.getLLVMType(element_type_name);
                        index_value = cg.castToType(index_value, c.LLVMInt64TypeInContext(cg.context));
                        const loaded_ptr = c.LLVMBuildLoad2(cg.builder, array_var_info.type_ref, array_var_info.value, "load_ptr_for_field_access");
                        var indices = [_]c.LLVMValueRef{index_value};
                        result_value = c.LLVMBuildGEP2(cg.builder, element_type, loaded_ptr, &indices[0], 1, "ptr_element_ptr");
                        result_type = element_type;
                        const elem_type_kind = c.LLVMGetTypeKind(element_type);
                        if (elem_type_kind == c.LLVMStructTypeKind) {
                            const name_ptr = c.LLVMGetStructName(element_type);
                            if (name_ptr != null) {
                                result_type_name = std.mem.span(name_ptr);
                            }
                        }
                    } else {
                        index_value = cg.castToType(index_value, c.LLVMInt32TypeInContext(cg.context));
                        const element_type = c.LLVMGetElementType(@ptrCast(array_var_info.type_ref));
                        var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0), index_value };
                        result_value = c.LLVMBuildGEP2(cg.builder, @ptrCast(array_var_info.type_ref), @ptrCast(array_var_info.value), &indices[0], 2, "array_element_ptr");
                        result_type = element_type;
                        const elem_type_kind = c.LLVMGetTypeKind(element_type);
                        if (elem_type_kind == c.LLVMStructTypeKind) {
                            const name_ptr = c.LLVMGetStructName(element_type);
                            if (name_ptr != null) {
                                result_type_name = std.mem.span(name_ptr);
                            }
                        }
                    }
                },
                .qualified_identifier => {
                    result_value = try cg.generateExpression(qual_id.base);
                    result_type = c.LLVMTypeOf(result_value);
                    const type_kind = c.LLVMGetTypeKind(result_type);
                    if (type_kind == c.LLVMStructTypeKind) {
                        const name_ptr = c.LLVMGetStructName(result_type);
                        if (name_ptr != null) {
                            result_type_name = std.mem.span(name_ptr);
                        }
                    }
                },
                else => {
                    result_value = try cg.generateExpression(qual_id.base);
                    result_type = c.LLVMTypeOf(result_value);
                },
            }
            const type_kind = c.LLVMGetTypeKind(result_type);
            if (type_kind == c.LLVMStructTypeKind) {
                if (result_type_name.len == 0) {
                    const name_ptr = c.LLVMGetStructName(result_type);
                    if (name_ptr != null) {
                        result_type_name = std.mem.span(name_ptr);
                    }
                }
                const struct_type = cg.struct_types.get(result_type_name) orelse return errors.CodegenError.TypeMismatch;
                const field_map = cg.struct_fields.get(result_type_name) orelse return errors.CodegenError.TypeMismatch;
                const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
                var struct_ptr = result_value;
                if (c.LLVMGetTypeKind(c.LLVMTypeOf(result_value)) != c.LLVMPointerTypeKind) {
                    const temp_alloca = cg.buildAllocaAtEntry(c.LLVMTypeOf(result_value), "temp_struct");
                    _ = c.LLVMBuildStore(cg.builder, result_value, temp_alloca);
                    struct_ptr = temp_alloca;
                }
                const field_ptr = try cg.getStructFieldPointer(@ptrCast(struct_type), @ptrCast(struct_ptr), field_index);
                const field_type = try structs.getFieldType(cg, struct_type, field_index);
                const field_type_kind = c.LLVMGetTypeKind(field_type);
                if (field_type_kind == c.LLVMArrayTypeKind) {
                    return @ptrCast(field_ptr);
                }
                const load_instr = c.LLVMBuildLoad2(cg.builder, field_type, @ptrCast(field_ptr), "struct_field");
                const alignment = cg.getAlignmentForType(@ptrCast(field_type));
                c.LLVMSetAlignment(load_instr, alignment);
                return load_instr;
            } else if (type_kind == c.LLVMPointerTypeKind) {
                if (qual_id.base.data == .identifier) {
                    const base_name = qual_id.base.data.identifier.name;
                    if (llvm.CodeGenerator.getVariable(cg, base_name)) |base_var| {
                        const base_type_name = base_var.type_name;
                        if (std.mem.startsWith(u8, base_type_name, "ptr<") and std.mem.endsWith(u8, base_type_name, ">")) {
                            const inner_type_name = base_type_name[4 .. base_type_name.len - 1];
                            const struct_type = cg.struct_types.get(inner_type_name) orelse return errors.CodegenError.TypeMismatch;
                            const field_map = cg.struct_fields.get(inner_type_name) orelse return errors.CodegenError.TypeMismatch;
                            const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
                            const loaded_ptr = c.LLVMBuildLoad2(cg.builder, result_type, result_value, "loaded_ptr");
                            const field_ptr = try cg.getStructFieldPointer(@ptrCast(struct_type), @ptrCast(loaded_ptr), field_index);
                            const field_type = try structs.getFieldType(cg, struct_type, field_index);
                            const field_type_kind = c.LLVMGetTypeKind(field_type);
                            if (field_type_kind == c.LLVMArrayTypeKind) {
                                return @ptrCast(field_ptr);
                            }
                            const load_instr = c.LLVMBuildLoad2(@ptrCast(cg.builder), @ptrCast(field_type), @ptrCast(field_ptr), "struct_field");
                            const alignment = cg.getAlignmentForType(field_type);
                            c.LLVMSetAlignment(load_instr, alignment);
                            return load_instr;
                        }
                    }
                }

                const pointee_type = c.LLVMGetElementType(result_type);
                const pointee_type_kind = c.LLVMGetTypeKind(pointee_type);
                if (pointee_type_kind == c.LLVMStructTypeKind) {
                    const name_ptr = c.LLVMGetStructName(pointee_type);
                    if (name_ptr != null) {
                        const struct_name = std.mem.span(name_ptr);
                        const struct_type = cg.struct_types.get(struct_name) orelse return errors.CodegenError.TypeMismatch;
                        const field_map = cg.struct_fields.get(struct_name) orelse return errors.CodegenError.TypeMismatch;
                        const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
                        const field_ptr = try cg.getStructFieldPointer(struct_type, result_value, field_index);
                        const field_type = try structs.getFieldType(cg, struct_type, field_index);
                        const field_type_kind = c.LLVMGetTypeKind(field_type);
                        if (field_type_kind == c.LLVMArrayTypeKind) {
                            return field_ptr;
                        }
                        const load_instr = c.LLVMBuildLoad2(cg.builder, field_type, field_ptr, "struct_field");
                        const alignment = cg.getAlignmentForType(field_type);
                        c.LLVMSetAlignment(load_instr, alignment);
                        return load_instr;
                    }
                }
            }
            return errors.CodegenError.TypeMismatch;
        },
        .float_literal => |float| {
            const float_val = numeric.parseFloatLiteral(float.value) catch 0.0;
            return c.LLVMConstReal(c.LLVMDoubleTypeInContext(cg.context), float_val);
        },
        .number_literal => |num| {
            if (std.mem.indexOf(u8, num.value, ".") != null) {
                const float_val = numeric.parseFloatLiteral(num.value) catch 0.0;
                return c.LLVMConstReal(c.LLVMDoubleTypeInContext(cg.context), float_val);
            } else {
                const value = numeric.parseNumericLiteral(num.value) catch 0;
                return c.LLVMConstInt(c.LLVMInt64TypeInContext(cg.context), @as(c_ulonglong, @intCast(value)), 0);
            }
        },
        .char_literal => |char| {
            return c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), @as(c_ulonglong, @intCast(char.value)), 0);
        },
        .bool_literal => |bool_val| {
            return c.LLVMConstInt(c.LLVMInt1TypeInContext(cg.context), if (bool_val.value) 1 else 0, 0);
        },
        .null_literal => {
            const void_ptr_type = c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0);
            return c.LLVMConstNull(void_ptr_type);
        },
        .string_literal => |str| {
            return try strings.generateStringLiteral(cg, str);
        },
        .unary_op => |un| {
            switch (un.op) {
                '-' => {
                    const operand_val = try cg.generateExpression(un.operand);
                    const operand_type = c.LLVMTypeOf(operand_val);
                    const type_kind = c.LLVMGetTypeKind(operand_type);
                    switch (type_kind) {
                        c.LLVMIntegerTypeKind => {
                            return c.LLVMBuildNeg(cg.builder, operand_val, "neg");
                        },
                        c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind => {
                            return c.LLVMBuildFNeg(cg.builder, operand_val, "fneg");
                        },
                        else => {
                            return errors.CodegenError.UnsupportedOperation;
                        },
                    }
                },
                '+' => {
                    const operand_val = try cg.generateExpression(un.operand);
                    return operand_val;
                },
                '!' => {
                    const operand_val = try cg.generateExpression(un.operand);
                    const bool_val = cg.convertToBool(operand_val);
                    const true_val = c.LLVMConstInt(c.LLVMInt1TypeInContext(cg.context), 1, 0);
                    return c.LLVMBuildXor(cg.builder, bool_val, true_val, "not");
                },
                '~' => {
                    const operand_val = try cg.generateExpression(un.operand);
                    const operand_type = c.LLVMTypeOf(operand_val);
                    const type_kind = c.LLVMGetTypeKind(operand_type);
                    if (type_kind == c.LLVMIntegerTypeKind) {
                        return c.LLVMBuildNot(cg.builder, operand_val, "bitnot");
                    } else {
                        return errors.CodegenError.UnsupportedOperation;
                    }
                },
                '&' => {
                    if (un.operand.data == .identifier) {
                        const ident = un.operand.data.identifier;
                        if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                            return var_info.value;
                        }
                    } else if (un.operand.data == .qualified_identifier) {
                        const pair = try cg.getQualifiedFieldPtrAndType(un.operand);
                        return pair.ptr;
                    } else if (un.operand.data == .array_index) {
                        const arr_idx = un.operand.data.array_index;
                        var collected_indices: std.ArrayList(c.LLVMValueRef) = .empty;
                        defer collected_indices.deinit(cg.allocator);

                        const base_node = try cg.collectArrayIndices(arr_idx.array, &collected_indices);
                        var index_value = try cg.generateExpression(arr_idx.index);

                        var base_ptr: c.LLVMValueRef = undefined;
                        var base_type: c.LLVMTypeRef = undefined;
                        var base_type_name: []const u8 = "";
                        var base_ptr_is_direct_value = false;

                        if (base_node.data == .qualified_identifier) {
                            const pair = try cg.getQualifiedFieldPtrAndType(base_node);
                            base_ptr = pair.ptr;
                            base_type = pair.ty;
                            base_type_name = cg.getTypeNameFromLLVMType(base_type);
                        } else {
                            if (base_node.data == .function_call) {
                                base_ptr = try cg.generateExpression(base_node);
                                base_type = c.LLVMTypeOf(base_ptr);
                                const call = base_node.data.function_call;
                                base_type_name = if (cg.function_return_types.get(call.name)) |rt|
                                    rt
                                else if (cg.function_asts.get(call.name)) |fn_ast|
                                    fn_ast.return_type
                                else
                                    try cg.inferType(base_node);
                                base_ptr_is_direct_value = true;
                            } else {
                                const array_name = try cg.getBaseIdentifierName(base_node);
                                defer cg.allocator.free(array_name);
                                const var_info = llvm.CodeGenerator.getVariable(cg, array_name) orelse return errors.CodegenError.UndefinedVariable;
                                base_ptr = var_info.value;
                                base_type = var_info.type_ref;
                                base_type_name = var_info.type_name;
                            }
                        }

                        const is_pointer_base = std.mem.startsWith(u8, base_type_name, "ptr<") or (base_ptr_is_direct_value and c.LLVMGetTypeKind(base_type) == c.LLVMPointerTypeKind);
                        if (is_pointer_base) {
                            if (c.LLVMTypeOf(index_value) != c.LLVMInt64TypeInContext(cg.context)) {
                                index_value = cg.castToType(index_value, c.LLVMInt64TypeInContext(cg.context));
                            }
                            try collected_indices.append(cg.allocator, index_value);
                            const ptr_val = if (base_ptr_is_direct_value)
                                base_ptr
                            else
                                c.LLVMBuildLoad2(cg.builder, base_type, base_ptr, "load_ptr_for_addr");
                            if (!(std.mem.startsWith(u8, base_type_name, "ptr<") and std.mem.endsWith(u8, base_type_name, ">"))) {
                                return errors.CodegenError.TypeMismatch;
                            }
                            const element_type_name = base_type_name[4 .. base_type_name.len - 1];
                            const element_type = try cg.getLLVMType(element_type_name);
                            const idx = collected_indices.items[collected_indices.items.len - 1];
                            var ptr_indices = [_]c.LLVMValueRef{idx};
                            return c.LLVMBuildGEP2(cg.builder, element_type, ptr_val, &ptr_indices[0], 1, "ptr_index_addr");
                        }

                        if (std.mem.startsWith(u8, base_type_name, "[]")) {
                            index_value = cg.castToType(index_value, c.LLVMInt32TypeInContext(cg.context));
                            try collected_indices.append(cg.allocator, index_value);

                            const element_type_name = base_type_name[2..];
                            const element_type = try cg.getLLVMType(element_type_name);

                            var ptr_indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0) };
                            const ptr_in_struct = c.LLVMBuildGEP2(cg.builder, base_type, base_ptr, &ptr_indices[0], 2, "slice_ptr_in_struct_addr");
                            const arg_ptr_type = c.LLVMPointerType(element_type, 0);
                            const loaded_ptr = c.LLVMBuildLoad2(cg.builder, arg_ptr_type, ptr_in_struct, "slice_ptr_val_addr");

                            const idx = collected_indices.items[collected_indices.items.len - 1];
                            var idx_only = [_]c.LLVMValueRef{idx};
                            return c.LLVMBuildGEP2(cg.builder, element_type, loaded_ptr, &idx_only[0], 1, "slice_element_ptr_addr");
                        }

                        index_value = cg.castToType(index_value, c.LLVMInt32TypeInContext(cg.context));
                        try collected_indices.append(cg.allocator, index_value);

                        var all_indices: std.ArrayList(c.LLVMValueRef) = .empty;
                        defer all_indices.deinit(cg.allocator);
                        try all_indices.append(cg.allocator, c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0));

                        var i = collected_indices.items.len;
                        while (i > 0) {
                            i -= 1;
                            try all_indices.append(cg.allocator, collected_indices.items[i]);
                        }

                        return c.LLVMBuildGEP2(cg.builder, base_type, base_ptr, all_indices.items.ptr, @intCast(all_indices.items.len), "array_element_ptr");
                    }
                    return errors.CodegenError.TypeMismatch;
                },
                '*' => {
                    if (un.operand.data == .unary_op and un.operand.data.unary_op.op == '&') return try cg.generateExpression(un.operand);
                    if (un.operand.data == .identifier) {
                        const ident = un.operand.data.identifier;
                        if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                            const ptr_ty = var_info.type_ref;
                            const ptr_val = c.LLVMBuildLoad2(cg.builder, @ptrCast(ptr_ty), @ptrCast(var_info.value), "load_ptr_for_deref");
                            if (std.mem.startsWith(u8, var_info.type_name, "ptr<") and std.mem.endsWith(u8, var_info.type_name, ">")) {
                                const elem_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                                const elem_ty: c.LLVMTypeRef = @ptrCast(try cg.getLLVMType(elem_type_name));
                                if (c.LLVMGetTypeKind(elem_ty) == c.LLVMFunctionTypeKind) {
                                    return ptr_val;
                                }
                                return c.LLVMBuildLoad2(cg.builder, elem_ty, ptr_val, "deref");
                            }
                            const ptr_val_ty = c.LLVMTypeOf(ptr_val);
                            const elem_ty = c.LLVMGetElementType(@ptrCast(ptr_val_ty));
                            if (c.LLVMGetTypeKind(elem_ty) == c.LLVMFunctionTypeKind) {
                                return ptr_val;
                            }
                            return c.LLVMBuildLoad2(cg.builder, elem_ty, ptr_val, "deref");
                        }
                    }
                    const operand_type_name = try cg.inferType(un.operand);
                    const operand_val = try cg.generateExpression(un.operand);
                    const operand_type = c.LLVMTypeOf(operand_val);
                    if (c.LLVMGetTypeKind(operand_type) != c.LLVMPointerTypeKind) {
                        return errors.CodegenError.TypeMismatch;
                    }
                    if (std.mem.startsWith(u8, operand_type_name, "ptr<") and std.mem.endsWith(u8, operand_type_name, ">")) {
                        const elem_type_name = operand_type_name[4 .. operand_type_name.len - 1];
                        const pointee_type: c.LLVMTypeRef = @ptrCast(try cg.getLLVMType(elem_type_name));
                        if (c.LLVMGetTypeKind(pointee_type) == c.LLVMFunctionTypeKind) {
                            return operand_val;
                        }
                        return c.LLVMBuildLoad2(cg.builder, pointee_type, operand_val, "deref");
                    }
                    const pointee_type = c.LLVMGetElementType(operand_type);
                    if (c.LLVMGetTypeKind(pointee_type) == c.LLVMFunctionTypeKind) {
                        return operand_val;
                    }
                    return c.LLVMBuildLoad2(cg.builder, pointee_type, operand_val, "deref");
                },
                'D' => {
                    if (un.operand.data == .identifier) {
                        const ident = un.operand.data.identifier;
                        if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                            const var_type_name = cg.getTypeNameFromLLVMType(var_info.type_ref);

                            if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
                                const ptr_val = c.LLVMBuildLoad2(cg.builder, var_info.type_ref, var_info.value, "load_ptr_for_dec");
                                const element_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                                const element_type = try cg.getLLVMType(element_type_name);
                                const minus_one = c.LLVMConstInt(c.LLVMInt64TypeInContext(cg.context), @bitCast(@as(i64, -1)), 1);
                                var indices = [_]c.LLVMValueRef{minus_one};
                                const new_ptr = c.LLVMBuildGEP2(cg.builder, element_type, ptr_val, &indices[0], 1, "ptr_dec");
                                _ = c.LLVMBuildStore(cg.builder, new_ptr, var_info.value);
                                return new_ptr;
                            }

                            const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                            var is_allowed = false;
                            for (allowed_types) |allowed| {
                                if (std.mem.eql(u8, var_type_name, allowed)) {
                                    is_allowed = true;
                                    break;
                                }
                            }

                            if (!is_allowed) {
                                return errors.CodegenError.UnsupportedOperation;
                            }

                            const var_ptr = var_info.value;
                            const var_type = var_info.type_ref;

                            const current_val = c.LLVMBuildLoad2(cg.builder, var_type, var_ptr, "load_for_dec");

                            const new_val = switch (c.LLVMGetTypeKind(var_type)) {
                                c.LLVMIntegerTypeKind => blk: {
                                    const one = c.LLVMConstInt(var_type, 1, 0);
                                    break :blk c.LLVMBuildSub(cg.builder, current_val, one, "dec");
                                },
                                c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => blk: {
                                    const one = c.LLVMConstReal(var_type, 1.0);
                                    break :blk c.LLVMBuildFSub(cg.builder, current_val, one, "fdec");
                                },
                                else => return errors.CodegenError.UnsupportedOperation,
                            };

                            _ = c.LLVMBuildStore(cg.builder, new_val, var_ptr);

                            return new_val;
                        } else {
                            return errors.CodegenError.UndefinedVariable;
                        }
                    } else if (un.operand.data == .array_index) {
                        const arr_idx = un.operand.data.array_index;
                        const array_name = try cg.getBaseIdentifierName(arr_idx.array);
                        defer cg.allocator.free(array_name);
                        if (llvm.CodeGenerator.getVariable(cg, array_name)) |var_info| {
                            var index_value = try cg.generateExpression(arr_idx.index);
                            index_value = cg.castToType(index_value, c.LLVMInt32TypeInContext(cg.context));
                            const element_type = c.LLVMGetElementType(var_info.type_ref);

                            const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                            const element_type_name = cg.getTypeNameFromLLVMType(element_type);
                            var is_allowed = false;
                            for (allowed_types) |allowed| {
                                if (std.mem.eql(u8, element_type_name, allowed)) {
                                    is_allowed = true;
                                    break;
                                }
                            }

                            if (!is_allowed) {
                                return errors.CodegenError.UnsupportedOperation;
                            }

                            var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0), index_value };
                            const element_ptr = c.LLVMBuildGEP2(cg.builder, var_info.type_ref, var_info.value, &indices[0], 2, "array_element_ptr");

                            const current_val = c.LLVMBuildLoad2(cg.builder, element_type, element_ptr, "load_for_dec");

                            const new_val = switch (c.LLVMGetTypeKind(element_type)) {
                                c.LLVMIntegerTypeKind => blk: {
                                    const one = c.LLVMConstInt(element_type, 1, 0);
                                    break :blk c.LLVMBuildSub(cg.builder, current_val, one, "dec");
                                },
                                c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => blk: {
                                    const one = c.LLVMConstReal(element_type, 1.0);
                                    break :blk c.LLVMBuildFSub(cg.builder, current_val, one, "fdec");
                                },
                                else => return errors.CodegenError.UnsupportedOperation,
                            };

                            _ = c.LLVMBuildStore(cg.builder, new_val, element_ptr);

                            return new_val;
                        } else {
                            return errors.CodegenError.UndefinedVariable;
                        }
                    } else if (un.operand.data == .qualified_identifier) {
                        const pair = try cg.getQualifiedFieldPtrAndType(un.operand);
                        const field_ptr = pair.ptr;
                        const field_ty = pair.ty;

                        const field_kind = c.LLVMGetTypeKind(field_ty);
                        const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                        const field_type_name = cg.getTypeNameFromLLVMType(field_ty);
                        var is_allowed = false;
                        for (allowed_types) |allowed| {
                            if (std.mem.eql(u8, field_type_name, allowed)) {
                                is_allowed = true;
                                break;
                            }
                        }
                        if (!is_allowed) return errors.CodegenError.UnsupportedOperation;

                        const current_val = c.LLVMBuildLoad2(cg.builder, field_ty, field_ptr, "load_struct_field_dec");
                        const new_val = switch (field_kind) {
                            c.LLVMIntegerTypeKind => blk: {
                                const one = c.LLVMConstInt(field_ty, 1, 0);
                                break :blk c.LLVMBuildSub(cg.builder, current_val, one, "dec");
                            },
                            c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => blk: {
                                const one = c.LLVMConstReal(field_ty, 1.0);
                                break :blk c.LLVMBuildFSub(cg.builder, current_val, one, "fdec");
                            },
                            else => return errors.CodegenError.UnsupportedOperation,
                        };
                        _ = c.LLVMBuildStore(cg.builder, new_val, field_ptr);
                        return new_val;
                    } else {
                        return errors.CodegenError.TypeMismatch;
                    }
                },
                'I' => {
                    if (un.operand.data == .identifier) {
                        const ident = un.operand.data.identifier;
                        if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                            const var_type_name = cg.getTypeNameFromLLVMType(var_info.type_ref);

                            if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
                                const ptr_val = c.LLVMBuildLoad2(cg.builder, var_info.type_ref, var_info.value, "load_ptr_for_inc");
                                const element_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                                const element_type = try cg.getLLVMType(element_type_name);
                                const one = c.LLVMConstInt(c.LLVMInt64TypeInContext(cg.context), 1, 0);
                                var indices = [_]c.LLVMValueRef{one};
                                const new_ptr = c.LLVMBuildGEP2(cg.builder, element_type, ptr_val, &indices[0], 1, "ptr_inc");
                                _ = c.LLVMBuildStore(cg.builder, new_ptr, var_info.value);
                                return new_ptr;
                            }

                            const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                            var is_allowed = false;
                            for (allowed_types) |allowed| {
                                if (std.mem.eql(u8, var_type_name, allowed)) {
                                    is_allowed = true;
                                    break;
                                }
                            }
                            if (!is_allowed) {
                                return errors.CodegenError.UnsupportedOperation;
                            }
                            const var_ptr = var_info.value;
                            const var_type = var_info.type_ref;

                            const current_val = c.LLVMBuildLoad2(cg.builder, var_type, var_ptr, "load_for_inc");

                            const new_val = switch (c.LLVMGetTypeKind(var_type)) {
                                c.LLVMIntegerTypeKind => blk: {
                                    const one = c.LLVMConstInt(var_type, 1, 0);
                                    break :blk c.LLVMBuildAdd(cg.builder, current_val, one, "inc");
                                },
                                c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => blk: {
                                    const one = c.LLVMConstReal(var_type, 1.0);
                                    break :blk c.LLVMBuildFAdd(cg.builder, current_val, one, "finc");
                                },
                                else => return errors.CodegenError.UnsupportedOperation,
                            };
                            _ = c.LLVMBuildStore(cg.builder, new_val, var_ptr);
                            return new_val;
                        } else {
                            return errors.CodegenError.UndefinedVariable;
                        }
                    } else if (un.operand.data == .array_index) {
                        const arr_idx = un.operand.data.array_index;
                        const array_name = try cg.getBaseIdentifierName(arr_idx.array);
                        defer cg.allocator.free(array_name);
                        if (llvm.CodeGenerator.getVariable(cg, array_name)) |var_info| {
                            var index_value = try cg.generateExpression(arr_idx.index);
                            index_value = cg.castToType(index_value, c.LLVMInt32TypeInContext(cg.context));
                            const element_type = c.LLVMGetElementType(var_info.type_ref);

                            const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                            const element_type_name = cg.getTypeNameFromLLVMType(element_type);
                            var is_allowed = false;
                            for (allowed_types) |allowed| {
                                if (std.mem.eql(u8, element_type_name, allowed)) {
                                    is_allowed = true;
                                    break;
                                }
                            }

                            if (!is_allowed) {
                                return errors.CodegenError.UnsupportedOperation;
                            }

                            var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0), index_value };
                            const element_ptr = c.LLVMBuildGEP2(cg.builder, var_info.type_ref, var_info.value, &indices[0], 2, "array_element_ptr");

                            const current_val = c.LLVMBuildLoad2(cg.builder, element_type, element_ptr, "load_for_inc");

                            const new_val = switch (c.LLVMGetTypeKind(element_type)) {
                                c.LLVMIntegerTypeKind => blk: {
                                    const one = c.LLVMConstInt(element_type, 1, 0);
                                    break :blk c.LLVMBuildAdd(cg.builder, current_val, one, "inc");
                                },
                                c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => blk: {
                                    const one = c.LLVMConstReal(element_type, 1.0);
                                    break :blk c.LLVMBuildFAdd(cg.builder, current_val, one, "finc");
                                },
                                else => return errors.CodegenError.UnsupportedOperation,
                            };

                            _ = c.LLVMBuildStore(cg.builder, new_val, element_ptr);

                            return new_val;
                        } else {
                            return errors.CodegenError.UndefinedVariable;
                        }
                    } else if (un.operand.data == .qualified_identifier) {
                        const pair = try cg.getQualifiedFieldPtrAndType(un.operand);
                        const field_ptr = pair.ptr;
                        const field_ty = pair.ty;

                        const field_kind = c.LLVMGetTypeKind(field_ty);
                        const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                        const field_type_name = cg.getTypeNameFromLLVMType(field_ty);
                        var is_allowed = false;
                        for (allowed_types) |allowed| {
                            if (std.mem.eql(u8, field_type_name, allowed)) {
                                is_allowed = true;
                                break;
                            }
                        }
                        if (!is_allowed) return errors.CodegenError.UnsupportedOperation;
                        const current_val = c.LLVMBuildLoad2(cg.builder, field_ty, field_ptr, "load_struct_field_inc");
                        const new_val = switch (field_kind) {
                            c.LLVMIntegerTypeKind => blk: {
                                const one = c.LLVMConstInt(field_ty, 1, 0);
                                break :blk c.LLVMBuildAdd(cg.builder, current_val, one, "inc");
                            },
                            c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => blk: {
                                const one = c.LLVMConstReal(field_ty, 1.0);
                                break :blk c.LLVMBuildFAdd(cg.builder, current_val, one, "finc");
                            },
                            else => return errors.CodegenError.UnsupportedOperation,
                        };
                        _ = c.LLVMBuildStore(cg.builder, new_val, field_ptr);
                        return new_val;
                    } else {
                        return errors.CodegenError.TypeMismatch;
                    }
                },
                else => {
                    return errors.CodegenError.UnsupportedOperation;
                },
            }
        },
        .comparison => |comp| {
            return generateComparison(cg, comp);
        },
        .binary_op => |b| {
            return generateBinaryOp(cg, b);
        },
        .method_call => |method| {
            var args: std.ArrayList(*ast.Node) = .empty;
            defer args.deinit(cg.allocator);
            try args.append(cg.allocator, method.object);
            for (method.args.items) |arg| {
                try args.append(cg.allocator, arg);
            }
            const function_call = ast.FunctionCall{
                .name = method.method_name,
                .is_libc = false,
                .args = args,
            };
            return try llvm.CodeGenerator.generateFunctionCall(cg, function_call, null);
        },
        .function_call => |call| {
            return try llvm.CodeGenerator.generateFunctionCall(cg, call, null);
        },
        .array_index => |arr_idx| {
            return try array.generateArrayIndexExpression(cg, arr_idx);
        },
        .struct_initializer => |struct_init| {
            return try cg.generateStructInitializer(struct_init);
        },
        .simd_method_call => |simd_method| {
            const simd_value = try cg.generateExpression(simd_method.simd);
            const simd_type = c.LLVMTypeOf(simd_value);

            if (c.LLVMGetTypeKind(simd_type) != c.LLVMVectorTypeKind) {
                return errors.CodegenError.TypeMismatch;
            }

            if (std.mem.eql(u8, simd_method.method_name, "sum") or
                std.mem.eql(u8, simd_method.method_name, "product") or
                std.mem.eql(u8, simd_method.method_name, "min") or
                std.mem.eql(u8, simd_method.method_name, "max"))
            {
                return try simd.generateSimdReduction(cg, simd_method.method_name, simd_value);
            }

            return errors.CodegenError.UnsupportedOperation;
        },
        .array_initializer => {
            return errors.CodegenError.TypeMismatch;
        },
        .simd_initializer => {
            return errors.CodegenError.TypeMismatch;
        },
        else => return errors.CodegenError.TypeMismatch,
    }
}

fn generateComparison(cg: *llvm.CodeGenerator, comparison: ast.Comparison) errors.CodegenError!c.LLVMValueRef {
    const lhs_value = try cg.generateExpression(comparison.lhs);
    const rhs_value = try cg.generateExpression(comparison.rhs);
    const lhs_type = c.LLVMTypeOf(lhs_value);
    const rhs_type = c.LLVMTypeOf(rhs_value);
    const lhs_kind = c.LLVMGetTypeKind(lhs_type);
    const rhs_kind = c.LLVMGetTypeKind(rhs_type);
    const is_lhs_pointer = lhs_kind == c.LLVMPointerTypeKind;
    const is_rhs_pointer = rhs_kind == c.LLVMPointerTypeKind;
    if (is_lhs_pointer and is_rhs_pointer) {
        if (comparison.op != '=' and comparison.op != '!') {
            return errors.CodegenError.TypeMismatch;
        }
        return switch (comparison.op) {
            '=' => c.LLVMBuildICmp(cg.builder, c.LLVMIntEQ, lhs_value, rhs_value, "ptr_cmp_eq"),
            '!' => c.LLVMBuildICmp(cg.builder, c.LLVMIntNE, lhs_value, rhs_value, "ptr_cmp_ne"),
            else => return errors.CodegenError.UnsupportedOperation,
        };
    }
    if (is_lhs_pointer or is_rhs_pointer) {
        if (comparison.op != '=' and comparison.op != '!') {
            return errors.CodegenError.TypeMismatch;
        }
        var casted_lhs = lhs_value;
        var casted_rhs = rhs_value;
        if (is_lhs_pointer and !is_rhs_pointer) {
            if (rhs_kind == c.LLVMIntegerTypeKind) {
                const rhs_const_int = c.LLVMIsAConstantInt(rhs_value);
                if (rhs_const_int != null and c.LLVMConstIntGetSExtValue(rhs_value) == 0) {
                    casted_rhs = c.LLVMConstNull(lhs_type);
                } else {
                    casted_rhs = c.LLVMBuildIntToPtr(cg.builder, rhs_value, lhs_type, "int_to_ptr");
                }
            } else {
                return errors.CodegenError.TypeMismatch;
            }
        } else if (!is_lhs_pointer and is_rhs_pointer) {
            if (lhs_kind == c.LLVMIntegerTypeKind) {
                const lhs_const_int = c.LLVMIsAConstantInt(lhs_value);
                if (lhs_const_int != null and c.LLVMConstIntGetSExtValue(lhs_value) == 0) {
                    casted_lhs = c.LLVMConstNull(rhs_type);
                } else {
                    casted_lhs = c.LLVMBuildIntToPtr(cg.builder, lhs_value, rhs_type, "int_to_ptr");
                }
            } else {
                return errors.CodegenError.TypeMismatch;
            }
        }
        return switch (comparison.op) {
            '=' => c.LLVMBuildICmp(cg.builder, c.LLVMIntEQ, casted_lhs, casted_rhs, "ptr_null_cmp_eq"),
            '!' => c.LLVMBuildICmp(cg.builder, c.LLVMIntNE, casted_lhs, casted_rhs, "ptr_null_cmp_ne"),
            else => return errors.CodegenError.UnsupportedOperation,
        };
    }
    const is_lhs_struct = lhs_kind == c.LLVMStructTypeKind;
    const is_rhs_struct = rhs_kind == c.LLVMStructTypeKind;
    if (is_lhs_struct or is_rhs_struct) return errors.CodegenError.TypeMismatch;

    const is_lhs_bool = lhs_kind == c.LLVMIntegerTypeKind and c.LLVMGetIntTypeWidth(lhs_type) == 1;
    const is_rhs_bool = rhs_kind == c.LLVMIntegerTypeKind and c.LLVMGetIntTypeWidth(rhs_type) == 1;
    const is_lhs_float = lhs_kind == c.LLVMFloatTypeKind or
        lhs_kind == c.LLVMDoubleTypeKind or
        lhs_kind == c.LLVMHalfTypeKind;
    const is_rhs_float = rhs_kind == c.LLVMFloatTypeKind or
        rhs_kind == c.LLVMDoubleTypeKind or
        rhs_kind == c.LLVMHalfTypeKind;
    if (is_lhs_bool and is_rhs_bool) {
        if (comparison.op != '=' and comparison.op != '!') {
            return errors.CodegenError.TypeMismatch;
        }

        return switch (comparison.op) {
            '=' => c.LLVMBuildICmp(cg.builder, c.LLVMIntEQ, lhs_value, rhs_value, "icmp_eq"),
            '!' => c.LLVMBuildICmp(cg.builder, c.LLVMIntNE, lhs_value, rhs_value, "icmp_ne"),
            else => return errors.CodegenError.UnsupportedOperation,
        };
    }
    var converted_lhs = lhs_value;
    var converted_rhs = rhs_value;
    if (is_lhs_bool and !is_rhs_bool) {
        if (is_rhs_float) {
            converted_lhs = c.LLVMBuildUIToFP(cg.builder, lhs_value, rhs_type, "bool_to_fp");
        } else {
            converted_lhs = c.LLVMBuildZExt(cg.builder, lhs_value, rhs_type, "bool_to_int");
        }
    } else if (is_rhs_bool and !is_lhs_bool) {
        if (is_lhs_float) {
            converted_rhs = c.LLVMBuildUIToFP(cg.builder, rhs_value, lhs_type, "bool_to_fp");
        } else {
            converted_rhs = c.LLVMBuildZExt(cg.builder, rhs_value, lhs_type, "bool_to_int");
        }
    }
    const final_lhs_type = c.LLVMTypeOf(converted_lhs);
    const final_rhs_type = c.LLVMTypeOf(converted_rhs);
    const final_lhs_kind = c.LLVMGetTypeKind(final_lhs_type);
    const final_rhs_kind = c.LLVMGetTypeKind(final_rhs_type);
    const final_is_lhs_float = final_lhs_kind == c.LLVMFloatTypeKind or
        final_lhs_kind == c.LLVMDoubleTypeKind or
        final_lhs_kind == c.LLVMHalfTypeKind;
    const final_is_rhs_float = final_rhs_kind == c.LLVMFloatTypeKind or
        final_rhs_kind == c.LLVMDoubleTypeKind or
        final_rhs_kind == c.LLVMHalfTypeKind;
    const is_float_comp = final_is_lhs_float or final_is_rhs_float;
    var result_type: c.LLVMTypeRef = undefined;

    if (is_float_comp) {
        if (final_lhs_kind == c.LLVMDoubleTypeKind or final_rhs_kind == c.LLVMDoubleTypeKind) {
            result_type = c.LLVMDoubleTypeInContext(cg.context);
        } else if (final_lhs_kind == c.LLVMFloatTypeKind or final_rhs_kind == c.LLVMFloatTypeKind) {
            result_type = c.LLVMFloatTypeInContext(cg.context);
        } else {
            result_type = c.LLVMFloatTypeInContext(cg.context);
        }
    } else {
        const lhs_width = c.LLVMGetIntTypeWidth(final_lhs_type);
        const rhs_width = c.LLVMGetIntTypeWidth(final_rhs_type);
        if (lhs_width >= rhs_width) {
            result_type = final_lhs_type;
        } else {
            result_type = final_rhs_type;
        }
    }
    const casted_lhs = cg.castToType(converted_lhs, result_type);
    const casted_rhs = cg.castToType(converted_rhs, result_type);
    return switch (comparison.op) {
        '=' => {
            if (is_float_comp) {
                return c.LLVMBuildFCmp(cg.builder, c.LLVMRealOEQ, casted_lhs, casted_rhs, "fcmp_eq");
            } else {
                return c.LLVMBuildICmp(cg.builder, c.LLVMIntEQ, casted_lhs, casted_rhs, "icmp_eq");
            }
        },
        '!' => {
            if (is_float_comp) {
                return c.LLVMBuildFCmp(cg.builder, c.LLVMRealONE, casted_lhs, casted_rhs, "fcmp_ne");
            } else {
                return c.LLVMBuildICmp(cg.builder, c.LLVMIntNE, casted_lhs, casted_rhs, "icmp_ne");
            }
        },
        '<' => {
            if (is_float_comp) {
                return c.LLVMBuildFCmp(cg.builder, c.LLVMRealOLT, casted_lhs, casted_rhs, "fcmp_lt");
            } else {
                const is_unsigned = utils.isUnsignedType(cg.getTypeNameFromLLVMType(result_type));
                return c.LLVMBuildICmp(cg.builder, if (is_unsigned) c.LLVMIntULT else c.LLVMIntSLT, casted_lhs, casted_rhs, "icmp_lt");
            }
        },
        '>' => {
            if (is_float_comp) {
                return c.LLVMBuildFCmp(cg.builder, c.LLVMRealOGT, casted_lhs, casted_rhs, "fcmp_gt");
            } else {
                const is_unsigned = utils.isUnsignedType(cg.getTypeNameFromLLVMType(result_type));
                return c.LLVMBuildICmp(cg.builder, if (is_unsigned) c.LLVMIntUGT else c.LLVMIntSGT, casted_lhs, casted_rhs, "icmp_gt");
            }
        },
        'L' => {
            if (is_float_comp) {
                return c.LLVMBuildFCmp(cg.builder, c.LLVMRealOLE, casted_lhs, casted_rhs, "fcmp_le");
            } else {
                const is_unsigned = utils.isUnsignedType(cg.getTypeNameFromLLVMType(result_type));
                return c.LLVMBuildICmp(cg.builder, if (is_unsigned) c.LLVMIntULE else c.LLVMIntSLE, casted_lhs, casted_rhs, "icmp_le");
            }
        },
        'G' => {
            if (is_float_comp) {
                return c.LLVMBuildFCmp(cg.builder, c.LLVMRealOGE, casted_lhs, casted_rhs, "fcmp_ge");
            } else {
                const is_unsigned = utils.isUnsignedType(cg.getTypeNameFromLLVMType(result_type));
                return c.LLVMBuildICmp(cg.builder, if (is_unsigned) c.LLVMIntUGE else c.LLVMIntSGE, casted_lhs, casted_rhs, "icmp_ge");
            }
        },
        else => return errors.CodegenError.UnsupportedOperation,
    };
}

fn getPointerElementTypeFromExpr(cg: *llvm.CodeGenerator, expr: *ast.Node, ptr_type: c.LLVMTypeRef) errors.CodegenError!c.LLVMTypeRef {
    _ = ptr_type;
    const inferred = try cg.inferType(expr);
    if (std.mem.startsWith(u8, inferred, "ptr<") and std.mem.endsWith(u8, inferred, ">")) {
        return try cg.getLLVMType(inferred[4 .. inferred.len - 1]);
    }
    if (expr.data == .identifier) {
        const name = expr.data.identifier.name;
        if (llvm.CodeGenerator.getVariable(cg, name)) |var_info| {
            if (std.mem.startsWith(u8, var_info.type_name, "ptr<") and std.mem.endsWith(u8, var_info.type_name, ">")) {
                return try cg.getLLVMType(var_info.type_name[4 .. var_info.type_name.len - 1]);
            }
        }
    }
    return c.LLVMInt8TypeInContext(cg.context);
}

fn generateBinaryOp(cg: *llvm.CodeGenerator, bin_op: ast.BinaryOp) errors.CodegenError!c.LLVMValueRef {
    if (bin_op.op == '&') return generateLogicalAnd(cg, bin_op.lhs, bin_op.rhs);
    if (bin_op.op == '|') return generateLogicalOr(cg, bin_op.lhs, bin_op.rhs);

    const lhs_value = try cg.generateExpression(bin_op.lhs);
    const lhs_type = c.LLVMTypeOf(lhs_value);
    const lhs_kind = c.LLVMGetTypeKind(lhs_type);
    if (c.LLVMGetTypeKind(lhs_type) == c.LLVMVectorTypeKind) {
        const rhs_value = try cg.generateExpression(bin_op.rhs);
        const rhs_type = c.LLVMTypeOf(rhs_value);
        if (c.LLVMGetTypeKind(rhs_type) == c.LLVMVectorTypeKind) {
            return try simd.generateSimdBinaryOp(cg, bin_op.op, lhs_value, rhs_value);
        }
    }

    if (lhs_kind == c.LLVMPointerTypeKind and (bin_op.op == '+' or bin_op.op == '-')) {
        const rhs_value = try cg.generateExpression(bin_op.rhs);
        const rhs_type = c.LLVMTypeOf(rhs_value);
        const rhs_kind = c.LLVMGetTypeKind(rhs_type);

        if (rhs_kind == c.LLVMIntegerTypeKind) {
            const element_type = try getPointerElementTypeFromExpr(cg, bin_op.lhs, lhs_type);
            var idx_val = rhs_value;
            if (c.LLVMTypeOf(rhs_value) != c.LLVMInt64TypeInContext(cg.context)) {
                idx_val = cg.castToType(rhs_value, c.LLVMInt64TypeInContext(cg.context));
            }
            const index = if (bin_op.op == '+') idx_val else c.LLVMBuildNeg(cg.builder, idx_val, "neg_offset");
            var indices = [_]c.LLVMValueRef{index};
            return c.LLVMBuildGEP2(cg.builder, element_type, lhs_value, &indices[0], 1, "ptr_arith");
        } else if (rhs_kind == c.LLVMPointerTypeKind and bin_op.op == '-') {
            const element_type = try getPointerElementTypeFromExpr(cg, bin_op.lhs, lhs_type);
            const lhs_int = c.LLVMBuildPtrToInt(cg.builder, lhs_value, c.LLVMInt64TypeInContext(cg.context), "ptr_to_int_lhs");
            const rhs_int = c.LLVMBuildPtrToInt(cg.builder, rhs_value, c.LLVMInt64TypeInContext(cg.context), "ptr_to_int_rhs");
            const byte_diff = c.LLVMBuildSub(cg.builder, lhs_int, rhs_int, "byte_diff");
            const element_size = c.LLVMABISizeOfType(c.LLVMGetModuleDataLayout(cg.module), element_type);
            const size_val = c.LLVMConstInt(c.LLVMInt64TypeInContext(cg.context), element_size, 0);
            return c.LLVMBuildSDiv(cg.builder, byte_diff, size_val, "ptr_diff");
        }
        return errors.CodegenError.TypeMismatch;
    }

    const lhs_type_name_inferred = try cg.inferType(bin_op.lhs);
    const has_concrete_lhs_context = !std.mem.eql(u8, lhs_type_name_inferred, "void") and
        !std.mem.eql(u8, lhs_type_name_inferred, "ptr") and
        !std.mem.eql(u8, lhs_type_name_inferred, "unknown");
    const rhs_value = if (has_concrete_lhs_context)
        try cg.generateExpressionWithContext(bin_op.rhs, lhs_type_name_inferred)
    else
        try cg.generateExpression(bin_op.rhs);
    const rhs_type_name_inferred = try cg.inferType(bin_op.rhs);

    const rhs_type = c.LLVMTypeOf(rhs_value);
    const rhs_kind = c.LLVMGetTypeKind(rhs_type);

    if (lhs_kind == c.LLVMIntegerTypeKind and rhs_kind == c.LLVMPointerTypeKind and bin_op.op == '+') {
        const element_type = try getPointerElementTypeFromExpr(cg, bin_op.rhs, rhs_type);
        if (bin_op.rhs.data == .identifier) {
            const var_info = llvm.CodeGenerator.getVariable(cg, bin_op.rhs.data.identifier.name) orelse return errors.CodegenError.UndefinedVariable;
            if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
                var idx_val = lhs_value;
                if (c.LLVMTypeOf(lhs_value) != c.LLVMInt64TypeInContext(cg.context)) {
                    idx_val = cg.castToType(lhs_value, c.LLVMInt64TypeInContext(cg.context));
                }
                var indices = [_]c.LLVMValueRef{idx_val};
                return c.LLVMBuildGEP2(cg.builder, element_type, rhs_value, &indices[0], 1, "ptr_arith");
            }
            var idx_val = lhs_value;
            if (c.LLVMTypeOf(lhs_value) != c.LLVMInt64TypeInContext(cg.context)) {
                idx_val = cg.castToType(lhs_value, c.LLVMInt64TypeInContext(cg.context));
            }
            var indices = [_]c.LLVMValueRef{idx_val};
            return c.LLVMBuildGEP2(cg.builder, element_type, rhs_value, &indices[0], 1, "ptr_arith");
        }
        var idx_val = lhs_value;
        if (c.LLVMTypeOf(lhs_value) != c.LLVMInt64TypeInContext(cg.context)) {
            idx_val = cg.castToType(lhs_value, c.LLVMInt64TypeInContext(cg.context));
        }
        var indices = [_]c.LLVMValueRef{idx_val};
        return c.LLVMBuildGEP2(cg.builder, element_type, rhs_value, &indices[0], 1, "ptr_arith");
    }

    if (lhs_kind == c.LLVMPointerTypeKind or rhs_kind == c.LLVMPointerTypeKind) {
        return errors.CodegenError.TypeMismatch;
    }
    if (bin_op.op != '&' and bin_op.op != '|') {
        if (lhs_kind == c.LLVMIntegerTypeKind and c.LLVMGetIntTypeWidth(lhs_type) == 1) {
            return errors.CodegenError.TypeMismatch;
        }
        if (rhs_kind == c.LLVMIntegerTypeKind and c.LLVMGetIntTypeWidth(rhs_type) == 1) {
            return errors.CodegenError.TypeMismatch;
        }
    }

    const is_lhs_float = lhs_kind == c.LLVMFloatTypeKind or
        lhs_kind == c.LLVMDoubleTypeKind or
        lhs_kind == c.LLVMHalfTypeKind;
    const is_rhs_float = rhs_kind == c.LLVMFloatTypeKind or
        rhs_kind == c.LLVMDoubleTypeKind or
        rhs_kind == c.LLVMHalfTypeKind;
    const inferred_float_op = is_lhs_float or is_rhs_float;
    const result_type = if (inferred_float_op) blk: {
        if (lhs_kind == c.LLVMDoubleTypeKind or rhs_kind == c.LLVMDoubleTypeKind) {
            break :blk c.LLVMDoubleTypeInContext(cg.context);
        } else if (lhs_kind == c.LLVMFloatTypeKind or rhs_kind == c.LLVMFloatTypeKind) {
            break :blk c.LLVMFloatTypeInContext(cg.context);
        } else {
            break :blk c.LLVMFloatTypeInContext(cg.context);
        }
    } else blk: {
        const lhs_width = if (lhs_kind == c.LLVMIntegerTypeKind) c.LLVMGetIntTypeWidth(lhs_type) else 0;
        const rhs_width = if (rhs_kind == c.LLVMIntegerTypeKind) c.LLVMGetIntTypeWidth(rhs_type) else 0;
        break :blk if (lhs_width >= rhs_width) lhs_type else rhs_type;
    };

    const result_type_name_inferred = mergeTypes(cg, lhs_type_name_inferred, rhs_type_name_inferred);

    var casted_lhs = lhs_value;
    var casted_rhs = rhs_value;

    if (inferred_float_op) {
        if (lhs_kind == c.LLVMIntegerTypeKind) {
            if (utils.isUnsignedType(lhs_type_name_inferred))
                casted_lhs = c.LLVMBuildUIToFP(cg.builder, lhs_value, result_type, "uitofp_lhs")
            else
                casted_lhs = c.LLVMBuildSIToFP(cg.builder, lhs_value, result_type, "sitofp_lhs");
        } else if (lhs_kind == c.LLVMHalfTypeKind and result_type != lhs_type) {
            casted_lhs = c.LLVMBuildFPExt(cg.builder, lhs_value, result_type, "fpext_lhs");
        } else if (lhs_kind == c.LLVMFloatTypeKind and c.LLVMGetTypeKind(result_type) == c.LLVMDoubleTypeKind) {
            casted_lhs = c.LLVMBuildFPExt(cg.builder, lhs_value, result_type, "fpext_lhs");
        }
        if (rhs_kind == c.LLVMIntegerTypeKind) {
            if (utils.isUnsignedType(rhs_type_name_inferred))
                casted_rhs = c.LLVMBuildUIToFP(cg.builder, rhs_value, result_type, "uitofp_rhs")
            else
                casted_rhs = c.LLVMBuildSIToFP(cg.builder, rhs_value, result_type, "sitofp_rhs");
        } else if (rhs_kind == c.LLVMHalfTypeKind and result_type != rhs_type) {
            casted_rhs = c.LLVMBuildFPExt(cg.builder, rhs_value, result_type, "fpext_rhs");
        } else if (rhs_kind == c.LLVMFloatTypeKind and c.LLVMGetTypeKind(result_type) == c.LLVMDoubleTypeKind) {
            casted_rhs = c.LLVMBuildFPExt(cg.builder, rhs_value, result_type, "fpext_rhs");
        }
        const lhs_ty_now = c.LLVMTypeOf(casted_lhs);
        if (lhs_ty_now != result_type) {
            const lhs_kind_now = c.LLVMGetTypeKind(lhs_ty_now);
            if (lhs_kind_now == c.LLVMIntegerTypeKind) {
                if (utils.isUnsignedType(lhs_type_name_inferred))
                    casted_lhs = c.LLVMBuildUIToFP(cg.builder, casted_lhs, result_type, "uitofp_lhs2")
                else
                    casted_lhs = c.LLVMBuildSIToFP(cg.builder, casted_lhs, result_type, "sitofp_lhs2");
            } else if ((lhs_kind_now == c.LLVMHalfTypeKind or lhs_kind_now == c.LLVMFloatTypeKind) and c.LLVMGetTypeKind(result_type) == c.LLVMDoubleTypeKind) {
                casted_lhs = c.LLVMBuildFPExt(cg.builder, casted_lhs, result_type, "fpext_lhs2");
            } else if (lhs_kind_now == c.LLVMDoubleTypeKind and c.LLVMGetTypeKind(result_type) == c.LLVMFloatTypeKind) {
                casted_lhs = c.LLVMBuildFPTrunc(cg.builder, casted_lhs, result_type, "fptrunc_lhs");
            }
        }
        const rhs_ty_now = c.LLVMTypeOf(casted_rhs);
        if (rhs_ty_now != result_type) {
            const rhs_kind_now = c.LLVMGetTypeKind(rhs_ty_now);
            if (rhs_kind_now == c.LLVMIntegerTypeKind) {
                if (utils.isUnsignedType(rhs_type_name_inferred))
                    casted_rhs = c.LLVMBuildUIToFP(cg.builder, casted_rhs, result_type, "uitofp_rhs2")
                else
                    casted_rhs = c.LLVMBuildSIToFP(cg.builder, casted_rhs, result_type, "sitofp_rhs2");
            } else if ((rhs_kind_now == c.LLVMHalfTypeKind or rhs_kind_now == c.LLVMFloatTypeKind) and c.LLVMGetTypeKind(result_type) == c.LLVMDoubleTypeKind) {
                casted_rhs = c.LLVMBuildFPExt(cg.builder, casted_rhs, result_type, "fpext_rhs2");
            } else if (rhs_kind_now == c.LLVMDoubleTypeKind and c.LLVMGetTypeKind(result_type) == c.LLVMFloatTypeKind) {
                casted_rhs = c.LLVMBuildFPTrunc(cg.builder, casted_rhs, result_type, "fptrunc_rhs");
            }
        }
    } else if (bin_op.op != '&' and bin_op.op != '|') {
        casted_lhs = cg.castToType(lhs_value, result_type);
        casted_rhs = cg.castToType(rhs_value, result_type);
    }

    const final_lhs_kind = c.LLVMGetTypeKind(c.LLVMTypeOf(casted_lhs));
    const final_rhs_kind = c.LLVMGetTypeKind(c.LLVMTypeOf(casted_rhs));
    const final_is_lhs_float = final_lhs_kind == c.LLVMFloatTypeKind or
        final_lhs_kind == c.LLVMDoubleTypeKind or
        final_lhs_kind == c.LLVMHalfTypeKind;
    const final_is_rhs_float = final_rhs_kind == c.LLVMFloatTypeKind or
        final_rhs_kind == c.LLVMDoubleTypeKind or
        final_rhs_kind == c.LLVMHalfTypeKind;
    const use_float_op = final_is_lhs_float or final_is_rhs_float;

    return switch (bin_op.op) {
        '+' => if (use_float_op)
            c.LLVMBuildFAdd(cg.builder, casted_lhs, casted_rhs, "fadd")
        else
            c.LLVMBuildAdd(cg.builder, casted_lhs, casted_rhs, "add"),
        '-' => if (use_float_op)
            c.LLVMBuildFSub(cg.builder, casted_lhs, casted_rhs, "fsub")
        else
            c.LLVMBuildSub(cg.builder, casted_lhs, casted_rhs, "sub"),
        '*' => if (use_float_op)
            c.LLVMBuildFMul(cg.builder, casted_lhs, casted_rhs, "fmul")
        else
            c.LLVMBuildMul(cg.builder, casted_lhs, casted_rhs, "mul"),
        '/' => if (use_float_op)
            c.LLVMBuildFDiv(cg.builder, casted_lhs, casted_rhs, "fdiv")
        else if (utils.isUnsignedType(result_type_name_inferred))
            c.LLVMBuildUDiv(cg.builder, casted_lhs, casted_rhs, "udiv")
        else
            c.LLVMBuildSDiv(cg.builder, casted_lhs, casted_rhs, "sdiv"),
        '%' => {
            if (use_float_op) {
                cg.uses_float_modulo = true;
                return c.LLVMBuildFRem(cg.builder, casted_lhs, casted_rhs, "frem");
            } else {
                const result_type_kind = c.LLVMGetTypeKind(result_type);
                if (result_type_kind != c.LLVMIntegerTypeKind) {
                    return errors.CodegenError.UnsupportedOperation;
                }
                const width = c.LLVMGetIntTypeWidth(result_type);
                if (width != 8 and width != 16 and width != 32 and width != 64) {
                    return errors.CodegenError.UnsupportedOperation;
                }
                if (utils.isUnsignedType(result_type_name_inferred))
                    return c.LLVMBuildURem(cg.builder, casted_lhs, casted_rhs, "urem")
                else
                    return c.LLVMBuildSRem(cg.builder, casted_lhs, casted_rhs, "srem");
            }
        },
        '&' => generateLogicalAnd(cg, bin_op.lhs, bin_op.rhs),
        '|' => generateLogicalOr(cg, bin_op.lhs, bin_op.rhs),
        'A' => {
            if (use_float_op) return errors.CodegenError.UnsupportedOperation;
            return c.LLVMBuildAnd(cg.builder, casted_lhs, casted_rhs, "and");
        },
        '$' => {
            if (use_float_op) return errors.CodegenError.UnsupportedOperation;
            return c.LLVMBuildOr(cg.builder, casted_lhs, casted_rhs, "or");
        },
        '^' => {
            if (use_float_op) return errors.CodegenError.UnsupportedOperation;
            return c.LLVMBuildXor(cg.builder, casted_lhs, casted_rhs, "xor");
        },
        '<' => {
            if (use_float_op) return errors.CodegenError.UnsupportedOperation;
            return c.LLVMBuildShl(cg.builder, casted_lhs, casted_rhs, "shl");
        },
        '>' => {
            if (use_float_op) return errors.CodegenError.UnsupportedOperation;
            if (utils.isUnsignedType(result_type_name_inferred))
                return c.LLVMBuildLShr(cg.builder, casted_lhs, casted_rhs, "lshr")
            else
                return c.LLVMBuildAShr(cg.builder, casted_lhs, casted_rhs, "ashr");
        },
        else => {
            return errors.CodegenError.UnsupportedOperation;
        },
    };
}

fn generateLogicalAnd(cg: *llvm.CodeGenerator, lhs_expr: *ast.Node, rhs_expr: *ast.Node) errors.CodegenError!c.LLVMValueRef {
    const current_function = cg.current_function orelse return errors.CodegenError.TypeMismatch;
    const rhs_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "and_rhs");
    const merge_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "and_merge");
    const lhs_value = try cg.generateExpression(lhs_expr);
    const lhs_bool = cg.convertToBool(lhs_value);
    const lhs_bb = c.LLVMGetInsertBlock(cg.builder);
    _ = c.LLVMBuildCondBr(cg.builder, lhs_bool, rhs_bb, merge_bb);
    c.LLVMPositionBuilderAtEnd(cg.builder, rhs_bb);
    const rhs_value = try cg.generateExpression(rhs_expr);
    const rhs_bool = cg.convertToBool(rhs_value);
    _ = c.LLVMBuildBr(cg.builder, merge_bb);
    const rhs_end_bb = c.LLVMGetInsertBlock(cg.builder);
    c.LLVMPositionBuilderAtEnd(cg.builder, merge_bb);
    const bool_type = c.LLVMInt1TypeInContext(cg.context);
    const phi = c.LLVMBuildPhi(cg.builder, bool_type, "and_result");
    const false_val = c.LLVMConstInt(bool_type, 0, 0);
    var phi_vals = [_]c.LLVMValueRef{ false_val, rhs_bool };
    var phi_blocks = [_]c.LLVMBasicBlockRef{ lhs_bb, rhs_end_bb };
    c.LLVMAddIncoming(phi, &phi_vals[0], &phi_blocks[0], 2);
    return phi;
}

fn generateLogicalOr(cg: *llvm.CodeGenerator, lhs_expr: *ast.Node, rhs_expr: *ast.Node) errors.CodegenError!c.LLVMValueRef {
    const current_function = cg.current_function orelse return errors.CodegenError.TypeMismatch;
    const rhs_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "or_rhs");
    const merge_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "or_merge");
    const lhs_value = try cg.generateExpression(lhs_expr);
    const lhs_bool = cg.convertToBool(lhs_value);
    const lhs_bb = c.LLVMGetInsertBlock(cg.builder);
    _ = c.LLVMBuildCondBr(cg.builder, lhs_bool, merge_bb, rhs_bb);
    c.LLVMPositionBuilderAtEnd(cg.builder, rhs_bb);
    const rhs_value = try cg.generateExpression(rhs_expr);
    const rhs_bool = cg.convertToBool(rhs_value);
    _ = c.LLVMBuildBr(cg.builder, merge_bb);
    const rhs_end_bb = c.LLVMGetInsertBlock(cg.builder);
    c.LLVMPositionBuilderAtEnd(cg.builder, merge_bb);
    const bool_type = c.LLVMInt1TypeInContext(cg.context);
    const phi = c.LLVMBuildPhi(cg.builder, bool_type, "or_result");
    const true_val = c.LLVMConstInt(bool_type, 1, 0);
    var phi_vals = [_]c.LLVMValueRef{ true_val, rhs_bool };
    var phi_blocks = [_]c.LLVMBasicBlockRef{ lhs_bb, rhs_end_bb };
    c.LLVMAddIncoming(phi, &phi_vals[0], &phi_blocks[0], 2);
    return phi;
}
