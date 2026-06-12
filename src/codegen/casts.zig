const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
const llvm = @import("llvm.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

pub fn castToType(cg: *llvm.CodeGenerator, value: c.LLVMValueRef, target_type: c.LLVMTypeRef) c.LLVMValueRef {
    return cg.castToTypeWithSourceInfo(value, target_type, null);
}

pub fn castToTypeWithSourceInfo(cg: *llvm.CodeGenerator, value: c.LLVMValueRef, target_type: c.LLVMTypeRef, source_type_name: ?[]const u8) c.LLVMValueRef {
    const value_type = c.LLVMTypeOf(value);

    if (value_type == target_type) {
        return value;
    }

    const value_kind = c.LLVMGetTypeKind(value_type);
    const target_kind = c.LLVMGetTypeKind(target_type);

    if (value_kind == c.LLVMIntegerTypeKind and target_kind == c.LLVMIntegerTypeKind) {
        const value_width = c.LLVMGetIntTypeWidth(value_type);
        const target_width = c.LLVMGetIntTypeWidth(target_type);

        if (value_width > target_width) {
            return c.LLVMBuildTrunc(cg.builder, value, target_type, "trunc");
        } else if (value_width < target_width) {
            if (value_width == 1) {
                return c.LLVMBuildZExt(cg.builder, value, target_type, "zext");
            } else {
                if (source_type_name) |src_name| {
                    if (utils.isUnsignedType(src_name)) {
                        return c.LLVMBuildZExt(cg.builder, value, target_type, "zext");
                    } else {
                        return c.LLVMBuildSExt(cg.builder, value, target_type, "sext");
                    }
                } else {
                    const target_type_name = cg.getTypeNameFromLLVMType(@ptrCast(target_type));
                    if (utils.isUnsignedType(target_type_name)) {
                        return c.LLVMBuildZExt(cg.builder, value, target_type, "zext");
                    } else {
                        return c.LLVMBuildSExt(cg.builder, value, target_type, "sext");
                    }
                }
            }
        }
    } else if ((value_kind == c.LLVMFloatTypeKind and (target_kind == c.LLVMDoubleTypeKind or target_kind == c.LLVMHalfTypeKind)) or
        (value_kind == c.LLVMDoubleTypeKind and (target_kind == c.LLVMFloatTypeKind or target_kind == c.LLVMHalfTypeKind)) or
        (value_kind == c.LLVMHalfTypeKind and (target_kind == c.LLVMFloatTypeKind or target_kind == c.LLVMDoubleTypeKind)))
    {
        if ((value_kind == c.LLVMFloatTypeKind or value_kind == c.LLVMHalfTypeKind) and
            (target_kind == c.LLVMDoubleTypeKind or (target_kind == c.LLVMFloatTypeKind and value_kind == c.LLVMHalfTypeKind)))
        {
            return c.LLVMBuildFPExt(cg.builder, value, target_type, "fpext");
        } else {
            return c.LLVMBuildFPTrunc(cg.builder, value, target_type, "fptrunc");
        }
    } else if ((value_kind == c.LLVMIntegerTypeKind and
        (target_kind == c.LLVMFloatTypeKind or target_kind == c.LLVMDoubleTypeKind or target_kind == c.LLVMHalfTypeKind)))
    {
        if (source_type_name) |src_name| {
            if (utils.isUnsignedType(src_name)) {
                return c.LLVMBuildUIToFP(cg.builder, value, target_type, "uitofp");
            }
        }
        return c.LLVMBuildSIToFP(cg.builder, value, target_type, "sitofp");
    } else if ((value_kind == c.LLVMFloatTypeKind or value_kind == c.LLVMDoubleTypeKind or value_kind == c.LLVMHalfTypeKind) and
        target_kind == c.LLVMIntegerTypeKind)
    {
        return c.LLVMBuildFPToSI(cg.builder, value, target_type, "fptosi");
    } else if (value_kind == c.LLVMPointerTypeKind and target_kind == c.LLVMStructTypeKind) {
        return c.LLVMBuildLoad2(cg.builder, target_type, value, "struct_load");
    } else if (value_kind == c.LLVMPointerTypeKind and target_kind == c.LLVMPointerTypeKind) {
        return c.LLVMBuildBitCast(cg.builder, value, target_type, "bitcast");
    } else if (value_kind == c.LLVMIntegerTypeKind and target_kind == c.LLVMPointerTypeKind) {
        const const_int = c.LLVMIsAConstantInt(value);
        if (const_int != null and c.LLVMConstIntGetSExtValue(value) == 0) {
            return c.LLVMConstNull(target_type);
        }
        return c.LLVMBuildIntToPtr(cg.builder, value, target_type, "int_to_ptr");
    } else if (value_kind == c.LLVMPointerTypeKind and target_kind == c.LLVMIntegerTypeKind) {
        return c.LLVMBuildPtrToInt(cg.builder, value, target_type, "ptr_to_int");
    }

    return value;
}

pub fn castWithSourceRules(cg: *llvm.CodeGenerator, value: c.LLVMValueRef, target_type: c.LLVMTypeRef, value_node: ?*ast.Node) errors.CodegenError!c.LLVMValueRef {
    return cg.castWithSourceRulesNamed(value, target_type, value_node, null);
}

pub fn castWithSourceRulesNamed(cg: *llvm.CodeGenerator, value: c.LLVMValueRef, target_type: c.LLVMTypeRef, value_node: ?*ast.Node, target_type_name: ?[]const u8) errors.CodegenError!c.LLVMValueRef {
    var val = value;
    var from_ty = c.LLVMTypeOf(val);
    var source_type_name: ?[]const u8 = null;
    if (value_node) |vn| {
        if (vn.data == .identifier) {
            const var_name = vn.data.identifier;
            if (llvm.CodeGenerator.getVariable(cg, var_name.name)) |var_info| {
                source_type_name = var_info.type_name;
            }
        } else if (vn.data == .qualified_identifier) {
            const qual_id = vn.data.qualified_identifier;
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
        } else if (vn.data == .cast) {
            const cst = vn.data.cast;
            if (cst.auto) {
                return cg.castToTypeWithSourceInfo(val, target_type, source_type_name);
            } else if (cst.type_name) |tn| {
                const named_ty = try cg.getLLVMType(tn);
                val = cg.castToTypeWithSourceInfo(val, @ptrCast(named_ty), source_type_name);
                from_ty = @ptrCast(named_ty);
            }
        }
    }
    if (target_type_name) |tname| {
        if (source_type_name) |sname| {
            if (utils.isIntPrimitive(sname) and utils.isIntPrimitive(tname) and
                !std.mem.eql(u8, sname, tname) and
                utils.isUnsignedType(sname) != utils.isUnsignedType(tname))
            {
                cg.reportErrorFmt("Type mismatch: cannot implicitly convert {s} to {s} (signedness differs)", .{ sname, tname }, "Add an explicit cast (as <type> or as _)");
                return errors.CodegenError.TypeMismatch;
            }
        }
    }
    if (cg.typesAreEqual(from_ty, target_type)) return val;
    const fk = c.LLVMGetTypeKind(from_ty);
    const tk = c.LLVMGetTypeKind(target_type);
    if (fk == c.LLVMIntegerTypeKind and tk == c.LLVMIntegerTypeKind) {
        const fw = c.LLVMGetIntTypeWidth(from_ty);
        const tw = c.LLVMGetIntTypeWidth(target_type);
        if (tw >= fw) {
            return cg.castToTypeWithSourceInfo(val, target_type, source_type_name);
        }
    } else {
        const f_float = fk == c.LLVMFloatTypeKind or fk == c.LLVMDoubleTypeKind or fk == c.LLVMHalfTypeKind;
        const t_float = tk == c.LLVMFloatTypeKind or tk == c.LLVMDoubleTypeKind or tk == c.LLVMHalfTypeKind;
        if (f_float and t_float and floatBitWidth(target_type) >= floatBitWidth(from_ty)) {
            return cg.castToTypeWithSourceInfo(val, target_type, source_type_name);
        }
    }
    if (fk == c.LLVMPointerTypeKind and tk == c.LLVMStructTypeKind) {
        var can_load_struct = false;
        if (value_node) |vn3| {
            switch (vn3.data) {
                .struct_initializer => can_load_struct = true,
                .function_call => can_load_struct = true,
                .method_call => can_load_struct = true,
                .identifier => |ident| {
                    if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                        can_load_struct = pointerTypeMatchesTargetStruct(cg, var_info.type_name, target_type);
                    }
                },
                .cast => |cst| {
                    if (cst.type_name) |tn| {
                        can_load_struct = pointerTypeMatchesTargetStruct(cg, tn, target_type);
                    }
                },
                else => {},
            }
        }
        if (!can_load_struct) {
            if (source_type_name) |src_name| {
                can_load_struct = pointerTypeMatchesTargetStruct(cg, src_name, target_type);
            }
        }
        if (can_load_struct) {
            return c.LLVMBuildLoad2(cg.builder, target_type, val, "struct_load");
        }
    }
    if (fk == c.LLVMPointerTypeKind and tk == c.LLVMPointerTypeKind) {
        return cg.castToTypeWithSourceInfo(val, target_type, source_type_name);
    }
    const to_kind = c.LLVMGetTypeKind(target_type);
    if (value_node) |vn2| {
        switch (vn2.data) {
            .number_literal => {
                if (to_kind == c.LLVMIntegerTypeKind) {
                    return cg.castToTypeWithSourceInfo(val, target_type, source_type_name);
                }
            },
            .char_literal => {
                if (to_kind == c.LLVMIntegerTypeKind) {
                    return cg.castToTypeWithSourceInfo(val, target_type, source_type_name);
                }
            },
            .float_literal => {
                if (to_kind == c.LLVMFloatTypeKind or to_kind == c.LLVMDoubleTypeKind or to_kind == c.LLVMHalfTypeKind) {
                    return cg.castToTypeWithSourceInfo(val, target_type, source_type_name);
                }
            },
            .unary_op => |un| {
                if (un.op == '-' or un.op == '+') {
                    const inner = un.operand;
                    switch (inner.data) {
                        .number_literal => {
                            if (to_kind == c.LLVMIntegerTypeKind) {
                                return cg.castToTypeWithSourceInfo(val, target_type, source_type_name);
                            }
                        },
                        .float_literal => {
                            if (to_kind == c.LLVMFloatTypeKind or to_kind == c.LLVMDoubleTypeKind or to_kind == c.LLVMHalfTypeKind) {
                                return cg.castToTypeWithSourceInfo(val, target_type, source_type_name);
                            }
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
    }

    const from_name = cg.getTypeNameFromLLVMType(@ptrCast(from_ty));
    const to_name = cg.getTypeNameFromLLVMType(@ptrCast(target_type));
    const hint = std.fmt.allocPrint(cg.allocator, "This conversion may lose information; use an explicit cast: `<value> as {s}`", .{to_name}) catch null;
    defer if (hint) |h| cg.allocator.free(h);
    if (value_node) |vn| {
        const old_line = cg.current_line;
        const old_col = cg.current_column;
        const old_token_text = cg.current_token_text;
        if (vn.line > 0) cg.current_line = vn.line;
        if (vn.column > 0) cg.current_column = vn.column;
        cg.current_token_text = llvm.CodeGenerator.tokenTextForNode(vn);
        cg.reportErrorFmt("Type mismatch: cannot convert {s} to {s} implicitly", .{ from_name, to_name }, hint);
        cg.current_line = old_line;
        cg.current_column = old_col;
        cg.current_token_text = old_token_text;
    } else {
        cg.reportErrorFmt("Type mismatch: cannot convert {s} to {s} implicitly", .{ from_name, to_name }, hint);
    }
    return errors.CodegenError.TypeMismatch;
}

fn floatBitWidth(ty: c.LLVMTypeRef) usize {
    return switch (c.LLVMGetTypeKind(ty)) {
        c.LLVMHalfTypeKind => 16,
        c.LLVMFloatTypeKind => 32,
        c.LLVMDoubleTypeKind => 64,
        else => 0,
    };
}

fn isWidening(from_ty: c.LLVMTypeRef, to_ty: c.LLVMTypeRef) bool {
    const from_kind = c.LLVMGetTypeKind(from_ty);
    const to_kind = c.LLVMGetTypeKind(to_ty);

    if (from_kind == c.LLVMIntegerTypeKind and to_kind == c.LLVMIntegerTypeKind) {
        return c.LLVMGetIntTypeWidth(to_ty) >= c.LLVMGetIntTypeWidth(from_ty);
    }

    const from_is_float = from_kind == c.LLVMFloatTypeKind or from_kind == c.LLVMDoubleTypeKind or from_kind == c.LLVMHalfTypeKind;
    const to_is_float = to_kind == c.LLVMFloatTypeKind or to_kind == c.LLVMDoubleTypeKind or to_kind == c.LLVMHalfTypeKind;
    if (from_is_float and to_is_float) {
        return floatBitWidth(to_ty) >= floatBitWidth(from_ty);
    }

    return false;
}

pub fn pointerTypeMatchesTargetStruct(cg: *llvm.CodeGenerator, type_name: []const u8, target_type: c.LLVMTypeRef) bool {
    if (!(std.mem.startsWith(u8, type_name, "ptr<") and std.mem.endsWith(u8, type_name, ">"))) {
        return false;
    }

    const inner_name = type_name[4 .. type_name.len - 1];
    const maybe_inner_ty = utils.getLLVMTypeSilent(cg, inner_name) catch null;
    if (maybe_inner_ty) |inner_ty| {
        if (c.LLVMGetTypeKind(inner_ty) == c.LLVMStructTypeKind and cg.typesAreEqual(inner_ty, target_type)) {
            return true;
        }
    }

    return false;
}

pub fn typesAreEqual(cg: *llvm.CodeGenerator, type1: c.LLVMTypeRef, type2: c.LLVMTypeRef) bool {
    if (type1 == type2) return true;

    const kind1 = c.LLVMGetTypeKind(type1);
    const kind2 = c.LLVMGetTypeKind(type2);

    if (kind1 != kind2) return false;

    switch (kind1) {
        c.LLVMIntegerTypeKind => {
            return c.LLVMGetIntTypeWidth(type1) == c.LLVMGetIntTypeWidth(type2);
        },
        c.LLVMArrayTypeKind => {
            const len1 = c.LLVMGetArrayLength(type1);
            const len2 = c.LLVMGetArrayLength(type2);
            if (len1 != len2) return false;
            const elem1 = c.LLVMGetElementType(type1);
            const elem2 = c.LLVMGetElementType(type2);
            return cg.typesAreEqual(elem1, elem2);
        },
        c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => {
            return true;
        },
        c.LLVMPointerTypeKind => {
            return true;
        },
        c.LLVMVoidTypeKind => {
            return true;
        },
        else => {
            return false;
        },
    }
}

pub fn castWithRules(cg: *llvm.CodeGenerator, value: c.LLVMValueRef, target_type: c.LLVMTypeRef, value_node: ?*ast.Node) errors.CodegenError!c.LLVMValueRef {
    var val = value;
    var from_ty = c.LLVMTypeOf(val);
    if (value_node) |vn| {
        if (vn.data == .cast) {
            const cst = vn.data.cast;
            if (cst.auto) {
                return cg.castToType(val, target_type);
            } else if (cst.type_name) |tn| {
                const named_ty = try cg.getLLVMType(tn);
                val = cg.castToType(val, @ptrCast(named_ty));
                from_ty = @ptrCast(named_ty);
            }
        }
    }
    if (cg.typesAreEqual(from_ty, target_type)) return val;

    const fk = c.LLVMGetTypeKind(from_ty);
    const tk = c.LLVMGetTypeKind(target_type);
    if (fk == c.LLVMIntegerTypeKind and tk == c.LLVMIntegerTypeKind) {
        const fw = c.LLVMGetIntTypeWidth(from_ty);
        const tw = c.LLVMGetIntTypeWidth(target_type);
        if (tw >= fw) {
            return cg.castToType(val, target_type);
        }
    } else {
        const f_float = fk == c.LLVMFloatTypeKind or fk == c.LLVMDoubleTypeKind or fk == c.LLVMHalfTypeKind;
        const t_float = tk == c.LLVMFloatTypeKind or tk == c.LLVMDoubleTypeKind or tk == c.LLVMHalfTypeKind;
        if (f_float and t_float and floatBitWidth(target_type) >= floatBitWidth(from_ty)) {
            return cg.castToType(val, target_type);
        }
    }

    if (fk == c.LLVMPointerTypeKind and tk == c.LLVMStructTypeKind) {
        var can_load_struct = false;
        if (value_node) |vn3| {
            switch (vn3.data) {
                .struct_initializer => can_load_struct = true,
                .function_call => can_load_struct = true,
                .method_call => can_load_struct = true,
                .identifier => |ident| {
                    if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                        can_load_struct = pointerTypeMatchesTargetStruct(cg, var_info.type_name, target_type);
                    }
                },
                .cast => |cst| {
                    if (cst.type_name) |tn| {
                        can_load_struct = pointerTypeMatchesTargetStruct(cg, tn, target_type);
                    }
                },
                else => {},
            }
        }
        if (can_load_struct) {
            return c.LLVMBuildLoad2(cg.builder, target_type, val, "struct_load");
        }
    }

    if (fk == c.LLVMPointerTypeKind and tk == c.LLVMPointerTypeKind) {
        return cg.castToType(val, target_type);
    }
    if (fk == c.LLVMVectorTypeKind and tk == c.LLVMVectorTypeKind) {
        const from_element_type = c.LLVMGetElementType(from_ty);
        const to_element_type = c.LLVMGetElementType(target_type);
        const from_size = c.LLVMGetVectorSize(from_ty);
        const to_size = c.LLVMGetVectorSize(target_type);
        if (cg.typesAreEqual(from_element_type, to_element_type) and from_size == to_size) {
            return cg.castToType(val, target_type);
        }
    }

    const to_kind = c.LLVMGetTypeKind(target_type);
    if (value_node) |vn2| {
        switch (vn2.data) {
            .number_literal => {
                if (to_kind == c.LLVMIntegerTypeKind) {
                    return cg.castToType(val, target_type);
                }
            },
            .char_literal => {
                if (to_kind == c.LLVMIntegerTypeKind) {
                    return cg.castToType(val, target_type);
                }
            },
            .float_literal => {
                if (to_kind == c.LLVMFloatTypeKind or to_kind == c.LLVMDoubleTypeKind or to_kind == c.LLVMHalfTypeKind) {
                    return cg.castToType(val, target_type);
                }
            },
            .unary_op => |un| {
                if (un.op == '-' or un.op == '+') {
                    const inner = un.operand;
                    switch (inner.data) {
                        .number_literal => {
                            if (to_kind == c.LLVMIntegerTypeKind) {
                                return cg.castToType(val, target_type);
                            }
                        },
                        .float_literal => {
                            if (to_kind == c.LLVMFloatTypeKind or to_kind == c.LLVMDoubleTypeKind or to_kind == c.LLVMHalfTypeKind) {
                                return cg.castToType(val, target_type);
                            }
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
    }
    const from_name = cg.getTypeNameFromLLVMType(@ptrCast(from_ty));
    const to_name = cg.getTypeNameFromLLVMType(@ptrCast(target_type));
    const hint = std.fmt.allocPrint(cg.allocator, "This conversion may lose information; use an explicit cast: `<value> as {s}`", .{to_name}) catch null;
    defer if (hint) |h| cg.allocator.free(h);
    if (value_node) |vn| {
        const old_line = cg.current_line;
        const old_col = cg.current_column;
        const old_token_text = cg.current_token_text;
        if (vn.line > 0) cg.current_line = vn.line;
        if (vn.column > 0) cg.current_column = vn.column;
        cg.current_token_text = llvm.CodeGenerator.tokenTextForNode(vn);
        cg.reportErrorFmt("Type mismatch: cannot convert {s} to {s} implicitly", .{ from_name, to_name }, hint);
        cg.current_line = old_line;
        cg.current_column = old_col;
        cg.current_token_text = old_token_text;
    } else {
        cg.reportErrorFmt("Type mismatch: cannot convert {s} to {s} implicitly", .{ from_name, to_name }, hint);
    }
    return errors.CodegenError.TypeMismatch;
}
