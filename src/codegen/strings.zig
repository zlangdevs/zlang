const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;
const llvm = @import("llvm.zig");

const INTERP_POOL_COUNT: u64 = 8;
const INTERP_BUF_SIZE: u64 = 4096;

pub fn parseEscape(allocator: std.mem.Allocator, str: []const u8) errors.CodegenError![]const u8 {
    var transformed_string = std.ArrayList(u8){};
    defer transformed_string.deinit(allocator);
    var i: usize = 0;
    while (i < str.len) : (i += 1) {
        if (str[i] == '\\') {
            i += 1;
            if (i < str.len) {
                switch (str[i]) {
                    'n' => transformed_string.append(allocator, '\n') catch return errors.CodegenError.OutOfMemory,
                    't' => transformed_string.append(allocator, '\t') catch return errors.CodegenError.OutOfMemory,
                    'r' => transformed_string.append(allocator, '\r') catch return errors.CodegenError.OutOfMemory,
                    '\'' => transformed_string.append(allocator, '\'') catch return errors.CodegenError.OutOfMemory,
                    '"' => transformed_string.append(allocator, '"') catch return errors.CodegenError.OutOfMemory,
                    '0' => transformed_string.append(allocator, 0) catch return errors.CodegenError.OutOfMemory,
                    '\\' => transformed_string.append(allocator, '\\') catch return errors.CodegenError.OutOfMemory,
                    '$' => transformed_string.append(allocator, '$') catch return errors.CodegenError.OutOfMemory,
                    else => {
                        transformed_string.append(allocator, '\\') catch return errors.CodegenError.OutOfMemory;
                        transformed_string.append(allocator, str[i]) catch return errors.CodegenError.OutOfMemory;
                    },
                }
            } else {
                transformed_string.append(allocator, '\\') catch return errors.CodegenError.OutOfMemory;
            }
        } else {
            transformed_string.append(allocator, str[i]) catch return errors.CodegenError.OutOfMemory;
        }
    }
    try transformed_string.append(allocator, 0);
    return transformed_string.toOwnedSlice(allocator);
}

pub fn generateStringLiteral(cg: *llvm.CodeGenerator, str_lit: ast.StringLiteral) errors.CodegenError!c.LLVMValueRef {
    const parsed_str = try parseEscape(cg.allocator, str_lit.value);
    defer cg.allocator.free(parsed_str);

    const str_with_null = try std.mem.concatWithSentinel(cg.allocator, u8, &.{parsed_str}, 0);
    defer cg.allocator.free(str_with_null);

    return c.LLVMBuildGlobalStringPtr(
        cg.builder,
        str_with_null.ptr,
        "str",
    );
}

fn getOrAddFunction(cg: *llvm.CodeGenerator, name: []const u8, ret_ty: c.LLVMTypeRef, param_tys: []const c.LLVMTypeRef, is_varargs: bool) c.LLVMValueRef {
    const name_z = cg.allocator.dupeZ(u8, name) catch unreachable;
    defer cg.allocator.free(name_z);

    if (c.LLVMGetNamedFunction(cg.module, name_z.ptr)) |f| {
        return f;
    }
    const fn_ty = if (param_tys.len == 0)
        c.LLVMFunctionType(ret_ty, null, 0, if (is_varargs) 1 else 0)
    else
        c.LLVMFunctionType(ret_ty, @constCast(param_tys.ptr), @intCast(param_tys.len), if (is_varargs) 1 else 0);

    return c.LLVMAddFunction(cg.module, name_z.ptr, fn_ty);
}

fn ensureInterpolationPool(cg: *llvm.CodeGenerator) struct { pool: c.LLVMValueRef, index: c.LLVMValueRef, outer_ty: c.LLVMTypeRef } {
    const i8_ty = c.LLVMInt8TypeInContext(cg.context);
    const i32_ty = c.LLVMInt32TypeInContext(cg.context);
    const inner_ty = c.LLVMArrayType(i8_ty, INTERP_BUF_SIZE);
    const outer_ty = c.LLVMArrayType(inner_ty, INTERP_POOL_COUNT);

    var pool = c.LLVMGetNamedGlobal(cg.module, "__zlang_interp_pool");
    if (pool == null) {
        pool = c.LLVMAddGlobal(cg.module, outer_ty, "__zlang_interp_pool");
        c.LLVMSetInitializer(pool, c.LLVMConstNull(outer_ty));
    }

    var idx = c.LLVMGetNamedGlobal(cg.module, "__zlang_interp_pool_idx");
    if (idx == null) {
        idx = c.LLVMAddGlobal(cg.module, i32_ty, "__zlang_interp_pool_idx");
        c.LLVMSetInitializer(idx, c.LLVMConstInt(i32_ty, 0, 0));
    }

    return .{ .pool = pool, .index = idx, .outer_ty = outer_ty };
}

fn appendFormatted(cg: *llvm.CodeGenerator, dst: c.LLVMValueRef, fmt: []const u8, args: []const c.LLVMValueRef) c.LLVMValueRef {
    const i8_ty = c.LLVMInt8TypeInContext(cg.context);
    const i8_ptr_ty = c.LLVMPointerType(i8_ty, 0);
    const size_t_ty = c.LLVMInt64TypeInContext(cg.context);
    const i32_ty = c.LLVMInt32TypeInContext(cg.context);

    const strlen = getOrAddFunction(cg, "strlen", size_t_ty, &[_]c.LLVMTypeRef{i8_ptr_ty}, false);
    const snprintf = getOrAddFunction(cg, "snprintf", i32_ty, &[_]c.LLVMTypeRef{ i8_ptr_ty, size_t_ty, i8_ptr_ty }, true);

    var strlen_args = [_]c.LLVMValueRef{dst};
    const cur_len = c.LLVMBuildCall2(cg.builder, c.LLVMGlobalGetValueType(strlen), strlen, &strlen_args, 1, "interp_len");

    const total_sz = c.LLVMConstInt(size_t_ty, INTERP_BUF_SIZE, 0);
    const remain = c.LLVMBuildSub(cg.builder, total_sz, cur_len, "interp_remain");

    var idxs = [_]c.LLVMValueRef{cur_len};
    const dst_tail = c.LLVMBuildGEP2(cg.builder, i8_ty, dst, &idxs, 1, "interp_tail");

    const fmt_z = cg.allocator.dupeZ(u8, fmt) catch unreachable;
    defer cg.allocator.free(fmt_z);
    const fmt_ptr = c.LLVMBuildGlobalStringPtr(cg.builder, fmt_z.ptr, "interp_fmt");

    var call_args = std.ArrayList(c.LLVMValueRef){};
    defer call_args.deinit(cg.allocator);
    call_args.append(cg.allocator, dst_tail) catch unreachable;
    call_args.append(cg.allocator, remain) catch unreachable;
    call_args.append(cg.allocator, fmt_ptr) catch unreachable;
    for (args) |a| call_args.append(cg.allocator, a) catch unreachable;

    _ = c.LLVMBuildCall2(cg.builder, c.LLVMGlobalGetValueType(snprintf), snprintf, call_args.items.ptr, @intCast(call_args.items.len), "");
    return dst;
}

fn appendAnyValue(cg: *llvm.CodeGenerator, dst: c.LLVMValueRef, value: c.LLVMValueRef, hint_type_name: ?[]const u8) errors.CodegenError!void {
    if (value == null) {
        _ = appendFormatted(cg, dst, "<null>", &[_]c.LLVMValueRef{});
        return;
    }

    const ty = c.LLVMTypeOf(value);
    const kind = c.LLVMGetTypeKind(ty);
    const i8_ty = c.LLVMInt8TypeInContext(cg.context);
    const i8_ptr_ty = c.LLVMPointerType(i8_ty, 0);
    const i64_ty = c.LLVMInt64TypeInContext(cg.context);
    const f64_ty = c.LLVMDoubleTypeInContext(cg.context);

    if (kind == c.LLVMPointerTypeKind) {
        if (hint_type_name) |hint| {
            if (std.mem.eql(u8, hint, "ptr<u8>")) {
                _ = appendFormatted(cg, dst, "%s", &[_]c.LLVMValueRef{value});
                return;
            }
        }

        if (c.LLVMIsNull(value) != 0) {
            _ = appendFormatted(cg, dst, "null", &[_]c.LLVMValueRef{});
            return;
        }

        const as_void_ptr = c.LLVMBuildBitCast(cg.builder, value, i8_ptr_ty, "interp_ptr");
        _ = appendFormatted(cg, dst, "%p", &[_]c.LLVMValueRef{as_void_ptr});
        return;
    }

    if (kind == c.LLVMIntegerTypeKind) {
        if (c.LLVMGetIntTypeWidth(ty) == 1) {
            const t = c.LLVMBuildGlobalStringPtr(cg.builder, "true", "interp_true");
            const f = c.LLVMBuildGlobalStringPtr(cg.builder, "false", "interp_false");
            const b = c.LLVMBuildSelect(cg.builder, value, t, f, "interp_bool");
            _ = appendFormatted(cg, dst, "%s", &[_]c.LLVMValueRef{b});
            return;
        }
        const i64_val = c.LLVMBuildSExt(cg.builder, value, i64_ty, "interp_i64");
        _ = appendFormatted(cg, dst, "%lld", &[_]c.LLVMValueRef{i64_val});
        return;
    }

    if (kind == c.LLVMFloatTypeKind or kind == c.LLVMHalfTypeKind or kind == c.LLVMDoubleTypeKind) {
        const f64_val = if (kind == c.LLVMDoubleTypeKind)
            value
        else
            c.LLVMBuildFPExt(cg.builder, value, f64_ty, "interp_f64");
        _ = appendFormatted(cg, dst, "%g", &[_]c.LLVMValueRef{f64_val});
        return;
    }

    if (kind == c.LLVMStructTypeKind) {
        const struct_name = if (hint_type_name) |n| n else blk: {
            const n_ptr = c.LLVMGetStructName(ty);
            if (n_ptr != null) {
                break :blk std.mem.span(n_ptr);
            }
            break :blk "";
        };

        if (struct_name.len == 0) {
            _ = appendFormatted(cg, dst, "{...}", &[_]c.LLVMValueRef{});
            return;
        }

        const decl = cg.struct_declarations.get(struct_name) orelse {
            _ = appendFormatted(cg, dst, "{...}", &[_]c.LLVMValueRef{});
            return;
        };

        _ = appendFormatted(cg, dst, "{", &[_]c.LLVMValueRef{});

        const tmp = c.LLVMBuildAlloca(cg.builder, ty, "interp_struct_tmp");
        _ = c.LLVMBuildStore(cg.builder, value, tmp);

        for (decl.fields.items, 0..) |field, idx| {
            if (idx > 0) _ = appendFormatted(cg, dst, ", ", &[_]c.LLVMValueRef{});
            _ = appendFormatted(cg, dst, field.name, &[_]c.LLVMValueRef{});
            _ = appendFormatted(cg, dst, " = ", &[_]c.LLVMValueRef{});

            const field_ptr = c.LLVMBuildStructGEP2(cg.builder, ty, tmp, @intCast(idx), "interp_field_ptr");
            const field_ty = c.LLVMStructGetTypeAtIndex(ty, @intCast(idx));
            const field_val = c.LLVMBuildLoad2(cg.builder, field_ty, field_ptr, "interp_field_val");
            try appendAnyValue(cg, dst, field_val, field.type_name);
        }

        _ = appendFormatted(cg, dst, "}", &[_]c.LLVMValueRef{});
        return;
    }

    _ = appendFormatted(cg, dst, "<value>", &[_]c.LLVMValueRef{});
}

pub fn generateInterpolationBuiltin(cg: *llvm.CodeGenerator, call: ast.FunctionCall) errors.CodegenError!c.LLVMValueRef {
    const i8_ty = c.LLVMInt8TypeInContext(cg.context);
    const i32_ty = c.LLVMInt32TypeInContext(cg.context);
    const i8_ptr_ty = c.LLVMPointerType(i8_ty, 0);

    const pool_info = ensureInterpolationPool(cg);
    const cur_idx = c.LLVMBuildLoad2(cg.builder, i32_ty, pool_info.index, "interp_idx");
    const one = c.LLVMConstInt(i32_ty, 1, 0);
    const count = c.LLVMConstInt(i32_ty, INTERP_POOL_COUNT, 0);
    const next = c.LLVMBuildURem(cg.builder, c.LLVMBuildAdd(cg.builder, cur_idx, one, "interp_next_idx"), count, "interp_mod");
    _ = c.LLVMBuildStore(cg.builder, next, pool_info.index);

    const zero = c.LLVMConstInt(i32_ty, 0, 0);
    var idxs = [_]c.LLVMValueRef{ zero, next, zero };
    const dst = c.LLVMBuildGEP2(cg.builder, pool_info.outer_ty, pool_info.pool, &idxs, 3, "interp_buf");

    _ = c.LLVMBuildStore(cg.builder, c.LLVMConstInt(i8_ty, 0, 0), dst);

    for (call.args.items) |arg| {
        if (arg.data == .null_literal) {
            _ = appendFormatted(cg, dst, "null", &[_]c.LLVMValueRef{});
            continue;
        }

        const val = try cg.generateExpression(arg);

        var hint_type_name: ?[]const u8 = null;
        switch (arg.data) {
            .identifier => |ident| {
                if (llvm.CodeGenerator.getVariable(cg, ident.name)) |var_info| {
                    hint_type_name = var_info.type_name;
                }
            },
            .cast => |cast_node| {
                if (cast_node.type_name) |tn| hint_type_name = tn;
            },
            else => {},
        }

        if (arg.data == .string_literal) {
            const as_cstr = c.LLVMBuildBitCast(cg.builder, val, i8_ptr_ty, "interp_cstr");
            _ = appendFormatted(cg, dst, "%s", &[_]c.LLVMValueRef{as_cstr});
        } else if (arg.data == .function_call or arg.data == .method_call) {
            const val_ty = c.LLVMTypeOf(val);
            if (c.LLVMGetTypeKind(val_ty) == c.LLVMPointerTypeKind) {
                const as_cstr = c.LLVMBuildBitCast(cg.builder, val, i8_ptr_ty, "interp_call_cstr");
                _ = appendFormatted(cg, dst, "%s", &[_]c.LLVMValueRef{as_cstr});
            } else {
                try appendAnyValue(cg, dst, val, hint_type_name);
            }
        } else {
            try appendAnyValue(cg, dst, val, hint_type_name);
        }
    }

    return dst;
}
