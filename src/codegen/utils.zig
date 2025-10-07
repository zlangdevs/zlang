const codegen = @import("llvm.zig");
const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

pub const LibcType = enum {
    void_type,
    int_type,
    char_ptr_type,
    size_t_type,
    file_ptr_type,
    long_type,
    double_type,
};

pub const LibcFunctionSignature = struct {
    return_type: LibcType,
    param_types: []const LibcType,
    is_varargs: bool = false,
};

// Temporary thing untill we have std lib
pub const LIBC_FUNCTIONS = std.StaticStringMap(LibcFunctionSignature).initComptime(.{
    .{ "printf", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{.char_ptr_type}, .is_varargs = true } },
    .{ "puts", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{.char_ptr_type} } },
    .{ "scanf", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{.char_ptr_type}, .is_varargs = true } },
    .{ "fprintf", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{ .file_ptr_type, .char_ptr_type }, .is_varargs = true } },
    .{ "sprintf", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{ .char_ptr_type, .char_ptr_type }, .is_varargs = true } },
    .{ "snprintf", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{ .char_ptr_type, .size_t_type, .char_ptr_type }, .is_varargs = true } },
    .{ "malloc", LibcFunctionSignature{ .return_type = .char_ptr_type, .param_types = &[_]LibcType{.size_t_type} } },
    .{ "free", LibcFunctionSignature{ .return_type = .void_type, .param_types = &[_]LibcType{.char_ptr_type} } },
    .{ "strlen", LibcFunctionSignature{ .return_type = .size_t_type, .param_types = &[_]LibcType{.char_ptr_type} } },
    .{ "strcpy", LibcFunctionSignature{ .return_type = .char_ptr_type, .param_types = &[_]LibcType{ .char_ptr_type, .char_ptr_type } } },
    .{ "strcmp", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{ .char_ptr_type, .char_ptr_type } } },
    .{ "memset", LibcFunctionSignature{ .return_type = .char_ptr_type, .param_types = &[_]LibcType{ .char_ptr_type, .int_type, .size_t_type } } },
    .{ "memcpy", LibcFunctionSignature{ .return_type = .char_ptr_type, .param_types = &[_]LibcType{ .char_ptr_type, .char_ptr_type, .size_t_type } } },
    .{ "exit", LibcFunctionSignature{ .return_type = .void_type, .param_types = &[_]LibcType{.int_type} } },
    .{ "atoi", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{.char_ptr_type} } },
    .{ "atol", LibcFunctionSignature{ .return_type = .long_type, .param_types = &[_]LibcType{.char_ptr_type} } },
    .{ "fopen", LibcFunctionSignature{ .return_type = .file_ptr_type, .param_types = &[_]LibcType{ .char_ptr_type, .char_ptr_type } } },
    .{ "fclose", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{.file_ptr_type} } },
    .{ "fread", LibcFunctionSignature{ .return_type = .size_t_type, .param_types = &[_]LibcType{ .char_ptr_type, .size_t_type, .size_t_type, .file_ptr_type } } },
    .{ "fwrite", LibcFunctionSignature{ .return_type = .size_t_type, .param_types = &[_]LibcType{ .char_ptr_type, .size_t_type, .size_t_type, .file_ptr_type } } },
    .{ "system", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{.char_ptr_type} } },
    .{ "putchar", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{.int_type} } },
    .{ "getchar", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{} } },
    .{ "rand", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{} } },
    .{ "srand", LibcFunctionSignature{ .return_type = .void_type, .param_types = &[_]LibcType{.int_type} } },
    .{ "square", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{.int_type} } },
});

pub fn libcTypeToLLVM(cg: *codegen.CodeGenerator, libc_type: LibcType) c.LLVMTypeRef {
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

pub fn isReturnStatement(stmt: *ast.Node) bool {
    return switch (stmt.data) {
        .return_stmt => true,
        else => false,
    };
}

pub fn convertToBool(cg: *codegen.CodeGenerator, value: c.LLVMValueRef) c.LLVMValueRef {
    const value_type = c.LLVMTypeOf(value);
    const type_kind = c.LLVMGetTypeKind(value_type);
    if (type_kind == c.LLVMIntegerTypeKind and c.LLVMGetIntTypeWidth(value_type) == 1) {
        return value;
    }
    if (type_kind == c.LLVMIntegerTypeKind) {
        const zero = c.LLVMConstInt(value_type, 0, 0);
        return c.LLVMBuildICmp(cg.builder, c.LLVMIntNE, value, zero, "tobool");
    }
    if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind) {
        const zero = c.LLVMConstReal(value_type, 0.0);
        return c.LLVMBuildFCmp(cg.builder, c.LLVMRealONE, value, zero, "tobool");
    }
    if (type_kind == c.LLVMPointerTypeKind) {
        const null_ptr = c.LLVMConstNull(value_type);
        return c.LLVMBuildICmp(cg.builder, c.LLVMIntNE, value, null_ptr, "tobool");
    }
    return c.LLVMConstInt(c.LLVMInt1TypeInContext(cg.context), 1, 0);
}

pub fn getDefaultValueForType(cg: *codegen.CodeGenerator, type_name: []const u8) c.LLVMValueRef {
    if (std.mem.eql(u8, type_name, "i8")) {
        return c.LLVMConstInt(c.LLVMInt8TypeInContext(@ptrCast(cg.context)), 0, 0);
    } else if (std.mem.eql(u8, type_name, "i16")) {
        return c.LLVMConstInt(c.LLVMInt16TypeInContext(@ptrCast(cg.context)), 0, 0);
    } else if (std.mem.eql(u8, type_name, "i32")) {
        return c.LLVMConstInt(c.LLVMInt32TypeInContext(@ptrCast(cg.context)), 0, 0);
    } else if (std.mem.eql(u8, type_name, "i64")) {
        return c.LLVMConstInt(c.LLVMInt64TypeInContext(@ptrCast(cg.context)), 0, 0);
    } else if (std.mem.eql(u8, type_name, "u8")) {
        return c.LLVMConstInt(c.LLVMInt8TypeInContext(@ptrCast(cg.context)), 0, 0);
    } else if (std.mem.eql(u8, type_name, "u16")) {
        return c.LLVMConstInt(c.LLVMInt16TypeInContext(@ptrCast(cg.context)), 0, 0);
    } else if (std.mem.eql(u8, type_name, "u32")) {
        return c.LLVMConstInt(c.LLVMInt32TypeInContext(@ptrCast(cg.context)), 0, 0);
    } else if (std.mem.eql(u8, type_name, "u64")) {
        return c.LLVMConstInt(c.LLVMInt64TypeInContext(@ptrCast(cg.context)), 0, 0);
    } else if (std.mem.eql(u8, type_name, "f16")) {
        return c.LLVMConstReal(c.LLVMHalfTypeInContext(@ptrCast(cg.context)), 0.0);
    } else if (std.mem.eql(u8, type_name, "f32")) {
        return c.LLVMConstReal(c.LLVMFloatTypeInContext(@ptrCast(cg.context)), 0.0);
    } else if (std.mem.eql(u8, type_name, "f64")) {
        return c.LLVMConstReal(c.LLVMDoubleTypeInContext(@ptrCast(cg.context)), 0.0);
    } else if (std.mem.eql(u8, type_name, "bool")) {
        return c.LLVMConstInt(c.LLVMInt1TypeInContext(@ptrCast(cg.context)), 0, 0);
    } else if (std.mem.startsWith(u8, type_name, "simd<") and std.mem.endsWith(u8, type_name, ">")) {
        const inner = type_name[5 .. type_name.len - 1];
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

            if (std.fmt.parseInt(u32, size_str, 10) catch null) |vector_size| {
                const element_default = getDefaultValueForType(cg, element_type_name);
                const elements = cg.allocator.alloc(c.LLVMValueRef, vector_size) catch return c.LLVMConstInt(c.LLVMInt32TypeInContext(@ptrCast(cg.context)), 0, 0);
                defer cg.allocator.free(elements);
                for (elements) |*element| {
                    element.* = element_default;
                }

                return c.LLVMConstVector(elements.ptr, vector_size);
            }
        }
        return c.LLVMConstInt(c.LLVMInt32TypeInContext(@ptrCast(cg.context)), 0, 0);
    }
    return c.LLVMConstInt(c.LLVMInt32TypeInContext(@ptrCast(cg.context)), 0, 0);
}

pub fn getLLVMType(self: *codegen.CodeGenerator, type_name: []const u8) c.LLVMTypeRef {
    if (std.mem.startsWith(u8, type_name, "ptr<") and std.mem.endsWith(u8, type_name, ">")) {
        const inner_type_name = type_name[4 .. type_name.len - 1];
        // detect function type inside ptr<ret(args)> without changing grammar
        if (std.mem.indexOfScalar(u8, inner_type_name, '(')) |lp| if (std.mem.lastIndexOfScalar(u8, inner_type_name, ')')) |rp| if (rp > lp) {
            const ret_part = std.mem.trim(u8, inner_type_name[0..lp], " \t");
            const args_part_full = inner_type_name[lp + 1 .. rp];
            var args_list = std.ArrayList(c.LLVMTypeRef){};
            defer args_list.deinit(self.allocator);
            var it = std.mem.tokenizeAny(u8, args_part_full, ",");
            while (it.next()) |arg_raw| {
                const arg_trim = std.mem.trim(u8, arg_raw, " \t");
                if (arg_trim.len == 0) continue;
                const arg_ty = self.getLLVMType(arg_trim);
                args_list.append(self.allocator, arg_ty) catch unreachable;
            }
            const ret_ty = self.getLLVMType(ret_part);
            const fn_ty = if (args_list.items.len > 0)
                c.LLVMFunctionType(ret_ty, args_list.items.ptr, @intCast(args_list.items.len), 0)
            else
                c.LLVMFunctionType(ret_ty, null, 0, 0);
            return c.LLVMPointerType(fn_ty, 0);
        };
        const inner_type = self.getLLVMType(inner_type_name);
        return c.LLVMPointerType(inner_type, 0);
    } else if (std.mem.startsWith(u8, type_name, "arr<") and std.mem.endsWith(u8, type_name, ">")) {
        const inner = type_name[4 .. type_name.len - 1];
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

            if (std.fmt.parseInt(u32, size_str, 10)) |array_size| {
                const element_type = self.getLLVMType(element_type_name);
                return c.LLVMArrayType(element_type, array_size);
            } else |_| {}
        } else {}
        return c.LLVMInt32TypeInContext(@ptrCast(self.context));
    } else if (std.mem.startsWith(u8, type_name, "simd<") and std.mem.endsWith(u8, type_name, ">")) {
        const inner = type_name[5 .. type_name.len - 1];
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

            if (std.fmt.parseInt(u32, size_str, 10)) |vector_size| {
                const element_type = self.getLLVMType(element_type_name);
                return c.LLVMVectorType(element_type, vector_size);
            } else |_| {}
        } else {}
        return c.LLVMInt32TypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "i8")) {
        return c.LLVMInt8TypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "i16")) {
        return c.LLVMInt16TypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "i32")) {
        return c.LLVMInt32TypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "i64")) {
        return c.LLVMInt64TypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "u8")) {
        return c.LLVMInt8TypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "u16")) {
        return c.LLVMInt16TypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "u32")) {
        return c.LLVMInt32TypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "u64")) {
        return c.LLVMInt64TypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "f16")) {
        return c.LLVMHalfTypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "f32")) {
        return c.LLVMFloatTypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "f64")) {
        return c.LLVMDoubleTypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "void")) {
        return c.LLVMVoidTypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "bool")) {
        return c.LLVMInt1TypeInContext(@ptrCast(self.context));
    } else {
        if (self.struct_types.get(type_name)) |struct_type| {
            return @ptrCast(struct_type);
        }
    }
    return c.LLVMInt32TypeInContext(@ptrCast(self.context));
}

pub fn isUnsignedType(type_name: []const u8) bool {
    return std.mem.startsWith(u8, type_name, "u");
}

pub fn getAlignmentForType(self: *codegen.CodeGenerator, llvm_type: c.LLVMTypeRef) c_uint {
    const type_kind = c.LLVMGetTypeKind(llvm_type);
    return switch (type_kind) {
        c.LLVMIntegerTypeKind => {
            const bit_width = c.LLVMGetIntTypeWidth(llvm_type);
            if (bit_width <= 8) return 1;
            if (bit_width <= 16) return 2;
            if (bit_width <= 32) return 4;
            if (bit_width <= 64) return 8;
            return 8;
        },
        c.LLVMFloatTypeKind => 4,
        c.LLVMDoubleTypeKind => 8,
        c.LLVMHalfTypeKind => 2,
        c.LLVMPointerTypeKind => 8,
        c.LLVMArrayTypeKind => self.getAlignmentForType(c.LLVMGetElementType(llvm_type)),
        c.LLVMStructTypeKind => 8,
        else => 4,
    };
}

pub fn getTypeNameFromLLVMType(self: *codegen.CodeGenerator, llvm_type: c.LLVMTypeRef) []const u8 {
    _ = self;
    const type_kind = c.LLVMGetTypeKind(llvm_type);

    return switch (type_kind) {
        c.LLVMVoidTypeKind => "void",
        c.LLVMIntegerTypeKind => {
            const width = c.LLVMGetIntTypeWidth(llvm_type);
            if (width == 1) {
                return "bool";
            } else {
                return switch (width) {
                    8 => "i8",
                    16 => "i16",
                    32 => "i32",
                    64 => "i64",
                    else => "i32",
                };
            }
        },
        c.LLVMFloatTypeKind => "f32",
        c.LLVMDoubleTypeKind => "f64",
        c.LLVMHalfTypeKind => "f16",
        c.LLVMPointerTypeKind => "ptr",
        c.LLVMArrayTypeKind => "array",
        c.LLVMVectorTypeKind => "simd",
        c.LLVMStructTypeKind => "struct",
        else => "unknown",
    };
}

pub fn getStructSizeBytes(_: *codegen.CodeGenerator, struct_type: c.LLVMTypeRef) u64 {
    const target_data = c.LLVMCreateTargetData("e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128");
    defer c.LLVMDisposeTargetData(target_data);
    return c.LLVMABISizeOfType(target_data, struct_type);
}

pub fn shouldUseByVal(cg: *codegen.CodeGenerator, struct_type: c.LLVMTypeRef) bool {
    return getStructSizeBytes(cg, struct_type) > 16;
}

pub fn shouldSplitAsVector(cg: *codegen.CodeGenerator, struct_type: c.LLVMTypeRef) bool {
    const size = getStructSizeBytes(cg, struct_type);
    if (size != 12) return false;
    const num_elements = c.LLVMCountStructElementTypes(struct_type);
    if (num_elements != 3) return false;
    var element_types: [3]c.LLVMTypeRef = undefined;
    c.LLVMGetStructElementTypes(struct_type, &element_types);
    for (element_types) |elem_type| {
        if (c.LLVMGetTypeKind(elem_type) != c.LLVMFloatTypeKind) {
            return false;
        }
    }

    return true;
}
