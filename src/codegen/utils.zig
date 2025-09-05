const codegen = @import("llvm.zig");
const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");

const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/IRReader.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/ExecutionEngine.h");
});

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
        .void_type => c.LLVMVoidTypeInContext(cg.context),
        .int_type => c.LLVMInt32TypeInContext(cg.context),
        .char_ptr_type => c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0),
        .size_t_type => c.LLVMInt64TypeInContext(cg.context),
        .file_ptr_type => c.LLVMPointerType(c.LLVMInt8TypeInContext(cg.context), 0),
        .long_type => c.LLVMInt64TypeInContext(cg.context),
        .double_type => c.LLVMDoubleTypeInContext(cg.context),
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
        return c.LLVMConstInt(c.LLVMInt8TypeInContext(cg.context), 0, 0);
    } else if (std.mem.eql(u8, type_name, "i16")) {
        return c.LLVMConstInt(c.LLVMInt16TypeInContext(cg.context), 0, 0);
    } else if (std.mem.eql(u8, type_name, "i32")) {
        return c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0);
    } else if (std.mem.eql(u8, type_name, "i64")) {
        return c.LLVMConstInt(c.LLVMInt64TypeInContext(cg.context), 0, 0);
    } else if (std.mem.eql(u8, type_name, "u8")) {
        return c.LLVMConstInt(c.LLVMInt8TypeInContext(cg.context), 0, 0);
    } else if (std.mem.eql(u8, type_name, "u16")) {
        return c.LLVMConstInt(c.LLVMInt16TypeInContext(cg.context), 0, 0);
    } else if (std.mem.eql(u8, type_name, "u32")) {
        return c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0);
    } else if (std.mem.eql(u8, type_name, "u64")) {
        return c.LLVMConstInt(c.LLVMInt64TypeInContext(cg.context), 0, 0);
    } else if (std.mem.eql(u8, type_name, "f16")) {
        return c.LLVMConstReal(c.LLVMHalfTypeInContext(cg.context), 0.0);
    } else if (std.mem.eql(u8, type_name, "f32")) {
        return c.LLVMConstReal(c.LLVMFloatTypeInContext(cg.context), 0.0);
    } else if (std.mem.eql(u8, type_name, "f64")) {
        return c.LLVMConstReal(c.LLVMDoubleTypeInContext(cg.context), 0.0);
    } else if (std.mem.eql(u8, type_name, "bool")) {
        return c.LLVMConstInt(c.LLVMInt1TypeInContext(cg.context), 0, 0);
    }
    return c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0);
}

pub fn getLLVMType(self: *codegen.CodeGenerator, type_name: []const u8) c.LLVMTypeRef {
    if (std.mem.startsWith(u8, type_name, "ptr<") and std.mem.endsWith(u8, type_name, ">")) {
        const inner_type_name = type_name[4 .. type_name.len - 1];
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
        return c.LLVMInt32TypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "i8")) {
        return c.LLVMInt8TypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "i16")) {
        return c.LLVMInt16TypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "i32")) {
        return c.LLVMInt32TypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "i64")) {
        return c.LLVMInt64TypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "u8")) {
        return c.LLVMInt8TypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "u16")) {
        return c.LLVMInt16TypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "u32")) {
        return c.LLVMInt32TypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "u64")) {
        return c.LLVMInt64TypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "f16")) {
        return c.LLVMHalfTypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "f32")) {
        return c.LLVMFloatTypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "f64")) {
        return c.LLVMDoubleTypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "void")) {
        return c.LLVMVoidTypeInContext(self.context);
    } else if (std.mem.eql(u8, type_name, "bool")) {
        return c.LLVMInt1TypeInContext(self.context);
    } else {
        if (self.struct_types.get(type_name)) |struct_type| {
            return struct_type;
        }
    }
    return c.LLVMInt32TypeInContext(self.context);
}
