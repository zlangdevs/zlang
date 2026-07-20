const codegen = @import("llvm.zig");
const std = @import("std");
const builtin = @import("builtin");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const c_abi = @import("../c_abi.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

// The C ABI's `va_list` is not one universal type: on some targets it is a
// small struct that must be passed *by address*, on others it is already a
// bare pointer that must be passed *by value*. We group targets into the
// families below so the compiler can lower `@va_start`/`va_list` argument
// passing correctly no matter what machine is compiling the program.
pub const VaListFamily = enum {
    // System V x86-64: va_list is `struct { u32, u32, void*, void* }[1]`,
    // an array type, so C callers always pass the address of the struct.
    x86_64,
    // AAPCS64 (aarch64 Linux): va_list is a 5-field struct; large aggregates
    // are passed indirectly under this ABI, so callers likewise pass the
    // address of the struct.
    aarch64,
    // Every other target we support (RISC-V, and the sane default for
    // anything unrecognized): va_list is simply `void*` — a pointer to the
    // next argument. That pointer itself is the value passed by callers,
    // not the address of wherever we happened to store it.
    generic_ptr,
};

pub fn classifyVaListFamily(arch: []const u8) VaListFamily {
    if (arch.len == 0) {
        return switch (builtin.cpu.arch) {
            .x86_64 => .x86_64,
            .aarch64, .aarch64_be => .aarch64,
            else => .generic_ptr,
        };
    }
    if (std.mem.indexOf(u8, arch, "x86_64") != null) return .x86_64;
    if (std.mem.indexOf(u8, arch, "aarch64") != null or std.mem.indexOf(u8, arch, "arm64") != null) return .aarch64;
    return .generic_ptr;
}

const VaListFieldSpec = struct { name: []const u8, type_name: []const u8 };

fn registerBuiltinVaListDecl(self: *codegen.CodeGenerator, field_specs: []const VaListFieldSpec) void {
    var fields: std.ArrayList(ast.StructField) = .empty;
    for (field_specs) |spec| {
        fields.append(self.allocator, ast.StructField{
            .name = spec.name,
            .type_name = spec.type_name,
            .default_value = null,
        }) catch return;
    }
    self.struct_declarations.put("va_list", ast.StructDecl{
        .name = "va_list",
        .fields = fields,
        .is_union = false,
    }) catch {};
}

// `va_list` is a compiler builtin, not a user struct: its layout must match
// whatever the real C library on the *target* expects (see VaListFamily),
// which a hand-written `.zl` struct declaration can't express since the
// language has no way to branch stdlib source on target architecture.
fn getBuiltinVaListType(self: *codegen.CodeGenerator) c.LLVMTypeRef {
    if (self.struct_types.get("va_list")) |cached| return @ptrCast(cached);

    const i8_ptr_ty = c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0);
    const ty: c.LLVMTypeRef = switch (self.va_list_family) {
        .x86_64 => blk: {
            const i32_ty = c.LLVMInt32TypeInContext(self.context);
            const named = c.LLVMStructCreateNamed(self.context, "va_list");
            var elems = [_]c.LLVMTypeRef{ i32_ty, i32_ty, i8_ptr_ty, i8_ptr_ty };
            c.LLVMStructSetBody(named, &elems, 4, 0);
            registerBuiltinVaListDecl(self, &[_]VaListFieldSpec{
                .{ .name = "gp_offset", .type_name = "u32" },
                .{ .name = "fp_offset", .type_name = "u32" },
                .{ .name = "overflow_arg_area", .type_name = "ptr<void>" },
                .{ .name = "reg_save_area", .type_name = "ptr<void>" },
            });
            break :blk named;
        },
        .aarch64 => blk: {
            const i32_ty = c.LLVMInt32TypeInContext(self.context);
            const named = c.LLVMStructCreateNamed(self.context, "va_list");
            var elems = [_]c.LLVMTypeRef{ i8_ptr_ty, i8_ptr_ty, i8_ptr_ty, i32_ty, i32_ty };
            c.LLVMStructSetBody(named, &elems, 5, 0);
            registerBuiltinVaListDecl(self, &[_]VaListFieldSpec{
                .{ .name = "stack", .type_name = "ptr<void>" },
                .{ .name = "gr_top", .type_name = "ptr<void>" },
                .{ .name = "vr_top", .type_name = "ptr<void>" },
                .{ .name = "gr_offs", .type_name = "i32" },
                .{ .name = "vr_offs", .type_name = "i32" },
            });
            break :blk named;
        },
        .generic_ptr => i8_ptr_ty,
    };
    self.struct_types.put("va_list", @ptrCast(ty)) catch {};
    return ty;
}

fn fatalOom() noreturn {
    std.debug.print("error: zlang compiler ran out of memory\n", .{});
    std.process.exit(1);
}

pub fn alloc(comptime T: type, allocator: std.mem.Allocator, n: usize) []T {
    return allocator.alloc(T, n) catch fatalOom();
}

pub fn dupe(comptime T: type, allocator: std.mem.Allocator, slice: []const T) []T {
    return allocator.dupe(T, slice) catch fatalOom();
}

pub fn dupeZ(allocator: std.mem.Allocator, slice: []const u8) [:0]u8 {
    return allocator.dupeZ(u8, slice) catch fatalOom();
}

pub fn create(comptime T: type, allocator: std.mem.Allocator) *T {
    return allocator.create(T) catch fatalOom();
}

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
    .{ "calloc", LibcFunctionSignature{ .return_type = .char_ptr_type, .param_types = &[_]LibcType{ .size_t_type, .size_t_type } } },
    .{ "realloc", LibcFunctionSignature{ .return_type = .char_ptr_type, .param_types = &[_]LibcType{ .char_ptr_type, .size_t_type } } },
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
    .{ "memmove", LibcFunctionSignature{ .return_type = .char_ptr_type, .param_types = &[_]LibcType{ .char_ptr_type, .char_ptr_type, .size_t_type } } },
    .{ "memcmp", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{ .char_ptr_type, .char_ptr_type, .size_t_type } } },
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
                const elements = alloc(c.LLVMValueRef, cg.allocator, vector_size);
                defer cg.allocator.free(elements);
                for (elements) |*element| {
                    element.* = element_default;
                }

                return c.LLVMConstVector(elements.ptr, vector_size);
            }
        }
        return c.LLVMConstInt(c.LLVMInt32TypeInContext(@ptrCast(cg.context)), 0, 0);
    }
    if (getLLVMTypeSilent(cg, type_name)) |llvm_type| {
        return c.LLVMConstNull(llvm_type);
    } else |_| {
        return c.LLVMConstInt(c.LLVMInt32TypeInContext(@ptrCast(cg.context)), 0, 0);
    }
}

pub fn isConstPointer(type_name: []const u8) bool {
    if (!std.mem.startsWith(u8, type_name, "ptr<") or !std.mem.endsWith(u8, type_name, ">")) {
        return false;
    }
    const inner = type_name[4 .. type_name.len - 1];
    const trimmed = std.mem.trim(u8, inner, " \t");
    return std.mem.startsWith(u8, trimmed, "const ");
}

pub fn stripConst(type_name: []const u8) []const u8 {
    const trimmed = std.mem.trim(u8, type_name, " \t");
    if (std.mem.startsWith(u8, trimmed, "const ")) {
        return std.mem.trim(u8, trimmed[6..], " \t");
    }
    return type_name;
}

pub fn isVarArgType(type_name: []const u8) bool {
    const trimmed = std.mem.trim(u8, type_name, " \t");
    if (std.mem.eql(u8, trimmed, "vararg<_>")) return true;
    return std.mem.startsWith(u8, trimmed, "vararg<") and std.mem.endsWith(u8, trimmed, ">");
}

pub fn getVarArgType(type_name: []const u8) ?[]const u8 {
    const trimmed = std.mem.trim(u8, type_name, " \t");
    if (std.mem.eql(u8, trimmed, "vararg<_>")) return null;
    if (std.mem.startsWith(u8, trimmed, "vararg<") and std.mem.endsWith(u8, trimmed, ">")) {
        return trimmed[7 .. trimmed.len - 1];
    }
    return null;
}

pub fn getLLVMType(self: *codegen.CodeGenerator, type_name: []const u8) errors.CodegenError!c.LLVMTypeRef {
    return getLLVMTypeInternal(self, type_name, true);
}

pub fn getLLVMTypeSilent(self: *codegen.CodeGenerator, type_name: []const u8) errors.CodegenError!c.LLVMTypeRef {
    return getLLVMTypeInternal(self, type_name, false);
}

pub fn getLLVMFunctionType(self: *codegen.CodeGenerator, sig: []const u8) errors.CodegenError!c.LLVMTypeRef {
    const lp = std.mem.indexOfScalar(u8, sig, '(') orelse return errors.CodegenError.TypeMismatch;
    const rp = std.mem.lastIndexOfScalar(u8, sig, ')') orelse return errors.CodegenError.TypeMismatch;

    const ret_part = std.mem.trim(u8, sig[0..lp], " \t");
    const args_part_full = sig[lp + 1 .. rp];
    var args_list: std.ArrayList(c.LLVMTypeRef) = .empty;
    defer args_list.deinit(self.allocator);
    var it = std.mem.tokenizeAny(u8, args_part_full, ",");
    var is_vararg_llvm: c.LLVMBool = 0;
    while (it.next()) |arg_raw| {
        const arg_trim = std.mem.trim(u8, arg_raw, " \t");
        if (arg_trim.len == 0) continue;
        if (isVarArgType(arg_trim)) {
            if (getVarArgType(arg_trim)) |et| {
                const elem_ty = try self.getLLVMType(et);
                try args_list.append(self.allocator, c.LLVMPointerType(elem_ty, 0));
                try args_list.append(self.allocator, c.LLVMInt32TypeInContext(self.context));
            } else {
                is_vararg_llvm = 1;
            }
            continue;
        }
        if (getLLVMTypeSilent(self, arg_trim)) |arg_ty| {
            if (c.LLVMGetTypeKind(@ptrCast(arg_ty)) == c.LLVMStructTypeKind and shouldUseByVal(self, @ptrCast(arg_ty))) {
                try args_list.append(self.allocator, c.LLVMPointerType(@ptrCast(arg_ty), 0));
            } else {
                try args_list.append(self.allocator, arg_ty);
            }
        } else |_| {
            const void_ptr_type = c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0);
            try args_list.append(self.allocator, void_ptr_type);
        }
    }
    const ret_ty = try self.getLLVMType(ret_part);
    return if (args_list.items.len > 0)
        c.LLVMFunctionType(ret_ty, args_list.items.ptr, @intCast(args_list.items.len), is_vararg_llvm)
    else
        c.LLVMFunctionType(ret_ty, null, 0, is_vararg_llvm);
}

fn getLLVMTypeInternal(self: *codegen.CodeGenerator, type_name: []const u8, verbose: bool) errors.CodegenError!c.LLVMTypeRef {
    if (self.template_substitutions) |subs| {
        if (subs.get(type_name)) |substituted_name| {
            return getLLVMTypeInternal(self, substituted_name, verbose);
        }
    }

    if (std.mem.startsWith(u8, type_name, "ptr<") and std.mem.endsWith(u8, type_name, ">")) {
        var inner_type_name = type_name[4 .. type_name.len - 1];

        inner_type_name = stripConst(inner_type_name);

        if (std.mem.indexOfScalar(u8, inner_type_name, '(')) |lp| {
            if (std.mem.lastIndexOfScalar(u8, inner_type_name, ')')) |rp| {
                if (rp > lp) {
                    const fn_ty = try getLLVMFunctionType(self, inner_type_name);
                    return c.LLVMPointerType(fn_ty, 0);
                }
            }
        }
        const inner_type = try getLLVMTypeInternal(self, inner_type_name, verbose);
        return c.LLVMPointerType(inner_type, 0);
    } else if (std.mem.indexOfScalar(u8, type_name, '(')) |lp| blk: {
        if (std.mem.lastIndexOfScalar(u8, type_name, ')')) |rp| {
            if (rp > lp) {
                return try getLLVMFunctionType(self, type_name);
            }
        }
        break :blk;
    } else if (std.mem.startsWith(u8, type_name, "[]")) {
        const element_type_name = type_name[2..];

        // Check if slice struct already exists
        if (self.struct_types.get(type_name)) |struct_type| {
            return @ptrCast(struct_type);
        }

        const type_name_dupe = dupe(u8, self.allocator, type_name);

        // Create new slice struct
        const element_type = try getLLVMTypeInternal(self, element_type_name, verbose);
        const struct_type = c.LLVMStructCreateNamed(self.context, type_name_dupe.ptr);

        // Struct body: { T*, i64 (len) }
        const ptr_type = c.LLVMPointerType(element_type, 0);
        const len_type = c.LLVMInt64TypeInContext(self.context);
        var element_types = [_]c.LLVMTypeRef{ ptr_type, len_type };
        c.LLVMStructSetBody(struct_type, &element_types[0], 2, 0);

        try self.struct_types.put(type_name_dupe, @ptrCast(struct_type));

        // Register dummy declaration so field access works
        var fields: std.ArrayList(ast.StructField) = .empty;
        const ptr_type_name = try std.fmt.allocPrint(self.allocator, "ptr<{s}>", .{element_type_name});
        try fields.append(self.allocator, ast.StructField{ .name = "ptr", .type_name = ptr_type_name, .default_value = null });
        try fields.append(self.allocator, ast.StructField{ .name = "len", .type_name = "i64", .default_value = null });

        const decl = ast.StructDecl{
            .name = type_name_dupe,
            .fields = fields,
            .is_union = false,
        };
        try self.struct_declarations.put(type_name_dupe, decl);

        // Register fields in struct_fields map for O(1) lookup
        var field_map = std.HashMap([]const u8, c_uint, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(self.allocator);
        try field_map.put("ptr", 0);
        try field_map.put("len", 1);
        try self.struct_fields.put(type_name_dupe, field_map);

        return struct_type;
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
                const element_type = try getLLVMTypeInternal(self, element_type_name, verbose);
                return c.LLVMArrayType(element_type, array_size);
            } else |_| {}
        } else {}
        if (verbose) {
            self.reportErrorFmt("Invalid array type syntax: {s}", .{type_name}, "Expected format: arr<element_type, size>");
        }
        return error.UnknownType;
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
                const element_type = try getLLVMTypeInternal(self, element_type_name, verbose);
                return c.LLVMVectorType(element_type, vector_size);
            } else |_| {}
        } else {}
        if (verbose) {
            self.reportErrorFmt("Invalid SIMD type syntax: {s}", .{type_name}, "Expected format: simd<element_type, width>");
        }
        return error.UnknownType;
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
    } else if (std.mem.eql(u8, type_name, "error")) {
        return c.LLVMInt32TypeInContext(@ptrCast(self.context));
    } else if (std.mem.eql(u8, type_name, "va_list")) {
        return getBuiltinVaListType(self);
    } else {
        if (self.struct_types.get(type_name)) |struct_type| {
            return @ptrCast(struct_type);
        }
    }
    if (verbose) {
        self.reportErrorFmt("Unknown type '{s}'", .{type_name}, "Check for typos or missing declarations/imports");
    }
    return error.UnknownType;
}

pub fn isUnsignedType(type_name: []const u8) bool {
    return std.mem.startsWith(u8, type_name, "u") or std.mem.eql(u8, type_name, "bool");
}

pub fn isIntPrimitive(type_name: []const u8) bool {
    const ints = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64" };
    for (ints) |t| {
        if (std.mem.eql(u8, t, type_name)) return true;
    }
    return false;
}

pub fn isFloatType(type_name: []const u8) bool {
    return std.mem.eql(u8, type_name, "f16") or
        std.mem.eql(u8, type_name, "f32") or
        std.mem.eql(u8, type_name, "f64");
}

pub fn getIntWidth(type_name: []const u8) u32 {
    if (std.mem.eql(u8, type_name, "bool")) return 1;
    if (type_name.len < 2) return 0;

    const width_str = type_name[1..];
    return std.fmt.parseInt(u32, width_str, 10) catch 0;
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
                    else => std.fmt.allocPrint(self.allocator, "i{d}", .{width}) catch "i32",
                };
            }
        },
        c.LLVMFloatTypeKind => "f32",
        c.LLVMDoubleTypeKind => "f64",
        c.LLVMHalfTypeKind => "f16",
        c.LLVMPointerTypeKind => "ptr",
        c.LLVMArrayTypeKind => {
            const element_type = c.LLVMGetElementType(llvm_type);
            const array_len = c.LLVMGetArrayLength(llvm_type);
            const element_type_str = getTypeNameFromLLVMType(self, element_type);
            return std.fmt.allocPrint(self.allocator, "arr<{s},{d}>", .{ element_type_str, array_len }) catch "array";
        },
        c.LLVMVectorTypeKind => {
            const element_type = c.LLVMGetElementType(llvm_type);
            const vector_size = c.LLVMGetVectorSize(llvm_type);
            const element_type_str = getTypeNameFromLLVMType(self, element_type);
            return std.fmt.allocPrint(self.allocator, "simd<{s},{d}>", .{ element_type_str, vector_size }) catch "simd";
        },
        c.LLVMStructTypeKind => {
            const name_ptr = c.LLVMGetStructName(llvm_type);
            if (name_ptr != null) {
                return std.mem.sliceTo(name_ptr, 0);
            }
            // Fallback to manual lookup if unnamed
            var it = self.struct_types.iterator();
            while (it.next()) |entry| {
                if (@intFromPtr(entry.value_ptr.*) == @intFromPtr(llvm_type)) {
                    return entry.key_ptr.*;
                }
            }
            return "struct";
        },
        else => "unknown",
    };
}

pub fn getStructSizeBytes(cg: *codegen.CodeGenerator, struct_type: c.LLVMTypeRef) u64 {
    return c_abi.getStructSizeBytesForModule(cg.module, struct_type);
}

pub fn shouldUseByVal(cg: *codegen.CodeGenerator, struct_type: c.LLVMTypeRef) bool {
    return getStructSizeBytes(cg, struct_type) > 16;
}

pub fn shouldAttachByValAttr(cg: *codegen.CodeGenerator, struct_type: c.LLVMTypeRef) bool {
    return getStructSizeBytes(cg, struct_type) <= 512;
}

pub fn isByValType(cg: *codegen.CodeGenerator, type_name: []const u8) bool {
    const ty = getLLVMTypeSilent(cg, type_name) catch return false;
    return c.LLVMGetTypeKind(@ptrCast(ty)) == c.LLVMStructTypeKind and shouldUseByVal(cg, @ptrCast(ty));
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
