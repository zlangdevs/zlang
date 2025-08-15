const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const bfck = @import("bf.zig");

const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/IRReader.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/ExecutionEngine.h");
});

const LibcFunctionSignature = struct {
    return_type: LibcType,
    param_types: []const LibcType,
    is_varargs: bool = false,
};

const LibcType = enum {
    void_type,
    int_type,
    char_ptr_type,
    size_t_type,
    file_ptr_type,
    long_type,
    double_type,
};

const LIBC_FUNCTIONS = std.StaticStringMap(LibcFunctionSignature).initComptime(.{
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
});

pub const CodeGenerator = struct {
    context: c.LLVMContextRef,
    module: c.LLVMModuleRef,
    builder: c.LLVMBuilderRef,
    allocator: std.mem.Allocator,
    functions: std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    variables: std.HashMap([]const u8, VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    current_function: ?c.LLVMValueRef,

    const VariableInfo = struct {
        value: c.LLVMValueRef,
        type_ref: c.LLVMTypeRef,
    };

    pub fn init(allocator: std.mem.Allocator) errors.CodegenError!CodeGenerator {
        _ = c.LLVMInitializeNativeTarget();
        _ = c.LLVMInitializeNativeAsmPrinter();
        _ = c.LLVMInitializeNativeAsmParser();

        const context = c.LLVMContextCreate();
        if (context == null) return errors.CodegenError.ModuleCreationFailed;

        const module = c.LLVMModuleCreateWithNameInContext("zlang_module", context);
        if (module == null) return errors.CodegenError.ModuleCreationFailed;

        const builder = c.LLVMCreateBuilderInContext(context);
        if (builder == null) return errors.CodegenError.BuilderCreationFailed;

        return CodeGenerator{
            .context = context,
            .module = module,
            .builder = builder,
            .allocator = allocator,
            .functions = std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .variables = std.HashMap([]const u8, VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .current_function = null,
        };
    }

    pub fn deinit(self: *CodeGenerator) void {
        self.functions.deinit();
        self.variables.deinit();
        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.context);
    }

    fn getLLVMType(self: *CodeGenerator, type_name: []const u8) c.LLVMTypeRef {
        if (std.mem.eql(u8, type_name, "i8")) {
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
        }
        return c.LLVMInt32TypeInContext(self.context);
    }

    fn libcTypeToLLVM(self: *CodeGenerator, libc_type: LibcType) c.LLVMTypeRef {
        return switch (libc_type) {
            .void_type => c.LLVMVoidTypeInContext(self.context),
            .int_type => c.LLVMInt32TypeInContext(self.context),
            .char_ptr_type => c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0),
            .size_t_type => c.LLVMInt64TypeInContext(self.context),
            .file_ptr_type => c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0),
            .long_type => c.LLVMInt64TypeInContext(self.context),
            .double_type => c.LLVMDoubleTypeInContext(self.context),
        };
    }

    fn declareLibcFunction(self: *CodeGenerator, func_name: []const u8) !c.LLVMValueRef {
        if (self.functions.get(func_name)) |existing| {
            return existing;
        }
        if (LIBC_FUNCTIONS.get(func_name)) |signature| {
            return self.createFunctionFromSignature(func_name, signature);
        }
        return self.createGenericLibcFunction(func_name);
    }

    fn createFunctionFromSignature(self: *CodeGenerator, func_name: []const u8, signature: LibcFunctionSignature) !c.LLVMValueRef {
        const return_type = self.libcTypeToLLVM(signature.return_type);

        var param_types = std.ArrayList(c.LLVMTypeRef).init(self.allocator);
        defer param_types.deinit();

        for (signature.param_types) |param_type| {
            try param_types.append(self.libcTypeToLLVM(param_type));
        }

        const function_type = if (param_types.items.len > 0)
            c.LLVMFunctionType(return_type, param_types.items.ptr, @intCast(param_types.items.len), if (signature.is_varargs) 1 else 0)
        else
            c.LLVMFunctionType(return_type, null, 0, if (signature.is_varargs) 1 else 0);

        const func_name_z = try self.allocator.dupeZ(u8, func_name);
        defer self.allocator.free(func_name_z);

        const llvm_func = c.LLVMAddFunction(self.module, func_name_z.ptr, function_type);
        try self.functions.put(try self.allocator.dupe(u8, func_name), llvm_func);

        return llvm_func;
    }

    fn createGenericLibcFunction(self: *CodeGenerator, func_name: []const u8) !c.LLVMValueRef {
        const i8_ptr_type = c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0);
        var generic_args = [_]c.LLVMTypeRef{i8_ptr_type};
        const function_type = c.LLVMFunctionType(c.LLVMInt32TypeInContext(self.context), &generic_args[0], 1, 1);

        const func_name_z = try self.allocator.dupeZ(u8, func_name);
        defer self.allocator.free(func_name_z);

        const llvm_func = c.LLVMAddFunction(self.module, func_name_z.ptr, function_type);
        try self.functions.put(try self.allocator.dupe(u8, func_name), llvm_func);

        std.debug.print("Warning: Creating generic signature for unknown libc function: {s}\n", .{func_name});

        return llvm_func;
    }

    pub fn generateCode(self: *CodeGenerator, program: *ast.Node) errors.CodegenError!void {
        switch (program.data) {
            .program => |prog| {
                for (prog.functions.items) |func| {
                    try self.generateFunction(func);
                }
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    fn generateFunction(self: *CodeGenerator, func_node: *ast.Node) errors.CodegenError!void {
        switch (func_node.data) {
            .function => |func| {
                const return_type = self.getLLVMType(func.return_type);
                const function_type = c.LLVMFunctionType(return_type, null, 0, 0);

                const func_name_z = self.allocator.dupeZ(u8, func.name) catch return errors.CodegenError.OutOfMemory;
                defer self.allocator.free(func_name_z);

                const llvm_func = c.LLVMAddFunction(self.module, func_name_z.ptr, function_type);
                try self.functions.put(func.name, llvm_func);

                self.current_function = llvm_func;

                const entry_block = c.LLVMAppendBasicBlockInContext(self.context, llvm_func, "entry");
                c.LLVMPositionBuilderAtEnd(self.builder, entry_block);

                self.variables.clearRetainingCapacity();

                for (func.body.items) |stmt| {
                    try self.generateStatement(stmt);
                }
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    fn generateStatement(self: *CodeGenerator, stmt: *ast.Node) errors.CodegenError!void {
        switch (stmt.data) {
            .assignment => |as| {
                const var_info = self.variables.get(as.name) orelse return errors.CodegenError.UndefinedVariable;
                const value = try self.generateExpression(as.value);
                const casted_value = self.castToType(value, var_info.type_ref);
                _ = c.LLVMBuildStore(self.builder, casted_value, var_info.value);
            },
            .var_decl => |decl| {
                if (self.variables.contains(decl.name)) {
                    std.debug.print("Error: variable '{s}' is already declared\n", .{decl.name});
                    return errors.CodegenError.RedeclaredVariable;
                }
                if (std.mem.eql(u8, decl.type_name, "void")) {
                    if (decl.initializer != null) {
                        std.debug.print("Error: void variables cannot be initialized with a value\n", .{});
                        return errors.CodegenError.TypeMismatch;
                    }
                    try self.variables.put(decl.name, VariableInfo{
                        .value = undefined,
                        .type_ref = c.LLVMVoidTypeInContext(self.context),
                    });
                    return;
                }

                const var_type = self.getLLVMType(decl.type_name);

                const alloca = c.LLVMBuildAlloca(self.builder, var_type, decl.name.ptr);

                try self.variables.put(decl.name, VariableInfo{
                    .value = alloca,
                    .type_ref = var_type,
                });

                if (decl.initializer) |initializer| {
                    const init_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                    const casted_value = self.castToType(init_value, var_type);
                    _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                }
            },
            .function_call => {
                _ = try self.generateExpression(stmt);
            },
            .return_stmt => |ret| {
                if (ret.expression) |expr| {
                    const ret_value = try self.generateExpression(expr);
                    _ = c.LLVMBuildRet(self.builder, ret_value);
                } else {
                    _ = c.LLVMBuildRetVoid(self.builder);
                }
            },
            .brainfuck => |bf| {
                _ = try self.generateBrainfuck(bf);
            },
            else => {},
        }
    }

    pub fn castToType(self: *CodeGenerator, value: c.LLVMValueRef, target_type: c.LLVMTypeRef) c.LLVMValueRef {
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
                return c.LLVMBuildTrunc(self.builder, value, target_type, "trunc");
            } else if (value_width < target_width) {
                if (value_width == 1) {
                    return c.LLVMBuildZExt(self.builder, value, target_type, "zext");
                } else {
                    return c.LLVMBuildSExt(self.builder, value, target_type, "sext");
                }
            }
        } else if ((value_kind == c.LLVMFloatTypeKind and (target_kind == c.LLVMDoubleTypeKind or target_kind == c.LLVMHalfTypeKind)) or
                   (value_kind == c.LLVMDoubleTypeKind and (target_kind == c.LLVMFloatTypeKind or target_kind == c.LLVMHalfTypeKind)) or
                   (value_kind == c.LLVMHalfTypeKind and (target_kind == c.LLVMFloatTypeKind or target_kind == c.LLVMDoubleTypeKind))) {
            if ((value_kind == c.LLVMFloatTypeKind or value_kind == c.LLVMHalfTypeKind) and
                (target_kind == c.LLVMDoubleTypeKind or (target_kind == c.LLVMFloatTypeKind and value_kind == c.LLVMHalfTypeKind))) {
                return c.LLVMBuildFPExt(self.builder, value, target_type, "fpext");
            } else {
                return c.LLVMBuildFPTrunc(self.builder, value, target_type, "fptrunc");
            }
        } else if ((value_kind == c.LLVMIntegerTypeKind and
                   (target_kind == c.LLVMFloatTypeKind or target_kind == c.LLVMDoubleTypeKind or target_kind == c.LLVMHalfTypeKind))) {
            return c.LLVMBuildSIToFP(self.builder, value, target_type, "sitofp");
        } else if ((value_kind == c.LLVMFloatTypeKind or value_kind == c.LLVMDoubleTypeKind or value_kind == c.LLVMHalfTypeKind) and
                   target_kind == c.LLVMIntegerTypeKind) {
            return c.LLVMBuildFPToSI(self.builder, value, target_type, "fptosi");
        }
    
        return value;
    }

    pub fn parse_escape(self: *CodeGenerator, str: []const u8) errors.CodegenError![]const u8 {
        var transformed_string = std.ArrayList(u8).init(self.allocator);
        var i: usize = 0;
        while (i < str.len) : (i += 1) {
            if (str[i] == '\\') {
                i += 1;
                if (i < str.len) {
                    switch (str[i]) {
                        'n' => transformed_string.append('\n') catch return errors.CodegenError.OutOfMemory,
                        't' => transformed_string.append('\t') catch return errors.CodegenError.OutOfMemory,
                        'r' => transformed_string.append('\r') catch return errors.CodegenError.OutOfMemory,
                        '\'' => transformed_string.append('\'') catch return errors.CodegenError.OutOfMemory,
                        '\"' => transformed_string.append('\"') catch return errors.CodegenError.OutOfMemory,
                        else => {
                            transformed_string.append('\\') catch return errors.CodegenError.OutOfMemory;
                            transformed_string.append(str[i]) catch return errors.CodegenError.OutOfMemory;
                        },
                    }
                } else {
                    transformed_string.append('\\') catch return errors.CodegenError.OutOfMemory;
                }
            } else {
                transformed_string.append(str[i]) catch return errors.CodegenError.OutOfMemory;
            }
        }
        try transformed_string.append(0);
        return transformed_string.toOwnedSlice();
    }

    fn prepareArgumentForLibcCall(self: *CodeGenerator, arg_value: c.LLVMValueRef, func_name: []const u8, arg_index: usize) c.LLVMValueRef {
        if (std.mem.eql(u8, func_name, "printf") or
            std.mem.eql(u8, func_name, "sprintf") or
            std.mem.eql(u8, func_name, "snprintf") or
            std.mem.eql(u8, func_name, "fprintf"))
        {
            if (arg_index > 0) {
                const arg_type = c.LLVMTypeOf(arg_value);
                const arg_kind = c.LLVMGetTypeKind(arg_type);

                if (arg_kind == c.LLVMIntegerTypeKind) {
                    const i32_type = c.LLVMInt32TypeInContext(self.context);
                    return self.castToType(arg_value, i32_type);
                } else if (arg_kind == c.LLVMFloatTypeKind or arg_kind == c.LLVMHalfTypeKind) {
                    const double_type = c.LLVMDoubleTypeInContext(self.context);
                    return c.LLVMBuildFPExt(self.builder, arg_value, double_type, "fpext");
                }
            }
        }

        return arg_value;
    }

    fn generateExpressionWithContext(self: *CodeGenerator, expr: *ast.Node, expected_type: ?[]const u8) errors.CodegenError!c.LLVMValueRef {
        switch (expr.data) {
            .float_literal => |float| {
                const float_val = std.fmt.parseFloat(f64, float.value) catch 0.0;

                if (expected_type) |type_name| {
                    if (std.mem.eql(u8, type_name, "f16")) {
                        return c.LLVMConstReal(c.LLVMHalfTypeInContext(self.context), float_val);
                    } else if (std.mem.eql(u8, type_name, "f32")) {
                        return c.LLVMConstReal(c.LLVMFloatTypeInContext(self.context), float_val);
                    } else if (std.mem.eql(u8, type_name, "f64")) {
                        return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), float_val);
                    }
                }

                return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), float_val);
            },
            .number_literal => |num| {
                if (std.mem.indexOf(u8, num.value, ".") != null) {
                    const float_val = std.fmt.parseFloat(f64, num.value) catch 0.0;

                    if (expected_type) |type_name| {
                        if (std.mem.eql(u8, type_name, "f16")) {
                            return c.LLVMConstReal(c.LLVMHalfTypeInContext(self.context), float_val);
                        } else if (std.mem.eql(u8, type_name, "f32")) {
                            return c.LLVMConstReal(c.LLVMFloatTypeInContext(self.context), float_val);
                        } else if (std.mem.eql(u8, type_name, "f64")) {
                            return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), float_val);
                        }
                    }

                    return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), float_val);
                } else {
                    const value = std.fmt.parseInt(i32, num.value, 10) catch 0;
                    return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(value)), 0);
                }
            },
            .identifier => |ident| {
                if (std.mem.eql(u8, ident.name, "true")) {
                    return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 1, 0);
                } else if (std.mem.eql(u8, ident.name, "false")) {
                    return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 0, 0);
                }
                if (self.variables.get(ident.name)) |var_info| {
                    if (c.LLVMGetTypeKind(var_info.type_ref) == c.LLVMVoidTypeKind) {
                        std.debug.print("Error: cannot use void variable '{s}' as a value\n", .{ident.name});
                        return errors.CodegenError.TypeMismatch;
                    }
                    return c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load");
                }
                return errors.CodegenError.UndefinedVariable;
            },
            else => return self.generateExpression(expr),
        }
    }

    fn generateExpression(self: *CodeGenerator, expr: *ast.Node) errors.CodegenError!c.LLVMValueRef {
        switch (expr.data) {
            .identifier => |ident| {
                if (std.mem.eql(u8, ident.name, "true")) {
                    return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 1, 0);
                } else if (std.mem.eql(u8, ident.name, "false")) {
                    return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 0, 0);
                }
                
                if (self.variables.get(ident.name)) |var_info| {
                    return c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load");
                }
                return errors.CodegenError.UndefinedVariable;
            },
            .float_literal => |float| {
                const float_val = std.fmt.parseFloat(f64, float.value) catch 0.0;
                return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), float_val);
            },
            .number_literal => |num| {
                if (std.mem.indexOf(u8, num.value, ".") != null) {
                    const float_val = std.fmt.parseFloat(f64, num.value) catch 0.0;
                    return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), float_val);
                } else {
                    const value = std.fmt.parseInt(i32, num.value, 10) catch 0;
                    return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(value)), 0);
                }
            },
            .bool_literal => |bool_val| {
                return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), if (bool_val.value) 1 else 0, 0);
            },
            .string_literal => |str| {
                const parsed_str = try self.parse_escape(str.value);
                defer self.allocator.free(parsed_str);

                return c.LLVMBuildGlobalStringPtr(
                    self.builder,
                    parsed_str.ptr,
                    "str",
                );
            },
            .binary_op => |b| {
                return self.generateBinaryOp(b);
            },
            .function_call => |call| {
                const func = if (call.is_libc)
                    try self.declareLibcFunction(call.name)
                else
                    self.functions.get(call.name) orelse return errors.CodegenError.UndefinedFunction;

                var args = std.ArrayList(c.LLVMValueRef).init(self.allocator);
                defer args.deinit();

                for (call.args.items, 0..) |arg, i| {
                    var arg_value = try self.generateExpression(arg);

                    if (call.is_libc) {
                        arg_value = self.prepareArgumentForLibcCall(arg_value, call.name, i);
                    }

                    try args.append(arg_value);
                }

                const func_type = c.LLVMGlobalGetValueType(func);
                const return_type = c.LLVMGetReturnType(func_type);
                const is_void = c.LLVMGetTypeKind(return_type) == c.LLVMVoidTypeKind;

                const call_name = if (is_void) "" else call.name;
                const call_name_z = self.allocator.dupeZ(u8, call_name) catch return errors.CodegenError.OutOfMemory;
                defer self.allocator.free(call_name_z);

                if (args.items.len > 0) {
                    return c.LLVMBuildCall2(self.builder, func_type, func, args.items.ptr, @as(c_uint, @intCast(args.items.len)), call_name_z.ptr);
                } else {
                    return c.LLVMBuildCall2(self.builder, func_type, func, null, 0, call_name_z.ptr);
                }
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }
    
    fn generateBinaryOp(self: *CodeGenerator, binary_op: ast.BinaryOp) errors.CodegenError!c.LLVMValueRef {
        const lhs_value = try self.generateExpression(binary_op.lhs);
        const rhs_value = try self.generateExpression(binary_op.rhs);
    
        const lhs_type = c.LLVMTypeOf(lhs_value);
        const rhs_type = c.LLVMTypeOf(rhs_value);
    
        const lhs_kind = c.LLVMGetTypeKind(lhs_type);
        const rhs_kind = c.LLVMGetTypeKind(rhs_type);
    
        if (lhs_kind == c.LLVMIntegerTypeKind and c.LLVMGetIntTypeWidth(lhs_type) == 1) {
            return errors.CodegenError.TypeMismatch;
        }
        if (rhs_kind == c.LLVMIntegerTypeKind and c.LLVMGetIntTypeWidth(rhs_type) == 1) {
            return errors.CodegenError.TypeMismatch;
        }
    
        const is_lhs_float = lhs_kind == c.LLVMFloatTypeKind or
                             lhs_kind == c.LLVMDoubleTypeKind or
                             lhs_kind == c.LLVMHalfTypeKind;
        const is_rhs_float = rhs_kind == c.LLVMFloatTypeKind or
                             rhs_kind == c.LLVMDoubleTypeKind or
                             rhs_kind == c.LLVMHalfTypeKind;
    
        const is_float_op = is_lhs_float or is_rhs_float;
        var result_type: c.LLVMTypeRef = undefined;
        if (is_float_op) {
            if (lhs_kind == c.LLVMDoubleTypeKind or rhs_kind == c.LLVMDoubleTypeKind) {
                result_type = c.LLVMDoubleTypeInContext(self.context);
            } else if (lhs_kind == c.LLVMFloatTypeKind or rhs_kind == c.LLVMFloatTypeKind) {
                result_type = c.LLVMFloatTypeInContext(self.context);
            } else {
                result_type = c.LLVMFloatTypeInContext(self.context);
            }
        } else {
            result_type = lhs_type;
        }
        
        var casted_lhs = lhs_value;
        var casted_rhs = rhs_value;
        
        if (is_float_op) {
            if (lhs_kind == c.LLVMIntegerTypeKind) {
                casted_lhs = c.LLVMBuildSIToFP(self.builder, lhs_value, result_type, "sitofp_lhs");
            } else if (lhs_kind == c.LLVMHalfTypeKind and result_type != lhs_type) {
                casted_lhs = c.LLVMBuildFPExt(self.builder, lhs_value, result_type, "fpext_lhs");
            } else if (lhs_kind == c.LLVMFloatTypeKind and c.LLVMGetTypeKind(result_type) == c.LLVMDoubleTypeKind) {
                casted_lhs = c.LLVMBuildFPExt(self.builder, lhs_value, result_type, "fpext_lhs");
            }
            if (rhs_kind == c.LLVMIntegerTypeKind) {
                casted_rhs = c.LLVMBuildSIToFP(self.builder, rhs_value, result_type, "sitofp_rhs");
            } else if (rhs_kind == c.LLVMHalfTypeKind and result_type != rhs_type) {
                casted_rhs = c.LLVMBuildFPExt(self.builder, rhs_value, result_type, "fpext_rhs");
            } else if (rhs_kind == c.LLVMFloatTypeKind and c.LLVMGetTypeKind(result_type) == c.LLVMDoubleTypeKind) {
                casted_rhs = c.LLVMBuildFPExt(self.builder, rhs_value, result_type, "fpext_rhs");
            }
        }
    
        return switch (binary_op.op) {
            '+' => if (is_float_op)
                c.LLVMBuildFAdd(self.builder, casted_lhs, casted_rhs, "fadd")
            else
                c.LLVMBuildAdd(self.builder, casted_lhs, casted_rhs, "add"),
            '-' => if (is_float_op)
                c.LLVMBuildFSub(self.builder, casted_lhs, casted_rhs, "fsub")
            else
                c.LLVMBuildSub(self.builder, casted_lhs, casted_rhs, "sub"),
            '*' => if (is_float_op)
                c.LLVMBuildFMul(self.builder, casted_lhs, casted_rhs, "fmul")
            else
                c.LLVMBuildMul(self.builder, casted_lhs, casted_rhs, "mul"),
            '/' => if (is_float_op)
                c.LLVMBuildFDiv(self.builder, casted_lhs, casted_rhs, "fdiv")
            else
                c.LLVMBuildSDiv(self.builder, casted_lhs, casted_rhs, "sdiv"),
            else => return errors.CodegenError.UnsupportedOperation,
        };
    }
    
    fn generateBrainfuck(self: *CodeGenerator, bf: ast.Brainfuck) !c.LLVMValueRef {
        const code = try self.allocator.dupeZ(u8, bf.code);
        const ctx = bfck.ParseBfContext(self.allocator, code);
        ctx.print();
        const mem_size = ctx.len * @divTrunc(ctx.cell_size, 8);
        std.debug.print("Bytes to be allocated: {}\n", .{mem_size});
        std.debug.print("Code: {s}\n", .{ctx.code});
        defer self.allocator.free(code);
        const string_ptr = c.LLVMBuildGlobalStringPtr(self.builder, code.ptr, "bf_str");
        const puts_func = try self.declareLibcFunction("puts");
        const puts_type = c.LLVMGlobalGetValueType(puts_func);
        var args = [_]c.LLVMValueRef{string_ptr};
        return c.LLVMBuildCall2(self.builder, puts_type, puts_func, &args[0], 1, "");
    }

    pub fn writeToFile(self: *CodeGenerator, filename: []const u8) !void {
        const filename_z = try self.allocator.dupeZ(u8, filename);
        defer self.allocator.free(filename_z);

        var error_msg: [*c]u8 = null;
        const result = c.LLVMPrintModuleToFile(self.module, filename_z.ptr, &error_msg);

        if (result != 0) {
            if (error_msg != null) {
                std.debug.print("LLVM Error: {s}\n", .{error_msg});
                c.LLVMDisposeMessage(error_msg);
            }
            return error.WriteFailed;
        }
    }

    pub fn compileToExecutable(self: *CodeGenerator, output_filename: []const u8, arch: []const u8) !void {
        const temp_ir_file = "temp_output.ll";
        try self.writeToFile(temp_ir_file);

        const output_filename_z = try self.allocator.dupeZ(u8, output_filename);
        defer self.allocator.free(output_filename_z);

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        const clang_args = if (std.mem.eql(u8, arch, ""))
            &[_][]const u8{
                "clang",
                temp_ir_file,
                "-o",
                output_filename,
                "-lc",
            }
        else blk: {
            const march_flag: []const u8 = try std.fmt.allocPrint(self.allocator, "--target={s}", .{arch});
            break :blk &[_][]const u8{
                "clang",
                temp_ir_file,
                "-o",
                output_filename,
                "-lc",
                march_flag,
            };
        };

        var child = std.process.Child.init(clang_args, arena_alloc);
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        try child.spawn();
        const result = try child.wait();

        std.fs.cwd().deleteFile(temp_ir_file) catch {};

        if (result != .Exited or result.Exited != 0) {
            std.debug.print("Clang compilation failed\n", .{});
            return error.CompilationFailed;
        }
    }
};