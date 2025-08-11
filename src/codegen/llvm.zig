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

// Signature information for libc functions
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
        // Initialize LLVM targets
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
        } else if (std.mem.eql(u8, type_name, "f32")) {
            return c.LLVMFloatTypeInContext(self.context);
        } else if (std.mem.eql(u8, type_name, "f64")) {
            return c.LLVMDoubleTypeInContext(self.context);
        } else if (std.mem.eql(u8, type_name, "void")) {
            return c.LLVMVoidTypeInContext(self.context);
        }
        return c.LLVMInt32TypeInContext(self.context);
    }

    fn libcTypeToLLVM(self: *CodeGenerator, libc_type: LibcType) c.LLVMTypeRef {
        return switch (libc_type) {
            .void_type => c.LLVMVoidTypeInContext(self.context),
            .int_type => c.LLVMInt32TypeInContext(self.context),
            .char_ptr_type => c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0),
            .size_t_type => c.LLVMInt64TypeInContext(self.context), // Assuming 64-bit system
            .file_ptr_type => c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0), // FILE* as i8*
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
        // Create a generic varargs function: int function(...)
        const i8_ptr_type = c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0);
        var generic_args = [_]c.LLVMTypeRef{i8_ptr_type}; // At least one parameter for varargs
        const function_type = c.LLVMFunctionType(c.LLVMInt32TypeInContext(self.context), // return int
            &generic_args[0], 1, 1 // varargs = true
        );

        const func_name_z = try self.allocator.dupeZ(u8, func_name);
        defer self.allocator.free(func_name_z);

        const llvm_func = c.LLVMAddFunction(self.module, func_name_z.ptr, function_type);
        try self.functions.put(try self.allocator.dupe(u8, func_name), llvm_func);

        std.debug.print("Warning: Creating generic signature for unknown libc function: {s}\n", .{func_name});

        return llvm_func;
    }

    pub fn generateCode(self: *CodeGenerator, program: *ast.Node) errors.CodegenError!void {
        // No longer need to pre-declare functions - they'll be declared on demand

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

                // Create entry basic block
                const entry_block = c.LLVMAppendBasicBlockInContext(self.context, llvm_func, "entry");
                c.LLVMPositionBuilderAtEnd(self.builder, entry_block);

                // Clear variables for new function scope
                self.variables.clearRetainingCapacity();

                // Generate function body
                for (func.body.items) |stmt| {
                    try self.generateStatement(stmt);
                }
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    fn generateStatement(self: *CodeGenerator, stmt: *ast.Node) errors.CodegenError!void {
        switch (stmt.data) {
            .var_decl => |decl| {
                const var_type = self.getLLVMType(decl.type_name);

                // Allocate space on stack
                const alloca = c.LLVMBuildAlloca(self.builder, var_type, decl.name.ptr);

                try self.variables.put(decl.name, VariableInfo{
                    .value = alloca,
                    .type_ref = var_type,
                });

                // Generate initializer if present
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
            else => {},
        }
    }

    fn castToType(self: *CodeGenerator, value: c.LLVMValueRef, target_type: c.LLVMTypeRef) c.LLVMValueRef {
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
                return c.LLVMBuildSExt(self.builder, value, target_type, "sext");
            }
        } else if (value_kind == c.LLVMFloatTypeKind and target_kind == c.LLVMFloatTypeKind) {
            // Both are floats, no conversion needed
            return value;
        } else if (value_kind == c.LLVMDoubleTypeKind and target_kind == c.LLVMDoubleTypeKind) {
            // Both are doubles, no conversion needed
            return value;
        } else if ((value_kind == c.LLVMFloatTypeKind and target_kind == c.LLVMDoubleTypeKind) or
            (value_kind == c.LLVMDoubleTypeKind and target_kind == c.LLVMFloatTypeKind))
        {
            // Float to double or double to float conversion
            if (value_kind == c.LLVMFloatTypeKind) {
                return c.LLVMBuildFPExt(self.builder, value, target_type, "fpext");
            } else {
                return c.LLVMBuildFPTrunc(self.builder, value, target_type, "fptrunc");
            }
        } else if ((value_kind == c.LLVMIntegerTypeKind and
            (target_kind == c.LLVMFloatTypeKind or target_kind == c.LLVMDoubleTypeKind)))
        {
            // Integer to float conversion
            return c.LLVMBuildSIToFP(self.builder, value, target_type, "sitofp");
        } else if (((value_kind == c.LLVMFloatTypeKind or value_kind == c.LLVMDoubleTypeKind) and
            target_kind == c.LLVMIntegerTypeKind))
        {
            // Float to integer conversion
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
                        // TODO: add more escape sequence
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
        // Special handling for known functions that need specific argument types
        if (std.mem.eql(u8, func_name, "printf") or
            std.mem.eql(u8, func_name, "sprintf") or
            std.mem.eql(u8, func_name, "snprintf") or
            std.mem.eql(u8, func_name, "fprintf"))
        {
            // For printf family functions, ensure integer arguments are i32
            // and float arguments are promoted to double for varargs
            if (arg_index > 0) { // Skip format string
                const arg_type = c.LLVMTypeOf(arg_value);
                const arg_kind = c.LLVMGetTypeKind(arg_type);

                if (arg_kind == c.LLVMIntegerTypeKind) {
                    const i32_type = c.LLVMInt32TypeInContext(self.context);
                    return self.castToType(arg_value, i32_type);
                } else if (arg_kind == c.LLVMFloatTypeKind) {
                    // Float arguments must be promoted to double for varargs
                    const double_type = c.LLVMDoubleTypeInContext(self.context);
                    return c.LLVMBuildFPExt(self.builder, arg_value, double_type, "fpext");
                }
            }
        }

        return arg_value;
    }

    // New function to generate expressions with type context
    fn generateExpressionWithContext(self: *CodeGenerator, expr: *ast.Node, expected_type: ?[]const u8) errors.CodegenError!c.LLVMValueRef {
        switch (expr.data) {
            .number_literal => |num| {
                // Check if it's a floating-point number
                if (std.mem.indexOf(u8, num.value, ".") != null) {
                    const float_val = std.fmt.parseFloat(f64, num.value) catch 0.0;
                    
                    // If we have context about expected type, create the appropriate LLVM type
                    if (expected_type) |type_name| {
                        if (std.mem.eql(u8, type_name, "f32")) {
                            return c.LLVMConstReal(c.LLVMFloatTypeInContext(self.context), float_val);
                        } else if (std.mem.eql(u8, type_name, "f64")) {
                            return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), float_val);
                        }
                    }
                    
                    // Default to double if no specific context
                    return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), float_val);
                } else {
                    // Integer literal
                    const value = std.fmt.parseInt(i32, num.value, 10) catch 0;
                    return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(value)), 0);
                }
            },
            else => return self.generateExpression(expr),
        }
    }

    fn generateExpression(self: *CodeGenerator, expr: *ast.Node) errors.CodegenError!c.LLVMValueRef {
        switch (expr.data) {
            .identifier => |ident| {
                if (self.variables.get(ident.name)) |var_info| {
                    return c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load");
                }
                return errors.CodegenError.UndefinedVariable;
            },
            .number_literal => |num| {
                // Check if it's a floating-point number
                if (std.mem.indexOf(u8, num.value, ".") != null) {
                    // Floating-point literal - default to double without context
                    const float_val = std.fmt.parseFloat(f64, num.value) catch 0.0;
                    return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), float_val);
                } else {
                    // Integer literal
                    const value = std.fmt.parseInt(i32, num.value, 10) catch 0;
                    return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(value)), 0);
                }
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
            .function_call => |call| {
                // For libc functions, declare them dynamically
                const func = if (call.is_libc)
                    try self.declareLibcFunction(call.name)
                else
                    self.functions.get(call.name) orelse return errors.CodegenError.UndefinedFunction;

                var args = std.ArrayList(c.LLVMValueRef).init(self.allocator);
                defer args.deinit();

                for (call.args.items, 0..) |arg, i| {
                    var arg_value = try self.generateExpression(arg);

                    // Apply argument preparation for libc calls
                    if (call.is_libc) {
                        arg_value = self.prepareArgumentForLibcCall(arg_value, call.name, i);
                    }

                    try args.append(arg_value);
                }

                // Check if function returns void
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
        // First write LLVM IR to temporary file
        const temp_ir_file = "temp_output.ll";
        try self.writeToFile(temp_ir_file);

        // Use system clang to compile LLVM IR to executable
        const output_filename_z = try self.allocator.dupeZ(u8, output_filename);
        defer self.allocator.free(output_filename_z);

        // Prepare clang command
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

        // Execute clang
        var child = std.process.Child.init(clang_args, arena_alloc);
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        try child.spawn();
        const result = try child.wait();

        // Clean up temp file
        std.fs.cwd().deleteFile(temp_ir_file) catch {};

        if (result != .Exited or result.Exited != 0) {
            std.debug.print("Clang compilation failed\n", .{});
            return error.CompilationFailed;
        }
    }
};