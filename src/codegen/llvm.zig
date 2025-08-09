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
        } else if (std.mem.eql(u8, type_name, "void")) {
            return c.LLVMVoidTypeInContext(self.context);
        }
        // Default to i32 for unknown types; consider logging a warning in production
        return c.LLVMInt32TypeInContext(self.context);
    }

    fn declareLibcFunctions(self: *CodeGenerator) void {
        // Declare printf: i32 (i8*, ...)
        const i8_ptr_type = c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0);
        var printf_args = [_]c.LLVMTypeRef{i8_ptr_type};
        const printf_type = c.LLVMFunctionType(c.LLVMInt32TypeInContext(self.context), &printf_args[0], 1, 1); // varargs = true
        const printf_func = c.LLVMAddFunction(self.module, "printf", printf_type);
        self.functions.put("printf", printf_func) catch {};

        // Declare puts: i32 (i8*)
        var puts_args = [_]c.LLVMTypeRef{i8_ptr_type};
        const puts_type = c.LLVMFunctionType(c.LLVMInt32TypeInContext(self.context), &puts_args[0], 1, 0);
        const puts_func = c.LLVMAddFunction(self.module, "puts", puts_type);
        self.functions.put("puts", puts_func) catch {};
    }

    pub fn generateCode(self: *CodeGenerator, program: *ast.Node) errors.CodegenError!void {
        self.declareLibcFunctions();

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
                    const init_value = try self.generateExpression(initializer);
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

    fn generateExpression(self: *CodeGenerator, expr: *ast.Node) errors.CodegenError!c.LLVMValueRef {
        switch (expr.data) {
            .identifier => |ident| {
                if (self.variables.get(ident.name)) |var_info| {
                    return c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load");
                }
                return errors.CodegenError.UndefinedVariable;
            },
            .number_literal => |num| {
                const value = std.fmt.parseInt(i32, num.value, 10) catch 0;
                return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(value)), 0);
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
                if (self.functions.get(call.name)) |func| {
                    var args = std.ArrayList(c.LLVMValueRef).init(self.allocator);
                    defer args.deinit();

                    for (call.args.items) |arg| {
                        var arg_value = try self.generateExpression(arg);

                        if (std.mem.eql(u8, call.name, "printf") and args.items.len > 0) {
                            const arg_type = c.LLVMTypeOf(arg_value);
                            const arg_kind = c.LLVMGetTypeKind(arg_type);

                            if (arg_kind == c.LLVMIntegerTypeKind) {
                                const i32_type = c.LLVMInt32TypeInContext(self.context);
                                arg_value = self.castToType(arg_value, i32_type);
                            }
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
                }
                return errors.CodegenError.UndefinedFunction;
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

    pub fn compileToExecutable(self: *CodeGenerator, output_filename: []const u8) !void {
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

        const clang_args = [_][]const u8{
            "clang",
            temp_ir_file,
            "-o",
            output_filename,
            "-lc", // Link with libc
        };

        // Execute clang
        var child = std.process.Child.init(&clang_args, arena_alloc);
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
