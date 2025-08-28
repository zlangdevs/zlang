const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const bfck = @import("bf.zig");
const control_flow = @import("control_flow.zig");

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
    .{ "putchar", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{.int_type} } },
    .{ "getchar", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{} } },
    .{ "rand", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{} } },
    .{ "srand", LibcFunctionSignature{ .return_type = .void_type, .param_types = &[_]LibcType{.int_type} } },
    .{ "square", LibcFunctionSignature{ .return_type = .int_type, .param_types = &[_]LibcType{.int_type} } },
});

pub const CodeGenerator = struct {
    context: c.LLVMContextRef,
    module: c.LLVMModuleRef,
    builder: c.LLVMBuilderRef,
    allocator: std.mem.Allocator,
    functions: std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    variables: std.HashMap([]const u8, VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    external_c_functions: std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    current_function: ?c.LLVMValueRef,
    current_function_return_type: []const u8,
    control_flow_analyzer: control_flow.ControlFlowAnalyzer,
    loop_context_stack: std.ArrayList(LoopContext),
    variable_scopes: std.ArrayList(std.HashMap([]const u8, VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage)),

    const VariableInfo = struct {
        value: c.LLVMValueRef,
        type_ref: c.LLVMTypeRef,
    };

    const LoopContext = struct {
        break_block: c.LLVMBasicBlockRef,
        continue_block: c.LLVMBasicBlockRef,
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
        const control_flow_analyzer = control_flow.ControlFlowAnalyzer.init(allocator);

        var variable_scopes = std.ArrayList(std.HashMap([]const u8, VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage)).init(allocator);
        try variable_scopes.append(std.HashMap([]const u8, VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator));

        return CodeGenerator{
            .context = context,
            .module = module,
            .builder = builder,
            .allocator = allocator,
            .functions = std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .variables = std.HashMap([]const u8, VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .external_c_functions = std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .current_function = null,
            .current_function_return_type = "",
            .control_flow_analyzer = control_flow_analyzer,
            .loop_context_stack = std.ArrayList(LoopContext).init(allocator),
            .variable_scopes = variable_scopes,
        };
    }

    pub fn deinit(self: *CodeGenerator) void {
        self.functions.deinit();
        self.variables.deinit();
        self.external_c_functions.deinit();
        self.loop_context_stack.deinit();
        for (self.variable_scopes.items) |*scope| {
            scope.deinit();
        }
        self.variable_scopes.deinit();
        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.context);
    }

    fn pushScope(self: *CodeGenerator) !void {
        const new_scope = std.HashMap([]const u8, VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(self.allocator);
        try self.variable_scopes.append(new_scope);
    }

    fn popScope(self: *CodeGenerator) void {
        if (self.variable_scopes.items.len > 1) { // Keep at least the global scope
            _ = self.variable_scopes.pop();
            // TODO: Properly deinit the popped scope to avoid memory leaks
            // For now, we'll leak the scope to get the scoping logic working
        }
    }

    fn clearCurrentFunctionScopes(self: *CodeGenerator) void {
        while (self.variable_scopes.items.len > 1) {
            var scope = self.variable_scopes.pop().?;
            scope.deinit();
        }
    }

    fn getVariable(self: *CodeGenerator, name: []const u8) ?VariableInfo {
        var i = self.variable_scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.variable_scopes.items[i].get(name)) |var_info| {
                return var_info;
            }
        }
        return null;
    }

    fn putVariable(self: *CodeGenerator, name: []const u8, var_info: VariableInfo) !void {
        // Only check for redeclaration in the current (innermost) scope
        const current_scope = &self.variable_scopes.items[self.variable_scopes.items.len - 1];
        if (current_scope.contains(name)) {
            std.debug.print("Error: variable '{s}' is already declared in current scope (scope count: {})\n", .{ name, self.variable_scopes.items.len });
            return errors.CodegenError.RedeclaredVariable;
        }

        // Store only in current scope - global variables map is no longer needed
        try current_scope.put(name, var_info);
    }

    fn variableExistsInCurrentScope(self: *CodeGenerator, name: []const u8) bool {
        if (self.variable_scopes.items.len == 0) return false;
        const current_scope = &self.variable_scopes.items[self.variable_scopes.items.len - 1];
        return current_scope.contains(name);
    }

    fn getLLVMType(self: *CodeGenerator, type_name: []const u8) c.LLVMTypeRef {
        if (std.mem.startsWith(u8, type_name, "ptr<") and std.mem.endsWith(u8, type_name, ">")) {
            const inner_type_name = type_name[4 .. type_name.len - 1];
            const inner_type = self.getLLVMType(inner_type_name);
            return c.LLVMPointerType(inner_type, 0);
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
        }
        return c.LLVMInt32TypeInContext(self.context);
    }

    fn isUnsignedType(type_name: []const u8) bool {
        return std.mem.startsWith(u8, type_name, "u");
    }

    fn getTypeNameFromLLVMType(self: *CodeGenerator, llvm_type: c.LLVMTypeRef) []const u8 {
        _ = self;
        const type_kind = c.LLVMGetTypeKind(llvm_type);
        const width = c.LLVMGetIntTypeWidth(llvm_type);

        return switch (type_kind) {
            c.LLVMIntegerTypeKind => {
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
            else => "i32",
        };
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
            const func = try self.createFunctionFromSignature(func_name, signature);
            try self.functions.put(try self.allocator.dupe(u8, func_name), func);
            return func;
        }
        const func = try self.createGenericLibcFunction(func_name);
        try self.functions.put(try self.allocator.dupe(u8, func_name), func);
        return func;
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
                    if (func.data == .c_function_decl) {
                        std.debug.print("Processing C function declaration: {s}\n", .{func.data.c_function_decl.name});
                        try self.generateCFunctionDeclaration(func.data.c_function_decl);
                    } else if (func.data == .function) {
                        try self.declareFunction(func);
                    }
                }
                self.external_c_functions = std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(self.allocator);
                for (prog.functions.items) |func| {
                    if (func.data == .function) {
                        try self.generateFunctionBody(func);
                    }
                }
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    fn declareFunction(self: *CodeGenerator, func_node: *ast.Node) errors.CodegenError!void {
        switch (func_node.data) {
            .function => |func| {
                var param_types = std.ArrayList(c.LLVMTypeRef).init(self.allocator);
                defer param_types.deinit();
                for (func.parameters.items) |param| {
                    try param_types.append(self.getLLVMType(param.type_name));
                }
                const return_type = self.getLLVMType(func.return_type);
                const function_type = if (param_types.items.len > 0)
                    c.LLVMFunctionType(return_type, param_types.items.ptr, @intCast(param_types.items.len), 0)
                else
                    c.LLVMFunctionType(return_type, null, 0, 0);

                const func_name_z = self.allocator.dupeZ(u8, func.name) catch return errors.CodegenError.OutOfMemory;
                defer self.allocator.free(func_name_z);

                const llvm_func = c.LLVMAddFunction(self.module, func_name_z.ptr, function_type);
                try self.functions.put(func.name, llvm_func);
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    fn generateFunctionBody(self: *CodeGenerator, func_node: *ast.Node) errors.CodegenError!void {
        switch (func_node.data) {
            .function => |func| {
                const llvm_func = self.functions.get(func.name) orelse return errors.CodegenError.UndefinedFunction;
                self.current_function = llvm_func;
                self.current_function_return_type = func.return_type;
                const entry_block = c.LLVMAppendBasicBlockInContext(self.context, llvm_func, "entry");
                c.LLVMPositionBuilderAtEnd(self.builder, entry_block);
                // Reset scopes for new function (keep global scope)
                self.clearCurrentFunctionScopes();
                // Create a new function-level scope for this function
                try self.pushScope();
                // Add parameters to the function-level scope
                for (func.parameters.items, 0..) |param, i| {
                    const param_value = c.LLVMGetParam(llvm_func, @intCast(i));
                    const param_type = self.getLLVMType(param.type_name);
                    const alloca = c.LLVMBuildAlloca(self.builder, param_type, param.name.ptr);
                    _ = c.LLVMBuildStore(self.builder, param_value, alloca);
                    try self.putVariable(param.name, VariableInfo{
                        .value = alloca,
                        .type_ref = param_type,
                    });
                }

                const valid_control_flow = try self.hasValidControlFlow(func_node);
                for (func.body.items) |stmt| {
                    try self.generateStatement(stmt);
                }
                if (!valid_control_flow and !std.mem.eql(u8, func.return_type, "void")) {
                    if (func.body.items.len == 0 or !self.isReturnStatement(func.body.items[func.body.items.len - 1])) {
                        std.debug.print("Error: Function '{s}' does not return a value on all code paths\n", .{func.name});
                        return errors.CodegenError.TypeMismatch;
                    }
                }
                if (valid_control_flow) {
                    const last_is_return = if (func.body.items.len > 0)
                        self.isReturnStatement(func.body.items[func.body.items.len - 1])
                    else
                        false;
                    if (std.mem.eql(u8, func.return_type, "void")) {
                        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                            _ = c.LLVMBuildRetVoid(self.builder);
                        }
                    } else {
                        if (!last_is_return) {
                            const default_value = self.getDefaultValueForType(func.return_type);
                            _ = c.LLVMBuildRet(self.builder, default_value);
                        } else {
                            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                                const default_value = self.getDefaultValueForType(func.return_type);
                                _ = c.LLVMBuildRet(self.builder, default_value);
                            }
                        }
                    }
                }
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    fn hasValidControlFlow(self: *CodeGenerator, func_node: *ast.Node) errors.CodegenError!bool {
        switch (func_node.data) {
            .function => |func| {
                if (std.mem.eql(u8, func.return_type, "void")) {
                    return true;
                }
                return self.control_flow_analyzer.analyzeStatementListEnhanced(func.body.items);
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    fn getDefaultValueForType(self: *CodeGenerator, type_name: []const u8) c.LLVMValueRef {
        if (std.mem.eql(u8, type_name, "i8")) {
            return c.LLVMConstInt(c.LLVMInt8TypeInContext(self.context), 0, 0);
        } else if (std.mem.eql(u8, type_name, "i16")) {
            return c.LLVMConstInt(c.LLVMInt16TypeInContext(self.context), 0, 0);
        } else if (std.mem.eql(u8, type_name, "i32")) {
            return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0);
        } else if (std.mem.eql(u8, type_name, "i64")) {
            return c.LLVMConstInt(c.LLVMInt64TypeInContext(self.context), 0, 0);
        } else if (std.mem.eql(u8, type_name, "u8")) {
            return c.LLVMConstInt(c.LLVMInt8TypeInContext(self.context), 0, 0);
        } else if (std.mem.eql(u8, type_name, "u16")) {
            return c.LLVMConstInt(c.LLVMInt16TypeInContext(self.context), 0, 0);
        } else if (std.mem.eql(u8, type_name, "u32")) {
            return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0);
        } else if (std.mem.eql(u8, type_name, "u64")) {
            return c.LLVMConstInt(c.LLVMInt64TypeInContext(self.context), 0, 0);
        } else if (std.mem.eql(u8, type_name, "f16")) {
            return c.LLVMConstReal(c.LLVMHalfTypeInContext(self.context), 0.0);
        } else if (std.mem.eql(u8, type_name, "f32")) {
            return c.LLVMConstReal(c.LLVMFloatTypeInContext(self.context), 0.0);
        } else if (std.mem.eql(u8, type_name, "f64")) {
            return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), 0.0);
        } else if (std.mem.eql(u8, type_name, "bool")) {
            return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 0, 0);
        }
        return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0);
    }
    fn isReturnStatement(self: *CodeGenerator, stmt: *ast.Node) bool {
        _ = self;
        return switch (stmt.data) {
            .return_stmt => true,
            else => false,
        };
    }

    fn generateStatement(self: *CodeGenerator, stmt: *ast.Node) errors.CodegenError!void {
        switch (stmt.data) {
            .assignment => |as| {
                const var_info = self.getVariable(as.name) orelse return errors.CodegenError.UndefinedVariable;
                const var_type_kind = c.LLVMGetTypeKind(var_info.type_ref);
                if (var_type_kind == c.LLVMArrayTypeKind) {
                    try self.generateArrayReassignment(as.name, as.value);
                } else {
                    const value = try self.generateExpression(as.value);
                    const casted_value = self.castToType(value, var_info.type_ref);
                    _ = c.LLVMBuildStore(self.builder, casted_value, var_info.value);
                }
            },
            .var_decl => |decl| {
                if (std.mem.eql(u8, decl.type_name, "void")) {
                    if (decl.initializer != null) {
                        std.debug.print("Error: void variables cannot be initialized with a value\n", .{});
                        return errors.CodegenError.TypeMismatch;
                    }
                    try self.putVariable(decl.name, VariableInfo{
                        .value = undefined,
                        .type_ref = c.LLVMVoidTypeInContext(self.context),
                    });
                    return;
                }

                if (std.mem.startsWith(u8, decl.type_name, "arr<") and std.mem.endsWith(u8, decl.type_name, ">")) {
                    try self.generateArrayDeclaration(decl);
                } else {
                    const var_type = self.getLLVMType(decl.type_name);
                    const alloca = c.LLVMBuildAlloca(self.builder, var_type, decl.name.ptr);
                    try self.putVariable(decl.name, VariableInfo{
                        .value = alloca,
                        .type_ref = var_type,
                    });
                    if (decl.initializer) |initializer| {
                        const init_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                        const casted_value = self.castToType(init_value, var_type);
                        _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                    }
                }
            },
            .function_call => {
                _ = try self.generateExpression(stmt);
            },
            .return_stmt => |ret| {
                if (ret.expression) |expr| {
                    const ret_value = try self.generateExpression(expr);
                    const casted_ret_value = self.castToType(ret_value, self.getLLVMType(self.current_function_return_type));
                    _ = c.LLVMBuildRet(self.builder, casted_ret_value);
                } else {
                    _ = c.LLVMBuildRetVoid(self.builder);
                }
            },
            .brainfuck => |bf| {
                _ = try self.generateBrainfuck(bf);
            },
            .if_stmt => |if_stmt| {
                try self.generateIfStatement(if_stmt);
            },
            .for_stmt => |for_stmt| {
                try self.generateForStatement(for_stmt);
            },
            .c_for_stmt => |c_for| {
                try self.generateCForStatement(c_for);
            },
            .break_stmt => {
                try self.generateBreakStatement();
            },
            .continue_stmt => {
                try self.generateContinueStatement();
            },
            .array_assignment => |arr_ass| {
                try self.generateArrayAssignment(arr_ass);
            },
            .c_function_decl => |c_func| {
                try self.generateCFunctionDeclaration(c_func);
            },
            .unary_op => {
                _ = try self.generateExpression(stmt);
            },
            else => {},
        }
    }

    fn generateIfStatement(self: *CodeGenerator, if_stmt: ast.IfStmt) errors.CodegenError!void {
        const current_function = self.current_function orelse return errors.CodegenError.TypeMismatch;
        const then_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "then");
        const else_bb = if (if_stmt.else_body != null)
            c.LLVMAppendBasicBlockInContext(self.context, current_function, "else")
        else
            null;
        const merge_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "ifcont");
        const condition_value = try self.generateExpression(if_stmt.condition);
        const condition_bool = self.convertToBool(condition_value);
        if (else_bb) |else_block| {
            _ = c.LLVMBuildCondBr(self.builder, condition_bool, then_bb, else_block);
        } else {
            _ = c.LLVMBuildCondBr(self.builder, condition_bool, then_bb, merge_bb);
        }

        c.LLVMPositionBuilderAtEnd(self.builder, then_bb);
        try self.pushScope();
        for (if_stmt.then_body.items) |stmt| {
            try self.generateStatement(stmt);
        }
        self.popScope();
        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
            _ = c.LLVMBuildBr(self.builder, merge_bb);
        }

        if (else_bb) |else_block| {
            c.LLVMPositionBuilderAtEnd(self.builder, else_block);
            if (if_stmt.else_body) |else_body| {
                try self.pushScope();
                for (else_body.items) |stmt| {
                    try self.generateStatement(stmt);
                }
                self.popScope();
            }
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                _ = c.LLVMBuildBr(self.builder, merge_bb);
            }
        }
        c.LLVMPositionBuilderAtEnd(self.builder, merge_bb);
    }

    fn generateForStatement(self: *CodeGenerator, for_stmt: ast.ForStmt) errors.CodegenError!void {
        const current_function = self.current_function orelse return errors.CodegenError.TypeMismatch;
        if (for_stmt.condition) |cond| {
            const condition_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "for_cond");
            const body_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "for_body");
            const exit_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "for_exit");
            const loop_context = LoopContext{
                .break_block = exit_bb,
                .continue_block = condition_bb,
            };
            try self.loop_context_stack.append(loop_context);
            _ = c.LLVMBuildBr(self.builder, condition_bb);
            c.LLVMPositionBuilderAtEnd(self.builder, condition_bb);
            const cond_value = try self.generateExpression(cond);
            const cond_bool = self.convertToBool(cond_value);
            _ = c.LLVMBuildCondBr(self.builder, cond_bool, body_bb, exit_bb);
            c.LLVMPositionBuilderAtEnd(self.builder, body_bb);
            try self.pushScope();
            for (for_stmt.body.items) |stmt| {
                try self.generateStatement(stmt);
            }
            self.popScope();
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                _ = c.LLVMBuildBr(self.builder, condition_bb);
            }
            c.LLVMPositionBuilderAtEnd(self.builder, exit_bb);
            _ = self.loop_context_stack.pop();
        } else {
            const body_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "for_body");
            const exit_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "for_exit");
            const loop_context = LoopContext{
                .break_block = exit_bb,
                .continue_block = body_bb,
            };
            try self.loop_context_stack.append(loop_context);
            _ = c.LLVMBuildBr(self.builder, body_bb);
            c.LLVMPositionBuilderAtEnd(self.builder, body_bb);
            try self.pushScope();
            for (for_stmt.body.items) |stmt| {
                try self.generateStatement(stmt);
            }
            self.popScope();

            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                _ = c.LLVMBuildBr(self.builder, body_bb);
            }

            c.LLVMPositionBuilderAtEnd(self.builder, exit_bb);
            _ = self.loop_context_stack.pop();
        }
    }

    fn generateCForStatement(self: *CodeGenerator, c_for_stmt: ast.CForStmt) errors.CodegenError!void {
        const current_function = self.current_function orelse return errors.CodegenError.TypeMismatch;
        if (c_for_stmt.init) |cinit| {
            try self.generateStatement(cinit);
        }

        const condition_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "c_for_cond");
        const body_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "c_for_body");
        const increment_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "c_for_inc");
        const exit_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "c_for_exit");
        _ = c.LLVMBuildBr(self.builder, condition_bb);
        c.LLVMPositionBuilderAtEnd(self.builder, condition_bb);
        if (c_for_stmt.condition) |cond| {
            const cond_value = try self.generateExpression(cond);
            const cond_bool = self.convertToBool(cond_value);
            _ = c.LLVMBuildCondBr(self.builder, cond_bool, body_bb, exit_bb);
        } else {
            _ = c.LLVMBuildBr(self.builder, body_bb);
        }
        const loop_context = LoopContext{
            .break_block = exit_bb,
            .continue_block = increment_bb,
        };
        try self.loop_context_stack.append(loop_context);
        c.LLVMPositionBuilderAtEnd(self.builder, body_bb);
        try self.pushScope();
        for (c_for_stmt.body.items) |stmt| {
            try self.generateStatement(stmt);
        }
        self.popScope();
        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
            _ = c.LLVMBuildBr(self.builder, increment_bb);
        }
        c.LLVMPositionBuilderAtEnd(self.builder, increment_bb);
        if (c_for_stmt.increment) |inc| {
            try self.generateStatement(inc);
        }
        _ = c.LLVMBuildBr(self.builder, condition_bb);
        c.LLVMPositionBuilderAtEnd(self.builder, exit_bb);
        _ = self.loop_context_stack.pop();
    }

    fn generateArrayAssignment(self: *CodeGenerator, arr_ass: ast.ArrayAssignment) errors.CodegenError!void {
        const var_info = self.getVariable(arr_ass.array_name) orelse return errors.CodegenError.UndefinedVariable;

        const index_value = try self.generateExpression(arr_ass.index);
        const value = try self.generateExpression(arr_ass.value);

        const element_type = c.LLVMGetElementType(var_info.type_ref);

        var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_value };
        const element_ptr = c.LLVMBuildGEP2(self.builder, var_info.type_ref, var_info.value, &indices[0], 2, "array_element_ptr");

        const casted_value = self.castToType(value, element_type);
        _ = c.LLVMBuildStore(self.builder, casted_value, element_ptr);
    }

    fn generateArrayReassignment(self: *CodeGenerator, array_name: []const u8, value_expr: *ast.Node) errors.CodegenError!void {
        const var_info = self.getVariable(array_name) orelse return errors.CodegenError.UndefinedVariable;

        switch (value_expr.data) {
            .array_initializer => |init_list| {
                for (init_list.elements.items, 0..) |element, idx| {
                    const element_value = try self.generateExpression(element);
                    const element_type = c.LLVMGetElementType(var_info.type_ref);

                    var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(idx)), 0) };
                    const element_ptr = c.LLVMBuildGEP2(self.builder, var_info.type_ref, var_info.value, &indices[0], 2, "array_element_ptr");

                    const casted_value = self.castToType(element_value, element_type);
                    _ = c.LLVMBuildStore(self.builder, casted_value, element_ptr);
                }
            },
            else => {
                std.debug.print("Error: invalid array re-assignment\n", .{});
                return errors.CodegenError.TypeMismatch;
            },
        }
    }

    fn generateArrayDeclaration(self: *CodeGenerator, decl: ast.VarDecl) errors.CodegenError!void {
        const inner = decl.type_name[4 .. decl.type_name.len - 1];
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
            const array_size = std.fmt.parseInt(usize, size_str, 10) catch {
                std.debug.print("Error: invalid array size\\n", .{});
                return errors.CodegenError.TypeMismatch;
            };
            const element_type = self.getLLVMType(element_type_name);
            const array_type = c.LLVMArrayType(element_type, @intCast(array_size));
            const alloca = c.LLVMBuildAlloca(self.builder, array_type, decl.name.ptr);

            try self.putVariable(decl.name, VariableInfo{
                .value = alloca,
                .type_ref = array_type,
            });
            if (decl.initializer) |initializer| {
                switch (initializer.data) {
                    .array_initializer => |init_list| {
                        for (init_list.elements.items, 0..) |element, idx| {
                            const element_value = try self.generateExpression(element);
                            var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(idx)), 0) };
                            const element_ptr = c.LLVMBuildGEP2(self.builder, array_type, alloca, &indices[0], 2, "array_element_ptr");
                            const casted_value = self.castToType(element_value, element_type);
                            _ = c.LLVMBuildStore(self.builder, casted_value, element_ptr);
                        }
                    },
                    else => {
                        std.debug.print("Error: invalid array initializer\\n", .{});
                        return errors.CodegenError.TypeMismatch;
                    },
                }
            }
        } else {
            std.debug.print("Error: invalid array type format\\n", .{});
            return errors.CodegenError.TypeMismatch;
        }
    }

    fn generateContinueStatement(self: *CodeGenerator) errors.CodegenError!void {
        if (self.loop_context_stack.items.len == 0) {
            std.debug.print("Error: continue statement not inside a loop\n", .{});
            return errors.CodegenError.TypeMismatch;
        }
        const loop_context = self.loop_context_stack.items[self.loop_context_stack.items.len - 1];
        const continue_block = loop_context.continue_block;
        _ = c.LLVMBuildBr(self.builder, continue_block);
    }

    fn generateBreakStatement(self: *CodeGenerator) errors.CodegenError!void {
        if (self.loop_context_stack.items.len == 0) {
            std.debug.print("Error: break statement not inside a loop\n", .{});
            return errors.CodegenError.TypeMismatch;
        }
        const loop_context = self.loop_context_stack.items[self.loop_context_stack.items.len - 1];
        const break_block = loop_context.break_block;
        _ = c.LLVMBuildBr(self.builder, break_block);
    }

    fn generateCFunctionDeclaration(self: *CodeGenerator, c_func: ast.CFunctionDecl) errors.CodegenError!void {
        var param_types = std.ArrayList(c.LLVMTypeRef).init(self.allocator);
        defer param_types.deinit();
        for (c_func.parameters.items) |param| {
            try param_types.append(self.getLLVMType(param.type_name));
        }
        const return_type = self.getLLVMType(c_func.return_type);
        const function_type = if (param_types.items.len > 0)
            c.LLVMFunctionType(return_type, param_types.items.ptr, @intCast(param_types.items.len), 0)
        else
            c.LLVMFunctionType(return_type, null, 0, 0);
        const func_name_z = try self.allocator.dupeZ(u8, c_func.name);
        defer self.allocator.free(func_name_z);
        _ = self.functions.remove(c_func.name);
        const llvm_func = c.LLVMAddFunction(self.module, func_name_z.ptr, function_type);
        try self.functions.put(try self.allocator.dupe(u8, c_func.name), llvm_func);
        c.LLVMSetLinkage(llvm_func, c.LLVMExternalLinkage);
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
                    const target_type_name = self.getTypeNameFromLLVMType(target_type);
                    if (isUnsignedType(target_type_name)) {
                        return c.LLVMBuildZExt(self.builder, value, target_type, "zext");
                    } else {
                        return c.LLVMBuildSExt(self.builder, value, target_type, "sext");
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
                return c.LLVMBuildFPExt(self.builder, value, target_type, "fpext");
            } else {
                return c.LLVMBuildFPTrunc(self.builder, value, target_type, "fptrunc");
            }
        } else if ((value_kind == c.LLVMIntegerTypeKind and
            (target_kind == c.LLVMFloatTypeKind or target_kind == c.LLVMDoubleTypeKind or target_kind == c.LLVMHalfTypeKind)))
        {
            return c.LLVMBuildSIToFP(self.builder, value, target_type, "sitofp");
        } else if ((value_kind == c.LLVMFloatTypeKind or value_kind == c.LLVMDoubleTypeKind or value_kind == c.LLVMHalfTypeKind) and
            target_kind == c.LLVMIntegerTypeKind)
        {
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
                        '"' => transformed_string.append('"') catch return errors.CodegenError.OutOfMemory,
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
                    const width = c.LLVMGetIntTypeWidth(arg_type);
                    if (width == 64) {
                        return arg_value;
                    } else {
                        const i32_type = c.LLVMInt32TypeInContext(self.context);
                        return self.castToType(arg_value, i32_type);
                    }
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
                    if (expected_type) |type_name| {
                        const llvm_type = self.getLLVMType(type_name);
                        if (std.mem.startsWith(u8, type_name, "u")) {
                            if (std.mem.startsWith(u8, num.value, "-")) {
                                std.debug.print("Error: Cannot assign negative value '{s}' to unsigned type {s}\n", .{ num.value, type_name });
                                return errors.CodegenError.TypeMismatch;
                            }
                            const value = std.fmt.parseInt(u64, num.value, 10) catch {
                                std.debug.print("Error: Invalid unsigned value '{s}' for type {s}\n", .{ num.value, type_name });
                                return errors.CodegenError.TypeMismatch;
                            };
                            return c.LLVMConstInt(llvm_type, @as(c_ulonglong, @intCast(value)), 0);
                        } else {
                            const value = std.fmt.parseInt(i64, num.value, 10) catch {
                                std.debug.print("Error: Invalid signed value '{s}' for type {s}\n", .{ num.value, type_name });
                                return errors.CodegenError.TypeMismatch;
                            };
                            return c.LLVMConstInt(llvm_type, @as(c_ulonglong, @intCast(value)), 0);
                        }
                    } else {
                        const value = std.fmt.parseInt(i64, num.value, 10) catch 0;
                        return c.LLVMConstInt(c.LLVMInt64TypeInContext(self.context), @as(c_ulonglong, @intCast(value)), 0);
                    }
                }
            },
            .identifier => |ident| {
                if (std.mem.eql(u8, ident.name, "true")) {
                    return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 1, 0);
                } else if (std.mem.eql(u8, ident.name, "false")) {
                    return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 0, 0);
                }
                if (self.getVariable(ident.name)) |var_info| {
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

                if (self.getVariable(ident.name)) |var_info| {
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
                    const value = std.fmt.parseInt(i64, num.value, 10) catch 0;
                    return c.LLVMConstInt(c.LLVMInt64TypeInContext(self.context), @as(c_ulonglong, @intCast(value)), 0);
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
            .unary_op => |un| {
                switch (un.op) {
                    '-' => {
                        const operand_val = try self.generateExpression(un.operand);
                        const operand_type = c.LLVMTypeOf(operand_val);
                        const type_kind = c.LLVMGetTypeKind(operand_type);
                        switch (type_kind) {
                            c.LLVMIntegerTypeKind => {
                                return c.LLVMBuildNeg(self.builder, operand_val, "neg");
                            },
                            c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind => {
                                return c.LLVMBuildFNeg(self.builder, operand_val, "fneg");
                            },
                            else => {
                                return errors.CodegenError.UnsupportedOperation;
                            },
                        }
                    },
                    '+' => {
                        const operand_val = try self.generateExpression(un.operand);
                        return operand_val;
                    },
                    '!' => {
                        const operand_val = try self.generateExpression(un.operand);
                        const bool_val = self.convertToBool(operand_val);
                        const true_val = c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 1, 0);
                        return c.LLVMBuildXor(self.builder, bool_val, true_val, "not");
                    },
                    '&' => {
                        if (un.operand.data == .identifier) {
                            const ident = un.operand.data.identifier;
                            if (self.getVariable(ident.name)) |var_info| {
                                return var_info.value;
                            }
                        }
                        std.debug.print("Error: Address-of operator can only be applied to variables\n", .{});
                        return errors.CodegenError.TypeMismatch;
                    },
                    '*' => {
                        const operand_val = try self.generateExpression(un.operand);
                        const operand_type = c.LLVMTypeOf(operand_val);
                        if (c.LLVMGetTypeKind(operand_type) == c.LLVMPointerTypeKind) {
                            const pointee_type = c.LLVMGetElementType(operand_type);
                            return c.LLVMBuildLoad2(self.builder, pointee_type, operand_val, "deref");
                        } else {
                            std.debug.print("Error: Dereference operator can only be applied to pointers\n", .{});
                            return errors.CodegenError.TypeMismatch;
                        }
                    },
                    'D' => {
                        if (un.operand.data != .identifier) {
                            std.debug.print("Error: Decrement operator can only be applied to variables\n", .{});
                            return errors.CodegenError.TypeMismatch;
                        }

                        const ident = un.operand.data.identifier;
                        if (self.getVariable(ident.name)) |var_info| {
                            const var_type_name = self.getTypeNameFromLLVMType(var_info.type_ref);

                            const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                            var is_allowed = false;
                            for (allowed_types) |allowed| {
                                if (std.mem.eql(u8, var_type_name, allowed)) {
                                    is_allowed = true;
                                    break;
                                }
                            }

                            if (!is_allowed) {
                                std.debug.print("Error: Decrement operator not supported for type {s}\n", .{var_type_name});
                                return errors.CodegenError.UnsupportedOperation;
                            }

                            const var_ptr = var_info.value;
                            const var_type = var_info.type_ref;

                            const current_val = c.LLVMBuildLoad2(self.builder, var_type, var_ptr, "load_for_dec");

                            const new_val = switch (c.LLVMGetTypeKind(var_type)) {
                                c.LLVMIntegerTypeKind => blk: {
                                    const one = c.LLVMConstInt(var_type, 1, 0);
                                    break :blk c.LLVMBuildSub(self.builder, current_val, one, "dec");
                                },
                                c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => blk: {
                                    const one = c.LLVMConstReal(var_type, 1.0);
                                    break :blk c.LLVMBuildFSub(self.builder, current_val, one, "fdec");
                                },
                                else => unreachable,
                            };

                            _ = c.LLVMBuildStore(self.builder, new_val, var_ptr);

                            return new_val;
                        } else {
                            std.debug.print("Error: Variable not found\n", .{});
                            return errors.CodegenError.UndefinedVariable;
                        }
                    },
                    'I' => {
                        if (un.operand.data != .identifier) {
                            std.debug.print("Error: Increment operator can only be applied to variables\n", .{});
                            return errors.CodegenError.TypeMismatch;
                        }

                        const ident = un.operand.data.identifier;
                        if (self.getVariable(ident.name)) |var_info| {
                            const var_type_name = self.getTypeNameFromLLVMType(var_info.type_ref);
                            const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                            var is_allowed = false;
                            for (allowed_types) |allowed| {
                                if (std.mem.eql(u8, var_type_name, allowed)) {
                                    is_allowed = true;
                                    break;
                                }
                            }
                            if (!is_allowed) {
                                std.debug.print("Error: Increment operator not supported for type {s}\n", .{var_type_name});
                                return errors.CodegenError.UnsupportedOperation;
                            }
                            const var_ptr = var_info.value;
                            const var_type = var_info.type_ref;

                            const current_val = c.LLVMBuildLoad2(self.builder, var_type, var_ptr, "load_for_inc");

                            const new_val = switch (c.LLVMGetTypeKind(var_type)) {
                                c.LLVMIntegerTypeKind => blk: {
                                    const one = c.LLVMConstInt(var_type, 1, 0);
                                    break :blk c.LLVMBuildAdd(self.builder, current_val, one, "inc");
                                },
                                c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => blk: {
                                    const one = c.LLVMConstReal(var_type, 1.0);
                                    break :blk c.LLVMBuildFAdd(self.builder, current_val, one, "finc");
                                },
                                else => unreachable,
                            };
                            _ = c.LLVMBuildStore(self.builder, new_val, var_ptr);
                            return new_val;
                        } else {
                            std.debug.print("Error: Variable not found\n", .{});
                            return errors.CodegenError.UndefinedVariable;
                        }
                    },
                    else => {
                        return errors.CodegenError.UnsupportedOperation;
                    },
                }
            },
            .comparison => |comp| {
                return self.generateComparison(comp);
            },
            .binary_op => |b| {
                return self.generateBinaryOp(b);
            },
            .function_call => |call| {
                var func: c.LLVMValueRef = undefined;
                if (call.is_libc) {
                    // For libc calls, we need to be very careful about which function we call
                    // The issue is that there might be both a declaration and a definition with the same name
                    // First, try to find an external declaration by iterating through all functions in the module
                    var found_external = false;
                    var func_iter = c.LLVMGetFirstFunction(self.module);
                    while (func_iter != null) : (func_iter = c.LLVMGetNextFunction(func_iter)) {
                        const func_name_ptr = c.LLVMGetValueName(func_iter);
                        if (func_name_ptr != null) {
                            const func_name_slice = std.mem.span(func_name_ptr);
                            // Look for a function with the exact name that is a declaration (external function)
                            if (std.mem.eql(u8, func_name_slice, call.name) and c.LLVMIsDeclaration(func_iter) != 0) {
                                func = func_iter;
                                found_external = true;
                                break;
                            }
                        }
                    }
                    if (!found_external) {
                        // If we didn't find an external declaration, create one
                        if (LIBC_FUNCTIONS.get(call.name)) |signature| {
                            func = try self.createFunctionFromSignature(call.name, signature);
                        } else {
                            func = try self.declareLibcFunction(call.name);
                        }
                    }
                } else {
                    // For non-libc calls, always prioritize external declarations over defined functions
                    // This is critical for wrapper functions that need to call external C functions
                    var found_external = false;
                    var func_iter = c.LLVMGetFirstFunction(self.module);
                    while (func_iter != null) : (func_iter = c.LLVMGetNextFunction(func_iter)) {
                        const func_name_ptr = c.LLVMGetValueName(func_iter);
                        if (func_name_ptr != null) {
                            const func_name_slice = std.mem.span(func_name_ptr);
                            if (std.mem.eql(u8, func_name_slice, call.name) and c.LLVMIsDeclaration(func_iter) != 0) {
                                func = func_iter;
                                found_external = true;
                                break;
                            }
                        }
                    }
                    if (!found_external) {
                        if (self.functions.get(call.name)) |declared_func| {
                            func = declared_func;
                        } else {
                            return errors.CodegenError.UndefinedFunction;
                        }
                    }
                }

                var args = std.ArrayList(c.LLVMValueRef).init(self.allocator);
                defer args.deinit();

                for (call.args.items, 0..) |arg, i| {
                    var arg_value = try self.generateExpression(arg);

                    // Cast argument to match the expected parameter type
                    if (call.is_libc) {
                        if (LIBC_FUNCTIONS.get(call.name)) |signature| {
                            if (i < signature.param_types.len) {
                                const expected_type = self.libcTypeToLLVM(signature.param_types[i]);
                                arg_value = self.castToType(arg_value, expected_type);
                            }
                        }
                    } else {
                        // For non-libc functions, try to cast based on external declaration
                        var func_iter = c.LLVMGetFirstFunction(self.module);
                        while (func_iter != null) : (func_iter = c.LLVMGetNextFunction(func_iter)) {
                            const func_name_ptr = c.LLVMGetValueName(func_iter);
                            if (func_name_ptr != null) {
                                const func_name_slice = std.mem.span(func_name_ptr);
                                if (std.mem.eql(u8, func_name_slice, call.name) and c.LLVMIsDeclaration(func_iter) != 0) {
                                    // Found external declaration, cast to match parameter type
                                    const func_type = c.LLVMGlobalGetValueType(func_iter);
                                    const param_count = c.LLVMCountParamTypes(func_type);
                                    if (i < @as(usize, @intCast(param_count))) {
                                        var param_types = std.ArrayList(c.LLVMTypeRef).init(self.allocator);
                                        defer param_types.deinit();
                                        try param_types.resize(@as(usize, @intCast(param_count)));
                                        c.LLVMGetParamTypes(func_type, param_types.items.ptr);
                                        const expected_type = param_types.items[i];
                                        arg_value = self.castToType(arg_value, expected_type);
                                    }
                                    break;
                                }
                            }
                        }
                    }

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
            .array_index => |arr_idx| {
                const var_info = self.getVariable(arr_idx.array_name) orelse return errors.CodegenError.UndefinedVariable;
                const index_value = try self.generateExpression(arr_idx.index);
                const element_type = c.LLVMGetElementType(var_info.type_ref);
                var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_value };
                const element_ptr = c.LLVMBuildGEP2(self.builder, var_info.type_ref, var_info.value, &indices[0], 2, "array_element_ptr");

                return c.LLVMBuildLoad2(self.builder, element_type, element_ptr, "array_element");
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    fn generateComparison(self: *CodeGenerator, comparison: ast.Comparison) errors.CodegenError!c.LLVMValueRef {
        const lhs_value = try self.generateExpression(comparison.lhs);
        const rhs_value = try self.generateExpression(comparison.rhs);
        const lhs_type = c.LLVMTypeOf(lhs_value);
        const rhs_type = c.LLVMTypeOf(rhs_value);
        const lhs_kind = c.LLVMGetTypeKind(lhs_type);
        const rhs_kind = c.LLVMGetTypeKind(rhs_type);
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
                std.debug.print("Error: boolean values can only be compared for equality/inequality\n", .{});
                return errors.CodegenError.TypeMismatch;
            }

            return switch (comparison.op) {
                '=' => c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, lhs_value, rhs_value, "icmp_eq"),
                '!' => c.LLVMBuildICmp(self.builder, c.LLVMIntNE, lhs_value, rhs_value, "icmp_ne"),
                else => return errors.CodegenError.UnsupportedOperation,
            };
        }
        var converted_lhs = lhs_value;
        var converted_rhs = rhs_value;
        if (is_lhs_bool and !is_rhs_bool) {
            if (is_rhs_float) {
                converted_lhs = c.LLVMBuildUIToFP(self.builder, lhs_value, rhs_type, "bool_to_fp");
            } else {
                converted_lhs = c.LLVMBuildZExt(self.builder, lhs_value, rhs_type, "bool_to_int");
            }
        } else if (is_rhs_bool and !is_lhs_bool) {
            if (is_lhs_float) {
                converted_rhs = c.LLVMBuildUIToFP(self.builder, rhs_value, lhs_type, "bool_to_fp");
            } else {
                converted_rhs = c.LLVMBuildZExt(self.builder, rhs_value, lhs_type, "bool_to_int");
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
                result_type = c.LLVMDoubleTypeInContext(self.context);
            } else if (final_lhs_kind == c.LLVMFloatTypeKind or final_rhs_kind == c.LLVMFloatTypeKind) {
                result_type = c.LLVMFloatTypeInContext(self.context);
            } else {
                result_type = c.LLVMFloatTypeInContext(self.context);
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
        const casted_lhs = self.castToType(converted_lhs, result_type);
        const casted_rhs = self.castToType(converted_rhs, result_type);
        return switch (comparison.op) {
            '=' => {
                if (is_float_comp) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealOEQ, casted_lhs, casted_rhs, "fcmp_eq");
                } else {
                    return c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, casted_lhs, casted_rhs, "icmp_eq");
                }
            },
            '!' => {
                if (is_float_comp) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealONE, casted_lhs, casted_rhs, "fcmp_ne");
                } else {
                    return c.LLVMBuildICmp(self.builder, c.LLVMIntNE, casted_lhs, casted_rhs, "icmp_ne");
                }
            },
            '<' => {
                if (is_float_comp) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, casted_lhs, casted_rhs, "fcmp_lt");
                } else {
                    const is_unsigned = isUnsignedType(self.getTypeNameFromLLVMType(result_type));
                    return c.LLVMBuildICmp(self.builder, if (is_unsigned) c.LLVMIntULT else c.LLVMIntSLT, casted_lhs, casted_rhs, "icmp_lt");
                }
            },
            '>' => {
                if (is_float_comp) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, casted_lhs, casted_rhs, "fcmp_gt");
                } else {
                    const is_unsigned = isUnsignedType(self.getTypeNameFromLLVMType(result_type));
                    return c.LLVMBuildICmp(self.builder, if (is_unsigned) c.LLVMIntUGT else c.LLVMIntSGT, casted_lhs, casted_rhs, "icmp_gt");
                }
            },
            'L' => {
                if (is_float_comp) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealOLE, casted_lhs, casted_rhs, "fcmp_le");
                } else {
                    const is_unsigned = isUnsignedType(self.getTypeNameFromLLVMType(result_type));
                    return c.LLVMBuildICmp(self.builder, if (is_unsigned) c.LLVMIntULE else c.LLVMIntSLE, casted_lhs, casted_rhs, "icmp_le");
                }
            },
            'G' => {
                if (is_float_comp) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealOGE, casted_lhs, casted_rhs, "fcmp_ge");
                } else {
                    const is_unsigned = isUnsignedType(self.getTypeNameFromLLVMType(result_type));
                    return c.LLVMBuildICmp(self.builder, if (is_unsigned) c.LLVMIntUGE else c.LLVMIntSGE, casted_lhs, casted_rhs, "icmp_ge");
                }
            },
            else => return errors.CodegenError.UnsupportedOperation,
        };
    }

    fn generateBinaryOp(self: *CodeGenerator, bin_op: ast.BinaryOp) errors.CodegenError!c.LLVMValueRef {
        const lhs_value = try self.generateExpression(bin_op.lhs);
        const rhs_value = try self.generateExpression(bin_op.rhs);
        const lhs_type = c.LLVMTypeOf(lhs_value);
        const rhs_type = c.LLVMTypeOf(rhs_value);

        const lhs_kind = c.LLVMGetTypeKind(lhs_type);
        const rhs_kind = c.LLVMGetTypeKind(rhs_type);
        if (bin_op.op != '&' and bin_op.op != '|') {
            if (lhs_kind == c.LLVMIntegerTypeKind and c.LLVMGetIntTypeWidth(lhs_type) == 1) {
                std.debug.print("Error: Boolean type i1 not allowed in arithmetic operations\n", .{});
                return errors.CodegenError.TypeMismatch;
            }
            if (rhs_kind == c.LLVMIntegerTypeKind and c.LLVMGetIntTypeWidth(rhs_type) == 1) {
                std.debug.print("Error: Boolean type i1 not allowed in arithmetic operations\n", .{});
                return errors.CodegenError.TypeMismatch;
            }
        }

        const is_lhs_float = lhs_kind == c.LLVMFloatTypeKind or
            lhs_kind == c.LLVMDoubleTypeKind or
            lhs_kind == c.LLVMHalfTypeKind;
        const is_rhs_float = rhs_kind == c.LLVMFloatTypeKind or
            rhs_kind == c.LLVMDoubleTypeKind or
            rhs_kind == c.LLVMHalfTypeKind;
        const is_float_op = is_lhs_float or is_rhs_float;
        const result_type = if (is_float_op) blk: {
            if (lhs_kind == c.LLVMDoubleTypeKind or rhs_kind == c.LLVMDoubleTypeKind) {
                break :blk c.LLVMDoubleTypeInContext(self.context);
            } else if (lhs_kind == c.LLVMFloatTypeKind or rhs_kind == c.LLVMFloatTypeKind) {
                break :blk c.LLVMFloatTypeInContext(self.context);
            } else {
                break :blk c.LLVMFloatTypeInContext(self.context);
            }
        } else blk: {
            const lhs_width = if (lhs_kind == c.LLVMIntegerTypeKind) c.LLVMGetIntTypeWidth(lhs_type) else 0;
            const rhs_width = if (rhs_kind == c.LLVMIntegerTypeKind) c.LLVMGetIntTypeWidth(rhs_type) else 0;
            break :blk if (lhs_width >= rhs_width) lhs_type else rhs_type;
        };

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
        } else if (bin_op.op != '&' and bin_op.op != '|') {
            casted_lhs = self.castToType(lhs_value, result_type);
            casted_rhs = self.castToType(rhs_value, result_type);
        }

        return switch (bin_op.op) {
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
            else if (isUnsignedType(self.getTypeNameFromLLVMType(result_type)))
                c.LLVMBuildUDiv(self.builder, casted_lhs, casted_rhs, "udiv")
            else
                c.LLVMBuildSDiv(self.builder, casted_lhs, casted_rhs, "sdiv"),
            '&' => self.generateLogicalAnd(bin_op.lhs, bin_op.rhs),
            '|' => self.generateLogicalOr(bin_op.lhs, bin_op.rhs),
            else => {
                std.debug.print("Error: Unsupported binary operator\n", .{});
                return errors.CodegenError.UnsupportedOperation;
            },
        };
    }

    fn generateLogicalAnd(self: *CodeGenerator, lhs_expr: *ast.Node, rhs_expr: *ast.Node) errors.CodegenError!c.LLVMValueRef {
        const current_function = self.current_function orelse return errors.CodegenError.TypeMismatch;
        const rhs_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "and_rhs");
        const merge_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "and_merge");
        const lhs_value = try self.generateExpression(lhs_expr);
        const lhs_bool = self.convertToBool(lhs_value);
        const lhs_bb = c.LLVMGetInsertBlock(self.builder);
        _ = c.LLVMBuildCondBr(self.builder, lhs_bool, rhs_bb, merge_bb);
        c.LLVMPositionBuilderAtEnd(self.builder, rhs_bb);
        const rhs_value = try self.generateExpression(rhs_expr);
        const rhs_bool = self.convertToBool(rhs_value);
        _ = c.LLVMBuildBr(self.builder, merge_bb);
        const rhs_end_bb = c.LLVMGetInsertBlock(self.builder);
        c.LLVMPositionBuilderAtEnd(self.builder, merge_bb);
        const bool_type = c.LLVMInt1TypeInContext(self.context);
        const phi = c.LLVMBuildPhi(self.builder, bool_type, "and_result");
        const false_val = c.LLVMConstInt(bool_type, 0, 0);
        var phi_vals = [_]c.LLVMValueRef{ false_val, rhs_bool };
        var phi_blocks = [_]c.LLVMBasicBlockRef{ lhs_bb, rhs_end_bb };
        c.LLVMAddIncoming(phi, &phi_vals[0], &phi_blocks[0], 2);
        return phi;
    }

    fn generateLogicalOr(self: *CodeGenerator, lhs_expr: *ast.Node, rhs_expr: *ast.Node) errors.CodegenError!c.LLVMValueRef {
        const current_function = self.current_function orelse return errors.CodegenError.TypeMismatch;
        const rhs_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "or_rhs");
        const merge_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "or_merge");
        const lhs_value = try self.generateExpression(lhs_expr);
        const lhs_bool = self.convertToBool(lhs_value);
        const lhs_bb = c.LLVMGetInsertBlock(self.builder);
        _ = c.LLVMBuildCondBr(self.builder, lhs_bool, merge_bb, rhs_bb);
        c.LLVMPositionBuilderAtEnd(self.builder, rhs_bb);
        const rhs_value = try self.generateExpression(rhs_expr);
        const rhs_bool = self.convertToBool(rhs_value);
        _ = c.LLVMBuildBr(self.builder, merge_bb);
        const rhs_end_bb = c.LLVMGetInsertBlock(self.builder);
        c.LLVMPositionBuilderAtEnd(self.builder, merge_bb);
        const bool_type = c.LLVMInt1TypeInContext(self.context);
        const phi = c.LLVMBuildPhi(self.builder, bool_type, "or_result");
        const true_val = c.LLVMConstInt(bool_type, 1, 0);
        var phi_vals = [_]c.LLVMValueRef{ true_val, rhs_bool };
        var phi_blocks = [_]c.LLVMBasicBlockRef{ lhs_bb, rhs_end_bb };
        c.LLVMAddIncoming(phi, &phi_vals[0], &phi_blocks[0], 2);
        return phi;
    }

    fn convertToBool(self: *CodeGenerator, value: c.LLVMValueRef) c.LLVMValueRef {
        const value_type = c.LLVMTypeOf(value);
        const type_kind = c.LLVMGetTypeKind(value_type);
        if (type_kind == c.LLVMIntegerTypeKind and c.LLVMGetIntTypeWidth(value_type) == 1) {
            return value;
        }
        if (type_kind == c.LLVMIntegerTypeKind) {
            const zero = c.LLVMConstInt(value_type, 0, 0);
            return c.LLVMBuildICmp(self.builder, c.LLVMIntNE, value, zero, "tobool");
        }
        if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind) {
            const zero = c.LLVMConstReal(value_type, 0.0);
            return c.LLVMBuildFCmp(self.builder, c.LLVMRealONE, value, zero, "tobool");
        }
        if (type_kind == c.LLVMPointerTypeKind) {
            const null_ptr = c.LLVMConstNull(value_type);
            return c.LLVMBuildICmp(self.builder, c.LLVMIntNE, value, null_ptr, "tobool");
        }
        return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 1, 0);
    }

    fn generateBrainfuck(self: *CodeGenerator, bf: ast.Brainfuck) !c.LLVMValueRef {
        var ctx = bfck.ParseBfContext(self.allocator, bf.code);
        defer ctx.requests.deinit();

        const current_function = self.current_function orelse return errors.CodegenError.TypeMismatch;

        // --- 1. Setup Brainfuck environment ---
        const cell_type = c.LLVMIntTypeInContext(self.context, @intCast(ctx.cell_size));
        const i32_type = c.LLVMInt32TypeInContext(self.context);
        const i8_type = c.LLVMInt8TypeInContext(self.context);
        const i8_ptr_type = c.LLVMPointerType(i8_type, 0);
        const size_t_type = self.libcTypeToLLVM(.size_t_type);

        const tape_len_val = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(ctx.len)), 0);
        const tape = c.LLVMBuildArrayAlloca(self.builder, cell_type, tape_len_val, "bf_tape");

        const ptr = c.LLVMBuildAlloca(self.builder, i32_type, "bf_ptr");
        _ = c.LLVMBuildStore(self.builder, c.LLVMConstInt(i32_type, 0, 0), ptr);

        // Initialize tape with zeros
        const memset_func = try self.declareLibcFunction("memset");
        const tape_i8_ptr = c.LLVMBuildBitCast(self.builder, tape, i8_ptr_type, "tape_i8_ptr");
        const cell_size_bytes = @divTrunc(ctx.cell_size, 8);
        const tape_size_bytes = ctx.len * cell_size_bytes;
        const tape_size_val = c.LLVMConstInt(size_t_type, @as(c_ulonglong, @intCast(tape_size_bytes)), 0);
        var memset_args: [3]c.LLVMValueRef = .{
            tape_i8_ptr,
            c.LLVMConstInt(i32_type, 0, 0),
            tape_size_val,
        };
        _ = c.LLVMBuildCall2(self.builder, c.LLVMGlobalGetValueType(memset_func), memset_func, &memset_args[0], 3, "");

        // --- 2. Load variables into tape ---
        for (ctx.requests.items) |req| {
            const var_info = self.getVariable(req.var_name) orelse {
                std.debug.print("Error: brainfuck block uses undefined variable '{s}'\n", .{req.var_name});
                return errors.CodegenError.UndefinedVariable;
            };

            const type_kind = c.LLVMGetTypeKind(var_info.type_ref);
            if (type_kind != c.LLVMIntegerTypeKind) {
                std.debug.print("Error: brainfuck load requests only support integer variables. '{s}' is not an integer.\n", .{req.var_name});
                return errors.CodegenError.TypeMismatch;
            }

            const var_size_bits = c.LLVMGetIntTypeWidth(var_info.type_ref);
            const var_value = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "var_value");

            // Calculate how many cells this variable needs
            const cells_needed: usize = (var_size_bits + @as(c_uint, @intCast(ctx.cell_size)) - 1) / @as(c_uint, @intCast(ctx.cell_size));

            if (cells_needed == 1) {
                // Variable fits in one cell - store directly
                const cell_index = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(req.load_idx)), 0);
                var gep_indices: [1]c.LLVMValueRef = .{cell_index};
                const cell_ptr = c.LLVMBuildGEP2(self.builder, cell_type, tape, &gep_indices[0], 1, "cell_ptr");

                const cell_value = if (var_size_bits == ctx.cell_size)
                    var_value
                else if (var_size_bits < ctx.cell_size)
                    c.LLVMBuildZExt(self.builder, var_value, cell_type, "zext_to_cell")
                else
                    c.LLVMBuildTrunc(self.builder, var_value, cell_type, "trunc_to_cell");

                _ = c.LLVMBuildStore(self.builder, cell_value, cell_ptr);
            } else {
                // Variable needs multiple cells - split by cell_size chunks (big-endian)
                var cell_idx: usize = 0;
                while (cell_idx < cells_needed) : (cell_idx += 1) {
                    const cell_index = req.load_idx + @as(i32, @intCast(cell_idx));
                    const byte_index = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(cell_index)), 0);
                    var gep_indices: [1]c.LLVMValueRef = .{byte_index};
                    const cell_ptr = c.LLVMBuildGEP2(self.builder, cell_type, tape, &gep_indices[0], 1, "cell_ptr");

                    // Extract the chunk for this cell (big-endian: most significant chunk first)
                    const chunk_pos = cells_needed - 1 - cell_idx;
                    const shift_amount = c.LLVMConstInt(var_info.type_ref, @intCast(chunk_pos * @as(usize, @intCast(ctx.cell_size))), 0);
                    var cell_value: c.LLVMValueRef = undefined;
                    if (@as(c_uint, @intCast(chunk_pos * @as(usize, @intCast(ctx.cell_size)))) >= var_size_bits) {
                        // This chunk is beyond the variable size, store 0
                        cell_value = c.LLVMConstInt(cell_type, 0, 0);
                    } else {
                        const shifted_value = c.LLVMBuildLShr(self.builder, var_value, shift_amount, "shifted");
                        cell_value = c.LLVMBuildTrunc(self.builder, shifted_value, cell_type, "chunk");
                    }

                    _ = c.LLVMBuildStore(self.builder, cell_value, cell_ptr);
                }
            }
        }

        // --- 3. Execute Brainfuck code ---
        const putchar_func = try self.declareLibcFunction("putchar");
        const getchar_func = try self.declareLibcFunction("getchar");

        var loop_stack = std.ArrayList(struct { cond: c.LLVMBasicBlockRef, exit: c.LLVMBasicBlockRef }).init(self.allocator);
        defer loop_stack.deinit();

        // Create constants for cell operations
        const cell_one = c.LLVMConstInt(cell_type, 1, 0);
        const cell_zero = c.LLVMConstInt(cell_type, 0, 0);

        // Create max value for unsigned overflow (2^cell_size - 1)
        // const max_cell_value = if (ctx.cell_size == 64)
        //     c.LLVMConstInt(cell_type, std.math.maxInt(u64), 0)
        // else
        //     c.LLVMConstInt(cell_type, (1 << @intCast(ctx.cell_size)) - 1, 0);

        for (ctx.code) |char| {
            switch (char) {
                '>' => {
                    const ptr_val = c.LLVMBuildLoad2(self.builder, i32_type, ptr, "ptr_val");
                    const new_ptr_val = c.LLVMBuildAdd(self.builder, ptr_val, c.LLVMConstInt(i32_type, 1, 0), "inc_ptr");
                    _ = c.LLVMBuildStore(self.builder, new_ptr_val, ptr);
                },
                '<' => {
                    const ptr_val = c.LLVMBuildLoad2(self.builder, i32_type, ptr, "ptr_val");
                    const new_ptr_val = c.LLVMBuildSub(self.builder, ptr_val, c.LLVMConstInt(i32_type, 1, 0), "dec_ptr");
                    _ = c.LLVMBuildStore(self.builder, new_ptr_val, ptr);
                },
                '+' => {
                    const ptr_val = c.LLVMBuildLoad2(self.builder, i32_type, ptr, "ptr_val");
                    var indices: [1]c.LLVMValueRef = .{ptr_val};
                    const cell_ptr = c.LLVMBuildGEP2(self.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                    const cell_val = c.LLVMBuildLoad2(self.builder, cell_type, cell_ptr, "cell_val");

                    var new_cell_val: c.LLVMValueRef = undefined;
                    if (ctx.cell_signed) {
                        // Signed arithmetic - let LLVM handle overflow
                        new_cell_val = c.LLVMBuildAdd(self.builder, cell_val, cell_one, "inc_cell");
                    } else {
                        // Unsigned arithmetic with proper wrapping
                        new_cell_val = c.LLVMBuildAdd(self.builder, cell_val, cell_one, "inc_cell");
                        // LLVM handles unsigned overflow correctly by default
                    }

                    _ = c.LLVMBuildStore(self.builder, new_cell_val, cell_ptr);
                },
                '-' => {
                    const ptr_val = c.LLVMBuildLoad2(self.builder, i32_type, ptr, "ptr_val");
                    var indices: [1]c.LLVMValueRef = .{ptr_val};
                    const cell_ptr = c.LLVMBuildGEP2(self.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                    const cell_val = c.LLVMBuildLoad2(self.builder, cell_type, cell_ptr, "cell_val");

                    var new_cell_val: c.LLVMValueRef = undefined;
                    if (ctx.cell_signed) {
                        // Signed arithmetic - let LLVM handle overflow
                        new_cell_val = c.LLVMBuildSub(self.builder, cell_val, cell_one, "dec_cell");
                    } else {
                        // Unsigned arithmetic with proper wrapping
                        new_cell_val = c.LLVMBuildSub(self.builder, cell_val, cell_one, "dec_cell");
                        // LLVM handles unsigned underflow correctly by default (0-1 = MAX_VALUE)
                    }

                    _ = c.LLVMBuildStore(self.builder, new_cell_val, cell_ptr);
                },
                '.' => {
                    const ptr_val = c.LLVMBuildLoad2(self.builder, i32_type, ptr, "ptr_val");
                    var indices: [1]c.LLVMValueRef = .{ptr_val};
                    const cell_ptr = c.LLVMBuildGEP2(self.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                    const cell_val = c.LLVMBuildLoad2(self.builder, cell_type, cell_ptr, "cell_val");
                    const char_val = self.castToType(cell_val, i32_type);
                    var putchar_args: [1]c.LLVMValueRef = .{char_val};
                    _ = c.LLVMBuildCall2(self.builder, c.LLVMGlobalGetValueType(putchar_func), putchar_func, &putchar_args[0], 1, "");
                },
                ',' => {
                    const ptr_val = c.LLVMBuildLoad2(self.builder, i32_type, ptr, "ptr_val");
                    var indices: [1]c.LLVMValueRef = .{ptr_val};
                    const cell_ptr = c.LLVMBuildGEP2(self.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                    const input_val = c.LLVMBuildCall2(self.builder, c.LLVMGlobalGetValueType(getchar_func), getchar_func, null, 0, "input");
                    const cell_val = self.castToType(input_val, cell_type);
                    _ = c.LLVMBuildStore(self.builder, cell_val, cell_ptr);
                },
                '[' => {
                    const loop_cond_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "bf_loop_cond");
                    const loop_body_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "bf_loop_body");
                    const loop_exit_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "bf_loop_exit");

                    _ = c.LLVMBuildBr(self.builder, loop_cond_bb);
                    c.LLVMPositionBuilderAtEnd(self.builder, loop_cond_bb);

                    const ptr_val = c.LLVMBuildLoad2(self.builder, i32_type, ptr, "ptr_val");
                    var indices: [1]c.LLVMValueRef = .{ptr_val};
                    const cell_ptr = c.LLVMBuildGEP2(self.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                    const cell_val = c.LLVMBuildLoad2(self.builder, cell_type, cell_ptr, "cell_val");
                    const cond = c.LLVMBuildICmp(self.builder, c.LLVMIntNE, cell_val, cell_zero, "loop_cond");
                    _ = c.LLVMBuildCondBr(self.builder, cond, loop_body_bb, loop_exit_bb);

                    c.LLVMPositionBuilderAtEnd(self.builder, loop_body_bb);
                    try loop_stack.append(.{ .cond = loop_cond_bb, .exit = loop_exit_bb });
                },
                ']' => {
                    if (loop_stack.items.len == 0) {
                        return errors.CodegenError.UnsupportedOperation; // Unmatched ]
                    }
                    const loop_blocks = loop_stack.pop();
                    _ = c.LLVMBuildBr(self.builder, loop_blocks.?.cond);
                    c.LLVMPositionBuilderAtEnd(self.builder, loop_blocks.?.exit);
                },
                else => {}, // Ignore non-brainfuck characters
            }
        }

        // --- 4. Sync variables back from tape ---
        for (ctx.requests.items) |req| {
            const var_info = self.getVariable(req.var_name) orelse continue;

            const type_kind = c.LLVMGetTypeKind(var_info.type_ref);
            if (type_kind != c.LLVMIntegerTypeKind) {
                continue;
            }

            const var_size_bits = c.LLVMGetIntTypeWidth(var_info.type_ref);
            const cells_needed: usize = (var_size_bits + @as(c_uint, @intCast(ctx.cell_size)) - 1) / @as(c_uint, @intCast(ctx.cell_size));

            if (cells_needed == 1) {
                // Variable fits in one cell - load directly
                const cell_index = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(req.load_idx)), 0);
                var gep_indices: [1]c.LLVMValueRef = .{cell_index};
                const cell_ptr = c.LLVMBuildGEP2(self.builder, cell_type, tape, &gep_indices[0], 1, "cell_ptr");
                const cell_val = c.LLVMBuildLoad2(self.builder, cell_type, cell_ptr, "cell_val");

                var reconstructed_value: c.LLVMValueRef = undefined;
                if (var_size_bits == ctx.cell_size) {
                    reconstructed_value = cell_val;
                } else if (var_size_bits < ctx.cell_size) {
                    // Truncate cell value to variable size
                    reconstructed_value = c.LLVMBuildTrunc(self.builder, cell_val, var_info.type_ref, "trunc_from_cell");
                } else {
                    // Zero-extend cell value to variable size
                    reconstructed_value = c.LLVMBuildZExt(self.builder, cell_val, var_info.type_ref, "zext_from_cell");
                }

                _ = c.LLVMBuildStore(self.builder, reconstructed_value, var_info.value);
            } else {
                // Variable needs multiple cells - reconstruct from chunks (big-endian)
                var reconstructed_value = c.LLVMConstInt(var_info.type_ref, 0, 0);

                var cell_idx: usize = 0;
                while (cell_idx < cells_needed) : (cell_idx += 1) {
                    const cell_index = req.load_idx + @as(i32, @intCast(cell_idx));
                    const byte_index = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(cell_index)), 0);
                    var gep_indices: [1]c.LLVMValueRef = .{byte_index};
                    const cell_ptr = c.LLVMBuildGEP2(self.builder, cell_type, tape, &gep_indices[0], 1, "cell_ptr");
                    const cell_val = c.LLVMBuildLoad2(self.builder, cell_type, cell_ptr, "cell_val");

                    // Zero-extend the cell value to the variable type
                    const extended_cell_val = c.LLVMBuildZExt(self.builder, cell_val, var_info.type_ref, "extended");

                    // For big-endian: cell_idx corresponds to chunk at position (cells_needed - 1 - cell_idx)
                    const chunk_pos = cells_needed - 1 - cell_idx;
                    const shift_amount = c.LLVMConstInt(var_info.type_ref, @intCast(chunk_pos * @as(usize, @intCast(ctx.cell_size))), 0);

                    if (@as(c_uint, @intCast(chunk_pos * @as(usize, @intCast(ctx.cell_size)))) < var_size_bits) {
                        if (shift_amount == c.LLVMConstInt(var_info.type_ref, 0, 0)) {
                            // Least significant chunk - just OR it
                            reconstructed_value = c.LLVMBuildOr(self.builder, reconstructed_value, extended_cell_val, "combined");
                        } else {
                            // Shift to the correct position and combine
                            const shifted_cell_val = c.LLVMBuildShl(self.builder, extended_cell_val, shift_amount, "shifted");
                            reconstructed_value = c.LLVMBuildOr(self.builder, reconstructed_value, shifted_cell_val, "combined");
                        }
                    }
                }

                // Store the reconstructed value back to the variable
                _ = c.LLVMBuildStore(self.builder, reconstructed_value, var_info.value);
            }
        }

        return c.LLVMConstInt(i32_type, 0, 0);
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

    pub fn compileToExecutable(self: *CodeGenerator, output_filename: []const u8, arch: []const u8, link_objects: []const []const u8) !void {
        const temp_ir_file = "temp_output.ll";
        try self.writeToFile(temp_ir_file);

        const output_filename_z = try self.allocator.dupeZ(u8, output_filename);
        defer self.allocator.free(output_filename_z);

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        var clang_args_list = std.ArrayList([]const u8).init(arena_alloc);
        try clang_args_list.appendSlice(&[_][]const u8{
            "clang",
            temp_ir_file,
            "-o",
            output_filename,
        });

        if (!std.mem.eql(u8, arch, "")) {
            const march_flag: []const u8 = try std.fmt.allocPrint(arena_alloc, "--target={s}", .{arch});
            try clang_args_list.append(march_flag);
        }

        for (link_objects) |obj| {
            try clang_args_list.append(obj);
        }

        try clang_args_list.append("-lc");

        var child = std.process.Child.init(clang_args_list.items, arena_alloc);
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
