const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
const structs = @import("structs.zig");
const strings = @import("strings.zig");
const modules = @import("modules.zig");
const control_flow = @import("control_flow.zig");
const variables = @import("variables.zig");
const functions = @import("functions.zig");
const numeric = @import("numeric.zig");
const array = @import("array.zig");
const simd = @import("simd.zig");
const enums = @import("enums.zig");
const diagnostics = @import("../diagnostics.zig");
const llvm_tools = @import("../llvm_tools.zig");
const backend = @import("backend.zig");
const casts = @import("casts.zig");
const expressions = @import("expressions.zig");
const error_flow = @import("error_flow.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

pub const CodeGenerator = struct {
    context: c.LLVMContextRef,
    module: c.LLVMModuleRef,
    builder: c.LLVMBuilderRef,
    allocator: std.mem.Allocator,
    functions: std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    variables: std.HashMap([]const u8, structs.VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    external_c_functions: std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    c_function_declarations: std.HashMap([]const u8, bool, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    c_function_param_signatures: std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    regular_functions: std.HashMap([]const u8, bool, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    function_return_types: std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    function_vararg_types: std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    typed_vararg_info: std.HashMap([]const u8, structs.TypedVarargInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    sret_functions: std.HashMap([]const u8, c.LLVMTypeRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    current_function: ?c.LLVMValueRef,
    current_function_return_type: []const u8,
    control_flow_analyzer: control_flow.ControlFlowAnalyzer,
    loop_context_stack: std.ArrayList(structs.LoopContext),
    variable_scopes: std.ArrayList(std.HashMap([]const u8, structs.VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage)),
    uses_float_modulo: bool,
    global_string_counter: usize,

    struct_types: std.HashMap([]const u8, c.LLVMTypeRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    struct_fields: std.HashMap([]const u8, std.HashMap([]const u8, c_uint, std.hash_map.StringContext, std.hash_map.default_max_load_percentage), std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    struct_declarations: std.HashMap([]const u8, ast.StructDecl, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    module_manager: modules.ModuleManager,
    label_blocks: std.HashMap([]const u8, c.LLVMBasicBlockRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    pending_gotos: std.ArrayList(structs.PendingGoto),
    current_line: usize,
    current_column: usize,
    current_token_text: ?[]const u8,
    emitted_error: bool,
    optimize_enabled: bool,
    template_substitutions: ?std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    function_overloads: std.HashMap([]const u8, std.ArrayList(structs.FunctionOverload), std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    pending_template_instantiations: std.ArrayList(structs.TemplateInstantiation),
    error_codes: std.HashMap([]const u8, i32, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    next_error_code: i32,
    last_error_global: ?c.LLVMValueRef,
    last_error_mode_global: ?c.LLVMValueRef,
    solicit_callback_global: ?c.LLVMValueRef,
    solicit_var_slots: std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    solicit_capture_slots: std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    next_solicit_callback_id: usize,
    function_asts: std.HashMap([]const u8, ast.Function, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    current_source_function_name: ?[]const u8,
    current_codegen_function_name: ?[]const u8,
    deferred_actions: std.ArrayList(DeferredAction),
    defer_scope_markers: std.ArrayList(usize),
    pending_global_inits: std.ArrayList(PendingGlobalInit),

    const DeferredAction = struct {
        expression: *ast.Node,
        active_flag_ptr: c.LLVMValueRef,
    };

    const PendingGlobalInit = struct {
        global_name: []const u8,
        global_var: c.LLVMValueRef,
        initializer: *ast.Node,
        type_ref: c.LLVMTypeRef,
        type_name: []const u8,
    };

    pub const BackendTiming = struct {
        pub const LinkBackend = enum {
            unknown,
            zig_cc,
            lld,
            clang,
        };

        opt_time_ns: u64 = 0,
        llc_time_ns: u64 = 0,
        link_time_ns: u64 = 0,
        llc_version_major: i16 = 0,
        opt_version_major: i16 = 0,
        clang_version_major: i16 = 0,
        lld_version_major: i16 = 0,
        link_backend: LinkBackend = .unknown,
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

        var variable_scopes: std.ArrayList(std.HashMap([]const u8, structs.VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage)) = .empty;
        try variable_scopes.append(allocator, std.HashMap([]const u8, structs.VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator));
        var defer_scope_markers: std.ArrayList(usize) = .empty;
        try defer_scope_markers.append(allocator, 0);

        return CodeGenerator{
            .context = context,
            .module = module,
            .builder = builder,
            .allocator = allocator,
            .functions = std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .variables = std.HashMap([]const u8, structs.VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .external_c_functions = std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .c_function_declarations = std.HashMap([]const u8, bool, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .c_function_param_signatures = std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .regular_functions = std.HashMap([]const u8, bool, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .function_return_types = std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .function_vararg_types = std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .typed_vararg_info = std.HashMap([]const u8, structs.TypedVarargInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .sret_functions = std.HashMap([]const u8, c.LLVMTypeRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .current_function = null,
            .current_function_return_type = "",
            .control_flow_analyzer = control_flow_analyzer,
            .loop_context_stack = .empty,
            .variable_scopes = variable_scopes,
            .uses_float_modulo = false,
            .global_string_counter = 0,
            .struct_types = std.HashMap([]const u8, c.LLVMTypeRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .struct_fields = std.HashMap([]const u8, std.HashMap([]const u8, c_uint, std.hash_map.StringContext, std.hash_map.default_max_load_percentage), std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .struct_declarations = std.HashMap([]const u8, ast.StructDecl, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .module_manager = modules.ModuleManager.init(allocator),
            .label_blocks = std.HashMap([]const u8, c.LLVMBasicBlockRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .pending_gotos = .empty,
            .current_line = 0,
            .current_column = 0,
            .current_token_text = null,
            .emitted_error = false,
            .optimize_enabled = false,
            .template_substitutions = null,
            .function_overloads = std.HashMap([]const u8, std.ArrayList(structs.FunctionOverload), std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .pending_template_instantiations = .empty,
            .error_codes = std.HashMap([]const u8, i32, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .next_error_code = 1,
            .last_error_global = null,
            .last_error_mode_global = null,
            .solicit_callback_global = null,
            .solicit_var_slots = std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .solicit_capture_slots = std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .next_solicit_callback_id = 1,
            .function_asts = std.HashMap([]const u8, ast.Function, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .current_source_function_name = null,
            .current_codegen_function_name = null,
            .deferred_actions = .empty,
            .defer_scope_markers = defer_scope_markers,
            .pending_global_inits = .empty,
        };
    }

    pub fn deinit(self: *CodeGenerator) void {
        self.functions.deinit();
        self.variables.deinit();
        self.external_c_functions.deinit();
        self.c_function_declarations.deinit();
        self.c_function_param_signatures.deinit();
        self.regular_functions.deinit();
        self.function_return_types.deinit();
        self.function_vararg_types.deinit();
        self.typed_vararg_info.deinit();
        self.sret_functions.deinit();
        self.loop_context_stack.deinit(self.allocator);
        self.label_blocks.deinit();
        self.pending_gotos.deinit(self.allocator);
        self.struct_types.deinit();
        var struct_fields_iter = self.struct_fields.iterator();
        while (struct_fields_iter.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.struct_fields.deinit();
        self.struct_declarations.deinit();

        var overload_iter = self.function_overloads.iterator();
        while (overload_iter.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.function_overloads.deinit();
        self.error_codes.deinit();
        self.solicit_var_slots.deinit();
        self.solicit_capture_slots.deinit();
        self.function_asts.deinit();
        self.deferred_actions.deinit(self.allocator);
        self.defer_scope_markers.deinit(self.allocator);
        self.pending_global_inits.deinit(self.allocator);

        self.module_manager.deinit();
        for (self.variable_scopes.items) |*scope| {
            scope.deinit();
        }
        self.variable_scopes.deinit(self.allocator);
        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.context);
    }

    pub fn registerModule(self: *CodeGenerator, module_name: []const u8, full_path: []const u8, deps: []const []const u8) !void {
        try self.module_manager.registerModule(module_name, full_path, deps);
    }

    fn reportError(self: *CodeGenerator, message: []const u8, hint: ?[]const u8) void {
        self.emitted_error = true;
        const file_path = self.module_manager.getCurrentModulePath();
        diagnostics.printDiagnostic(self.allocator, .{
            .file_path = file_path,
            .line = self.current_line,
            .column = self.current_column,
            .message = message,
            .hint = hint,
            .severity = .Error,
            .token_text = self.current_token_text,
        });
    }

    pub fn tokenTextForNode(node: *ast.Node) ?[]const u8 {
        return switch (node.data) {
            .identifier => |ident| ident.name,
            .qualified_identifier => |qual_id| qual_id.field,
            .var_decl => |decl| decl.name,
            .function => |func| func.name,
            .function_call => |call| call.name,
            .method_call => |call| call.method_name,
            .c_function_decl => |decl| decl.name,
            .enum_decl => |decl| decl.name,
            .struct_decl => |decl| decl.name,
            .struct_initializer => |struct_init| struct_init.struct_name,
            .use_stmt => |u| u.module_path,
            .goto_stmt => |g| g.label,
            .label_stmt => |l| l.label,
            .number_literal => |lit| lit.value,
            .float_literal => |lit| lit.value,
            .string_literal => |lit| lit.value,
            .assignment => |as| tokenTextForNode(as.target),
            .compound_assignment => |as| tokenTextForNode(as.target),
            .array_index => |idx| tokenTextForNode(idx.array),
            .array_assignment => |as| tokenTextForNode(as.array),
            .array_compound_assignment => |as| tokenTextForNode(as.array),
            .simd_index => |idx| tokenTextForNode(idx.simd),
            .simd_assignment => |as| tokenTextForNode(as.simd),
            .simd_compound_assignment => |as| tokenTextForNode(as.simd),
            .unary_op => |un| tokenTextForNode(un.operand),
            .cast => |cast_expr| tokenTextForNode(cast_expr.expr),
            .expression_block => |block| tokenTextForNode(block.result),
            .error_decl => |decl| decl.name,
            .send_stmt => |send_stmt| send_stmt.error_name,
            .solicit_stmt => |solicit_stmt| solicit_stmt.error_name,
            .defer_stmt => |defer_stmt| tokenTextForNode(defer_stmt.expression),
            .handled_call_stmt => |handled| tokenTextForNode(handled.call),
            else => null,
        };
    }

    fn setCurrentNodeContext(self: *CodeGenerator, node: *ast.Node) void {
        self.current_line = node.line;
        self.current_column = node.column;
        self.current_token_text = tokenTextForNode(node);
    }

    pub fn reportErrorFmt(self: *CodeGenerator, comptime fmt: []const u8, args: anytype, hint: ?[]const u8) void {
        const msg = std.fmt.allocPrint(self.allocator, fmt, args) catch return;
        defer self.allocator.free(msg);
        self.reportError(msg, hint);
    }

    /// Convert an LLVM type to a human-readable string
    pub fn typeToString(self: *CodeGenerator, llvm_type: c.LLVMTypeRef) []const u8 {
        return self.getTypeNameFromLLVMType(llvm_type);
    }

    /// Report a type mismatch error with detailed type information
    pub fn reportTypeMismatch(self: *CodeGenerator, expected_type: c.LLVMTypeRef, actual_type: c.LLVMTypeRef, context: []const u8) void {
        const expected_str = self.typeToString(expected_type);
        const actual_str = self.typeToString(actual_type);

        const msg = std.fmt.allocPrint(self.allocator, "Type mismatch in {s}: expected '{s}' but got '{s}'", .{ context, expected_str, actual_str }) catch {
            self.reportError("Type mismatch", null);
            return;
        };
        defer self.allocator.free(msg);

        self.reportError(msg, "Consider using an explicit cast if this conversion is intended");

        // Clean up type strings if they were allocated
        const expected_kind = c.LLVMGetTypeKind(expected_type);
        const actual_kind = c.LLVMGetTypeKind(actual_type);

        if (expected_kind == c.LLVMIntegerTypeKind or expected_kind == c.LLVMPointerTypeKind or
            expected_kind == c.LLVMVoidTypeKind or expected_kind == c.LLVMFloatTypeKind or
            expected_kind == c.LLVMDoubleTypeKind)
        {
            // These are static strings, don't free
        } else {
            self.allocator.free(expected_str);
        }

        if (actual_kind == c.LLVMIntegerTypeKind or actual_kind == c.LLVMPointerTypeKind or
            actual_kind == c.LLVMVoidTypeKind or actual_kind == c.LLVMFloatTypeKind or
            actual_kind == c.LLVMDoubleTypeKind)
        {
            // These are static strings, don't free
        } else {
            self.allocator.free(actual_str);
        }
    }

    /// Report a type mismatch error with a custom expected type description
    pub fn reportTypeMismatchStr(self: *CodeGenerator, expected_desc: []const u8, actual_type: c.LLVMTypeRef, context: []const u8) void {
        const actual_str = self.typeToString(actual_type);

        const msg = std.fmt.allocPrint(self.allocator, "Type mismatch in {s}: expected {s} but got '{s}'", .{ context, expected_desc, actual_str }) catch {
            self.reportError("Type mismatch", null);
            return;
        };
        defer self.allocator.free(msg);

        self.reportError(msg, "Check the types of the operands");

        // Clean up type string if allocated
        const actual_kind = c.LLVMGetTypeKind(actual_type);
        if (actual_kind != c.LLVMIntegerTypeKind and actual_kind != c.LLVMPointerTypeKind and
            actual_kind != c.LLVMVoidTypeKind and actual_kind != c.LLVMFloatTypeKind and
            actual_kind != c.LLVMDoubleTypeKind)
        {
            self.allocator.free(actual_str);
        }
    }

    /// Report a generic type mismatch error with just a context
    pub fn reportTypeMismatchGeneric(self: *CodeGenerator, context: []const u8, hint: ?[]const u8) void {
        const msg = std.fmt.allocPrint(self.allocator, "Type mismatch in {s}", .{context}) catch {
            self.reportError("Type mismatch", hint);
            return;
        };
        defer self.allocator.free(msg);
        self.reportError(msg, hint);
    }

    pub fn emitMemcpy(self: *CodeGenerator, dst_ptr: c.LLVMValueRef, src_ptr: c.LLVMValueRef, ty: c.LLVMTypeRef) !void {
        const memcpy_func = try self.declareLibcFunction("memcpy");
        const i8_ty = c.LLVMInt8TypeInContext(self.context);
        const i8_ptr_ty = c.LLVMPointerType(i8_ty, 0);
        const size_ty = utils.libcTypeToLLVM(self, .size_t_type);
        const dst_i8 = c.LLVMBuildBitCast(self.builder, dst_ptr, i8_ptr_ty, "memcpy_dst");
        const src_i8 = c.LLVMBuildBitCast(self.builder, src_ptr, i8_ptr_ty, "memcpy_src");
        const size = c.LLVMABISizeOfType(c.LLVMGetModuleDataLayout(self.module), ty);
        const size_val = c.LLVMConstInt(size_ty, @as(c_ulonglong, @intCast(size)), 0);
        var args = [_]c.LLVMValueRef{ dst_i8, src_i8, size_val };
        _ = c.LLVMBuildCall2(self.builder, c.LLVMGlobalGetValueType(memcpy_func), memcpy_func, &args[0], 3, "");
    }

    pub fn registerFunctionModule(self: *CodeGenerator, func_name: []const u8, module_name: []const u8) !void {
        try self.module_manager.registerFunctionModule(func_name, module_name);
    }

    pub fn registerGlobalModule(self: *CodeGenerator, global_name: []const u8, module_name: []const u8) !void {
        try self.module_manager.registerGlobalModule(global_name, module_name);
    }

    pub fn setCurrentModule(self: *CodeGenerator, module_name: []const u8) void {
        self.module_manager.setCurrentModule(module_name);
    }

    pub fn setCurrentModuleByFunction(self: *CodeGenerator, func_name: []const u8) void {
        self.module_manager.setCurrentModuleByFunction(func_name);
    }

    pub fn setCurrentModuleByGlobal(self: *CodeGenerator, global_name: []const u8) void {
        self.module_manager.setCurrentModuleByGlobal(global_name);
    }

    pub fn canAccess(self: *CodeGenerator, from_module: []const u8, target_module: []const u8) bool {
        return self.module_manager.canAccess(from_module, target_module);
    }

    pub fn buildAllocaAtEntry(self: *CodeGenerator, llvm_type: c.LLVMTypeRef, name: []const u8) c.LLVMValueRef {
        const current_block = c.LLVMGetInsertBlock(self.builder);
        if (current_block == null) {
            return c.LLVMBuildAlloca(self.builder, llvm_type, name.ptr);
        }

        const current_fn = c.LLVMGetBasicBlockParent(current_block);
        if (current_fn == null) {
            return c.LLVMBuildAlloca(self.builder, llvm_type, name.ptr);
        }

        const entry_block = c.LLVMGetEntryBasicBlock(current_fn);
        if (entry_block == null) {
            return c.LLVMBuildAlloca(self.builder, llvm_type, name.ptr);
        }

        const temp_builder = c.LLVMCreateBuilderInContext(self.context);
        defer c.LLVMDisposeBuilder(temp_builder);

        if (c.LLVMGetFirstInstruction(entry_block)) |first_inst| {
            c.LLVMPositionBuilderBefore(temp_builder, first_inst);
        } else {
            c.LLVMPositionBuilderAtEnd(temp_builder, entry_block);
        }

        return c.LLVMBuildAlloca(temp_builder, llvm_type, name.ptr);
    }

    fn buildStoreAtEntry(self: *CodeGenerator, ptr: c.LLVMValueRef, value: c.LLVMValueRef) void {
        const current_fn = self.current_function orelse return;
        const entry_block = c.LLVMGetEntryBasicBlock(current_fn);
        if (entry_block == null) return;
        const temp_builder = c.LLVMCreateBuilderInContext(self.context);
        defer c.LLVMDisposeBuilder(temp_builder);
        if (c.LLVMGetBasicBlockTerminator(entry_block)) |term| {
            c.LLVMPositionBuilderBefore(temp_builder, term);
        } else {
            c.LLVMPositionBuilderAtEnd(temp_builder, entry_block);
        }
        _ = c.LLVMBuildStore(temp_builder, value, ptr);
    }

    fn registerDeferredExpression(self: *CodeGenerator, expression: *ast.Node) errors.CodegenError!void {
        const i1_ty = c.LLVMInt1TypeInContext(self.context);
        const name = try std.fmt.allocPrint(self.allocator, "defer_active_{d}", .{self.deferred_actions.items.len});
        defer self.allocator.free(name);
        const flag_ptr = self.buildAllocaAtEntry(i1_ty, name);
        self.buildStoreAtEntry(flag_ptr, c.LLVMConstInt(i1_ty, 0, 0));
        _ = c.LLVMBuildStore(self.builder, c.LLVMConstInt(i1_ty, 1, 0), flag_ptr);
        try self.deferred_actions.append(self.allocator, .{
            .expression = expression,
            .active_flag_ptr = flag_ptr,
        });
    }

    pub fn runDeferredActionsFrom(self: *CodeGenerator, from_index: usize) errors.CodegenError!void {
        if (self.deferred_actions.items.len == 0) return;
        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) != null) return;
        const current_fn = self.current_function orelse return errors.CodegenError.TypeMismatch;
        const i1_ty = c.LLVMInt1TypeInContext(self.context);
        var i = self.deferred_actions.items.len;
        while (i > 0) {
            i -= 1;
            if (i < from_index) break;
            const action = self.deferred_actions.items[i];
            const is_active = c.LLVMBuildLoad2(self.builder, i1_ty, action.active_flag_ptr, "defer_active");
            const run_bb = c.LLVMAppendBasicBlockInContext(self.context, current_fn, "defer.run");
            const next_bb = c.LLVMAppendBasicBlockInContext(self.context, current_fn, "defer.next");
            _ = c.LLVMBuildCondBr(self.builder, is_active, run_bb, next_bb);
            c.LLVMPositionBuilderAtEnd(self.builder, run_bb);
            self.setCurrentNodeContext(action.expression);
            _ = try self.generateExpression(action.expression);
            _ = c.LLVMBuildStore(self.builder, c.LLVMConstInt(i1_ty, 0, 0), action.active_flag_ptr);
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                _ = c.LLVMBuildBr(self.builder, next_bb);
            }
            c.LLVMPositionBuilderAtEnd(self.builder, next_bb);
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) != null) return;
        }
    }

    pub fn runDeferredActions(self: *CodeGenerator) errors.CodegenError!void {
        return self.runDeferredActionsFrom(0);
    }

    fn buildQualifiedName(self: *CodeGenerator, node: *ast.Node) ![]const u8 {
        switch (node.data) {
            .identifier => |ident| {
                return utils.dupe(u8, self.allocator, ident.name);
            },
            .qualified_identifier => |qual_id| {
                const base_name = try self.buildQualifiedName(qual_id.base);
                defer self.allocator.free(base_name);
                return try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ base_name, qual_id.field });
            },
            .array_index => |arr_idx| {
                return try self.buildQualifiedName(arr_idx.array);
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    pub fn getBaseIdentifierName(self: *CodeGenerator, node: *ast.Node) ![]const u8 {
        switch (node.data) {
            .identifier => |ident| return utils.dupe(u8, self.allocator, ident.name),
            .qualified_identifier => |qual_id| return try self.getBaseIdentifierName(qual_id.base),
            .array_index => |arr_idx| return try self.getBaseIdentifierName(arr_idx.array),
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    pub const containsArrayIndex = array.containsArrayIndex;

    pub const extractArrayIndex = array.extractArrayIndex;

    pub const collectArrayIndices = array.collectArrayIndices;

    fn getQualifiedPointerAndType(self: *CodeGenerator, qual_node: *ast.Node) errors.CodegenError!struct { ptr: c.LLVMValueRef, ty: c.LLVMTypeRef } {
        if (qual_node.data != .qualified_identifier) return errors.CodegenError.TypeMismatch;
        const qual_id = qual_node.data.qualified_identifier;
        var base_ptr: c.LLVMValueRef = undefined;
        var base_ty: c.LLVMTypeRef = undefined;

        switch (qual_id.base.data) {
            .identifier => {
                const base_name = qual_id.base.data.identifier.name;
                const var_info = CodeGenerator.getVariable(self, base_name) orelse return errors.CodegenError.UndefinedVariable;
                base_ptr = var_info.value;
                base_ty = var_info.type_ref;
            },
            .array_index => {
                const arr_idx = qual_id.base.data.array_index;
                const array_name = try self.getBaseIdentifierName(qual_id.base);
                defer self.allocator.free(array_name);
                const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;
                var collected_indices: std.ArrayList(c.LLVMValueRef) = .empty;
                defer collected_indices.deinit(self.allocator);
                const base_node = try self.collectArrayIndices(arr_idx.array, &collected_indices);
                _ = base_node;
                const index_value = try self.generateExpression(arr_idx.index);
                try collected_indices.append(self.allocator, index_value);

                var all_indices: std.ArrayList(c.LLVMValueRef) = .empty;
                defer all_indices.deinit(self.allocator);
                try all_indices.append(self.allocator, c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0));

                var i: usize = collected_indices.items.len;
                while (i > 0) {
                    i -= 1;
                    try all_indices.append(self.allocator, collected_indices.items[i]);
                }

                base_ptr = c.LLVMBuildGEP2(self.builder, var_info.type_ref, var_info.value, all_indices.items.ptr, @intCast(all_indices.items.len), "array_element_ptr");
                base_ty = var_info.type_ref;
                for (0..collected_indices.items.len) |_| {
                    base_ty = c.LLVMGetElementType(base_ty);
                }
            },
            .qualified_identifier => {
                const inner = try self.getQualifiedPointerAndType(qual_id.base);
                base_ptr = inner.ptr;
                base_ty = inner.ty;
            },
            else => return errors.CodegenError.TypeMismatch,
        }

        var struct_ty = base_ty;
        if (c.LLVMGetTypeKind(struct_ty) == c.LLVMPointerTypeKind) {
            struct_ty = c.LLVMGetElementType(struct_ty);
        }

        if (c.LLVMGetTypeKind(struct_ty) != c.LLVMStructTypeKind) {
            return errors.CodegenError.TypeMismatch;
        }

        const struct_name = try self.getStructTypeName(struct_ty);
        defer self.allocator.free(struct_name);
        const field_map = self.struct_fields.get(struct_name) orelse return errors.CodegenError.TypeMismatch;
        const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
        const struct_ptr = base_ptr;
        if (c.LLVMGetTypeKind(c.LLVMTypeOf(struct_ptr)) != c.LLVMPointerTypeKind or c.LLVMGetTypeKind(c.LLVMGetElementType(c.LLVMTypeOf(struct_ptr))) != c.LLVMStructTypeKind) {
            return errors.CodegenError.TypeMismatch;
        }

        const field_ptr = try self.getStructFieldPointer(struct_ty, struct_ptr, field_index);
        const field_ty = c.LLVMStructGetTypeAtIndex(struct_ty, field_index);

        return .{ .ptr = field_ptr, .ty = field_ty };
    }

    pub fn getQualifiedFieldPtrAndType(self: *CodeGenerator, qual_node: *ast.Node) errors.CodegenError!struct { ptr: c.LLVMValueRef, ty: c.LLVMTypeRef } {
        if (qual_node.data != .qualified_identifier) {
            return errors.CodegenError.TypeMismatch;
        }
        const qual_id = qual_node.data.qualified_identifier;
        var base_val: c.LLVMValueRef = undefined;
        var base_ty: c.LLVMTypeRef = undefined;

        switch (qual_id.base.data) {
            .identifier => {
                const base_name = qual_id.base.data.identifier.name;
                const var_info = CodeGenerator.getVariable(self, base_name) orelse return errors.CodegenError.UndefinedVariable;
                if (std.mem.startsWith(u8, var_info.type_name, "ptr<") and std.mem.endsWith(u8, var_info.type_name, ">")) {
                    base_val = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_ptr_for_field");
                    const inner_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                    base_ty = try self.getLLVMType(inner_type_name);
                } else {
                    base_val = var_info.value;
                    base_ty = var_info.type_ref;
                }
            },
            .array_index => {
                const arr_idx = qual_id.base.data.array_index;
                const array_name = try self.getBaseIdentifierName(qual_id.base);
                defer self.allocator.free(array_name);
                const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;
                var index_value = try self.generateExpression(arr_idx.index);
                index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
                var indices = [_]c.LLVMValueRef{
                    c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0),
                    index_value,
                };
                base_val = c.LLVMBuildGEP2(self.builder, var_info.type_ref, var_info.value, &indices[0], 2, "array_element_ptr");
                base_ty = c.LLVMGetElementType(var_info.type_ref);
            },
            .qualified_identifier => {
                const inner = try self.getQualifiedFieldPtrAndType(qual_id.base);
                base_val = inner.ptr;
                base_ty = inner.ty;
            },
            else => return errors.CodegenError.TypeMismatch,
        }
        var struct_ptr = base_val;
        var struct_ty = base_ty;
        switch (c.LLVMGetTypeKind(struct_ty)) {
            c.LLVMPointerTypeKind => {
                const pointee = c.LLVMGetElementType(struct_ty);
                if (c.LLVMGetTypeKind(pointee) != c.LLVMStructTypeKind) return errors.CodegenError.TypeMismatch;
                struct_ty = pointee;
            },
            c.LLVMStructTypeKind => {
                if (c.LLVMGetTypeKind(c.LLVMTypeOf(struct_ptr)) != c.LLVMPointerTypeKind) {
                    const tmp = self.buildAllocaAtEntry(struct_ty, "tmp_struct");
                    _ = c.LLVMBuildStore(self.builder, struct_ptr, tmp);
                    struct_ptr = tmp;
                }
            },
            else => return errors.CodegenError.TypeMismatch,
        }

        const struct_name = try self.getStructTypeName(struct_ty);
        defer self.allocator.free(struct_name);
        const field_map = self.struct_fields.get(struct_name) orelse return errors.CodegenError.TypeMismatch;
        const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
        const field_ptr = try self.getStructFieldPointer(struct_ty, struct_ptr, field_index);
        const field_ty = c.LLVMStructGetTypeAtIndex(struct_ty, field_index);
        return .{ .ptr = field_ptr, .ty = field_ty };
    }

    pub const generateArrayElementFieldAssignment = array.generateArrayElementFieldAssignment;

    pub const getStructTypeName = structs.getStructTypeName;

    pub const generateRecursiveFieldAssignment = structs.generateRecursiveFieldAssignment;

    pub fn currentDeferMarker(self: *CodeGenerator) usize {
        if (self.defer_scope_markers.items.len == 0) return 0;
        return self.defer_scope_markers.items[self.defer_scope_markers.items.len - 1];
    }

    pub fn pushScope(self: *CodeGenerator) errors.CodegenError!void {
        try variables.pushScope(self);
        try self.defer_scope_markers.append(self.allocator, self.deferred_actions.items.len);
    }

    pub fn popScope(self: *CodeGenerator) void {
        var marker: usize = self.deferred_actions.items.len;
        if (self.defer_scope_markers.items.len > 1) {
            marker = self.defer_scope_markers.pop().?;
        }
        if (self.deferred_actions.items.len > marker) {
            self.deferred_actions.shrinkRetainingCapacity(marker);
        }
        variables.popScope(self);
    }

    pub fn clearCurrentFunctionScopes(self: *CodeGenerator) void {
        variables.clearCurrentFunctionScopes(self);
        self.deferred_actions.clearRetainingCapacity();
        self.defer_scope_markers.clearRetainingCapacity();
        self.defer_scope_markers.append(self.allocator, 0) catch {};
    }

    pub const getVariable = variables.getVariable;

    pub const putVariable = variables.putVariable;

    pub const variableExistsInCurrentScope = variables.variableExistsInCurrentScope;

    pub const declareLibcFunction = functions.declareLibcFunction;

    pub const declareFunction = functions.declareFunction;

    pub const generateFunctionBody = functions.generateFunctionBody;

    pub const generateCFunctionDeclaration = functions.generateCFunctionDeclaration;

    pub const generateFunctionCall = functions.generateFunctionCall;

    pub const libcTypeToLLVM = functions.libcTypeToLLVM;

    pub const generateRecursiveFieldAccess = structs.generateRecursiveFieldAccess;

    pub const generateStructFieldAssignment = structs.generateStructFieldAssignment;

    pub fn generateExpressionFromString(self: *CodeGenerator, expr_str: []const u8) errors.CodegenError!c.LLVMValueRef {
        if (numeric.parseNumericLiteral(expr_str)) |value| {
            return c.LLVMConstInt(c.LLVMInt64TypeInContext(self.context), @as(c_ulonglong, @intCast(value)), 0);
        } else |_| {
            return errors.CodegenError.TypeMismatch;
        }
    }

    fn generateRegularVariableAssignment(self: *CodeGenerator, as: ast.Assignment) errors.CodegenError!void {
        const ident = as.target.data.identifier;
        const var_info = CodeGenerator.getVariable(self, ident.name) orelse return errors.CodegenError.UndefinedVariable;

        if (var_info.is_const) {
            if (self.current_line > 0) {
                self.reportErrorFmt("Cannot reassign const variable '{s}'", .{ident.name}, "Variable is declared as const");
            } else {
                self.reportErrorFmt("Cannot reassign const variable '{s}'", .{ident.name}, "Variable is declared as const");
            }
            return errors.CodegenError.ConstReassignment;
        }

        const var_type_kind = c.LLVMGetTypeKind(@ptrCast(var_info.type_ref));
        if (var_type_kind == c.LLVMArrayTypeKind) {
            try array.generateArrayReassignment(self, ident.name, as.value);
        } else {
            if (var_type_kind == c.LLVMStructTypeKind and as.value.data == .array_initializer) {
                const arr = as.value.data.array_initializer;
                if (arr.elements.items.len == 1 and arr.elements.items[0].data == .number_literal and std.mem.eql(u8, arr.elements.items[0].data.number_literal.value, "0")) {
                    _ = c.LLVMBuildStore(self.builder, c.LLVMConstNull(var_info.type_ref), var_info.value);
                    return;
                }
            }

            if (var_type_kind == c.LLVMStructTypeKind and as.value.data == .identifier) {
                const src_name = as.value.data.identifier.name;
                if (CodeGenerator.getVariable(self, src_name)) |src_var| {
                    if (std.mem.eql(u8, src_var.type_name, var_info.type_name)) {
                        try self.emitMemcpy(var_info.value, src_var.value, var_info.type_ref);
                        return;
                    }
                }
            }

            const value_raw = try self.generateExpressionWithContext(as.value, var_info.type_name);
            if (var_type_kind == c.LLVMStructTypeKind and
                (as.value.data == .function_call or as.value.data == .method_call) and
                c.LLVMGetTypeKind(c.LLVMTypeOf(value_raw)) == c.LLVMPointerTypeKind)
            {
                try self.emitMemcpy(var_info.value, value_raw, var_info.type_ref);
                return;
            }
            const final_value = try self.castWithSourceRulesNamed(value_raw, @ptrCast(var_info.type_ref), as.value, var_info.type_name);
            _ = c.LLVMBuildStore(@ptrCast(self.builder), @ptrCast(final_value), @ptrCast(var_info.value));
        }
    }

    pub const getStructFieldPointer = structs.getStructFieldPointer;

    fn assignToField(self: *CodeGenerator, field_ptr: c.LLVMValueRef, field_type: c.LLVMTypeRef, value_expr: *ast.Node) errors.CodegenError!void {
        const field_type_kind = c.LLVMGetTypeKind(field_type);
        if (value_expr.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
            try self.assignStringLiteralToArrayField(field_ptr, field_type, value_expr.data.string_literal);
            return;
        }
        const value_raw = try self.generateExpression(value_expr);
        const final_val = try self.castWithRules(value_raw, field_type, value_expr);
        const store_instr = c.LLVMBuildStore(self.builder, final_val, field_ptr);
        const alignment = self.getAlignmentForType(field_type);
        c.LLVMSetAlignment(store_instr, alignment);
    }

    pub const assignStringLiteralToArrayField = structs.assignStringLiteralToArrayField;

    pub const generateStructFieldAccess = structs.generateStructFieldAccess;

    pub const getLLVMStructType = structs.getLLVMStructType;

    pub const getLLVMType = utils.getLLVMType;

    pub const isUnsignedType = utils.isUnsignedType;

    pub const getAlignmentForType = utils.getAlignmentForType;

    pub const getTypeNameFromLLVMType = utils.getTypeNameFromLLVMType;

    pub const generateStructType = structs.generateStructType;

    pub const generateStructInitializer = structs.generateStructInitializer;

    pub const getStructDecl = structs.getStructDecl;

    pub fn generateCode(self: *CodeGenerator, program: *ast.Node) errors.CodegenError!void {
        self.emitted_error = false;
        switch (program.data) {
            .program => |prog| {
                try error_flow.registerBuiltinErrorGuard(self);
                for (prog.functions.items) |func| {
                    self.setCurrentNodeContext(func);
                    if (func.data == .error_decl) {
                        const error_decl = func.data.error_decl;
                        if (error_decl.code_kind != .alias) {
                            try error_flow.registerErrorDeclaration(self, error_decl);
                        }
                    }
                }
                for (prog.functions.items) |func| {
                    self.setCurrentNodeContext(func);
                    if (func.data == .error_decl) {
                        const error_decl = func.data.error_decl;
                        if (error_decl.code_kind == .alias) {
                            try error_flow.registerErrorDeclaration(self, error_decl);
                        }
                    }
                }
                for (prog.functions.items) |func| {
                    self.setCurrentNodeContext(func);
                    if (func.data == .struct_decl) {
                        const struct_decl = func.data.struct_decl;
                        if (self.struct_types.get(struct_decl.name) == null) {
                            const struct_name_z = utils.dupeZ(self.allocator, struct_decl.name);
                            defer self.allocator.free(struct_name_z);
                            const struct_type = c.LLVMStructCreateNamed(@ptrCast(self.context), struct_name_z.ptr);
                            try self.struct_types.put(struct_decl.name, @ptrCast(struct_type));
                        }
                    }
                }
                for (prog.functions.items) |func| {
                    self.setCurrentNodeContext(func);
                    if (func.data == .enum_decl) {
                        try enums.generateEnumDeclaration(self, func.data.enum_decl);
                    } else if (func.data == .struct_decl) {
                        try self.generateStructType(func.data.struct_decl);
                    } else if (func.data == .c_function_decl) {
                        try self.generateCFunctionDeclaration(func.data.c_function_decl);
                    } else if (func.data == .function) {
                        try self.function_asts.put(func.data.function.name, func.data.function);
                    }
                }
                for (prog.functions.items) |func| {
                    self.setCurrentNodeContext(func);
                    if (func.data == .function) {
                        try self.declareFunction(func.data.function);
                    }
                }
                for (prog.globals.items) |glob| {
                    self.setCurrentNodeContext(glob);
                    try self.generateGlobalDeclaration(glob);
                }
                try self.generatePendingGlobalInitializers();
                for (prog.functions.items) |func| {
                    self.setCurrentNodeContext(func);
                    if (func.data == .function) try self.generateFunctionBody(func.data.function);
                }
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    fn generatePendingGlobalInitializers(self: *CodeGenerator) errors.CodegenError!void {
        if (self.pending_global_inits.items.len == 0) return;

        const fn_ty = c.LLVMFunctionType(c.LLVMVoidTypeInContext(self.context), null, 0, 0);
        const init_fn = c.LLVMAddFunction(self.module, "__zlang_global_init", fn_ty);
        c.LLVMSetLinkage(init_fn, c.LLVMInternalLinkage);

        const entry = c.LLVMAppendBasicBlockInContext(self.context, init_fn, "entry");
        c.LLVMPositionBuilderAtEnd(self.builder, entry);

        const prev_fn = self.current_function;
        const prev_ret = self.current_function_return_type;
        self.current_function = init_fn;
        self.current_function_return_type = "void";
        defer {
            self.current_function = prev_fn;
            self.current_function_return_type = prev_ret;
        }

        for (self.pending_global_inits.items) |pending| {
            self.setCurrentNodeContext(pending.initializer);
            self.setCurrentModuleByGlobal(pending.global_name);
            const value_raw = try self.generateExpressionWithContext(pending.initializer, pending.type_name);
            const final_value = try self.castWithSourceRulesNamed(value_raw, pending.type_ref, pending.initializer, pending.type_name);
            _ = c.LLVMBuildStore(self.builder, final_value, pending.global_var);
        }

        _ = c.LLVMBuildRetVoid(self.builder);

        const i32_ty = c.LLVMInt32TypeInContext(self.context);
        const i8_ptr = c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0);
        var ctor_field_tys = [_]c.LLVMTypeRef{ i32_ty, i8_ptr, i8_ptr };
        const ctor_struct_ty = c.LLVMStructTypeInContext(self.context, &ctor_field_tys, 3, 0);

        const priority = c.LLVMConstInt(i32_ty, 65535, 0);
        const fn_ptr = c.LLVMConstBitCast(init_fn, i8_ptr);
        const data_ptr = c.LLVMConstNull(i8_ptr);
        var ctor_vals = [_]c.LLVMValueRef{ priority, fn_ptr, data_ptr };
        const ctor_item = c.LLVMConstNamedStruct(ctor_struct_ty, &ctor_vals, 3);

        const ctor_array_ty = c.LLVMArrayType(ctor_struct_ty, 1);
        var ctor_items = [_]c.LLVMValueRef{ctor_item};
        const ctor_array = c.LLVMConstArray(ctor_struct_ty, &ctor_items, 1);

        const ctors_global = c.LLVMAddGlobal(self.module, ctor_array_ty, "llvm.global_ctors");
        c.LLVMSetInitializer(ctors_global, ctor_array);
        c.LLVMSetLinkage(ctors_global, c.LLVMAppendingLinkage);
        c.LLVMSetGlobalConstant(ctors_global, 1);
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

    pub fn generateStatement(self: *CodeGenerator, stmt: *ast.Node) errors.CodegenError!void {
        const current_block = c.LLVMGetInsertBlock(@ptrCast(self.builder));
        if (current_block != null and c.LLVMGetBasicBlockTerminator(current_block) != null and stmt.data != .label_stmt) {
            return;
        }
        self.setCurrentNodeContext(stmt);
        switch (stmt.data) {
            .assignment => |as| {
                switch (as.target.data) {
                    .unary_op => |un| {
                        if (un.op == '*') {
                            const ptr_val = try self.generateExpression(un.operand);
                            const ptr_type = c.LLVMTypeOf(ptr_val);
                            if (c.LLVMGetTypeKind(ptr_type) != c.LLVMPointerTypeKind) {
                                return errors.CodegenError.TypeMismatch;
                            }

                            if (un.operand.data == .identifier) {
                                const ptr_name = un.operand.data.identifier.name;
                                if (CodeGenerator.getVariable(self, ptr_name)) |var_info| {
                                    if (utils.isConstPointer(var_info.type_name)) {
                                        if (self.current_line > 0) {
                                            self.reportErrorFmt("Cannot modify value through pointer to const '{s}'", .{ptr_name}, "Pointer points to a const value");
                                        } else {
                                            self.reportErrorFmt("Cannot modify value through pointer to const '{s}'", .{ptr_name}, "Pointer points to a const value");
                                        }
                                        return errors.CodegenError.ConstReassignment;
                                    }
                                }
                            }

                            const ptr_type_name = try self.inferType(un.operand);
                            if (!(std.mem.startsWith(u8, ptr_type_name, "ptr<") and std.mem.endsWith(u8, ptr_type_name, ">"))) {
                                self.reportTypeMismatchGeneric("dereference assignment", "Cannot dereference non-pointer type");
                                return errors.CodegenError.TypeMismatch;
                            }
                            const elem_name = ptr_type_name[4 .. ptr_type_name.len - 1];
                            const elem_ty = try self.getLLVMType(elem_name);
                            const value_raw = try self.generateExpressionWithContext(as.value, elem_name);
                            const casted_val = try self.castWithRules(value_raw, elem_ty, as.value);
                            _ = c.LLVMBuildStore(self.builder, casted_val, ptr_val);
                        } else {
                            self.reportTypeMismatchGeneric("assignment target", "Target must be a dereferenceable pointer");
                            return errors.CodegenError.TypeMismatch;
                        }
                    },
                    .identifier => {
                        if (as.value.data == .struct_initializer) {
                            const ident = as.target.data.identifier;
                            const var_info = CodeGenerator.getVariable(self, ident.name) orelse return errors.CodegenError.UndefinedVariable;
                            const struct_init = as.value.data.struct_initializer;
                            const struct_type = self.struct_types.get(struct_init.struct_name) orelse {
                                if (self.current_line > 0) {
                                    self.reportErrorFmt("Unknown struct type '{s}'", .{struct_init.struct_name}, "Struct type must be declared before use");
                                } else {
                                    self.reportErrorFmt("Unknown struct type '{s}'", .{struct_init.struct_name}, "Struct type must be declared before use");
                                }
                                return errors.CodegenError.TypeMismatch;
                            };
                            const field_map = self.struct_fields.get(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
                            const struct_decl = self.getStructDecl(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
                            for (struct_decl.fields.items, 0..) |field, i| {
                                if (field.default_value) |default_val| {
                                    const field_ptr = try self.getStructFieldPointer(@ptrCast(struct_type), var_info.value, @intCast(i));
                                    const field_type = try self.getLLVMType(field.type_name);
                                    const field_type_kind = c.LLVMGetTypeKind(@ptrCast(field_type));
                                    if (default_val.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
                                        try self.assignStringLiteralToArrayField(field_ptr, @ptrCast(field_type), default_val.data.string_literal);
                                    } else {
                                        const value = try self.generateExpression(default_val);
                                        const casted_value = self.castToType(value, @ptrCast(field_type));
                                        _ = c.LLVMBuildStore(@ptrCast(self.builder), @ptrCast(casted_value), @ptrCast(field_ptr));
                                    }
                                }
                            }

                            for (struct_init.field_values.items, 0..) |field_val, idx| {
                                const field_index = if (field_val.field_name) |field_name|
                                    field_map.get(field_name) orelse return errors.CodegenError.UndefinedVariable
                                else
                                    @as(c_uint, @intCast(idx));
                                const field_ptr = try self.getStructFieldPointer(@ptrCast(struct_type), var_info.value, field_index);
                                const field_type = try structs.getFieldType(self, struct_type, field_index);
                                const field_type_kind = c.LLVMGetTypeKind(field_type);
                                if (field_val.value.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
                                    try self.assignStringLiteralToArrayField(field_ptr, @ptrCast(field_type), field_val.value.data.string_literal);
                                } else {
                                    const value = try self.generateExpression(field_val.value);
                                    const casted_value = try self.castWithRules(value, field_type, field_val.value);
                                    _ = c.LLVMBuildStore(@ptrCast(self.builder), @ptrCast(casted_value), @ptrCast(field_ptr));
                                }
                            }
                        } else {
                            try self.generateRegularVariableAssignment(as);
                        }
                    },
                    .qualified_identifier => {
                        const qual_id = as.target.data.qualified_identifier;
                        const has_array_index = try self.containsArrayIndex(qual_id.base);
                        if (has_array_index) {
                            const full_name = try self.buildQualifiedName(as.target);
                            defer self.allocator.free(full_name);
                            const array_name = try self.getBaseIdentifierName(as.target);
                            defer self.allocator.free(array_name);
                            const field_path = utils.dupe(u8, self.allocator, full_name[array_name.len + 1 ..]);
                            defer self.allocator.free(field_path);
                            const array_index = try self.extractArrayIndex(qual_id.base);
                            try self.generateArrayElementFieldAssignment(array_index, field_path, as.value);
                        } else {
                            const struct_name = try self.getBaseIdentifierName(as.target);
                            defer self.allocator.free(struct_name);
                            const full_name = try self.buildQualifiedName(as.target);
                            defer self.allocator.free(full_name);
                            const field_path = utils.dupe(u8, self.allocator, full_name[struct_name.len + 1 ..]);
                            defer self.allocator.free(field_path);

                            try self.generateStructFieldAssignment(struct_name, field_path, as.value);
                        }
                    },
                    .array_index => |arr_idx| {
                        const arr_ass = ast.ArrayAssignment{
                            .array = arr_idx.array,
                            .index = arr_idx.index,
                            .value = as.value,
                        };
                        try array.generateArrayAssignment(self, arr_ass);
                    },
                    else => {
                        return errors.CodegenError.TypeMismatch;
                    },
                }
            },
            .var_decl => |decl| {
                if (utils.isVarArgType(decl.type_name)) {
                    if (self.current_line > 0) {
                        self.reportErrorFmt("Invalid type 'vararg' for variable '{s}'", .{decl.name}, "Variadic arguments can only be used as the last parameter of a function");
                    } else {
                        self.reportErrorFmt("Invalid type 'vararg' for variable '{s}'", .{decl.name}, "Variadic arguments can only be used as the last parameter of a function");
                    }
                    return errors.CodegenError.TypeMismatch;
                }
                if (std.mem.eql(u8, decl.type_name, "void")) {
                    if (decl.initializer != null) {
                        if (self.current_line > 0) {
                            self.reportError("Cannot initialize void variable", "Void variables cannot hold values");
                        } else {
                            self.reportError("Cannot initialize void variable", "Void variables cannot hold values");
                        }
                        return errors.CodegenError.TypeMismatch;
                    }
                    try CodeGenerator.putVariable(self, decl.name, structs.VariableInfo{
                        .value = null,
                        .type_ref = @ptrCast(c.LLVMVoidTypeInContext(@ptrCast(self.context))),
                        .type_name = decl.type_name,
                        .is_const = decl.is_const,
                    });
                    return;
                }

                if (std.mem.startsWith(u8, decl.type_name, "arr<") and std.mem.endsWith(u8, decl.type_name, ">")) {
                    try array.generateArrayDeclaration(self, decl);
                } else if (std.mem.startsWith(u8, decl.type_name, "simd<") and std.mem.endsWith(u8, decl.type_name, ">")) {
                    try simd.generateSimdDeclaration(self, decl);
                } else {
                    const var_type = try self.getLLVMType(decl.type_name);
                    const alloca = self.buildAllocaAtEntry(@ptrCast(var_type), decl.name);
                    try CodeGenerator.putVariable(self, decl.name, structs.VariableInfo{
                        .value = @ptrCast(alloca),
                        .type_ref = @ptrCast(var_type),
                        .type_name = decl.type_name,
                        .is_const = decl.is_const,
                    });
                    if (decl.initializer) |initializer| {
                        if (initializer.data == .struct_initializer) {
                            const struct_init = initializer.data.struct_initializer;
                            const struct_type = self.struct_types.get(struct_init.struct_name) orelse {
                                if (self.current_line > 0) {
                                    self.reportErrorFmt("Unknown struct type '{s}'", .{struct_init.struct_name}, "Struct type must be declared before use");
                                } else {
                                    self.reportErrorFmt("Unknown struct type '{s}'", .{struct_init.struct_name}, "Struct type must be declared before use");
                                }
                                return errors.CodegenError.TypeMismatch;
                            };
                            const field_map = self.struct_fields.get(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
                            const struct_decl = self.getStructDecl(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
                            for (struct_decl.fields.items, 0..) |field, i| {
                                if (field.default_value) |default_val| {
                                    const field_ptr = try self.getStructFieldPointer(@ptrCast(struct_type), @ptrCast(alloca), @intCast(i));
                                    const field_type = try self.getLLVMType(field.type_name);
                                    const field_type_kind = c.LLVMGetTypeKind(field_type);
                                    if (default_val.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
                                        try self.assignStringLiteralToArrayField(field_ptr, field_type, default_val.data.string_literal);
                                    } else {
                                        const value = try self.generateExpression(default_val);
                                        const casted_value = try self.castWithRules(value, field_type, default_val);
                                        _ = c.LLVMBuildStore(self.builder, casted_value, field_ptr);
                                    }
                                }
                            }
                            for (struct_init.field_values.items, 0..) |field_val, idx| {
                                const field_index = if (field_val.field_name) |field_name|
                                    field_map.get(field_name) orelse return errors.CodegenError.UndefinedVariable
                                else
                                    @as(c_uint, @intCast(idx));
                                const field_ptr = try self.getStructFieldPointer(struct_type, alloca, field_index);
                                const field_type = try structs.getFieldType(self, struct_type, field_index);
                                const field_type_kind = c.LLVMGetTypeKind(field_type);
                                if (field_val.value.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
                                    try self.assignStringLiteralToArrayField(field_ptr, field_type, field_val.value.data.string_literal);
                                } else {
                                    const value = try self.generateExpression(field_val.value);
                                    const casted_value = try self.castWithRules(value, field_type, field_val.value);
                                    _ = c.LLVMBuildStore(self.builder, casted_value, field_ptr);
                                }
                            }
                        } else if (c.LLVMGetTypeKind(var_type) == c.LLVMStructTypeKind) {
                            const is_zero_struct_init = initializer.data == .array_initializer and blk: {
                                const arr = initializer.data.array_initializer;
                                break :blk arr.elements.items.len == 1 and
                                    arr.elements.items[0].data == .number_literal and
                                    std.mem.eql(u8, arr.elements.items[0].data.number_literal.value, "0");
                            };

                            if (is_zero_struct_init) {
                                _ = c.LLVMBuildStore(self.builder, c.LLVMConstNull(var_type), alloca);
                            } else if (initializer.data == .identifier) {
                                const src_name = initializer.data.identifier.name;
                                if (CodeGenerator.getVariable(self, src_name)) |src_var| {
                                    if (std.mem.eql(u8, src_var.type_name, decl.type_name)) {
                                        try self.emitMemcpy(alloca, src_var.value, var_type);
                                    } else {
                                        const init_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                                        const casted_value = try self.castWithSourceRulesNamed(init_value, var_type, initializer, decl.type_name);
                                        _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                                    }
                                } else {
                                    const init_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                                    const casted_value = try self.castWithSourceRulesNamed(init_value, var_type, initializer, decl.type_name);
                                    _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                                }
                            } else {
                                const struct_name = std.mem.span(c.LLVMGetStructName(var_type));
                                const struct_decl = self.getStructDecl(struct_name) orelse return errors.CodegenError.TypeMismatch;

                                for (struct_decl.fields.items, 0..) |field, i| {
                                    const field_ptr = try self.getStructFieldPointer(var_type, alloca, @intCast(i));
                                    const zero_value = utils.getDefaultValueForType(self, field.type_name);
                                    _ = c.LLVMBuildStore(self.builder, zero_value, field_ptr);
                                }

                                for (struct_decl.fields.items, 0..) |field, i| {
                                    if (field.default_value) |default_val| {
                                        const field_ptr = try self.getStructFieldPointer(var_type, alloca, @intCast(i));
                                        const value = try self.generateExpression(default_val);
                                        const casted_value = try self.castWithRules(value, try self.getLLVMType(field.type_name), default_val);
                                        _ = c.LLVMBuildStore(self.builder, casted_value, field_ptr);
                                    }
                                }
                                const init_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                                if ((initializer.data == .function_call or initializer.data == .method_call) and c.LLVMGetTypeKind(c.LLVMTypeOf(init_value)) == c.LLVMPointerTypeKind) {
                                    try self.emitMemcpy(alloca, init_value, var_type);
                                    return;
                                }
                                const casted_value = try self.castWithSourceRulesNamed(init_value, var_type, initializer, decl.type_name);
                                _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                            }
                        } else {
                            const init_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                            const casted_value = try self.castWithSourceRulesNamed(init_value, var_type, initializer, decl.type_name);
                            _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                        }
                    } else if (c.LLVMGetTypeKind(var_type) == c.LLVMStructTypeKind) {
                        const struct_name = std.mem.span(c.LLVMGetStructName(var_type));
                        const struct_decl = self.getStructDecl(struct_name) orelse return errors.CodegenError.TypeMismatch;
                        for (struct_decl.fields.items, 0..) |field, i| {
                            const field_ptr = try self.getStructFieldPointer(var_type, alloca, @intCast(i));
                            const zero_value = utils.getDefaultValueForType(self, field.type_name);
                            _ = c.LLVMBuildStore(self.builder, zero_value, field_ptr);
                        }
                        for (struct_decl.fields.items, 0..) |field, i| {
                            if (field.default_value) |default_val| {
                                const field_ptr = try self.getStructFieldPointer(var_type, alloca, @intCast(i));
                                const field_type = try self.getLLVMType(field.type_name);
                                const field_type_kind = c.LLVMGetTypeKind(field_type);
                                if (default_val.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
                                    try self.assignStringLiteralToArrayField(field_ptr, field_type, default_val.data.string_literal);
                                } else {
                                    const value = try self.generateExpression(default_val);
                                    const casted_value = try self.castWithRules(value, field_type, default_val);
                                    _ = c.LLVMBuildStore(self.builder, casted_value, field_ptr);
                                }
                            }
                        }
                    }
                }
            },
            .compound_assignment => |cas| {
                switch (cas.target.data) {
                    .unary_op => |un| {
                        if (un.op == '*') {
                            const ptr_val = try self.generateExpression(un.operand);
                            const ptr_type = c.LLVMTypeOf(ptr_val);
                            if (c.LLVMGetTypeKind(ptr_type) != c.LLVMPointerTypeKind) {
                                self.reportTypeMismatchStr("a pointer type", ptr_type, "compound assignment dereference");
                                return errors.CodegenError.TypeMismatch;
                            }

                            if (un.operand.data == .identifier) {
                                const ptr_name = un.operand.data.identifier.name;
                                if (CodeGenerator.getVariable(self, ptr_name)) |var_info| {
                                    if (utils.isConstPointer(var_info.type_name)) {
                                        if (self.current_line > 0) {
                                            self.reportErrorFmt("Cannot modify value through pointer to const '{s}'", .{ptr_name}, "Pointer points to a const value");
                                        } else {
                                            self.reportErrorFmt("Cannot modify value through pointer to const '{s}'", .{ptr_name}, "Pointer points to a const value");
                                        }
                                        return errors.CodegenError.ConstReassignment;
                                    }
                                }
                            }

                            const elem_type = c.LLVMGetElementType(ptr_type);
                            const current_value = c.LLVMBuildLoad2(self.builder, elem_type, ptr_val, "deref_compound");
                            const rhs_value = try self.generateExpression(cas.value);

                            const current_type_kind = c.LLVMGetTypeKind(elem_type);
                            const is_float = current_type_kind == c.LLVMFloatTypeKind or
                                current_type_kind == c.LLVMDoubleTypeKind or
                                current_type_kind == c.LLVMHalfTypeKind;
                            const is_unsigned = !is_float and isUnsignedType(self.getTypeNameFromLLVMType(elem_type));

                            const new_value = switch (cas.op) {
                                '+' => if (is_float)
                                    c.LLVMBuildFAdd(self.builder, current_value, rhs_value, "fadd_compound")
                                else
                                    c.LLVMBuildAdd(self.builder, current_value, rhs_value, "add_compound"),
                                '-' => if (is_float)
                                    c.LLVMBuildFSub(self.builder, current_value, rhs_value, "fsub_compound")
                                else
                                    c.LLVMBuildSub(self.builder, current_value, rhs_value, "sub_compound"),
                                '*' => if (is_float)
                                    c.LLVMBuildFMul(self.builder, current_value, rhs_value, "fmul_compound")
                                else
                                    c.LLVMBuildMul(self.builder, current_value, rhs_value, "mul_compound"),
                                '/' => if (is_float)
                                    c.LLVMBuildFDiv(self.builder, current_value, rhs_value, "fdiv_compound")
                                else if (is_unsigned)
                                    c.LLVMBuildUDiv(self.builder, current_value, rhs_value, "udiv_compound")
                                else
                                    c.LLVMBuildSDiv(self.builder, current_value, rhs_value, "sdiv_compound"),
                                else => return errors.CodegenError.UnsupportedOperation,
                            };
                            _ = c.LLVMBuildStore(self.builder, new_value, ptr_val);
                        } else {
                            self.reportTypeMismatchGeneric("compound assignment target", "Target must be a dereferenceable pointer or a variable");
                            return errors.CodegenError.TypeMismatch;
                        }
                    },
                    .identifier => |ident| {
                        const var_info = CodeGenerator.getVariable(self, ident.name) orelse return errors.CodegenError.UndefinedVariable;

                        if (var_info.is_const) {
                            if (self.current_line > 0) {
                                self.reportErrorFmt("Cannot reassign const variable '{s}'", .{ident.name}, "Variable is declared as const");
                            } else {
                                self.reportErrorFmt("Cannot reassign const variable '{s}'", .{ident.name}, "Variable is declared as const");
                            }
                            return errors.CodegenError.ConstReassignment;
                        }

                        const current_value = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_current");
                        const current_type_kind = c.LLVMGetTypeKind(var_info.type_ref);
                        const is_float = current_type_kind == c.LLVMFloatTypeKind or
                            current_type_kind == c.LLVMDoubleTypeKind or
                            current_type_kind == c.LLVMHalfTypeKind;
                        const is_unsigned = !is_float and isUnsignedType(var_info.type_name);
                        const rhs_value = try self.generateExpressionWithContext(cas.value, var_info.type_name);
                        const rhs_type = c.LLVMTypeOf(rhs_value);
                        const rhs_kind = c.LLVMGetTypeKind(rhs_type);
                        const rhs_casted = if (is_float and rhs_kind == c.LLVMIntegerTypeKind)
                            self.castToType(rhs_value, @ptrCast(var_info.type_ref))
                        else
                            try self.castWithRules(rhs_value, @ptrCast(var_info.type_ref), cas.value);

                        const new_value = switch (cas.op) {
                            '+' => if (is_float)
                                c.LLVMBuildFAdd(self.builder, current_value, rhs_casted, "fadd_compound")
                            else
                                c.LLVMBuildAdd(self.builder, current_value, rhs_casted, "add_compound"),
                            '-' => if (is_float)
                                c.LLVMBuildFSub(self.builder, current_value, rhs_casted, "fsub_compound")
                            else
                                c.LLVMBuildSub(self.builder, current_value, rhs_casted, "sub_compound"),
                            '*' => if (is_float)
                                c.LLVMBuildFMul(self.builder, current_value, rhs_casted, "fmul_compound")
                            else
                                c.LLVMBuildMul(self.builder, current_value, rhs_casted, "mul_compound"),
                            '/' => if (is_float)
                                c.LLVMBuildFDiv(self.builder, current_value, rhs_casted, "fdiv_compound")
                            else if (is_unsigned)
                                c.LLVMBuildUDiv(self.builder, current_value, rhs_casted, "udiv_compound")
                            else
                                c.LLVMBuildSDiv(self.builder, current_value, rhs_casted, "sdiv_compound"),
                            '%' => if (is_float) blk: {
                                self.uses_float_modulo = true;
                                break :blk c.LLVMBuildFRem(self.builder, current_value, rhs_casted, "frem_compound");
                            } else if (is_unsigned)
                                c.LLVMBuildURem(self.builder, current_value, rhs_casted, "urem_compound")
                            else
                                c.LLVMBuildSRem(self.builder, current_value, rhs_casted, "srem_compound"),
                            'A', '&' => c.LLVMBuildAnd(self.builder, current_value, rhs_casted, "and_compound"),
                            '$', '|' => c.LLVMBuildOr(self.builder, current_value, rhs_casted, "or_compound"),
                            '^' => c.LLVMBuildXor(self.builder, current_value, rhs_casted, "xor_compound"),
                            '<' => c.LLVMBuildShl(self.builder, current_value, rhs_casted, "shl_compound"),
                            '>' => if (is_unsigned)
                                c.LLVMBuildLShr(self.builder, current_value, rhs_casted, "lshr_compound")
                            else
                                c.LLVMBuildAShr(self.builder, current_value, rhs_casted, "ashr_compound"),
                            else => return errors.CodegenError.UnsupportedOperation,
                        };
                        _ = c.LLVMBuildStore(self.builder, new_value, var_info.value);
                    },
                    .array_index => |arr_idx| {
                        const access = try array.getArrayElementPtrAndType(self, arr_idx);
                        const current_val = c.LLVMBuildLoad2(self.builder, access.element_type, access.ptr, "load_elem");
                        const expected_ty_name = access.element_type_name;
                        const rhs_raw = try self.generateExpressionWithContext(cas.value, expected_ty_name);
                        const rhs_val = try self.castWithRules(rhs_raw, access.element_type, cas.value);

                        const is_float = std.mem.eql(u8, expected_ty_name, "f16") or
                            std.mem.eql(u8, expected_ty_name, "f32") or
                            std.mem.eql(u8, expected_ty_name, "f64");
                        const is_unsigned = !is_float and isUnsignedType(expected_ty_name);

                        const new_value = switch (cas.op) {
                            '+' => if (is_float)
                                c.LLVMBuildFAdd(self.builder, current_val, rhs_val, "fadd_array_compound")
                            else
                                c.LLVMBuildAdd(self.builder, current_val, rhs_val, "add_array_compound"),
                            '-' => if (is_float)
                                c.LLVMBuildFSub(self.builder, current_val, rhs_val, "fsub_array_compound")
                            else
                                c.LLVMBuildSub(self.builder, current_val, rhs_val, "sub_array_compound"),
                            '*' => if (is_float)
                                c.LLVMBuildFMul(self.builder, current_val, rhs_val, "fmul_array_compound")
                            else
                                c.LLVMBuildMul(self.builder, current_val, rhs_val, "mul_array_compound"),
                            '/' => if (is_float)
                                c.LLVMBuildFDiv(self.builder, current_val, rhs_val, "fdiv_array_compound")
                            else if (is_unsigned)
                                c.LLVMBuildUDiv(self.builder, current_val, rhs_val, "udiv_array_compound")
                            else
                                c.LLVMBuildSDiv(self.builder, current_val, rhs_val, "sdiv_array_compound"),
                            '%' => if (is_float) blk: {
                                self.uses_float_modulo = true;
                                break :blk c.LLVMBuildFRem(self.builder, current_val, rhs_val, "frem_array_compound");
                            } else if (is_unsigned)
                                c.LLVMBuildURem(self.builder, current_val, rhs_val, "urem_array_compound")
                            else
                                c.LLVMBuildSRem(self.builder, current_val, rhs_val, "srem_array_compound"),
                            'A', '&' => c.LLVMBuildAnd(self.builder, current_val, rhs_val, "and_array_compound"),
                            '$', '|' => c.LLVMBuildOr(self.builder, current_val, rhs_val, "or_array_compound"),
                            '^' => c.LLVMBuildXor(self.builder, current_val, rhs_val, "xor_array_compound"),
                            '<' => c.LLVMBuildShl(self.builder, current_val, rhs_val, "shl_array_compound"),
                            '>' => if (is_unsigned)
                                c.LLVMBuildLShr(self.builder, current_val, rhs_val, "lshr_array_compound")
                            else
                                c.LLVMBuildAShr(self.builder, current_val, rhs_val, "ashr_array_compound"),
                            else => return errors.CodegenError.UnsupportedOperation,
                        };
                        _ = c.LLVMBuildStore(self.builder, new_value, access.ptr);
                    },
                    .qualified_identifier => |qident| {
                        const base_name = try self.getBaseIdentifierName(cas.target);
                        defer self.allocator.free(base_name);
                        const var_info = CodeGenerator.getVariable(self, base_name) orelse return errors.CodegenError.UndefinedVariable;
                        const struct_name = var_info.type_name;
                        const field_map = self.struct_fields.get(struct_name) orelse return errors.CodegenError.TypeMismatch;
                        const field_index = field_map.get(qident.field) orelse return errors.CodegenError.UndefinedVariable;
                        const struct_type = self.struct_types.get(struct_name) orelse return errors.CodegenError.TypeMismatch;
                        const field_ptr = try self.getStructFieldPointer(@ptrCast(struct_type), var_info.value, @intCast(field_index));
                        const struct_decl = self.getStructDecl(struct_name) orelse return errors.CodegenError.TypeMismatch;
                        const field_type_name = struct_decl.fields.items[field_index].type_name;
                        const field_type = try self.getLLVMType(field_type_name);
                        const current_value = c.LLVMBuildLoad2(self.builder, field_type, field_ptr, "load_field");
                        const field_type_kind = c.LLVMGetTypeKind(field_type);
                        const is_float = field_type_kind == c.LLVMFloatTypeKind or
                            field_type_kind == c.LLVMDoubleTypeKind or
                            field_type_kind == c.LLVMHalfTypeKind;
                        const is_unsigned = !is_float and isUnsignedType(field_type_name);
                        const rhs_value = try self.generateExpressionWithContext(cas.value, field_type_name);
                        const rhs_type = c.LLVMTypeOf(rhs_value);
                        const rhs_kind = c.LLVMGetTypeKind(rhs_type);
                        const rhs_casted = if (is_float and rhs_kind == c.LLVMIntegerTypeKind)
                            self.castToType(rhs_value, field_type)
                        else
                            try self.castWithRules(rhs_value, field_type, cas.value);

                        const new_value = switch (cas.op) {
                            '+' => if (is_float)
                                c.LLVMBuildFAdd(self.builder, current_value, rhs_casted, "fadd_field_compound")
                            else
                                c.LLVMBuildAdd(self.builder, current_value, rhs_casted, "add_field_compound"),
                            '-' => if (is_float)
                                c.LLVMBuildFSub(self.builder, current_value, rhs_casted, "fsub_field_compound")
                            else
                                c.LLVMBuildSub(self.builder, current_value, rhs_casted, "sub_field_compound"),
                            '*' => if (is_float)
                                c.LLVMBuildFMul(self.builder, current_value, rhs_casted, "fmul_field_compound")
                            else
                                c.LLVMBuildMul(self.builder, current_value, rhs_casted, "mul_field_compound"),
                            '/' => if (is_float)
                                c.LLVMBuildFDiv(self.builder, current_value, rhs_casted, "fdiv_field_compound")
                            else if (is_unsigned)
                                c.LLVMBuildUDiv(self.builder, current_value, rhs_casted, "udiv_field_compound")
                            else
                                c.LLVMBuildSDiv(self.builder, current_value, rhs_casted, "sdiv_field_compound"),
                            '%' => if (is_float) blk: {
                                self.uses_float_modulo = true;
                                break :blk c.LLVMBuildFRem(self.builder, current_value, rhs_casted, "frem_field_compound");
                            } else if (is_unsigned)
                                c.LLVMBuildURem(self.builder, current_value, rhs_casted, "urem_field_compound")
                            else
                                c.LLVMBuildSRem(self.builder, current_value, rhs_casted, "srem_field_compound"),
                            'A', '&' => c.LLVMBuildAnd(self.builder, current_value, rhs_casted, "and_field_compound"),
                            '$', '|' => c.LLVMBuildOr(self.builder, current_value, rhs_casted, "or_field_compound"),
                            '^' => c.LLVMBuildXor(self.builder, current_value, rhs_casted, "xor_field_compound"),
                            '<' => c.LLVMBuildShl(self.builder, current_value, rhs_casted, "shl_field_compound"),
                            '>' => if (is_unsigned)
                                c.LLVMBuildLShr(self.builder, current_value, rhs_casted, "lshr_field_compound")
                            else
                                c.LLVMBuildAShr(self.builder, current_value, rhs_casted, "ashr_field_compound"),
                            else => return errors.CodegenError.UnsupportedOperation,
                        };
                        _ = c.LLVMBuildStore(self.builder, new_value, field_ptr);
                    },
                    else => {
                        return errors.CodegenError.UnsupportedOperation;
                    },
                }
            },
            .function_call => {
                _ = try self.generateExpression(stmt);
            },
            .handled_call_stmt => |handled| {
                _ = try error_flow.generateHandledCall(self, handled, null);
            },
            .method_call => {
                _ = try self.generateExpression(stmt);
            },
            .send_stmt => |send_stmt| {
                const code = self.error_codes.get(send_stmt.error_name) orelse return errors.CodegenError.TypeMismatch;
                const err_global = error_flow.ensureLastErrorGlobal(self);
                const mode_global = error_flow.ensureLastErrorModeGlobal(self);
                const i32_ty = c.LLVMInt32TypeInContext(self.context);
                const code_bits: c_ulonglong = @bitCast(@as(i64, code));
                const code_val = c.LLVMConstInt(i32_ty, code_bits, 1);
                _ = c.LLVMBuildStore(self.builder, code_val, err_global);
                _ = c.LLVMBuildStore(self.builder, c.LLVMConstInt(i32_ty, 1, 0), mode_global);
            },
            .solicit_stmt => |solicit_stmt| {
                const code = self.error_codes.get(solicit_stmt.error_name) orelse return errors.CodegenError.TypeMismatch;
                const err_global = error_flow.ensureLastErrorGlobal(self);
                const mode_global = error_flow.ensureLastErrorModeGlobal(self);
                const callback_global = error_flow.ensureSolicitCallbackGlobal(self);
                const i32_ty = c.LLVMInt32TypeInContext(self.context);
                const i8_ptr = c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0);
                const code_bits: c_ulonglong = @bitCast(@as(i64, code));
                const code_val = c.LLVMConstInt(i32_ty, code_bits, 1);
                _ = c.LLVMBuildStore(self.builder, code_val, err_global);
                _ = c.LLVMBuildStore(self.builder, c.LLVMConstInt(i32_ty, 2, 0), mode_global);

                try error_flow.exposeCurrentFunctionVarsForSolicit(self);

                const callback_raw = c.LLVMBuildLoad2(self.builder, i8_ptr, callback_global, "solicit.cb.raw");
                const has_callback = c.LLVMBuildICmp(self.builder, c.LLVMIntNE, callback_raw, c.LLVMConstNull(i8_ptr), "solicit.has_cb");

                const current_fn = self.current_function orelse return errors.CodegenError.TypeMismatch;
                const call_bb = c.LLVMAppendBasicBlockInContext(self.context, current_fn, "solicit.cb.call");
                const cont_bb = c.LLVMAppendBasicBlockInContext(self.context, current_fn, "solicit.cb.cont");
                _ = c.LLVMBuildCondBr(self.builder, has_callback, call_bb, cont_bb);

                c.LLVMPositionBuilderAtEnd(self.builder, call_bb);
                var callback_params = [_]c.LLVMTypeRef{i32_ty};
                const callback_fn_ty = c.LLVMFunctionType(c.LLVMVoidTypeInContext(self.context), &callback_params, 1, 0);
                const callback_ptr_ty = c.LLVMPointerType(callback_fn_ty, 0);
                const callback_fn = c.LLVMBuildBitCast(self.builder, callback_raw, callback_ptr_ty, "solicit.cb.fn");
                var callback_args = [_]c.LLVMValueRef{code_val};
                _ = c.LLVMBuildCall2(self.builder, callback_fn_ty, callback_fn, &callback_args, 1, "");
                _ = c.LLVMBuildBr(self.builder, cont_bb);

                c.LLVMPositionBuilderAtEnd(self.builder, cont_bb);
                _ = c.LLVMBuildStore(self.builder, c.LLVMConstInt(i32_ty, 0, 0), err_global);
                _ = c.LLVMBuildStore(self.builder, c.LLVMConstInt(i32_ty, 0, 0), mode_global);
            },
            .defer_stmt => |defer_stmt| {
                try self.registerDeferredExpression(defer_stmt.expression);
            },
            .return_stmt => |ret| {
                if (ret.expression) |expr| {
                    const target_ty_name = self.current_function_return_type;
                    const target_ty = try self.getLLVMType(target_ty_name);
                    var sret_ptr: ?c.LLVMValueRef = null;
                    if (self.current_codegen_function_name) |fname| {
                        if (self.sret_functions.get(fname) != null) {
                            if (self.current_function) |fn_val| {
                                sret_ptr = c.LLVMGetParam(fn_val, 0);
                            }
                        }
                    }

                    if (sret_ptr != null and expr.data == .identifier) {
                        const ident_name = expr.data.identifier.name;
                        if (CodeGenerator.getVariable(self, ident_name)) |var_info| {
                            if (var_info.type_ref == target_ty) {
                                try self.runDeferredActions();
                                try self.emitMemcpy(sret_ptr.?, var_info.value, target_ty);
                                _ = c.LLVMBuildRetVoid(self.builder);
                                return;
                            }
                        }
                    }

                    const ret_raw = try self.generateExpressionWithContext(expr, target_ty_name);

                    if (sret_ptr != null and c.LLVMGetTypeKind(c.LLVMTypeOf(ret_raw)) == c.LLVMPointerTypeKind and c.LLVMGetTypeKind(target_ty) == c.LLVMStructTypeKind) {
                        try self.runDeferredActions();
                        try self.emitMemcpy(sret_ptr.?, ret_raw, target_ty);
                        _ = c.LLVMBuildRetVoid(self.builder);
                        return;
                    }

                    const final_ret = try self.castWithRules(ret_raw, target_ty, expr);
                    try self.runDeferredActions();
                    if (sret_ptr) |ptr| {
                        _ = c.LLVMBuildStore(self.builder, final_ret, ptr);
                        _ = c.LLVMBuildRetVoid(self.builder);
                    } else {
                        _ = c.LLVMBuildRet(self.builder, final_ret);
                    }
                } else {
                    try self.runDeferredActions();
                    _ = c.LLVMBuildRetVoid(self.builder);
                }
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
            .goto_stmt => |goto_stmt| {
                try self.generateGotoStatement(goto_stmt);
            },
            .label_stmt => |label_stmt| {
                try self.generateLabelStatement(label_stmt);
            },
            .match_stmt => |match_stmt| {
                try self.generateMatchStatement(match_stmt);
            },
            .array_assignment => |arr_ass| {
                const array_name = try self.getBaseIdentifierName(arr_ass.array);
                defer self.allocator.free(array_name);

                if (!try simd.handleSimdAssignment(self, arr_ass)) {
                    try array.generateArrayAssignment(self, arr_ass);
                }
            },
            .array_compound_assignment => |arr_cass| {
                if (try simd.handleSimdArrayCompoundAssignment(self, arr_cass)) {
                    return;
                }

                var collected_indices: std.ArrayList(c.LLVMValueRef) = .empty;
                defer collected_indices.deinit(self.allocator);
                const base_node = try self.collectArrayIndices(arr_cass.array, &collected_indices);
                var index_value = try self.generateExpression(arr_cass.index);
                const array_name = try self.getBaseIdentifierName(base_node);
                defer self.allocator.free(array_name);
                const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;

                if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
                    if (collected_indices.items.len == 0) {
                        return errors.CodegenError.TypeMismatch;
                    }

                    if (c.LLVMTypeOf(index_value) != c.LLVMInt64TypeInContext(self.context)) {
                        index_value = self.castToType(index_value, c.LLVMInt64TypeInContext(self.context));
                    }
                    try collected_indices.append(self.allocator, index_value);
                    const ptr_val = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_ptr_for_compound");
                    const element_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                    const element_type = try self.getLLVMType(element_type_name);
                    const idx = collected_indices.items[collected_indices.items.len - 1];
                    var indices = [_]c.LLVMValueRef{idx};
                    const element_ptr = c.LLVMBuildGEP2(self.builder, element_type, ptr_val, &indices[0], 1, "ptr_index_compound");
                    const current_val = c.LLVMBuildLoad2(self.builder, element_type, element_ptr, "load_elem");
                    const expected_ty_name = self.getTypeNameFromLLVMType(element_type);
                    const rhs_raw = try self.generateExpressionWithContext(arr_cass.value, expected_ty_name);
                    const rhs_val = try self.castWithRules(rhs_raw, element_type, arr_cass.value);
                    const final_kind = c.LLVMGetTypeKind(element_type);
                    const op = arr_cass.op;
                    var new_val: c.LLVMValueRef = undefined;
                    if (final_kind == c.LLVMFloatTypeKind or final_kind == c.LLVMDoubleTypeKind or final_kind == c.LLVMHalfTypeKind) {
                        new_val = switch (op) {
                            '+' => c.LLVMBuildFAdd(self.builder, current_val, rhs_val, "fadd"),
                            '-' => c.LLVMBuildFSub(self.builder, current_val, rhs_val, "fsub"),
                            '*' => c.LLVMBuildFMul(self.builder, current_val, rhs_val, "fmul"),
                            '/' => c.LLVMBuildFDiv(self.builder, current_val, rhs_val, "fdiv"),
                            else => return errors.CodegenError.UnsupportedOperation,
                        };
                    } else if (final_kind == c.LLVMIntegerTypeKind) {
                        new_val = switch (op) {
                            '+' => c.LLVMBuildAdd(self.builder, current_val, rhs_val, "add"),
                            '-' => c.LLVMBuildSub(self.builder, current_val, rhs_val, "sub"),
                            '*' => c.LLVMBuildMul(self.builder, current_val, rhs_val, "mul"),
                            '/' => blk: {
                                const is_unsigned = isUnsignedType(self.getTypeNameFromLLVMType(element_type));
                                break :blk if (is_unsigned)
                                    c.LLVMBuildUDiv(self.builder, current_val, rhs_val, "udiv")
                                else
                                    c.LLVMBuildSDiv(self.builder, current_val, rhs_val, "sdiv");
                            },
                            else => return errors.CodegenError.UnsupportedOperation,
                        };
                    } else return errors.CodegenError.UnsupportedOperation;
                    _ = c.LLVMBuildStore(self.builder, new_val, element_ptr);
                    return;
                }

                index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
                try collected_indices.append(self.allocator, index_value);
                var final_type = var_info.type_ref;
                for (0..collected_indices.items.len) |_| final_type = c.LLVMGetElementType(final_type);
                var all_indices: std.ArrayList(c.LLVMValueRef) = .empty;
                defer all_indices.deinit(self.allocator);
                try all_indices.append(self.allocator, c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0));
                var i = collected_indices.items.len;
                while (i > 0) {
                    i -= 1;
                    try all_indices.append(self.allocator, collected_indices.items[i]);
                }
                const element_ptr = c.LLVMBuildGEP2(self.builder, var_info.type_ref, var_info.value, all_indices.items.ptr, @intCast(all_indices.items.len), "array_element_ptr");
                const current_val = c.LLVMBuildLoad2(self.builder, final_type, element_ptr, "load_elem");
                const expected_ty_name = self.getTypeNameFromLLVMType(final_type);
                const rhs_raw = try self.generateExpressionWithContext(arr_cass.value, expected_ty_name);
                const rhs_val = try self.castWithRules(rhs_raw, final_type, arr_cass.value);
                const final_kind = c.LLVMGetTypeKind(final_type);
                const op = arr_cass.op;
                var new_val: c.LLVMValueRef = undefined;
                if (final_kind == c.LLVMFloatTypeKind or final_kind == c.LLVMDoubleTypeKind or final_kind == c.LLVMHalfTypeKind) {
                    new_val = switch (op) {
                        '+' => c.LLVMBuildFAdd(self.builder, current_val, rhs_val, "fadd"),
                        '-' => c.LLVMBuildFSub(self.builder, current_val, rhs_val, "fsub"),
                        '*' => c.LLVMBuildFMul(self.builder, current_val, rhs_val, "fmul"),
                        '/' => c.LLVMBuildFDiv(self.builder, current_val, rhs_val, "fdiv"),
                        '%' => blk: {
                            self.uses_float_modulo = true;
                            break :blk c.LLVMBuildFRem(self.builder, current_val, rhs_val, "frem");
                        },
                        else => return errors.CodegenError.UnsupportedOperation,
                    };
                } else if (final_kind == c.LLVMIntegerTypeKind) {
                    new_val = switch (op) {
                        '+' => c.LLVMBuildAdd(self.builder, current_val, rhs_val, "add"),
                        '-' => c.LLVMBuildSub(self.builder, current_val, rhs_val, "sub"),
                        '*' => c.LLVMBuildMul(self.builder, current_val, rhs_val, "mul"),
                        '/' => blk: {
                            const is_unsigned = isUnsignedType(self.getTypeNameFromLLVMType(final_type));
                            break :blk if (is_unsigned)
                                c.LLVMBuildUDiv(self.builder, current_val, rhs_val, "udiv")
                            else
                                c.LLVMBuildSDiv(self.builder, current_val, rhs_val, "sdiv");
                        },
                        '%' => blk: {
                            const is_unsigned = isUnsignedType(self.getTypeNameFromLLVMType(final_type));
                            break :blk if (is_unsigned)
                                c.LLVMBuildURem(self.builder, current_val, rhs_val, "urem")
                            else
                                c.LLVMBuildSRem(self.builder, current_val, rhs_val, "srem");
                        },
                        else => return errors.CodegenError.UnsupportedOperation,
                    };
                } else {
                    return errors.CodegenError.UnsupportedOperation;
                }

                _ = c.LLVMBuildStore(self.builder, new_val, element_ptr);
            },
            .simd_compound_assignment => |simd_cass| {
                try simd.generateSimdCompoundAssignment(self, simd_cass);
            },
            .c_function_decl => |c_func| {
                try CodeGenerator.generateCFunctionDeclaration(self, c_func);
            },
            .enum_decl => |enum_decl| {
                try enums.generateEnumDeclaration(self, enum_decl);
            },
            .error_decl => |error_decl| {
                try error_flow.registerErrorDeclaration(self, error_decl);
            },
            .struct_decl => {},
            .unary_op => {
                _ = try self.generateExpression(stmt);
            },
            .expression_block => {
                _ = try self.generateExpression(stmt);
            },
            else => {},
        }
    }

    pub fn generateExpressionBlock(self: *CodeGenerator, block: ast.ExpressionBlock) errors.CodegenError!c.LLVMValueRef {
        const target_type = try self.getLLVMType(block.type_name);

        try CodeGenerator.pushScope(self);
        const scope_marker = self.currentDeferMarker();
        defer CodeGenerator.popScope(self);

        for (block.statements.items) |stmt| {
            try self.generateStatement(stmt);
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) != null) {
                self.reportError("Expression block terminated before final value", "Remove return/break/continue/goto from expression block body");
                return errors.CodegenError.TypeMismatch;
            }
        }

        const value_raw = try self.generateExpressionWithContext(block.result, block.type_name);
        const value = try self.castWithRules(value_raw, target_type, block.result);
        try self.runDeferredActionsFrom(scope_marker);
        return value;
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
        try CodeGenerator.pushScope(self);
        const then_marker = self.currentDeferMarker();
        for (if_stmt.then_body.items) |stmt| {
            try self.generateStatement(stmt);
        }
        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
            try self.runDeferredActionsFrom(then_marker);
        }
        CodeGenerator.popScope(self);
        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
            _ = c.LLVMBuildBr(self.builder, merge_bb);
        }

        if (else_bb) |else_block| {
            c.LLVMPositionBuilderAtEnd(self.builder, else_block);
            if (if_stmt.else_body) |else_body| {
                try CodeGenerator.pushScope(self);
                const else_marker = self.currentDeferMarker();
                for (else_body.items) |stmt| {
                    try self.generateStatement(stmt);
                }
                if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                    try self.runDeferredActionsFrom(else_marker);
                }
                CodeGenerator.popScope(self);
            }
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                _ = c.LLVMBuildBr(self.builder, merge_bb);
            }
        }
        c.LLVMPositionBuilderAtEnd(self.builder, merge_bb);
    }

    fn generateMatchStatement(self: *CodeGenerator, match_stmt: ast.MatchStmt) errors.CodegenError!void {
        const current_function = self.current_function orelse return errors.CodegenError.TypeMismatch;
        if (match_stmt.cases.items.len == 0) return;

        const condition_value = try self.generateExpression(match_stmt.condition);
        const condition_type = c.LLVMTypeOf(condition_value);
        const condition_type_kind = c.LLVMGetTypeKind(condition_type);
        const condition_type_name = self.getTypeNameFromLLVMType(condition_type);

        const merge_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "match_end");
        var next_check_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "match_check");

        _ = c.LLVMBuildBr(self.builder, next_check_bb);

        for (match_stmt.cases.items, 0..) |case, i| {
            const case_body_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "match_case");
            const no_match_bb = if (i + 1 < match_stmt.cases.items.len)
                c.LLVMAppendBasicBlockInContext(self.context, current_function, "match_check")
            else
                merge_bb;

            c.LLVMPositionBuilderAtEnd(self.builder, next_check_bb);

            var case_match: ?c.LLVMValueRef = null;
            for (case.values.items) |value_node| {
                const raw_case_value = try self.generateExpressionWithContext(value_node, condition_type_name);
                const casted_case_value = try self.castWithRules(raw_case_value, condition_type, value_node);

                const cmp = switch (condition_type_kind) {
                    c.LLVMIntegerTypeKind, c.LLVMPointerTypeKind => c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, condition_value, casted_case_value, "match_cmp"),
                    c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => c.LLVMBuildFCmp(self.builder, c.LLVMRealOEQ, condition_value, casted_case_value, "match_cmp"),
                    else => return errors.CodegenError.TypeMismatch,
                };

                case_match = if (case_match) |existing|
                    c.LLVMBuildOr(self.builder, existing, cmp, "match_or")
                else
                    cmp;
            }

            const condition = case_match orelse c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 0, 0);
            _ = c.LLVMBuildCondBr(self.builder, condition, case_body_bb, no_match_bb);

            c.LLVMPositionBuilderAtEnd(self.builder, case_body_bb);
            try CodeGenerator.pushScope(self);
            const case_marker = self.currentDeferMarker();
            for (case.body.items) |stmt| {
                try self.generateStatement(stmt);
            }
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                try self.runDeferredActionsFrom(case_marker);
            }
            CodeGenerator.popScope(self);

            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                _ = c.LLVMBuildBr(self.builder, merge_bb);
            }

            next_check_bb = no_match_bb;
        }

        c.LLVMPositionBuilderAtEnd(self.builder, merge_bb);
    }

    fn generateForStatement(self: *CodeGenerator, for_stmt: ast.ForStmt) errors.CodegenError!void {
        const current_function = self.current_function orelse return errors.CodegenError.TypeMismatch;
        if (for_stmt.condition) |cond| {
            const condition_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "for_cond");
            const body_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "for_body");
            const exit_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "for_exit");
            _ = c.LLVMBuildBr(self.builder, condition_bb);
            c.LLVMPositionBuilderAtEnd(self.builder, condition_bb);
            const cond_value = try self.generateExpression(cond);
            const cond_bool = self.convertToBool(cond_value);
            _ = c.LLVMBuildCondBr(self.builder, cond_bool, body_bb, exit_bb);
            c.LLVMPositionBuilderAtEnd(self.builder, body_bb);
            try CodeGenerator.pushScope(self);
            const body_marker = self.currentDeferMarker();
            const loop_context = structs.LoopContext{
                .break_block = exit_bb,
                .continue_block = condition_bb,
                .defer_marker = body_marker,
            };
            try self.loop_context_stack.append(self.allocator, loop_context);
            for (for_stmt.body.items) |stmt| {
                try self.generateStatement(stmt);
            }
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                try self.runDeferredActionsFrom(body_marker);
                if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                    _ = c.LLVMBuildBr(self.builder, condition_bb);
                }
            }
            _ = self.loop_context_stack.pop();
            CodeGenerator.popScope(self);
            c.LLVMPositionBuilderAtEnd(self.builder, exit_bb);
        } else {
            const body_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "for_body");
            const exit_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "for_exit");
            _ = c.LLVMBuildBr(self.builder, body_bb);
            c.LLVMPositionBuilderAtEnd(self.builder, body_bb);
            try CodeGenerator.pushScope(self);
            const body_marker = self.currentDeferMarker();
            const loop_context = structs.LoopContext{
                .break_block = exit_bb,
                .continue_block = body_bb,
                .defer_marker = body_marker,
            };
            try self.loop_context_stack.append(self.allocator, loop_context);
            for (for_stmt.body.items) |stmt| {
                try self.generateStatement(stmt);
            }

            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                try self.runDeferredActionsFrom(body_marker);
                if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                    _ = c.LLVMBuildBr(self.builder, body_bb);
                }
            }

            _ = self.loop_context_stack.pop();
            CodeGenerator.popScope(self);
            c.LLVMPositionBuilderAtEnd(self.builder, exit_bb);
        }
    }

    fn generateCForStatement(self: *CodeGenerator, c_for_stmt: ast.CForStmt) errors.CodegenError!void {
        const current_function = self.current_function orelse return errors.CodegenError.TypeMismatch;
        try CodeGenerator.pushScope(self);
        const loop_marker = self.currentDeferMarker();
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
        const loop_context = structs.LoopContext{
            .break_block = exit_bb,
            .continue_block = increment_bb,
            .defer_marker = loop_marker,
        };
        try self.loop_context_stack.append(self.allocator, loop_context);
        c.LLVMPositionBuilderAtEnd(self.builder, body_bb);
        for (c_for_stmt.body.items) |stmt| {
            try self.generateStatement(stmt);
        }
        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
            try self.runDeferredActionsFrom(loop_marker);
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                _ = c.LLVMBuildBr(self.builder, increment_bb);
            }
        }
        c.LLVMPositionBuilderAtEnd(self.builder, increment_bb);
        if (c_for_stmt.increment) |inc| {
            try self.generateStatement(inc);
        }
        _ = c.LLVMBuildBr(self.builder, condition_bb);
        c.LLVMPositionBuilderAtEnd(self.builder, exit_bb);
        _ = self.loop_context_stack.pop();
        CodeGenerator.popScope(self);
    }

    fn generateContinueStatement(self: *CodeGenerator) errors.CodegenError!void {
        if (self.loop_context_stack.items.len == 0) {
            return errors.CodegenError.TypeMismatch;
        }
        const loop_context = self.loop_context_stack.items[self.loop_context_stack.items.len - 1];
        try self.runDeferredActionsFrom(loop_context.defer_marker);
        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) != null) return;
        const continue_block = loop_context.continue_block;
        _ = c.LLVMBuildBr(self.builder, continue_block);
    }

    fn generateBreakStatement(self: *CodeGenerator) errors.CodegenError!void {
        if (self.loop_context_stack.items.len == 0) {
            return errors.CodegenError.TypeMismatch;
        }
        const loop_context = self.loop_context_stack.items[self.loop_context_stack.items.len - 1];
        try self.runDeferredActionsFrom(loop_context.defer_marker);
        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) != null) return;
        const break_block = loop_context.break_block;
        _ = c.LLVMBuildBr(self.builder, break_block);
    }

    fn generateGotoStatement(self: *CodeGenerator, goto_stmt: ast.GotoStmt) errors.CodegenError!void {
        const current_function = self.current_function orelse return errors.CodegenError.TypeMismatch;
        if (self.label_blocks.get(goto_stmt.label)) |label_block| {
            _ = c.LLVMBuildBr(self.builder, label_block);
        } else {
            const goto_block = c.LLVMAppendBasicBlockInContext(self.context, current_function, "goto_pending");
            const label_copy = utils.dupe(u8, self.allocator, goto_stmt.label);
            try self.pending_gotos.append(self.allocator, structs.PendingGoto{
                .label = label_copy,
                .goto_block = goto_block,
            });
            _ = c.LLVMBuildBr(self.builder, goto_block);
        }
        const unreachable_block = c.LLVMAppendBasicBlockInContext(self.context, current_function, "after_goto");
        c.LLVMPositionBuilderAtEnd(self.builder, unreachable_block);
    }

    fn generateLabelStatement(self: *CodeGenerator, label_stmt: ast.LabelStmt) errors.CodegenError!void {
        const current_function = self.current_function orelse return errors.CodegenError.TypeMismatch;
        const label_block = c.LLVMAppendBasicBlockInContext(self.context, current_function, "label");
        const label_copy = utils.dupe(u8, self.allocator, label_stmt.label);
        try self.label_blocks.put(label_copy, label_block);
        const saved_block = c.LLVMGetInsertBlock(self.builder);
        var i: usize = 0;
        while (i < self.pending_gotos.items.len) {
            const pending = self.pending_gotos.items[i];
            if (std.mem.eql(u8, pending.label, label_stmt.label)) {
                c.LLVMPositionBuilderAtEnd(self.builder, pending.goto_block);
                _ = c.LLVMBuildBr(self.builder, label_block);
                self.allocator.free(pending.label);
                _ = self.pending_gotos.orderedRemove(i);
            } else {
                i += 1;
            }
        }

        c.LLVMPositionBuilderAtEnd(self.builder, saved_block);
        const current_block = c.LLVMGetInsertBlock(self.builder);
        const terminator = c.LLVMGetBasicBlockTerminator(current_block);
        if (terminator == null) {
            const block_name_ptr = c.LLVMGetBasicBlockName(current_block);
            const is_after_goto = if (block_name_ptr != null)
                std.mem.startsWith(u8, std.mem.span(block_name_ptr), "after_goto")
            else
                false;

            if (is_after_goto) {
                _ = c.LLVMBuildUnreachable(self.builder);
            } else {
                _ = c.LLVMBuildBr(self.builder, label_block);
            }
        }

        c.LLVMPositionBuilderAtEnd(self.builder, label_block);
    }

    pub const processGlobalEnums = enums.processGlobalEnums;

    const FixedArrayInfo = struct {
        element_type_name: []const u8,
        array_size: usize,
    };

    pub fn parseFixedArrayType(type_name: []const u8) ?FixedArrayInfo {
        if (!(std.mem.startsWith(u8, type_name, "arr<") and std.mem.endsWith(u8, type_name, ">"))) {
            return null;
        }

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
            const parsed_size = std.fmt.parseInt(usize, size_str, 10) catch return null;
            return .{
                .element_type_name = element_type_name,
                .array_size = parsed_size,
            };
        }

        return null;
    }

    fn createGlobalConstStringPtr(self: *CodeGenerator, str_lit: ast.StringLiteral) errors.CodegenError!c.LLVMValueRef {
        const parsed = try strings.parseEscape(self.allocator, str_lit.value);
        defer self.allocator.free(parsed);

        const arr_ty = c.LLVMArrayType(c.LLVMInt8TypeInContext(self.context), @intCast(parsed.len));
        const str_const = c.LLVMConstStringInContext(self.context, parsed.ptr, @intCast(parsed.len), 1);

        const name = try std.fmt.allocPrint(self.allocator, "__zlang_gstr_{d}", .{self.global_string_counter});
        defer self.allocator.free(name);
        self.global_string_counter += 1;

        const name_z = utils.dupeZ(self.allocator, name);
        defer self.allocator.free(name_z);

        const global_str = c.LLVMAddGlobal(self.module, arr_ty, name_z.ptr);
        c.LLVMSetInitializer(global_str, str_const);
        c.LLVMSetLinkage(global_str, c.LLVMPrivateLinkage);
        c.LLVMSetGlobalConstant(global_str, 1);

        const i8_ptr = c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0);
        return c.LLVMConstBitCast(global_str, i8_ptr);
    }

    fn buildGlobalConstStructInitializer(self: *CodeGenerator, struct_init: ast.StructInitializer, struct_type: c.LLVMTypeRef) errors.CodegenError!c.LLVMValueRef {
        const struct_decl = self.getStructDecl(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
        const field_map = self.struct_fields.get(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;

        var field_values: std.ArrayList(c.LLVMValueRef) = .empty;
        defer field_values.deinit(self.allocator);
        try field_values.resize(self.allocator, struct_decl.fields.items.len);

        for (struct_decl.fields.items, 0..) |field, i| {
            const field_type = try self.getLLVMType(field.type_name);
            if (field.default_value) |default_val| {
                field_values.items[i] = try self.buildGlobalConstValue(default_val, field_type, field.type_name);
            } else {
                field_values.items[i] = c.LLVMConstNull(field_type);
            }
        }

        for (struct_init.field_values.items, 0..) |field_val, idx| {
            const field_index = if (field_val.field_name) |field_name|
                field_map.get(field_name) orelse return errors.CodegenError.UndefinedVariable
            else
                @as(c_uint, @intCast(idx));
            const field_decl = struct_decl.fields.items[field_index];
            const field_type = try self.getLLVMType(field_decl.type_name);
            field_values.items[field_index] = try self.buildGlobalConstValue(field_val.value, field_type, field_decl.type_name);
        }

        return c.LLVMConstNamedStruct(struct_type, field_values.items.ptr, @intCast(field_values.items.len));
    }

    fn buildGlobalConstValue(self: *CodeGenerator, expr: *ast.Node, expected_type: c.LLVMTypeRef, expected_type_name: []const u8) errors.CodegenError!c.LLVMValueRef {
        switch (expr.data) {
            .string_literal => |str_lit| {
                const kind = c.LLVMGetTypeKind(expected_type);
                if (kind == c.LLVMArrayTypeKind) {
                    const elem_type = c.LLVMGetElementType(expected_type);
                    if (c.LLVMGetTypeKind(elem_type) != c.LLVMIntegerTypeKind or c.LLVMGetIntTypeWidth(elem_type) != 8) {
                        return errors.CodegenError.TypeMismatch;
                    }

                    const parsed = try strings.parseEscape(self.allocator, str_lit.value);
                    defer self.allocator.free(parsed);

                    const arr_len = c.LLVMGetArrayLength(expected_type);
                    if (parsed.len > arr_len) return errors.CodegenError.TypeMismatch;

                    var bytes: std.ArrayList(c.LLVMValueRef) = .empty;
                    defer bytes.deinit(self.allocator);
                    try bytes.resize(self.allocator, arr_len);
                    const zero = c.LLVMConstInt(elem_type, 0, 0);
                    for (0..arr_len) |i| bytes.items[i] = zero;
                    for (parsed, 0..) |ch, i| {
                        bytes.items[i] = c.LLVMConstInt(elem_type, @intCast(ch), 0);
                    }
                    return c.LLVMConstArray(elem_type, bytes.items.ptr, arr_len);
                }

                if (kind == c.LLVMPointerTypeKind and std.mem.eql(u8, expected_type_name, "ptr<u8>")) {
                    const ptr_val = try self.createGlobalConstStringPtr(str_lit);
                    if (self.typesAreEqual(c.LLVMTypeOf(ptr_val), expected_type)) return ptr_val;
                    return c.LLVMConstBitCast(ptr_val, expected_type);
                }

                return errors.CodegenError.TypeMismatch;
            },
            .struct_initializer => |struct_init| {
                if (c.LLVMGetTypeKind(expected_type) != c.LLVMStructTypeKind) return errors.CodegenError.TypeMismatch;
                return self.buildGlobalConstStructInitializer(struct_init, expected_type);
            },
            .array_initializer => |arr| {
                if (c.LLVMGetTypeKind(expected_type) != c.LLVMArrayTypeKind) return errors.CodegenError.TypeMismatch;
                const element_type = c.LLVMGetElementType(expected_type);
                const arr_len = c.LLVMGetArrayLength(expected_type);
                if (arr.elements.items.len > arr_len) return errors.CodegenError.TypeMismatch;

                var elems: std.ArrayList(c.LLVMValueRef) = .empty;
                defer elems.deinit(self.allocator);
                try elems.resize(self.allocator, arr_len);
                const zero = c.LLVMConstNull(element_type);
                for (0..arr_len) |i| elems.items[i] = zero;

                for (arr.elements.items, 0..) |item, i| {
                    const v = try self.buildGlobalConstValue(item, element_type, self.getTypeNameFromLLVMType(element_type));
                    elems.items[i] = v;
                }
                return c.LLVMConstArray(element_type, elems.items.ptr, arr_len);
            },
            else => {
                var v = try self.generateExpressionWithContext(expr, expected_type_name);
                if (!self.typesAreEqual(c.LLVMTypeOf(v), expected_type)) {
                    v = try self.castWithSourceRules(v, expected_type, expr);
                }
                if (c.LLVMIsConstant(v) == 0) return errors.CodegenError.TypeMismatch;
                return v;
            },
        }
    }

    fn buildGlobalArrayInitializer(self: *CodeGenerator, decl: ast.VarDecl, array_type: c.LLVMTypeRef, array_info: FixedArrayInfo) errors.CodegenError!c.LLVMValueRef {
        const element_type = c.LLVMGetElementType(array_type);

        var const_elements: std.ArrayList(c.LLVMValueRef) = .empty;
        defer const_elements.deinit(self.allocator);
        try const_elements.resize(self.allocator, array_info.array_size);

        const zero = c.LLVMConstNull(element_type);
        for (0..array_info.array_size) |i| {
            const_elements.items[i] = zero;
        }

        if (decl.initializer) |initializer| {
            switch (initializer.data) {
                .array_initializer => |init_list| {
                    if (init_list.elements.items.len > array_info.array_size) {
                        self.reportErrorFmt("Array initializer for global '{s}' has {d} elements, but size is {d}", .{ decl.name, init_list.elements.items.len, array_info.array_size }, "Reduce elements or increase declared array size");
                        return errors.CodegenError.TypeMismatch;
                    }

                    for (init_list.elements.items, 0..) |element, idx| {
                        const element_const = self.buildGlobalConstValue(element, element_type, array_info.element_type_name) catch {
                            self.reportErrorFmt("Global array '{s}' must be initialized with compile-time constants", .{decl.name}, "Use only literals in global array initializers");
                            return errors.CodegenError.TypeMismatch;
                        };

                        const_elements.items[idx] = element_const;
                    }
                },
                .string_literal => |str_lit| {
                    const elem_kind = c.LLVMGetTypeKind(element_type);
                    if (elem_kind != c.LLVMIntegerTypeKind or c.LLVMGetIntTypeWidth(element_type) != 8) {
                        self.reportTypeMismatchStr("arr<u8, N>", array_type, "global string initializer");
                        return errors.CodegenError.TypeMismatch;
                    }

                    const parsed_str = try strings.parseEscape(self.allocator, str_lit.value);
                    defer self.allocator.free(parsed_str);

                    if (parsed_str.len > array_info.array_size) {
                        self.reportErrorFmt("String literal for global '{s}' is too long ({d} > {d})", .{ decl.name, parsed_str.len, array_info.array_size }, "Increase array size or shorten the literal");
                        return errors.CodegenError.TypeMismatch;
                    }

                    for (parsed_str, 0..) |byte, idx| {
                        const_elements.items[idx] = c.LLVMConstInt(element_type, @as(c_ulonglong, @intCast(byte)), 0);
                    }
                },
                else => {
                    self.reportErrorFmt("Unsupported global array initializer for '{s}'", .{decl.name}, "Use array literals or string literals");
                    return errors.CodegenError.TypeMismatch;
                },
            }
        }

        return c.LLVMConstArray(element_type, const_elements.items.ptr, @intCast(array_info.array_size));
    }

    fn generateGlobalDeclaration(self: *CodeGenerator, global_node: *ast.Node) errors.CodegenError!void {
        self.setCurrentNodeContext(global_node);
        switch (global_node.data) {
            .var_decl => |decl| {
                self.setCurrentModuleByGlobal(decl.name);
                if (utils.isVarArgType(decl.type_name)) {
                    if (self.current_line > 0) {
                        self.reportErrorFmt("Invalid type 'vararg' for variable '{s}'", .{decl.name}, "Variadic arguments can only be used as the last parameter of a function");
                    } else {
                        self.reportErrorFmt("Invalid type 'vararg' for variable '{s}'", .{decl.name}, "Variadic arguments can only be used as the last parameter of a function");
                    }
                    return errors.CodegenError.TypeMismatch;
                }
                const var_type = try self.getLLVMType(decl.type_name);
                const var_name_z = utils.dupeZ(self.allocator, decl.name);
                defer self.allocator.free(var_name_z);
                const global_var = c.LLVMAddGlobal(self.module, @ptrCast(var_type), var_name_z.ptr);

                const fixed_array_info = parseFixedArrayType(decl.type_name);
                const is_fixed_array = fixed_array_info != null and c.LLVMGetTypeKind(var_type) == c.LLVMArrayTypeKind;

                if (is_fixed_array) {
                    const array_init = try self.buildGlobalArrayInitializer(decl, var_type, fixed_array_info.?);
                    c.LLVMSetInitializer(global_var, array_init);
                } else if (decl.initializer) |initializer| {
                    var allow_runtime_init = false;

                    const casted_init_value = blk: {
                        if (initializer.data == .function_call or initializer.data == .method_call) {
                            allow_runtime_init = true;
                            break :blk c.LLVMConstNull(var_type);
                        }

                        if (initializer.data == .array_initializer and c.LLVMGetTypeKind(var_type) == c.LLVMStructTypeKind) {
                            const arr = initializer.data.array_initializer;
                            if (arr.elements.items.len == 1 and arr.elements.items[0].data == .number_literal and std.mem.eql(u8, arr.elements.items[0].data.number_literal.value, "0")) {
                                break :blk c.LLVMConstNull(var_type);
                            }
                        }

                        break :blk self.buildGlobalConstValue(initializer, var_type, decl.type_name) catch {
                            allow_runtime_init = true;
                            break :blk c.LLVMConstNull(var_type);
                        };
                    };

                    if (c.LLVMIsConstant(casted_init_value) == 0) {
                        self.reportErrorFmt("Global variable '{s}' must be initialized with compile-time constants", .{decl.name}, "Use literal values or compile-time constants for globals");
                        return errors.CodegenError.TypeMismatch;
                    }
                    c.LLVMSetInitializer(global_var, casted_init_value);

                    if (allow_runtime_init and !decl.is_const) {
                        try self.pending_global_inits.append(self.allocator, .{
                            .global_name = decl.name,
                            .global_var = global_var,
                            .initializer = initializer,
                            .type_ref = var_type,
                            .type_name = decl.type_name,
                        });
                    } else if (allow_runtime_init and decl.is_const) {
                        self.reportErrorFmt("Global variable '{s}' must be initialized with compile-time constants", .{decl.name}, "const globals cannot use runtime initialization");
                        return errors.CodegenError.TypeMismatch;
                    }
                } else {
                    const default_value = utils.getDefaultValueForType(self, decl.type_name);
                    c.LLVMSetInitializer(global_var, @ptrCast(default_value));
                }

                c.LLVMSetLinkage(global_var, c.LLVMExternalLinkage);
                if (decl.is_const) {
                    c.LLVMSetGlobalConstant(global_var, 1);
                }
                try self.variables.put(utils.dupe(u8, self.allocator, decl.name), structs.VariableInfo{
                    .value = @ptrCast(global_var),
                    .type_ref = @ptrCast(var_type),
                    .type_name = decl.type_name,
                    .is_const = decl.is_const,
                });
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    pub const castToType = casts.castToType;
    pub const castToTypeWithSourceInfo = casts.castToTypeWithSourceInfo;
    pub const castWithSourceRules = casts.castWithSourceRules;
    pub const castWithSourceRulesNamed = casts.castWithSourceRulesNamed;
    pub const castWithRules = casts.castWithRules;
    pub const typesAreEqual = casts.typesAreEqual;
    pub const pointerTypeMatchesTargetStruct = casts.pointerTypeMatchesTargetStruct;

    pub const parse_escape = strings.parseEscape;

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

    pub const generateExpressionWithContext = expressions.generateExpressionWithContext;
    pub const inferType = expressions.inferType;
    pub const generateExpression = expressions.generateExpression;
    pub const convertToBool = utils.convertToBool;

    pub fn writeToFile(self: *CodeGenerator, filename: []const u8) !void {
        return backend.writeToFile(self, filename);
    }

    pub fn writeBitcodeToFile(self: *CodeGenerator, filename: []const u8) !void {
        return backend.writeBitcodeToFile(self, filename);
    }

    pub fn verifyModule(self: *CodeGenerator) !void {
        return backend.verifyModule(self);
    }

    pub fn emitLLVMIR(self: *CodeGenerator, base_name: []const u8, optimize: bool) ![]const u8 {
        return backend.emitLLVMIR(self, base_name, optimize);
    }

    pub fn compileIRToObjectFile(self: *CodeGenerator, ir_input_file: []const u8, obj_output_file: []const u8, arch: []const u8, optimize: bool, max_threads: usize, llc_time_ns: ?*u64) !void {
        return backend.compileIRToObjectFile(self, ir_input_file, obj_output_file, arch, optimize, max_threads, llc_time_ns);
    }

    pub fn splitBitcodeIntoUnits(self: *CodeGenerator, bitcode_file: []const u8, output_prefix: []const u8, unit_count: usize, arch: []const u8, out_units: *std.ArrayList([]const u8)) !bool {
        return backend.splitBitcodeIntoUnits(self, bitcode_file, output_prefix, unit_count, arch, out_units);
    }

    pub fn linkObjectFilesToExecutable(self: *CodeGenerator, output: []const u8, arch: []const u8, generated_objects: []const []const u8, link_objects: []const []const u8, extra_flags: []const []const u8, max_threads: usize, timing: ?*BackendTiming) !void {
        return backend.linkObjectFilesToExecutable(self, output, arch, generated_objects, link_objects, extra_flags, max_threads, timing);
    }

    pub fn compileToExecutable(self: *CodeGenerator, output: []const u8, arch: []const u8, link_objects: []const []const u8, keep_ll: bool, optimize: bool, extra_flags: []const []const u8, max_threads: usize, timing: ?*BackendTiming) !void {
        return backend.compileToExecutable(self, output, arch, link_objects, keep_ll, optimize, extra_flags, max_threads, timing);
    }
};
