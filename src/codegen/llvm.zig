const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
const structs = @import("structs.zig");
const bfck = @import("bf.zig");
const control_flow = @import("control_flow.zig");
const variables = @import("variables.zig");
const functions = @import("functions.zig");
const numeric = @import("numeric.zig");

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
    regular_functions: std.HashMap([]const u8, bool, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    sret_functions: std.HashMap([]const u8, c.LLVMTypeRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    current_function: ?c.LLVMValueRef,
    current_function_return_type: []const u8,
    control_flow_analyzer: control_flow.ControlFlowAnalyzer,
    loop_context_stack: std.ArrayList(structs.LoopContext),
    variable_scopes: std.ArrayList(std.HashMap([]const u8, structs.VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage)),
    uses_float_modulo: bool,
    struct_types: std.HashMap([]const u8, c.LLVMTypeRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    struct_fields: std.HashMap([]const u8, std.HashMap([]const u8, c_uint, std.hash_map.StringContext, std.hash_map.default_max_load_percentage), std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    struct_declarations: std.HashMap([]const u8, ast.StructDecl, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    function_to_module: std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    module_dependencies: std.HashMap([]const u8, std.ArrayList([]const u8), std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    current_module_name: []const u8,
    label_blocks: std.HashMap([]const u8, c.LLVMBasicBlockRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    pending_gotos: std.ArrayList(structs.PendingGoto),

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

        var variable_scopes = std.ArrayList(std.HashMap([]const u8, structs.VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage)){};
        try variable_scopes.append(allocator, std.HashMap([]const u8, structs.VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator));

        return CodeGenerator{
            .context = context,
            .module = module,
            .builder = builder,
            .allocator = allocator,
            .functions = std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .variables = std.HashMap([]const u8, structs.VariableInfo, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .external_c_functions = std.HashMap([]const u8, c.LLVMValueRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .c_function_declarations = std.HashMap([]const u8, bool, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .regular_functions = std.HashMap([]const u8, bool, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .sret_functions = std.HashMap([]const u8, c.LLVMTypeRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .current_function = null,
            .current_function_return_type = "",
            .control_flow_analyzer = control_flow_analyzer,
            .loop_context_stack = std.ArrayList(structs.LoopContext){},
            .variable_scopes = variable_scopes,
            .uses_float_modulo = false,
            .struct_types = std.HashMap([]const u8, c.LLVMTypeRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .struct_fields = std.HashMap([]const u8, std.HashMap([]const u8, c_uint, std.hash_map.StringContext, std.hash_map.default_max_load_percentage), std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .struct_declarations = std.HashMap([]const u8, ast.StructDecl, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .function_to_module = std.HashMap([]const u8, []const u8, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .module_dependencies = std.HashMap([]const u8, std.ArrayList([]const u8), std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .current_module_name = "",
            .label_blocks = std.HashMap([]const u8, c.LLVMBasicBlockRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .pending_gotos = std.ArrayList(structs.PendingGoto){},
        };
    }

    pub fn deinit(self: *CodeGenerator) void {
        self.functions.deinit();
        self.variables.deinit();
        self.external_c_functions.deinit();
        self.c_function_declarations.deinit();
        self.regular_functions.deinit();
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

        var deps_it = self.module_dependencies.iterator();
        while (deps_it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.module_dependencies.deinit();
        self.function_to_module.deinit();
        for (self.variable_scopes.items) |*scope| {
            scope.deinit();
        }
        self.variable_scopes.deinit(self.allocator);
        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.context);
    }

    pub fn registerModule(self: *CodeGenerator, module_name: []const u8, deps: []const []const u8) !void {
        var list = std.ArrayList([]const u8){};
        for (deps) |d| {
            try list.append(self.allocator, utils.dupe(u8, self.allocator, d));
        }
        try self.module_dependencies.put(utils.dupe(u8, self.allocator, module_name), list);
    }

    pub fn registerFunctionModule(self: *CodeGenerator, func_name: []const u8, module_name: []const u8) !void {
        try self.function_to_module.put(utils.dupe(u8, self.allocator, func_name), utils.dupe(u8, self.allocator, module_name));
    }

    pub fn setCurrentModuleByFunction(self: *CodeGenerator, func_name: []const u8) void {
        if (self.function_to_module.get(func_name)) |m| {
            self.current_module_name = m;
        } else {
            self.current_module_name = "";
        }
    }

    pub fn canAccess(self: *CodeGenerator, from_module: []const u8, target_module: []const u8) bool {
        if (from_module.len == 0 or std.mem.eql(u8, from_module, target_module)) return true;
        if (self.module_dependencies.get(from_module)) |list| {
            for (list.items) |dep| {
                if (std.mem.eql(u8, dep, target_module)) return true;
            }
        }
        return false;
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

    fn getBaseIdentifierName(self: *CodeGenerator, node: *ast.Node) ![]const u8 {
        switch (node.data) {
            .identifier => |ident| return utils.dupe(u8, self.allocator, ident.name),
            .qualified_identifier => |qual_id| return try self.getBaseIdentifierName(qual_id.base),
            .array_index => |arr_idx| return try self.getBaseIdentifierName(arr_idx.array),
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    fn containsArrayIndex(self: *CodeGenerator, node: *ast.Node) !bool {
        switch (node.data) {
            .array_index => return true,
            .qualified_identifier => |qual_id| return try self.containsArrayIndex(qual_id.base),
            else => return false,
        }
    }

    fn extractArrayIndex(self: *CodeGenerator, node: *ast.Node) !ast.ArrayIndex {
        switch (node.data) {
            .array_index => |arr_idx| return arr_idx,
            .qualified_identifier => |qual_id| return try self.extractArrayIndex(qual_id.base),
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    fn collectArrayIndices(self: *CodeGenerator, node: *ast.Node, indices: *std.ArrayList(c.LLVMValueRef)) !*ast.Node {
        switch (node.data) {
            .array_index => |arr_idx| {
                const base = try self.collectArrayIndices(arr_idx.array, indices);
                var index_value = try self.generateExpression(arr_idx.index);
                index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
                try indices.append(self.allocator, index_value);
                return base;
            },
            else => return node,
        }
    }

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
                var collected_indices = std.ArrayList(c.LLVMValueRef){};
                defer collected_indices.deinit(self.allocator);
                const base_node = try self.collectArrayIndices(arr_idx.array, &collected_indices);
                _ = base_node;
                const index_value = try self.generateExpression(arr_idx.index);
                try collected_indices.append(self.allocator, index_value);

                var all_indices = std.ArrayList(c.LLVMValueRef){};
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

    fn getQualifiedFieldPtrAndType(self: *CodeGenerator, qual_node: *ast.Node) errors.CodegenError!struct { ptr: c.LLVMValueRef, ty: c.LLVMTypeRef } {
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
                base_val = var_info.value;
                base_ty = var_info.type_ref;
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
                    const tmp = c.LLVMBuildAlloca(self.builder, struct_ty, "tmp_struct");
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

    fn generateArrayElementFieldAssignment(self: *CodeGenerator, array_index: ast.ArrayIndex, field_path: []const u8, value_expr: *ast.Node) errors.CodegenError!void {
        const array_name = try self.getBaseIdentifierName(array_index.array);
        defer self.allocator.free(array_name);
        const array_var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;
        var index_value = try self.generateExpression(array_index.index);
        index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
        const element_type = c.LLVMGetElementType(@ptrCast(array_var_info.type_ref));
        const element_type_kind = c.LLVMGetTypeKind(element_type);
        if (element_type_kind != c.LLVMStructTypeKind) {
            return errors.CodegenError.TypeMismatch;
        }
        const struct_name = try self.getStructTypeName(@ptrCast(element_type));
        defer self.allocator.free(struct_name);
        var array_indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_value };
        const array_element_ptr = c.LLVMBuildGEP2(@ptrCast(self.builder), @ptrCast(array_var_info.type_ref), @ptrCast(array_var_info.value), &array_indices[0], 2, "array_element_ptr");
        try self.generateRecursiveFieldAssignment(array_element_ptr, element_type, struct_name, field_path, value_expr);
    }

    pub const getStructTypeName = structs.getStructTypeName;

    pub const generateRecursiveFieldAssignment = structs.generateRecursiveFieldAssignment;

    pub const pushScope = variables.pushScope;

    pub const popScope = variables.popScope;

    pub const clearCurrentFunctionScopes = variables.clearCurrentFunctionScopes;

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
            std.debug.print("Error: Cannot reassign const variable '{s}'\n", .{ident.name});
            return errors.CodegenError.ConstReassignment;
        }

        const var_type_kind = c.LLVMGetTypeKind(@ptrCast(var_info.type_ref));
        if (var_type_kind == c.LLVMArrayTypeKind) {
            try self.generateArrayReassignment(ident.name, as.value);
        } else {
            const value_raw = try self.generateExpressionWithContext(as.value, var_info.type_name);
            const final_value = try self.castWithSourceRules(value_raw, @ptrCast(var_info.type_ref), as.value);
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
        switch (program.data) {
            .program => |prog| {
                for (prog.globals.items) |glob| try self.generateGlobalDeclaration(glob);
                for (prog.functions.items) |func| {
                    if (func.data == .enum_decl) {
                        try self.generateEnumDeclaration(func.data.enum_decl);
                    } else if (func.data == .struct_decl) {
                        try self.generateStructType(func.data.struct_decl);
                    } else if (func.data == .c_function_decl) {
                        try self.generateCFunctionDeclaration(func.data.c_function_decl);
                    } else if (func.data == .function) {
                        try self.declareFunction(func.data.function);
                    }
                }
                for (prog.functions.items) |func| {
                    if (func.data == .function) try self.generateFunctionBody(func.data.function);
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

    pub fn generateStatement(self: *CodeGenerator, stmt: *ast.Node) errors.CodegenError!void {
        switch (stmt.data) {
            .assignment => |as| {
                switch (as.target.data) {
                    .identifier => {
                        if (as.value.data == .struct_initializer) {
                            const ident = as.target.data.identifier;
                            const var_info = CodeGenerator.getVariable(self, ident.name) orelse return errors.CodegenError.UndefinedVariable;
                            const struct_init = as.value.data.struct_initializer;
                            const struct_type = self.struct_types.get(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
                            const field_map = self.struct_fields.get(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
                            const struct_decl = self.getStructDecl(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
                            for (struct_decl.fields.items, 0..) |field, i| {
                                if (field.default_value) |default_val| {
                                    const field_ptr = try self.getStructFieldPointer(@ptrCast(struct_type), var_info.value, @intCast(i));
                                    const field_type = self.getLLVMType(field.type_name);
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
                                const field_type = c.LLVMStructGetTypeAtIndex(struct_type, field_index);
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
                        try self.generateArrayAssignment(arr_ass);
                    },
                    else => {
                        return errors.CodegenError.TypeMismatch;
                    },
                }
            },
            .var_decl => |decl| {
                if (std.mem.eql(u8, decl.type_name, "void")) {
                    if (decl.initializer != null) {
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
                    try self.generateArrayDeclaration(decl);
                } else if (std.mem.startsWith(u8, decl.type_name, "simd<") and std.mem.endsWith(u8, decl.type_name, ">")) {
                    try self.generateSimdDeclaration(decl);
                } else {
                    const var_type = self.getLLVMType(decl.type_name);
                    const alloca = c.LLVMBuildAlloca(@ptrCast(self.builder), @ptrCast(var_type), decl.name.ptr);
                    try CodeGenerator.putVariable(self, decl.name, structs.VariableInfo{
                        .value = @ptrCast(alloca),
                        .type_ref = @ptrCast(var_type),
                        .type_name = decl.type_name,
                        .is_const = decl.is_const,
                    });
                    if (decl.initializer) |initializer| {
                        if (initializer.data == .struct_initializer) {
                            const struct_init = initializer.data.struct_initializer;
                            const struct_type = self.struct_types.get(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
                            const field_map = self.struct_fields.get(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
                            const struct_decl = self.getStructDecl(struct_init.struct_name) orelse return errors.CodegenError.TypeMismatch;
                            for (struct_decl.fields.items, 0..) |field, i| {
                                if (field.default_value) |default_val| {
                                    const field_ptr = try self.getStructFieldPointer(@ptrCast(struct_type), @ptrCast(alloca), @intCast(i));
                                    const field_type = self.getLLVMType(field.type_name);
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
                                const field_type = c.LLVMStructGetTypeAtIndex(struct_type, field_index);
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
                            const struct_name = std.mem.span(c.LLVMGetStructName(var_type));
                            const struct_decl = self.getStructDecl(struct_name) orelse return errors.CodegenError.TypeMismatch;

                            // Zero-initialize all fields
                            for (struct_decl.fields.items, 0..) |field, i| {
                                const field_ptr = try self.getStructFieldPointer(var_type, alloca, @intCast(i));
                                const zero_value = utils.getDefaultValueForType(self, field.type_name);
                                _ = c.LLVMBuildStore(self.builder, zero_value, field_ptr);
                            }

                            // Apply default values if they exist
                            for (struct_decl.fields.items, 0..) |field, i| {
                                if (field.default_value) |default_val| {
                                    const field_ptr = try self.getStructFieldPointer(var_type, alloca, @intCast(i));
                                    const value = try self.generateExpression(default_val);
                                    const casted_value = try self.castWithRules(value, self.getLLVMType(field.type_name), default_val);
                                    _ = c.LLVMBuildStore(self.builder, casted_value, field_ptr);
                                }
                            }
                            const init_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                            const casted_value = try self.castWithSourceRules(init_value, var_type, initializer);
                            _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                        } else {
                            const init_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                            const casted_value = try self.castWithSourceRules(init_value, var_type, initializer);
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
                                const field_type = self.getLLVMType(field.type_name);
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
                    .identifier => |ident| {
                        const var_info = CodeGenerator.getVariable(self, ident.name) orelse return errors.CodegenError.UndefinedVariable;

                        if (var_info.is_const) {
                            std.debug.print("Error: Cannot reassign const variable '{s}'\n", .{ident.name});
                            return errors.CodegenError.ConstReassignment;
                        }

                        const current_value = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_current");
                        const rhs_value = try self.generateExpressionWithContext(cas.value, var_info.type_name);
                        const rhs_casted = try self.castWithSourceRules(rhs_value, @ptrCast(var_info.type_ref), cas.value);

                        const is_float = std.mem.eql(u8, var_info.type_name, "f16") or
                            std.mem.eql(u8, var_info.type_name, "f32") or
                            std.mem.eql(u8, var_info.type_name, "f64");

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
                            else
                                c.LLVMBuildSDiv(self.builder, current_value, rhs_casted, "sdiv_compound"),
                            '%' => if (is_float) blk: {
                                self.uses_float_modulo = true;
                                break :blk c.LLVMBuildFRem(self.builder, current_value, rhs_casted, "frem_compound");
                            } else blk: {
                                const is_unsigned = isUnsignedType(var_info.type_name);
                                break :blk if (is_unsigned)
                                    c.LLVMBuildURem(self.builder, current_value, rhs_casted, "urem_compound")
                                else
                                    c.LLVMBuildSRem(self.builder, current_value, rhs_casted, "srem_compound");
                            },
                            'A' => c.LLVMBuildAnd(self.builder, current_value, rhs_casted, "and_compound"),
                            '$' => c.LLVMBuildOr(self.builder, current_value, rhs_casted, "or_compound"),
                            '^' => c.LLVMBuildXor(self.builder, current_value, rhs_casted, "xor_compound"),
                            '<' => c.LLVMBuildShl(self.builder, current_value, rhs_casted, "shl_compound"),
                            '>' => c.LLVMBuildAShr(self.builder, current_value, rhs_casted, "ashr_compound"),
                            else => return errors.CodegenError.UnsupportedOperation,
                        };
                        _ = c.LLVMBuildStore(self.builder, new_value, var_info.value);
                    },
                    .array_index => |arr_idx| {
                        var collected_indices = std.ArrayList(c.LLVMValueRef){};
                        defer collected_indices.deinit(self.allocator);
                        const base_node = try self.collectArrayIndices(arr_idx.array, &collected_indices);
                        var index_value = try self.generateExpression(arr_idx.index);
                        index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
                        try collected_indices.append(self.allocator, index_value);
                        const array_name = try self.getBaseIdentifierName(base_node);
                        defer self.allocator.free(array_name);
                        const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;

                        if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
                            const ptr_val = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_ptr_compound");
                            const element_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                            const element_type = self.getLLVMType(element_type_name);
                            const idx = collected_indices.items[collected_indices.items.len - 1];
                            var indices = [_]c.LLVMValueRef{idx};
                            const element_ptr = c.LLVMBuildGEP2(self.builder, element_type, ptr_val, &indices[0], 1, "ptr_index_compound");
                            const current_val = c.LLVMBuildLoad2(self.builder, element_type, element_ptr, "load_elem");
                            const expected_ty_name = self.getTypeNameFromLLVMType(element_type);
                            const rhs_raw = try self.generateExpressionWithContext(cas.value, expected_ty_name);
                            const rhs_val = try self.castWithRules(rhs_raw, element_type, cas.value);

                            const is_float = std.mem.eql(u8, expected_ty_name, "f16") or
                                std.mem.eql(u8, expected_ty_name, "f32") or
                                std.mem.eql(u8, expected_ty_name, "f64");

                            const new_value = switch (cas.op) {
                                '+' => if (is_float)
                                    c.LLVMBuildFAdd(self.builder, current_val, rhs_val, "fadd_ptr_compound")
                                else
                                    c.LLVMBuildAdd(self.builder, current_val, rhs_val, "add_ptr_compound"),
                                '-' => if (is_float)
                                    c.LLVMBuildFSub(self.builder, current_val, rhs_val, "fsub_ptr_compound")
                                else
                                    c.LLVMBuildSub(self.builder, current_val, rhs_val, "sub_ptr_compound"),
                                '*' => if (is_float)
                                    c.LLVMBuildFMul(self.builder, current_val, rhs_val, "fmul_ptr_compound")
                                else
                                    c.LLVMBuildMul(self.builder, current_val, rhs_val, "mul_ptr_compound"),
                                '/' => if (is_float)
                                    c.LLVMBuildFDiv(self.builder, current_val, rhs_val, "fdiv_ptr_compound")
                                else
                                    c.LLVMBuildSDiv(self.builder, current_val, rhs_val, "sdiv_ptr_compound"),
                                '%' => if (is_float) blk: {
                                    self.uses_float_modulo = true;
                                    break :blk c.LLVMBuildFRem(self.builder, current_val, rhs_val, "frem_ptr_compound");
                                } else blk: {
                                    const is_unsigned = isUnsignedType(expected_ty_name);
                                    break :blk if (is_unsigned)
                                        c.LLVMBuildURem(self.builder, current_val, rhs_val, "urem_ptr_compound")
                                    else
                                        c.LLVMBuildSRem(self.builder, current_val, rhs_val, "srem_ptr_compound");
                                },
                                'A' => c.LLVMBuildAnd(self.builder, current_val, rhs_val, "and_ptr_compound"),
                                '$' => c.LLVMBuildOr(self.builder, current_val, rhs_val, "or_ptr_compound"),
                                '^' => c.LLVMBuildXor(self.builder, current_val, rhs_val, "xor_ptr_compound"),
                                '<' => c.LLVMBuildShl(self.builder, current_val, rhs_val, "shl_ptr_compound"),
                                '>' => c.LLVMBuildAShr(self.builder, current_val, rhs_val, "ashr_ptr_compound"),
                                else => return errors.CodegenError.UnsupportedOperation,
                            };
                            _ = c.LLVMBuildStore(self.builder, new_value, element_ptr);
                            return;
                        }

                        var final_type = var_info.type_ref;
                        for (0..collected_indices.items.len) |_| final_type = c.LLVMGetElementType(final_type);
                        var all_indices = std.ArrayList(c.LLVMValueRef){};
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
                        const rhs_raw = try self.generateExpressionWithContext(cas.value, expected_ty_name);
                        const rhs_val = try self.castWithRules(rhs_raw, final_type, cas.value);

                        const is_float = std.mem.eql(u8, expected_ty_name, "f16") or
                            std.mem.eql(u8, expected_ty_name, "f32") or
                            std.mem.eql(u8, expected_ty_name, "f64");

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
                            else
                                c.LLVMBuildSDiv(self.builder, current_val, rhs_val, "sdiv_array_compound"),
                            '%' => if (is_float) blk: {
                                self.uses_float_modulo = true;
                                break :blk c.LLVMBuildFRem(self.builder, current_val, rhs_val, "frem_array_compound");
                            } else blk: {
                                const is_unsigned = isUnsignedType(expected_ty_name);
                                break :blk if (is_unsigned)
                                    c.LLVMBuildURem(self.builder, current_val, rhs_val, "urem_array_compound")
                                else
                                    c.LLVMBuildSRem(self.builder, current_val, rhs_val, "srem_array_compound");
                            },
                            'A' => c.LLVMBuildAnd(self.builder, current_val, rhs_val, "and_array_compound"),
                            '$' => c.LLVMBuildOr(self.builder, current_val, rhs_val, "or_array_compound"),
                            '^' => c.LLVMBuildXor(self.builder, current_val, rhs_val, "xor_array_compound"),
                            '<' => c.LLVMBuildShl(self.builder, current_val, rhs_val, "shl_array_compound"),
                            '>' => c.LLVMBuildAShr(self.builder, current_val, rhs_val, "ashr_array_compound"),
                            else => return errors.CodegenError.UnsupportedOperation,
                        };
                        _ = c.LLVMBuildStore(self.builder, new_value, element_ptr);
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
                        const field_type = self.getLLVMType(field_type_name);
                        const current_value = c.LLVMBuildLoad2(self.builder, field_type, field_ptr, "load_field");
                        const rhs_value = try self.generateExpressionWithContext(cas.value, field_type_name);
                        const rhs_casted = try self.castWithSourceRules(rhs_value, field_type, cas.value);

                        const is_float = std.mem.eql(u8, field_type_name, "f16") or
                            std.mem.eql(u8, field_type_name, "f32") or
                            std.mem.eql(u8, field_type_name, "f64");

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
                            else
                                c.LLVMBuildSDiv(self.builder, current_value, rhs_casted, "sdiv_field_compound"),
                            '%' => if (is_float) blk: {
                                self.uses_float_modulo = true;
                                break :blk c.LLVMBuildFRem(self.builder, current_value, rhs_casted, "frem_field_compound");
                            } else blk: {
                                const is_unsigned = isUnsignedType(field_type_name);
                                break :blk if (is_unsigned)
                                    c.LLVMBuildURem(self.builder, current_value, rhs_casted, "urem_field_compound")
                                else
                                    c.LLVMBuildSRem(self.builder, current_value, rhs_casted, "srem_field_compound");
                            },
                            'A' => c.LLVMBuildAnd(self.builder, current_value, rhs_casted, "and_field_compound"),
                            '$' => c.LLVMBuildOr(self.builder, current_value, rhs_casted, "or_field_compound"),
                            '^' => c.LLVMBuildXor(self.builder, current_value, rhs_casted, "xor_field_compound"),
                            '<' => c.LLVMBuildShl(self.builder, current_value, rhs_casted, "shl_field_compound"),
                            '>' => c.LLVMBuildAShr(self.builder, current_value, rhs_casted, "ashr_field_compound"),
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
            .method_call => {
                _ = try self.generateExpression(stmt);
            },
            .return_stmt => |ret| {
                if (ret.expression) |expr| {
                    const target_ty_name = self.current_function_return_type;
                    const ret_raw = try self.generateExpressionWithContext(expr, target_ty_name);
                    const target_ty = self.getLLVMType(target_ty_name);
                    const final_ret = try self.castWithRules(ret_raw, target_ty, expr);
                    _ = c.LLVMBuildRet(self.builder, final_ret);
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
            .goto_stmt => |goto_stmt| {
                try self.generateGotoStatement(goto_stmt);
            },
            .label_stmt => |label_stmt| {
                try self.generateLabelStatement(label_stmt);
            },
            .array_assignment => |arr_ass| {
                const array_name = try self.getBaseIdentifierName(arr_ass.array);
                defer self.allocator.free(array_name);
                const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;

                // Check if this is a SIMD vector assignment
                if (std.mem.startsWith(u8, var_info.type_name, "simd<")) {
                    const simd_val = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_simd");
                    var index_value = try self.generateExpression(arr_ass.index);
                    index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
                    const element_type = c.LLVMGetElementType(var_info.type_ref);
                    const element_value = try self.generateExpression(arr_ass.value);
                    const casted_value = try self.castWithRules(element_value, element_type, arr_ass.value);
                    const updated_simd = c.LLVMBuildInsertElement(self.builder, simd_val, casted_value, index_value, "simd_insert");
                    _ = c.LLVMBuildStore(self.builder, updated_simd, var_info.value);
                } else {
                    try self.generateArrayAssignment(arr_ass);
                }
            },
            .array_compound_assignment => |arr_cass| {
                var collected_indices = std.ArrayList(c.LLVMValueRef){};
                defer collected_indices.deinit(self.allocator);
                const base_node = try self.collectArrayIndices(arr_cass.array, &collected_indices);
                const index_value = try self.generateExpression(arr_cass.index);
                try collected_indices.append(self.allocator, index_value);
                const array_name = try self.getBaseIdentifierName(base_node);
                defer self.allocator.free(array_name);
                const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;
                if (std.mem.startsWith(u8, var_info.type_name, "simd<")) {
                    const current_simd = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_simd");
                    const current_element = c.LLVMBuildExtractElement(self.builder, current_simd, index_value, "extract_elem");
                    const element_type = c.LLVMGetElementType(var_info.type_ref);
                    const expected_ty_name = self.getTypeNameFromLLVMType(element_type);
                    const rhs_raw = try self.generateExpressionWithContext(arr_cass.value, expected_ty_name);
                    const rhs_val = try self.castWithRules(rhs_raw, element_type, arr_cass.value);
                    const element_kind = c.LLVMGetTypeKind(element_type);
                    const new_element = if (element_kind == c.LLVMFloatTypeKind or element_kind == c.LLVMDoubleTypeKind or element_kind == c.LLVMHalfTypeKind) blk: {
                        break :blk switch (arr_cass.op) {
                            '+' => c.LLVMBuildFAdd(self.builder, current_element, rhs_val, "fadd"),
                            '-' => c.LLVMBuildFSub(self.builder, current_element, rhs_val, "fsub"),
                            '*' => c.LLVMBuildFMul(self.builder, current_element, rhs_val, "fmul"),
                            '/' => c.LLVMBuildFDiv(self.builder, current_element, rhs_val, "fdiv"),
                            else => return errors.CodegenError.UnsupportedOperation,
                        };
                    } else if (element_kind == c.LLVMIntegerTypeKind) blk: {
                        break :blk switch (arr_cass.op) {
                            '+' => c.LLVMBuildAdd(self.builder, current_element, rhs_val, "add"),
                            '-' => c.LLVMBuildSub(self.builder, current_element, rhs_val, "sub"),
                            '*' => c.LLVMBuildMul(self.builder, current_element, rhs_val, "mul"),
                            '/' => blk2: {
                                const is_unsigned = isUnsignedType(self.getTypeNameFromLLVMType(element_type));
                                break :blk2 if (is_unsigned)
                                    c.LLVMBuildUDiv(self.builder, current_element, rhs_val, "udiv")
                                else
                                    c.LLVMBuildSDiv(self.builder, current_element, rhs_val, "sdiv");
                            },
                            else => return errors.CodegenError.UnsupportedOperation,
                        };
                    } else return errors.CodegenError.UnsupportedOperation;
                    const updated_simd = c.LLVMBuildInsertElement(self.builder, current_simd, new_element, index_value, "simd_update");
                    _ = c.LLVMBuildStore(self.builder, updated_simd, var_info.value);
                    return;
                }

                if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
                    if (collected_indices.items.len == 0) {
                        return errors.CodegenError.TypeMismatch;
                    }

                    const ptr_val = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_ptr_for_compound");
                    const element_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                    const element_type = self.getLLVMType(element_type_name);
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

                var final_type = var_info.type_ref;
                for (0..collected_indices.items.len) |_| final_type = c.LLVMGetElementType(final_type);
                var all_indices = std.ArrayList(c.LLVMValueRef){};
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
                const simd_name = try self.getBaseIdentifierName(simd_cass.simd);
                defer self.allocator.free(simd_name);
                const var_info = CodeGenerator.getVariable(self, simd_name) orelse return errors.CodegenError.UndefinedVariable;

                // Load the current SIMD vector
                const current_simd = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_simd");

                // Get the index and cast to i32
                var index_value = try self.generateExpression(simd_cass.index);
                index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));

                // Extract the current element
                const current_element = c.LLVMBuildExtractElement(self.builder, current_simd, index_value, "extract_elem");

                // Generate the RHS value
                const element_type = c.LLVMGetElementType(var_info.type_ref);
                const expected_ty_name = self.getTypeNameFromLLVMType(element_type);
                const rhs_raw = try self.generateExpressionWithContext(simd_cass.value, expected_ty_name);
                const rhs_val = try self.castWithRules(rhs_raw, element_type, simd_cass.value);

                // Perform the compound operation using scalar operations
                const element_kind = c.LLVMGetTypeKind(element_type);
                const new_element = if (element_kind == c.LLVMFloatTypeKind or element_kind == c.LLVMDoubleTypeKind or element_kind == c.LLVMHalfTypeKind) blk: {
                    break :blk switch (simd_cass.op) {
                        '+' => c.LLVMBuildFAdd(self.builder, current_element, rhs_val, "fadd"),
                        '-' => c.LLVMBuildFSub(self.builder, current_element, rhs_val, "fsub"),
                        '*' => c.LLVMBuildFMul(self.builder, current_element, rhs_val, "fmul"),
                        '/' => c.LLVMBuildFDiv(self.builder, current_element, rhs_val, "fdiv"),
                        else => return errors.CodegenError.UnsupportedOperation,
                    };
                } else if (element_kind == c.LLVMIntegerTypeKind) blk: {
                    break :blk switch (simd_cass.op) {
                        '+' => c.LLVMBuildAdd(self.builder, current_element, rhs_val, "add"),
                        '-' => c.LLVMBuildSub(self.builder, current_element, rhs_val, "sub"),
                        '*' => c.LLVMBuildMul(self.builder, current_element, rhs_val, "mul"),
                        '/' => blk2: {
                            const is_unsigned = isUnsignedType(self.getTypeNameFromLLVMType(element_type));
                            break :blk2 if (is_unsigned)
                                c.LLVMBuildUDiv(self.builder, current_element, rhs_val, "udiv")
                            else
                                c.LLVMBuildSDiv(self.builder, current_element, rhs_val, "sdiv");
                        },
                        else => return errors.CodegenError.UnsupportedOperation,
                    };
                } else return errors.CodegenError.UnsupportedOperation;

                // Insert the new element back into the vector
                const updated_simd = c.LLVMBuildInsertElement(self.builder, current_simd, new_element, index_value, "simd_update");

                // Store the updated vector
                _ = c.LLVMBuildStore(self.builder, updated_simd, var_info.value);
            },
            .c_function_decl => |c_func| {
                try CodeGenerator.generateCFunctionDeclaration(self, c_func);
            },
            .enum_decl => |enum_decl| {
                try self.generateEnumDeclaration(enum_decl);
            },
            .struct_decl => {
                // Struct declarations are only valid at top level, not in statements
                // This case should not be reached for valid code
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
        try CodeGenerator.pushScope(self);
        for (if_stmt.then_body.items) |stmt| {
            try self.generateStatement(stmt);
        }
        CodeGenerator.popScope(self);
        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
            _ = c.LLVMBuildBr(self.builder, merge_bb);
        }

        if (else_bb) |else_block| {
            c.LLVMPositionBuilderAtEnd(self.builder, else_block);
            if (if_stmt.else_body) |else_body| {
                try CodeGenerator.pushScope(self);
                for (else_body.items) |stmt| {
                    try self.generateStatement(stmt);
                }
                CodeGenerator.popScope(self);
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
            const loop_context = structs.LoopContext{
                .break_block = exit_bb,
                .continue_block = condition_bb,
            };
            try self.loop_context_stack.append(self.allocator, loop_context);
            _ = c.LLVMBuildBr(self.builder, condition_bb);
            c.LLVMPositionBuilderAtEnd(self.builder, condition_bb);
            const cond_value = try self.generateExpression(cond);
            const cond_bool = self.convertToBool(cond_value);
            _ = c.LLVMBuildCondBr(self.builder, cond_bool, body_bb, exit_bb);
            c.LLVMPositionBuilderAtEnd(self.builder, body_bb);
            try CodeGenerator.pushScope(self);
            for (for_stmt.body.items) |stmt| {
                try self.generateStatement(stmt);
            }
            CodeGenerator.popScope(self);
            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                _ = c.LLVMBuildBr(self.builder, condition_bb);
            }
            c.LLVMPositionBuilderAtEnd(self.builder, exit_bb);
            _ = self.loop_context_stack.pop();
        } else {
            const body_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "for_body");
            const exit_bb = c.LLVMAppendBasicBlockInContext(self.context, current_function, "for_exit");
            const loop_context = structs.LoopContext{
                .break_block = exit_bb,
                .continue_block = body_bb,
            };
            try self.loop_context_stack.append(self.allocator, loop_context);
            _ = c.LLVMBuildBr(self.builder, body_bb);
            c.LLVMPositionBuilderAtEnd(self.builder, body_bb);
            try CodeGenerator.pushScope(self);
            for (for_stmt.body.items) |stmt| {
                try self.generateStatement(stmt);
            }
            CodeGenerator.popScope(self);

            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                _ = c.LLVMBuildBr(self.builder, body_bb);
            }

            c.LLVMPositionBuilderAtEnd(self.builder, exit_bb);
            _ = self.loop_context_stack.pop();
        }
    }

    fn generateCForStatement(self: *CodeGenerator, c_for_stmt: ast.CForStmt) errors.CodegenError!void {
        const current_function = self.current_function orelse return errors.CodegenError.TypeMismatch;
        try CodeGenerator.pushScope(self);
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
        };
        try self.loop_context_stack.append(self.allocator, loop_context);
        c.LLVMPositionBuilderAtEnd(self.builder, body_bb);
        for (c_for_stmt.body.items) |stmt| {
            try self.generateStatement(stmt);
        }
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
        CodeGenerator.popScope(self);
    }

    fn generateArrayAssignment(self: *CodeGenerator, arr_ass: ast.ArrayAssignment) errors.CodegenError!void {
        var collected_indices = std.ArrayList(c.LLVMValueRef){};
        defer collected_indices.deinit(self.allocator);

        const base_node = try self.collectArrayIndices(arr_ass.array, &collected_indices);
        var index_value = try self.generateExpression(arr_ass.index);
        index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
        try collected_indices.append(self.allocator, index_value);

        const array_name = try self.getBaseIdentifierName(base_node);
        defer self.allocator.free(array_name);
        const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;

        if (var_info.is_const) {
            std.debug.print("Error: Cannot modify element of const array '{s}'\n", .{array_name});
            return errors.CodegenError.ConstReassignment;
        }

        if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
            const ptr_val = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_ptr_for_assign");
            const element_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
            const element_type = self.getLLVMType(element_type_name);
            const idx = collected_indices.items[collected_indices.items.len - 1];
            var indices = [_]c.LLVMValueRef{idx};
            const element_ptr = c.LLVMBuildGEP2(self.builder, element_type, ptr_val, &indices[0], 1, "ptr_index_assign");
            const expected_ty_name = self.getTypeNameFromLLVMType(element_type);
            const value_raw = try self.generateExpressionWithContext(arr_ass.value, expected_ty_name);
            const final_val = try self.castWithRules(value_raw, element_type, arr_ass.value);
            _ = c.LLVMBuildStore(self.builder, final_val, element_ptr);
            return;
        }

        var final_type = var_info.type_ref;
        for (0..collected_indices.items.len) |_| {
            final_type = c.LLVMGetElementType(@ptrCast(final_type));
        }

        var all_indices = std.ArrayList(c.LLVMValueRef){};
        defer all_indices.deinit(self.allocator);
        try all_indices.append(self.allocator, c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0));

        var i = collected_indices.items.len;
        while (i > 0) {
            i -= 1;
            try all_indices.append(self.allocator, collected_indices.items[i]);
        }

        const element_ptr = c.LLVMBuildGEP2(self.builder, var_info.type_ref, var_info.value, all_indices.items.ptr, @intCast(all_indices.items.len), "array_element_ptr");

        const expected_ty_name = self.getTypeNameFromLLVMType(final_type);
        const value_raw = try self.generateExpressionWithContext(arr_ass.value, expected_ty_name);
        const final_val = try self.castWithRules(value_raw, final_type, arr_ass.value);
        _ = c.LLVMBuildStore(self.builder, final_val, element_ptr);
    }

    fn generateArrayReassignment(self: *CodeGenerator, array_name: []const u8, value_expr: *ast.Node) errors.CodegenError!void {
        const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;

        switch (value_expr.data) {
            .array_initializer => |init_list| {
                for (init_list.elements.items, 0..) |element, idx| {
                    const element_type = c.LLVMGetElementType(@ptrCast(var_info.type_ref));
                    const expected_ty_name = self.getTypeNameFromLLVMType(@ptrCast(element_type));
                    const element_value_raw = try self.generateExpressionWithContext(element, expected_ty_name);

                    var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(idx)), 0) };
                    const element_ptr = c.LLVMBuildGEP2(@ptrCast(self.builder), @ptrCast(var_info.type_ref), @ptrCast(var_info.value), &indices[0], 2, "array_element_ptr");

                    const casted_value = try self.castWithRules(element_value_raw, element_type, element);
                    _ = c.LLVMBuildStore(self.builder, casted_value, element_ptr);
                }
            },
            .string_literal => |str_lit| {
                const element_type = c.LLVMGetElementType(var_info.type_ref);
                const element_type_kind = c.LLVMGetTypeKind(element_type);
                if (element_type_kind != c.LLVMIntegerTypeKind or c.LLVMGetIntTypeWidth(element_type) != 8) {
                    return errors.CodegenError.TypeMismatch;
                }
                const parsed_str = try self.parse_escape(str_lit.value);
                defer self.allocator.free(parsed_str);
                const str_len = parsed_str.len;
                const array_type = var_info.type_ref;
                const array_length = c.LLVMGetArrayLength(array_type);
                if (str_len > array_length) {
                    return errors.CodegenError.TypeMismatch;
                }
                const memcpy_func = try CodeGenerator.declareLibcFunction(self, "memcpy");
                const i8_type = c.LLVMInt8TypeInContext(self.context);
                const i8_ptr_type = c.LLVMPointerType(i8_type, 0);
                const size_t_type = self.libcTypeToLLVM(.size_t_type);
                const array_i8_ptr = c.LLVMBuildBitCast(self.builder, var_info.value, i8_ptr_type, "array_i8_ptr");
                const global_str = c.LLVMBuildGlobalStringPtr(self.builder, parsed_str.ptr, "temp_str_ptr");
                const copy_size = c.LLVMConstInt(size_t_type, @as(c_ulonglong, @intCast(str_len)), 0);
                var memcpy_args = [_]c.LLVMValueRef{ array_i8_ptr, global_str, copy_size };
                _ = c.LLVMBuildCall2(self.builder, c.LLVMGlobalGetValueType(memcpy_func), memcpy_func, &memcpy_args[0], 3, "");
            },
            else => {
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
            var array_size: usize = 0;
            if (std.mem.eql(u8, size_str, "_")) {
                if (decl.initializer) |initializer| {
                    switch (initializer.data) {
                        .array_initializer => |init_list| {
                            array_size = init_list.elements.items.len;
                        },
                        .string_literal => |str_lit| {
                            const parsed_str = try self.parse_escape(str_lit.value);
                            defer self.allocator.free(parsed_str);
                            array_size = parsed_str.len;
                        },
                        else => {
                            return errors.CodegenError.TypeMismatch;
                        },
                    }
                } else {
                    return errors.CodegenError.TypeMismatch;
                }
            } else {
                array_size = std.fmt.parseInt(usize, size_str, 10) catch {
                    return errors.CodegenError.TypeMismatch;
                };
            }

            const element_type = self.getLLVMType(element_type_name);
            const array_type = c.LLVMArrayType(@ptrCast(element_type), @intCast(array_size));
            const alloca = c.LLVMBuildAlloca(self.builder, array_type, decl.name.ptr);

            try CodeGenerator.putVariable(self, decl.name, structs.VariableInfo{
                .value = @ptrCast(alloca),
                .type_ref = @ptrCast(array_type),
                .type_name = decl.type_name,
                .is_const = decl.is_const,
            });
            if (decl.initializer) |initializer| {
                switch (initializer.data) {
                    .array_initializer => |init_list| {
                        for (init_list.elements.items, 0..) |element, idx| {
                            const expected_ty_name = self.getTypeNameFromLLVMType(element_type);
                            const element_value_raw = try self.generateExpressionWithContext(element, expected_ty_name);
                            var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(idx)), 0) };
                            const element_ptr = c.LLVMBuildGEP2(self.builder, array_type, alloca, &indices[0], 2, "array_element_ptr");
                            const casted_value = try self.castWithRules(element_value_raw, @ptrCast(element_type), element);
                            _ = c.LLVMBuildStore(self.builder, casted_value, element_ptr);
                        }
                    },
                    .string_literal => |str_lit| {
                        const element_type_kind = c.LLVMGetTypeKind(element_type);
                        if (element_type_kind != c.LLVMIntegerTypeKind or c.LLVMGetIntTypeWidth(element_type) != 8) {
                            return errors.CodegenError.TypeMismatch;
                        }
                        const parsed_str = try self.parse_escape(str_lit.value);
                        defer self.allocator.free(parsed_str);
                        for (parsed_str, 0..) |byte, idx| {
                            if (idx >= array_size) break;
                            var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(idx)), 0) };
                            const element_ptr = c.LLVMBuildGEP2(self.builder, array_type, alloca, &indices[0], 2, "array_element_ptr");
                            const byte_value = c.LLVMConstInt(c.LLVMInt8TypeInContext(self.context), @as(c_ulonglong, @intCast(byte)), 0);
                            _ = c.LLVMBuildStore(self.builder, byte_value, element_ptr);
                        }
                    },
                    else => {
                        return errors.CodegenError.TypeMismatch;
                    },
                }
            }
        } else {
            return errors.CodegenError.TypeMismatch;
        }
    }

    fn generateSimdDeclaration(self: *CodeGenerator, decl: ast.VarDecl) errors.CodegenError!void {
        const inner = decl.type_name[5 .. decl.type_name.len - 1];
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
            const vector_size = std.fmt.parseInt(u32, size_str, 10) catch {
                return errors.CodegenError.TypeMismatch;
            };
            const element_type = self.getLLVMType(element_type_name);
            const vector_type = c.LLVMVectorType(@ptrCast(element_type), vector_size);
            const alloca = c.LLVMBuildAlloca(self.builder, vector_type, decl.name.ptr);
            try CodeGenerator.putVariable(self, decl.name, structs.VariableInfo{
                .value = @ptrCast(alloca),
                .type_ref = @ptrCast(vector_type),
                .type_name = decl.type_name,
                .is_const = decl.is_const,
            });
            if (decl.initializer) |initializer| {
                switch (initializer.data) {
                    .array_initializer => |init_list| {
                        if (init_list.elements.items.len == 0) {
                            return errors.CodegenError.TypeMismatch;
                        }
                        var result = c.LLVMGetUndef(vector_type);
                        for (init_list.elements.items, 0..) |element, i| {
                            const element_value = try self.generateExpression(element);
                            const casted_value = self.castToType(element_value, element_type);
                            const index = c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(i)), 0);
                            result = c.LLVMBuildInsertElement(self.builder, result, casted_value, index, "simd_init");
                        }
                        _ = c.LLVMBuildStore(self.builder, result, alloca);
                    },
                    .binary_op => {
                        const expr_value = try self.generateExpression(initializer);
                        _ = c.LLVMBuildStore(self.builder, expr_value, alloca);
                    },
                    .function_call => {
                        const expr_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                        const casted_value = try self.castWithRules(expr_value, vector_type, initializer);
                        _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                    },
                    .identifier => {
                        const expr_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                        const casted_value = try self.castWithRules(expr_value, vector_type, initializer);
                        _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                    },
                    .method_call => {
                        const expr_value = try self.generateExpressionWithContext(initializer, decl.type_name);
                        const casted_value = try self.castWithRules(expr_value, vector_type, initializer);
                        _ = c.LLVMBuildStore(self.builder, casted_value, alloca);
                    },
                    else => {
                        return errors.CodegenError.TypeMismatch;
                    },
                }
            }
        } else {
            return errors.CodegenError.TypeMismatch;
        }
    }

    fn generateSimdBinaryOp(self: *CodeGenerator, op: u8, lhs: c.LLVMValueRef, rhs: c.LLVMValueRef) errors.CodegenError!c.LLVMValueRef {
        const lhs_type = c.LLVMTypeOf(lhs);
        const element_type = c.LLVMGetElementType(lhs_type);
        const type_kind = c.LLVMGetTypeKind(element_type);
        return switch (op) {
            '+' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                c.LLVMBuildFAdd(self.builder, lhs, rhs, "simd_fadd")
            else
                c.LLVMBuildAdd(self.builder, lhs, rhs, "simd_add"),
            '-' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                c.LLVMBuildFSub(self.builder, lhs, rhs, "simd_fsub")
            else
                c.LLVMBuildSub(self.builder, lhs, rhs, "simd_sub"),
            '*' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                c.LLVMBuildFMul(self.builder, lhs, rhs, "simd_fmul")
            else
                c.LLVMBuildMul(self.builder, lhs, rhs, "simd_mul"),
            '/' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                c.LLVMBuildFDiv(self.builder, lhs, rhs, "simd_fdiv")
            else
                c.LLVMBuildSDiv(self.builder, lhs, rhs, "simd_sdiv"),
            'A' => c.LLVMBuildAnd(self.builder, lhs, rhs, "simd_and"),
            '$' => c.LLVMBuildOr(self.builder, lhs, rhs, "simd_or"),
            '^' => c.LLVMBuildXor(self.builder, lhs, rhs, "simd_xor"),
            '=' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                c.LLVMBuildFCmp(self.builder, c.LLVMRealOEQ, lhs, rhs, "simd_fcmp_eq")
            else
                c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, lhs, rhs, "simd_icmp_eq"),
            '!' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                c.LLVMBuildFCmp(self.builder, c.LLVMRealONE, lhs, rhs, "simd_fcmp_ne")
            else
                c.LLVMBuildICmp(self.builder, c.LLVMIntNE, lhs, rhs, "simd_icmp_ne"),
            'L' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, lhs, rhs, "simd_fcmp_lt")
            else
                c.LLVMBuildICmp(self.builder, c.LLVMIntSLT, lhs, rhs, "simd_icmp_slt"),
            'G' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, lhs, rhs, "simd_fcmp_gt")
            else
                c.LLVMBuildICmp(self.builder, c.LLVMIntSGT, lhs, rhs, "simd_icmp_sgt"),
            'l' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                c.LLVMBuildFCmp(self.builder, c.LLVMRealOLE, lhs, rhs, "simd_fcmp_le")
            else
                c.LLVMBuildICmp(self.builder, c.LLVMIntSLE, lhs, rhs, "simd_icmp_sle"),
            'g' => if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                c.LLVMBuildFCmp(self.builder, c.LLVMRealOGE, lhs, rhs, "simd_fcmp_ge")
            else
                c.LLVMBuildICmp(self.builder, c.LLVMIntSGE, lhs, rhs, "simd_icmp_sge"),
            else => errors.CodegenError.UnsupportedOperation,
        };
    }

    fn generateSimdReduction(self: *CodeGenerator, operation: []const u8, simd_value: c.LLVMValueRef) errors.CodegenError!c.LLVMValueRef {
        const simd_type = c.LLVMTypeOf(simd_value);
        const element_type = c.LLVMGetElementType(simd_type);
        const type_kind = c.LLVMGetTypeKind(element_type);
        const vector_size = c.LLVMGetVectorSize(simd_type);
        var result = c.LLVMBuildExtractElement(self.builder, simd_value, c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), "extract_0");
        var i: u32 = 1;
        while (i < vector_size) : (i += 1) {
            const index = c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(i)), 0);
            const element = c.LLVMBuildExtractElement(self.builder, simd_value, index, "extract_elem");
            if (std.mem.eql(u8, operation, "sum")) {
                result = if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                    c.LLVMBuildFAdd(self.builder, result, element, "reduce_fadd")
                else
                    c.LLVMBuildAdd(self.builder, result, element, "reduce_add");
            } else if (std.mem.eql(u8, operation, "product")) {
                result = if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind)
                    c.LLVMBuildFMul(self.builder, result, element, "reduce_fmul")
                else
                    c.LLVMBuildMul(self.builder, result, element, "reduce_mul");
            } else if (std.mem.eql(u8, operation, "min")) {
                if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind) {
                    const cmp = c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, result, element, "min_cmp");
                    result = c.LLVMBuildSelect(self.builder, cmp, result, element, "reduce_fmin");
                } else {
                    const cmp = c.LLVMBuildICmp(self.builder, c.LLVMIntSLT, result, element, "min_cmp");
                    result = c.LLVMBuildSelect(self.builder, cmp, result, element, "reduce_min");
                }
            } else if (std.mem.eql(u8, operation, "max")) {
                if (type_kind == c.LLVMFloatTypeKind or type_kind == c.LLVMDoubleTypeKind or type_kind == c.LLVMHalfTypeKind) {
                    const cmp = c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, result, element, "max_cmp");
                    result = c.LLVMBuildSelect(self.builder, cmp, result, element, "reduce_fmax");
                } else {
                    const cmp = c.LLVMBuildICmp(self.builder, c.LLVMIntSGT, result, element, "max_cmp");
                    result = c.LLVMBuildSelect(self.builder, cmp, result, element, "reduce_max");
                }
            } else {
                return errors.CodegenError.UnsupportedOperation;
            }
        }
        return result;
    }

    fn generateContinueStatement(self: *CodeGenerator) errors.CodegenError!void {
        if (self.loop_context_stack.items.len == 0) {
            return errors.CodegenError.TypeMismatch;
        }
        const loop_context = self.loop_context_stack.items[self.loop_context_stack.items.len - 1];
        const continue_block = loop_context.continue_block;
        _ = c.LLVMBuildBr(self.builder, continue_block);
    }

    fn generateBreakStatement(self: *CodeGenerator) errors.CodegenError!void {
        if (self.loop_context_stack.items.len == 0) {
            return errors.CodegenError.TypeMismatch;
        }
        const loop_context = self.loop_context_stack.items[self.loop_context_stack.items.len - 1];
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

    fn processGlobalEnums(self: *CodeGenerator, func_node: *ast.Node) errors.CodegenError!void {
        switch (func_node.data) {
            .function => |func| {
                for (func.body.items) |stmt| {
                    try self.findAndProcessEnums(stmt);
                }
            },
            else => {},
        }
    }

    fn findAndProcessEnums(self: *CodeGenerator, node: *ast.Node) errors.CodegenError!void {
        switch (node.data) {
            .enum_decl => |enum_decl| {
                try self.generateEnumDeclaration(enum_decl);
            },
            .if_stmt => |if_stmt| {
                for (if_stmt.then_body.items) |stmt| {
                    try self.findAndProcessEnums(stmt);
                }
                if (if_stmt.else_body) |else_body| {
                    for (else_body.items) |stmt| {
                        try self.findAndProcessEnums(stmt);
                    }
                }
            },
            .for_stmt => |for_stmt| {
                for (for_stmt.body.items) |stmt| {
                    try self.findAndProcessEnums(stmt);
                }
            },
            .c_for_stmt => |c_for_stmt| {
                for (c_for_stmt.body.items) |stmt| {
                    try self.findAndProcessEnums(stmt);
                }
            },
            else => {},
        }
    }

    fn generateGlobalDeclaration(self: *CodeGenerator, global_node: *ast.Node) errors.CodegenError!void {
        switch (global_node.data) {
            .var_decl => |decl| {
                const var_type = self.getLLVMType(decl.type_name);
                const var_name_z = utils.dupeZ(self.allocator, decl.name);
                defer self.allocator.free(var_name_z);
                const global_var = c.LLVMAddGlobal(self.module, @ptrCast(var_type), var_name_z.ptr);

                if (decl.initializer) |initializer| {
                    const init_value = try self.generateExpression(initializer);
                    const casted_init_value = try self.castWithSourceRules(init_value, @ptrCast(var_type), initializer);
                    c.LLVMSetInitializer(global_var, casted_init_value);
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

    fn generateEnumDeclaration(self: *CodeGenerator, enum_decl: ast.EnumDecl) errors.CodegenError!void {
        var current_value: i32 = 0;
        for (enum_decl.values.items) |enum_value| {
            var value: i32 = current_value;
            if (enum_value.value) |expr| {
                const const_value = try self.generateExpression(expr);
                if (c.LLVMIsConstant(const_value) == 0) {
                    return errors.CodegenError.TypeMismatch;
                }
                const int_value = c.LLVMConstIntGetSExtValue(const_value);
                value = @intCast(int_value);
            }
            const enum_const = c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(value)), 0);
            const enum_name_z = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ enum_decl.name, enum_value.name });
            defer self.allocator.free(enum_name_z);
            const enum_name_z_null = utils.dupeZ(self.allocator, enum_name_z);
            defer self.allocator.free(enum_name_z_null);
            const global_var = c.LLVMAddGlobal(self.module, c.LLVMInt32TypeInContext(self.context), enum_name_z_null.ptr);
            c.LLVMSetInitializer(global_var, enum_const);
            c.LLVMSetLinkage(global_var, c.LLVMExternalLinkage);
            c.LLVMSetGlobalConstant(global_var, 1);

            const var_info = structs.VariableInfo{
                .value = @ptrCast(global_var),
                .type_ref = @ptrCast(c.LLVMInt32TypeInContext(@ptrCast(self.context))),
                .type_name = "i32",
                .is_const = true,
            };
            try self.variables.put(utils.dupe(u8, self.allocator, enum_name_z), var_info);
            try self.variables.put(utils.dupe(u8, self.allocator, enum_value.name), var_info);

            current_value = value + 1;
        }
    }

    pub fn castToType(self: *CodeGenerator, value: c.LLVMValueRef, target_type: c.LLVMTypeRef) c.LLVMValueRef {
        return self.castToTypeWithSourceInfo(value, target_type, null);
    }

    pub fn castToTypeWithSourceInfo(self: *CodeGenerator, value: c.LLVMValueRef, target_type: c.LLVMTypeRef, source_type_name: ?[]const u8) c.LLVMValueRef {
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
                    if (source_type_name) |src_name| {
                        if (isUnsignedType(src_name)) {
                            return c.LLVMBuildZExt(self.builder, value, target_type, "zext");
                        } else {
                            return c.LLVMBuildSExt(self.builder, value, target_type, "sext");
                        }
                    } else {
                        const target_type_name = self.getTypeNameFromLLVMType(@ptrCast(target_type));
                        if (isUnsignedType(target_type_name)) {
                            return c.LLVMBuildZExt(self.builder, value, target_type, "zext");
                        } else {
                            return c.LLVMBuildSExt(self.builder, value, target_type, "sext");
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
        } else if (value_kind == c.LLVMPointerTypeKind and target_kind == c.LLVMStructTypeKind) {
            const pointee_type = c.LLVMGetElementType(value_type);
            if (pointee_type == target_type) {
                return c.LLVMBuildLoad2(self.builder, target_type, value, "struct_load");
            }
        } else if (value_kind == c.LLVMPointerTypeKind and target_kind == c.LLVMPointerTypeKind) {
            return c.LLVMBuildBitCast(self.builder, value, target_type, "bitcast");
        }

        return value;
    }

    pub fn castWithSourceRules(self: *CodeGenerator, value: c.LLVMValueRef, target_type: c.LLVMTypeRef, value_node: ?*ast.Node) errors.CodegenError!c.LLVMValueRef {
        var val = value;
        var from_ty = c.LLVMTypeOf(val);
        var source_type_name: ?[]const u8 = null;
        if (value_node) |vn| {
            if (vn.data == .identifier) {
                const var_name = vn.data.identifier;
                if (CodeGenerator.getVariable(self, var_name.name)) |var_info| {
                    source_type_name = var_info.type_name;
                }
            } else if (vn.data == .cast) {
                const cst = vn.data.cast;
                if (cst.auto) {
                    return self.castToTypeWithSourceInfo(val, target_type, source_type_name);
                } else if (cst.type_name) |tn| {
                    const named_ty = self.getLLVMType(tn);
                    val = self.castToTypeWithSourceInfo(val, @ptrCast(named_ty), source_type_name);
                    from_ty = @ptrCast(named_ty);
                }
            }
        }
        if (self.typesAreEqual(from_ty, target_type)) return val;
        const fk = c.LLVMGetTypeKind(from_ty);
        const tk = c.LLVMGetTypeKind(target_type);
        if (fk == c.LLVMIntegerTypeKind and tk == c.LLVMIntegerTypeKind) {
            const fw = c.LLVMGetIntTypeWidth(from_ty);
            const tw = c.LLVMGetIntTypeWidth(target_type);
            if (tw >= fw) {
                return self.castToTypeWithSourceInfo(val, target_type, source_type_name);
            }
        } else {
            const f_float = fk == c.LLVMFloatTypeKind or fk == c.LLVMDoubleTypeKind or fk == c.LLVMHalfTypeKind;
            const t_float = tk == c.LLVMFloatTypeKind or tk == c.LLVMDoubleTypeKind or tk == c.LLVMHalfTypeKind;
            if (f_float and t_float and floatBitWidth(target_type) >= floatBitWidth(from_ty)) {
                return self.castToTypeWithSourceInfo(val, target_type, source_type_name);
            }
        }
        if (fk == c.LLVMPointerTypeKind and tk == c.LLVMPointerTypeKind) {
            return self.castToTypeWithSourceInfo(val, target_type, source_type_name);
        }
        const to_kind = c.LLVMGetTypeKind(target_type);
        if (value_node) |vn2| {
            switch (vn2.data) {
                .number_literal => {
                    if (to_kind == c.LLVMIntegerTypeKind) {
                        return self.castToTypeWithSourceInfo(val, target_type, source_type_name);
                    }
                },
                .char_literal => {
                    if (to_kind == c.LLVMIntegerTypeKind) {
                        return self.castToTypeWithSourceInfo(val, target_type, source_type_name);
                    }
                },
                .float_literal => {
                    if (to_kind == c.LLVMFloatTypeKind or to_kind == c.LLVMDoubleTypeKind or to_kind == c.LLVMHalfTypeKind) {
                        return self.castToTypeWithSourceInfo(val, target_type, source_type_name);
                    }
                },
                .unary_op => |un| {
                    if (un.op == '-' or un.op == '+') {
                        const inner = un.operand;
                        switch (inner.data) {
                            .number_literal => {
                                if (to_kind == c.LLVMIntegerTypeKind) {
                                    return self.castToTypeWithSourceInfo(val, target_type, source_type_name);
                                }
                            },
                            .float_literal => {
                                if (to_kind == c.LLVMFloatTypeKind or to_kind == c.LLVMDoubleTypeKind or to_kind == c.LLVMHalfTypeKind) {
                                    return self.castToTypeWithSourceInfo(val, target_type, source_type_name);
                                }
                            },
                            else => {},
                        }
                    }
                },
                else => {},
            }
        }

        return val;
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

    fn typesAreEqual(self: *CodeGenerator, type1: c.LLVMTypeRef, type2: c.LLVMTypeRef) bool {
        _ = self;
        if (type1 == type2) return true;

        const kind1 = c.LLVMGetTypeKind(type1);
        const kind2 = c.LLVMGetTypeKind(type2);

        if (kind1 != kind2) return false;

        switch (kind1) {
            c.LLVMIntegerTypeKind => {
                return c.LLVMGetIntTypeWidth(type1) == c.LLVMGetIntTypeWidth(type2);
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

    pub fn castWithRules(self: *CodeGenerator, value: c.LLVMValueRef, target_type: c.LLVMTypeRef, value_node: ?*ast.Node) errors.CodegenError!c.LLVMValueRef {
        var val = value;
        var from_ty = c.LLVMTypeOf(val);
        if (value_node) |vn| {
            if (vn.data == .cast) {
                const cst = vn.data.cast;
                if (cst.auto) {
                    return self.castToType(val, target_type);
                } else if (cst.type_name) |tn| {
                    const named_ty = self.getLLVMType(tn);
                    val = self.castToType(val, @ptrCast(named_ty));
                    from_ty = @ptrCast(named_ty);
                }
            }
        }
        if (self.typesAreEqual(from_ty, target_type)) return val;

        const fk = c.LLVMGetTypeKind(from_ty);
        const tk = c.LLVMGetTypeKind(target_type);
        if (fk == c.LLVMIntegerTypeKind and tk == c.LLVMIntegerTypeKind) {
            const fw = c.LLVMGetIntTypeWidth(from_ty);
            const tw = c.LLVMGetIntTypeWidth(target_type);
            if (tw >= fw) {
                return self.castToType(val, target_type);
            }
        } else {
            const f_float = fk == c.LLVMFloatTypeKind or fk == c.LLVMDoubleTypeKind or fk == c.LLVMHalfTypeKind;
            const t_float = tk == c.LLVMFloatTypeKind or tk == c.LLVMDoubleTypeKind or tk == c.LLVMHalfTypeKind;
            if (f_float and t_float and floatBitWidth(target_type) >= floatBitWidth(from_ty)) {
                return self.castToType(val, target_type);
            }
        }

        // Allow pointer-to-pointer conversions (including null/void* to typed pointer)
        if (fk == c.LLVMPointerTypeKind and tk == c.LLVMPointerTypeKind) {
            return self.castToType(val, target_type);
        }
        if (fk == c.LLVMVectorTypeKind and tk == c.LLVMVectorTypeKind) {
            const from_element_type = c.LLVMGetElementType(from_ty);
            const to_element_type = c.LLVMGetElementType(target_type);
            const from_size = c.LLVMGetVectorSize(from_ty);
            const to_size = c.LLVMGetVectorSize(target_type);
            if (self.typesAreEqual(from_element_type, to_element_type) and from_size == to_size) {
                return self.castToType(val, target_type);
            }
        }

        const to_kind = c.LLVMGetTypeKind(target_type);
        if (value_node) |vn2| {
            switch (vn2.data) {
                .number_literal => {
                    if (to_kind == c.LLVMIntegerTypeKind) {
                        return self.castToType(val, target_type);
                    }
                },
                .char_literal => {
                    if (to_kind == c.LLVMIntegerTypeKind) {
                        return self.castToType(val, target_type);
                    }
                },
                .float_literal => {
                    if (to_kind == c.LLVMFloatTypeKind or to_kind == c.LLVMDoubleTypeKind or to_kind == c.LLVMHalfTypeKind) {
                        return self.castToType(val, target_type);
                    }
                },
                .unary_op => |un| {
                    if (un.op == '-' or un.op == '+') {
                        const inner = un.operand;
                        switch (inner.data) {
                            .number_literal => {
                                if (to_kind == c.LLVMIntegerTypeKind) {
                                    return self.castToType(val, target_type);
                                }
                            },
                            .float_literal => {
                                if (to_kind == c.LLVMFloatTypeKind or to_kind == c.LLVMDoubleTypeKind or to_kind == c.LLVMHalfTypeKind) {
                                    return self.castToType(val, target_type);
                                }
                            },
                            else => {},
                        }
                    }
                },
                else => {},
            }
        }
        const from_name = self.getTypeNameFromLLVMType(@ptrCast(from_ty));
        const to_name = self.getTypeNameFromLLVMType(@ptrCast(target_type));

        if (value_node) |vn3| {
            std.debug.print("Type mismatch at line {d}: cannot convert {s} to {s}\n", .{ vn3.line, from_name, to_name });
        } else {
            std.debug.print("Type mismatch: cannot convert {s} to {s}\n", .{ from_name, to_name });
        }
        return errors.CodegenError.TypeMismatch;
    }

    pub fn parse_escape(self: *CodeGenerator, str: []const u8) errors.CodegenError![]const u8 {
        var transformed_string = std.ArrayList(u8){};
        defer transformed_string.deinit(self.allocator);
        var i: usize = 0;
        while (i < str.len) : (i += 1) {
            if (str[i] == '\\') {
                i += 1;
                if (i < str.len) {
                    switch (str[i]) {
                        'n' => transformed_string.append(self.allocator, '\n') catch return errors.CodegenError.OutOfMemory,
                        't' => transformed_string.append(self.allocator, '\t') catch return errors.CodegenError.OutOfMemory,
                        'r' => transformed_string.append(self.allocator, '\r') catch return errors.CodegenError.OutOfMemory,
                        '\'' => transformed_string.append(self.allocator, '\'') catch return errors.CodegenError.OutOfMemory,
                        '"' => transformed_string.append(self.allocator, '"') catch return errors.CodegenError.OutOfMemory,
                        '0' => transformed_string.append(self.allocator, 0) catch return errors.CodegenError.OutOfMemory,
                        '\\' => transformed_string.append(self.allocator, '\\') catch return errors.CodegenError.OutOfMemory,
                        else => {
                            transformed_string.append(self.allocator, '\\') catch return errors.CodegenError.OutOfMemory;
                            transformed_string.append(self.allocator, str[i]) catch return errors.CodegenError.OutOfMemory;
                        },
                    }
                } else {
                    transformed_string.append(self.allocator, '\\') catch return errors.CodegenError.OutOfMemory;
                }
            } else {
                transformed_string.append(self.allocator, str[i]) catch return errors.CodegenError.OutOfMemory;
            }
        }
        try transformed_string.append(self.allocator, 0);
        return transformed_string.toOwnedSlice(self.allocator);
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

    pub fn generateExpressionWithContext(self: *CodeGenerator, expr: *ast.Node, expected_type: ?[]const u8) errors.CodegenError!c.LLVMValueRef {
        switch (expr.data) {
            .cast => |cst| {
                if (cst.auto) {
                    if (expected_type) |type_name| {
                        const target_ty = self.getLLVMType(type_name);
                        const inner = try self.generateExpression(cst.expr);
                        var source_type_name: ?[]const u8 = null;
                        if (cst.expr.data == .identifier) {
                            const ident = cst.expr.data.identifier;
                            if (CodeGenerator.getVariable(self, ident.name)) |var_info| {
                                source_type_name = var_info.type_name;
                            }
                        } else if (cst.expr.data == .qualified_identifier) {
                            const qual_id = cst.expr.data.qualified_identifier;
                            if (qual_id.base.data == .identifier) {
                                const base_name = qual_id.base.data.identifier.name;
                                if (CodeGenerator.getVariable(self, base_name)) |base_var| {
                                    if (std.mem.startsWith(u8, base_var.type_name, "ptr<") and std.mem.endsWith(u8, base_var.type_name, ">")) {
                                        const inner_type_name = base_var.type_name[4 .. base_var.type_name.len - 1];
                                        const struct_decl = self.struct_declarations.get(inner_type_name) orelse return errors.CodegenError.TypeMismatch;
                                        const field_map = self.struct_fields.get(inner_type_name) orelse return errors.CodegenError.TypeMismatch;
                                        const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
                                        source_type_name = struct_decl.fields.items[field_index].type_name;
                                    } else {
                                        const struct_decl = self.struct_declarations.get(base_var.type_name) orelse return errors.CodegenError.TypeMismatch;
                                        const field_map = self.struct_fields.get(base_var.type_name) orelse return errors.CodegenError.TypeMismatch;
                                        const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
                                        source_type_name = struct_decl.fields.items[field_index].type_name;
                                    }
                                }
                            }
                        }
                        return @ptrCast(self.castToTypeWithSourceInfo(inner, @ptrCast(target_ty), source_type_name));
                    } else {
                        return errors.CodegenError.TypeMismatch;
                    }
                } else if (cst.type_name) |tn| {
                    const target_ty = self.getLLVMType(tn);
                    const inner = try self.generateExpression(cst.expr);
                    var source_type_name: ?[]const u8 = null;
                    if (cst.expr.data == .identifier) {
                        const ident = cst.expr.data.identifier;
                        if (CodeGenerator.getVariable(self, ident.name)) |var_info| {
                            source_type_name = var_info.type_name;
                        }
                    }
                    return self.castToTypeWithSourceInfo(inner, @ptrCast(target_ty), source_type_name);
                } else {
                    return errors.CodegenError.TypeMismatch;
                }
            },
            .float_literal => |float| {
                const float_val = numeric.parseFloatLiteral(float.value) catch 0.0;

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
                    const float_val = numeric.parseFloatLiteral(num.value) catch 0.0;

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
                        if (std.mem.eql(u8, type_name, "f16") or std.mem.eql(u8, type_name, "f32") or std.mem.eql(u8, type_name, "f64")) {
                            const ival = numeric.parseNumericLiteral(num.value) catch 0;
                            const fval: f64 = @floatFromInt(ival);
                            if (std.mem.eql(u8, type_name, "f16")) {
                                return c.LLVMConstReal(c.LLVMHalfTypeInContext(self.context), fval);
                            } else if (std.mem.eql(u8, type_name, "f32")) {
                                return c.LLVMConstReal(c.LLVMFloatTypeInContext(self.context), fval);
                            } else {
                                return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), fval);
                            }
                        }
                        const llvm_type = self.getLLVMType(type_name);
                        if (std.mem.startsWith(u8, type_name, "u")) {
                            if (std.mem.startsWith(u8, num.value, "-")) {
                                const sval = numeric.parseNumericLiteral(num.value) catch {
                                    return errors.CodegenError.TypeMismatch;
                                };
                                const uval: u64 = @bitCast(sval);
                                return c.LLVMConstInt(llvm_type, @as(c_ulonglong, @intCast(uval)), 0);
                            } else {
                                const value = numeric.parseNumericLiteralUnsigned(num.value) catch {
                                    return errors.CodegenError.TypeMismatch;
                                };
                                return c.LLVMConstInt(llvm_type, @as(c_ulonglong, @intCast(value)), 0);
                            }
                        } else {
                            const value = numeric.parseNumericLiteral(num.value) catch {
                                return errors.CodegenError.TypeMismatch;
                            };
                            return c.LLVMConstInt(llvm_type, @as(c_ulonglong, @intCast(value)), 0);
                        }
                    } else {
                        const value = numeric.parseNumericLiteral(num.value) catch 0;
                        return c.LLVMConstInt(c.LLVMInt64TypeInContext(self.context), @as(c_ulonglong, @intCast(value)), 0);
                    }
                }
            },
            .char_literal => |char| {
                return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(char.value)), 0);
            },
            .identifier => |ident| {
                if (std.mem.eql(u8, ident.name, "true")) {
                    return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 1, 0);
                } else if (std.mem.eql(u8, ident.name, "false")) {
                    return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 0, 0);
                }

                if (CodeGenerator.getVariable(self, ident.name)) |var_info| {
                    return c.LLVMBuildLoad2(self.builder, @ptrCast(var_info.type_ref), @ptrCast(var_info.value), "load");
                }
                var func_iter = c.LLVMGetFirstFunction(self.module);
                while (func_iter != null) : (func_iter = c.LLVMGetNextFunction(func_iter)) {
                    const func_name_ptr = c.LLVMGetValueName(func_iter);
                    if (func_name_ptr != null) {
                        const func_name_slice = std.mem.span(func_name_ptr);
                        if (std.mem.eql(u8, func_name_slice, ident.name)) {
                            return func_iter;
                        }
                    }
                }
                if (self.functions.get(ident.name)) |declared_func| {
                    return declared_func;
                }
                return errors.CodegenError.UndefinedVariable;
            },
            .null_literal => {
                if (expected_type) |type_name| {
                    if (std.mem.startsWith(u8, type_name, "ptr<")) {
                        const llvm_type = self.getLLVMType(type_name);
                        return c.LLVMConstNull(llvm_type);
                    } else {
                        return errors.CodegenError.NullNotAllowedInNonPointerType;
                    }
                } else {
                    const void_ptr_type = c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0);
                    return c.LLVMConstNull(void_ptr_type);
                }
            },
            .binary_op => |b| {
                if (expected_type) |type_name| {
                    const target_ty = self.getLLVMType(type_name);
                    const kind = c.LLVMGetTypeKind(target_ty);
                    if (kind == c.LLVMPointerTypeKind) {
                        return self.generateBinaryOp(b);
                    }

                    const lhs_val = try self.generateExpressionWithContext(b.lhs, expected_type);
                    const rhs_val = try self.generateExpressionWithContext(b.rhs, expected_type);
                    if (kind == c.LLVMFloatTypeKind or kind == c.LLVMDoubleTypeKind or kind == c.LLVMHalfTypeKind) {
                        const lhs_casted = try self.castWithRules(lhs_val, target_ty, b.lhs);
                        const rhs_casted = try self.castWithRules(rhs_val, target_ty, b.rhs);
                        return switch (b.op) {
                            '+' => c.LLVMBuildFAdd(self.builder, lhs_casted, rhs_casted, "fadd"),
                            '-' => c.LLVMBuildFSub(self.builder, lhs_casted, rhs_casted, "fsub"),
                            '*' => c.LLVMBuildFMul(self.builder, lhs_casted, rhs_casted, "fmul"),
                            '/' => c.LLVMBuildFDiv(self.builder, lhs_casted, rhs_casted, "fdiv"),
                            '%' => blk: {
                                self.uses_float_modulo = true;
                                break :blk c.LLVMBuildFRem(self.builder, lhs_casted, rhs_casted, "frem");
                            },
                            '&', '|', 'A', '$', '^', '<', '>' => try self.generateBinaryOp(b),
                            else => errors.CodegenError.UnsupportedOperation,
                        };
                    } else if (kind == c.LLVMIntegerTypeKind) {
                        return switch (b.op) {
                            '+' => c.LLVMBuildAdd(self.builder, lhs_val, rhs_val, "add"),
                            '-' => c.LLVMBuildSub(self.builder, lhs_val, rhs_val, "sub"),
                            '*' => c.LLVMBuildMul(self.builder, lhs_val, rhs_val, "mul"),
                            '/' => blk: {
                                const is_unsigned = isUnsignedType(self.getTypeNameFromLLVMType(target_ty));
                                break :blk if (is_unsigned)
                                    c.LLVMBuildUDiv(self.builder, lhs_val, rhs_val, "udiv")
                                else
                                    c.LLVMBuildSDiv(self.builder, lhs_val, rhs_val, "sdiv");
                            },
                            '%' => blk: {
                                const is_unsigned = isUnsignedType(self.getTypeNameFromLLVMType(target_ty));
                                break :blk if (is_unsigned)
                                    c.LLVMBuildURem(self.builder, lhs_val, rhs_val, "urem")
                                else
                                    c.LLVMBuildSRem(self.builder, lhs_val, rhs_val, "srem");
                            },
                            '&', '|', 'A', '$', '^', '<', '>' => try self.generateBinaryOp(b),
                            else => errors.CodegenError.UnsupportedOperation,
                        };
                    } else {
                        return errors.CodegenError.UnsupportedOperation;
                    }
                }
                return self.generateBinaryOp(b);
            },
            .unary_op => |un| {
                switch (un.op) {
                    '-' => {
                        const operand_val = try self.generateExpressionWithContext(un.operand, expected_type);
                        const operand_type = c.LLVMTypeOf(operand_val);
                        const type_kind = c.LLVMGetTypeKind(operand_type);
                        return switch (type_kind) {
                            c.LLVMIntegerTypeKind => c.LLVMBuildNeg(self.builder, operand_val, "neg"),
                            c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => c.LLVMBuildFNeg(self.builder, operand_val, "fneg"),
                            else => errors.CodegenError.UnsupportedOperation,
                        };
                    },
                    '+' => {
                        return try self.generateExpressionWithContext(un.operand, expected_type);
                    },
                    '~' => {
                        const operand_val = try self.generateExpressionWithContext(un.operand, expected_type);
                        const operand_type = c.LLVMTypeOf(operand_val);
                        const type_kind = c.LLVMGetTypeKind(operand_type);
                        if (type_kind == c.LLVMIntegerTypeKind) {
                            return c.LLVMBuildNot(self.builder, operand_val, "bitnot");
                        } else {
                            return errors.CodegenError.UnsupportedOperation;
                        }
                    },
                    '*' => {
                        var ptr_val: c.LLVMValueRef = undefined;
                        if (expected_type) |tn| {
                            const inner_expected = std.fmt.allocPrint(self.allocator, "ptr<{s}>", .{tn}) catch return errors.CodegenError.OutOfMemory;
                            defer self.allocator.free(inner_expected);
                            ptr_val = try self.generateExpressionWithContext(un.operand, inner_expected);
                        } else {
                            ptr_val = try self.generateExpression(un.operand);
                        }
                        const ptr_ty = c.LLVMTypeOf(ptr_val);
                        if (c.LLVMGetTypeKind(ptr_ty) != c.LLVMPointerTypeKind) return errors.CodegenError.TypeMismatch;
                        if (expected_type) |tn2| {
                            const load_ty = self.getLLVMType(tn2);
                            if (c.LLVMGetTypeKind(load_ty) == c.LLVMFunctionTypeKind) return ptr_val;
                            return c.LLVMBuildLoad2(self.builder, @ptrCast(load_ty), ptr_val, "deref_ctx");
                        }
                        const elem_ty = c.LLVMGetElementType(ptr_ty);
                        if (c.LLVMGetTypeKind(elem_ty) == c.LLVMFunctionTypeKind) return ptr_val;
                        return c.LLVMBuildLoad2(self.builder, elem_ty, ptr_val, "deref");
                    },
                    else => return self.generateExpression(expr),
                }
            },
            .array_initializer => |init_list| {
                if (expected_type) |type_name| {
                    if (std.mem.startsWith(u8, type_name, "simd<")) {
                        const vector_type = self.getLLVMType(type_name);
                        const element_type = c.LLVMGetElementType(vector_type);

                        if (init_list.elements.items.len == 0) {
                            return errors.CodegenError.TypeMismatch;
                        }

                        var result = c.LLVMGetUndef(vector_type);
                        for (init_list.elements.items, 0..) |element, i| {
                            const element_value = try self.generateExpression(element);
                            const casted_value = self.castToType(element_value, element_type);
                            const index = c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(i)), 0);
                            result = c.LLVMBuildInsertElement(self.builder, result, casted_value, index, "simd_init");
                        }
                        return result;
                    }
                }
                return errors.CodegenError.TypeMismatch;
            },
            .simd_initializer => |simd_init| {
                if (expected_type) |type_name| {
                    if (std.mem.startsWith(u8, type_name, "simd<")) {
                        const vector_type = self.getLLVMType(type_name);
                        const element_type = c.LLVMGetElementType(vector_type);
                        if (simd_init.elements.items.len == 0) {
                            return errors.CodegenError.TypeMismatch;
                        }
                        var result = c.LLVMGetUndef(vector_type);
                        for (simd_init.elements.items, 0..) |element, i| {
                            const element_value = try self.generateExpression(element);
                            const casted_value = self.castToType(element_value, element_type);
                            const index = c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(i)), 0);
                            result = c.LLVMBuildInsertElement(self.builder, result, casted_value, index, "simd_init");
                        }
                        return result;
                    }
                }
                return errors.CodegenError.TypeMismatch;
            },
            else => return self.generateExpression(expr),
        }
    }

    pub fn generateExpression(self: *CodeGenerator, expr: *ast.Node) errors.CodegenError!c.LLVMValueRef {
        switch (expr.data) {
            .cast => |cst| {
                if (cst.auto) return try self.generateExpression(cst.expr);
                if (cst.type_name) |tn| {
                    const target_ty = self.getLLVMType(tn);
                    const inner = try self.generateExpression(cst.expr);
                    var source_type_name: ?[]const u8 = null;
                    if (cst.expr.data == .identifier) {
                        const ident = cst.expr.data.identifier;
                        if (CodeGenerator.getVariable(self, ident.name)) |var_info| {
                            source_type_name = var_info.type_name;
                        }
                    } else if (cst.expr.data == .qualified_identifier) {
                        const qual_id = cst.expr.data.qualified_identifier;
                        if (qual_id.base.data == .identifier) {
                            const base_name = qual_id.base.data.identifier.name;
                            if (CodeGenerator.getVariable(self, base_name)) |base_var| {
                                if (std.mem.startsWith(u8, base_var.type_name, "ptr<") and std.mem.endsWith(u8, base_var.type_name, ">")) {
                                    const inner_type_name = base_var.type_name[4 .. base_var.type_name.len - 1];
                                    const struct_decl = self.struct_declarations.get(inner_type_name) orelse return errors.CodegenError.TypeMismatch;
                                    const field_map = self.struct_fields.get(inner_type_name) orelse return errors.CodegenError.TypeMismatch;
                                    const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
                                    source_type_name = struct_decl.fields.items[field_index].type_name;
                                } else {
                                    const struct_decl = self.struct_declarations.get(base_var.type_name) orelse return errors.CodegenError.TypeMismatch;
                                    const field_map = self.struct_fields.get(base_var.type_name) orelse return errors.CodegenError.TypeMismatch;
                                    const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
                                    source_type_name = struct_decl.fields.items[field_index].type_name;
                                }
                            }
                        }
                    }
                    return self.castToTypeWithSourceInfo(inner, @ptrCast(target_ty), source_type_name);
                }
                return errors.CodegenError.TypeMismatch;
            },
            .identifier => |ident| {
                if (std.mem.eql(u8, ident.name, "true")) {
                    return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 1, 0);
                } else if (std.mem.eql(u8, ident.name, "false")) {
                    return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 0, 0);
                }

                if (CodeGenerator.getVariable(self, ident.name)) |var_info| {
                    return c.LLVMBuildLoad2(self.builder, @ptrCast(var_info.type_ref), @ptrCast(var_info.value), "load");
                }
                var func_iter = c.LLVMGetFirstFunction(self.module);
                while (func_iter != null) : (func_iter = c.LLVMGetNextFunction(func_iter)) {
                    const func_name_ptr = c.LLVMGetValueName(func_iter);
                    if (func_name_ptr != null) {
                        const func_name_slice = std.mem.span(func_name_ptr);
                        if (std.mem.eql(u8, func_name_slice, ident.name)) {
                            return func_iter;
                        }
                    }
                }
                if (self.functions.get(ident.name)) |declared_func| {
                    return declared_func;
                }
                return errors.CodegenError.UndefinedVariable;
            },
            .qualified_identifier => {
                const qual_id = expr.data.qualified_identifier;
                if (qual_id.base.data == .identifier) {
                    const base_name = qual_id.base.data.identifier.name;
                    const enum_var_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ base_name, qual_id.field });
                    defer self.allocator.free(enum_var_name);
                    if (CodeGenerator.getVariable(self, enum_var_name)) |enum_var_info| {
                        const load_instr = c.LLVMBuildLoad2(self.builder, @ptrCast(enum_var_info.type_ref), @ptrCast(enum_var_info.value), "enum_value");
                        const alignment = self.getAlignmentForType(@ptrCast(enum_var_info.type_ref));
                        c.LLVMSetAlignment(load_instr, alignment);
                        return load_instr;
                    }
                }
                var result_value: c.LLVMValueRef = undefined;
                var result_type: c.LLVMTypeRef = undefined;
                var result_type_name: []const u8 = "";
                switch (qual_id.base.data) {
                    .identifier => {
                        const base_name = qual_id.base.data.identifier.name;
                        const var_info = CodeGenerator.getVariable(self, base_name) orelse return errors.CodegenError.UndefinedVariable;
                        result_value = @ptrCast(var_info.value);
                        result_type = @ptrCast(var_info.type_ref);
                        result_type_name = var_info.type_name;
                    },
                    .array_index => {
                        const array_index = qual_id.base.data.array_index;
                        const array_name = try self.getBaseIdentifierName(qual_id.base);
                        defer self.allocator.free(array_name);
                        const array_var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;
                        const index_value = try self.generateExpression(array_index.index);
                        const element_type = c.LLVMGetElementType(@ptrCast(array_var_info.type_ref));
                        var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_value };
                        result_value = c.LLVMBuildGEP2(self.builder, @ptrCast(array_var_info.type_ref), @ptrCast(array_var_info.value), &indices[0], 2, "array_element_ptr");
                        result_type = element_type;
                        const elem_type_kind = c.LLVMGetTypeKind(element_type);
                        if (elem_type_kind == c.LLVMStructTypeKind) {
                            const name_ptr = c.LLVMGetStructName(element_type);
                            if (name_ptr != null) {
                                result_type_name = std.mem.span(name_ptr);
                            }
                        }
                    },
                    .qualified_identifier => {
                        result_value = try self.generateExpression(qual_id.base);
                        result_type = c.LLVMTypeOf(result_value);
                        const type_kind = c.LLVMGetTypeKind(result_type);
                        if (type_kind == c.LLVMStructTypeKind) {
                            const name_ptr = c.LLVMGetStructName(result_type);
                            if (name_ptr != null) {
                                result_type_name = std.mem.span(name_ptr);
                            }
                        }
                    },
                    else => {
                        result_value = try self.generateExpression(qual_id.base);
                        result_type = c.LLVMTypeOf(result_value);
                    },
                }
                const type_kind = c.LLVMGetTypeKind(result_type);
                if (type_kind == c.LLVMStructTypeKind) {
                    const struct_type = self.struct_types.get(result_type_name) orelse return errors.CodegenError.TypeMismatch;
                    const field_map = self.struct_fields.get(result_type_name) orelse return errors.CodegenError.TypeMismatch;
                    const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
                    var struct_ptr = result_value;
                    if (c.LLVMGetTypeKind(c.LLVMTypeOf(result_value)) != c.LLVMPointerTypeKind) {
                        const temp_alloca = c.LLVMBuildAlloca(self.builder, c.LLVMTypeOf(result_value), "temp_struct");
                        _ = c.LLVMBuildStore(self.builder, result_value, temp_alloca);
                        struct_ptr = temp_alloca;
                    }
                    const field_ptr = try self.getStructFieldPointer(@ptrCast(struct_type), @ptrCast(struct_ptr), field_index);
                    const field_type = c.LLVMStructGetTypeAtIndex(struct_type, field_index);
                    const field_type_kind = c.LLVMGetTypeKind(field_type);
                    if (field_type_kind == c.LLVMArrayTypeKind) {
                        return @ptrCast(field_ptr);
                    }
                    const load_instr = c.LLVMBuildLoad2(self.builder, field_type, @ptrCast(field_ptr), "struct_field");
                    const alignment = self.getAlignmentForType(@ptrCast(field_type));
                    c.LLVMSetAlignment(load_instr, alignment);
                    return load_instr;
                } else if (type_kind == c.LLVMPointerTypeKind) {
                    if (qual_id.base.data == .identifier) {
                        const base_name = qual_id.base.data.identifier.name;
                        if (CodeGenerator.getVariable(self, base_name)) |base_var| {
                            const base_type_name = base_var.type_name;
                            if (std.mem.startsWith(u8, base_type_name, "ptr<") and std.mem.endsWith(u8, base_type_name, ">")) {
                                const inner_type_name = base_type_name[4 .. base_type_name.len - 1];
                                const struct_type = self.struct_types.get(inner_type_name) orelse return errors.CodegenError.TypeMismatch;
                                const field_map = self.struct_fields.get(inner_type_name) orelse return errors.CodegenError.TypeMismatch;
                                const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
                                const field_ptr = try self.getStructFieldPointer(@ptrCast(struct_type), @ptrCast(result_value), field_index);
                                const field_type = c.LLVMStructGetTypeAtIndex(struct_type, field_index);
                                const field_type_kind = c.LLVMGetTypeKind(field_type);
                                if (field_type_kind == c.LLVMArrayTypeKind) {
                                    return @ptrCast(field_ptr);
                                }
                                const load_instr = c.LLVMBuildLoad2(@ptrCast(self.builder), @ptrCast(field_type), @ptrCast(field_ptr), "struct_field");
                                const alignment = self.getAlignmentForType(field_type);
                                c.LLVMSetAlignment(load_instr, alignment);
                                return load_instr;
                            }
                        }
                    }

                    // Fallback to old method
                    const pointee_type = c.LLVMGetElementType(result_type);
                    const pointee_type_kind = c.LLVMGetTypeKind(pointee_type);
                    if (pointee_type_kind == c.LLVMStructTypeKind) {
                        const name_ptr = c.LLVMGetStructName(pointee_type);
                        if (name_ptr != null) {
                            const struct_name = std.mem.span(name_ptr);
                            const struct_type = self.struct_types.get(struct_name) orelse return errors.CodegenError.TypeMismatch;
                            const field_map = self.struct_fields.get(struct_name) orelse return errors.CodegenError.TypeMismatch;
                            const field_index = field_map.get(qual_id.field) orelse return errors.CodegenError.UndefinedVariable;
                            const field_ptr = try self.getStructFieldPointer(struct_type, result_value, field_index);
                            const field_type = c.LLVMStructGetTypeAtIndex(struct_type, field_index);
                            const field_type_kind = c.LLVMGetTypeKind(field_type);
                            if (field_type_kind == c.LLVMArrayTypeKind) {
                                return field_ptr;
                            }
                            const load_instr = c.LLVMBuildLoad2(self.builder, field_type, field_ptr, "struct_field");
                            const alignment = self.getAlignmentForType(field_type);
                            c.LLVMSetAlignment(load_instr, alignment);
                            return load_instr;
                        }
                    }
                }
                return errors.CodegenError.TypeMismatch;
            },
            .float_literal => |float| {
                const float_val = numeric.parseFloatLiteral(float.value) catch 0.0;
                return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), float_val);
            },
            .number_literal => |num| {
                if (std.mem.indexOf(u8, num.value, ".") != null) {
                    const float_val = numeric.parseFloatLiteral(num.value) catch 0.0;
                    return c.LLVMConstReal(c.LLVMDoubleTypeInContext(self.context), float_val);
                } else {
                    const value = numeric.parseNumericLiteral(num.value) catch 0;
                    return c.LLVMConstInt(c.LLVMInt64TypeInContext(self.context), @as(c_ulonglong, @intCast(value)), 0);
                }
            },
            .char_literal => |char| {
                return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(char.value)), 0);
            },
            .bool_literal => |bool_val| {
                return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), if (bool_val.value) 1 else 0, 0);
            },
            .null_literal => {
                const void_ptr_type = c.LLVMPointerType(c.LLVMInt8TypeInContext(self.context), 0);
                return c.LLVMConstNull(void_ptr_type);
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
                    '~' => {
                        const operand_val = try self.generateExpression(un.operand);
                        const operand_type = c.LLVMTypeOf(operand_val);
                        const type_kind = c.LLVMGetTypeKind(operand_type);
                        if (type_kind == c.LLVMIntegerTypeKind) {
                            return c.LLVMBuildNot(self.builder, operand_val, "bitnot");
                        } else {
                            return errors.CodegenError.UnsupportedOperation;
                        }
                    },
                    '&' => {
                        if (un.operand.data == .identifier) {
                            const ident = un.operand.data.identifier;
                            if (CodeGenerator.getVariable(self, ident.name)) |var_info| {
                                return var_info.value;
                            }
                        } else if (un.operand.data == .qualified_identifier) {
                            return try self.generateExpression(un.operand);
                        } else if (un.operand.data == .array_index) {
                            const arr_idx = un.operand.data.array_index;
                            const array_name = try self.getBaseIdentifierName(arr_idx.array);
                            defer self.allocator.free(array_name);
                            const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;
                            var index_value = try self.generateExpression(arr_idx.index);
                            index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
                            var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_value };
                            const element_ptr = c.LLVMBuildGEP2(self.builder, var_info.type_ref, var_info.value, &indices[0], 2, "array_element_ptr");
                            return element_ptr;
                        }
                        return errors.CodegenError.TypeMismatch;
                    },
                    '*' => {
                        if (un.operand.data == .unary_op and un.operand.data.unary_op.op == '&') return try self.generateExpression(un.operand);
                        if (un.operand.data == .identifier) {
                            const ident = un.operand.data.identifier;
                            if (CodeGenerator.getVariable(self, ident.name)) |var_info| {
                                const ptr_ty = var_info.type_ref;
                                const ptr_val = c.LLVMBuildLoad2(self.builder, @ptrCast(ptr_ty), @ptrCast(var_info.value), "load_ptr_for_deref");
                                const elem_ty = c.LLVMGetElementType(@ptrCast(ptr_ty));
                                if (c.LLVMGetTypeKind(elem_ty) == c.LLVMFunctionTypeKind) {
                                    return ptr_val;
                                }
                                return c.LLVMBuildLoad2(self.builder, elem_ty, ptr_val, "deref");
                            }
                        }
                        const operand_val = try self.generateExpression(un.operand);
                        const operand_type = c.LLVMTypeOf(operand_val);
                        if (c.LLVMGetTypeKind(operand_type) != c.LLVMPointerTypeKind) {
                            return errors.CodegenError.TypeMismatch;
                        }
                        const pointee_type = c.LLVMGetElementType(operand_type);
                        if (c.LLVMGetTypeKind(pointee_type) == c.LLVMFunctionTypeKind) {
                            return operand_val;
                        }
                        return c.LLVMBuildLoad2(self.builder, pointee_type, operand_val, "deref");
                    },
                    'D' => {
                        if (un.operand.data == .identifier) {
                            const ident = un.operand.data.identifier;
                            if (CodeGenerator.getVariable(self, ident.name)) |var_info| {
                                const var_type_name = self.getTypeNameFromLLVMType(var_info.type_ref);

                                if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
                                    const ptr_val = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_ptr_for_dec");
                                    const element_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                                    const element_type = self.getLLVMType(element_type_name);
                                    const minus_one = c.LLVMConstInt(c.LLVMInt64TypeInContext(self.context), @bitCast(@as(i64, -1)), 1);
                                    var indices = [_]c.LLVMValueRef{minus_one};
                                    const new_ptr = c.LLVMBuildGEP2(self.builder, element_type, ptr_val, &indices[0], 1, "ptr_dec");
                                    _ = c.LLVMBuildStore(self.builder, new_ptr, var_info.value);
                                    return new_ptr;
                                }

                                const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                                var is_allowed = false;
                                for (allowed_types) |allowed| {
                                    if (std.mem.eql(u8, var_type_name, allowed)) {
                                        is_allowed = true;
                                        break;
                                    }
                                }

                                if (!is_allowed) {
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
                                return errors.CodegenError.UndefinedVariable;
                            }
                        } else if (un.operand.data == .array_index) {
                            const arr_idx = un.operand.data.array_index;
                            const array_name = try self.getBaseIdentifierName(arr_idx.array);
                            defer self.allocator.free(array_name);
                            if (CodeGenerator.getVariable(self, array_name)) |var_info| {
                                var index_value = try self.generateExpression(arr_idx.index);
                                index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
                                const element_type = c.LLVMGetElementType(var_info.type_ref);

                                const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                                const element_type_name = self.getTypeNameFromLLVMType(element_type);
                                var is_allowed = false;
                                for (allowed_types) |allowed| {
                                    if (std.mem.eql(u8, element_type_name, allowed)) {
                                        is_allowed = true;
                                        break;
                                    }
                                }

                                if (!is_allowed) {
                                    return errors.CodegenError.UnsupportedOperation;
                                }

                                var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_value };
                                const element_ptr = c.LLVMBuildGEP2(self.builder, var_info.type_ref, var_info.value, &indices[0], 2, "array_element_ptr");

                                const current_val = c.LLVMBuildLoad2(self.builder, element_type, element_ptr, "load_for_dec");

                                const new_val = switch (c.LLVMGetTypeKind(element_type)) {
                                    c.LLVMIntegerTypeKind => blk: {
                                        const one = c.LLVMConstInt(element_type, 1, 0);
                                        break :blk c.LLVMBuildSub(self.builder, current_val, one, "dec");
                                    },
                                    c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => blk: {
                                        const one = c.LLVMConstReal(element_type, 1.0);
                                        break :blk c.LLVMBuildFSub(self.builder, current_val, one, "fdec");
                                    },
                                    else => unreachable,
                                };

                                _ = c.LLVMBuildStore(self.builder, new_val, element_ptr);

                                return new_val;
                            } else {
                                return errors.CodegenError.UndefinedVariable;
                            }
                        } else if (un.operand.data == .qualified_identifier) {
                            const pair = try self.getQualifiedFieldPtrAndType(un.operand);
                            const field_ptr = pair.ptr;
                            const field_ty = pair.ty;

                            const field_kind = c.LLVMGetTypeKind(field_ty);
                            const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                            const field_type_name = self.getTypeNameFromLLVMType(field_ty);
                            var is_allowed = false;
                            for (allowed_types) |allowed| {
                                if (std.mem.eql(u8, field_type_name, allowed)) {
                                    is_allowed = true;
                                    break;
                                }
                            }
                            if (!is_allowed) return errors.CodegenError.UnsupportedOperation;

                            const current_val = c.LLVMBuildLoad2(self.builder, field_ty, field_ptr, "load_struct_field_dec");
                            const new_val = switch (field_kind) {
                                c.LLVMIntegerTypeKind => blk: {
                                    const one = c.LLVMConstInt(field_ty, 1, 0);
                                    break :blk c.LLVMBuildSub(self.builder, current_val, one, "dec");
                                },
                                c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => blk: {
                                    const one = c.LLVMConstReal(field_ty, 1.0);
                                    break :blk c.LLVMBuildFSub(self.builder, current_val, one, "fdec");
                                },
                                else => return errors.CodegenError.UnsupportedOperation,
                            };
                            _ = c.LLVMBuildStore(self.builder, new_val, field_ptr);
                            return new_val;
                        } else {
                            return errors.CodegenError.TypeMismatch;
                        }
                    },
                    'I' => {
                        if (un.operand.data == .identifier) {
                            const ident = un.operand.data.identifier;
                            if (CodeGenerator.getVariable(self, ident.name)) |var_info| {
                                const var_type_name = self.getTypeNameFromLLVMType(var_info.type_ref);

                                if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
                                    const ptr_val = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_ptr_for_inc");
                                    const element_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                                    const element_type = self.getLLVMType(element_type_name);
                                    const one = c.LLVMConstInt(c.LLVMInt64TypeInContext(self.context), 1, 0);
                                    var indices = [_]c.LLVMValueRef{one};
                                    const new_ptr = c.LLVMBuildGEP2(self.builder, element_type, ptr_val, &indices[0], 1, "ptr_inc");
                                    _ = c.LLVMBuildStore(self.builder, new_ptr, var_info.value);
                                    return new_ptr;
                                }

                                const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                                var is_allowed = false;
                                for (allowed_types) |allowed| {
                                    if (std.mem.eql(u8, var_type_name, allowed)) {
                                        is_allowed = true;
                                        break;
                                    }
                                }
                                if (!is_allowed) {
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
                                return errors.CodegenError.UndefinedVariable;
                            }
                        } else if (un.operand.data == .array_index) {
                            const arr_idx = un.operand.data.array_index;
                            const array_name = try self.getBaseIdentifierName(arr_idx.array);
                            defer self.allocator.free(array_name);
                            if (CodeGenerator.getVariable(self, array_name)) |var_info| {
                                var index_value = try self.generateExpression(arr_idx.index);
                                index_value = self.castToType(index_value, c.LLVMInt32TypeInContext(self.context));
                                const element_type = c.LLVMGetElementType(var_info.type_ref);

                                const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                                const element_type_name = self.getTypeNameFromLLVMType(element_type);
                                var is_allowed = false;
                                for (allowed_types) |allowed| {
                                    if (std.mem.eql(u8, element_type_name, allowed)) {
                                        is_allowed = true;
                                        break;
                                    }
                                }

                                if (!is_allowed) {
                                    return errors.CodegenError.UnsupportedOperation;
                                }

                                var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_value };
                                const element_ptr = c.LLVMBuildGEP2(self.builder, var_info.type_ref, var_info.value, &indices[0], 2, "array_element_ptr");

                                const current_val = c.LLVMBuildLoad2(self.builder, element_type, element_ptr, "load_for_inc");

                                const new_val = switch (c.LLVMGetTypeKind(element_type)) {
                                    c.LLVMIntegerTypeKind => blk: {
                                        const one = c.LLVMConstInt(element_type, 1, 0);
                                        break :blk c.LLVMBuildAdd(self.builder, current_val, one, "inc");
                                    },
                                    c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => blk: {
                                        const one = c.LLVMConstReal(element_type, 1.0);
                                        break :blk c.LLVMBuildFAdd(self.builder, current_val, one, "finc");
                                    },
                                    else => unreachable,
                                };

                                _ = c.LLVMBuildStore(self.builder, new_val, element_ptr);

                                return new_val;
                            } else {
                                return errors.CodegenError.UndefinedVariable;
                            }
                        } else if (un.operand.data == .qualified_identifier) {
                            const pair = try self.getQualifiedFieldPtrAndType(un.operand);
                            const field_ptr = pair.ptr;
                            const field_ty = pair.ty;

                            const field_kind = c.LLVMGetTypeKind(field_ty);
                            const allowed_types = [_][]const u8{ "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f16", "f32", "f64" };
                            const field_type_name = self.getTypeNameFromLLVMType(field_ty);
                            var is_allowed = false;
                            for (allowed_types) |allowed| {
                                if (std.mem.eql(u8, field_type_name, allowed)) {
                                    is_allowed = true;
                                    break;
                                }
                            }
                            if (!is_allowed) return errors.CodegenError.UnsupportedOperation;
                            const current_val = c.LLVMBuildLoad2(self.builder, field_ty, field_ptr, "load_struct_field_inc");
                            const new_val = switch (field_kind) {
                                c.LLVMIntegerTypeKind => blk: {
                                    const one = c.LLVMConstInt(field_ty, 1, 0);
                                    break :blk c.LLVMBuildAdd(self.builder, current_val, one, "inc");
                                },
                                c.LLVMFloatTypeKind, c.LLVMDoubleTypeKind, c.LLVMHalfTypeKind => blk: {
                                    const one = c.LLVMConstReal(field_ty, 1.0);
                                    break :blk c.LLVMBuildFAdd(self.builder, current_val, one, "finc");
                                },
                                else => return errors.CodegenError.UnsupportedOperation,
                            };
                            _ = c.LLVMBuildStore(self.builder, new_val, field_ptr);
                            return new_val;
                        } else {
                            return errors.CodegenError.TypeMismatch;
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
            .method_call => |method| {
                // Convert method call to function call: obj.method(args) -> method(obj, args)
                var args = std.ArrayList(*ast.Node){};
                defer args.deinit(self.allocator);
                try args.append(self.allocator, method.object);
                for (method.args.items) |arg| {
                    try args.append(self.allocator, arg);
                }
                const function_call = ast.FunctionCall{
                    .name = method.method_name,
                    .is_libc = false,
                    .args = args,
                };
                return try CodeGenerator.generateFunctionCall(self, function_call);
            },
            .function_call => |call| {
                return try CodeGenerator.generateFunctionCall(self, call);
            },
            .array_index => |arr_idx| {
                var collected_indices = std.ArrayList(c.LLVMValueRef){};
                defer collected_indices.deinit(self.allocator);

                const base_node = try self.collectArrayIndices(arr_idx.array, &collected_indices);
                const index_value = try self.generateExpression(arr_idx.index);
                try collected_indices.append(self.allocator, index_value);

                const array_name = try self.getBaseIdentifierName(base_node);
                defer self.allocator.free(array_name);
                const var_info = CodeGenerator.getVariable(self, array_name) orelse return errors.CodegenError.UndefinedVariable;

                if (std.mem.startsWith(u8, var_info.type_name, "simd<")) {
                    const simd_val = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_simd");
                    var simd_index_value = collected_indices.items[collected_indices.items.len - 1];
                    simd_index_value = self.castToType(simd_index_value, c.LLVMInt32TypeInContext(self.context));
                    return c.LLVMBuildExtractElement(self.builder, simd_val, simd_index_value, "simd_extract");
                }

                if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
                    const ptr_val = c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "load_ptr");
                    const element_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                    const element_type = self.getLLVMType(element_type_name);
                    const idx = collected_indices.items[collected_indices.items.len - 1];
                    var indices = [_]c.LLVMValueRef{idx};
                    const element_ptr = c.LLVMBuildGEP2(self.builder, element_type, ptr_val, &indices[0], 1, "ptr_index");
                    return c.LLVMBuildLoad2(self.builder, element_type, element_ptr, "ptr_element");
                }

                var final_type = var_info.type_ref;
                for (0..collected_indices.items.len) |_| {
                    final_type = c.LLVMGetElementType(final_type);
                }

                var all_indices = std.ArrayList(c.LLVMValueRef){};
                defer all_indices.deinit(self.allocator);
                try all_indices.append(self.allocator, c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0));

                var i = collected_indices.items.len;
                while (i > 0) {
                    i -= 1;
                    try all_indices.append(self.allocator, collected_indices.items[i]);
                }

                const element_ptr = c.LLVMBuildGEP2(self.builder, var_info.type_ref, var_info.value, all_indices.items.ptr, @intCast(all_indices.items.len), "array_element_ptr");

                return c.LLVMBuildLoad2(self.builder, final_type, element_ptr, "array_element");
            },
            .struct_initializer => |struct_init| {
                return try self.generateStructInitializer(struct_init);
            },
            .simd_method_call => |simd_method| {
                const simd_value = try self.generateExpression(simd_method.simd);
                const simd_type = c.LLVMTypeOf(simd_value);

                // Verify this is actually a SIMD vector type
                if (c.LLVMGetTypeKind(simd_type) != c.LLVMVectorTypeKind) {
                    return errors.CodegenError.TypeMismatch;
                }

                // Handle SIMD reduction operations
                if (std.mem.eql(u8, simd_method.method_name, "sum") or
                    std.mem.eql(u8, simd_method.method_name, "product") or
                    std.mem.eql(u8, simd_method.method_name, "min") or
                    std.mem.eql(u8, simd_method.method_name, "max"))
                {
                    return try self.generateSimdReduction(simd_method.method_name, simd_value);
                }

                return errors.CodegenError.UnsupportedOperation;
            },
            .array_initializer => |_| {
                // Array initializers need context to determine type
                // This case should typically be handled by generateExpressionWithContext
                return errors.CodegenError.TypeMismatch;
            },
            .simd_initializer => |_| {
                // SIMD initializers need context to determine type
                // This case should typically be handled by generateExpressionWithContext
                return errors.CodegenError.TypeMismatch;
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
        const is_lhs_pointer = lhs_kind == c.LLVMPointerTypeKind;
        const is_rhs_pointer = rhs_kind == c.LLVMPointerTypeKind;
        if (is_lhs_pointer and is_rhs_pointer) {
            if (comparison.op != '=' and comparison.op != '!') {
                return errors.CodegenError.TypeMismatch;
            }
            return switch (comparison.op) {
                '=' => c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, lhs_value, rhs_value, "ptr_cmp_eq"),
                '!' => c.LLVMBuildICmp(self.builder, c.LLVMIntNE, lhs_value, rhs_value, "ptr_cmp_ne"),
                else => return errors.CodegenError.UnsupportedOperation,
            };
        }
        if (is_lhs_pointer or is_rhs_pointer) {
            if (comparison.op != '=' and comparison.op != '!') {
                return errors.CodegenError.TypeMismatch;
            }
            var casted_lhs = lhs_value;
            var casted_rhs = rhs_value;
            if (is_lhs_pointer and !is_rhs_pointer) {
                casted_rhs = c.LLVMBuildPointerCast(self.builder, rhs_value, lhs_type, "cast_to_ptr");
            } else if (!is_lhs_pointer and is_rhs_pointer) {
                casted_lhs = c.LLVMBuildPointerCast(self.builder, lhs_value, rhs_type, "cast_to_ptr");
            }
            return switch (comparison.op) {
                '=' => c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, casted_lhs, casted_rhs, "ptr_null_cmp_eq"),
                '!' => c.LLVMBuildICmp(self.builder, c.LLVMIntNE, casted_lhs, casted_rhs, "ptr_null_cmp_ne"),
                else => return errors.CodegenError.UnsupportedOperation,
            };
        }
        const is_lhs_struct = lhs_kind == c.LLVMStructTypeKind;
        const is_rhs_struct = rhs_kind == c.LLVMStructTypeKind;
        if (is_lhs_struct or is_rhs_struct) return errors.CodegenError.TypeMismatch;

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
        const lhs_type = c.LLVMTypeOf(lhs_value);
        const lhs_kind = c.LLVMGetTypeKind(lhs_type);
        if (c.LLVMGetTypeKind(lhs_type) == c.LLVMVectorTypeKind) {
            const rhs_value = try self.generateExpression(bin_op.rhs);
            const rhs_type = c.LLVMTypeOf(rhs_value);
            if (c.LLVMGetTypeKind(rhs_type) == c.LLVMVectorTypeKind) {
                return try self.generateSimdBinaryOp(bin_op.op, lhs_value, rhs_value);
            }
        }

        if (lhs_kind == c.LLVMPointerTypeKind and (bin_op.op == '+' or bin_op.op == '-')) {
            const rhs_value = try self.generateExpression(bin_op.rhs);
            const rhs_type = c.LLVMTypeOf(rhs_value);
            const rhs_kind = c.LLVMGetTypeKind(rhs_type);

            if (rhs_kind == c.LLVMIntegerTypeKind) {
                var element_type: c.LLVMTypeRef = undefined;
                if (bin_op.lhs.data == .identifier) {
                    const var_info = CodeGenerator.getVariable(self, bin_op.lhs.data.identifier.name) orelse return errors.CodegenError.UndefinedVariable;
                    if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
                        const element_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                        element_type = self.getLLVMType(element_type_name);
                    } else {
                        return errors.CodegenError.TypeMismatch;
                    }
                } else {
                    return errors.CodegenError.TypeMismatch;
                }
                const index = if (bin_op.op == '+') rhs_value else c.LLVMBuildNeg(self.builder, rhs_value, "neg_offset");
                var indices = [_]c.LLVMValueRef{index};
                return c.LLVMBuildGEP2(self.builder, element_type, lhs_value, &indices[0], 1, "ptr_arith");
            } else if (rhs_kind == c.LLVMPointerTypeKind and bin_op.op == '-') {
                var element_type: c.LLVMTypeRef = undefined;
                if (bin_op.lhs.data == .identifier) {
                    const var_info = CodeGenerator.getVariable(self, bin_op.lhs.data.identifier.name) orelse return errors.CodegenError.UndefinedVariable;
                    if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
                        const element_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                        element_type = self.getLLVMType(element_type_name);
                    } else {
                        return errors.CodegenError.TypeMismatch;
                    }
                } else {
                    return errors.CodegenError.TypeMismatch;
                }
                const lhs_int = c.LLVMBuildPtrToInt(self.builder, lhs_value, c.LLVMInt64TypeInContext(self.context), "ptr_to_int_lhs");
                const rhs_int = c.LLVMBuildPtrToInt(self.builder, rhs_value, c.LLVMInt64TypeInContext(self.context), "ptr_to_int_rhs");
                const byte_diff = c.LLVMBuildSub(self.builder, lhs_int, rhs_int, "byte_diff");
                const element_size = c.LLVMABISizeOfType(c.LLVMGetModuleDataLayout(self.module), element_type);
                const size_val = c.LLVMConstInt(c.LLVMInt64TypeInContext(self.context), element_size, 0);
                return c.LLVMBuildSDiv(self.builder, byte_diff, size_val, "ptr_diff");
            }
            return errors.CodegenError.TypeMismatch;
        }

        const lhs_type_name = self.getTypeNameFromLLVMType(lhs_type);
        const rhs_value = try self.generateExpressionWithContext(bin_op.rhs, lhs_type_name);

        const rhs_type = c.LLVMTypeOf(rhs_value);
        const rhs_kind = c.LLVMGetTypeKind(rhs_type);

        if (lhs_kind == c.LLVMIntegerTypeKind and rhs_kind == c.LLVMPointerTypeKind and bin_op.op == '+') {
            if (bin_op.rhs.data == .identifier) {
                const var_info = CodeGenerator.getVariable(self, bin_op.rhs.data.identifier.name) orelse return errors.CodegenError.UndefinedVariable;
                if (std.mem.startsWith(u8, var_info.type_name, "ptr<")) {
                    const element_type_name = var_info.type_name[4 .. var_info.type_name.len - 1];
                    const element_type = self.getLLVMType(element_type_name);
                    var indices = [_]c.LLVMValueRef{lhs_value};
                    return c.LLVMBuildGEP2(self.builder, element_type, rhs_value, &indices[0], 1, "ptr_arith");
                }
            }
            return errors.CodegenError.TypeMismatch;
        }

        if (lhs_kind == c.LLVMPointerTypeKind or rhs_kind == c.LLVMPointerTypeKind) {
            return errors.CodegenError.TypeMismatch;
        }
        if (bin_op.op != '&' and bin_op.op != '|') {
            if (lhs_kind == c.LLVMIntegerTypeKind and c.LLVMGetIntTypeWidth(lhs_type) == 1) {
                return errors.CodegenError.TypeMismatch;
            }
            if (rhs_kind == c.LLVMIntegerTypeKind and c.LLVMGetIntTypeWidth(rhs_type) == 1) {
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
            const lhs_ty_now = c.LLVMTypeOf(casted_lhs);
            if (lhs_ty_now != result_type) {
                const lhs_kind_now = c.LLVMGetTypeKind(lhs_ty_now);
                if (lhs_kind_now == c.LLVMIntegerTypeKind) {
                    casted_lhs = c.LLVMBuildSIToFP(self.builder, casted_lhs, result_type, "sitofp_lhs2");
                } else if ((lhs_kind_now == c.LLVMHalfTypeKind or lhs_kind_now == c.LLVMFloatTypeKind) and c.LLVMGetTypeKind(result_type) == c.LLVMDoubleTypeKind) {
                    casted_lhs = c.LLVMBuildFPExt(self.builder, casted_lhs, result_type, "fpext_lhs2");
                } else if (lhs_kind_now == c.LLVMDoubleTypeKind and c.LLVMGetTypeKind(result_type) == c.LLVMFloatTypeKind) {
                    casted_lhs = c.LLVMBuildFPTrunc(self.builder, casted_lhs, result_type, "fptrunc_lhs");
                }
            }
            const rhs_ty_now = c.LLVMTypeOf(casted_rhs);
            if (rhs_ty_now != result_type) {
                const rhs_kind_now = c.LLVMGetTypeKind(rhs_ty_now);
                if (rhs_kind_now == c.LLVMIntegerTypeKind) {
                    casted_rhs = c.LLVMBuildSIToFP(self.builder, casted_rhs, result_type, "sitofp_rhs2");
                } else if ((rhs_kind_now == c.LLVMHalfTypeKind or rhs_kind_now == c.LLVMFloatTypeKind) and c.LLVMGetTypeKind(result_type) == c.LLVMDoubleTypeKind) {
                    casted_rhs = c.LLVMBuildFPExt(self.builder, casted_rhs, result_type, "fpext_rhs2");
                } else if (rhs_kind_now == c.LLVMDoubleTypeKind and c.LLVMGetTypeKind(result_type) == c.LLVMFloatTypeKind) {
                    casted_rhs = c.LLVMBuildFPTrunc(self.builder, casted_rhs, result_type, "fptrunc_rhs");
                }
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
            '%' => {
                if (is_float_op) {
                    self.uses_float_modulo = true;
                    return c.LLVMBuildFRem(self.builder, casted_lhs, casted_rhs, "frem");
                } else {
                    const result_type_kind = c.LLVMGetTypeKind(result_type);
                    if (result_type_kind != c.LLVMIntegerTypeKind) {
                        return errors.CodegenError.UnsupportedOperation;
                    }
                    const width = c.LLVMGetIntTypeWidth(result_type);
                    if (width != 8 and width != 16 and width != 32 and width != 64) {
                        return errors.CodegenError.UnsupportedOperation;
                    }
                    if (isUnsignedType(self.getTypeNameFromLLVMType(result_type)))
                        return c.LLVMBuildURem(self.builder, casted_lhs, casted_rhs, "urem")
                    else
                        return c.LLVMBuildSRem(self.builder, casted_lhs, casted_rhs, "srem");
                }
            },
            '&' => self.generateLogicalAnd(bin_op.lhs, bin_op.rhs),
            '|' => self.generateLogicalOr(bin_op.lhs, bin_op.rhs),
            'A' => {
                if (is_float_op) return errors.CodegenError.UnsupportedOperation;
                return c.LLVMBuildAnd(self.builder, casted_lhs, casted_rhs, "and");
            },
            '$' => {
                if (is_float_op) return errors.CodegenError.UnsupportedOperation;
                return c.LLVMBuildOr(self.builder, casted_lhs, casted_rhs, "or");
            },
            '^' => {
                if (is_float_op) return errors.CodegenError.UnsupportedOperation;
                return c.LLVMBuildXor(self.builder, casted_lhs, casted_rhs, "xor");
            },
            '<' => {
                if (is_float_op) return errors.CodegenError.UnsupportedOperation;
                return c.LLVMBuildShl(self.builder, casted_lhs, casted_rhs, "shl");
            },
            '>' => {
                if (is_float_op) return errors.CodegenError.UnsupportedOperation;
                if (isUnsignedType(self.getTypeNameFromLLVMType(result_type)))
                    return c.LLVMBuildLShr(self.builder, casted_lhs, casted_rhs, "lshr")
                else
                    return c.LLVMBuildAShr(self.builder, casted_lhs, casted_rhs, "ashr");
            },
            else => {
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

    pub const convertToBool = utils.convertToBool;

    pub const generateBrainfuck = bfck.generateBrainfuck;

    pub fn writeToFile(self: *CodeGenerator, filename: []const u8) !void {
        const filename_z = utils.dupeZ(self.allocator, filename);
        defer self.allocator.free(filename_z);

        var error_msg: [*c]u8 = null;
        const result = c.LLVMPrintModuleToFile(self.module, filename_z.ptr, &error_msg);

        if (result != 0) {
            if (error_msg != null) {
                c.LLVMDisposeMessage(error_msg);
            }
            return error.WriteFailed;
        }
    }

    pub fn compileToExecutable(self: *CodeGenerator, output: []const u8, arch: []const u8, link_objects: []const []const u8, keep_ll: bool, optimize: bool, extra_flags: []const []const u8) !void {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        const ir_file = try std.fmt.allocPrint(arena_alloc, "{s}.ll", .{output});
        defer arena_alloc.free(ir_file);
        try self.writeToFile(ir_file);

        var clang_args_list = std.ArrayList([]const u8){};
        try clang_args_list.appendSlice(arena_alloc, &[_][]const u8{ "clang", ir_file, "-o", output, "-lc" });

        if (arch.len != 0) {
            const march_flag: []const u8 = try std.fmt.allocPrint(arena_alloc, "--target={s}", .{arch});
            try clang_args_list.append(arena_alloc, march_flag);
        }

        for (link_objects) |obj| {
            try clang_args_list.append(arena_alloc, obj);
        }

        if (self.uses_float_modulo) {
            try clang_args_list.append(arena_alloc, "-lm");
        }

        if (optimize) {
            // Optimize llvm ir
            var opt_args_list = std.ArrayList([]const u8){};
            try opt_args_list.appendSlice(arena_alloc, &[_][]const u8{
                "opt",
                "-O3",
                ir_file,
                "-o",
                ir_file, // Owerwrite previous one
            });

            var opt_child_process = std.process.Child.init(opt_args_list.items, arena_alloc);
            opt_child_process.stdout_behavior = .Pipe;
            opt_child_process.stderr_behavior = .Pipe;
            try opt_child_process.spawn();

            const result = try opt_child_process.wait();
            if (result != .Exited or result.Exited != 0) {
                if (opt_child_process.stderr) |s| {
                    var buf: [4096]u8 = undefined;
                    const n = s.read(&buf) catch 0;
                    if (n > 0) std.debug.print("opt stderr: {s}\n", .{buf[0..n]});
                }
                return error.CompilationFailed;
            }

            try clang_args_list.append(arena_alloc, "-O3"); // Append additional flag to clang (+100% fps)
        }

        // Append additional arguments
        for (extra_flags) |ef| {
            try clang_args_list.append(arena_alloc, ef);
        }

        var child = std.process.Child.init(clang_args_list.items, arena_alloc);
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        try child.spawn();
        const result = try child.wait();

        if (!keep_ll and (result == .Exited and result.Exited == 0)) {
            std.fs.cwd().deleteFile(ir_file) catch {};
        }

        if (result != .Exited or result.Exited != 0) {
            if (child.stderr) |s| {
                var buf: [8192]u8 = undefined;
                const n = s.read(&buf) catch 0;
                if (n > 0) std.debug.print("clang stderr: {s}\n", .{buf[0..n]});
            }
            return error.CompilationFailed;
        }
    }
};
