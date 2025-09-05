const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const utils = @import("utils.zig");
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
    uses_float_modulo: bool,
    struct_types: std.HashMap([]const u8, c.LLVMTypeRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage),
    struct_fields: std.HashMap([]const u8, std.HashMap([]const u8, c_uint, std.hash_map.StringContext, std.hash_map.default_max_load_percentage), std.hash_map.StringContext, std.hash_map.default_max_load_percentage),

    const VariableInfo = struct {
        value: c.LLVMValueRef,
        type_ref: c.LLVMTypeRef,
        type_name: []const u8,
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
            .uses_float_modulo = false,
            .struct_types = std.HashMap([]const u8, c.LLVMTypeRef, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
            .struct_fields = std.HashMap([]const u8, std.HashMap([]const u8, c_uint, std.hash_map.StringContext, std.hash_map.default_max_load_percentage), std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(allocator),
        };
    }

    pub fn deinit(self: *CodeGenerator) void {
        self.functions.deinit();
        self.variables.deinit();
        self.external_c_functions.deinit();
        self.loop_context_stack.deinit();
        self.struct_types.deinit();
        var struct_fields_iter = self.struct_fields.iterator();
        while (struct_fields_iter.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.struct_fields.deinit();

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
        if (self.variable_scopes.items.len > 1) {
            var scope = self.variable_scopes.pop().?;
            scope.deinit();
        }
    }

    fn clearCurrentFunctionScopes(self: *CodeGenerator) void {
        while (self.variable_scopes.items.len > 1) {
            var scope = self.variable_scopes.pop().?;
            scope.deinit();
        }
    }

    fn buildQualifiedName(self: *CodeGenerator, node: *ast.Node) ![]const u8 {
        switch (node.data) {
            .identifier => |ident| {
                return try self.allocator.dupe(u8, ident.name);
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
            .identifier => |ident| return try self.allocator.dupe(u8, ident.name),
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

    fn generateArrayElementFieldAssignment(self: *CodeGenerator, array_index: ast.ArrayIndex, field_path: []const u8, value_expr: *ast.Node) errors.CodegenError!void {
        const array_name = try self.getBaseIdentifierName(array_index.array);
        defer self.allocator.free(array_name);
        const array_var_info = self.getVariable(array_name) orelse return errors.CodegenError.UndefinedVariable;
        const index_value = try self.generateExpression(array_index.index);
        const element_type = c.LLVMGetElementType(array_var_info.type_ref);
        const element_type_kind = c.LLVMGetTypeKind(element_type);
        if (element_type_kind != c.LLVMStructTypeKind) {
            return errors.CodegenError.TypeMismatch;
        }
        const struct_name = try self.getStructTypeName(element_type);
        defer self.allocator.free(struct_name);
        var array_indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_value };
        const array_element_ptr = c.LLVMBuildGEP2(self.builder, array_var_info.type_ref, array_var_info.value, &array_indices[0], 2, "array_element_ptr");
        try self.generateRecursiveFieldAssignment(array_element_ptr, element_type, struct_name, field_path, value_expr);
    }

    fn getStructTypeName(self: *CodeGenerator, struct_type: c.LLVMTypeRef) ![]const u8 {
        const name_ptr = c.LLVMGetStructName(struct_type);
        if (name_ptr != null) {
            return try self.allocator.dupe(u8, std.mem.span(name_ptr));
        }
        var it = self.struct_types.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* == struct_type) {
                return try self.allocator.dupe(u8, entry.key_ptr.*);
            }
        }
        return try self.allocator.dupe(u8, "anonymous_struct");
    }

    fn generateRecursiveFieldAssignment(self: *CodeGenerator, base_value: c.LLVMValueRef, base_type: c.LLVMTypeRef, base_type_name: []const u8, field_path: []const u8, value_expr: *ast.Node) errors.CodegenError!void {
        var current_value = base_value;
        var current_type = base_type;
        var current_type_name = base_type_name;
        var path = field_path;

        while (path.len > 0) {
            if (std.mem.startsWith(u8, path, "[")) {
                const close_bracket_pos = std.mem.indexOfScalar(u8, path, ']') orelse return errors.CodegenError.TypeMismatch;
                const index_str = path[1..close_bracket_pos];
                const index_expr = if (self.getVariable(index_str)) |var_info| blk: {
                    break :blk c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "index_var");
                } else blk: {
                    break :blk try self.generateExpressionFromString(index_str);
                };
                const var_type_kind = c.LLVMGetTypeKind(current_type);
                if (var_type_kind != c.LLVMArrayTypeKind) return errors.CodegenError.TypeMismatch;
                const element_type = c.LLVMGetElementType(current_type);
                var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_expr };
                const element_ptr = c.LLVMBuildGEP2(self.builder, current_type, current_value, &indices[0], 2, "array_element_ptr");
                current_value = element_ptr;
                current_type = element_type;
                const elem_type_kind = c.LLVMGetTypeKind(element_type);
                if (elem_type_kind == c.LLVMStructTypeKind) {
                    const name_ptr = c.LLVMGetStructName(element_type);
                    if (name_ptr != null) {
                        current_type_name = std.mem.span(name_ptr);
                    }
                }
                path = path[close_bracket_pos + 1 ..];
                if (std.mem.startsWith(u8, path, ".")) {
                    path = path[1..];
                }
            } else {
                const dot_pos = std.mem.indexOfScalar(u8, path, '.');
                const field_name_len = if (dot_pos) |pos| pos else path.len;
                const field_name = path[0..field_name_len];
                const var_type_kind = c.LLVMGetTypeKind(current_type);
                if (var_type_kind != c.LLVMStructTypeKind) return errors.CodegenError.TypeMismatch;
                const struct_type = self.struct_types.get(current_type_name) orelse return errors.CodegenError.TypeMismatch;
                const field_map = self.struct_fields.get(current_type_name) orelse return errors.CodegenError.TypeMismatch;
                const field_index = field_map.get(field_name) orelse return errors.CodegenError.UndefinedVariable;
                const field_count = c.LLVMCountStructElementTypes(struct_type);
                if (field_index >= field_count) {
                    return errors.CodegenError.TypeMismatch;
                }
                const field_ptr = try self.getStructFieldPointer(struct_type, current_value, field_index);
                const field_type = c.LLVMStructGetTypeAtIndex(struct_type, field_index);
                if (dot_pos != null) {
                    const field_type_kind = c.LLVMGetTypeKind(field_type);
                    if (field_type_kind != c.LLVMStructTypeKind) return errors.CodegenError.TypeMismatch;
                    current_value = field_ptr;
                    current_type = field_type;
                    const name_ptr = c.LLVMGetStructName(field_type);
                    if (name_ptr == null) return errors.CodegenError.TypeMismatch;
                    current_type_name = std.mem.span(name_ptr);
                    path = path[dot_pos.? + 1 ..];
                } else {
                    try self.assignToField(field_ptr, field_type, value_expr);
                    return;
                }
            }
        }
        return errors.CodegenError.TypeMismatch;
    }

    fn generateStructFieldAssignment(self: *CodeGenerator, struct_name: []const u8, field_path: []const u8, value_expr: *ast.Node) errors.CodegenError!void {
        const struct_var_info = self.getVariable(struct_name) orelse return errors.CodegenError.UndefinedVariable;
        return try self.generateRecursiveFieldAssignment(struct_var_info.value, struct_var_info.type_ref, struct_var_info.type_name, field_path, value_expr);
    }

    fn generateExpressionFromString(self: *CodeGenerator, expr_str: []const u8) errors.CodegenError!c.LLVMValueRef {
        if (std.fmt.parseInt(i64, expr_str, 10)) |value| {
            return c.LLVMConstInt(c.LLVMInt64TypeInContext(self.context), @as(c_ulonglong, @intCast(value)), 0);
        } else |_| {
            return errors.CodegenError.TypeMismatch;
        }
    }

    fn generateRegularVariableAssignment(self: *CodeGenerator, as: ast.Assignment) errors.CodegenError!void {
        const ident = as.target.data.identifier;
        const var_info = self.getVariable(ident.name) orelse return errors.CodegenError.UndefinedVariable;
        const var_type_kind = c.LLVMGetTypeKind(var_info.type_ref);
        if (var_type_kind == c.LLVMArrayTypeKind) {
            try self.generateArrayReassignment(ident.name, as.value);
        } else {
            const value = try self.generateExpression(as.value);
            const casted_value = self.castToType(value, var_info.type_ref);
            _ = c.LLVMBuildStore(self.builder, casted_value, var_info.value);
        }
    }

    fn getStructFieldPointer(self: *CodeGenerator, struct_type: c.LLVMTypeRef, struct_value: c.LLVMValueRef, field_index: c_uint) errors.CodegenError!c.LLVMValueRef {
        var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), field_index, 0) };
        return c.LLVMBuildGEP2(self.builder, struct_type, struct_value, &indices[0], 2, "struct_field_ptr");
    }

    fn assignToField(self: *CodeGenerator, field_ptr: c.LLVMValueRef, field_type: c.LLVMTypeRef, value_expr: *ast.Node) errors.CodegenError!void {
        const field_type_kind = c.LLVMGetTypeKind(field_type);
        if (value_expr.data == .string_literal and field_type_kind == c.LLVMArrayTypeKind) {
            try self.assignStringLiteralToArrayField(field_ptr, field_type, value_expr.data.string_literal);
            return;
        }
        const value = try self.generateExpression(value_expr);
        const casted_value = self.castToType(value, field_type);
        const store_instr = c.LLVMBuildStore(self.builder, casted_value, field_ptr);
        const alignment = self.getAlignmentForType(field_type);
        c.LLVMSetAlignment(store_instr, alignment);
    }

    fn assignStringLiteralToArrayField(self: *CodeGenerator, field_ptr: c.LLVMValueRef, field_type: c.LLVMTypeRef, str_lit: ast.StringLiteral) errors.CodegenError!void {
        const element_type = c.LLVMGetElementType(field_type);
        const element_type_kind = c.LLVMGetTypeKind(element_type);
        if (element_type_kind != c.LLVMIntegerTypeKind or c.LLVMGetIntTypeWidth(element_type) != 8) {
            return;
        }
        const parsed_str = try self.parse_escape(str_lit.value);
        defer self.allocator.free(parsed_str);
        const str_len = parsed_str.len;
        const array_length = c.LLVMGetArrayLength(field_type);
        if (str_len > array_length) {
            return errors.CodegenError.TypeMismatch;
        }
        const memcpy_func = try self.declareLibcFunction("memcpy");
        const i8_type = c.LLVMInt8TypeInContext(self.context);
        const i8_ptr_type = c.LLVMPointerType(i8_type, 0);
        const size_t_type = utils.libcTypeToLLVM(self, .size_t_type);
        const field_i8_ptr = c.LLVMBuildBitCast(self.builder, field_ptr, i8_ptr_type, "field_i8_ptr");
        const global_str = c.LLVMBuildGlobalStringPtr(self.builder, parsed_str.ptr, "temp_str_ptr");
        const copy_size = c.LLVMConstInt(size_t_type, @as(c_ulonglong, @intCast(str_len)), 0);
        var memcpy_args = [_]c.LLVMValueRef{ field_i8_ptr, global_str, copy_size };
        _ = c.LLVMBuildCall2(self.builder, c.LLVMGlobalGetValueType(memcpy_func), memcpy_func, &memcpy_args[0], 3, "");
    }

    fn generateRecursiveFieldAccess(self: *CodeGenerator, base_value: c.LLVMValueRef, base_type: c.LLVMTypeRef, base_type_name: []const u8, field_path: []const u8) errors.CodegenError!c.LLVMValueRef {
        var current_value = base_value;
        var current_type = base_type;
        var current_type_name = base_type_name;
        var path = field_path;
        while (path.len > 0) {
            if (std.mem.startsWith(u8, path, "[")) {
                const close_bracket_pos = std.mem.indexOfScalar(u8, path, ']') orelse return errors.CodegenError.TypeMismatch;
                const index_str = path[1..close_bracket_pos];
                const index_expr = if (self.getVariable(index_str)) |var_info| blk: {
                    break :blk c.LLVMBuildLoad2(self.builder, var_info.type_ref, var_info.value, "index_var");
                } else blk: {
                    break :blk try self.generateExpressionFromString(index_str);
                };
                const var_type_kind = c.LLVMGetTypeKind(current_type);
                if (var_type_kind != c.LLVMArrayTypeKind) return errors.CodegenError.TypeMismatch;
                const element_type = c.LLVMGetElementType(current_type);
                var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_expr };
                const element_ptr = c.LLVMBuildGEP2(self.builder, current_type, current_value, &indices[0], 2, "array_element_ptr");
                current_value = element_ptr;
                current_type = element_type;
                const elem_type_kind = c.LLVMGetTypeKind(element_type);
                if (elem_type_kind == c.LLVMStructTypeKind) {
                    const name_ptr = c.LLVMGetStructName(element_type);
                    if (name_ptr != null) {
                        current_type_name = std.mem.span(name_ptr);
                    }
                }
                path = path[close_bracket_pos + 1 ..];
                if (std.mem.startsWith(u8, path, ".")) {
                    path = path[1..];
                }
            } else {
                const dot_pos = std.mem.indexOfScalar(u8, path, '.');
                const field_name_len = if (dot_pos) |pos| pos else path.len;
                const field_name = path[0..field_name_len];
                const var_type_kind = c.LLVMGetTypeKind(current_type);
                if (var_type_kind != c.LLVMStructTypeKind) return errors.CodegenError.TypeMismatch;
                const struct_type = self.struct_types.get(current_type_name) orelse {
                    return errors.CodegenError.TypeMismatch;
                };
                const field_map = self.struct_fields.get(current_type_name) orelse return errors.CodegenError.TypeMismatch;
                const field_index = field_map.get(field_name) orelse return errors.CodegenError.UndefinedVariable;
                const field_count = c.LLVMCountStructElementTypes(struct_type);
                if (field_index >= field_count) {
                    return errors.CodegenError.TypeMismatch;
                }
                const field_ptr = try self.getStructFieldPointer(struct_type, current_value, field_index);
                const field_type = c.LLVMStructGetTypeAtIndex(struct_type, field_index);
                if (dot_pos != null) {
                    const field_type_kind = c.LLVMGetTypeKind(field_type);
                    if (field_type_kind != c.LLVMStructTypeKind) return errors.CodegenError.TypeMismatch;
                    current_value = field_ptr;
                    current_type = field_type;
                    const name_ptr = c.LLVMGetStructName(field_type);
                    if (name_ptr == null) return errors.CodegenError.TypeMismatch;
                    current_type_name = std.mem.span(name_ptr);
                    path = path[dot_pos.? + 1 ..];
                } else {
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
    }

    fn generateStructFieldAccess(self: *CodeGenerator, struct_var_info: VariableInfo, field_path: []const u8) errors.CodegenError!c.LLVMValueRef {
        return try self.generateRecursiveFieldAccess(struct_var_info.value, struct_var_info.type_ref, struct_var_info.type_name, field_path);
    }

    // TODO:  move this to interface
    pub fn getVariable(self: *CodeGenerator, name: []const u8) ?VariableInfo {
        var i = self.variable_scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.variable_scopes.items[i].get(name)) |var_info| {
                return var_info;
            }
        }
        if (self.variables.get(name)) |var_info| {
            return var_info;
        }
        return null;
    }

    fn putVariable(self: *CodeGenerator, name: []const u8, var_info: VariableInfo) !void {
        const current_scope = &self.variable_scopes.items[self.variable_scopes.items.len - 1];
        if (current_scope.contains(name)) {
            return errors.CodegenError.RedeclaredVariable;
        }

        try current_scope.put(name, var_info);
    }

    fn variableExistsInCurrentScope(self: *CodeGenerator, name: []const u8) bool {
        if (self.variable_scopes.items.len == 0) return false;
        const current_scope = &self.variable_scopes.items[self.variable_scopes.items.len - 1];
        return current_scope.contains(name);
    }

    fn getLLVMStructType(self: *CodeGenerator, struct_name: []const u8) !c.LLVMTypeRef {
        if (self.struct_types.get(struct_name)) |existing| {
            return existing;
        }

        // Look for the struct declaration in the AST
        // This is a simplified approach - in a real implementation we'd need to
        // build a symbol table for struct declarations during code generation
        return errors.CodegenError.TypeMismatch;
    }

    fn getLLVMType(self: *CodeGenerator, type_name: []const u8) c.LLVMTypeRef {
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

    fn isUnsignedType(type_name: []const u8) bool {
        return std.mem.startsWith(u8, type_name, "u");
    }

    fn getAlignmentForType(self: *CodeGenerator, llvm_type: c.LLVMTypeRef) c_uint {
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

    // TODO:  move this to interface
    pub fn declareLibcFunction(self: *CodeGenerator, func_name: []const u8) !c.LLVMValueRef {
        if (self.functions.get(func_name)) |existing| {
            return existing;
        }
        if (utils.LIBC_FUNCTIONS.get(func_name)) |signature| {
            const func = try self.createFunctionFromSignature(func_name, signature);
            try self.functions.put(try self.allocator.dupe(u8, func_name), func);
            return func;
        }
        const func = try self.createGenericLibcFunction(func_name);
        try self.functions.put(try self.allocator.dupe(u8, func_name), func);
        return func;
    }

    fn createFunctionFromSignature(self: *CodeGenerator, func_name: []const u8, signature: utils.LibcFunctionSignature) !c.LLVMValueRef {
        const return_type = utils.libcTypeToLLVM(self, signature.return_type);

        var param_types = std.ArrayList(c.LLVMTypeRef).init(self.allocator);
        defer param_types.deinit();

        for (signature.param_types) |param_type| {
            try param_types.append(utils.libcTypeToLLVM(self, param_type));
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

        return llvm_func;
    }

    fn collectStructDeclarations(self: *CodeGenerator, node: *ast.Node) errors.CodegenError!void {
        switch (node.data) {
            .program => |prog| {
                for (prog.functions.items) |func| {
                    if (func.data == .struct_decl) {
                        try self.generateStructType(func.data.struct_decl);
                    }
                }
            },
            else => {},
        }
    }

    fn generateStructType(self: *CodeGenerator, struct_decl: ast.StructDecl) errors.CodegenError!void {
        var field_types = std.ArrayList(c.LLVMTypeRef).init(self.allocator);
        defer field_types.deinit();
        var field_map = std.HashMap([]const u8, c_uint, std.hash_map.StringContext, std.hash_map.default_max_load_percentage).init(self.allocator);
        for (struct_decl.fields.items, 0..) |field, i| {
            const field_type = self.getLLVMType(field.type_name);
            try field_types.append(field_type);
            try field_map.put(field.name, @intCast(i));
        }
        const struct_name_z = try self.allocator.dupeZ(u8, struct_decl.name);
        defer self.allocator.free(struct_name_z);
        const struct_type = c.LLVMStructCreateNamed(self.context, struct_name_z.ptr);
        c.LLVMStructSetBody(struct_type, field_types.items.ptr, @intCast(field_types.items.len), 0);
        try self.struct_types.put(struct_decl.name, struct_type);
        try self.struct_fields.put(struct_decl.name, field_map);
    }

    pub fn generateCode(self: *CodeGenerator, program: *ast.Node) errors.CodegenError!void {
        switch (program.data) {
            .program => |prog| {
                try self.collectStructDeclarations(program);

                for (prog.functions.items) |func| {
                    if (func.data == .enum_decl) {
                        try self.generateEnumDeclaration(func.data.enum_decl);
                    } else if (func.data == .struct_decl) {
                        // Struct declarations are handled at declaration time, no additional processing needed
                        // The struct type information is stored in the AST but doesn't need LLVM IR generation
                        // since structs are compiled to LLVM types when used in variables/functions
                    }
                }
                for (prog.functions.items) |func| {
                    if (func.data == .c_function_decl) {
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
                        .type_name = param.type_name,
                    });
                }

                const valid_control_flow = try self.hasValidControlFlow(func_node);
                for (func.body.items) |stmt| {
                    try self.generateStatement(stmt);
                }
                if (!valid_control_flow and !std.mem.eql(u8, func.return_type, "void")) {
                    if (func.body.items.len == 0 or !self.isReturnStatement(func.body.items[func.body.items.len - 1])) {
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
                            const default_value = utils.getDefaultValueForType(self, func.return_type);
                            _ = c.LLVMBuildRet(self.builder, default_value);
                        } else {
                            if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
                                const default_value = utils.getDefaultValueForType(self, func.return_type);
                                _ = c.LLVMBuildRet(self.builder, default_value);
                            }
                        }
                    }
                }
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }

    // move to control_flow.zig
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
                switch (as.target.data) {
                    .identifier => {
                        try self.generateRegularVariableAssignment(as);
                    },
                    .qualified_identifier => {
                        const qual_id = as.target.data.qualified_identifier;
                        const has_array_index = try self.containsArrayIndex(qual_id.base);
                        if (has_array_index) {
                            const full_name = try self.buildQualifiedName(as.target);
                            defer self.allocator.free(full_name);
                            const array_name = try self.getBaseIdentifierName(as.target);
                            defer self.allocator.free(array_name);
                            const field_path = try self.allocator.dupe(u8, full_name[array_name.len + 1 ..]);
                            defer self.allocator.free(field_path);
                            const array_index = try self.extractArrayIndex(qual_id.base);
                            try self.generateArrayElementFieldAssignment(array_index, field_path, as.value);
                        } else {
                            const struct_name = try self.getBaseIdentifierName(as.target);
                            defer self.allocator.free(struct_name);
                            const full_name = try self.buildQualifiedName(as.target);
                            defer self.allocator.free(full_name);
                            const field_path = try self.allocator.dupe(u8, full_name[struct_name.len + 1 ..]);
                            defer self.allocator.free(field_path);

                            try self.generateStructFieldAssignment(struct_name, field_path, as.value);
                        }
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
                    try self.putVariable(decl.name, VariableInfo{
                        .value = undefined,
                        .type_ref = c.LLVMVoidTypeInContext(self.context),
                        .type_name = decl.type_name,
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
                        .type_name = decl.type_name,
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
                _ = try bfck.generateBrainfuck(self, bf);
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
        try self.pushScope();
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
        self.popScope();
    }

    fn generateArrayAssignment(self: *CodeGenerator, arr_ass: ast.ArrayAssignment) errors.CodegenError!void {
        const array_name = try self.getBaseIdentifierName(arr_ass.array);
        defer self.allocator.free(array_name);
        const var_info = self.getVariable(array_name) orelse return errors.CodegenError.UndefinedVariable;

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
                const memcpy_func = try self.declareLibcFunction("memcpy");
                const i8_type = c.LLVMInt8TypeInContext(self.context);
                const i8_ptr_type = c.LLVMPointerType(i8_type, 0);
                const size_t_type = utils.libcTypeToLLVM(self, .size_t_type);
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
            const array_size = std.fmt.parseInt(usize, size_str, 10) catch {
                return errors.CodegenError.TypeMismatch;
            };
            const element_type = self.getLLVMType(element_type_name);
            const array_type = c.LLVMArrayType(element_type, @intCast(array_size));
            const alloca = c.LLVMBuildAlloca(self.builder, array_type, decl.name.ptr);

            try self.putVariable(decl.name, VariableInfo{
                .value = alloca,
                .type_ref = array_type,
                .type_name = decl.type_name,
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
                        return errors.CodegenError.TypeMismatch;
                    },
                }
            }
        } else {
            return errors.CodegenError.TypeMismatch;
        }
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
            const enum_name_z_null = try self.allocator.dupeZ(u8, enum_name_z);
            defer self.allocator.free(enum_name_z_null);
            const global_var = c.LLVMAddGlobal(self.module, c.LLVMInt32TypeInContext(self.context), enum_name_z_null.ptr);
            c.LLVMSetInitializer(global_var, enum_const);
            c.LLVMSetLinkage(global_var, c.LLVMExternalLinkage);
            c.LLVMSetGlobalConstant(global_var, 1);
            try self.variables.put(try self.allocator.dupe(u8, enum_name_z), VariableInfo{
                .value = global_var,
                .type_ref = c.LLVMInt32TypeInContext(self.context),
                .type_name = "i32",
            });
            current_value = value + 1;
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
                                return errors.CodegenError.TypeMismatch;
                            }
                            const value = std.fmt.parseInt(u64, num.value, 10) catch {
                                return errors.CodegenError.TypeMismatch;
                            };
                            return c.LLVMConstInt(llvm_type, @as(c_ulonglong, @intCast(value)), 0);
                        } else {
                            const value = std.fmt.parseInt(i64, num.value, 10) catch {
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
            .char_literal => |char| {
                return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(char.value)), 0);
            },
            .identifier => |ident| {
                if (std.mem.eql(u8, ident.name, "true")) {
                    return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 1, 0);
                } else if (std.mem.eql(u8, ident.name, "false")) {
                    return c.LLVMConstInt(c.LLVMInt1TypeInContext(self.context), 0, 0);
                }
                if (self.getVariable(ident.name)) |var_info| {
                    if (c.LLVMGetTypeKind(var_info.type_ref) == c.LLVMVoidTypeKind) {
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
            .qualified_identifier => {
                const qual_id = expr.data.qualified_identifier;
                if (qual_id.base.data == .identifier) {
                    const base_name = qual_id.base.data.identifier.name;
                    const enum_var_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ base_name, qual_id.field });
                    defer self.allocator.free(enum_var_name);
                    if (self.getVariable(enum_var_name)) |enum_var_info| {
                        const load_instr = c.LLVMBuildLoad2(self.builder, enum_var_info.type_ref, enum_var_info.value, "enum_value");
                        const alignment = self.getAlignmentForType(enum_var_info.type_ref);
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
                        const var_info = self.getVariable(base_name) orelse return errors.CodegenError.UndefinedVariable;
                        result_value = var_info.value;
                        result_type = var_info.type_ref;
                        result_type_name = var_info.type_name;
                    },
                    .array_index => {
                        const array_index = qual_id.base.data.array_index;
                        const array_name = try self.getBaseIdentifierName(qual_id.base);
                        defer self.allocator.free(array_name);
                        const array_var_info = self.getVariable(array_name) orelse return errors.CodegenError.UndefinedVariable;
                        const index_value = try self.generateExpression(array_index.index);
                        const element_type = c.LLVMGetElementType(array_var_info.type_ref);
                        var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_value };
                        result_value = c.LLVMBuildGEP2(self.builder, array_var_info.type_ref, array_var_info.value, &indices[0], 2, "array_element_ptr");
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
                    const field_ptr = try self.getStructFieldPointer(struct_type, struct_ptr, field_index);
                    const field_type = c.LLVMStructGetTypeAtIndex(struct_type, field_index);
                    const field_type_kind = c.LLVMGetTypeKind(field_type);
                    if (field_type_kind == c.LLVMArrayTypeKind) {
                        return field_ptr;
                    }
                    const load_instr = c.LLVMBuildLoad2(self.builder, field_type, field_ptr, "struct_field");
                    const alignment = self.getAlignmentForType(field_type);
                    c.LLVMSetAlignment(load_instr, alignment);
                    return load_instr;
                } else if (type_kind == c.LLVMPointerTypeKind) {
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
            .char_literal => |char| {
                return c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), @as(c_ulonglong, @intCast(char.value)), 0);
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
                        } else if (un.operand.data == .qualified_identifier) {
                            return try self.generateExpression(un.operand);
                        } else if (un.operand.data == .array_index) {
                            const arr_idx = un.operand.data.array_index;
                            const array_name = try self.getBaseIdentifierName(arr_idx.array);
                            defer self.allocator.free(array_name);
                            const var_info = self.getVariable(array_name) orelse return errors.CodegenError.UndefinedVariable;
                            const index_value = try self.generateExpression(arr_idx.index);
                            var indices = [_]c.LLVMValueRef{ c.LLVMConstInt(c.LLVMInt32TypeInContext(self.context), 0, 0), index_value };
                            const element_ptr = c.LLVMBuildGEP2(self.builder, var_info.type_ref, var_info.value, &indices[0], 2, "array_element_ptr");
                            return element_ptr;
                        }
                        return errors.CodegenError.TypeMismatch;
                    },
                    '*' => {
                        const operand_val = try self.generateExpression(un.operand);
                        const operand_type = c.LLVMTypeOf(operand_val);
                        if (c.LLVMGetTypeKind(operand_type) == c.LLVMPointerTypeKind) {
                            const pointee_type = c.LLVMGetElementType(operand_type);
                            return c.LLVMBuildLoad2(self.builder, pointee_type, operand_val, "deref");
                        } else {
                            return errors.CodegenError.TypeMismatch;
                        }
                    },
                    'D' => {
                        if (un.operand.data == .identifier) {
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
                            if (self.getVariable(array_name)) |var_info| {
                                const index_value = try self.generateExpression(arr_idx.index);
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
                        } else {
                            return errors.CodegenError.TypeMismatch;
                        }
                    },
                    'I' => {
                        if (un.operand.data == .identifier) {
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
                            if (self.getVariable(array_name)) |var_info| {
                                const index_value = try self.generateExpression(arr_idx.index);
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
                        if (utils.LIBC_FUNCTIONS.get(call.name)) |signature| {
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
                        if (utils.LIBC_FUNCTIONS.get(call.name)) |signature| {
                            if (i < signature.param_types.len) {
                                const expected_type = utils.libcTypeToLLVM(self, signature.param_types[i]);
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
                const array_name = try self.getBaseIdentifierName(arr_idx.array);
                defer self.allocator.free(array_name);
                const var_info = self.getVariable(array_name) orelse return errors.CodegenError.UndefinedVariable;
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
        const filename_z = try self.allocator.dupeZ(u8, filename);
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

    pub fn compileToExecutable(self: *CodeGenerator, output: []const u8, arch: []const u8, link_objects: []const []const u8, keep_ll: bool, optimize: bool) !void {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        const ir_file = try std.fmt.allocPrint(arena_alloc, "{s}.ll", .{output});
        defer arena_alloc.free(ir_file);
        try self.writeToFile(ir_file);

        var clang_args_list = std.ArrayList([]const u8).init(arena_alloc);
        try clang_args_list.appendSlice(&[_][]const u8{ "clang", ir_file, "-o", output, "-lc" });

        if (arch.len != 0) {
            const march_flag: []const u8 = try std.fmt.allocPrint(arena_alloc, "--target={s}", .{arch});
            try clang_args_list.append(march_flag);
        }

        for (link_objects) |obj| {
            try clang_args_list.append(obj);
        }

        if (self.uses_float_modulo) {
            try clang_args_list.append("-lm");
        }

        if (optimize) {
            // Optimize llvm ir
            var opt_args_list = std.ArrayList([]const u8).init(arena_alloc);
            try opt_args_list.appendSlice(&[_][]const u8{
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
                return error.CompilationFailed;
            }

            try clang_args_list.append("-O3"); // Append additional flag to clang (+100% fps)
        }

        var child = std.process.Child.init(clang_args_list.items, arena_alloc);
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        try child.spawn();
        const result = try child.wait();

        if (!keep_ll) {
            std.fs.cwd().deleteFile(ir_file) catch {};
        }

        if (result != .Exited or result.Exited != 0) {
            return error.CompilationFailed;
        }
    }
};
