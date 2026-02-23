const std = @import("std");
const ast = @import("parser/ast.zig");
const errors = @import("errors.zig");
const diagnostics = @import("diagnostics.zig");

const VarInfo = struct {
    is_const: bool,
};

pub const Analyzer = struct {
    allocator: std.mem.Allocator,
    fallback_file_path: []const u8,
    has_errors: bool,
    functions: std.StringHashMap(void),
    enum_types: std.StringHashMap(void),
    enum_values: std.StringHashMap(void),
    globals: std.StringHashMap(VarInfo),
    scopes: std.ArrayList(std.StringHashMap(VarInfo)),
    loop_depth: usize,

    pub fn init(allocator: std.mem.Allocator, fallback_file_path: []const u8) Analyzer {
        return Analyzer{
            .allocator = allocator,
            .fallback_file_path = fallback_file_path,
            .has_errors = false,
            .functions = std.StringHashMap(void).init(allocator),
            .enum_types = std.StringHashMap(void).init(allocator),
            .enum_values = std.StringHashMap(void).init(allocator),
            .globals = std.StringHashMap(VarInfo).init(allocator),
            .scopes = std.ArrayList(std.StringHashMap(VarInfo)){},
            .loop_depth = 0,
        };
    }

    pub fn deinit(self: *Analyzer) void {
        self.functions.deinit();
        self.enum_types.deinit();
        self.enum_values.deinit();
        self.globals.deinit();
        for (self.scopes.items) |*scope| {
            scope.deinit();
        }
        self.scopes.deinit(self.allocator);
    }

    pub fn analyze(self: *Analyzer, root: *ast.Node) errors.SemanticError!void {
        switch (root.data) {
            .program => |program| {
                try self.collectTopLevelSymbols(program);
                try self.analyzeGlobals(program);
                try self.analyzeFunctions(program);
            },
            else => {
                self.reportNodeError(root, "Semantic analysis expects a program root", null);
            },
        }

        if (self.has_errors) {
            return errors.SemanticError.SemanticFailed;
        }
    }

    fn collectTopLevelSymbols(self: *Analyzer, program: ast.Program) errors.SemanticError!void {
        for (program.functions.items) |node| {
            switch (node.data) {
                .function => |func| {
                    try self.functions.put(func.name, {});
                },
                .c_function_decl => |decl| {
                    try self.functions.put(decl.name, {});
                },
                .enum_decl => |enum_decl| {
                    try self.enum_types.put(enum_decl.name, {});
                    for (enum_decl.values.items) |value| {
                        try self.enum_values.put(value.name, {});
                    }
                },
                else => {},
            }
        }

        for (program.globals.items) |node| {
            if (node.data == .var_decl) {
                const decl = node.data.var_decl;
                if (self.globals.contains(decl.name)) {
                    self.reportNodeErrorFmt(node, "Redeclared global variable '{s}'", .{decl.name}, "Global names must be unique");
                } else {
                    try self.globals.put(decl.name, .{ .is_const = decl.is_const });
                }
            }
        }
    }

    fn analyzeGlobals(self: *Analyzer, program: ast.Program) errors.SemanticError!void {
        for (program.globals.items) |node| {
            if (node.data == .var_decl) {
                const decl = node.data.var_decl;
                if (decl.initializer) |initializer_node| {
                    try self.analyzeExpression(initializer_node);
                }
            }
        }
    }

    fn analyzeFunctions(self: *Analyzer, program: ast.Program) errors.SemanticError!void {
        for (program.functions.items) |node| {
            if (node.data != .function) continue;

            const func = node.data.function;
            self.loop_depth = 0;

            var labels = std.StringHashMap(void).init(self.allocator);
            defer labels.deinit();

            try self.collectLabels(func.body.items, &labels);

            try self.pushScope();
            defer self.popScope();

            for (func.parameters.items) |param| {
                try self.declareVariable(node, param.name, false);
            }

            if (func.guard) |guard| {
                try self.analyzeExpression(guard);
            }

            try self.analyzeStatementList(func.body.items, &labels);
        }
    }

    fn collectLabels(self: *Analyzer, statements: []const *ast.Node, labels: *std.StringHashMap(void)) errors.SemanticError!void {
        for (statements) |stmt| {
            try self.collectLabelsFromNode(stmt, labels);
        }
    }

    fn collectLabelsFromNode(self: *Analyzer, node: *ast.Node, labels: *std.StringHashMap(void)) errors.SemanticError!void {
        switch (node.data) {
            .label_stmt => |label_stmt| {
                if (labels.contains(label_stmt.label)) {
                    self.reportNodeErrorFmt(node, "Duplicate label '{s}'", .{label_stmt.label}, "Label names must be unique inside a function");
                } else {
                    try labels.put(label_stmt.label, {});
                }
            },
            .if_stmt => |if_stmt| {
                try self.collectLabels(if_stmt.then_body.items, labels);
                if (if_stmt.else_body) |else_body| {
                    try self.collectLabels(else_body.items, labels);
                }
            },
            .for_stmt => |for_stmt| {
                try self.collectLabels(for_stmt.body.items, labels);
            },
            .c_for_stmt => |c_for_stmt| {
                try self.collectLabels(c_for_stmt.body.items, labels);
            },
            .match_stmt => |match_stmt| {
                for (match_stmt.cases.items) |case| {
                    try self.collectLabels(case.body.items, labels);
                }
            },
            .expression_block => |block| {
                try self.collectLabels(block.statements.items, labels);
            },
            else => {},
        }
    }

    fn pushScope(self: *Analyzer) errors.SemanticError!void {
        try self.scopes.append(self.allocator, std.StringHashMap(VarInfo).init(self.allocator));
    }

    fn popScope(self: *Analyzer) void {
        if (self.scopes.items.len == 0) return;
        var scope = self.scopes.pop().?;
        scope.deinit();
    }

    fn declareVariable(self: *Analyzer, node: *ast.Node, name: []const u8, is_const: bool) errors.SemanticError!void {
        if (self.scopes.items.len == 0) {
            self.reportNodeError(node, "Internal semantic scope error", null);
            return;
        }

        var scope = &self.scopes.items[self.scopes.items.len - 1];
        if (scope.contains(name)) {
            self.reportNodeErrorFmt(node, "Redeclared variable '{s}'", .{name}, "A variable with this name already exists in this scope");
            return;
        }

        try scope.put(name, .{ .is_const = is_const });
    }

    fn lookupVariable(self: *Analyzer, name: []const u8) ?VarInfo {
        var i = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].get(name)) |info| {
                return info;
            }
        }

        if (self.globals.get(name)) |info| {
            return info;
        }

        return null;
    }

    fn analyzeStatementList(self: *Analyzer, statements: []const *ast.Node, labels: *const std.StringHashMap(void)) errors.SemanticError!void {
        for (statements) |stmt| {
            try self.analyzeStatement(stmt, labels);
        }
    }

    fn analyzeStatement(self: *Analyzer, stmt: *ast.Node, labels: *const std.StringHashMap(void)) errors.SemanticError!void {
        switch (stmt.data) {
            .var_decl => |decl| {
                if (decl.initializer) |initializer_node| {
                    try self.analyzeExpression(initializer_node);
                }
                try self.declareVariable(stmt, decl.name, decl.is_const);
            },
            .assignment => |as| {
                try self.analyzeAssignmentTarget(as.target, true);
                try self.analyzeExpression(as.value);
            },
            .compound_assignment => |as| {
                try self.analyzeAssignmentTarget(as.target, true);
                try self.analyzeExpression(as.value);
            },
            .function_call => |call| {
                try self.analyzeFunctionCall(stmt, call);
            },
            .method_call => |method| {
                try self.analyzeExpression(method.object);
                for (method.args.items) |arg| {
                    try self.analyzeExpression(arg);
                }
            },
            .return_stmt => |ret| {
                if (ret.expression) |expr| {
                    try self.analyzeExpression(expr);
                }
            },
            .if_stmt => |if_stmt| {
                try self.analyzeExpression(if_stmt.condition);

                try self.pushScope();
                defer self.popScope();
                try self.analyzeStatementList(if_stmt.then_body.items, labels);

                if (if_stmt.else_body) |else_body| {
                    try self.pushScope();
                    defer self.popScope();
                    try self.analyzeStatementList(else_body.items, labels);
                }
            },
            .for_stmt => |for_stmt| {
                if (for_stmt.condition) |condition| {
                    try self.analyzeExpression(condition);
                }

                self.loop_depth += 1;
                defer self.loop_depth -= 1;

                try self.pushScope();
                defer self.popScope();
                try self.analyzeStatementList(for_stmt.body.items, labels);
            },
            .c_for_stmt => |c_for_stmt| {
                try self.pushScope();
                defer self.popScope();

                if (c_for_stmt.init) |init_stmt| {
                    try self.analyzeStatement(init_stmt, labels);
                }
                if (c_for_stmt.condition) |condition| {
                    try self.analyzeExpression(condition);
                }

                self.loop_depth += 1;
                defer self.loop_depth -= 1;
                try self.analyzeStatementList(c_for_stmt.body.items, labels);

                if (c_for_stmt.increment) |increment| {
                    try self.analyzeStatement(increment, labels);
                }
            },
            .break_stmt => {
                if (self.loop_depth == 0) {
                    self.reportNodeError(stmt, "'break' used outside of a loop", "Use 'break' only inside for/c_for blocks");
                }
            },
            .continue_stmt => {
                if (self.loop_depth == 0) {
                    self.reportNodeError(stmt, "'continue' used outside of a loop", "Use 'continue' only inside for/c_for blocks");
                }
            },
            .goto_stmt => |goto_stmt| {
                if (!labels.contains(goto_stmt.label)) {
                    self.reportNodeErrorFmt(stmt, "Undefined label '{s}'", .{goto_stmt.label}, "Declare the label before or after the goto target");
                }
            },
            .label_stmt => {},
            .array_assignment => |arr_ass| {
                try self.analyzeExpression(arr_ass.array);
                try self.analyzeExpression(arr_ass.index);
                try self.analyzeExpression(arr_ass.value);
            },
            .array_compound_assignment => |arr_cass| {
                try self.analyzeExpression(arr_cass.array);
                try self.analyzeExpression(arr_cass.index);
                try self.analyzeExpression(arr_cass.value);
            },
            .simd_assignment => |simd_ass| {
                try self.analyzeExpression(simd_ass.simd);
                try self.analyzeExpression(simd_ass.index);
                try self.analyzeExpression(simd_ass.value);
            },
            .simd_compound_assignment => |simd_cass| {
                try self.analyzeExpression(simd_cass.simd);
                try self.analyzeExpression(simd_cass.index);
                try self.analyzeExpression(simd_cass.value);
            },
            .match_stmt => |match_stmt| {
                try self.analyzeExpression(match_stmt.condition);

                for (match_stmt.cases.items) |case| {
                    for (case.values.items) |value_node| {
                        try self.analyzeExpression(value_node);
                    }

                    try self.pushScope();
                    defer self.popScope();
                    try self.analyzeStatementList(case.body.items, labels);
                }
            },
            .c_function_decl, .use_stmt, .enum_decl, .struct_decl => {},
            else => {
                try self.analyzeExpression(stmt);
            },
        }
    }

    fn analyzeAssignmentTarget(self: *Analyzer, target: *ast.Node, check_const: bool) errors.SemanticError!void {
        switch (target.data) {
            .identifier => |ident| {
                if (self.lookupVariable(ident.name)) |var_info| {
                    if (check_const and var_info.is_const) {
                        self.reportNodeErrorFmt(target, "Cannot reassign const variable '{s}'", .{ident.name}, "Variable is declared as const");
                    }
                } else {
                    self.reportNodeErrorFmt(target, "Undefined variable '{s}'", .{ident.name}, "Variable is not declared in this scope");
                }
            },
            .qualified_identifier => |qual| {
                if (qual.base.data == .identifier) {
                    const base_name = qual.base.data.identifier.name;
                    if (self.enum_types.contains(base_name)) {
                        self.reportNodeError(target, "Cannot assign to enum value", "Enum values are immutable");
                        return;
                    }
                }
                try self.analyzeExpression(qual.base);
            },
            .array_index => |arr_idx| {
                try self.analyzeExpression(arr_idx.array);
                try self.analyzeExpression(arr_idx.index);
            },
            .simd_index => |simd_idx| {
                try self.analyzeExpression(simd_idx.simd);
                try self.analyzeExpression(simd_idx.index);
            },
            .unary_op => |un| {
                try self.analyzeExpression(un.operand);
            },
            else => {
                try self.analyzeExpression(target);
            },
        }
    }

    fn analyzeFunctionCall(self: *Analyzer, node: *ast.Node, call: ast.FunctionCall) errors.SemanticError!void {
        if (!call.is_libc and !self.functions.contains(call.name) and self.lookupVariable(call.name) == null) {
            self.reportNodeErrorFmt(node, "Undefined function '{s}'", .{call.name}, "Declare it or import its module with use (maybe you forgot to import it?)");
        }

        for (call.args.items) |arg| {
            try self.analyzeExpression(arg);
        }
    }

    fn analyzeExpression(self: *Analyzer, expr: *ast.Node) errors.SemanticError!void {
        switch (expr.data) {
            .identifier => |ident| {
                if (self.isBuiltinIdentifier(ident.name)) return;
                if (self.enum_values.contains(ident.name)) return;
                if (self.lookupVariable(ident.name) != null) return;
                if (self.functions.contains(ident.name)) return;

                self.reportNodeErrorFmt(expr, "Undefined variable '{s}'", .{ident.name}, "Variable is not declared in this scope");
            },
            .qualified_identifier => |qual| {
                if (qual.base.data == .identifier and self.enum_types.contains(qual.base.data.identifier.name)) {
                    return;
                }
                try self.analyzeExpression(qual.base);
            },
            .function_call => |call| {
                try self.analyzeFunctionCall(expr, call);
            },
            .method_call => |method| {
                try self.analyzeExpression(method.object);
                for (method.args.items) |arg| {
                    try self.analyzeExpression(arg);
                }
            },
            .unary_op => |un| {
                try self.analyzeExpression(un.operand);
            },
            .binary_op => |bin_op| {
                try self.analyzeExpression(bin_op.lhs);
                try self.analyzeExpression(bin_op.rhs);
            },
            .comparison => |comparison| {
                try self.analyzeExpression(comparison.lhs);
                try self.analyzeExpression(comparison.rhs);
            },
            .cast => |cast_expr| {
                try self.analyzeExpression(cast_expr.expr);
            },
            .expression_block => |block| {
                try self.pushScope();
                defer self.popScope();

                var block_labels = std.StringHashMap(void).init(self.allocator);
                defer block_labels.deinit();
                try self.collectLabels(block.statements.items, &block_labels);

                for (block.statements.items) |stmt| {
                    if (stmt.data == .return_stmt) {
                        self.reportNodeError(stmt, "'return' is not allowed inside expression block", "Use the final expression as the block result");
                    }
                    try self.analyzeStatement(stmt, &block_labels);
                }
                try self.analyzeExpression(block.result);
            },
            .array_initializer => |arr_init| {
                for (arr_init.elements.items) |element| {
                    try self.analyzeExpression(element);
                }
            },
            .simd_initializer => |simd_init| {
                for (simd_init.elements.items) |element| {
                    try self.analyzeExpression(element);
                }
            },
            .array_index => |arr_idx| {
                try self.analyzeExpression(arr_idx.array);
                try self.analyzeExpression(arr_idx.index);
            },
            .simd_index => |simd_idx| {
                try self.analyzeExpression(simd_idx.simd);
                try self.analyzeExpression(simd_idx.index);
            },
            .struct_initializer => |struct_init| {
                for (struct_init.field_values.items) |field_value| {
                    try self.analyzeExpression(field_value.value);
                }
            },
            .number_literal,
            .float_literal,
            .char_literal,
            .string_literal,
            .bool_literal,
            .null_literal,
            .brainfuck,
            .break_stmt,
            .continue_stmt,
            .goto_stmt,
            .label_stmt,
            .c_function_decl,
            .use_stmt,
            .enum_decl,
            .struct_decl,
            .program,
            .function,
            .match_stmt,
            .return_stmt,
            .if_stmt,
            .for_stmt,
            .c_for_stmt,
            .assignment,
            .compound_assignment,
            .var_decl,
            .array_assignment,
            .array_compound_assignment,
            .simd_assignment,
            .simd_compound_assignment,
            .simd_method_call,
            => {},
        }
    }

    fn isBuiltinIdentifier(self: *Analyzer, name: []const u8) bool {
        _ = self;
        return std.mem.eql(u8, name, "true") or std.mem.eql(u8, name, "false");
    }

    fn reportNodeError(self: *Analyzer, node: *ast.Node, message: []const u8, hint: ?[]const u8) void {
        self.has_errors = true;
        diagnostics.printDiagnostic(self.allocator, .{
            .file_path = self.fallback_file_path,
            .line = if (node.line == 0) 1 else node.line,
            .column = if (node.column == 0) 1 else node.column,
            .message = message,
            .severity = .Error,
            .hint = hint,
            .token_text = tokenTextForNode(node),
        });
    }

    fn reportNodeErrorFmt(self: *Analyzer, node: *ast.Node, comptime fmt: []const u8, args: anytype, hint: ?[]const u8) void {
        const msg = std.fmt.allocPrint(self.allocator, fmt, args) catch {
            self.reportNodeError(node, "Semantic error", hint);
            return;
        };
        defer self.allocator.free(msg);
        self.reportNodeError(node, msg, hint);
    }
};

fn tokenTextForNode(node: *ast.Node) ?[]const u8 {
    return switch (node.data) {
        .identifier => |ident| ident.name,
        .qualified_identifier => |qual| qual.field,
        .var_decl => |decl| decl.name,
        .function => |func| func.name,
        .function_call => |call| call.name,
        .method_call => |call| call.method_name,
        .c_function_decl => |decl| decl.name,
        .enum_decl => |decl| decl.name,
        .struct_decl => |decl| decl.name,
        .struct_initializer => |init| init.struct_name,
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
        else => null,
    };
}

pub fn analyzeProgram(allocator: std.mem.Allocator, root: *ast.Node, fallback_file_path: []const u8) errors.SemanticError!void {
    var analyzer = Analyzer.init(allocator, fallback_file_path);
    defer analyzer.deinit();
    try analyzer.analyze(root);
}
