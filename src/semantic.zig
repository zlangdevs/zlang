const std = @import("std");
const ast = @import("parser/ast.zig");
const errors = @import("errors.zig");
const diagnostics = @import("diagnostics.zig");

const VarInfo = struct {
    is_const: bool,
};

const FunctionFlowInfo = struct {
    send_errors: std.ArrayList([]const u8),
    solicit_errors: std.ArrayList([]const u8),

    fn init() FunctionFlowInfo {
        return .{
            .send_errors = std.ArrayList([]const u8){},
            .solicit_errors = std.ArrayList([]const u8){},
        };
    }

    fn deinit(self: *FunctionFlowInfo, allocator: std.mem.Allocator) void {
        self.send_errors.deinit(allocator);
        self.solicit_errors.deinit(allocator);
    }
};

pub const Analyzer = struct {
    allocator: std.mem.Allocator,
    fallback_file_path: []const u8,
    has_errors: bool,
    functions: std.StringHashMap(void),
    enum_types: std.StringHashMap(void),
    enum_values: std.StringHashMap(void),
    error_codes: std.StringHashMap(i32),
<<<<<<< HEAD
    function_errors: std.StringHashMap(std.ArrayList([]const u8)),
=======
    function_flows: std.StringHashMap(FunctionFlowInfo),
>>>>>>> b9d8f8f (solicit implemented)
    globals: std.StringHashMap(VarInfo),
    scopes: std.ArrayList(std.StringHashMap(VarInfo)),
    loop_depth: usize,
    current_function_name: ?[]const u8,
<<<<<<< HEAD
<<<<<<< HEAD
    handler_scope_floor: ?usize,
=======
>>>>>>> 6d63e72 (smart byref values in send callbacks)
=======
    allow_unknown_identifiers: bool,
>>>>>>> b9d8f8f (solicit implemented)

    pub fn init(allocator: std.mem.Allocator, fallback_file_path: []const u8) Analyzer {
        return Analyzer{
            .allocator = allocator,
            .fallback_file_path = fallback_file_path,
            .has_errors = false,
            .functions = std.StringHashMap(void).init(allocator),
            .enum_types = std.StringHashMap(void).init(allocator),
            .enum_values = std.StringHashMap(void).init(allocator),
            .error_codes = std.StringHashMap(i32).init(allocator),
<<<<<<< HEAD
            .function_errors = std.StringHashMap(std.ArrayList([]const u8)).init(allocator),
=======
            .function_flows = std.StringHashMap(FunctionFlowInfo).init(allocator),
>>>>>>> b9d8f8f (solicit implemented)
            .globals = std.StringHashMap(VarInfo).init(allocator),
            .scopes = std.ArrayList(std.StringHashMap(VarInfo)){},
            .loop_depth = 0,
            .current_function_name = null,
<<<<<<< HEAD
<<<<<<< HEAD
            .handler_scope_floor = null,
=======
>>>>>>> 6d63e72 (smart byref values in send callbacks)
=======
            .allow_unknown_identifiers = false,
>>>>>>> b9d8f8f (solicit implemented)
        };
    }

    pub fn deinit(self: *Analyzer) void {
        self.functions.deinit();
        self.enum_types.deinit();
        self.enum_values.deinit();
        self.error_codes.deinit();
<<<<<<< HEAD
        var function_errors_it = self.function_errors.iterator();
        while (function_errors_it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.function_errors.deinit();
=======
        var function_flows_it = self.function_flows.iterator();
        while (function_flows_it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.function_flows.deinit();
>>>>>>> b9d8f8f (solicit implemented)
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
        try self.error_codes.put("ErrorGuard", 0);

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
                .error_decl => |error_decl| {
                    if (self.error_codes.contains(error_decl.name)) {
                        self.reportNodeErrorFmt(node, "Redeclared error '{s}'", .{error_decl.name}, "Error names must be unique");
                    } else {
                        try self.error_codes.put(error_decl.name, 0);
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
<<<<<<< HEAD
            var list = std.ArrayList([]const u8){};
            try self.collectFunctionSends(node.data.function.body.items, &list);
            try self.function_errors.put(node.data.function.name, list);
=======
            var flow_info = FunctionFlowInfo.init();
            try self.collectFunctionFlows(node.data.function.body.items, &flow_info.send_errors, &flow_info.solicit_errors);
            try self.function_flows.put(node.data.function.name, flow_info);
>>>>>>> b9d8f8f (solicit implemented)
        }

        for (program.functions.items) |node| {
            if (node.data != .function) continue;

            const func = node.data.function;
            self.loop_depth = 0;
            self.current_function_name = func.name;
            defer self.current_function_name = null;

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
            .handled_call_stmt => |handled| {
                for (handled.handlers.items) |handler| {
                    try self.collectLabels(handler.body.items, labels);
                }
            },
            else => {},
        }
    }

    fn appendUniqueErrorName(self: *Analyzer, list: *std.ArrayList([]const u8), name: []const u8) errors.SemanticError!void {
        for (list.items) |existing| {
            if (std.mem.eql(u8, existing, name)) return;
        }
        try list.append(self.allocator, name);
    }

<<<<<<< HEAD
    fn collectFunctionSends(self: *Analyzer, statements: []const *ast.Node, list: *std.ArrayList([]const u8)) errors.SemanticError!void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .send_stmt => |send_stmt| {
                    try self.appendUniqueErrorName(list, send_stmt.error_name);
                },
                .if_stmt => |if_stmt| {
                    try self.collectFunctionSends(if_stmt.then_body.items, list);
                    if (if_stmt.else_body) |else_body| {
                        try self.collectFunctionSends(else_body.items, list);
                    }
                },
                .for_stmt => |for_stmt| {
                    try self.collectFunctionSends(for_stmt.body.items, list);
                },
                .c_for_stmt => |c_for| {
                    try self.collectFunctionSends(c_for.body.items, list);
                },
                .match_stmt => |match_stmt| {
                    for (match_stmt.cases.items) |case| {
                        try self.collectFunctionSends(case.body.items, list);
                    }
                },
                .expression_block => |block| {
                    try self.collectFunctionSends(block.statements.items, list);
                },
                .handled_call_stmt => |handled| {
                    for (handled.handlers.items) |handler| {
                        try self.collectFunctionSends(handler.body.items, list);
=======
    fn errorNameInList(list: []const []const u8, name: []const u8) bool {
        for (list) |entry| {
            if (std.mem.eql(u8, entry, name)) return true;
        }
        return false;
    }

    fn collectFunctionFlows(self: *Analyzer, statements: []const *ast.Node, send_list: *std.ArrayList([]const u8), solicit_list: *std.ArrayList([]const u8)) errors.SemanticError!void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .send_stmt => |send_stmt| {
                    try self.appendUniqueErrorName(send_list, send_stmt.error_name);
                },
                .solicit_stmt => |solicit_stmt| {
                    try self.appendUniqueErrorName(solicit_list, solicit_stmt.error_name);
                },
                .if_stmt => |if_stmt| {
                    try self.collectFunctionFlows(if_stmt.then_body.items, send_list, solicit_list);
                    if (if_stmt.else_body) |else_body| {
                        try self.collectFunctionFlows(else_body.items, send_list, solicit_list);
                    }
                },
                .for_stmt => |for_stmt| {
                    try self.collectFunctionFlows(for_stmt.body.items, send_list, solicit_list);
                },
                .c_for_stmt => |c_for| {
                    try self.collectFunctionFlows(c_for.body.items, send_list, solicit_list);
                },
                .match_stmt => |match_stmt| {
                    for (match_stmt.cases.items) |case| {
                        try self.collectFunctionFlows(case.body.items, send_list, solicit_list);
                    }
                },
                .expression_block => |block| {
                    try self.collectFunctionFlows(block.statements.items, send_list, solicit_list);
                },
                .handled_call_stmt => |handled| {
                    for (handled.handlers.items) |handler| {
                        try self.collectFunctionFlows(handler.body.items, send_list, solicit_list);
>>>>>>> b9d8f8f (solicit implemented)
                    }
                },
                else => {},
            }
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
        const floor = self.handler_scope_floor orelse 0;
        var i = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (i < floor) break;
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
                try self.analyzeFunctionCall(stmt, call, false);
            },
            .handled_call_stmt => |handled| {
<<<<<<< HEAD
<<<<<<< HEAD
                if (handled.call.data != .function_call) {
                    self.reportNodeError(stmt, "Invalid handled call target", "Only function calls can use on-handlers");
                } else {
                    try self.analyzeFunctionCall(handled.call, handled.call.data.function_call, true);
                }

                const call_name = if (handled.call.data == .function_call)
                    handled.call.data.function_call.name
                else
                    "";

                var has_catch_all = false;
                for (handled.handlers.items) |handler| {
                    if (handler.error_name) |error_name| {
                        if (!self.error_codes.contains(error_name)) {
                            self.reportNodeErrorFmt(stmt, "Unknown error '{s}' in handler", .{error_name}, "Declare the error before using it in on-handler");
                        }
                    } else {
                        has_catch_all = true;
                    }

                    try self.pushScope();
                    const saved_floor = self.handler_scope_floor;
                    self.handler_scope_floor = self.scopes.items.len - 1;
                    defer {
                        self.handler_scope_floor = saved_floor;
                        self.popScope();
                    }
                    try self.analyzeStatementList(handler.body.items, labels);
                }

                if (self.function_errors.get(call_name)) |sent_errors| {
                    for (handled.handlers.items) |handler| {
                        if (handler.error_name) |error_name| {
                            var matched = false;
                            for (sent_errors.items) |sent_name| {
                                if (std.mem.eql(u8, sent_name, error_name)) {
                                    matched = true;
                                    break;
                                }
                            }
                            if (!matched) {
                                self.reportNodeWarningFmt(stmt, "Handler for '{s}' is ignored", .{error_name}, "Called function never sends this error");
                            }
                        }
                    }

                    if (!has_catch_all) {
                        for (sent_errors.items) |sent_name| {
                            var handled_error = false;
                            for (handled.handlers.items) |handler| {
                                if (handler.error_name) |error_name| {
                                    if (std.mem.eql(u8, error_name, sent_name)) {
                                        handled_error = true;
                                        break;
                                    }
                                }
                            }
                            if (!handled_error) {
                                self.reportNodeWarningFmt(stmt, "Unhandled error '{s}' from function call", .{sent_name}, "Add an on-handler or on _ block");
                            }
                        }
                    }
                }
=======
                try self.analyzeHandledCall(stmt, handled, labels);
>>>>>>> 6d63e72 (smart byref values in send callbacks)
=======
                try self.analyzeHandledCall(stmt, handled, labels);
>>>>>>> b9d8f8f (solicit implemented)
            },
            .method_call => |method| {
                try self.analyzeExpression(method.object);
                for (method.args.items) |arg| {
                    try self.analyzeExpression(arg);
                }
            },
            .send_stmt => |send_stmt| {
                if (!self.error_codes.contains(send_stmt.error_name)) {
                    self.reportNodeErrorFmt(stmt, "Unknown error '{s}'", .{send_stmt.error_name}, "Declare the error before sending it");
                }
            },
<<<<<<< HEAD
=======
            .solicit_stmt => |solicit_stmt| {
                if (!self.error_codes.contains(solicit_stmt.error_name)) {
                    self.reportNodeErrorFmt(stmt, "Unknown error '{s}'", .{solicit_stmt.error_name}, "Declare the error before soliciting it");
                }
            },
>>>>>>> b9d8f8f (solicit implemented)
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
            .c_function_decl, .use_stmt, .enum_decl, .struct_decl, .error_decl => {},
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
                    if (self.allow_unknown_identifiers) return;
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

<<<<<<< HEAD
<<<<<<< HEAD
=======
=======
>>>>>>> b9d8f8f (solicit implemented)
    fn analyzeHandledCall(self: *Analyzer, node: *ast.Node, handled: ast.HandledCallStmt, labels: *const std.StringHashMap(void)) errors.SemanticError!void {
        if (handled.call.data != .function_call) {
            self.reportNodeError(node, "Invalid handled call target", "Only function calls can use on-handlers");
        } else {
            try self.analyzeFunctionCall(handled.call, handled.call.data.function_call, true);
        }

        const call_name = if (handled.call.data == .function_call)
            handled.call.data.function_call.name
        else
            "";

<<<<<<< HEAD
        var has_catch_all = false;
=======
        var has_send_catch_all = false;
        var has_solicit_catch_all = false;
>>>>>>> b9d8f8f (solicit implemented)
        for (handled.handlers.items) |handler| {
            if (handler.error_name) |error_name| {
                if (!self.error_codes.contains(error_name)) {
                    self.reportNodeErrorFmt(node, "Unknown error '{s}' in handler", .{error_name}, "Declare the error before using it in on-handler");
                }
            } else {
<<<<<<< HEAD
                has_catch_all = true;
            }

            try self.pushScope();
            defer self.popScope();
            try self.analyzeStatementList(handler.body.items, labels);
        }

        if (self.function_errors.get(call_name)) |sent_errors| {
            for (handled.handlers.items) |handler| {
                if (handler.error_name) |error_name| {
                    var matched = false;
                    for (sent_errors.items) |sent_name| {
                        if (std.mem.eql(u8, sent_name, error_name)) {
                            matched = true;
                            break;
                        }
                    }
                    if (!matched) {
                        self.reportNodeWarningFmt(node, "Handler for '{s}' is ignored", .{error_name}, "Called function never sends this error");
=======
                switch (handler.kind) {
                    .send => has_send_catch_all = true,
                    .solicit => has_solicit_catch_all = true,
                }
            }

            try self.pushScope();
            const saved_allow_unknown = self.allow_unknown_identifiers;
            if (handler.kind == .solicit) {
                self.allow_unknown_identifiers = true;
            }
            defer {
                self.allow_unknown_identifiers = saved_allow_unknown;
                self.popScope();
            }
            try self.analyzeStatementList(handler.body.items, labels);
        }

        if (self.function_flows.get(call_name)) |flow| {
            for (handled.handlers.items) |handler| {
                if (handler.error_name) |error_name| {
                    const relevant_list = switch (handler.kind) {
                        .send => flow.send_errors.items,
                        .solicit => flow.solicit_errors.items,
                    };
                    const matched = errorNameInList(relevant_list, error_name);
                    if (!matched) {
                        const hint = switch (handler.kind) {
                            .send => "Called function never sends this error",
                            .solicit => "Called function never solicits this error",
                        };
                        self.reportNodeWarningFmt(node, "Handler for '{s}' is ignored", .{error_name}, hint);
>>>>>>> b9d8f8f (solicit implemented)
                    }
                }
            }

<<<<<<< HEAD
            if (!has_catch_all) {
                for (sent_errors.items) |sent_name| {
                    var handled_error = false;
                    for (handled.handlers.items) |handler| {
=======
            if (!has_send_catch_all) {
                for (flow.send_errors.items) |sent_name| {
                    var handled_error = false;
                    for (handled.handlers.items) |handler| {
                        if (handler.kind != .send) continue;
>>>>>>> b9d8f8f (solicit implemented)
                        if (handler.error_name) |error_name| {
                            if (std.mem.eql(u8, error_name, sent_name)) {
                                handled_error = true;
                                break;
                            }
                        }
                    }
                    if (!handled_error) {
                        self.reportNodeWarningFmt(node, "Unhandled error '{s}' from function call", .{sent_name}, "Add an on-handler or on _ block");
                    }
                }
            }
<<<<<<< HEAD
        }
    }

>>>>>>> 6d63e72 (smart byref values in send callbacks)
=======

            if (!has_solicit_catch_all) {
                for (flow.solicit_errors.items) |solicit_name| {
                    var handled_solicit = false;
                    for (handled.handlers.items) |handler| {
                        if (handler.kind != .solicit) continue;
                        if (handler.error_name) |error_name| {
                            if (std.mem.eql(u8, error_name, solicit_name)) {
                                handled_solicit = true;
                                break;
                            }
                        }
                    }
                    if (!handled_solicit) {
                        self.reportNodeWarningFmt(node, "Unhandled solicit-error '{s}' from function call", .{solicit_name}, "Add an on solicit-handler or on solicit _ block");
                    }
                }
            }
        }
    }

>>>>>>> b9d8f8f (solicit implemented)
    fn analyzeFunctionCall(self: *Analyzer, node: *ast.Node, call: ast.FunctionCall, suppress_unhandled_warning: bool) errors.SemanticError!void {
        if (!call.is_libc and !self.functions.contains(call.name) and self.lookupVariable(call.name) == null) {
            self.reportNodeErrorFmt(node, "Undefined function '{s}'", .{call.name}, "Declare it or import its module with use (maybe you forgot to import it?)");
        }

        if (!suppress_unhandled_warning) {
<<<<<<< HEAD
            if (self.function_errors.get(call.name)) |sent_errors| {
                if (sent_errors.items.len > 0) {
                    self.reportNodeWarningFmt(node, "Unhandled errors from function '{s}'", .{call.name}, "Use on-handlers to handle possible errors");
                }
=======
            if (self.function_flows.get(call.name)) |flow| {
                if (flow.send_errors.items.len > 0) {
                    self.reportNodeWarningFmt(node, "Unhandled errors from function '{s}'", .{call.name}, "Use on-handlers to handle possible errors");
                }
                if (flow.solicit_errors.items.len > 0) {
                    self.reportNodeWarningFmt(node, "Unhandled solicit-errors from function '{s}'", .{call.name}, "Use on solicit-handlers to handle solicit paths");
                }
>>>>>>> b9d8f8f (solicit implemented)
            }
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
                if (self.error_codes.contains(ident.name)) return;
                if (self.lookupVariable(ident.name) != null) return;
                if (self.functions.contains(ident.name)) return;
                if (self.allow_unknown_identifiers) return;

                self.reportNodeErrorFmt(expr, "Undefined variable '{s}'", .{ident.name}, "Variable is not declared in this scope");
            },
            .qualified_identifier => |qual| {
                if (qual.base.data == .identifier and self.enum_types.contains(qual.base.data.identifier.name)) {
                    return;
                }
                try self.analyzeExpression(qual.base);
            },
            .function_call => |call| {
                try self.analyzeFunctionCall(expr, call, false);
<<<<<<< HEAD
<<<<<<< HEAD
=======
=======
>>>>>>> b9d8f8f (solicit implemented)
            },
            .handled_call_stmt => |handled| {
                var empty_labels = std.StringHashMap(void).init(self.allocator);
                defer empty_labels.deinit();
                try self.analyzeHandledCall(expr, handled, &empty_labels);
<<<<<<< HEAD
>>>>>>> 6d63e72 (smart byref values in send callbacks)
=======
>>>>>>> b9d8f8f (solicit implemented)
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
            .error_decl,
            .send_stmt,
<<<<<<< HEAD
<<<<<<< HEAD
            .handled_call_stmt,
=======
>>>>>>> 6d63e72 (smart byref values in send callbacks)
=======
            .solicit_stmt,
>>>>>>> b9d8f8f (solicit implemented)
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

    fn reportNodeWarning(self: *Analyzer, node: *ast.Node, message: []const u8, hint: ?[]const u8) void {
        diagnostics.printDiagnostic(self.allocator, .{
            .file_path = self.fallback_file_path,
            .line = if (node.line == 0) 1 else node.line,
            .column = if (node.column == 0) 1 else node.column,
            .message = message,
            .severity = .Warning,
            .hint = hint,
            .token_text = tokenTextForNode(node),
        });
    }

    fn reportNodeWarningFmt(self: *Analyzer, node: *ast.Node, comptime fmt: []const u8, args: anytype, hint: ?[]const u8) void {
        const msg = std.fmt.allocPrint(self.allocator, fmt, args) catch {
            self.reportNodeWarning(node, "Semantic warning", hint);
            return;
        };
        defer self.allocator.free(msg);
        self.reportNodeWarning(node, msg, hint);
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
        .error_decl => |err| err.name,
        .send_stmt => |send_stmt| send_stmt.error_name,
<<<<<<< HEAD
=======
        .solicit_stmt => |solicit_stmt| solicit_stmt.error_name,
>>>>>>> b9d8f8f (solicit implemented)
        .handled_call_stmt => |handled| tokenTextForNode(handled.call),
        else => null,
    };
}

pub fn analyzeProgram(allocator: std.mem.Allocator, root: *ast.Node, fallback_file_path: []const u8) errors.SemanticError!void {
    var analyzer = Analyzer.init(allocator, fallback_file_path);
    defer analyzer.deinit();
    try analyzer.analyze(root);
}
