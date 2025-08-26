const std = @import("std");
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");

pub const ControlFlowAnalyzer = struct {
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator) ControlFlowAnalyzer {
        return ControlFlowAnalyzer{
            .allocator = allocator,
        };
    }
    
    pub fn analyzeFunction(self: *ControlFlowAnalyzer, func: *ast.Node) errors.CodegenError!bool {
        switch (func.data) {
            .function => |f| {
                if (std.mem.eql(u8, f.return_type, "void")) {
                    return true;
                }
                return self.analyzeStatementList(f.body.items);
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }
    
    fn analyzeStatementList(self: *ControlFlowAnalyzer, statements: []const *ast.Node) errors.CodegenError!bool {
        if (statements.len == 0) return false;
        var i: usize = 0;
        while (i < statements.len) : (i += 1) {
            const stmt = statements[i];
            if (self.isReturnStatement(stmt)) {
                if (i == statements.len - 1) {
                    return true;
                }
                var has_unreachable_code = false;
                for (i + 1..statements.len) |j| {
                    const next_stmt = statements[j];
                    if (!self.isUnconditionalTerminator(next_stmt) and !self.isReturnStatement(next_stmt)) {
                        has_unreachable_code = true;
                        break;
                    }
                }
                if (has_unreachable_code) {
                    std.debug.print("Warning: Unreachable code detected after return statement\n", .{});
                }
            }
            
            if (self.isIfStatement(stmt)) {
                const all_paths_return = try self.analyzeIfStatement(stmt);
                if (all_paths_return and i == statements.len - 1) {
                    return true;
                }
            }
            if (self.isUnconditionalTerminator(stmt) and i == statements.len - 1) {
                return true;
            }
        }
        return false;
    }
    pub fn analyzeStatementListEnhanced(self: *ControlFlowAnalyzer, statements: []const *ast.Node) errors.CodegenError!bool {
        if (statements.len == 0) return false;
        return self.analyzeControlFlow(statements, 0, false);
    }
    
    fn analyzeControlFlow(self: *ControlFlowAnalyzer, statements: []const *ast.Node, index: usize, in_unreachable: bool) errors.CodegenError!bool {
        if (index >= statements.len) return false;
        const stmt = statements[index];
        if (in_unreachable) {
            return self.analyzeControlFlow(statements, index + 1, true);
        }
        if (self.isReturnStatement(stmt)) {
            if (index == statements.len - 1) {
                return true;
            }
            return self.analyzeControlFlow(statements, index + 1, true);
        }
        if (self.isIfStatement(stmt)) {
            const all_paths_return = try self.analyzeIfStatementEnhanced(stmt);
            if (all_paths_return) {
                if (index == statements.len - 1) {
                    return true;
                }
                return self.analyzeControlFlow(statements, index + 1, true);
            } else {
                return self.analyzeControlFlow(statements, index + 1, false);
            }
        }
        
        if (self.isUnconditionalTerminator(stmt)) {
            if (index == statements.len - 1) {
                return true;
            }
            return self.analyzeControlFlow(statements, index + 1, true);
        }
        return self.analyzeControlFlow(statements, index + 1, false);
    }
    
    fn analyzeIfStatementEnhanced(self: *ControlFlowAnalyzer, stmt: *ast.Node) errors.CodegenError!bool {
        switch (stmt.data) {
            .if_stmt => |if_stmt| {
                const then_returns = try self.analyzeStatementListEnhanced(if_stmt.then_body.items);
                if (if_stmt.else_body) |else_body| {
                    const else_returns = try self.analyzeStatementListEnhanced(else_body.items);
                    return then_returns and else_returns;
                } else {
                    return false;
                }
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }
    
    fn isReturnStatement(self: *ControlFlowAnalyzer, stmt: *ast.Node) bool {
        _ = self;
        return switch (stmt.data) {
            .return_stmt => true,
            else => false,
        };
    }
    
    fn isIfStatement(self: *ControlFlowAnalyzer, stmt: *ast.Node) bool {
        _ = self;
        return switch (stmt.data) {
            .if_stmt => true,
            else => false,
        };
    }
    
    fn isUnconditionalTerminator(self: *ControlFlowAnalyzer, stmt: *ast.Node) bool {
        _ = self;
        return switch (stmt.data) {
            .function_call => |call| std.mem.eql(u8, call.name, "exit"),
            else => false,
        };
    }
    
    fn analyzeIfStatement(self: *ControlFlowAnalyzer, stmt: *ast.Node) errors.CodegenError!bool {
        switch (stmt.data) {
            .if_stmt => |if_stmt| {
                const then_returns = try self.analyzeStatementList(if_stmt.then_body.items);
                if (if_stmt.else_body) |else_body| {
                    const else_returns = try self.analyzeStatementList(else_body.items);
                    return then_returns and else_returns;
                } else {
                    return false;
                }
            },
            else => return errors.CodegenError.TypeMismatch,
        }
    }
    
    pub fn analyzeIfElseChain(self: *ControlFlowAnalyzer, statements: []const *ast.Node) errors.CodegenError!bool {
        if (statements.len == 0) {
            return false;
        }
        var i: usize = 0;
        var has_else = false;
        var all_branches_return = true;
        
        while (i < statements.len) : (i += 1) {
            const stmt = statements[i];
            
            if (self.isIfStatement(stmt)) {
                switch (stmt.data) {
                    .if_stmt => |if_stmt| {
                        const is_last = (i == statements.len - 1);
                        const next_is_else_if = !is_last and self.isIfStatement(statements[i + 1]);
                        const then_returns = try self.analyzeStatementList(if_stmt.then_body.items);
                        all_branches_return = all_branches_return and then_returns;
                        if (if_stmt.else_body) |else_body| {
                            const else_returns = try self.analyzeStatementList(else_body.items);
                            all_branches_return = all_branches_return and else_returns;
                            has_else = true;
                        } else if (!next_is_else_if) {
                            has_else = false;
                            all_branches_return = false;
                        }
                    },
                    else => return errors.CodegenError.TypeMismatch,
                }
            } else if (self.isReturnStatement(stmt)) {
                return all_branches_return;
            } else {
                break;
            }
        }
        return has_else and all_branches_return;
    }
    
    fn isBooleanLiteral(self: *ControlFlowAnalyzer, node: *ast.Node) bool {
        _ = self;
        return switch (node.data) {
            .bool_literal => true,
            .identifier => |ident| std.mem.eql(u8, ident.name, "true") or std.mem.eql(u8, ident.name, "false"),
            else => false,
        };
    }
    
    fn isTrueLiteral(self: *ControlFlowAnalyzer, node: *ast.Node) bool {
        _ = self;
        return switch (node.data) {
            .bool_literal => |bool_val| bool_val.value,
            .identifier => |ident| std.mem.eql(u8, ident.name, "true"),
            else => false,
        };
    }
    
    fn isFalseLiteral(self: *ControlFlowAnalyzer, node: *ast.Node) bool {
        _ = self;
        return switch (node.data) {
            .bool_literal => |bool_val| !bool_val.value,
            .identifier => |ident| std.mem.eql(u8, ident.name, "false"),
            else => false,
        };
    }
    
    pub fn analyzeConditionalStructure(self: *ControlFlowAnalyzer, statements: []const *ast.Node) errors.CodegenError!bool {
        if (statements.len == 0) {
            return false;
        }
        if (statements.len > 0 and self.isReturnStatement(statements[statements.len - 1])) {
            return true;
        }
        if (try self.analyzeIfElseChain(statements)) {
            return true;
        }
        var i: usize = 0;
        while (i < statements.len) : (i += 1) {
            const stmt = statements[i];
            
            if (self.isIfStatement(stmt)) {
                switch (stmt.data) {
                    .if_stmt => |if_stmt| {
                        if (self.isTrueLiteral(if_stmt.condition)) {
                            const then_returns = try self.analyzeStatementList(if_stmt.then_body.items);
                            if (then_returns and i == statements.len - 1) {
                                return true;
                            }
                        }
                        else if (self.isFalseLiteral(if_stmt.condition)) {
                            if (if_stmt.else_body) |else_body| {
                                const else_returns = try self.analyzeStatementList(else_body.items);
                                if (else_returns and i == statements.len - 1) {
                                    return true;
                                }
                            } else {
                                return false;
                            }
                        }
                    },
                    else => return errors.CodegenError.TypeMismatch,
                }
            }
        }
        
        return false;
    }
};