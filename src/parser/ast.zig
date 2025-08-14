const std = @import("std");

pub const NodeType = enum {
    program,
    function,
    assignment,
    var_decl,
    function_call,
    return_stmt,
    identifier,
    float_literal,
    number_literal,
    string_literal,
    bool_literal,
    binary_op,
};

pub const BinaryOp = struct {
    op: u8,
    lhs: *Node,
    rhs: *Node,
};

pub const Type = struct {
    name: []const u8,
};

pub const Identifier = struct {
    name: []const u8,
};

pub const FloatLiteral = struct {
    value: []const u8,
};

pub const NumberLiteral = struct {
    value: []const u8,
};

pub const StringLiteral = struct {
    value: []const u8,
};

pub const BoolLiteral = struct {
    value: bool,
};

pub const Assignment = struct {
    name: []const u8,
    value: *Node,
};

pub const VarDecl = struct {
    type_name: []const u8,
    name: []const u8,
    initializer: ?*Node,
};

pub const FunctionCall = struct {
    name: []const u8,
    is_libc: bool, // true if starts with @
    args: std.ArrayList(*Node),
};

pub const ReturnStmt = struct {
    expression: ?*Node,
};

pub const Function = struct {
    name: []const u8,
    return_type: []const u8,
    body: std.ArrayList(*Node),
};

pub const Program = struct {
    functions: std.ArrayList(*Node),
};

pub const NodeData = union(NodeType) {
    program: Program,
    function: Function,
    assignment: Assignment,
    var_decl: VarDecl,
    function_call: FunctionCall,
    return_stmt: ReturnStmt,
    identifier: Identifier,
    float_literal: FloatLiteral,
    number_literal: NumberLiteral,
    string_literal: StringLiteral,
    bool_literal: BoolLiteral,
    binary_op: BinaryOp,
};

pub const Node = struct {
    data: NodeData,
    allocator: std.mem.Allocator,

    pub fn create(allocator: std.mem.Allocator, data: NodeData) !*Node {
        const node = try allocator.create(Node);
        node.* = Node{
            .data = data,
            .allocator = allocator,
        };
        return node;
    }

    pub fn destroy(self: *Node) void {
        switch (self.data) {
            .program => |prog| {
                for (prog.functions.items) |func| {
                    func.destroy();
                }
                prog.functions.deinit();
            },
            .function => |func| {
                for (func.body.items) |stmt| {
                    stmt.destroy();
                }
                func.body.deinit();
            },
            .function_call => |call| {
                for (call.args.items) |arg| {
                    arg.destroy();
                }
                call.args.deinit();
            },
            .var_decl => |decl| {
                if (decl.initializer) |init| {
                    init.destroy();
                }
            },
            .return_stmt => |ret| {
                if (ret.expression) |expr| {
                    expr.destroy();
                }
            },
            else => {},
        }
        self.allocator.destroy(self);
    }
};

fn printIndent(indent: u32, is_last: bool, is_root: bool) void {
    if (is_root) return;

    var i: u32 = 0;
    while (i < indent - 1) : (i += 1) {
        std.debug.print("│   ", .{});
    }

    if (is_last) {
        std.debug.print("└── ", .{});
    } else {
        std.debug.print("├── ", .{});
    }
}

pub fn printAST(node: *Node, indent: u32, is_last: bool, is_root: bool) void {
    printIndent(indent, is_last, is_root);

    switch (node.data) {
        .program => |prog| {
            std.debug.print("📁 Program ({} function{s})\n", .{ prog.functions.items.len, if (prog.functions.items.len == 1) @as([]const u8, "") else "s" });
            for (prog.functions.items, 0..) |func, i| {
                const is_func_last = i == prog.functions.items.len - 1;
                printAST(func, indent + 1, is_func_last, false);
            }
        },
        .function => |func| {
            std.debug.print("⚡ Function: \x1b[32m{s}\x1b[0m -> \x1b[33m{s}\x1b[0m ({} statement{s})\n", .{ func.name, func.return_type, func.body.items.len, if (func.body.items.len == 1) @as([]const u8, "") else "s" });
            for (func.body.items, 0..) |stmt, i| {
                const is_stmt_last = i == func.body.items.len - 1;
                printAST(stmt, indent + 1, is_stmt_last, false);
            }
        },
        .assignment => |as| {
            std.debug.print("➡️  Assignment: \x1b[36m{s}\x1b[0m =\n", .{as.name});
            printAST(as.value, indent + 1, true, false);
        },
        .var_decl => |decl| {
            if (decl.initializer) |init| {
                std.debug.print("📋 VarDecl: \x1b[33m{s}\x1b[0m \x1b[36m{s}\x1b[0m = \n", .{ decl.type_name, decl.name });
                printAST(init, indent + 1, true, false);
            } else {
                std.debug.print("📋 VarDecl: \x1b[33m{s}\x1b[0m \x1b[36m{s}\x1b[0m\n", .{ decl.type_name, decl.name });
            }
        },
        .function_call => |call| {
            const prefix = if (call.is_libc) "@" else "";
            const icon = if (call.is_libc) "🔧" else "📞";
            std.debug.print("{s} FunctionCall: \x1b[35m{s}{s}\x1b[0m ({} arg{s})\n", .{ icon, prefix, call.name, call.args.items.len, if (call.args.items.len == 1) @as([]const u8, "") else "s" });
            for (call.args.items, 0..) |arg, i| {
                const is_arg_last = i == call.args.items.len - 1;
                printAST(arg, indent + 1, is_arg_last, false);
            }
        },
        .return_stmt => |ret| {
            if (ret.expression) |expr| {
                std.debug.print("↩️  Return:\n", .{});
                printAST(expr, indent + 1, true, false);
            } else {
                std.debug.print("↩️  Return (void)\n", .{});
            }
        },
        .binary_op => |binary_op| {
            std.debug.print("🧮 BinaryOp: \x1b[35m{c}\x1b[0m\n", .{binary_op.op});
            printAST(binary_op.lhs, indent + 1, false, false);
            printAST(binary_op.rhs, indent + 1, true, false);
        },
        .identifier => |ident| {
            std.debug.print("🔤 Identifier: \x1b[36m{s}\x1b[0m\n", .{ident.name});
        },
        .float_literal => |float| {
            std.debug.print("🔢 Float: \x1b[31m{s}\x1b[0m\n", .{float.value});
        },
        .number_literal => |num| {
            std.debug.print("🔢 Number: \x1b[31m{s}\x1b[0m\n", .{num.value});
        },
        .string_literal => |str| {
            std.debug.print("💬 String: \x1b[32m\"{s}\"\x1b[0m\n", .{str.value});
        },
        .bool_literal => |bool_val| {
            std.debug.print("✅ Boolean: \x1b[35m{s}\x1b[0m\n", .{if (bool_val.value) "true" else "false"});
        },
    }
}

pub fn printASTTree(root: *Node) void {
    std.debug.print("\n\x1b[1m🌳 Abstract Syntax Tree\x1b[0m\n", .{});
    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});
    printAST(root, 0, true, true);
    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n", .{});
}
