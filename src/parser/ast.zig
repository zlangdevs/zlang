const std = @import("std");

pub const NodeType = enum {
    program,
    function,
    assignment,
    var_decl,
    function_call,
    return_stmt,
    identifier,
    unary_op,
    float_literal,
    number_literal,
    char_literal,
    string_literal,
    bool_literal,
    binary_op,
    brainfuck,
    comparison,
    if_stmt,
    for_stmt,
    break_stmt,
    continue_stmt,
    c_for_stmt,
    array_initializer,
    array_index,
    array_assignment,
    c_function_decl,
    use_stmt,
    enum_decl,
    struct_decl,
    qualified_identifier,
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

pub const UnaryOp = struct {
    op: u8,
    operand: *Node,
};

pub const FloatLiteral = struct {
    value: []const u8,
};

pub const NumberLiteral = struct {
    value: []const u8,
};

pub const CharLiteral = struct {
    value: i32,
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

pub const Comparison = struct {
    op: u8,
    lhs: *Node,
    rhs: *Node,
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

pub const Parameter = struct {
    name: []const u8,
    type_name: []const u8,
};

pub const Function = struct {
    name: []const u8,
    return_type: []const u8,
    parameters: std.ArrayList(Parameter),
    body: std.ArrayList(*Node),
};

pub const Program = struct {
    functions: std.ArrayList(*Node),
};

pub const Brainfuck = struct {
    code: []const u8,
};

pub const IfStmt = struct {
    condition: *Node,
    then_body: std.ArrayList(*Node),
    else_body: ?std.ArrayList(*Node),
};

pub const ForStmt = struct {
    condition: ?*Node,
    body: std.ArrayList(*Node),
};

pub const CForStmt = struct {
    init: ?*Node,
    condition: ?*Node,
    increment: ?*Node,
    body: std.ArrayList(*Node),
};

pub const BreakStmt = struct {};

pub const ContinueStmt = struct {};

pub const ArrayInitializer = struct {
    elements: std.ArrayList(*Node),
};

pub const ArrayIndex = struct {
    array_name: []const u8,
    index: *Node,
};

pub const ArrayAssignment = struct {
    array_name: []const u8,
    index: *Node,
    value: *Node,
};

pub const CFunctionDecl = struct {
    name: []const u8,
    return_type: []const u8,
    parameters: std.ArrayList(Parameter),
};

pub const UseStmt = struct {
    module_path: []const u8,
};

pub const EnumValue = struct {
    name: []const u8,
    value: ?*Node,
};

pub const EnumDecl = struct {
    name: []const u8,
    values: std.ArrayList(EnumValue),
};

pub const StructField = struct {
    name: []const u8,
    type_name: []const u8,
};

pub const StructDecl = struct {
    name: []const u8,
    fields: std.ArrayList(StructField),
};

pub const QualifiedIdentifier = struct {
    qualifier: []const u8,
    name: []const u8,
};

pub const NodeData = union(NodeType) {
    program: Program,
    function: Function,
    assignment: Assignment,
    var_decl: VarDecl,
    function_call: FunctionCall,
    return_stmt: ReturnStmt,
    identifier: Identifier,
    unary_op: UnaryOp,
    float_literal: FloatLiteral,
    number_literal: NumberLiteral,
    char_literal: CharLiteral,
    string_literal: StringLiteral,
    bool_literal: BoolLiteral,
    binary_op: BinaryOp,
    brainfuck: Brainfuck,
    comparison: Comparison,
    if_stmt: IfStmt,
    for_stmt: ForStmt,
    break_stmt: BreakStmt,
    continue_stmt: ContinueStmt,
    c_for_stmt: CForStmt,
    array_initializer: ArrayInitializer,
    array_index: ArrayIndex,
    array_assignment: ArrayAssignment,
    c_function_decl: CFunctionDecl,
    use_stmt: UseStmt,
    enum_decl: EnumDecl,
    struct_decl: StructDecl,
    qualified_identifier: QualifiedIdentifier,
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
            .comparison => |comp| {
                comp.lhs.destroy();
                comp.rhs.destroy();
            },
            .binary_op => |bop| {
                bop.lhs.destroy();
                bop.rhs.destroy();
            },
            .unary_op => |un| {
                un.operand.destroy();
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
            .if_stmt => |if_stmt| {
                if_stmt.condition.destroy();
                for (if_stmt.then_body.items) |stmt| {
                    stmt.destroy();
                }
                if_stmt.then_body.deinit();
                if (if_stmt.else_body) |else_body| {
                    for (else_body.items) |stmt| {
                        stmt.destroy();
                    }
                    else_body.deinit();
                }
            },
            .for_stmt => |for_stmt| {
                if (for_stmt.condition) |cond| {
                    cond.destroy();
                }
                for (for_stmt.body.items) |stmt| {
                    stmt.destroy();
                }
                for_stmt.body.deinit();
            },
            .c_for_stmt => |c_for| {
                if (c_for.init) |init| init.destroy();
                if (c_for.condition) |cond| cond.destroy();
                if (c_for.increment) |inc| inc.destroy();
                for (c_for.body.items) |stmt| stmt.destroy();
                c_for.body.deinit();
            },
            .array_initializer => |arr_init| {
                for (arr_init.elements.items) |element| {
                    element.destroy();
                }
                arr_init.elements.deinit();
            },
            .array_index => |arr_idx| {
                arr_idx.index.destroy();
            },
            .array_assignment => |arr_ass| {
                arr_ass.index.destroy();
                arr_ass.value.destroy();
            },
            .use_stmt => |use_stmt| {
                self.allocator.free(use_stmt.module_path);
            },
            .enum_decl => |enum_decl| {
                self.allocator.free(enum_decl.name);
                for (enum_decl.values.items) |*value| {
                    self.allocator.free(value.name);
                    if (value.value) |val| {
                        val.destroy();
                    }
                }
                enum_decl.values.deinit();
            },
            .struct_decl => |struct_decl| {
                self.allocator.free(struct_decl.name);
                for (struct_decl.fields.items) |*field| {
                    self.allocator.free(field.name);
                    self.allocator.free(field.type_name);
                }
                struct_decl.fields.deinit();
            },
            .qualified_identifier => |qual_id| {
                self.allocator.free(qual_id.qualifier);
                self.allocator.free(qual_id.name);
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
            if (func.parameters.items.len > 0) {
                std.debug.print("⚡ Function: \x1b[32m{s}\x1b[0m(", .{func.name});
                for (func.parameters.items, 0..) |param, i| {
                    if (i > 0) std.debug.print(", ", .{});
                    std.debug.print("\x1b[36m{s}\x1b[0m: \x1b[33m{s}\x1b[0m", .{ param.name, param.type_name });
                }
                std.debug.print(") -> \x1b[33m{s}\x1b[0m ({} statement{s})\n", .{ func.return_type, func.body.items.len, if (func.body.items.len == 1) @as([]const u8, "") else "s" });
            } else {
                std.debug.print("⚡ Function: \x1b[32m{s}\x1b[0m() -> \x1b[33m{s}\x1b[0m ({} statement{s})\n", .{ func.name, func.return_type, func.body.items.len, if (func.body.items.len == 1) @as([]const u8, "") else "s" });
            }
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
        .comparison => |comp| {
            std.debug.print("⚖️ Comparison: \x1b[35m{c}\x1b[0m\n", .{comp.op});
            printAST(comp.lhs, indent + 1, false, false);
            printAST(comp.rhs, indent + 1, true, false);
        },
        .unary_op => |unary_op| {
            std.debug.print("📐 UnaryOp: \x1b[35m{c}\x1b[0m\n", .{unary_op.op});
            printAST(unary_op.operand, indent + 1, true, false);
        },
        .binary_op => |binary_op| {
            std.debug.print("🧮 BinaryOp: \x1b[35m{c}\x1b[0m\n", .{binary_op.op});
            printAST(binary_op.lhs, indent + 1, false, false);
            printAST(binary_op.rhs, indent + 1, true, false);
        },
        .brainfuck => |bf| {
            std.debug.print("🧠 Brainfuck:\n", .{});
            var lines = std.mem.splitScalar(u8, bf.code, '\n');
            var line_list = std.ArrayList([]const u8).init(std.heap.page_allocator);
            defer line_list.deinit();
            while (lines.next()) |line| {
                const trimmed = std.mem.trim(u8, line, " \t\r");
                if (trimmed.len > 0) {
                    line_list.append(trimmed) catch continue;
                }
            }
            for (line_list.items, 0..) |line, i| {
                const is_line_last = i == line_list.items.len - 1;
                printIndent(indent + 1, is_line_last, false);
                if (std.mem.startsWith(u8, line, "?") and std.mem.endsWith(u8, line, "?")) {
                    const content = line[1 .. line.len - 1];
                    if (std.mem.indexOf(u8, content, " ")) |space_idx| {
                        const key = content[0..space_idx];
                        const value = content[space_idx + 1 ..];
                        std.debug.print("\x1b[90m{s}: {s}\x1b[0m\n", .{ key, value });
                    } else {
                        std.debug.print("\x1b[90m{s}\x1b[0m\n", .{content});
                    }
                } else {
                    std.debug.print("\x1b[35m{s}\x1b[0m\n", .{line});
                }
            }
        },
        .identifier => |ident| {
            std.debug.print("🔤 Identifier: \x1b[36m{s}\x1b[0m\n", .{ident.name});
        },
        .qualified_identifier => |qual_id| {
            std.debug.print("🔗 Qualified Identifier: \x1b[36m{s}\x1b[0m.\x1b[36m{s}\x1b[0m\n", .{ qual_id.qualifier, qual_id.name });
        },
        .float_literal => |float| {
            std.debug.print("🔢 Float: \x1b[31m{s}\x1b[0m\n", .{float.value});
        },
        .number_literal => |num| {
            std.debug.print("🔢 Number: \x1b[31m{s}\x1b[0m\n", .{num.value});
        },
        .char_literal => |char| {
            if (char.value >= 32 and char.value <= 126) {
                std.debug.print("🔤 Char: \x1b[35m'{c}'\x1b[0m ({d})\n", .{ @as(u8, @intCast(char.value)), char.value });
            } else {
                std.debug.print("🔤 Char: \x1b[35m{d}\x1b[0m\n", .{char.value});
            }
        },
        .string_literal => |str| {
            std.debug.print("💬 String: \x1b[32m\"{s}\"\x1b[0m\n", .{str.value});
        },
        .bool_literal => |bool_val| {
            std.debug.print("✅ Boolean: \x1b[35m{s}\x1b[0m\n", .{if (bool_val.value) "true" else "false"});
        },
        .if_stmt => |if_stmt| {
            const has_else = if (if_stmt.else_body) |else_body| else_body.items.len > 0 else false;
            std.debug.print("❓ If Statement:\n", .{});
            printAST(if_stmt.condition, indent + 1, false, false);
            for (if_stmt.then_body.items, 0..) |stmt, i| {
                const is_stmt_last = i == if_stmt.then_body.items.len - 1 and !has_else;
                printAST(stmt, indent + 1, is_stmt_last, false);
            }
            if (if_stmt.else_body) |else_body| {
                if (else_body.items.len > 0) {
                    for (else_body.items, 0..) |stmt, i| {
                        const is_stmt_last = i == else_body.items.len - 1;
                        printAST(stmt, indent + 1, is_stmt_last, false);
                    }
                }
            }
        },
        .for_stmt => |for_stmt| {
            if (for_stmt.condition) |cond| {
                std.debug.print("🔁 For Statement (condition):\n", .{});
                printAST(cond, indent + 1, false, false);
            } else {
                std.debug.print("🔁 For Statement (infinite):\n", .{});
            }
            for (for_stmt.body.items, 0..) |stmt, i| {
                const is_stmt_last = i == for_stmt.body.items.len - 1;
                printAST(stmt, indent + 1, is_stmt_last, false);
            }
        },
        .c_for_stmt => |c_for| {
            std.debug.print("🔄 C-style For Statement:\n", .{});
            if (c_for.init) |init| {
                printIndent(indent + 1, false, false);
                std.debug.print("Init:\n", .{});
                printAST(init, indent + 2, false, false);
            }
            if (c_for.condition) |cond| {
                printIndent(indent + 1, false, false);
                std.debug.print("Condition:\n", .{});
                printAST(cond, indent + 2, false, false);
            }
            if (c_for.increment) |inc| {
                printIndent(indent + 1, false, false);
                std.debug.print("Increment:\n", .{});
                printAST(inc, indent + 2, false, false);
            }
            printIndent(indent + 1, true, false);
            std.debug.print("Body:\n", .{});
            for (c_for.body.items, 0..) |stmt, i| {
                const is_stmt_last = i == c_for.body.items.len - 1;
                printAST(stmt, indent + 2, is_stmt_last, false);
            }
        },
        .break_stmt => {
            std.debug.print("🚫 Break Statement\n", .{});
        },
        .continue_stmt => {
            std.debug.print("➡️  Continue Statement\n", .{});
        },
        .array_initializer => |arr_init| {
            std.debug.print("🔢 Array Initializer ({} elements)\n", .{arr_init.elements.items.len});
            for (arr_init.elements.items, 0..) |element, i| {
                const is_elem_last = i == arr_init.elements.items.len - 1;
                printAST(element, indent + 1, is_elem_last, false);
            }
        },
        .array_index => |arr_idx| {
            std.debug.print("🔍 Array Index: \x1b[36m{s}\x1b[0m[\n", .{arr_idx.array_name});
            printAST(arr_idx.index, indent + 1, true, false);
            std.debug.print("    ]\n", .{});
        },
        .array_assignment => |arr_ass| {
            std.debug.print("🔄 Array Assignment: \x1b[36m{s}\x1b[0m[\n", .{arr_ass.array_name});
            printAST(arr_ass.index, indent + 1, false, false);
            std.debug.print("    ] = \n", .{});
            printAST(arr_ass.value, indent + 1, true, false);
        },
        .c_function_decl => |c_func| {
            if (c_func.parameters.items.len > 0) {
                std.debug.print("🔗 C Function Decl: \x1b[32m{s}\x1b[0m(", .{c_func.name});
                for (c_func.parameters.items, 0..) |param, i| {
                    if (i > 0) std.debug.print(", ", .{});
                    std.debug.print("\x1b[36m{s}\x1b[0m: \x1b[33m{s}\x1b[0m", .{ param.name, param.type_name });
                }
                std.debug.print(") -> \x1b[33m{s}\x1b[0m\n", .{c_func.return_type});
            } else {
                std.debug.print("🔗 C Function Decl: \x1b[32m{s}\x1b[0m() -> \x1b[33m{s}\x1b[0m\n", .{ c_func.name, c_func.return_type });
            }
        },
        .use_stmt => |use_stmt| {
            std.debug.print("📦 Use Statement: \x1b[35m{s}\x1b[0m\n", .{use_stmt.module_path});
        },
        .enum_decl => |enum_decl| {
            std.debug.print("🔢 Enum Declaration: \x1b[32m{s}\x1b[0m ({} values)\n", .{ enum_decl.name, enum_decl.values.items.len });
            for (enum_decl.values.items, 0..) |value, i| {
                const is_last_value = i == enum_decl.values.items.len - 1;
                printIndent(indent + 1, is_last_value, false);
                std.debug.print("Value: \x1b[36m{s}\x1b[0m", .{value.name});
                if (value.value) |val| {
                    std.debug.print(" = \n", .{});
                    printAST(val, indent + 2, true, false);
                } else {
                    std.debug.print("\n", .{});
                }
            }
        },
        .struct_decl => |struct_decl| {
            std.debug.print("🏗️  Struct Declaration: \x1b[32m{s}\x1b[0m ({} fields)\n", .{ struct_decl.name, struct_decl.fields.items.len });
            for (struct_decl.fields.items, 0..) |field, i| {
                const is_last_field = i == struct_decl.fields.items.len - 1;
                printIndent(indent + 1, is_last_field, false);
                std.debug.print("Field: \x1b[36m{s}\x1b[0m: \x1b[33m{s}\x1b[0m\n", .{ field.name, field.type_name });
            }
        },
    }
}

pub fn printASTTree(root: *Node) void {
    std.debug.print("\n\x1b[1m🌳 Abstract Syntax Tree\x1b[0m\n", .{});
    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n", .{});
    printAST(root, 0, true, true);
    std.debug.print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n", .{});
}
