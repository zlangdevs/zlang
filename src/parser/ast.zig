const std = @import("std");
const utils = @import("../codegen/utils.zig");

pub const NodeType = enum {
    program,
    function,
    assignment,
    compound_assignment,
    var_decl,
    function_call,
    method_call,
    return_stmt,
    identifier,
    unary_op,
    float_literal,
    number_literal,
    char_literal,
    string_literal,
    bool_literal,
    null_literal,
    binary_op,
    brainfuck,
    comparison,
    if_stmt,
    for_stmt,
    break_stmt,
    continue_stmt,
    goto_stmt,
    label_stmt,
    c_for_stmt,
    array_initializer,
    array_index,
    array_assignment,
    array_compound_assignment,
    simd_initializer,
    simd_index,
    simd_assignment,
    simd_compound_assignment,
    simd_method_call,
    c_function_decl,
    use_stmt,
    enum_decl,
    struct_decl,
    struct_initializer,
    qualified_identifier,
    cast,
    expression_block,
    match_stmt,
    error_decl,
    send_stmt,
    solicit_stmt,
    handled_call_stmt,
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

pub const NullLiteral = struct {};

pub const Assignment = struct {
    target: *Node,
    value: *Node,
};

pub const CompoundAssignment = struct {
    target: *Node,
    value: *Node,
    op: u8,
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
    is_const: bool = false,
};

pub const FunctionCall = struct {
    name: []const u8,
    is_libc: bool,
    args: std.ArrayList(*Node),
};

pub const MethodCall = struct {
    object: *Node,
    method_name: []const u8,
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
    guard: ?*Node,
    body: std.ArrayList(*Node),
};

pub const Program = struct {
    functions: std.ArrayList(*Node),
    globals: std.ArrayList(*Node),
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

pub const GotoStmt = struct {
    label: []const u8,
};

pub const LabelStmt = struct {
    label: []const u8,
};

pub const ArrayInitializer = struct {
    elements: std.ArrayList(*Node),
};

pub const SimdInitializer = struct {
    elements: std.ArrayList(*Node),
};

pub const ArrayIndex = struct {
    array: *Node,
    index: *Node,
};

pub const ArrayAssignment = struct {
    array: *Node,
    index: *Node,
    value: *Node,
};

pub const ArrayCompoundAssignment = struct {
    array: *Node,
    index: *Node,
    value: *Node,
    op: u8,
};

pub const SimdIndex = struct {
    simd: *Node,
    index: *Node,
};

pub const SimdAssignment = struct {
    simd: *Node,
    index: *Node,
    value: *Node,
};

pub const SimdCompoundAssignment = struct {
    simd: *Node,
    index: *Node,
    value: *Node,
    op: u8,
};

pub const SimdMethodCall = struct {
    simd: *Node,
    method_name: []const u8,
    args: std.ArrayList(*Node),
};

pub const CFunctionDecl = struct {
    name: []const u8,
    return_type: []const u8,
    parameters: std.ArrayList(Parameter),
    is_wrapped: bool,
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
    default_value: ?*Node,
};

pub const StructDecl = struct {
    name: []const u8,
    fields: std.ArrayList(StructField),
    is_union: bool = false,
};

pub const StructInitializer = struct {
    struct_name: []const u8,
    field_values: std.ArrayList(StructFieldValue),
};

pub const StructFieldValue = struct {
    field_name: ?[]const u8,
    value: *Node,
};

pub const QualifiedIdentifier = struct {
    base: *Node,
    field: []const u8,
};

pub const Cast = struct {
    expr: *Node,
    type_name: ?[]const u8,
    auto: bool,
};

pub const ExpressionBlock = struct {
    type_name: []const u8,
    statements: std.ArrayList(*Node),
    result: *Node,
};

pub const MatchCase = struct {
    values: std.ArrayList(*Node),
    body: std.ArrayList(*Node),
};

pub const MatchStmt = struct {
    condition: *Node,
    cases: std.ArrayList(MatchCase),
};

pub const ErrorDeclCodeKind = enum {
    explicit,
    alias,
    auto,
};

pub const ErrorDecl = struct {
    name: []const u8,
    code_kind: ErrorDeclCodeKind,
    explicit_code: i32,
    alias_name: ?[]const u8,
};

pub const SendStmt = struct {
    error_name: []const u8,
};

pub const SolicitStmt = struct {
    error_name: []const u8,
};

pub const ErrorHandlerKind = enum {
    send,
    solicit,
};

pub const ErrorHandler = struct {
    kind: ErrorHandlerKind,
    error_name: ?[]const u8,
    error_code: ?i32,
    body: std.ArrayList(*Node),
};

pub const HandledCallStmt = struct {
    call: *Node,
    handlers: std.ArrayList(ErrorHandler),
};

pub const NodeData = union(NodeType) {
    program: Program,
    function: Function,
    assignment: Assignment,
    compound_assignment: CompoundAssignment,
    var_decl: VarDecl,
    function_call: FunctionCall,
    method_call: MethodCall,
    return_stmt: ReturnStmt,
    identifier: Identifier,
    unary_op: UnaryOp,
    float_literal: FloatLiteral,
    number_literal: NumberLiteral,
    char_literal: CharLiteral,
    string_literal: StringLiteral,
    bool_literal: BoolLiteral,
    null_literal: NullLiteral,
    binary_op: BinaryOp,
    brainfuck: Brainfuck,
    comparison: Comparison,
    if_stmt: IfStmt,
    for_stmt: ForStmt,
    break_stmt: BreakStmt,
    continue_stmt: ContinueStmt,
    goto_stmt: GotoStmt,
    label_stmt: LabelStmt,
    c_for_stmt: CForStmt,
    array_initializer: ArrayInitializer,
    array_index: ArrayIndex,
    array_assignment: ArrayAssignment,
    array_compound_assignment: ArrayCompoundAssignment,
    simd_initializer: SimdInitializer,
    simd_index: SimdIndex,
    simd_assignment: SimdAssignment,
    simd_compound_assignment: SimdCompoundAssignment,
    simd_method_call: SimdMethodCall,
    c_function_decl: CFunctionDecl,
    use_stmt: UseStmt,
    enum_decl: EnumDecl,
    struct_decl: StructDecl,
    struct_initializer: StructInitializer,
    qualified_identifier: QualifiedIdentifier,
    cast: Cast,
    expression_block: ExpressionBlock,
    match_stmt: MatchStmt,
    error_decl: ErrorDecl,
    send_stmt: SendStmt,
    solicit_stmt: SolicitStmt,
    handled_call_stmt: HandledCallStmt,
};

pub const ArenaAST = struct {
    arena: std.heap.ArenaAllocator,
    root: *Node,

    pub fn init(backing_allocator: std.mem.Allocator) ArenaAST {
        return ArenaAST{
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
            .root = undefined,
        };
    }

    pub fn allocator(self: *ArenaAST) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn setRoot(self: *ArenaAST, root: *Node) void {
        self.root = root;
    }

    pub fn getRoot(self: *const ArenaAST) *Node {
        return self.root;
    }

    pub fn deinit(self: *ArenaAST) void {
        self.arena.deinit();
    }
};

pub const Node = struct {
    data: NodeData,
    allocator: std.mem.Allocator,
    line: usize,
    column: usize,

    pub fn create(allocator: std.mem.Allocator, data: NodeData) *Node {
        const node = utils.create(Node, allocator);
        node.* = Node{
            .data = data,
            .allocator = allocator,
            .line = 0,
            .column = 0,
        };
        return node;
    }

    pub fn destroy(self: *Node) void {
        switch (self.data) {
            .program => |*prog| {
                for (prog.functions.items) |func| {
                    func.destroy();
                }
                prog.functions.deinit(self.allocator);
                for (prog.globals.items) |glob| {
                    glob.destroy();
                }
                prog.globals.deinit(self.allocator);
            },
            .function => |*func| {
                if (func.guard) |guard| {
                    guard.destroy();
                }
                for (func.body.items) |stmt| {
                    stmt.destroy();
                }
                func.body.deinit(self.allocator);
            },
            .function_call => |*call| {
                for (call.args.items) |arg| {
                    arg.destroy();
                }
                call.args.deinit(self.allocator);
                self.allocator.free(call.name);
            },
            .method_call => |*method| {
                method.object.destroy();
                for (method.args.items) |arg| {
                    arg.destroy();
                }
                method.args.deinit(self.allocator);
                self.allocator.free(method.method_name);
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
            .assignment => |as| {
                as.target.destroy();
                as.value.destroy();
            },
            .compound_assignment => |cas| {
                cas.target.destroy();
                cas.value.destroy();
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
            .if_stmt => |*if_stmt| {
                if_stmt.condition.destroy();
                for (if_stmt.then_body.items) |stmt| {
                    stmt.destroy();
                }
                if_stmt.then_body.deinit(self.allocator);
                if (if_stmt.else_body != null) {
                    for (if_stmt.else_body.?.items) |stmt| {
                        stmt.destroy();
                    }
                    if_stmt.else_body.?.deinit(self.allocator);
                }
            },
            .for_stmt => |*for_stmt| {
                if (for_stmt.condition) |cond| {
                    cond.destroy();
                }
                for (for_stmt.body.items) |stmt| {
                    stmt.destroy();
                }
                for_stmt.body.deinit(self.allocator);
            },
            .c_for_stmt => |*c_for| {
                if (c_for.init) |init| init.destroy();
                if (c_for.condition) |cond| cond.destroy();
                if (c_for.increment) |inc| inc.destroy();
                for (c_for.body.items) |stmt| stmt.destroy();
                c_for.body.deinit(self.allocator);
            },
            .array_initializer => |*arr_init| {
                for (arr_init.elements.items) |element| {
                    element.destroy();
                }
                arr_init.elements.deinit(self.allocator);
            },
            .array_index => |arr_idx| {
                arr_idx.array.destroy();
                arr_idx.index.destroy();
            },
            .array_assignment => |arr_ass| {
                arr_ass.array.destroy();
                arr_ass.index.destroy();
                arr_ass.value.destroy();
            },
            .array_compound_assignment => |arr_cass| {
                arr_cass.array.destroy();
                arr_cass.index.destroy();
                arr_cass.value.destroy();
            },
            .simd_initializer => |*simd_init| {
                for (simd_init.elements.items) |element| {
                    element.destroy();
                }
                simd_init.elements.deinit(self.allocator);
            },
            .simd_index => |simd_idx| {
                simd_idx.simd.destroy();
                simd_idx.index.destroy();
            },
            .simd_assignment => |simd_ass| {
                simd_ass.simd.destroy();
                simd_ass.index.destroy();
                simd_ass.value.destroy();
            },
            .simd_compound_assignment => |simd_cass| {
                simd_cass.simd.destroy();
                simd_cass.index.destroy();
                simd_cass.value.destroy();
            },
            .simd_method_call => |*simd_method| {
                simd_method.simd.destroy();
                for (simd_method.args.items) |arg| {
                    arg.destroy();
                }
                simd_method.args.deinit(self.allocator);
            },
            .c_function_decl => |*c_func| {
                self.allocator.free(c_func.name);
                self.allocator.free(c_func.return_type);
                for (c_func.parameters.items) |param| {
                    self.allocator.free(param.name);
                    self.allocator.free(param.type_name);
                }
                c_func.parameters.deinit(self.allocator);
            },
            .use_stmt => |use_stmt| {
                self.allocator.free(use_stmt.module_path);
            },
            .enum_decl => |*enum_decl| {
                self.allocator.free(enum_decl.name);
                for (enum_decl.values.items) |*value| {
                    self.allocator.free(value.name);
                    if (value.value) |val| {
                        val.destroy();
                    }
                }
                enum_decl.values.deinit(self.allocator);
            },
            .struct_decl => |*struct_decl| {
                self.allocator.free(struct_decl.name);
                for (struct_decl.fields.items) |*field| {
                    self.allocator.free(field.name);
                    self.allocator.free(field.type_name);
                    if (field.default_value) |default_val| {
                        default_val.destroy();
                    }
                }
                struct_decl.fields.deinit(self.allocator);
            },
            .struct_initializer => |*struct_init| {
                self.allocator.free(struct_init.struct_name);
                for (struct_init.field_values.items) |*field_val| {
                    if (field_val.field_name) |fname| {
                        self.allocator.free(fname);
                    }
                    field_val.value.destroy();
                }
                struct_init.field_values.deinit(self.allocator);
            },
            .qualified_identifier => |qual_id| {
                qual_id.base.destroy();
                self.allocator.free(qual_id.field);
            },
            .cast => |c| {
                c.expr.destroy();
                if (c.type_name) |tn| self.allocator.free(tn);
            },
            .expression_block => |*block| {
                self.allocator.free(block.type_name);
                for (block.statements.items) |stmt| {
                    stmt.destroy();
                }
                block.statements.deinit(self.allocator);
                block.result.destroy();
            },
            .goto_stmt => |goto_stmt| {
                self.allocator.free(goto_stmt.label);
            },
            .label_stmt => |label_stmt| {
                self.allocator.free(label_stmt.label);
            },
            .match_stmt => |*match_stmt| {
                match_stmt.condition.destroy();
                for (match_stmt.cases.items) |*case| {
                    for (case.values.items) |val| val.destroy();
                    case.values.deinit(self.allocator);
                    for (case.body.items) |stmt| stmt.destroy();
                    case.body.deinit(self.allocator);
                }
                match_stmt.cases.deinit(self.allocator);
            },
            .error_decl => |error_decl| {
                self.allocator.free(error_decl.name);
                if (error_decl.alias_name) |alias_name| {
                    self.allocator.free(alias_name);
                }
            },
            .send_stmt => |send_stmt| {
                self.allocator.free(send_stmt.error_name);
            },
            .solicit_stmt => |solicit_stmt| {
                self.allocator.free(solicit_stmt.error_name);
            },
            .handled_call_stmt => |*handled_call| {
                handled_call.call.destroy();
                for (handled_call.handlers.items) |*handler| {
                    if (handler.error_name) |error_name| {
                        self.allocator.free(error_name);
                    }
                    for (handler.body.items) |stmt| {
                        stmt.destroy();
                    }
                    handler.body.deinit(self.allocator);
                }
                handled_call.handlers.deinit(self.allocator);
            },
            else => {},
        }
        _ = self.allocator;
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
            const total_items = prog.functions.items.len + prog.globals.items.len;
            std.debug.print("📁 Program ({} item{s})\n", .{ total_items, if (total_items == 1) @as([]const u8, "") else "s" });
            for (prog.globals.items, 0..) |glob, i| {
                const is_glob_last = i == prog.globals.items.len - 1 and prog.functions.items.len == 0;
                printAST(glob, indent + 1, is_glob_last, false);
            }
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
            if (func.guard) |guard| {
                printAST(guard, indent + 1, true, false);
            }
        },
        .assignment => |as| {
            std.debug.print("➡️  Assignment:\n", .{});
            printAST(as.target, indent + 1, false, false);
            printIndent(indent + 1, true, false);
            std.debug.print("= \n", .{});
            printAST(as.value, indent + 2, true, false);
        },
        .compound_assignment => |cas| {
            std.debug.print("🔄 Compound Assignment: \x1b[35m{c}= \x1b[0m\n", .{cas.op});
            printAST(cas.target, indent + 1, false, false);
            printIndent(indent + 1, true, false);
            std.debug.print("\x1b[35m{c}= \x1b[0m\n", .{cas.op});
            printAST(cas.value, indent + 2, true, false);
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
        .method_call => |method| {
            std.debug.print("📞 MethodCall: \x1b[35m{s}\x1b[0m ({} arg{s})\n", .{ method.method_name, method.args.items.len, if (method.args.items.len == 1) @as([]const u8, "") else "s" });
            std.debug.print("    Object:\n", .{});
            printAST(method.object, indent + 1, false, false);
            for (method.args.items, 0..) |arg, i| {
                const is_arg_last = i == method.args.items.len - 1;
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
            var line_list = std.ArrayList([]const u8){};
            defer line_list.deinit(std.heap.page_allocator);
            while (lines.next()) |line| {
                const trimmed = std.mem.trim(u8, line, " \t\r");
                if (trimmed.len > 0) {
                    line_list.append(std.heap.page_allocator, trimmed) catch continue;
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
            std.debug.print("🔗 Qualified Identifier: \n", .{});
            printAST(qual_id.base, indent + 1, false, false);
            printIndent(indent + 1, true, false);
            std.debug.print(".\x1b[36m{s}\x1b[0m\n", .{qual_id.field});
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
        .null_literal => {
            std.debug.print("⬛ Null: \x1b[35mnull\x1b[0m\n", .{});
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
            std.debug.print("🔄 Continue Statement\n", .{});
        },
        .match_stmt => |match_stmt| {
            std.debug.print("🔀 Match Statement:\n", .{});
            printAST(match_stmt.condition, indent + 1, false, false);
            for (match_stmt.cases.items, 0..) |case, i| {
                const is_case_last = i == match_stmt.cases.items.len - 1;
                printIndent(indent + 1, is_case_last, false);
                std.debug.print("Case:\n", .{});
                for (case.values.items, 0..) |val, j| {
                    printAST(val, indent + 2, j == case.values.items.len - 1, false);
                }
                printIndent(indent + 2, true, false);
                std.debug.print("Body:\n", .{});
                for (case.body.items, 0..) |stmt, k| {
                    printAST(stmt, indent + 3, k == case.body.items.len - 1, false);
                }
            }
        },
        .goto_stmt => |goto_stmt| {
            std.debug.print("🎯 Goto: \x1b[36m{s}\x1b[0m\n", .{goto_stmt.label});
        },
        .label_stmt => |label_stmt| {
            std.debug.print("🏷️  Label: \x1b[36m{s}\x1b[0m\n", .{label_stmt.label});
        },
        .array_initializer => |arr_init| {
            std.debug.print("🔢 Array Initializer ({} elements)\n", .{arr_init.elements.items.len});
            for (arr_init.elements.items, 0..) |element, i| {
                const is_elem_last = i == arr_init.elements.items.len - 1;
                printAST(element, indent + 1, is_elem_last, false);
            }
        },
        .array_index => |arr_idx| {
            std.debug.print("🔍 Array Index:\n", .{});
            printAST(arr_idx.array, indent + 1, false, false);
            printIndent(indent + 1, true, false);
            std.debug.print("[\n", .{});
            printAST(arr_idx.index, indent + 2, true, false);
            std.debug.print("    ]\n", .{});
        },
        .array_assignment => |arr_ass| {
            std.debug.print("🔄 Array Assignment:\n", .{});
            printAST(arr_ass.array, indent + 1, false, false);
            printIndent(indent + 1, false, false);
            std.debug.print("[\n", .{});
            printAST(arr_ass.index, indent + 2, false, false);
            printIndent(indent + 1, true, false);
            std.debug.print("] = \n", .{});
            printAST(arr_ass.value, indent + 2, true, false);
        },
        .array_compound_assignment => |arr_cass| {
            std.debug.print("🔄 Array Compound Assignment: \x1b[35m{c}= \x1b[0m\n", .{arr_cass.op});
            printAST(arr_cass.array, indent + 1, false, false);
            printIndent(indent + 1, false, false);
            std.debug.print("[\n", .{});
            printAST(arr_cass.index, indent + 2, false, false);
            printIndent(indent + 1, true, false);
            std.debug.print("] and \n", .{});
            printAST(arr_cass.value, indent + 2, true, false);
        },
        .simd_initializer => |simd_init| {
            std.debug.print("⚡ SIMD Initializer ({} elements)\n", .{simd_init.elements.items.len});
            for (simd_init.elements.items, 0..) |element, i| {
                const is_elem_last = i == simd_init.elements.items.len - 1;
                printAST(element, indent + 1, is_elem_last, false);
            }
        },
        .simd_index => |simd_idx| {
            std.debug.print("⚡ SIMD Index:\n", .{});
            printAST(simd_idx.simd, indent + 1, false, false);
            printIndent(indent + 1, true, false);
            std.debug.print("[\n", .{});
            printAST(simd_idx.index, indent + 2, true, false);
            std.debug.print("    ]\n", .{});
        },
        .simd_assignment => |simd_ass| {
            std.debug.print("⚡ SIMD Assignment:\n", .{});
            printAST(simd_ass.simd, indent + 1, false, false);
            printIndent(indent + 1, false, false);
            std.debug.print("[\n", .{});
            printAST(simd_ass.index, indent + 2, false, false);
            printIndent(indent + 1, true, false);
            std.debug.print("] = \n", .{});
            printAST(simd_ass.value, indent + 2, true, false);
        },
        .simd_compound_assignment => |simd_cass| {
            std.debug.print("⚡ SIMD Compound Assignment: \x1b[35m{c}= \x1b[0m\n", .{simd_cass.op});
            printAST(simd_cass.simd, indent + 1, false, false);
            printIndent(indent + 1, false, false);
            std.debug.print("[\n", .{});
            printAST(simd_cass.index, indent + 2, false, false);
            printIndent(indent + 1, true, false);
            std.debug.print("] and \n", .{});
            printAST(simd_cass.value, indent + 2, true, false);
        },
        .simd_method_call => |simd_method| {
            std.debug.print("⚡ SIMD Method Call: \x1b[32m{s}\x1b[0m\n", .{simd_method.method_name});
            std.debug.print("SIMD object:\n", .{});
            printAST(simd_method.simd, indent + 1, false, false);
            if (simd_method.args.items.len > 0) {
                std.debug.print("Arguments:\n", .{});
                for (simd_method.args.items, 0..) |arg, i| {
                    const is_arg_last = i == simd_method.args.items.len - 1;
                    printAST(arg, indent + 1, is_arg_last, false);
                }
            }
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
                std.debug.print("Field: \x1b[36m{s}\x1b[0m: \x1b[33m{s}\x1b[0m", .{ field.name, field.type_name });
                if (field.default_value) |default_val| {
                    std.debug.print(" = \n", .{});
                    printAST(default_val, indent + 2, true, false);
                } else {
                    std.debug.print("\n", .{});
                }
            }
        },
        .struct_initializer => |struct_init| {
            std.debug.print("🏗️  Struct Initializer: \x1b[32m{s}\x1b[0m ({} field values)\n", .{ struct_init.struct_name, struct_init.field_values.items.len });
            for (struct_init.field_values.items, 0..) |field_val, i| {
                const is_last_val = i == struct_init.field_values.items.len - 1;
                printIndent(indent + 1, is_last_val, false);
                if (field_val.field_name) |fname| {
                    std.debug.print("Field: \x1b[36m{s}\x1b[0m = \n", .{fname});
                } else {
                    std.debug.print("Field[{d}] = \n", .{i});
                }
                printAST(field_val.value, indent + 2, true, false);
            }
        },
        .cast => |c| {
            if (c.auto) {
                std.debug.print("🎯 Cast: auto\n", .{});
            } else if (c.type_name) |tn| {
                std.debug.print("🎯 Cast: \x1b[33m{s}\x1b[0m\n", .{tn});
            } else {
                std.debug.print("🎯 Cast\n", .{});
            }
            printAST(c.expr, indent + 1, true, false);
        },
        .expression_block => |block| {
            std.debug.print("🧩 Expression Block: \x1b[33m{s}\x1b[0m\n", .{block.type_name});
            for (block.statements.items) |stmt| {
                printAST(stmt, indent + 1, false, false);
            }
            printAST(block.result, indent + 1, true, false);
        },
        .error_decl => |error_decl| {
            switch (error_decl.code_kind) {
                .explicit => std.debug.print("🚨 Error Decl: \x1b[36m{s}\x1b[0m = {d}\n", .{ error_decl.name, error_decl.explicit_code }),
                .alias => std.debug.print("🚨 Error Decl: \x1b[36m{s}\x1b[0m = {s}\n", .{ error_decl.name, error_decl.alias_name orelse "_" }),
                .auto => std.debug.print("🚨 Error Decl: \x1b[36m{s}\x1b[0m = _\n", .{error_decl.name}),
            }
        },
        .send_stmt => |send_stmt| {
            std.debug.print("📤 Send: \x1b[36m{s}\x1b[0m\n", .{send_stmt.error_name});
        },
        .solicit_stmt => |solicit_stmt| {
            std.debug.print("🗣️ Solicit: \x1b[36m{s}\x1b[0m\n", .{solicit_stmt.error_name});
        },
        .handled_call_stmt => |handled_call| {
            std.debug.print("🛟 Handled Call\n", .{});
            const no_handlers = handled_call.handlers.items.len == 0;
            printAST(handled_call.call, indent + 1, no_handlers, false);
            for (handled_call.handlers.items, 0..) |handler, i| {
                const is_last_handler = i == handled_call.handlers.items.len - 1;
                printIndent(indent + 1, is_last_handler and handler.body.items.len == 0, false);
                const kind_text = switch (handler.kind) {
                    .send => "",
                    .solicit => "solicit ",
                };
                if (handler.error_name) |name| {
                    std.debug.print("on {s}{s}\n", .{ kind_text, name });
                } else if (handler.error_code) |code| {
                    std.debug.print("on {s}{d}\n", .{ kind_text, code });
                } else {
                    std.debug.print("on {s}_\n", .{kind_text});
                }
                for (handler.body.items, 0..) |stmt, j| {
                    const is_last_stmt = is_last_handler and j == handler.body.items.len - 1;
                    printAST(stmt, indent + 2, is_last_stmt, false);
                }
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
