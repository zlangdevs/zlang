const std = @import("std");
const ast = @import("ast.zig");
const errors = @import("../errors.zig");

var global_allocator: std.mem.Allocator = undefined;

// External functions from generated parser
extern fn yyparse() c_int;
extern fn zlang_lex_init(scanner: *?*anyopaque) c_int;
extern fn zlang_lex_destroy(scanner: ?*anyopaque) c_int;
extern fn zlang_set_in(file: ?*anyopaque, scanner: ?*anyopaque) void;
extern fn fmemopen(buffer: [*c]const u8, size: usize, mode: [*c]const u8) ?*anyopaque;
extern fn fclose(file: ?*anyopaque) c_int;

// Global variables used by Bison parser
export var current_scanner: ?*anyopaque = null;
extern var ast_root: ?*anyopaque;

const ParameterList = struct {
    items: std.ArrayList(ast.Parameter),

    fn init(allocator: std.mem.Allocator) ParameterList {
        return ParameterList{
            .items = std.ArrayList(ast.Parameter).init(allocator),
        };
    }
};

// Helper struct to manage lists
const NodeList = struct {
    items: std.ArrayList(*ast.Node),

    fn init(allocator: std.mem.Allocator) NodeList {
        return NodeList{
            .items = std.ArrayList(*ast.Node).init(allocator),
        };
    }
};

// C-callable functions for AST creation
export fn zig_create_program() ?*anyopaque {
    const program_data = ast.NodeData{
        .program = ast.Program{
            .functions = std.ArrayList(*ast.Node).init(global_allocator),
            .globals = std.ArrayList(*ast.Node).init(global_allocator),
        },
    };

    const node = ast.Node.create(global_allocator, program_data) catch return null;
    const node_ptr = @as(*anyopaque, @ptrCast(node));
    ast_root = node_ptr;
    return node_ptr;
}

export fn zig_create_parameter(name_ptr: [*c]const u8, type_ptr: [*c]const u8) ?*anyopaque {
    const name = std.mem.span(name_ptr);
    const type_name = std.mem.span(type_ptr);

    const name_copy = global_allocator.dupe(u8, name) catch return null;
    const type_copy = global_allocator.dupe(u8, type_name) catch return null;

    const param = global_allocator.create(ast.Parameter) catch return null;
    param.* = ast.Parameter{
        .name = name_copy,
        .type_name = type_copy,
    };

    return @as(*anyopaque, @ptrCast(param));
}

export fn zig_create_param_list() ?*anyopaque {
    const param_list = global_allocator.create(ParameterList) catch return null;
    param_list.* = ParameterList.init(global_allocator);
    return @as(*anyopaque, @ptrCast(param_list));
}

export fn zig_add_to_param_list(list_ptr: ?*anyopaque, param_ptr: ?*anyopaque) void {
    if (list_ptr == null or param_ptr == null) return;

    const param_list = @as(*ParameterList, @ptrFromInt(@intFromPtr(list_ptr.?)));
    const param = @as(*ast.Parameter, @ptrFromInt(@intFromPtr(param_ptr.?)));

    param_list.items.append(param.*) catch return;
}

export fn zig_create_function(name_ptr: [*c]const u8, return_type_ptr: [*c]const u8, params_ptr: ?*anyopaque, body_ptr: ?*anyopaque) ?*anyopaque {
    const name = std.mem.span(name_ptr);
    const return_type = std.mem.span(return_type_ptr);

    const name_copy = global_allocator.dupe(u8, name) catch return null;
    const return_type_copy = global_allocator.dupe(u8, return_type) catch return null;

    var parameters = std.ArrayList(ast.Parameter).init(global_allocator);
    if (params_ptr) |ptr| {
        const param_list = @as(*ParameterList, @ptrFromInt(@intFromPtr(ptr)));
        parameters = param_list.items;
    }

    var body = std.ArrayList(*ast.Node).init(global_allocator);
    if (body_ptr) |ptr| {
        const node_list = @as(*NodeList, @ptrFromInt(@intFromPtr(ptr)));
        body = node_list.items;
    }

    const function_data = ast.NodeData{
        .function = ast.Function{
            .name = name_copy,
            .return_type = return_type_copy,
            .parameters = parameters,
            .body = body,
        },
    };

    const node = ast.Node.create(global_allocator, function_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_comparison(op: u8, lhs_ptr: *anyopaque, rhs_ptr: *anyopaque) ?*anyopaque {
    const lhs = @as(*ast.Node, @ptrFromInt(@intFromPtr(lhs_ptr)));
    const rhs = @as(*ast.Node, @ptrFromInt(@intFromPtr(rhs_ptr)));

    const comp_data = ast.NodeData{
        .comparison = ast.Comparison{
            .op = op,
            .lhs = lhs,
            .rhs = rhs,
        },
    };

    const node = ast.Node.create(global_allocator, comp_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_binary_op(op: u8, lhs_ptr: *anyopaque, rhs_ptr: *anyopaque) ?*anyopaque {
    const lhs = @as(*ast.Node, @ptrFromInt(@intFromPtr(lhs_ptr)));
    const rhs = @as(*ast.Node, @ptrFromInt(@intFromPtr(rhs_ptr)));

    const binary_op_data = ast.NodeData{
        .binary_op = ast.BinaryOp{
            .op = op,
            .lhs = lhs,
            .rhs = rhs,
        },
    };

    const node = ast.Node.create(global_allocator, binary_op_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_assignment(target_ptr: ?*anyopaque, value_ptr: ?*anyopaque) ?*anyopaque {
    var target: ?*ast.Node = null;
    if (target_ptr) |ptr| {
        target = @as(*ast.Node, @ptrFromInt(@intFromPtr(ptr)));
    } else {
        return null;
    }
    var value: ?*ast.Node = null;
    if (value_ptr) |ptr| {
        value = @as(*ast.Node, @ptrFromInt(@intFromPtr(ptr)));
    } else {
        return null;
    }
    const assignment_data = ast.NodeData{
        .assignment = ast.Assignment{
            .target = target.?,
            .value = value.?,
        },
    };
    const node = ast.Node.create(global_allocator, assignment_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_var_decl(type_name_ptr: [*c]const u8, name_ptr: [*c]const u8, initializer_ptr: ?*anyopaque) ?*anyopaque {
    const type_name = std.mem.span(type_name_ptr);
    const name = std.mem.span(name_ptr);

    const type_name_copy = global_allocator.dupe(u8, type_name) catch return null;
    const name_copy = global_allocator.dupe(u8, name) catch return null;

    var initializer: ?*ast.Node = null;
    if (initializer_ptr) |ptr| {
        initializer = @as(*ast.Node, @ptrFromInt(@intFromPtr(ptr)));
    }

    const var_decl_data = ast.NodeData{
        .var_decl = ast.VarDecl{
            .type_name = type_name_copy,
            .name = name_copy,
            .initializer = initializer,
        },
    };

    const node = ast.Node.create(global_allocator, var_decl_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_function_call(name_ptr: [*c]const u8, is_libc: c_int, args_ptr: ?*anyopaque) ?*anyopaque {
    const name = std.mem.span(name_ptr);
    const name_copy = global_allocator.dupe(u8, name) catch return null;

    var args = std.ArrayList(*ast.Node).init(global_allocator);
    if (args_ptr) |ptr| {
        const node_list = @as(*NodeList, @ptrFromInt(@intFromPtr(ptr)));
        args = node_list.items;
    }

    const function_call_data = ast.NodeData{
        .function_call = ast.FunctionCall{
            .name = name_copy,
            .is_libc = is_libc != 0,
            .args = args,
        },
    };

    const node = ast.Node.create(global_allocator, function_call_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_return_stmt(expression_ptr: ?*anyopaque) ?*anyopaque {
    var expression: ?*ast.Node = null;
    if (expression_ptr) |ptr| {
        expression = @as(*ast.Node, @ptrFromInt(@intFromPtr(ptr)));
    }

    const return_stmt_data = ast.NodeData{
        .return_stmt = ast.ReturnStmt{
            .expression = expression,
        },
    };

    const node = ast.Node.create(global_allocator, return_stmt_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_identifier(name_ptr: [*c]const u8) ?*anyopaque {
    const name = std.mem.span(name_ptr);
    const name_copy = global_allocator.dupe(u8, name) catch return null;

    const identifier_data = ast.NodeData{
        .identifier = ast.Identifier{
            .name = name_copy,
        },
    };

    const node = ast.Node.create(global_allocator, identifier_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_qualified_identifier(base_ptr: ?*anyopaque, field_ptr: [*c]const u8) ?*anyopaque {
    var base: ?*ast.Node = null;
    if (base_ptr) |ptr| {
        base = @as(*ast.Node, @ptrFromInt(@intFromPtr(ptr)));
    } else {
        return null;
    }
    const field = std.mem.span(field_ptr);
    const field_copy = global_allocator.dupe(u8, field) catch return null;

    const qualified_identifier_data = ast.NodeData{
        .qualified_identifier = ast.QualifiedIdentifier{
            .base = base.?,
            .field = field_copy,
        },
    };

    const node = ast.Node.create(global_allocator, qualified_identifier_data) catch {
        global_allocator.free(field_copy);
        return null;
    };
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_unary_op(op: u8, operand_ptr: ?*anyopaque) ?*anyopaque {
    if (operand_ptr == null) return null;

    const operand = @as(*ast.Node, @ptrFromInt(@intFromPtr(operand_ptr.?)));

    const unary_op_data = ast.NodeData{
        .unary_op = ast.UnaryOp{
            .op = op,
            .operand = operand,
        },
    };

    const node = ast.Node.create(global_allocator, unary_op_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_float_literal(value_ptr: [*c]const u8) ?*anyopaque {
    const value = std.mem.span(value_ptr);
    const value_copy = global_allocator.dupe(u8, value) catch return null;

    const float_data = ast.NodeData{
        .float_literal = ast.FloatLiteral{
            .value = value_copy,
        },
    };

    const node = ast.Node.create(global_allocator, float_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_number_literal(value_ptr: [*c]const u8) ?*anyopaque {
    const value = std.mem.span(value_ptr);
    const value_copy = global_allocator.dupe(u8, value) catch return null;

    const number_data = ast.NodeData{
        .number_literal = ast.NumberLiteral{
            .value = value_copy,
        },
    };

    const node = ast.Node.create(global_allocator, number_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_char_literal(value: c_int) ?*anyopaque {
    const char_data = ast.NodeData{
        .char_literal = ast.CharLiteral{
            .value = value,
        },
    };

    const node = ast.Node.create(global_allocator, char_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_string_literal(value_ptr: [*c]const u8) ?*anyopaque {
    const value = std.mem.span(value_ptr);
    const value_copy = global_allocator.dupe(u8, value) catch return null;

    const string_data = ast.NodeData{
        .string_literal = ast.StringLiteral{
            .value = value_copy,
        },
    };

    const node = ast.Node.create(global_allocator, string_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

const EnumValueList = struct {
    items: std.ArrayList(ast.EnumValue),

    fn init(allocator: std.mem.Allocator) EnumValueList {
        return EnumValueList{
            .items = std.ArrayList(ast.EnumValue).init(allocator),
        };
    }
};

const StructFieldList = struct {
    items: std.ArrayList(ast.StructField),

    fn init(allocator: std.mem.Allocator) StructFieldList {
        return StructFieldList{
            .items = std.ArrayList(ast.StructField).init(allocator),
        };
    }
};

const StructFieldValueList = struct {
    items: std.ArrayList(ast.StructFieldValue),

    fn init(allocator: std.mem.Allocator) StructFieldValueList {
        return StructFieldValueList{
            .items = std.ArrayList(ast.StructFieldValue).init(allocator),
        };
    }
};

export fn zig_create_enum_value_list() ?*anyopaque {
    const enum_value_list = global_allocator.create(EnumValueList) catch return null;
    enum_value_list.* = EnumValueList.init(global_allocator);
    return @as(*anyopaque, @ptrCast(enum_value_list));
}

export fn zig_add_enum_value(list_ptr: ?*anyopaque, name_ptr: [*c]const u8, value_ptr: ?*anyopaque) void {
    if (list_ptr == null) return;
    const enum_value_list = @as(*EnumValueList, @ptrFromInt(@intFromPtr(list_ptr.?)));
    const name = std.mem.span(name_ptr);
    const name_copy = global_allocator.dupe(u8, name) catch return;
    var value: ?*ast.Node = null;
    if (value_ptr) |ptr| {
        value = @as(*ast.Node, @ptrFromInt(@intFromPtr(ptr)));
    }
    const enum_value = ast.EnumValue{
        .name = name_copy,
        .value = value,
    };

    enum_value_list.items.append(enum_value) catch return;
}

export fn zig_create_enum_decl(name_ptr: [*c]const u8, values_ptr: ?*anyopaque) ?*anyopaque {
    const name = std.mem.span(name_ptr);
    const name_copy = global_allocator.dupe(u8, name) catch return null;
    var values = std.ArrayList(ast.EnumValue).init(global_allocator);
    if (values_ptr) |ptr| {
        const enum_value_list = @as(*EnumValueList, @ptrFromInt(@intFromPtr(ptr)));
        values = enum_value_list.items;
    }
    const enum_decl_data = ast.NodeData{
        .enum_decl = ast.EnumDecl{
            .name = name_copy,
            .values = values,
        },
    };
    const node = ast.Node.create(global_allocator, enum_decl_data) catch {
        global_allocator.free(name_copy);
        return null;
    };
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_struct_field_list() ?*anyopaque {
    const struct_field_list = global_allocator.create(StructFieldList) catch return null;
    struct_field_list.* = StructFieldList.init(global_allocator);
    return @as(*anyopaque, @ptrCast(struct_field_list));
}

export fn zig_add_struct_field(list_ptr: ?*anyopaque, name_ptr: [*c]const u8, type_name_ptr: [*c]const u8) void {
    if (list_ptr == null) return;
    const struct_field_list = @as(*StructFieldList, @ptrFromInt(@intFromPtr(list_ptr.?)));
    const name = std.mem.span(name_ptr);
    const type_name = std.mem.span(type_name_ptr);
    const name_copy = global_allocator.dupe(u8, name) catch return;
    const type_name_copy = global_allocator.dupe(u8, type_name) catch {
        global_allocator.free(name_copy);
        return;
    };
    const struct_field = ast.StructField{
        .name = name_copy,
        .type_name = type_name_copy,
        .default_value = null,
    };

    struct_field_list.items.append(struct_field) catch return;
}

export fn zig_add_struct_field_with_default(list_ptr: ?*anyopaque, name_ptr: [*c]const u8, type_name_ptr: [*c]const u8, default_value_ptr: ?*anyopaque) void {
    if (list_ptr == null) return;
    const struct_field_list = @as(*StructFieldList, @ptrFromInt(@intFromPtr(list_ptr.?)));
    const name = std.mem.span(name_ptr);
    const type_name = std.mem.span(type_name_ptr);
    const name_copy = global_allocator.dupe(u8, name) catch return;
    const type_name_copy = global_allocator.dupe(u8, type_name) catch {
        global_allocator.free(name_copy);
        return;
    };
    var default_value: ?*ast.Node = null;
    if (default_value_ptr) |ptr| {
        default_value = @as(*ast.Node, @ptrFromInt(@intFromPtr(ptr)));
    }
    const struct_field = ast.StructField{
        .name = name_copy,
        .type_name = type_name_copy,
        .default_value = default_value,
    };

    struct_field_list.items.append(struct_field) catch return;
}

export fn zig_create_struct_decl(name_ptr: [*c]const u8, fields_ptr: ?*anyopaque) ?*anyopaque {
    const name = std.mem.span(name_ptr);
    const name_copy = global_allocator.dupe(u8, name) catch return null;
    var fields = std.ArrayList(ast.StructField).init(global_allocator);
    if (fields_ptr) |ptr| {
        const struct_field_list = @as(*StructFieldList, @ptrFromInt(@intFromPtr(ptr)));
        fields = struct_field_list.items;
    }
    const struct_decl_data = ast.NodeData{
        .struct_decl = ast.StructDecl{
            .name = name_copy,
            .fields = fields,
        },
    };
    const node = ast.Node.create(global_allocator, struct_decl_data) catch {
        global_allocator.free(name_copy);
        return null;
    };
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_struct_initializer(struct_name_ptr: [*c]const u8, field_values_ptr: ?*anyopaque) ?*anyopaque {
    const struct_name = std.mem.span(struct_name_ptr);
    const struct_name_copy = global_allocator.dupe(u8, struct_name) catch return null;
    var field_values = std.ArrayList(ast.StructFieldValue).init(global_allocator);
    if (field_values_ptr) |ptr| {
        const struct_field_value_list = @as(*StructFieldValueList, @ptrFromInt(@intFromPtr(ptr)));
        field_values = struct_field_value_list.items;
    }
    const struct_init_data = ast.NodeData{
        .struct_initializer = ast.StructInitializer{
            .struct_name = struct_name_copy,
            .field_values = field_values,
        },
    };
    const node = ast.Node.create(global_allocator, struct_init_data) catch {
        global_allocator.free(struct_name_copy);
        return null;
    };
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_struct_field_value_list() ?*anyopaque {
    const struct_field_value_list = global_allocator.create(StructFieldValueList) catch return null;
    struct_field_value_list.* = StructFieldValueList.init(global_allocator);
    return @as(*anyopaque, @ptrCast(struct_field_value_list));
}

export fn zig_add_struct_field_value(list_ptr: ?*anyopaque, field_name_ptr: [*c]const u8, value_ptr: ?*anyopaque) void {
    if (list_ptr == null or field_name_ptr == null or value_ptr == null) return;
    const struct_field_value_list = @as(*StructFieldValueList, @ptrFromInt(@intFromPtr(list_ptr.?)));
    const field_name = std.mem.span(field_name_ptr);
    const field_name_copy = global_allocator.dupe(u8, field_name) catch return;
    const value = @as(*ast.Node, @ptrFromInt(@intFromPtr(value_ptr.?)));
    const struct_field_value = ast.StructFieldValue{
        .field_name = field_name_copy,
        .value = value,
    };

    struct_field_value_list.items.append(struct_field_value) catch return;
}

export fn zig_create_bool_literal(value: c_int) ?*anyopaque {
    const bool_data = ast.NodeData{
        .bool_literal = ast.BoolLiteral{
            .value = value != 0,
        },
    };

    const node = ast.Node.create(global_allocator, bool_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_null_literal() ?*anyopaque {
    const null_data = ast.NodeData{
        .null_literal = ast.NullLiteral{},
    };

    const node = ast.Node.create(global_allocator, null_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_stmt_list() ?*anyopaque {
    const node_list = global_allocator.create(NodeList) catch return null;
    node_list.* = NodeList.init(global_allocator);
    return @as(*anyopaque, @ptrCast(node_list));
}

export fn zig_create_arg_list() ?*anyopaque {
    const node_list = global_allocator.create(NodeList) catch return null;
    node_list.* = NodeList.init(global_allocator);
    return @as(*anyopaque, @ptrCast(node_list));
}

export fn zig_add_to_program(program_ptr: ?*anyopaque, function_ptr: ?*anyopaque) void {
    if (program_ptr == null or function_ptr == null) return;

    const program_node = @as(*ast.Node, @ptrFromInt(@intFromPtr(program_ptr.?)));
    const function_node = @as(*ast.Node, @ptrFromInt(@intFromPtr(function_ptr.?)));

    switch (program_node.data) {
        .program => |*prog| {
            prog.functions.append(function_node) catch return;
        },
        else => return,
    }
}

export fn zig_add_global_to_program(program_ptr: ?*anyopaque, global_ptr: ?*anyopaque) void {
    if (program_ptr == null or global_ptr == null) return;

    const program_node = @as(*ast.Node, @ptrFromInt(@intFromPtr(program_ptr.?)));
    const global_node = @as(*ast.Node, @ptrFromInt(@intFromPtr(global_ptr.?)));

    switch (program_node.data) {
        .program => |*prog| {
            prog.globals.append(global_node) catch return;
        },
        else => return,
    }
}

export fn zig_add_to_stmt_list(list_ptr: ?*anyopaque, stmt_ptr: ?*anyopaque) void {
    if (list_ptr == null or stmt_ptr == null) return;

    const node_list = @as(*NodeList, @ptrFromInt(@intFromPtr(list_ptr.?)));
    const stmt_node = @as(*ast.Node, @ptrFromInt(@intFromPtr(stmt_ptr.?)));

    node_list.items.append(stmt_node) catch return;
}

export fn zig_add_to_arg_list(list_ptr: ?*anyopaque, arg_ptr: ?*anyopaque) void {
    if (list_ptr == null or arg_ptr == null) return;

    const node_list = @as(*NodeList, @ptrFromInt(@intFromPtr(list_ptr.?)));
    const arg_node = @as(*ast.Node, @ptrFromInt(@intFromPtr(arg_ptr.?)));

    node_list.items.append(arg_node) catch return;
}

export fn zig_create_brainfuck(code_ptr: [*c]const u8) ?*anyopaque {
    const code = std.mem.span(code_ptr);
    const code_copy = global_allocator.dupe(u8, code) catch return null;
    const brainfuck_data = ast.NodeData{
        .brainfuck = ast.Brainfuck{
            .code = code_copy,
        },
    };
    const node = ast.Node.create(global_allocator, brainfuck_data) catch {
        global_allocator.free(code_copy);
        return null;
    };
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_if_stmt(condition_ptr: ?*anyopaque, then_body_ptr: ?*anyopaque, else_body_ptr: ?*anyopaque) ?*anyopaque {
    if (condition_ptr == null or then_body_ptr == null) return null;

    const condition = @as(*ast.Node, @ptrFromInt(@intFromPtr(condition_ptr.?)));
    const then_node_list = @as(*NodeList, @ptrFromInt(@intFromPtr(then_body_ptr.?)));
    const then_body = then_node_list.items;

    var else_body_opt: ?std.ArrayList(*ast.Node) = null;
    if (else_body_ptr) |ptr| {
        const else_node_list = @as(*NodeList, @ptrFromInt(@intFromPtr(ptr)));
        else_body_opt = else_node_list.items;
    }

    const if_data = ast.NodeData{
        .if_stmt = ast.IfStmt{
            .condition = condition,
            .then_body = then_body,
            .else_body = else_body_opt,
        },
    };

    const node = ast.Node.create(global_allocator, if_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_for_stmt(condition_ptr: ?*anyopaque, body_ptr: ?*anyopaque) ?*anyopaque {
    var condition: ?*ast.Node = null;
    if (condition_ptr) |ptr| {
        condition = @as(*ast.Node, @ptrFromInt(@intFromPtr(ptr)));
    }

    var body = std.ArrayList(*ast.Node).init(global_allocator);
    if (body_ptr) |ptr| {
        const node_list = @as(*NodeList, @ptrFromInt(@intFromPtr(ptr)));
        body = node_list.items;
    }

    const for_data = ast.NodeData{
        .for_stmt = ast.ForStmt{
            .condition = condition,
            .body = body,
        },
    };

    const node = ast.Node.create(global_allocator, for_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_c_for_stmt(init_ptr: ?*anyopaque, cond_ptr: ?*anyopaque, inc_ptr: ?*anyopaque, body_ptr: ?*anyopaque) ?*anyopaque {
    var init: ?*ast.Node = null;
    var cond: ?*ast.Node = null;
    var increment: ?*ast.Node = null;

    if (init_ptr) |ptr| init = @as(*ast.Node, @ptrFromInt(@intFromPtr(ptr)));
    if (cond_ptr) |ptr| cond = @as(*ast.Node, @ptrFromInt(@intFromPtr(ptr)));
    if (inc_ptr) |ptr| increment = @as(*ast.Node, @ptrFromInt(@intFromPtr(ptr)));

    var body = std.ArrayList(*ast.Node).init(global_allocator);
    if (body_ptr) |ptr| {
        const node_list = @as(*NodeList, @ptrFromInt(@intFromPtr(ptr)));
        body = node_list.items;
    }

    const c_for_data = ast.NodeData{
        .c_for_stmt = ast.CForStmt{
            .init = init,
            .condition = cond,
            .increment = increment,
            .body = body,
        },
    };

    const node = ast.Node.create(global_allocator, c_for_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_break_stmt() ?*anyopaque {
    const break_data = ast.NodeData{
        .break_stmt = ast.BreakStmt{},
    };

    const node = ast.Node.create(global_allocator, break_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_continue_stmt() ?*anyopaque {
    const continue_data = ast.NodeData{
        .continue_stmt = ast.ContinueStmt{},
    };

    const node = ast.Node.create(global_allocator, continue_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_array_initializer(elements_ptr: ?*anyopaque) ?*anyopaque {
    var elements = std.ArrayList(*ast.Node).init(global_allocator);
    if (elements_ptr) |ptr| {
        const node_list = @as(*NodeList, @ptrFromInt(@intFromPtr(ptr)));
        elements = node_list.items;
    }

    const array_init_data = ast.NodeData{
        .array_initializer = ast.ArrayInitializer{
            .elements = elements,
        },
    };

    const node = ast.Node.create(global_allocator, array_init_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_array_index(array_ptr: ?*anyopaque, index_ptr: ?*anyopaque) ?*anyopaque {
    if (array_ptr == null or index_ptr == null) return null;
    const array = @as(*ast.Node, @ptrFromInt(@intFromPtr(array_ptr.?)));
    const index = @as(*ast.Node, @ptrFromInt(@intFromPtr(index_ptr.?)));

    const array_index_data = ast.NodeData{
        .array_index = ast.ArrayIndex{
            .array = array,
            .index = index,
        },
    };

    const node = ast.Node.create(global_allocator, array_index_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_array_assignment(array_ptr: ?*anyopaque, index_ptr: ?*anyopaque, value_ptr: ?*anyopaque) ?*anyopaque {
    if (array_ptr == null or index_ptr == null or value_ptr == null) return null;
    const array = @as(*ast.Node, @ptrFromInt(@intFromPtr(array_ptr.?)));
    const index = @as(*ast.Node, @ptrFromInt(@intFromPtr(index_ptr.?)));
    const value = @as(*ast.Node, @ptrFromInt(@intFromPtr(value_ptr.?)));

    const array_assignment_data = ast.NodeData{
        .array_assignment = ast.ArrayAssignment{
            .array = array,
            .index = index,
            .value = value,
        },
    };

    const node = ast.Node.create(global_allocator, array_assignment_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_array_compound_assignment(array_ptr: ?*anyopaque, index_ptr: ?*anyopaque, value_ptr: ?*anyopaque, op: c_int) ?*anyopaque {
    if (array_ptr == null or index_ptr == null or value_ptr == null) return null;
    const array = @as(*ast.Node, @ptrFromInt(@intFromPtr(array_ptr.?)));
    const index = @as(*ast.Node, @ptrFromInt(@intFromPtr(index_ptr.?)));
    const value = @as(*ast.Node, @ptrFromInt(@intFromPtr(value_ptr.?)));

    const array_compound_assignment_data = ast.NodeData{
        .array_compound_assignment = ast.ArrayCompoundAssignment{
            .array = array,
            .index = index,
            .value = value,
            .op = @as(u8, @intCast(op)),
        },
    };

    const node = ast.Node.create(global_allocator, array_compound_assignment_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_method_call(object_ptr: ?*anyopaque, method_name_ptr: [*c]const u8, args_ptr: ?*anyopaque) ?*anyopaque {
    if (object_ptr == null or method_name_ptr == null) return null;

    const object = @as(*ast.Node, @ptrFromInt(@intFromPtr(object_ptr.?)));
    const method_name = std.mem.span(method_name_ptr);
    const method_name_copy = global_allocator.dupe(u8, method_name) catch return null;

    var args = std.ArrayList(*ast.Node).init(global_allocator);
    if (args_ptr) |ptr| {
        const node_list = @as(*NodeList, @ptrFromInt(@intFromPtr(ptr)));
        args = node_list.items;
    }

    const method_call_data = ast.NodeData{
        .method_call = ast.MethodCall{
            .object = object,
            .method_name = method_name_copy,
            .args = args,
        },
    };

    const node = ast.Node.create(global_allocator, method_call_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_c_function_decl(name_ptr: [*c]const u8, return_type_ptr: [*c]const u8, params_ptr: ?*anyopaque) ?*anyopaque {
    const name = std.mem.span(name_ptr);
    const return_type = std.mem.span(return_type_ptr);

    const name_copy = global_allocator.dupe(u8, name) catch return null;
    const return_type_copy = global_allocator.dupe(u8, return_type) catch return null;

    var parameters = std.ArrayList(ast.Parameter).init(global_allocator);
    if (params_ptr) |ptr| {
        const param_list = @as(*ParameterList, @ptrFromInt(@intFromPtr(ptr)));
        parameters = param_list.items;
    }

    const c_function_decl_data = ast.NodeData{
        .c_function_decl = ast.CFunctionDecl{
            .name = name_copy,
            .return_type = return_type_copy,
            .parameters = parameters,
        },
    };

    const node = ast.Node.create(global_allocator, c_function_decl_data) catch return null;
    return @as(*anyopaque, @ptrCast(node));
}

export fn zig_create_use_stmt(module_path_ptr: [*c]const u8) ?*anyopaque {
    const module_path = std.mem.span(module_path_ptr);
    const module_path_copy = global_allocator.dupe(u8, module_path) catch return null;

    const use_stmt_data = ast.NodeData{
        .use_stmt = ast.UseStmt{
            .module_path = module_path_copy,
        },
    };

    const node = ast.Node.create(global_allocator, use_stmt_data) catch {
        global_allocator.free(module_path_copy);
        return null;
    };
    return @as(*anyopaque, @ptrCast(node));
}

pub fn parse(allocator: std.mem.Allocator, input: []const u8) errors.ParseError!?*ast.Node {
    global_allocator = allocator;
    ast_root = null;

    // Initialize scanner
    if (zlang_lex_init(&current_scanner) != 0) {
        return errors.ParseError.LexerInitFailed;
    }
    defer _ = zlang_lex_destroy(current_scanner);

    // Create null-terminated input
    const null_terminated_input = try allocator.dupeZ(u8, input);
    defer allocator.free(null_terminated_input);

    // Create file handle from memory
    const file = fmemopen(null_terminated_input.ptr, input.len, "r");
    if (file == null) {
        return errors.ParseError.FileOpenFailed;
    }
    defer _ = fclose(file);

    // Set input for scanner
    zlang_set_in(file, current_scanner);

    // Parse
    const result = yyparse();
    if (result != 0) {
        return errors.ParseError.ParseFailed;
    }

    if (ast_root) |root_ptr| {
        return @as(*ast.Node, @ptrFromInt(@intFromPtr(root_ptr)));
    }
    return null;
}
