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
        },
    };

    const node = ast.Node.create(global_allocator, program_data) catch return null;
    const node_ptr = @as(*anyopaque, @ptrCast(node));
    ast_root = node_ptr;
    return node_ptr;
}

export fn zig_create_function(name_ptr: [*c]const u8, return_type_ptr: [*c]const u8, body_ptr: ?*anyopaque) ?*anyopaque {
    const name = std.mem.span(name_ptr);
    const return_type = std.mem.span(return_type_ptr);

    const name_copy = global_allocator.dupe(u8, name) catch return null;
    const return_type_copy = global_allocator.dupe(u8, return_type) catch return null;

    var body = std.ArrayList(*ast.Node).init(global_allocator);
    if (body_ptr) |ptr| {
        const node_list = @as(*NodeList, @ptrFromInt(@intFromPtr(ptr)));
        body = node_list.items;
    }

    const function_data = ast.NodeData{
        .function = ast.Function{
            .name = name_copy,
            .return_type = return_type_copy,
            .body = body,
        },
    };

    const node = ast.Node.create(global_allocator, function_data) catch return null;
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

export fn zig_create_assignment(name_ptr: [*c]const u8, value_ptr: ?*anyopaque) ?*anyopaque {
    const name = std.mem.span(name_ptr);
    const name_copy = global_allocator.dupe(u8, name) catch return null;
    var value: ?*ast.Node = null;
    if (value_ptr) |ptr| {
        value = @as(*ast.Node, @ptrFromInt(@intFromPtr(ptr)));
    } else {
        return null;
    }
    const assignment_data = ast.NodeData{
        .assignment = ast.Assignment{
            .name = name_copy,
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

export fn zig_create_bool_literal(value: c_int) ?*anyopaque {
    const bool_data = ast.NodeData{
        .bool_literal = ast.BoolLiteral{
            .value = value != 0,
        },
    };

    const node = ast.Node.create(global_allocator, bool_data) catch return null;
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
