const std = @import("std");
const utils = @import("utils.zig");
const codegen = @import("llvm.zig").CodeGenerator;
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");
const structs = @import("structs.zig");

const c_bindings = @import("c_bindings.zig");
const c = c_bindings.c;

const BfLoadRequest = struct {
    var_name: []const u8,
    load_idx: i32,
    array_index: ?u32 = null,
};

const BrainfuckContext = struct {
    cell_size: i32 = 8,
    cell_signed: bool = false,
    len: i32 = 100,
    requests: std.ArrayList(BfLoadRequest),
    code: []const u8,

    pub fn print(self: *const BrainfuckContext) void {
        const stdout = std.io.getStdOut().writer();
        stdout.print("🧠 Brainfuck Context 🧠\n", .{}) catch return;
        stdout.print("======================\n", .{}) catch return;
        stdout.print("🔢 Cell size: {d} bits\n", .{self.cell_size}) catch return;
        stdout.print("🔀 Signed cells: {s}\n", .{if (self.cell_signed) "✅ yes" else "❌ no"}) catch return;
        stdout.print("📏 Tape length: {d}\n", .{self.len}) catch return;

        if (self.requests.items.len > 0) {
            stdout.print("\n📦 Load requests:\n", .{}) catch return;
            for (self.requests.items) |req| {
                stdout.print("  - 🏷️ '{s}' at index {d}\n", .{ req.var_name, req.load_idx }) catch return;
            }
        } else {
            stdout.print("\n📦 No load requests\n", .{}) catch return;
        }
        stdout.print("\n💻 Brainfuck code:\n", .{}) catch return;
        if (self.code.len > 0) {
            stdout.print("{s}\n", .{self.code}) catch return;
        } else {
            stdout.print("(no code)\n", .{}) catch return;
        }
        stdout.print("======================\n", .{}) catch return;
    }
};

fn ParseBfContext(allocator: std.mem.Allocator, input: []const u8) BrainfuckContext {
    var ctx = BrainfuckContext{
        .requests = std.ArrayList(BfLoadRequest){},
        .code = "",
    };
    var search_pos: usize = 0;
    while (search_pos < input.len) {
        const line_start = search_pos;
        const newline_pos = std.mem.indexOfScalar(u8, input[line_start..], '\n');
        const line_end = if (newline_pos) |pos| line_start + pos else input.len;
        const line = input[line_start..line_end];
        search_pos = line_end + 1;
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0) continue;
        if (trimmed[0] != '?') {
            ctx.code = std.mem.trim(u8, input[line_start..], " \t\r\n");
            break;
        }
        var line_pos: usize = 0;
        while (line_pos < trimmed.len) {
            const directive_start = std.mem.indexOfScalarPos(u8, trimmed, line_pos, '?');
            if (directive_start == null) break;
            const start_pos = directive_start.?;
            if (start_pos >= trimmed.len) break;
            const directive_end = std.mem.indexOfScalarPos(u8, trimmed, start_pos + 1, '?');
            if (directive_end == null) break;

            const end_pos = directive_end.?;
            if (end_pos <= start_pos + 1) {
                line_pos = end_pos + 1;
                continue;
            }
            const content = trimmed[start_pos + 1 .. end_pos];
            const content_trimmed = std.mem.trim(u8, content, " \t");
            var parts = std.mem.splitSequence(u8, content_trimmed, " ");
            const arg_name = parts.next() orelse {
                line_pos = end_pos + 1;
                continue;
            };
            const arg_value = parts.rest();
            const arg_value_trimmed = std.mem.trim(u8, arg_value, " \t");
            if (std.mem.eql(u8, arg_name, "cell_size")) {
                ctx.cell_size = std.fmt.parseInt(i32, arg_value_trimmed, 10) catch ctx.cell_size;
            } else if (std.mem.eql(u8, arg_name, "cell_signed")) {
                ctx.cell_signed = std.mem.eql(u8, arg_value_trimmed, "true");
            } else if (std.mem.eql(u8, arg_name, "len")) {
                ctx.len = std.fmt.parseInt(i32, arg_value_trimmed, 10) catch ctx.len;
            } else if (std.mem.eql(u8, arg_name, "load")) {
                var load_parts = std.mem.splitSequence(u8, arg_value_trimmed, " ");
                const var_name = load_parts.next() orelse {
                    line_pos = end_pos + 1;
                    continue;
                };
                const load_idx_str = load_parts.next() orelse {
                    line_pos = end_pos + 1;
                    continue;
                };
                if (std.fmt.parseInt(i32, std.mem.trim(u8, load_idx_str, " \t"), 10)) |load_idx| {
                    if (try expandArrayLoad(allocator, &ctx, var_name, load_idx)) {} else {
                        ctx.requests.append(allocator, .{
                            .var_name = var_name,
                            .load_idx = load_idx,
                        }) catch {};
                    }
                } else |_| {}
            }
            line_pos = end_pos + 1;
        }
    }
    return ctx;
}

fn expandArrayLoad(allocator: std.mem.Allocator, ctx: *BrainfuckContext, var_name: []const u8, start_pos: i32) !bool {
    _ = allocator;
    _ = ctx;
    _ = var_name;
    _ = start_pos;
    return false;
}

fn expandArrayLoads(cg: *codegen, ctx: *BrainfuckContext) !void {
    var new_requests = std.ArrayList(BfLoadRequest){};
    defer new_requests.deinit(cg.allocator);

    for (ctx.requests.items) |req| {
        const var_info = cg.getVariable(req.var_name) orelse {
            try new_requests.append(cg.allocator, req);
            continue;
        };

        const type_kind = c.LLVMGetTypeKind(var_info.type_ref);
        if (type_kind == c.LLVMArrayTypeKind) {
            const element_type = c.LLVMGetElementType(var_info.type_ref);
            const array_length = c.LLVMGetArrayLength(var_info.type_ref);
            const element_type_kind = c.LLVMGetTypeKind(element_type);

            if (element_type_kind == c.LLVMIntegerTypeKind) {
                const element_size_bits = c.LLVMGetIntTypeWidth(element_type);
                const cells_per_element: i32 = @intCast((element_size_bits + @as(c_uint, @intCast(ctx.cell_size)) - 1) / @as(c_uint, @intCast(ctx.cell_size)));
                var i: u32 = 0;
                while (i < array_length) : (i += 1) {
                    const element_pos = req.load_idx + @as(i32, @intCast(i)) * cells_per_element;

                    try new_requests.append(cg.allocator, .{
                        .var_name = req.var_name,
                        .load_idx = element_pos,
                        .array_index = i,
                    });
                }
            } else {
                try new_requests.append(cg.allocator, req);
            }
        } else {
            try new_requests.append(cg.allocator, req);
        }
    }

    ctx.requests.deinit(cg.allocator);
    ctx.requests = new_requests;
    new_requests = std.ArrayList(BfLoadRequest){};
}

const LinearLoopOp = struct {
    offset: i32,
    factor: i32,
};

const BfOp = union(enum) {
    inc_ptr: i32,
    add_val: i32,
    output,
    input,
    loop_start,
    loop_end,
    set_zero,
    linear_loop: std.ArrayList(LinearLoopOp),
};

fn parseBrainfuckOps(allocator: std.mem.Allocator, code: []const u8) !std.ArrayList(BfOp) {
    var ops = std.ArrayList(BfOp){};
    errdefer ops.deinit(allocator);

    for (code) |char| {
        switch (char) {
            '>' => try ops.append(allocator, .{ .inc_ptr = 1 }),
            '<' => try ops.append(allocator, .{ .inc_ptr = -1 }),
            '+' => try ops.append(allocator, .{ .add_val = 1 }),
            '-' => try ops.append(allocator, .{ .add_val = -1 }),
            '.' => try ops.append(allocator, .output),
            ',' => try ops.append(allocator, .input),
            '[' => try ops.append(allocator, .loop_start),
            ']' => try ops.append(allocator, .loop_end),
            else => {},
        }
    }
    return ops;
}

fn checkLinearLoop(allocator: std.mem.Allocator, body: []const BfOp) !?std.ArrayList(LinearLoopOp) {
    var effects = std.AutoHashMapUnmanaged(i32, i32){};
    defer effects.deinit(allocator);

    var ptr_offset: i32 = 0;
    for (body) |op| {
        switch (op) {
            .inc_ptr => |val| ptr_offset += val,
            .add_val => |val| {
                const entry = try effects.getOrPut(allocator, ptr_offset);
                if (!entry.found_existing) entry.value_ptr.* = 0;
                entry.value_ptr.* += val;
            },
            else => return null,
        }
    }

    if (ptr_offset != 0) return null;

    const start_change = effects.get(0) orelse return null;
    if (start_change != -1) return null;

    var factors = std.ArrayList(LinearLoopOp){};
    errdefer factors.deinit(allocator);

    var it = effects.iterator();
    while (it.next()) |entry| {
        const offset = entry.key_ptr.*;
        const factor = entry.value_ptr.*;
        if (offset == 0) continue;
        if (factor != 0) {
            try factors.append(allocator, .{ .offset = offset, .factor = factor });
        }
    }
    return factors;
}

fn freeBfOps(allocator: std.mem.Allocator, ops: []const BfOp) void {
    for (ops) |op| {
        switch (op) {
            .linear_loop => |list| {
                var mut_list = list;
                mut_list.deinit(allocator);
            },
            else => {},
        }
    }
}

fn optimizeOps(allocator: std.mem.Allocator, initial_ops: []const BfOp) !std.ArrayList(BfOp) {
    var contracted = std.ArrayList(BfOp){};
    defer contracted.deinit(allocator);
    var i: usize = 0;
    while (i < initial_ops.len) {
        const op = initial_ops[i];
        switch (op) {
            .inc_ptr => |val| {
                var total = val;
                var j = i + 1;
                while (j < initial_ops.len) : (j += 1) {
                    if (initial_ops[j] == .inc_ptr) {
                        total += initial_ops[j].inc_ptr;
                    } else {
                        break;
                    }
                }
                if (total != 0) {
                    try contracted.append(allocator, .{ .inc_ptr = total });
                }
                i = j;
            },
            .add_val => |val| {
                var total = val;
                var j = i + 1;
                while (j < initial_ops.len) : (j += 1) {
                    if (initial_ops[j] == .add_val) {
                        total += initial_ops[j].add_val;
                    } else {
                        break;
                    }
                }
                if (total != 0) {
                    try contracted.append(allocator, .{ .add_val = total });
                }
                i = j;
            },
            else => {
                try contracted.append(allocator, op);
                i += 1;
            },
        }
    }
    var optimized = std.ArrayList(BfOp){};
    errdefer {
        freeBfOps(allocator, optimized.items);
        optimized.deinit(allocator);
    }
    i = 0;
    const ops = contracted.items;
    while (i < ops.len) {
        const op = ops[i];
        if (op == .loop_start) {
            if (i + 2 < ops.len and ops[i + 1] == .add_val and ops[i + 2] == .loop_end) {
                const val = ops[i + 1].add_val;
                if (val == 1 or val == -1) {
                    try optimized.append(allocator, .set_zero);
                    i += 3;
                    continue;
                }
            }
            var depth: i32 = 1;
            var j = i + 1;
            var possible_linear = true;
            while (j < ops.len) : (j += 1) {
                if (ops[j] == .loop_start) {
                    depth += 1;
                    possible_linear = false;
                } else if (ops[j] == .loop_end) {
                    depth -= 1;
                }
                if (depth == 0) break;
            }
            if (depth == 0 and possible_linear) {
                const body = ops[i + 1 .. j];
                if (try checkLinearLoop(allocator, body)) |factors| {
                    try optimized.append(allocator, .{ .linear_loop = factors });
                    i = j + 1;
                    continue;
                }
            }
        }
        try optimized.append(allocator, op);
        i += 1;
    }

    return optimized;
}

pub fn generateBrainfuck(cg: *codegen, bf: ast.Brainfuck) !c.LLVMValueRef {
    var ctx = ParseBfContext(cg.allocator, bf.code);
    defer {
        freeExpandedRequests(cg.allocator, ctx.requests.items);
        ctx.requests.deinit(cg.allocator);
    }

    try expandArrayLoads(cg, &ctx);
    var ops = try parseBrainfuckOps(cg.allocator, ctx.code);
    defer ops.deinit(cg.allocator);
    var optimized_ops = try optimizeOps(cg.allocator, ops.items);
    defer {
        freeBfOps(cg.allocator, optimized_ops.items);
        optimized_ops.deinit(cg.allocator);
    }

    const current_function = cg.current_function orelse return errors.CodegenError.TypeMismatch;

    // --- 1. Setup Brainfuck environment ---
    const cell_type = c.LLVMIntTypeInContext(cg.context, @intCast(ctx.cell_size));
    const i32_type = c.LLVMInt32TypeInContext(cg.context);
    const i8_type = c.LLVMInt8TypeInContext(cg.context);
    const i8_ptr_type = c.LLVMPointerType(i8_type, 0);
    const size_t_type = utils.libcTypeToLLVM(cg, .size_t_type);

    const tape_len_val = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(ctx.len)), 0);
    const tape = c.LLVMBuildArrayAlloca(cg.builder, cell_type, tape_len_val, "bf_tape");

    const ptr = c.LLVMBuildAlloca(cg.builder, i32_type, "bf_ptr");
    _ = c.LLVMBuildStore(cg.builder, c.LLVMConstInt(i32_type, 0, 0), ptr);

    // Initialize tape with zeros
    const memset_func = try cg.declareLibcFunction("memset");
    const tape_i8_ptr = c.LLVMBuildBitCast(cg.builder, tape, i8_ptr_type, "tape_i8_ptr");
    const cell_size_bytes = @divTrunc(ctx.cell_size, 8);
    const tape_size_bytes = ctx.len * cell_size_bytes;
    const tape_size_val = c.LLVMConstInt(size_t_type, @as(c_ulonglong, @intCast(tape_size_bytes)), 0);
    var memset_args: [3]c.LLVMValueRef = .{
        tape_i8_ptr,
        c.LLVMConstInt(i32_type, 0, 0),
        tape_size_val,
    };
    _ = c.LLVMBuildCall2(cg.builder, c.LLVMGlobalGetValueType(memset_func), memset_func, &memset_args[0], 3, "");

    // --- 2. Load variables into tape ---
    for (ctx.requests.items) |req| {
        // Handle array element access using array_index field
        const var_info = if (req.array_index) |index| blk: {
            const array_var_info = cg.getVariable(req.var_name) orelse return errors.CodegenError.UndefinedVariable;
            const array_type_kind = c.LLVMGetTypeKind(array_var_info.type_ref);
            if (array_type_kind != c.LLVMArrayTypeKind) return errors.CodegenError.TypeMismatch;

            const element_type = c.LLVMGetElementType(array_var_info.type_ref);
            const array_length = c.LLVMGetArrayLength(array_var_info.type_ref);
            if (index >= array_length) return errors.CodegenError.UndefinedVariable;

            // Get pointer to the specific array element
            const index_val = c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), @as(c_ulonglong, @intCast(index)), 0);
            var gep_indices: [2]c.LLVMValueRef = .{
                c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0),
                index_val,
            };
            const element_ptr = c.LLVMBuildGEP2(cg.builder, array_var_info.type_ref, array_var_info.value, &gep_indices[0], 2, "array_element_ptr");

            break :blk structs.VariableInfo{
                .value = element_ptr,
                .type_ref = element_type,
                .type_name = array_var_info.type_name, // Use array's type name for now
            };
        } else cg.getVariable(req.var_name) orelse {
            return errors.CodegenError.UndefinedVariable;
        };

        const type_kind = c.LLVMGetTypeKind(var_info.type_ref);
        if (type_kind != c.LLVMIntegerTypeKind) {
            return errors.CodegenError.TypeMismatch;
        }

        const var_size_bits = c.LLVMGetIntTypeWidth(var_info.type_ref);
        const var_value = c.LLVMBuildLoad2(cg.builder, var_info.type_ref, var_info.value, "var_value");

        // Calculate how many cells this variable needs
        const cells_needed: usize = (var_size_bits + @as(c_uint, @intCast(ctx.cell_size)) - 1) / @as(c_uint, @intCast(ctx.cell_size));

        if (cells_needed == 1) {
            // Variable fits in one cell - store directly
            const cell_index = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(req.load_idx)), 0);
            var gep_indices: [1]c.LLVMValueRef = .{cell_index};
            const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &gep_indices[0], 1, "cell_ptr");

            const cell_value = if (var_size_bits == ctx.cell_size)
                var_value
            else if (var_size_bits < ctx.cell_size)
                c.LLVMBuildZExt(cg.builder, var_value, cell_type, "zext_to_cell")
            else
                c.LLVMBuildTrunc(cg.builder, var_value, cell_type, "trunc_to_cell");

            _ = c.LLVMBuildStore(cg.builder, cell_value, cell_ptr);
        } else {
            // Variable needs multiple cells - split by cell_size chunks (big-endian)
            var cell_idx: usize = 0;
            while (cell_idx < cells_needed) : (cell_idx += 1) {
                const cell_index = req.load_idx + @as(i32, @intCast(cell_idx));
                const byte_index = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(cell_index)), 0);
                var gep_indices: [1]c.LLVMValueRef = .{byte_index};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &gep_indices[0], 1, "cell_ptr");

                // Extract the chunk for this cell (big-endian: most significant chunk first)
                const chunk_pos = cells_needed - 1 - cell_idx;
                const shift_amount = c.LLVMConstInt(var_info.type_ref, @intCast(chunk_pos * @as(usize, @intCast(ctx.cell_size))), 0);
                var cell_value: c.LLVMValueRef = undefined;
                if (@as(c_uint, @intCast(chunk_pos * @as(usize, @intCast(ctx.cell_size)))) >= var_size_bits) {
                    // This chunk is beyond the variable size, store 0
                    cell_value = c.LLVMConstInt(cell_type, 0, 0);
                } else {
                    const shifted_value = c.LLVMBuildLShr(cg.builder, var_value, shift_amount, "shifted");
                    cell_value = c.LLVMBuildTrunc(cg.builder, shifted_value, cell_type, "chunk");
                }

                _ = c.LLVMBuildStore(cg.builder, cell_value, cell_ptr);
            }
        }
    }

    // --- 3. Execute Brainfuck code ---
    const putchar_func = try cg.declareLibcFunction("putchar");
    const getchar_func = try cg.declareLibcFunction("getchar");

    var loop_stack = std.ArrayList(struct { cond: c.LLVMBasicBlockRef, exit: c.LLVMBasicBlockRef }){};
    defer loop_stack.deinit(cg.allocator);

    const cell_zero = c.LLVMConstInt(cell_type, 0, 0);

    for (optimized_ops.items) |op| {
        switch (op) {
            .inc_ptr => |val| {
                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                const offset = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(@abs(val))), 0);
                const new_ptr_val = if (val > 0)
                    c.LLVMBuildAdd(cg.builder, ptr_val, offset, "inc_ptr")
                else
                    c.LLVMBuildSub(cg.builder, ptr_val, offset, "dec_ptr");
                _ = c.LLVMBuildStore(cg.builder, new_ptr_val, ptr);
            },
            .add_val => |val| {
                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                var indices: [1]c.LLVMValueRef = .{ptr_val};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                const cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "cell_val");

                const diff = c.LLVMConstInt(cell_type, @as(c_ulonglong, @intCast(@abs(val))), 0);
                const new_cell_val = if (val > 0)
                    c.LLVMBuildAdd(cg.builder, cell_val, diff, "inc_cell")
                else
                    c.LLVMBuildSub(cg.builder, cell_val, diff, "dec_cell");

                _ = c.LLVMBuildStore(cg.builder, new_cell_val, cell_ptr);
            },
            .output => {
                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                var indices: [1]c.LLVMValueRef = .{ptr_val};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                const cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "cell_val");
                const char_val = cg.castToType(cell_val, i32_type);
                var putchar_args: [1]c.LLVMValueRef = .{char_val};
                _ = c.LLVMBuildCall2(cg.builder, c.LLVMGlobalGetValueType(putchar_func), putchar_func, &putchar_args[0], 1, "");
            },
            .input => {
                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                var indices: [1]c.LLVMValueRef = .{ptr_val};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                const input_val = c.LLVMBuildCall2(cg.builder, c.LLVMGlobalGetValueType(getchar_func), getchar_func, null, 0, "input");
                const cell_val = cg.castToType(input_val, cell_type);
                _ = c.LLVMBuildStore(cg.builder, cell_val, cell_ptr);
            },
            .loop_start => {
                const loop_cond_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "bf_loop_cond");
                const loop_body_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "bf_loop_body");
                const loop_exit_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "bf_loop_exit");

                _ = c.LLVMBuildBr(cg.builder, loop_cond_bb);
                c.LLVMPositionBuilderAtEnd(cg.builder, loop_cond_bb);

                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                var indices: [1]c.LLVMValueRef = .{ptr_val};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                const cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "cell_val");
                const cond = c.LLVMBuildICmp(cg.builder, c.LLVMIntNE, cell_val, cell_zero, "loop_cond");
                _ = c.LLVMBuildCondBr(cg.builder, cond, loop_body_bb, loop_exit_bb);

                c.LLVMPositionBuilderAtEnd(cg.builder, loop_body_bb);
                try loop_stack.append(cg.allocator, .{ .cond = loop_cond_bb, .exit = loop_exit_bb });
            },
            .loop_end => {
                if (loop_stack.items.len == 0) {
                    return errors.CodegenError.UnsupportedOperation; // Unmatched ]
                }
                const loop_blocks = loop_stack.pop();
                _ = c.LLVMBuildBr(cg.builder, loop_blocks.?.cond);
                c.LLVMPositionBuilderAtEnd(cg.builder, loop_blocks.?.exit);
            },
            .set_zero => {
                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                var indices: [1]c.LLVMValueRef = .{ptr_val};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                _ = c.LLVMBuildStore(cg.builder, cell_zero, cell_ptr);
            },
            .linear_loop => |factors| {
                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                var indices: [1]c.LLVMValueRef = .{ptr_val};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                const count_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "count_val");
                for (factors.items) |factor| {
                    const offset_val = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(@abs(factor.offset))), 0);
                    const dest_ptr_val = if (factor.offset > 0)
                        c.LLVMBuildAdd(cg.builder, ptr_val, offset_val, "dest_ptr")
                    else
                        c.LLVMBuildSub(cg.builder, ptr_val, offset_val, "dest_ptr");
                    var dest_indices: [1]c.LLVMValueRef = .{dest_ptr_val};
                    const dest_cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &dest_indices[0], 1, "dest_cell_ptr");
                    const dest_cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, dest_cell_ptr, "dest_cell_val");
                    const factor_val = c.LLVMConstInt(cell_type, @as(c_ulonglong, @intCast(@abs(factor.factor))), 0);
                    const mul_val = c.LLVMBuildMul(cg.builder, count_val, factor_val, "mul_val");
                    const new_dest_val = if (factor.factor > 0)
                        c.LLVMBuildAdd(cg.builder, dest_cell_val, mul_val, "add_val")
                    else
                        c.LLVMBuildSub(cg.builder, dest_cell_val, mul_val, "sub_val");
                    _ = c.LLVMBuildStore(cg.builder, new_dest_val, dest_cell_ptr);
                }
                _ = c.LLVMBuildStore(cg.builder, cell_zero, cell_ptr);
            },
        }
    }

    // --- 4. Sync variables back from tape ---
    for (ctx.requests.items) |req| {
        // Handle array element access using array_index field for sync back
        const var_info = if (req.array_index) |index| blk: {
            const array_var_info = cg.getVariable(req.var_name) orelse continue;
            const array_type_kind = c.LLVMGetTypeKind(array_var_info.type_ref);
            if (array_type_kind != c.LLVMArrayTypeKind) continue;

            const element_type = c.LLVMGetElementType(array_var_info.type_ref);
            const array_length = c.LLVMGetArrayLength(array_var_info.type_ref);
            if (index >= array_length) continue;

            // Get pointer to the specific array element
            const index_val = c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), @as(c_ulonglong, @intCast(index)), 0);
            var gep_indices: [2]c.LLVMValueRef = .{
                c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0),
                index_val,
            };
            const element_ptr = c.LLVMBuildGEP2(cg.builder, array_var_info.type_ref, array_var_info.value, &gep_indices[0], 2, "array_element_ptr");

            break :blk structs.VariableInfo{
                .value = element_ptr,
                .type_ref = element_type,
                .type_name = array_var_info.type_name, // Use array's type name for now
            };
        } else cg.getVariable(req.var_name) orelse continue;

        const type_kind = c.LLVMGetTypeKind(var_info.type_ref);
        if (type_kind != c.LLVMIntegerTypeKind) {
            continue;
        }

        const var_size_bits = c.LLVMGetIntTypeWidth(var_info.type_ref);
        const cells_needed: usize = (var_size_bits + @as(c_uint, @intCast(ctx.cell_size)) - 1) / @as(c_uint, @intCast(ctx.cell_size));

        if (cells_needed == 1) {
            // Variable fits in one cell - load directly
            const cell_index = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(req.load_idx)), 0);
            var gep_indices: [1]c.LLVMValueRef = .{cell_index};
            const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &gep_indices[0], 1, "cell_ptr");
            const cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "cell_val");

            var reconstructed_value: c.LLVMValueRef = undefined;
            if (var_size_bits == ctx.cell_size) {
                reconstructed_value = cell_val;
            } else if (var_size_bits < ctx.cell_size) {
                // Truncate cell value to variable size
                reconstructed_value = c.LLVMBuildTrunc(cg.builder, cell_val, var_info.type_ref, "trunc_from_cell");
            } else {
                // Zero-extend cell value to variable size
                reconstructed_value = c.LLVMBuildZExt(cg.builder, cell_val, var_info.type_ref, "zext_from_cell");
            }

            _ = c.LLVMBuildStore(cg.builder, reconstructed_value, var_info.value);
        } else {
            // Variable needs multiple cells - reconstruct from chunks (big-endian)
            var reconstructed_value = c.LLVMConstInt(var_info.type_ref, 0, 0);

            var cell_idx: usize = 0;
            while (cell_idx < cells_needed) : (cell_idx += 1) {
                const cell_index = req.load_idx + @as(i32, @intCast(cell_idx));
                const byte_index = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(cell_index)), 0);
                var gep_indices: [1]c.LLVMValueRef = .{byte_index};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &gep_indices[0], 1, "cell_ptr");
                const cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "cell_val");

                // Zero-extend the cell value to the variable type
                const extended_cell_val = c.LLVMBuildZExt(cg.builder, cell_val, var_info.type_ref, "extended");

                // For big-endian: cell_idx corresponds to chunk at position (cells_needed - 1 - cell_idx)
                const chunk_pos = cells_needed - 1 - cell_idx;
                const shift_amount = c.LLVMConstInt(var_info.type_ref, @intCast(chunk_pos * @as(usize, @intCast(ctx.cell_size))), 0);

                if (@as(c_uint, @intCast(chunk_pos * @as(usize, @intCast(ctx.cell_size)))) < var_size_bits) {
                    if (shift_amount == c.LLVMConstInt(var_info.type_ref, 0, 0)) {
                        // Least significant chunk - just OR it
                        reconstructed_value = c.LLVMBuildOr(cg.builder, reconstructed_value, extended_cell_val, "combined");
                    } else {
                        // Shift to the correct position and combine
                        const shifted_cell_val = c.LLVMBuildShl(cg.builder, extended_cell_val, shift_amount, "shifted");
                        reconstructed_value = c.LLVMBuildOr(cg.builder, reconstructed_value, shifted_cell_val, "combined");
                    }
                }
            }

            // Store the reconstructed value back to the variable
            _ = c.LLVMBuildStore(cg.builder, reconstructed_value, var_info.value);
        }
    }

    return c.LLVMConstInt(i32_type, 0, 0);
}

// Helper function to free allocated variable names in requests
fn freeExpandedRequests(allocator: std.mem.Allocator, requests: []const BfLoadRequest) void {
    // No longer need to free synthetic names since we don't create them
    _ = allocator;
    _ = requests;
}
