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
    scan_left,
    scan_right,
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

fn checkScanLoop(body: []const BfOp) ?i32 {
    if (body.len != 1) return null;
    if (body[0] == .inc_ptr) {
        const offset = body[0].inc_ptr;
        if (offset == 1 or offset == -1) return offset;
    }
    return null;
}

const ComptimeInterpreter = struct {
    tape: []i64,
    ptr: usize,
    allocator: std.mem.Allocator,
    max_iterations: usize,
    iterations: usize,
    output: std.ArrayList(u8),
    jump_table: []usize,

    fn init(allocator: std.mem.Allocator, tape_size: usize) !ComptimeInterpreter {
        const tape = try allocator.alloc(i64, tape_size);
        @memset(tape, 0);
        return ComptimeInterpreter{
            .tape = tape,
            .ptr = 0,
            .allocator = allocator,
            .max_iterations = 0,
            .iterations = 0,
            .output = std.ArrayList(u8){},
            .jump_table = &[_]usize{},
        };
    }

    fn deinit(self: *ComptimeInterpreter) void {
        self.allocator.free(self.tape);
        self.output.deinit(self.allocator);
        if (self.jump_table.len > 0) {
            self.allocator.free(self.jump_table);
        }
    }

    fn buildJumpTable(self: *ComptimeInterpreter, ops: []const BfOp) !void {
        self.jump_table = try self.allocator.alloc(usize, ops.len);
        @memset(self.jump_table, 0);

        var stack = std.ArrayList(usize){};
        defer stack.deinit(self.allocator);

        for (ops, 0..) |op, i| {
            if (op == .loop_start) {
                try stack.append(self.allocator, i);
            } else if (op == .loop_end) {
                if (stack.items.len > 0) {
                    const start = stack.pop().?;
                    self.jump_table[start] = i;
                    self.jump_table[i] = start;
                }
            }
        }
    }

    fn execute(self: *ComptimeInterpreter, ops: []const BfOp, capture_output: bool) !bool {
        try self.buildJumpTable(ops);

        var pc: usize = 0;

        while (pc < ops.len) {
            self.iterations += 1;

            const op = ops[pc];
            switch (op) {
                .inc_ptr => |val| {
                    const new_ptr = @as(i64, @intCast(self.ptr)) + val;
                    if (new_ptr < 0 or new_ptr >= self.tape.len) return false;
                    self.ptr = @intCast(new_ptr);
                    pc += 1;
                },
                .add_val => |val| {
                    self.tape[self.ptr] +%= val;
                    pc += 1;
                },
                .output => {
                    if (capture_output) {
                        const byte: u8 = @truncate(@as(u64, @bitCast(self.tape[self.ptr])));
                        try self.output.append(self.allocator, byte);
                        pc += 1;
                    } else {
                        return false;
                    }
                },
                .input => return false,
                .set_zero => {
                    self.tape[self.ptr] = 0;
                    pc += 1;
                },
                .loop_start => {
                    if (self.tape[self.ptr] == 0) {
                        pc = self.jump_table[pc] + 1;
                    } else {
                        if (pc + 2 < ops.len) {
                            if (ops[pc + 2] == .loop_end) {
                                if (ops[pc + 1] == .add_val) {
                                    const val = ops[pc + 1].add_val;
                                    if (val == 1 or val == -1) {
                                        self.tape[self.ptr] = 0;
                                        pc += 3;
                                        continue;
                                    }
                                } else if (ops[pc + 1] == .inc_ptr) {
                                    const dir = ops[pc + 1].inc_ptr;
                                    if (dir == 1) {
                                        while (self.ptr < self.tape.len - 1 and self.tape[self.ptr] != 0) {
                                            self.ptr += 1;
                                            self.iterations += 1;
                                        }
                                        pc += 3;
                                        continue;
                                    } else if (dir == -1) {
                                        while (self.ptr > 0 and self.tape[self.ptr] != 0) {
                                            self.ptr -= 1;
                                            self.iterations += 1;
                                        }
                                        pc += 3;
                                        continue;
                                    }
                                }
                            } else if (pc + 3 < ops.len and ops[pc + 3] == .loop_end) {
                                if (ops[pc + 1] == .inc_ptr and ops[pc + 2] == .add_val) {
                                    const offset = ops[pc + 1].inc_ptr;
                                    const factor = ops[pc + 2].add_val;
                                    const new_ptr = @as(i64, @intCast(self.ptr)) + offset;
                                    if (new_ptr >= 0 and new_ptr < self.tape.len) {
                                        const target_ptr: usize = @intCast(new_ptr);
                                        const count = self.tape[self.ptr];
                                        self.tape[target_ptr] +%= count *% factor;
                                        self.tape[self.ptr] = 0;
                                        pc += 4;
                                        continue;
                                    }
                                }
                            }
                        }
                        pc += 1;
                    }
                },
                .loop_end => {
                    if (self.tape[self.ptr] != 0) {
                        pc = self.jump_table[pc] + 1;
                    } else {
                        pc += 1;
                    }
                },
                .linear_loop => return false,
                .scan_left, .scan_right => return false,
            }
        }
        return true;
    }

    fn getNonZeroRegion(self: *const ComptimeInterpreter) ?struct { start: usize, end: usize } {
        var start: ?usize = null;
        var end: usize = 0;

        for (self.tape, 0..) |val, i| {
            if (val != 0) {
                if (start == null) start = i;
                end = i;
            }
        }

        if (start) |s| {
            return .{ .start = s, .end = end };
        }
        return null;
    }
};

fn generateDiffOps(allocator: std.mem.Allocator, initial_state: []const i64, final_state: []const i64, initial_ptr: usize, final_ptr: usize) !std.ArrayList(BfOp) {
    var result = std.ArrayList(BfOp){};
    errdefer result.deinit(allocator);

    var current_ptr: i32 = @intCast(initial_ptr);

    for (initial_state, 0..) |initial_val, i| {
        const final_val = final_state[i];
        const diff = final_val - initial_val;
        if (diff == 0) continue;

        const target_ptr: i32 = @intCast(i);
        const move = target_ptr - current_ptr;
        if (move != 0) {
            try result.append(allocator, .{ .inc_ptr = move });
            current_ptr = target_ptr;
        }

        try result.append(allocator, .{ .add_val = @intCast(diff) });
    }

    const target_final_ptr: i32 = @intCast(final_ptr);
    if (target_final_ptr != current_ptr) {
        try result.append(allocator, .{ .inc_ptr = target_final_ptr - current_ptr });
    }

    return result;
}

fn isInputUsed(ops: []const BfOp, input_index: usize) bool {
    if (input_index >= ops.len) return false;

    var i = input_index + 1;
    while (i < ops.len) : (i += 1) {
        const op = ops[i];
        switch (op) {
            .add_val => |val| {
                if (val != 0) return true;
            },
            .output => return true,
            .inc_ptr => {},
            .set_zero => return false,
            .loop_start => {
                if (i + 2 < ops.len and ops[i + 2] == .loop_end) {
                    if (ops[i + 1] == .add_val) {
                        const val = ops[i + 1].add_val;
                        if (val == 1 or val == -1) {
                            i += 2;
                            continue;
                        }
                    }
                }
                return true;
            },
            .input => return false,
            else => return true,
        }
    }
    return false;
}

fn findFirstRealInput(ops: []const BfOp) ?usize {
    for (ops, 0..) |op, i| {
        if (op == .input) {
            if (isInputUsed(ops, i)) {
                return i;
            }
        }
    }
    return null;
}

fn tryFullProgramPrecompute(allocator: std.mem.Allocator, ops: []const BfOp) !?[]const u8 {
    const first_real_input = findFirstRealInput(ops);

    if (first_real_input) |input_pos| {
        if (input_pos == 0) {
            if (ops.len > 1) {
                const after_first_input = ops[1..];
                const next_real_input = findFirstRealInput(after_first_input);

                if (next_real_input == null) {
                    return null;
                }
            }
            return null;
        }

        if (input_pos < ops.len) {
            const after_input = ops[input_pos + 1 ..];
            const next_real_input = findFirstRealInput(after_input);

            if (next_real_input == null) {
                return null;
            }
        }

        return null;
    }

    var has_input_op = false;
    for (ops) |op| {
        if (op == .input) {
            has_input_op = true;
            break;
        }
    }
    if (has_input_op) return null;

    std.debug.print("Applying compile-time optimization... (Ctrl+C to cancel)\n", .{});

    var interpreter = try ComptimeInterpreter.init(allocator, 30000);
    defer interpreter.deinit();

    const success = interpreter.execute(ops, true) catch return null;
    if (!success) return null;

    if (interpreter.output.items.len == 0) return null;

    std.debug.print("Compile-time optimization complete! Pre-computed {} bytes of output.\n", .{interpreter.output.items.len});

    const output_copy = try allocator.dupe(u8, interpreter.output.items);
    return output_copy;
}

fn tryComptimeOptimizeSingleSegment(allocator: std.mem.Allocator, ops: []const BfOp) !?std.ArrayList(BfOp) {
    if (ops.len == 0) return null;

    var interpreter = try ComptimeInterpreter.init(allocator, 30000);
    defer interpreter.deinit();

    const initial_tape = try allocator.alloc(i64, interpreter.tape.len);
    defer allocator.free(initial_tape);
    @memcpy(initial_tape, interpreter.tape);
    const initial_ptr = interpreter.ptr;

    const success = interpreter.execute(ops, false) catch return null;
    if (!success) return null;

    const min_iterations_needed = 15;
    if (interpreter.iterations < min_iterations_needed) return null;

    var optimized = try generateDiffOps(allocator, initial_tape, interpreter.tape, initial_ptr, interpreter.ptr);

    if (optimized.items.len == 0) {
        optimized.deinit(allocator);
        return null;
    }

    const estimated_optimized_cost = optimized.items.len * 2;
    if (interpreter.iterations < estimated_optimized_cost + 10) {
        optimized.deinit(allocator);
        return null;
    }

    return optimized;
}

const PrecomputedProgram = struct {
    output: []const u8,
};

fn trySplitAndPrecompute(allocator: std.mem.Allocator, ops: []const BfOp) !?std.ArrayList(BfOp) {
    const first_real_input = findFirstRealInput(ops) orelse return null;

    if (first_real_input == 0) return null;

    const prefix = ops[0..first_real_input];
    const suffix = ops[first_real_input..];

    if (prefix.len < 10) return null;

    var suffix_has_more_inputs = false;
    for (suffix[1..]) |op| {
        if (op == .input) {
            suffix_has_more_inputs = true;
            break;
        }
    }

    if (!suffix_has_more_inputs and suffix.len > 1000) {
        return null;
    }

    std.debug.print("Found unused inputs before position {}. Pre-computing prefix... (Ctrl+C to cancel)\n", .{first_real_input});

    var interpreter = try ComptimeInterpreter.init(allocator, 30000);
    defer interpreter.deinit();

    const success = interpreter.execute(prefix, true) catch return null;
    if (!success) return null;

    std.debug.print("Prefix pre-computed! {} bytes of output, continuing with remaining code...\n", .{interpreter.output.items.len});

    var result = std.ArrayList(BfOp){};
    errdefer result.deinit(allocator);

    for (interpreter.output.items) |byte| {
        try result.append(allocator, .{ .add_val = @intCast(byte) });
        try result.append(allocator, .output);
        if (byte != 0) {
            try result.append(allocator, .{ .add_val = -@as(i32, @intCast(byte)) });
        }
    }

    const initial_tape = try allocator.alloc(i64, interpreter.tape.len);
    defer allocator.free(initial_tape);
    @memcpy(initial_tape, interpreter.tape);
    const initial_ptr = interpreter.ptr;

    if (initial_ptr != 0) {
        try result.append(allocator, .{ .inc_ptr = @intCast(initial_ptr) });
    }

    for (initial_tape, 0..) |val, i| {
        if (val == 0) continue;
        const offset: i32 = @intCast(i);
        const current_offset = initial_ptr;
        const move = offset - @as(i32, @intCast(current_offset));
        if (move != 0) {
            try result.append(allocator, .{ .inc_ptr = move });
        }
        try result.append(allocator, .{ .add_val = @intCast(val) });
        if (move != 0) {
            try result.append(allocator, .{ .inc_ptr = -move });
        }
    }

    for (suffix) |op| {
        try result.append(allocator, op);
    }

    return result;
}

fn trySkipUnusedInputAndOptimize(allocator: std.mem.Allocator, ops: []const BfOp) !?std.ArrayList(BfOp) {
    const first_real_input = findFirstRealInput(ops);
    if (first_real_input == null) return null;

    const input_pos = first_real_input.?;
    if (input_pos >= ops.len) return null;

    const before_input = ops[0..input_pos];
    const after_input = ops[input_pos + 1 ..];

    const next_real_input = findFirstRealInput(after_input);
    if (next_real_input != null) return null;

    if (after_input.len == 0) return null;

    std.debug.print("Skipping unused input at position {} and pre-computing rest... (Ctrl+C to cancel)\n", .{input_pos});

    var interpreter = try ComptimeInterpreter.init(allocator, 30000);
    defer interpreter.deinit();

    const success_before = interpreter.execute(before_input, false) catch return null;
    if (!success_before) return null;

    const state_after_before = try allocator.alloc(i64, interpreter.tape.len);
    defer allocator.free(state_after_before);
    @memcpy(state_after_before, interpreter.tape);
    const ptr_after_before = interpreter.ptr;

    const success_after = interpreter.execute(after_input, true) catch return null;
    if (!success_after) return null;

    std.debug.print("Pre-computed {} bytes of output, recreating memory state...\n", .{interpreter.output.items.len});

    var result = std.ArrayList(BfOp){};
    errdefer result.deinit(allocator);

    for (interpreter.output.items) |byte| {
        try result.append(allocator, .{ .add_val = @intCast(byte) });
        try result.append(allocator, .output);
        if (byte != 0) {
            try result.append(allocator, .{ .add_val = -@as(i32, @intCast(byte)) });
        }
    }

    if (ptr_after_before != 0) {
        try result.append(allocator, .{ .inc_ptr = @intCast(ptr_after_before) });
    }

    var current_ptr: i32 = @intCast(ptr_after_before);
    for (interpreter.tape, 0..) |val, i| {
        const diff = val - state_after_before[i];
        if (diff == 0) continue;

        const target_ptr: i32 = @intCast(i);
        const move = target_ptr - current_ptr;
        if (move != 0) {
            try result.append(allocator, .{ .inc_ptr = move });
            current_ptr = target_ptr;
        }

        try result.append(allocator, .{ .add_val = @intCast(diff) });
    }

    const final_ptr: i32 = @intCast(interpreter.ptr);
    if (final_ptr != current_ptr) {
        try result.append(allocator, .{ .inc_ptr = final_ptr - current_ptr });
    }

    return result;
}

fn tryComptimeOptimization(allocator: std.mem.Allocator, ops: []const BfOp, optimize_enabled: bool) !?std.ArrayList(BfOp) {
    if (!optimize_enabled) return null;
    if (ops.len == 0) return null;

    if (try tryFullProgramPrecompute(allocator, ops)) |precomputed_output| {
        defer allocator.free(precomputed_output);
        var result = std.ArrayList(BfOp){};
        for (precomputed_output) |byte| {
            try result.append(allocator, .{ .add_val = @intCast(byte) });
            try result.append(allocator, .output);
            if (byte != 0) {
                try result.append(allocator, .{ .add_val = -@as(i32, @intCast(byte)) });
            }
        }
        return result;
    }

    if (try trySkipUnusedInputAndOptimize(allocator, ops)) |optimized| {
        return optimized;
    }

    if (try trySplitAndPrecompute(allocator, ops)) |split_result| {
        return split_result;
    }

    if (ops.len > 100) {
        std.debug.print("Applying segmented compile-time optimization... (Ctrl+C to cancel)\n", .{});
    }

    var segments = std.ArrayList(std.ArrayList(BfOp)){};
    defer {
        for (segments.items) |*seg| seg.deinit(allocator);
        segments.deinit(allocator);
    }

    var current_segment = std.ArrayList(BfOp){};
    errdefer current_segment.deinit(allocator);
    var has_io = false;

    for (ops) |op| {
        if (op == .input or op == .output) {
            has_io = true;
            if (current_segment.items.len > 0) {
                try segments.append(allocator, current_segment);
                current_segment = std.ArrayList(BfOp){};
            }
            var io_segment = std.ArrayList(BfOp){};
            try io_segment.append(allocator, op);
            try segments.append(allocator, io_segment);
        } else {
            try current_segment.append(allocator, op);
        }
    }

    if (current_segment.items.len > 0) {
        try segments.append(allocator, current_segment);
    }

    if (!has_io) {
        if (ops.len > 10000) return null;
        return try tryComptimeOptimizeSingleSegment(allocator, ops);
    }

    var optimized_total = std.ArrayList(BfOp){};
    errdefer optimized_total.deinit(allocator);

    var optimized_count: usize = 0;

    for (segments.items) |segment| {
        if (segment.items.len > 0 and (segment.items[0] == .input or segment.items[0] == .output)) {
            try optimized_total.append(allocator, segment.items[0]);
            continue;
        }

        if (try tryComptimeOptimizeSingleSegment(allocator, segment.items)) |optimized_seg| {
            optimized_count += 1;
            for (optimized_seg.items) |op| {
                try optimized_total.append(allocator, op);
            }
            var mut_seg = optimized_seg;
            mut_seg.deinit(allocator);
        } else {
            for (segment.items) |op| {
                try optimized_total.append(allocator, op);
            }
        }
    }

    if (optimized_count == 0) {
        optimized_total.deinit(allocator);
        return null;
    }

    return optimized_total;
}

fn optimizeOps(allocator: std.mem.Allocator, initial_ops: []const BfOp, enable_comptime: bool) !std.ArrayList(BfOp) {
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

    if (enable_comptime) {
        if (try tryComptimeOptimization(allocator, contracted.items, enable_comptime)) |comptime_ops| {
            return comptime_ops;
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

            if (depth == 0) {
                const body = ops[i + 1 .. j];

                if (body.len == 1 and body[0] == .add_val) {
                    const val = body[0].add_val;
                    if (val == 1 or val == -1) {
                        try optimized.append(allocator, .set_zero);
                        i = j + 1;
                        continue;
                    }
                }

                if (checkScanLoop(body)) |direction| {
                    if (direction == 1) {
                        try optimized.append(allocator, .scan_right);
                    } else {
                        try optimized.append(allocator, .scan_left);
                    }
                    i = j + 1;
                    continue;
                }

                if (possible_linear) {
                    if (try checkLinearLoop(allocator, body)) |factors| {
                        try optimized.append(allocator, .{ .linear_loop = factors });
                        i = j + 1;
                        continue;
                    }
                }
            }
        }
        try optimized.append(allocator, op);
        i += 1;
    }

    return optimized;
}

pub fn generateBrainfuck(cg: *codegen, bf: ast.Brainfuck, enable_comptime_opt: bool) !c.LLVMValueRef {
    var ctx = ParseBfContext(cg.allocator, bf.code);
    defer {
        freeExpandedRequests(cg.allocator, ctx.requests.items);
        ctx.requests.deinit(cg.allocator);
    }

    try expandArrayLoads(cg, &ctx);
    var ops = try parseBrainfuckOps(cg.allocator, ctx.code);
    defer ops.deinit(cg.allocator);
    var optimized_ops = try optimizeOps(cg.allocator, ops.items, enable_comptime_opt);
    defer {
        freeBfOps(cg.allocator, optimized_ops.items);
        optimized_ops.deinit(cg.allocator);
    }

    const current_function = cg.current_function orelse return errors.CodegenError.TypeMismatch;

    const cell_type = c.LLVMIntTypeInContext(cg.context, @intCast(ctx.cell_size));
    const i32_type = c.LLVMInt32TypeInContext(cg.context);
    const i8_type = c.LLVMInt8TypeInContext(cg.context);
    const i8_ptr_type = c.LLVMPointerType(i8_type, 0);
    const size_t_type = utils.libcTypeToLLVM(cg, .size_t_type);

    const tape_len_val = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(ctx.len)), 0);
    const tape = c.LLVMBuildArrayAlloca(cg.builder, cell_type, tape_len_val, "bf_tape");

    const ptr = c.LLVMBuildAlloca(cg.builder, i32_type, "bf_ptr");
    _ = c.LLVMBuildStore(cg.builder, c.LLVMConstInt(i32_type, 0, 0), ptr);

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

    for (ctx.requests.items) |req| {
        const var_info = if (req.array_index) |index| blk: {
            const array_var_info = cg.getVariable(req.var_name) orelse return errors.CodegenError.UndefinedVariable;
            const array_type_kind = c.LLVMGetTypeKind(array_var_info.type_ref);
            if (array_type_kind != c.LLVMArrayTypeKind) return errors.CodegenError.TypeMismatch;

            const element_type = c.LLVMGetElementType(array_var_info.type_ref);
            const array_length = c.LLVMGetArrayLength(array_var_info.type_ref);
            if (index >= array_length) return errors.CodegenError.UndefinedVariable;

            const index_val = c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), @as(c_ulonglong, @intCast(index)), 0);
            var gep_indices: [2]c.LLVMValueRef = .{
                c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0),
                index_val,
            };
            const element_ptr = c.LLVMBuildGEP2(cg.builder, array_var_info.type_ref, array_var_info.value, &gep_indices[0], 2, "array_element_ptr");

            break :blk structs.VariableInfo{
                .value = element_ptr,
                .type_ref = element_type,
                .type_name = array_var_info.type_name,
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

        const cells_needed: usize = (var_size_bits + @as(c_uint, @intCast(ctx.cell_size)) - 1) / @as(c_uint, @intCast(ctx.cell_size));

        if (cells_needed == 1) {
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
            var cell_idx: usize = 0;
            while (cell_idx < cells_needed) : (cell_idx += 1) {
                const cell_index = req.load_idx + @as(i32, @intCast(cell_idx));
                const byte_index = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(cell_index)), 0);
                var gep_indices: [1]c.LLVMValueRef = .{byte_index};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &gep_indices[0], 1, "cell_ptr");

                const chunk_pos = cells_needed - 1 - cell_idx;
                const shift_amount = c.LLVMConstInt(var_info.type_ref, @intCast(chunk_pos * @as(usize, @intCast(ctx.cell_size))), 0);
                var cell_value: c.LLVMValueRef = undefined;
                if (@as(c_uint, @intCast(chunk_pos * @as(usize, @intCast(ctx.cell_size)))) >= var_size_bits) {
                    cell_value = c.LLVMConstInt(cell_type, 0, 0);
                } else {
                    const shifted_value = c.LLVMBuildLShr(cg.builder, var_value, shift_amount, "shifted");
                    cell_value = c.LLVMBuildTrunc(cg.builder, shifted_value, cell_type, "chunk");
                }

                _ = c.LLVMBuildStore(cg.builder, cell_value, cell_ptr);
            }
        }
    }

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
                    return errors.CodegenError.UnsupportedOperation;
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
            .scan_right => {
                const scan_cond_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "scan_right_cond");
                const scan_body_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "scan_right_body");
                const scan_exit_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "scan_right_exit");

                _ = c.LLVMBuildBr(cg.builder, scan_cond_bb);
                c.LLVMPositionBuilderAtEnd(cg.builder, scan_cond_bb);

                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                var indices: [1]c.LLVMValueRef = .{ptr_val};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                const cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "cell_val");
                const cond = c.LLVMBuildICmp(cg.builder, c.LLVMIntNE, cell_val, cell_zero, "scan_cond");
                _ = c.LLVMBuildCondBr(cg.builder, cond, scan_body_bb, scan_exit_bb);

                c.LLVMPositionBuilderAtEnd(cg.builder, scan_body_bb);
                const new_ptr_val = c.LLVMBuildAdd(cg.builder, ptr_val, c.LLVMConstInt(i32_type, 1, 0), "inc_ptr");
                _ = c.LLVMBuildStore(cg.builder, new_ptr_val, ptr);
                _ = c.LLVMBuildBr(cg.builder, scan_cond_bb);

                c.LLVMPositionBuilderAtEnd(cg.builder, scan_exit_bb);
            },
            .scan_left => {
                const scan_cond_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "scan_left_cond");
                const scan_body_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "scan_left_body");
                const scan_exit_bb = c.LLVMAppendBasicBlockInContext(cg.context, current_function, "scan_left_exit");

                _ = c.LLVMBuildBr(cg.builder, scan_cond_bb);
                c.LLVMPositionBuilderAtEnd(cg.builder, scan_cond_bb);

                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                var indices: [1]c.LLVMValueRef = .{ptr_val};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                const cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "cell_val");
                const cond = c.LLVMBuildICmp(cg.builder, c.LLVMIntNE, cell_val, cell_zero, "scan_cond");
                _ = c.LLVMBuildCondBr(cg.builder, cond, scan_body_bb, scan_exit_bb);

                c.LLVMPositionBuilderAtEnd(cg.builder, scan_body_bb);
                const new_ptr_val = c.LLVMBuildSub(cg.builder, ptr_val, c.LLVMConstInt(i32_type, 1, 0), "dec_ptr");
                _ = c.LLVMBuildStore(cg.builder, new_ptr_val, ptr);
                _ = c.LLVMBuildBr(cg.builder, scan_cond_bb);

                c.LLVMPositionBuilderAtEnd(cg.builder, scan_exit_bb);
            },
        }
    }

    for (ctx.requests.items) |req| {
        const var_info = if (req.array_index) |index| blk: {
            const array_var_info = cg.getVariable(req.var_name) orelse continue;
            const array_type_kind = c.LLVMGetTypeKind(array_var_info.type_ref);
            if (array_type_kind != c.LLVMArrayTypeKind) continue;

            const element_type = c.LLVMGetElementType(array_var_info.type_ref);
            const array_length = c.LLVMGetArrayLength(array_var_info.type_ref);
            if (index >= array_length) continue;

            const index_val = c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), @as(c_ulonglong, @intCast(index)), 0);
            var gep_indices: [2]c.LLVMValueRef = .{
                c.LLVMConstInt(c.LLVMInt32TypeInContext(cg.context), 0, 0),
                index_val,
            };
            const element_ptr = c.LLVMBuildGEP2(cg.builder, array_var_info.type_ref, array_var_info.value, &gep_indices[0], 2, "array_element_ptr");

            break :blk structs.VariableInfo{
                .value = element_ptr,
                .type_ref = element_type,
                .type_name = array_var_info.type_name,
            };
        } else cg.getVariable(req.var_name) orelse continue;

        const type_kind = c.LLVMGetTypeKind(var_info.type_ref);
        if (type_kind != c.LLVMIntegerTypeKind) {
            continue;
        }

        const var_size_bits = c.LLVMGetIntTypeWidth(var_info.type_ref);
        const cells_needed: usize = (var_size_bits + @as(c_uint, @intCast(ctx.cell_size)) - 1) / @as(c_uint, @intCast(ctx.cell_size));

        if (cells_needed == 1) {
            const cell_index = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(req.load_idx)), 0);
            var gep_indices: [1]c.LLVMValueRef = .{cell_index};
            const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &gep_indices[0], 1, "cell_ptr");
            const cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "cell_val");

            var reconstructed_value: c.LLVMValueRef = undefined;
            if (var_size_bits == ctx.cell_size) {
                reconstructed_value = cell_val;
            } else if (var_size_bits < ctx.cell_size) {
                reconstructed_value = c.LLVMBuildTrunc(cg.builder, cell_val, var_info.type_ref, "trunc_from_cell");
            } else {
                reconstructed_value = c.LLVMBuildZExt(cg.builder, cell_val, var_info.type_ref, "zext_from_cell");
            }

            _ = c.LLVMBuildStore(cg.builder, reconstructed_value, var_info.value);
        } else {
            var reconstructed_value = c.LLVMConstInt(var_info.type_ref, 0, 0);

            var cell_idx: usize = 0;
            while (cell_idx < cells_needed) : (cell_idx += 1) {
                const cell_index = req.load_idx + @as(i32, @intCast(cell_idx));
                const byte_index = c.LLVMConstInt(i32_type, @as(c_ulonglong, @intCast(cell_index)), 0);
                var gep_indices: [1]c.LLVMValueRef = .{byte_index};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &gep_indices[0], 1, "cell_ptr");
                const cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "cell_val");

                const extended_cell_val = c.LLVMBuildZExt(cg.builder, cell_val, var_info.type_ref, "extended");

                const chunk_pos = cells_needed - 1 - cell_idx;
                const shift_amount = c.LLVMConstInt(var_info.type_ref, @intCast(chunk_pos * @as(usize, @intCast(ctx.cell_size))), 0);

                if (@as(c_uint, @intCast(chunk_pos * @as(usize, @intCast(ctx.cell_size)))) < var_size_bits) {
                    if (shift_amount == c.LLVMConstInt(var_info.type_ref, 0, 0)) {
                        reconstructed_value = c.LLVMBuildOr(cg.builder, reconstructed_value, extended_cell_val, "combined");
                    } else {
                        const shifted_cell_val = c.LLVMBuildShl(cg.builder, extended_cell_val, shift_amount, "shifted");
                        reconstructed_value = c.LLVMBuildOr(cg.builder, reconstructed_value, shifted_cell_val, "combined");
                    }
                }
            }

            _ = c.LLVMBuildStore(cg.builder, reconstructed_value, var_info.value);
        }
    }

    return c.LLVMConstInt(i32_type, 0, 0);
}

fn freeExpandedRequests(allocator: std.mem.Allocator, requests: []const BfLoadRequest) void {
    _ = allocator;
    _ = requests;
}
