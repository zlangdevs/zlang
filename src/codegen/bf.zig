const std = @import("std");
const utils = @import("utils.zig");
const codegen = @import("llvm.zig").CodeGenerator;
const ast = @import("../parser/ast.zig");
const errors = @import("../errors.zig");

const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/IRReader.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/ExecutionEngine.h");
});

const BfLoadRequest = struct {
    var_name: []const u8,
    load_idx: i32,
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
        .requests = std.ArrayList(BfLoadRequest).init(allocator),
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
                    ctx.requests.append(.{
                        .var_name = var_name,
                        .load_idx = load_idx,
                    }) catch {};
                } else |_| {}
            }
            line_pos = end_pos + 1;
        }
    }
    return ctx;
}

pub fn generateBrainfuck(cg: *codegen, bf: ast.Brainfuck) !c.LLVMValueRef {
    var ctx = ParseBfContext(cg.allocator, bf.code);
    defer ctx.requests.deinit();

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
        const var_info = cg.getVariable(req.var_name) orelse {
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

    var loop_stack = std.ArrayList(struct { cond: c.LLVMBasicBlockRef, exit: c.LLVMBasicBlockRef }).init(cg.allocator);
    defer loop_stack.deinit();

    // Create constants for cell operations
    const cell_one = c.LLVMConstInt(cell_type, 1, 0);
    const cell_zero = c.LLVMConstInt(cell_type, 0, 0);

    // Create max value for unsigned overflow (2^cell_size - 1)
    // const max_cell_value = if (ctx.cell_size == 64)
    //     c.LLVMConstInt(cell_type, std.math.maxInt(u64), 0)
    // else
    //     c.LLVMConstInt(cell_type, (1 << @intCast(ctx.cell_size)) - 1, 0);

    for (ctx.code) |char| {
        switch (char) {
            '>' => {
                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                const new_ptr_val = c.LLVMBuildAdd(cg.builder, ptr_val, c.LLVMConstInt(i32_type, 1, 0), "inc_ptr");
                _ = c.LLVMBuildStore(cg.builder, new_ptr_val, ptr);
            },
            '<' => {
                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                const new_ptr_val = c.LLVMBuildSub(cg.builder, ptr_val, c.LLVMConstInt(i32_type, 1, 0), "dec_ptr");
                _ = c.LLVMBuildStore(cg.builder, new_ptr_val, ptr);
            },
            '+' => {
                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                var indices: [1]c.LLVMValueRef = .{ptr_val};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                const cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "cell_val");

                var new_cell_val: c.LLVMValueRef = undefined;
                if (ctx.cell_signed) {
                    // Signed arithmetic - let LLVM handle overflow
                    new_cell_val = c.LLVMBuildAdd(cg.builder, cell_val, cell_one, "inc_cell");
                } else {
                    // Unsigned arithmetic with proper wrapping
                    new_cell_val = c.LLVMBuildAdd(cg.builder, cell_val, cell_one, "inc_cell");
                    // LLVM handles unsigned overflow correctly by default
                }

                _ = c.LLVMBuildStore(cg.builder, new_cell_val, cell_ptr);
            },
            '-' => {
                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                var indices: [1]c.LLVMValueRef = .{ptr_val};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                const cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "cell_val");

                var new_cell_val: c.LLVMValueRef = undefined;
                if (ctx.cell_signed) {
                    // Signed arithmetic - let LLVM handle overflow
                    new_cell_val = c.LLVMBuildSub(cg.builder, cell_val, cell_one, "dec_cell");
                } else {
                    // Unsigned arithmetic with proper wrapping
                    new_cell_val = c.LLVMBuildSub(cg.builder, cell_val, cell_one, "dec_cell");
                    // LLVM handles unsigned underflow correctly by default (0-1 = MAX_VALUE)
                }

                _ = c.LLVMBuildStore(cg.builder, new_cell_val, cell_ptr);
            },
            '.' => {
                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                var indices: [1]c.LLVMValueRef = .{ptr_val};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                const cell_val = c.LLVMBuildLoad2(cg.builder, cell_type, cell_ptr, "cell_val");
                const char_val = cg.castToType(cell_val, i32_type);
                var putchar_args: [1]c.LLVMValueRef = .{char_val};
                _ = c.LLVMBuildCall2(cg.builder, c.LLVMGlobalGetValueType(putchar_func), putchar_func, &putchar_args[0], 1, "");
            },
            ',' => {
                const ptr_val = c.LLVMBuildLoad2(cg.builder, i32_type, ptr, "ptr_val");
                var indices: [1]c.LLVMValueRef = .{ptr_val};
                const cell_ptr = c.LLVMBuildGEP2(cg.builder, cell_type, tape, &indices[0], 1, "cell_ptr");
                const input_val = c.LLVMBuildCall2(cg.builder, c.LLVMGlobalGetValueType(getchar_func), getchar_func, null, 0, "input");
                const cell_val = cg.castToType(input_val, cell_type);
                _ = c.LLVMBuildStore(cg.builder, cell_val, cell_ptr);
            },
            '[' => {
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
                try loop_stack.append(.{ .cond = loop_cond_bb, .exit = loop_exit_bb });
            },
            ']' => {
                if (loop_stack.items.len == 0) {
                    return errors.CodegenError.UnsupportedOperation; // Unmatched ]
                }
                const loop_blocks = loop_stack.pop();
                _ = c.LLVMBuildBr(cg.builder, loop_blocks.?.cond);
                c.LLVMPositionBuilderAtEnd(cg.builder, loop_blocks.?.exit);
            },
            else => {}, // Ignore non-brainfuck characters
        }
    }

    // --- 4. Sync variables back from tape ---
    for (ctx.requests.items) |req| {
        const var_info = cg.getVariable(req.var_name) orelse continue;

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
