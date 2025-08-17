const std = @import("std");

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

pub fn ParseBfContext(allocator: std.mem.Allocator, input: []const u8) BrainfuckContext {
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
        if (trimmed.len >= 2 and trimmed[0] == '?' and trimmed[trimmed.len - 1] == '?') {
            const content = trimmed[1 .. trimmed.len - 1];
            const content_trimmed = std.mem.trim(u8, content, " \t");
            var parts = std.mem.splitSequence(u8, content_trimmed, " ");
            const arg_name = parts.next() orelse continue;
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
                const var_name = load_parts.next() orelse continue;
                const load_idx_str = load_parts.next() orelse continue;
                if (std.fmt.parseInt(i32, std.mem.trim(u8, load_idx_str, " \t"), 10)) |load_idx| {
                    ctx.requests.append(.{
                        .var_name = var_name,
                        .load_idx = load_idx,
                    }) catch continue;
                } else |_| {}
            }
        }
    }
    return ctx;
}
