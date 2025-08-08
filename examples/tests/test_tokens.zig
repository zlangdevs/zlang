const std = @import("std");
const tokenizer = @import("src/tokenizer.zig");

const allocator = std.heap.page_allocator;

pub fn main() !void {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: test_tokens <input_text>\n", .{});
        return;
    }

    const input = args[1];
    std.debug.print("Input: {s}\n", .{input});
    std.debug.print("==================\n", .{});

    const tokens = tokenizer.tokenize(allocator, input) catch |err| {
        std.debug.print("Error tokenizing: {}\n", .{err});
        return;
    };
    defer tokenizer.freeTokens(allocator, tokens);

    tokenizer.printTokens(tokens);
}
