const std = @import("std");
const consts = @import("consts.zig");
const errors = @import("errors.zig");
const tokenizer = @import("tokenizer.zig");

const allocator = std.heap.page_allocator;

pub fn read_file(file_name: []const u8) anyerror![]const u8 {
    const cwd = std.fs.cwd();
    cwd.access(file_name, .{}) catch |err| {
        if (err == error.FileNotFound) {
            return errors.ReadFileError.FileNotFound;
        }
        return errors.ReadFileError.AccessDenied;
    };

    const file_stat = try cwd.statFile(file_name);
    if (file_stat.kind == .file) {
        const file = cwd.openFile(file_name, .{ .mode = .read_only }) catch {
            return errors.ReadFileError.AccessDenied;
        };
        defer file.close();
        const buffer = try file.readToEndAlloc(allocator, consts.MAX_BUFF_SIZE);
        return buffer;
    } else if (file_stat.kind == .directory) {
        return errors.ReadFileError.InvalidPath;
    }
    return "";
}

pub fn main() !void {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        std.debug.print("Usage: zlang <path to file>", .{});
        return;
    }
    const input_file = args[1];
    const input = read_file(input_file) catch |err| {
        const error_msg = switch (err) {
            error.FileNotFound => "Specified file does not exist or path is invalid.",
            error.AccessDenied => "Error accessing file.",
            error.InvalidPath => "We don't support directories yet.",
            error.OutOfMemory => "",
            error.IOError => "", // TODO: implement these errors
            else => "",
        };
        std.debug.print("Error: {s}\n", .{error_msg});
        return;
    };
    std.debug.print("Content of input file: {s}", .{input});
    // for (args, 0..) |arg, i| {
    //     std.debug.print("Arg {}: {s}\n", .{ i, arg });
    // }

    const tokens = tokenizer.tokenize(allocator, input) catch |err| {
        const error_msg = switch (err) {
            error.LexerInitFailed => "Failed to initialize the lexer.",
            error.FileOpenFailed => "Failed to open file.",
            error.OutOfMemory => "Out of memory.",
        };
        std.debug.print("Error tokenizing: .{s}\n", .{error_msg});
        return;
    };
    defer tokenizer.freeTokens(allocator, tokens);
    tokenizer.printTokens(tokens);
}
