const std = @import("std");

pub fn main() !void {
    std.debug.print("Hello, World!\n", .{});
    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        std.debug.print("Usage: zlang <path to file>");
    }
    const cwd = std.fs.cwd();
    cwd.access(args[1], .{ .mode = .read }) catch |err| {
        if (err == error.FileNotFound) {
            std.debug.print("File or directory does not exist: {s}\n", .{args[1]});
            return;
        }
        std.debug.print("Error accessing file: {}\n", .{err});
        return;
    };
    const input_path = args[1];
    const file_stat = try cwd.statFile(input_path);
    if (file_stat.kind == .File) {
        const file = cwd.openFile(input_path, .{ .mode = .read_only }) catch |err| {
            std.debug.print("Error accessing file {}: {}\n", .{input_path, err});
            return;
        };
        defer file.close();
        
        const max_buffer_size = 1024 * 1024 * 1024;

        const buffer = try file.readToEndAlloc(allocator, max_buffer_size);
        defer allocator.free(buffer);
        std.debug.print("Reading file:\n{s}\n", .{buffer});
        
    } else if (file_stat.kind == .Directory) {
        std.debug.print("We don't yet support directories\n", .{input_path});
    }
    for (args, 0..) |arg, i| {
        std.debug.print("Arg {}: {s}\n", .{ i, arg });
    }
    
}
