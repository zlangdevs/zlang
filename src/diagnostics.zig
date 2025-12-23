const std = @import("std");
const utils = @import("codegen/utils.zig");

pub const Severity = enum {
    Error,
    Warning,
    Note,
};

pub const Diagnostic = struct {
    file_path: []const u8,
    line: usize,
    column: usize,
    message: []const u8,
    hint: ?[]const u8 = null,
    severity: Severity = .Error,
    token_text: ?[]const u8 = null,
};

fn getLineContent(allocator: std.mem.Allocator, file_path: []const u8, line_num: usize) !?[]const u8 {
    const file = std.fs.cwd().openFile(file_path, .{}) catch return null;
    defer file.close();

    const content_buffer = utils.alloc(u8, allocator, 1024 * 1024);
    const bytes_read = file.readAll(content_buffer) catch {
        allocator.free(content_buffer);
        return null;
    };
    const content = content_buffer[0..bytes_read];
    defer allocator.free(content_buffer);

    var it = std.mem.splitScalar(u8, content, '\n');
    var current_line: usize = 1;
    while (it.next()) |line| {
        if (current_line == line_num) {
            return utils.dupe(u8, allocator, line);
        }
        current_line += 1;
    }
    return null;
}

pub fn printDiagnostic(allocator: std.mem.Allocator, diag: Diagnostic) void {
    const red = "\x1b[31m";
    const blue = "\x1b[34m";
    const yellow = "\x1b[33m";
    const bold = "\x1b[1m";
    const reset = "\x1b[0m";

    const color = switch (diag.severity) {
        .Error => red,
        .Warning => yellow,
        .Note => blue,
    };

    const label = switch (diag.severity) {
        .Error => "error",
        .Warning => "warning",
        .Note => "note",
    };

    std.debug.print("{s}{s}{s}: {s}{s}{s}\n", .{ bold, color, label, bold, reset, diag.message });

    std.debug.print("{s}-->{s} {s}:{d}:{d}\n", .{ blue, reset, diag.file_path, diag.line, diag.column });

    if (getLineContent(allocator, diag.file_path, diag.line) catch null) |line_content| {
        defer allocator.free(line_content);
        const trimmed_line = std.mem.trimRight(u8, line_content, "\r");

        std.debug.print("{s}    |{s}\n", .{ blue, reset });

        std.debug.print("{s}{d:3} |{s} {s}\n", .{ blue, diag.line, reset, trimmed_line });

        var col = diag.column;
        var len: usize = 1;

        if (diag.token_text) |token| {
            if (std.mem.indexOf(u8, trimmed_line, token)) |idx| {
                col = idx + 1;
                len = token.len;
            }
        }

        std.debug.print("{s}    |{s} ", .{ blue, reset });

        var i: usize = 1;
        while (i < col) : (i += 1) {
            std.debug.print(" ", .{});
        }

        std.debug.print("{s}{s}", .{ bold, color });
        i = 0;
        while (i < len) : (i += 1) {
            std.debug.print("^", .{});
        }

        if (diag.hint) |hint| {
            std.debug.print(" {s}", .{hint});
        }
        std.debug.print("{s}\n", .{reset});
    }

    std.debug.print("\n", .{});
}