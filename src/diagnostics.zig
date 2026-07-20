const std = @import("std");
const utils = @import("codegen/utils.zig");

pub const Severity = enum {
    Error,
    Warning,
    Note,
};

pub const RelatedInfo = struct {
    file_path: []const u8,
    line: usize,
    column: usize,
    message: []const u8,
    severity: Severity = .Note,
};

pub const Diagnostic = struct {
    file_path: []const u8,
    line: usize,
    column: usize,
    message: []const u8,
    hint: ?[]const u8 = null,
    severity: Severity = .Error,
    token_text: ?[]const u8 = null,
    source_text: ?[]const u8 = null,
    related: []const RelatedInfo = &.{},
};

fn stdoutIsTty() bool {
    return std.c.isatty(std.posix.STDOUT_FILENO) != 0;
}

fn getLineFromSource(source: []const u8, line_num: usize) ?[]const u8 {
    var it = std.mem.splitScalar(u8, source, '\n');
    var current: usize = 1;
    while (it.next()) |line| {
        if (current == line_num) return line;
        current += 1;
    }
    return null;
}

fn getLineContent(allocator: std.mem.Allocator, file_path: []const u8, line_num: usize) !?[]const u8 {
    var threaded: std.Io.Threaded = .init(allocator, .{});
    defer threaded.deinit();
    const content = std.Io.Dir.cwd().readFileAlloc(threaded.io(), file_path, allocator, .limited(1024 * 1024)) catch return null;
    const content_buffer = content;
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
    const use_color = stdoutIsTty();
    const red = if (use_color) "\x1b[31m" else "";
    const blue = if (use_color) "\x1b[34m" else "";
    const yellow = if (use_color) "\x1b[33m" else "";
    const bold = if (use_color) "\x1b[1m" else "";
    const reset = if (use_color) "\x1b[0m" else "";

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

    const line_content_owned = blk: {
        if (diag.source_text) |src| {
            if (getLineFromSource(src, diag.line)) |line| {
                break :blk utils.dupe(u8, allocator, line);
            }
        }
        break :blk getLineContent(allocator, diag.file_path, diag.line) catch null;
    };
    if (line_content_owned) |line_content| {
        defer allocator.free(line_content);
        const trimmed_line = std.mem.trimEnd(u8, line_content, "\r");

        std.debug.print("{s}    |{s}\n", .{ blue, reset });

        std.debug.print("{s}{d:3} |{s} {s}\n", .{ blue, diag.line, reset, trimmed_line });

        const col = diag.column;
        var len: usize = 1;

        if (diag.token_text) |token| {
            if (col <= trimmed_line.len and std.mem.indexOfPos(u8, trimmed_line, col -| 1, token) != null) {
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

    for (diag.related) |note| {
        const note_color = switch (note.severity) {
            .Error => red,
            .Warning => yellow,
            .Note => blue,
        };
        const note_label = switch (note.severity) {
            .Error => "error",
            .Warning => "warning",
            .Note => "note",
        };
        std.debug.print("{s}{s}:{s} {s}\n", .{ bold, note_label, reset, note.message });
        std.debug.print("{s}-->{s} {s}:{d}:{d}\n", .{ blue, reset, note.file_path, note.line, note.column });
        if (getLineContent(allocator, note.file_path, note.line) catch null) |line_content| {
            defer allocator.free(line_content);
            const trimmed = std.mem.trimEnd(u8, line_content, "\r");
            std.debug.print("{s}    |{s}\n", .{ blue, reset });
            std.debug.print("{s}{d:3} |{s} {s}\n", .{ blue, note.line, reset, trimmed });
            std.debug.print("{s}    |{s} ", .{ blue, reset });
            var j: usize = 1;
            while (j < note.column) : (j += 1) std.debug.print(" ", .{});
            std.debug.print("{s}{s}^{s}\n", .{ bold, note_color, reset });
        }
    }

    std.debug.print("\n", .{});
}
