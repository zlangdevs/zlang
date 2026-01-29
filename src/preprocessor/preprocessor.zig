const std = @import("std");
const errors = @import("../errors.zig");

fn isIdentStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isIdentChar(c: u8) bool {
    return isIdentStart(c) or (c >= '0' and c <= '9');
}

fn isSpace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\r';
}

fn skipSpaces(s: []const u8, i: *usize) void {
    while (i.* < s.len and isSpace(s[i.*])) : (i.* += 1) {}
}

pub const PreprocessOutput = struct {
    text: []u8,
    flags: std.ArrayList([]const u8),

    pub fn deinitFlags(self: *PreprocessOutput, allocator: std.mem.Allocator) void {
        for (self.flags.items) |flag| {
            allocator.free(flag);
        }
        self.flags.deinit(allocator);
    }
};

fn appendExpandedText(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(u8),
    defs: *const std.StringHashMap([]const u8),
    text: []const u8,
    depth: usize,
) errors.PreprocessError!void {
    if (depth > 32) return errors.PreprocessError.ExpansionLimit;

    var i: usize = 0;
    while (i < text.len) {
        const ch = text[i];

        if (ch == '"' or ch == '\'') {
            const quote = ch;
            try out.append(allocator, ch);
            i += 1;
            while (i < text.len) {
                const c = text[i];
                try out.append(allocator, c);
                i += 1;
                if (c == '\\') {
                    if (i < text.len) {
                        try out.append(allocator, text[i]);
                        i += 1;
                    }
                    continue;
                }
                if (c == quote) break;
            }
            continue;
        }

        if (ch == '/' and i + 1 < text.len and text[i + 1] == '/') {
            try out.append(allocator, '/');
            try out.append(allocator, '/');
            i += 2;
            while (i < text.len) {
                const c = text[i];
                try out.append(allocator, c);
                i += 1;
                if (c == '\n') break;
            }
            continue;
        }

        if (ch == '/' and i + 1 < text.len and text[i + 1] == '*') {
            try out.append(allocator, '/');
            try out.append(allocator, '*');
            i += 2;
            while (i < text.len) {
                const c = text[i];
                try out.append(allocator, c);
                if (c == '*' and i + 1 < text.len and text[i + 1] == '/') {
                    i += 1;
                    try out.append(allocator, '/');
                    i += 1;
                    break;
                }
                i += 1;
            }
            continue;
        }

        if (isIdentStart(ch)) {
            const start = i;
            i += 1;
            while (i < text.len and isIdentChar(text[i])) : (i += 1) {}
            const ident = text[start..i];
            if (defs.get(ident)) |rep| {
                try appendExpandedText(allocator, out, defs, rep, depth + 1);
            } else {
                try out.appendSlice(allocator, ident);
            }
            continue;
        }

        try out.append(allocator, ch);
        i += 1;
    }
}

fn parseDefineLine(allocator: std.mem.Allocator, defs: *std.StringHashMap([]const u8), line: []const u8) errors.PreprocessError!void {
    var i: usize = 0;
    skipSpaces(line, &i);
    if (i >= line.len or line[i] != '#') return errors.PreprocessError.InvalidDirective;
    i += 1;
    skipSpaces(line, &i);

    const kw_start = i;
    while (i < line.len and isIdentChar(line[i])) : (i += 1) {}
    const kw = line[kw_start..i];
    if (!std.mem.eql(u8, kw, "define")) return errors.PreprocessError.InvalidDirective;

    skipSpaces(line, &i);
    if (i >= line.len or !isIdentStart(line[i])) return errors.PreprocessError.InvalidDefine;

    const name_start = i;
    i += 1;
    while (i < line.len and isIdentChar(line[i])) : (i += 1) {}
    const name = line[name_start..i];

    skipSpaces(line, &i);
    const value = std.mem.trimRight(u8, line[i..], " \t\r");

    const name_copy = try allocator.dupe(u8, name);
    const value_copy = try allocator.dupe(u8, value);

    const old_opt = defs.fetchPut(name_copy, value_copy) catch {
        allocator.free(name_copy);
        allocator.free(value_copy);
        return errors.PreprocessError.OutOfMemory;
    };
    if (old_opt) |old| {
        allocator.free(old.key);
        allocator.free(old.value);
    }
}

fn parseFlagLine(allocator: std.mem.Allocator, flags: *std.ArrayList([]const u8), line: []const u8) errors.PreprocessError!void {
    var i: usize = 0;
    skipSpaces(line, &i);
    if (i >= line.len or line[i] != '#') return errors.PreprocessError.InvalidDirective;
    i += 1;
    skipSpaces(line, &i);

    const kw_start = i;
    while (i < line.len and isIdentChar(line[i])) : (i += 1) {}
    const kw = line[kw_start..i];
    if (!std.mem.eql(u8, kw, "flag")) return errors.PreprocessError.InvalidDirective;

    skipSpaces(line, &i);
    const rest = std.mem.trimRight(u8, line[i..], " \t\r");
    if (rest.len == 0) return errors.PreprocessError.InvalidDirective;

    var it = std.mem.tokenizeAny(u8, rest, " \t");
    while (it.next()) |token| {
        const token_copy = allocator.dupe(u8, token) catch return errors.PreprocessError.OutOfMemory;
        flags.append(allocator, token_copy) catch {
            allocator.free(token_copy);
            return errors.PreprocessError.OutOfMemory;
        };
    }
}

fn parseDirectiveLine(
    allocator: std.mem.Allocator,
    defs: *std.StringHashMap([]const u8),
    flags: *std.ArrayList([]const u8),
    line: []const u8,
) errors.PreprocessError!void {
    var i: usize = 0;
    skipSpaces(line, &i);
    if (i >= line.len or line[i] != '#') return errors.PreprocessError.InvalidDirective;
    i += 1;
    skipSpaces(line, &i);

    const kw_start = i;
    while (i < line.len and isIdentChar(line[i])) : (i += 1) {}
    const kw = line[kw_start..i];

    if (std.mem.eql(u8, kw, "define")) {
        try parseDefineLine(allocator, defs, line);
        return;
    }

    if (std.mem.eql(u8, kw, "flag")) {
        try parseFlagLine(allocator, flags, line);
        return;
    }

    return errors.PreprocessError.InvalidDirective;
}

pub fn preprocessWithFlags(allocator: std.mem.Allocator, input: []const u8) errors.PreprocessError!PreprocessOutput {
    var defs = std.StringHashMap([]const u8).init(allocator);
    defer {
        var it = defs.iterator();
        while (it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.*);
        }
        defs.deinit();
    }

    var out = std.ArrayList(u8){};
    errdefer out.deinit(allocator);

    var flags = std.ArrayList([]const u8){};
    errdefer {
        for (flags.items) |flag| {
            allocator.free(flag);
        }
        flags.deinit(allocator);
    }

    var i: usize = 0;
    var at_line_start = true;

    while (i < input.len) {
        if (at_line_start) {
            var j = i;
            while (j < input.len and (input[j] == ' ' or input[j] == '\t')) : (j += 1) {}
            if (j < input.len and input[j] == '#') {
                var line_end = j;
                while (line_end < input.len and input[line_end] != '\n') : (line_end += 1) {}
                const line = input[i..line_end];
                try parseDirectiveLine(allocator, &defs, &flags, line);
                if (line_end < input.len and input[line_end] == '\n') {
                    try out.append(allocator, '\n');
                    i = line_end + 1;
                    at_line_start = true;
                    continue;
                }
                i = line_end;
                at_line_start = true;
                continue;
            }
        }

        const ch = input[i];

        if (ch == '"' or ch == '\'') {
            const quote = ch;
            try out.append(allocator, ch);
            i += 1;
            while (i < input.len) {
                const c = input[i];
                try out.append(allocator, c);
                i += 1;
                if (c == '\\') {
                    if (i < input.len) {
                        try out.append(allocator, input[i]);
                        i += 1;
                    }
                    continue;
                }
                if (c == quote) break;
                if (c == '\n') at_line_start = true else at_line_start = false;
            }
            continue;
        }

        if (ch == '/' and i + 1 < input.len and input[i + 1] == '/') {
            try out.append(allocator, '/');
            try out.append(allocator, '/');
            i += 2;
            while (i < input.len) {
                const c = input[i];
                try out.append(allocator, c);
                i += 1;
                if (c == '\n') {
                    at_line_start = true;
                    break;
                }
            }
            continue;
        }

        if (ch == '/' and i + 1 < input.len and input[i + 1] == '*') {
            try out.append(allocator, '/');
            try out.append(allocator, '*');
            i += 2;
            while (i < input.len) {
                const c = input[i];
                try out.append(allocator, c);
                if (c == '*' and i + 1 < input.len and input[i + 1] == '/') {
                    i += 1;
                    try out.append(allocator, '/');
                    i += 1;
                    at_line_start = false;
                    break;
                }
                if (c == '\n') at_line_start = true else at_line_start = false;
                i += 1;
            }
            continue;
        }

        if (isIdentStart(ch)) {
            const start = i;
            i += 1;
            while (i < input.len and isIdentChar(input[i])) : (i += 1) {}
            const ident = input[start..i];
            if (defs.get(ident)) |rep| {
                try appendExpandedText(allocator, &out, &defs, rep, 0);
            } else {
                try out.appendSlice(allocator, ident);
            }
            at_line_start = false;
            continue;
        }

        try out.append(allocator, ch);
        i += 1;
        at_line_start = ch == '\n';
    }

    return PreprocessOutput{
        .text = try out.toOwnedSlice(allocator),
        .flags = flags,
    };
}

pub fn preprocess(allocator: std.mem.Allocator, input: []const u8) errors.PreprocessError![]u8 {
    var result = try preprocessWithFlags(allocator, input);
    result.deinitFlags(allocator);
    return result.text;
}
