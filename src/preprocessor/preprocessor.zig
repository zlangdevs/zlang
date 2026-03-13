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

fn appendQuotedRawSegment(allocator: std.mem.Allocator, out: *std.ArrayList(u8), segment: []const u8) errors.PreprocessError!void {
    try out.append(allocator, '"');
    try out.appendSlice(allocator, segment);
    try out.append(allocator, '"');
}

fn findInterpolationExprEnd(s: []const u8, start: usize, limit: usize) ?usize {
    var i = start;
    var depth: usize = 1;

    while (i < limit) {
        const ch = s[i];

        if (ch == '"' or ch == '\'') {
            const quote = ch;
            i += 1;
            while (i < limit) {
                const c = s[i];
                if (c == '\\') {
                    i += 2;
                    continue;
                }
                if (c == quote) {
                    i += 1;
                    break;
                }
                i += 1;
            }
            continue;
        }

        if (ch == '/' and i + 1 < limit and s[i + 1] == '/') {
            i += 2;
            while (i < limit and s[i] != '\n') : (i += 1) {}
            continue;
        }

        if (ch == '/' and i + 1 < limit and s[i + 1] == '*') {
            i += 2;
            while (i + 1 < limit) : (i += 1) {
                if (s[i] == '*' and s[i + 1] == '/') {
                    i += 2;
                    break;
                }
            }
            continue;
        }

        if (ch == '{') {
            depth += 1;
            i += 1;
            continue;
        }

        if (ch == '}') {
            depth -= 1;
            if (depth == 0) return i;
            i += 1;
            continue;
        }

        i += 1;
    }

    return null;
}

fn appendStringWithInterpolation(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(u8),
    input: []const u8,
    start_idx: usize,
    end_idx: usize,
) errors.PreprocessError!void {
    var has_interpolation = false;
    var i = start_idx + 1;
    while (i < end_idx) {
        const ch = input[i];
        if (ch == '\\') {
            i += 2;
            continue;
        }
        if (ch == '$' and i + 1 < end_idx and input[i + 1] == '{') {
            has_interpolation = true;
            break;
        }
        i += 1;
    }

    if (!has_interpolation) {
        try out.appendSlice(allocator, input[start_idx .. end_idx + 1]);
        return;
    }

    var probe = start_idx + 1;
    var valid = true;
    while (probe < end_idx) {
        const ch = input[probe];
        if (ch == '\\') {
            probe += 2;
            continue;
        }
        if (ch == '$' and probe + 1 < end_idx and input[probe + 1] == '{') {
            const expr_end = findInterpolationExprEnd(input, probe + 2, end_idx) orelse {
                valid = false;
                break;
            };
            probe = expr_end + 1;
            continue;
        }
        probe += 1;
    }

    if (!valid) {
        try out.appendSlice(allocator, input[start_idx .. end_idx + 1]);
        return;
    }

    try out.appendSlice(allocator, "@__zinterp(");

    var first = true;
    var seg_start = start_idx + 1;
    var cur = start_idx + 1;

    while (cur < end_idx) {
        const ch = input[cur];
        if (ch == '\\') {
            cur += 2;
            continue;
        }

        if (ch == '$' and cur + 1 < end_idx and input[cur + 1] == '{') {
            if (!first) try out.appendSlice(allocator, ", ");
            try appendQuotedRawSegment(allocator, out, input[seg_start..cur]);
            first = false;

            const expr_start = cur + 2;
            const expr_end = findInterpolationExprEnd(input, expr_start, end_idx).?;
            try out.appendSlice(allocator, ", (");
            try out.appendSlice(allocator, std.mem.trim(u8, input[expr_start..expr_end], " \t\r\n"));
            try out.appendSlice(allocator, ")");

            cur = expr_end + 1;
            seg_start = cur;
            continue;
        }

        cur += 1;
    }

    if (!first) {
        try out.appendSlice(allocator, ", ");
        try appendQuotedRawSegment(allocator, out, input[seg_start..end_idx]);
    } else {
        try appendQuotedRawSegment(allocator, out, input[start_idx + 1 .. end_idx]);
    }

    try out.append(allocator, ')');
}

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
            const string_start = i;
            i += 1;
            while (i < text.len) {
                const c = text[i];
                i += 1;
                if (c == '\\') {
                    if (i < text.len) i += 1;
                    continue;
                }
                if (c == quote) break;
            }

            const string_end = i - 1;
            if (quote == '"') {
                try appendStringWithInterpolation(allocator, out, text, string_start, string_end);
            } else {
                try out.appendSlice(allocator, text[string_start..i]);
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
            const string_start = i;
            i += 1;
            while (i < input.len) {
                const c = input[i];
                i += 1;
                if (c == '\\') {
                    if (i < input.len) i += 1;
                    continue;
                }
                if (c == quote) break;
            }

            const string_end = i - 1;
            if (quote == '"') {
                try appendStringWithInterpolation(allocator, &out, input, string_start, string_end);
            } else {
                try out.appendSlice(allocator, input[string_start..i]);
            }
            at_line_start = false;
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
