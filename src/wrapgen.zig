const std = @import("std");
const utils = @import("codegen/utils.zig");
const c_abi = @import("c_abi.zig");
const llvm_tools = @import("llvm_tools.zig");

const AliasMap = std.StringHashMap([]const u8);

const StructFieldInfo = struct {
    name: []const u8,
    z_type: []const u8,
};

const StructInfo = struct {
    name: []const u8,
    is_union: bool,
    fields: std.ArrayList(StructFieldInfo),
    abi_mapping: c_abi.StructAbiMapping,
};

const StructMap = std.StringHashMap(*StructInfo);

const ParamInfo = struct {
    name: []const u8,
    c_type: []const u8,
    z_type: []const u8,
    abi_type: []const u8,
    conversion: c_abi.ConversionKind,
    struct_name: ?[]const u8,
};

const FunctionInfo = struct {
    name: []const u8,
    c_ret_type: []const u8,
    ret_z_type: []const u8,
    ret_abi_type: []const u8,
    ret_conversion: c_abi.ConversionKind,
    ret_struct_name: ?[]const u8,
    params: std.ArrayList(ParamInfo),
    has_varargs: bool,
};

const EnumValueInfo = struct {
    name: []const u8,
    expr: ?[]const u8,
};

const EnumInfo = struct {
    name: []const u8,
    values: std.ArrayList(EnumValueInfo),
};

const ConstType = enum {
    i32,
    f32,
    bool,
};

const ConstInfo = struct {
    name: []const u8,
    expr: []const u8,
    ty: ConstType,
};

const DeclNameInfo = struct {
    name: []const u8,
    array_len: ?usize,
    had_array_suffix: bool,
};

fn isSpace(ch: u8) bool {
    return ch == ' ' or ch == '\t' or ch == '\n' or ch == '\r';
}

fn isIdentChar(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or
        (ch >= 'A' and ch <= 'Z') or
        (ch >= '0' and ch <= '9') or
        ch == '_';
}

fn trimSpaces(s: []const u8) []const u8 {
    return std.mem.trim(u8, s, " \t\r\n");
}

fn asciiLower(alloc: std.mem.Allocator, input: []const u8) ![]u8 {
    var out = utils.alloc(u8, alloc, input.len);
    for (input, 0..) |ch, i| {
        out[i] = std.ascii.toLower(ch);
    }
    return out;
}

fn squeezeSpaces(alloc: std.mem.Allocator, input: []const u8) ![]u8 {
    var out = std.ArrayList(u8){};
    defer out.deinit(alloc);

    var prev_space = false;
    for (input) |ch| {
        if (isSpace(ch)) {
            if (!prev_space) {
                try out.append(alloc, ' ');
            }
            prev_space = true;
        } else {
            prev_space = false;
            try out.append(alloc, ch);
        }
    }
    return utils.dupe(u8, alloc, trimSpaces(out.items));
}

fn stripCommentsAndPreproc(alloc: std.mem.Allocator, input: []const u8) ![]u8 {
    var out = std.ArrayList(u8){};
    defer out.deinit(alloc);

    var i: usize = 0;
    while (i < input.len) : (i += 1) {
        if (i + 1 < input.len and input[i] == '/' and input[i + 1] == '*') {
            i += 2;
            while (i + 1 < input.len and !(input[i] == '*' and input[i + 1] == '/')) : (i += 1) {}
            if (i + 1 < input.len) i += 1;
            continue;
        }

        if (i + 1 < input.len and input[i] == '/' and input[i + 1] == '/') {
            while (i < input.len and input[i] != '\n') : (i += 1) {}
            continue;
        }

        if (input[i] == '#') {
            var line_start: usize = i;
            while (line_start > 0 and input[line_start - 1] != '\n') : (line_start -= 1) {}

            var cursor = line_start;
            while (cursor < i and (input[cursor] == ' ' or input[cursor] == '\t')) : (cursor += 1) {}

            if (cursor == i) {
                while (i < input.len and input[i] != '\n') : (i += 1) {}
                continue;
            }
        }

        if (input[i] != '\r') {
            try out.append(alloc, input[i]);
        }
    }

    return out.toOwnedSlice(alloc);
}

fn removeExternCGuards(alloc: std.mem.Allocator, input: []const u8) ![]u8 {
    var out = std.ArrayList(u8){};
    defer out.deinit(alloc);

    var it = std.mem.splitScalar(u8, input, '\n');
    while (it.next()) |line_raw| {
        const line = trimSpaces(line_raw);
        if (std.mem.eql(u8, line, "extern \"C\" {") or std.mem.eql(u8, line, "extern \"C\"{")) {
            continue;
        }
        if (std.mem.eql(u8, line, "}")) {
            continue;
        }

        try out.appendSlice(alloc, line_raw);
        try out.append(alloc, '\n');
    }

    return out.toOwnedSlice(alloc);
}

fn stripCommentsOnly(alloc: std.mem.Allocator, input: []const u8) ![]u8 {
    var out = std.ArrayList(u8){};
    defer out.deinit(alloc);

    var i: usize = 0;
    while (i < input.len) : (i += 1) {
        if (i + 1 < input.len and input[i] == '/' and input[i + 1] == '*') {
            i += 2;
            while (i + 1 < input.len and !(input[i] == '*' and input[i + 1] == '/')) : (i += 1) {}
            if (i + 1 < input.len) i += 1;
            continue;
        }

        if (i + 1 < input.len and input[i] == '/' and input[i + 1] == '/') {
            while (i < input.len and input[i] != '\n') : (i += 1) {}
            continue;
        }

        if (input[i] != '\r') {
            try out.append(alloc, input[i]);
        }
    }

    return out.toOwnedSlice(alloc);
}

fn isNumericToken(token: []const u8) bool {
    if (token.len == 0) return false;
    if (std.ascii.isDigit(token[0])) return true;
    return token.len >= 2 and token[0] == '.' and std.ascii.isDigit(token[1]);
}

fn sanitizeNumericToken(alloc: std.mem.Allocator, token: []const u8) ![]const u8 {
    var tmp = std.ArrayList(u8){};
    defer tmp.deinit(alloc);

    for (token) |ch| {
        if (ch == '\'') continue;
        try tmp.append(alloc, ch);
    }

    while (tmp.items.len > 0) {
        const last = tmp.items[tmp.items.len - 1];
        if (last == 'u' or last == 'U' or last == 'l' or last == 'L' or last == 'f' or last == 'F') {
            _ = tmp.pop();
            continue;
        }
        break;
    }

    if (tmp.items.len == 0) return error.InvalidCharacter;
    return utils.dupe(u8, alloc, tmp.items);
}

fn isWrappedByParens(input: []const u8) bool {
    if (input.len < 2) return false;
    if (input[0] != '(' or input[input.len - 1] != ')') return false;

    var depth: usize = 0;
    for (input, 0..) |ch, i| {
        if (ch == '(') depth += 1;
        if (ch == ')') {
            if (depth == 0) return false;
            depth -= 1;
            if (depth == 0 and i < input.len - 1) return false;
        }
    }

    return depth == 0;
}

fn sanitizeConstantExpr(alloc: std.mem.Allocator, raw_expr: []const u8) !?[]const u8 {
    var expr = trimSpaces(raw_expr);
    if (expr.len == 0) return null;

    if (std.mem.indexOfScalar(u8, expr, '"') != null or std.mem.indexOfScalar(u8, expr, '\'') != null) {
        return null;
    }

    while (isWrappedByParens(expr)) {
        expr = trimSpaces(expr[1 .. expr.len - 1]);
    }

    var out = std.ArrayList(u8){};
    defer out.deinit(alloc);

    var i: usize = 0;
    while (i < expr.len) {
        const ch = expr[i];
        const starts_token = std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '.';

        if (!starts_token) {
            try out.append(alloc, ch);
            i += 1;
            continue;
        }

        const start = i;
        while (i < expr.len) : (i += 1) {
            const c = expr[i];
            if (!(std.ascii.isAlphanumeric(c) or c == '_' or c == '.' or c == '\'')) {
                break;
            }
        }

        const token = expr[start..i];
        if (isNumericToken(token)) {
            const cleaned = try sanitizeNumericToken(alloc, token);
            try out.appendSlice(alloc, cleaned);
        } else {
            try out.appendSlice(alloc, token);
        }
    }

    const normalized = trimSpaces(out.items);
    if (normalized.len == 0) return null;
    return utils.dupe(u8, alloc, normalized);
}

fn inferConstType(expr: []const u8) ConstType {
    if (std.mem.eql(u8, expr, "true") or std.mem.eql(u8, expr, "false")) return .bool;

    var i: usize = 0;
    while (i < expr.len) {
        const ch = expr[i];
        if (!(std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '.' or ch == '\'')) {
            i += 1;
            continue;
        }

        const start = i;
        while (i < expr.len and (std.ascii.isAlphanumeric(expr[i]) or expr[i] == '_' or expr[i] == '.' or expr[i] == '\'')) : (i += 1) {}
        const token = expr[start..i];
        if (!isNumericToken(token)) continue;

        if (std.mem.indexOfScalar(u8, token, '.')) |_| {
            return .f32;
        }

        const is_hex = std.mem.startsWith(u8, token, "0x") or std.mem.startsWith(u8, token, "0X");
        if (!is_hex and (std.mem.indexOfScalar(u8, token, 'e') != null or std.mem.indexOfScalar(u8, token, 'E') != null)) {
            return .f32;
        }
    }

    return .i32;
}

fn parseTypedefEnum(
    alloc: std.mem.Allocator,
    stmt: []const u8,
    enums: *std.ArrayList(EnumInfo),
    enum_value_names: *std.StringHashMap(void),
) !void {
    const trimmed = trimSpaces(stmt);
    if (trimmed.len == 0) return;

    var lowered = try asciiLower(alloc, trimmed);
    lowered = try squeezeSpaces(alloc, lowered);
    if (!std.mem.startsWith(u8, lowered, "typedef enum")) return;

    const open_brace = std.mem.indexOfScalar(u8, trimmed, '{') orelse return;
    const close_brace = std.mem.lastIndexOfScalar(u8, trimmed, '}') orelse return;
    if (close_brace <= open_brace) return;

    const enum_prefix = trimSpaces(trimmed[12..open_brace]);
    const prefix_name = findIdentifierBeforeIndex(enum_prefix, enum_prefix.len);

    const alias_part = trimSpaces(trimmed[close_brace + 1 ..]);
    const alias_name = findIdentifierBeforeIndex(alias_part, alias_part.len);

    const enum_name = alias_name orelse prefix_name orelse return;

    var enum_info = EnumInfo{
        .name = utils.dupe(u8, alloc, enum_name),
        .values = std.ArrayList(EnumValueInfo){},
    };

    const body = trimmed[open_brace + 1 .. close_brace];
    var entries = try splitTopLevel(alloc, body, ',');
    defer entries.deinit(alloc);

    for (entries.items) |entry_raw| {
        const entry = trimSpaces(entry_raw);
        if (entry.len == 0) continue;

        if (std.mem.indexOfScalar(u8, entry, '=')) |eq_pos| {
            const value_name = trimSpaces(entry[0..eq_pos]);
            if (value_name.len == 0) continue;

            var maybe_expr = try sanitizeConstantExpr(alloc, entry[eq_pos + 1 ..]);
            if (maybe_expr) |expr| {
                if (!isSimpleEnumExprForCodegen(expr)) {
                    std.debug.print("warning: skipped enum initializer {s}.{s}: complex expression '{s}'\n", .{ enum_name, value_name, expr });
                    maybe_expr = null;
                }
            }
            try enum_info.values.append(alloc, .{
                .name = utils.dupe(u8, alloc, value_name),
                .expr = maybe_expr,
            });
            try enum_value_names.put(utils.dupe(u8, alloc, value_name), {});
        } else {
            const value_name = trimSpaces(entry);
            if (value_name.len == 0) continue;

            try enum_info.values.append(alloc, .{
                .name = utils.dupe(u8, alloc, value_name),
                .expr = null,
            });
            try enum_value_names.put(utils.dupe(u8, alloc, value_name), {});
        }
    }

    if (enum_info.values.items.len == 0) {
        enum_info.values.deinit(alloc);
        return;
    }

    try enums.append(alloc, enum_info);
}

fn parseDefineConstants(alloc: std.mem.Allocator, source: []const u8, constants: *std.ArrayList(ConstInfo)) !void {
    var current = std.ArrayList(u8){};
    defer current.deinit(alloc);

    var collecting = false;
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line_raw| {
        const line = trimSpaces(line_raw);
        if (!collecting) {
            if (!std.mem.startsWith(u8, line, "#define ")) continue;
            current.clearRetainingCapacity();
            try current.appendSlice(alloc, line);
            collecting = true;
        } else {
            if (line.len > 0) {
                try current.append(alloc, ' ');
                try current.appendSlice(alloc, line);
            }
        }

        while (current.items.len > 0 and isSpace(current.items[current.items.len - 1])) {
            _ = current.pop();
        }

        if (current.items.len > 0 and current.items[current.items.len - 1] == '\\') {
            _ = current.pop();
            continue;
        }

        collecting = false;

        const define_line = trimSpaces(current.items);
        if (!std.mem.startsWith(u8, define_line, "#define ")) continue;

        const rest = trimSpaces(define_line[8..]);
        if (rest.len == 0) continue;

        var pos: usize = 0;
        while (pos < rest.len and isIdentChar(rest[pos])) : (pos += 1) {}
        if (pos == 0) continue;

        const name = rest[0..pos];
        if (std.mem.startsWith(u8, name, "__")) continue;
        if (std.mem.eql(u8, name, "_LP64")) continue;
        if (std.mem.eql(u8, name, "true")) continue;
        if (std.mem.eql(u8, name, "false")) continue;
        if (pos < rest.len and rest[pos] == '(') continue;

        const value_raw = trimSpaces(rest[pos..]);
        if (value_raw.len == 0) continue;

        const sanitized = try sanitizeConstantExpr(alloc, value_raw) orelse continue;
        if (std.mem.indexOf(u8, sanitized, "sizeof") != null) continue;
        if (std.mem.indexOfScalar(u8, sanitized, ':') != null) continue;

        try constants.append(alloc, .{
            .name = utils.dupe(u8, alloc, name),
            .expr = sanitized,
            .ty = inferConstType(sanitized),
        });
    }
}

fn emitEnumDeclarations(writer: anytype, enums: []const EnumInfo) !void {
    for (enums) |enum_info| {
        try writer.print("enum {s} {{\n", .{enum_info.name});
        for (enum_info.values.items) |value| {
            if (value.expr) |expr| {
                try writer.print("    {s} = {s},\n", .{ value.name, expr });
            } else {
                try writer.print("    {s},\n", .{value.name});
            }
        }
        try writer.writeAll("}\n\n");
    }
}

fn isIdentifierStart(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or ch == '_';
}

fn isConstExprResolvable(expr: []const u8, known_symbols: *const std.StringHashMap(void)) bool {
    var i: usize = 0;
    while (i < expr.len) {
        const ch = expr[i];
        if (!(std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '.' or ch == '\'')) {
            i += 1;
            continue;
        }

        const start = i;
        while (i < expr.len and (std.ascii.isAlphanumeric(expr[i]) or expr[i] == '_' or expr[i] == '.' or expr[i] == '\'')) : (i += 1) {}
        const token = expr[start..i];

        if (isNumericToken(token)) continue;
        if (!isIdentifierStart(token[0])) continue;

        const ident = token;

        if (std.mem.eql(u8, ident, "true") or std.mem.eql(u8, ident, "false")) {
            continue;
        }
        if (!known_symbols.contains(ident)) {
            return false;
        }
    }

    return true;
}

fn constTypeName(ty: ConstType) []const u8 {
    return switch (ty) {
        .i32 => "i32",
        .f32 => "f32",
        .bool => "bool",
    };
}

fn isSimpleConstExprForCodegen(expr: []const u8) bool {
    const trimmed = trimSpaces(expr);
    if (trimmed.len == 0) return false;

    var saw_token = false;

    for (trimmed, 0..) |ch, i| {
        switch (ch) {
            '+', '*', '/', '%', '<', '>', '&', '|', '^', '!', '~', '(', ')' => return false,
            '-' => {
                if (i != 0) return false;
            },
            else => {},
        }
    }

    var i: usize = 0;
    while (i < trimmed.len) {
        const ch = trimmed[i];
        const starts_token = std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '.' or ch == '\'';
        if (!starts_token) {
            i += 1;
            continue;
        }

        const start = i;
        while (i < trimmed.len and (std.ascii.isAlphanumeric(trimmed[i]) or trimmed[i] == '_' or trimmed[i] == '.' or trimmed[i] == '\'')) : (i += 1) {}
        const token = trimmed[start..i];
        saw_token = true;

        if (isNumericToken(token)) continue;
        if (std.mem.eql(u8, token, "true") or std.mem.eql(u8, token, "false")) continue;
        return false;
    }

    return saw_token;
}

fn isSimpleEnumExprForCodegen(expr: []const u8) bool {
    const trimmed = trimSpaces(expr);
    if (trimmed.len == 0) return false;

    var saw_token = false;

    for (trimmed, 0..) |ch, i| {
        switch (ch) {
            '+', '*', '/', '%', '<', '>', '&', '|', '^', '!', '~', '(', ')' => return false,
            '-' => {
                if (i != 0) return false;
            },
            else => {},
        }
    }

    var i: usize = 0;
    while (i < trimmed.len) {
        const ch = trimmed[i];
        const starts_token = std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '.' or ch == '\'';
        if (!starts_token) {
            i += 1;
            continue;
        }

        const start = i;
        while (i < trimmed.len and (std.ascii.isAlphanumeric(trimmed[i]) or trimmed[i] == '_' or trimmed[i] == '.' or trimmed[i] == '\'')) : (i += 1) {}
        const token = trimmed[start..i];
        saw_token = true;

        if (isNumericToken(token)) continue;
        if (std.mem.eql(u8, token, "true") or std.mem.eql(u8, token, "false")) continue;
        if (isIdentifierStart(token[0])) continue;
        return false;
    }

    return saw_token;
}

fn emitConstants(
    writer: anytype,
    constants: []const ConstInfo,
    known_symbols: *std.StringHashMap(void),
    emitted_names: *std.StringHashMap(void),
    skipped_names: *std.StringHashMap(void),
) !void {
    var progress = true;
    while (progress) {
        progress = false;
        for (constants) |constant| {
            if (emitted_names.contains(constant.name)) continue;
            if (known_symbols.contains(constant.name)) continue;
            if (!isConstExprResolvable(constant.expr, known_symbols)) continue;

            if (!isSimpleConstExprForCodegen(constant.expr)) {
                if (!skipped_names.contains(constant.name)) {
                    std.debug.print("warning: skipped const {s}: non-literal expression '{s}'\n", .{ constant.name, constant.expr });
                    try skipped_names.put(constant.name, {});
                }
                continue;
            }

            try writer.print("const {s} {s} = {s};\n", .{ constTypeName(constant.ty), constant.name, constant.expr });
            try emitted_names.put(constant.name, {});
            try known_symbols.put(constant.name, {});
            progress = true;
        }
    }
}

fn emitKnownHeaderFlags(writer: anytype, alloc: std.mem.Allocator, header_path: []const u8) !void {
    const base = std.fs.path.basename(header_path);
    const is_raylib_header = std.mem.eql(u8, base, "raylib.h") or std.mem.eql(u8, base, "raylib_wrap_subset.h");
    if (!is_raylib_header) return;

    const header_dir = std.fs.path.dirname(header_path) orelse ".";
    const lib_dir = try std.fs.path.join(alloc, &[_][]const u8{ header_dir, "bin" });
    defer alloc.free(lib_dir);

    try writer.print("#flag -L{s}\n", .{lib_dir});
    try writer.writeAll("#flag -lraylib\n");
    try writer.writeAll("#flag -lGL\n");
    try writer.writeAll("#flag -lm\n");
    try writer.writeAll("#flag -lpthread\n");
    try writer.writeAll("#flag -ldl\n");
    try writer.writeAll("#flag -lrt\n");
    try writer.writeAll("#flag -lX11\n");
    try writer.writeAll("module raylib;\n\n");
}

fn splitTopLevel(alloc: std.mem.Allocator, input: []const u8, delimiter: u8) !std.ArrayList([]const u8) {
    var parts = std.ArrayList([]const u8){};
    var current = std.ArrayList(u8){};

    var paren_depth: usize = 0;
    var brace_depth: usize = 0;
    var bracket_depth: usize = 0;

    for (input) |ch| {
        switch (ch) {
            '(' => paren_depth += 1,
            ')' => {
                if (paren_depth > 0) paren_depth -= 1;
            },
            '{' => brace_depth += 1,
            '}' => {
                if (brace_depth > 0) brace_depth -= 1;
            },
            '[' => bracket_depth += 1,
            ']' => {
                if (bracket_depth > 0) bracket_depth -= 1;
            },
            else => {},
        }

        if (ch == delimiter and paren_depth == 0 and brace_depth == 0 and bracket_depth == 0) {
            const part = utils.dupe(u8, alloc, trimSpaces(current.items));
            try parts.append(alloc, part);
            current.clearRetainingCapacity();
            continue;
        }

        try current.append(alloc, ch);
    }

    const tail = trimSpaces(current.items);
    if (tail.len > 0) {
        try parts.append(alloc, utils.dupe(u8, alloc, tail));
    }

    current.deinit(alloc);
    return parts;
}

fn collectStatements(alloc: std.mem.Allocator, content: []const u8) !std.ArrayList([]const u8) {
    return splitTopLevel(alloc, content, ';');
}

fn stripArraySuffix(token: []const u8) []const u8 {
    if (std.mem.indexOfScalar(u8, token, '[')) |idx| {
        return trimSpaces(token[0..idx]);
    }
    return trimSpaces(token);
}

fn stripTrailingParamName(param_token: []const u8) []const u8 {
    var token = stripArraySuffix(param_token);
    token = trimSpaces(token);
    if (token.len == 0) return token;

    var idx: isize = @as(isize, @intCast(token.len)) - 1;
    while (idx >= 0 and isSpace(token[@as(usize, @intCast(idx))])) : (idx -= 1) {}
    if (idx < 0) return token;

    var end_ident: isize = idx;
    while (end_ident >= 0 and isIdentChar(token[@as(usize, @intCast(end_ident))])) : (end_ident -= 1) {}

    const id_start: isize = end_ident + 1;
    if (id_start < 0 or id_start > idx) return token;

    const word = token[@as(usize, @intCast(id_start))..@as(usize, @intCast(idx + 1))];
    const known_words = [_][]const u8{
        "const",    "volatile", "restrict",
        "unsigned", "signed",   "short",
        "long",     "int",      "char",
        "float",    "double",   "void",
        "size_t",   "struct",   "union",
        "enum",     "bool",     "_Bool",
        "RLAPI",    "extern",   "static",
        "inline",   "register", "typedef",
    };

    for (known_words) |kw| {
        if (std.ascii.eqlIgnoreCase(word, kw)) return token;
    }

    var ident_count: usize = 0;
    var first_ident: []const u8 = "";
    var j: usize = 0;
    while (j < token.len) {
        while (j < token.len and !isIdentChar(token[j])) : (j += 1) {}
        if (j >= token.len) break;
        const start = j;
        while (j < token.len and isIdentChar(token[j])) : (j += 1) {}
        const ident = token[start..j];
        if (ident_count == 0) first_ident = ident;
        ident_count += 1;
    }

    if (ident_count == 2) {
        const qualifier_first = std.ascii.eqlIgnoreCase(first_ident, "const") or
            std.ascii.eqlIgnoreCase(first_ident, "volatile") or
            std.ascii.eqlIgnoreCase(first_ident, "restrict") or
            std.ascii.eqlIgnoreCase(first_ident, "signed") or
            std.ascii.eqlIgnoreCase(first_ident, "unsigned") or
            std.ascii.eqlIgnoreCase(first_ident, "short") or
            std.ascii.eqlIgnoreCase(first_ident, "long");
        if (qualifier_first) return token;
    }

    var p: isize = id_start - 1;
    while (p >= 0 and isSpace(token[@as(usize, @intCast(p))])) : (p -= 1) {}
    if (p >= 0 and token[@as(usize, @intCast(p))] == '*') {
        return trimSpaces(token[0..@as(usize, @intCast(id_start))]);
    }

    return trimSpaces(token[0..@as(usize, @intCast(id_start))]);
}

fn countSubstring(hay: []const u8, needle: []const u8) usize {
    var count: usize = 0;
    var start: usize = 0;
    while (std.mem.indexOfPos(u8, hay, start, needle)) |pos| {
        count += 1;
        start = pos + needle.len;
        if (start >= hay.len) break;
    }
    return count;
}

fn buildPtrType(alloc: std.mem.Allocator, base: []const u8, depth: usize) ![]u8 {
    var result = utils.dupe(u8, alloc, base);
    var i: usize = 0;
    while (i < depth) : (i += 1) {
        result = try std.fmt.allocPrint(alloc, "ptr<{s}>", .{result});
    }
    return result;
}

fn stripTrailingStars(raw: []const u8) []const u8 {
    var i: isize = @as(isize, @intCast(raw.len)) - 1;
    while (i >= 0) : (i -= 1) {
        const ch = raw[@as(usize, @intCast(i))];
        if (isSpace(ch) or ch == '*') {
            continue;
        }
        return trimSpaces(raw[0..@as(usize, @intCast(i + 1))]);
    }
    return "";
}

fn findIdentifierBeforeIndex(input: []const u8, end_exclusive: usize) ?[]const u8 {
    if (end_exclusive == 0) return null;

    var i: isize = @as(isize, @intCast(end_exclusive)) - 1;
    while (i >= 0 and isSpace(input[@as(usize, @intCast(i))])) : (i -= 1) {}
    if (i < 0) return null;

    const end_idx = @as(usize, @intCast(i + 1));
    while (i >= 0 and isIdentChar(input[@as(usize, @intCast(i))])) : (i -= 1) {}
    const start_idx = @as(usize, @intCast(i + 1));

    if (start_idx >= end_idx) return null;
    return input[start_idx..end_idx];
}

fn parseDeclNameInfo(decl: []const u8) ?DeclNameInfo {
    var end = decl.len;
    while (end > 0 and isSpace(decl[end - 1])) : (end -= 1) {}
    if (end == 0) return null;

    var array_len: ?usize = null;
    var had_array_suffix = false;
    if (decl[end - 1] == ']') {
        had_array_suffix = true;
        var lb = end - 1;
        while (lb > 0 and decl[lb] != '[') : (lb -= 1) {}
        if (decl[lb] == '[') {
            const inside = trimSpaces(decl[lb + 1 .. end - 1]);
            array_len = std.fmt.parseInt(usize, inside, 10) catch null;
            end = lb;
            while (end > 0 and isSpace(decl[end - 1])) : (end -= 1) {}
        }
    }

    const name = findIdentifierBeforeIndex(decl, end) orelse return null;
    return .{ .name = name, .array_len = array_len, .had_array_suffix = had_array_suffix };
}

fn resolveAlias(aliases: *const AliasMap, alias_name: []const u8) []const u8 {
    var current = alias_name;
    var i: usize = 0;
    while (i < 16) : (i += 1) {
        if (aliases.get(current)) |next| {
            if (std.mem.eql(u8, next, current)) break;
            current = next;
            continue;
        }
        break;
    }
    return current;
}

fn mapCTypeToZType(alloc: std.mem.Allocator, raw0: []const u8, aliases: *const AliasMap) !?[]u8 {
    var raw = trimSpaces(raw0);
    if (raw.len == 0) return null;

    if (std.mem.indexOfScalar(u8, raw, '(') != null and std.mem.indexOfScalar(u8, raw, ')') != null) {
        return utils.dupe(u8, alloc, "ptr<void>");
    }

    var ptr_depth: usize = 0;
    {
        var tmp = std.ArrayList(u8){};
        defer tmp.deinit(alloc);
        for (raw) |ch| {
            if (ch == '*') {
                ptr_depth += 1;
            } else {
                try tmp.append(alloc, ch);
            }
        }
        raw = utils.dupe(u8, alloc, trimSpaces(tmp.items));
    }

    var lowered = try asciiLower(alloc, raw);
    lowered = try squeezeSpaces(alloc, lowered);

    const has_unsigned = std.mem.indexOf(u8, lowered, "unsigned") != null;
    const has_signed = std.mem.indexOf(u8, lowered, "signed") != null;
    const has_short = std.mem.indexOf(u8, lowered, "short") != null;
    const long_count = countSubstring(lowered, "long");
    const has_int = std.mem.indexOf(u8, lowered, "int") != null;
    const has_char = std.mem.indexOf(u8, lowered, "char") != null;
    const has_float = std.mem.indexOf(u8, lowered, "float") != null;
    const has_double = std.mem.indexOf(u8, lowered, "double") != null;
    const has_size_t = std.mem.indexOf(u8, lowered, "size_t") != null;
    const has_void = std.mem.indexOf(u8, lowered, "void") != null;
    const has_bool = std.mem.indexOf(u8, lowered, "bool") != null or std.mem.indexOf(u8, lowered, "_bool") != null;
    const has_enum_kw = std.mem.indexOf(u8, lowered, "enum ") != null;

    var base: []const u8 = "";
    if (has_size_t) {
        base = "u64";
    } else if (has_bool and !has_int and !has_char) {
        base = "bool";
    } else if (has_float and !has_double) {
        base = "f32";
    } else if (has_double) {
        base = "f64";
    } else if (has_char) {
        base = if (ptr_depth > 0 or has_unsigned) "u8" else "i8";
    } else if (has_short) {
        base = if (has_unsigned) "u16" else "i16";
    } else if (long_count >= 2) {
        base = if (has_unsigned) "u64" else "i64";
    } else if (long_count == 1 and !has_double) {
        base = if (has_unsigned) "u64" else "i64";
    } else if (has_int or has_signed or has_unsigned) {
        base = if (has_unsigned) "u32" else "i32";
    } else if (has_void) {
        base = "void";
    } else if (has_enum_kw) {
        base = "i32";
    } else {
        const custom_name = findIdentifierBeforeIndex(raw, raw.len) orelse {
            if (ptr_depth > 0) {
                return utils.dupe(u8, alloc, "ptr<void>");
            }
            return null;
        };

        if (std.ascii.eqlIgnoreCase(custom_name, "const") or
            std.ascii.eqlIgnoreCase(custom_name, "volatile") or
            std.ascii.eqlIgnoreCase(custom_name, "restrict") or
            std.ascii.eqlIgnoreCase(custom_name, "register") or
            std.ascii.eqlIgnoreCase(custom_name, "extern") or
            std.ascii.eqlIgnoreCase(custom_name, "static"))
        {
            if (ptr_depth > 0) {
                return utils.dupe(u8, alloc, "ptr<void>");
            }
            return null;
        }

        base = resolveAlias(aliases, custom_name);
    }

    if (std.mem.eql(u8, base, "void") and ptr_depth == 0) {
        return utils.dupe(u8, alloc, base);
    }

    if (ptr_depth > 0) {
        return try buildPtrType(alloc, base, ptr_depth);
    }

    return utils.dupe(u8, alloc, base);
}

fn parseTypedefAlias(alloc: std.mem.Allocator, stmt: []const u8, aliases: *AliasMap) !void {
    const trimmed = trimSpaces(stmt);
    if (trimmed.len == 0) return;

    var lowered = try asciiLower(alloc, trimmed);
    lowered = try squeezeSpaces(alloc, lowered);
    if (!std.mem.startsWith(u8, lowered, "typedef ")) return;

    if (std.mem.indexOf(u8, trimmed, "(*")) |start| {
        if (std.mem.indexOfPos(u8, trimmed, start + 2, ")")) |end| {
            const alias = trimSpaces(trimmed[start + 2 .. end]);
            if (alias.len > 0) {
                try aliases.put(utils.dupe(u8, alloc, alias), "ptr<void>");
            }
            return;
        }
    }

    if (std.mem.indexOfScalar(u8, trimmed, '{') != null or
        std.mem.indexOfScalar(u8, trimmed, '}') != null or
        std.mem.indexOfScalar(u8, trimmed, '(') != null)
    {
        return;
    }

    const body = trimSpaces(trimmed[7..]);
    const alias_name = findIdentifierBeforeIndex(body, body.len) orelse return;
    if (alias_name.len == 0) return;

    const alias_start = std.mem.lastIndexOf(u8, body, alias_name) orelse return;
    const base_raw = trimSpaces(body[0..alias_start]);
    if (base_raw.len == 0) return;

    const mapped = try mapCTypeToZType(alloc, base_raw, aliases) orelse return;
    try aliases.put(utils.dupe(u8, alloc, alias_name), mapped);
}

fn parseOpaqueStructAlias(
    alloc: std.mem.Allocator,
    stmt: []const u8,
    structs: *StructMap,
    ordered_structs: *std.ArrayList(*StructInfo),
) !void {
    const trimmed = trimSpaces(stmt);
    if (trimmed.len == 0) return;

    var lowered = try asciiLower(alloc, trimmed);
    lowered = try squeezeSpaces(alloc, lowered);

    const is_struct = std.mem.startsWith(u8, lowered, "typedef struct");
    const is_union = std.mem.startsWith(u8, lowered, "typedef union");
    if (!is_struct and !is_union) return;
    if (std.mem.indexOfScalar(u8, trimmed, '{') != null) return;

    const alias_name = findIdentifierBeforeIndex(trimmed, trimmed.len) orelse return;
    if (alias_name.len == 0) return;

    const kw_len: usize = if (is_struct) "typedef struct".len else "typedef union".len;
    const between = trimSpaces(trimmed[kw_len..]);
    const alias_pos = std.mem.lastIndexOf(u8, between, alias_name) orelse 0;
    const tag_candidate = trimSpaces(between[0..alias_pos]);

    const chosen_name = if (tag_candidate.len > 0 and !std.mem.eql(u8, tag_candidate, alias_name))
        tag_candidate
    else
        alias_name;

    if (structs.contains(chosen_name)) return;

    const info = utils.create(StructInfo, alloc);
    info.* = .{
        .name = utils.dupe(u8, alloc, chosen_name),
        .is_union = is_union,
        .fields = std.ArrayList(StructFieldInfo){},
        .abi_mapping = .{},
    };

    try structs.put(info.name, info);
    try ordered_structs.append(alloc, info);
}

fn parseFieldDeclaration(alloc: std.mem.Allocator, decl: []const u8, aliases: *const AliasMap, out_fields: *std.ArrayList(StructFieldInfo)) !void {
    const trimmed = trimSpaces(decl);
    if (trimmed.len == 0) return;
    if (std.mem.indexOfScalar(u8, trimmed, ':') != null) return;

    if (std.mem.indexOfScalar(u8, trimmed, '(') != null and std.mem.indexOfScalar(u8, trimmed, ')') != null) {
        if (std.mem.indexOf(u8, trimmed, "(*")) |start| {
            if (std.mem.indexOfPos(u8, trimmed, start + 2, ")")) |end| {
                const name = trimSpaces(trimmed[start + 2 .. end]);
                if (name.len > 0) {
                    try out_fields.append(alloc, .{
                        .name = utils.dupe(u8, alloc, name),
                        .z_type = "ptr<void>",
                    });
                }
            }
        }
        return;
    }

    var declarators = try splitTopLevel(alloc, trimmed, ',');
    defer declarators.deinit(alloc);
    if (declarators.items.len == 0) return;

    const first_decl = trimSpaces(declarators.items[0]);
    const first_type_raw = stripTrailingParamName(first_decl);
    const common_base = stripTrailingStars(first_type_raw);

    for (declarators.items, 0..) |chunk, i| {
        const decl_piece = trimSpaces(chunk);
        if (decl_piece.len == 0) continue;

        const full_decl = if (i == 0)
            decl_piece
        else
            try std.fmt.allocPrint(alloc, "{s} {s}", .{ common_base, decl_piece });

        const type_raw = stripTrailingParamName(full_decl);
        var mapped_type = try mapCTypeToZType(alloc, type_raw, aliases) orelse "ptr<void>";

        const decl_name_info = parseDeclNameInfo(full_decl) orelse continue;

        if (decl_name_info.had_array_suffix) {
            if (decl_name_info.array_len) |len| {
                mapped_type = try std.fmt.allocPrint(alloc, "arr<{s}, {d}>", .{ mapped_type, len });
            } else {
                mapped_type = try std.fmt.allocPrint(alloc, "ptr<{s}>", .{mapped_type});
            }
        }

        try out_fields.append(alloc, .{
            .name = utils.dupe(u8, alloc, decl_name_info.name),
            .z_type = mapped_type,
        });
    }
}

fn parseTypedefStruct(
    alloc: std.mem.Allocator,
    stmt: []const u8,
    aliases: *const AliasMap,
    structs: *StructMap,
    ordered_structs: *std.ArrayList(*StructInfo),
) !void {
    const trimmed = trimSpaces(stmt);
    if (trimmed.len == 0) return;

    var lowered = try asciiLower(alloc, trimmed);
    lowered = try squeezeSpaces(alloc, lowered);

    const is_struct = std.mem.startsWith(u8, lowered, "typedef struct");
    const is_union = std.mem.startsWith(u8, lowered, "typedef union");
    if (!is_struct and !is_union) return;

    const open_brace = std.mem.indexOfScalar(u8, trimmed, '{') orelse return;
    const close_brace = std.mem.lastIndexOfScalar(u8, trimmed, '}') orelse return;
    if (close_brace <= open_brace) return;

    const alias_part = trimSpaces(trimmed[close_brace + 1 ..]);
    const struct_name = findIdentifierBeforeIndex(alias_part, alias_part.len) orelse return;

    const body = trimmed[open_brace + 1 .. close_brace];
    var field_stmts = try splitTopLevel(alloc, body, ';');
    defer field_stmts.deinit(alloc);

    var struct_info: *StructInfo = undefined;
    var is_new = false;

    if (structs.get(struct_name)) |existing| {
        if (existing.fields.items.len > 0) return;
        existing.is_union = is_union;
        existing.fields.clearRetainingCapacity();
        existing.abi_mapping = .{};
        struct_info = existing;
    } else {
        const created = utils.create(StructInfo, alloc);
        created.* = .{
            .name = utils.dupe(u8, alloc, struct_name),
            .is_union = is_union,
            .fields = std.ArrayList(StructFieldInfo){},
            .abi_mapping = .{},
        };
        struct_info = created;
        is_new = true;
    }

    for (field_stmts.items) |field_stmt| {
        try parseFieldDeclaration(alloc, field_stmt, aliases, &struct_info.fields);
    }

    var abi_fields = std.ArrayList(c_abi.StructField){};
    defer abi_fields.deinit(alloc);
    for (struct_info.fields.items) |field| {
        try abi_fields.append(alloc, .{
            .name = field.name,
            .z_type = field.z_type,
        });
    }
    struct_info.abi_mapping = c_abi.classifyStruct(abi_fields.items);

    if (is_new) {
        try structs.put(struct_info.name, struct_info);
        try ordered_structs.append(alloc, struct_info);
    }
}

fn isPointerType(type_name: []const u8) bool {
    return std.mem.startsWith(u8, type_name, "ptr<") and std.mem.endsWith(u8, type_name, ">");
}

fn classifyTypeForAbi(type_name: []const u8, structs: *const StructMap) struct {
    abi_type: []const u8,
    conversion: c_abi.ConversionKind,
    struct_name: ?[]const u8,
} {
    if (isPointerType(type_name)) {
        return .{ .abi_type = type_name, .conversion = .none, .struct_name = null };
    }

    if (structs.get(type_name)) |info| {
        if (info.abi_mapping.hasConversion()) {
            return .{
                .abi_type = info.abi_mapping.abi_type.?,
                .conversion = info.abi_mapping.conversion,
                .struct_name = info.name,
            };
        }
    }

    return .{ .abi_type = type_name, .conversion = .none, .struct_name = null };
}

fn parseFunctionStatement(alloc: std.mem.Allocator, stmt_in: []const u8, aliases: *const AliasMap, structs: *const StructMap) !?FunctionInfo {
    const stmt = trimSpaces(stmt_in);
    if (stmt.len == 0) return null;
    if (std.mem.indexOf(u8, stmt, "typedef") != null) return null;
    if (std.mem.indexOfScalar(u8, stmt, '{') != null or std.mem.indexOfScalar(u8, stmt, '}') != null) return null;
    if (std.mem.indexOfScalar(u8, stmt, '=') != null) return null;

    const open = std.mem.indexOfScalar(u8, stmt, '(') orelse return null;
    const close = std.mem.lastIndexOfScalar(u8, stmt, ')') orelse return null;
    if (close <= open) return null;

    const inner = stmt[open + 1 .. close];
    if (std.mem.indexOfScalar(u8, inner, '(') != null or std.mem.indexOfScalar(u8, inner, ')') != null) return null;

    const name = findIdentifierBeforeIndex(stmt, open) orelse return null;
    const name_start = std.mem.lastIndexOf(u8, stmt[0..open], name) orelse return null;

    const ret_raw = trimSpaces(stmt[0..name_start]);
    const ret_z = try mapCTypeToZType(alloc, ret_raw, aliases) orelse return null;
    const ret_info = classifyTypeForAbi(ret_z, structs);

    var fn_info = FunctionInfo{
        .name = utils.dupe(u8, alloc, name),
        .c_ret_type = utils.dupe(u8, alloc, ret_raw),
        .ret_z_type = ret_z,
        .ret_abi_type = ret_info.abi_type,
        .ret_conversion = ret_info.conversion,
        .ret_struct_name = ret_info.struct_name,
        .params = std.ArrayList(ParamInfo){},
        .has_varargs = false,
    };

    const inner_trim = trimSpaces(inner);
    if (!(inner_trim.len == 0 or std.mem.eql(u8, inner_trim, "void"))) {
        var params = try splitTopLevel(alloc, inner_trim, ',');
        defer params.deinit(alloc);

        for (params.items, 0..) |param_raw, index| {
            const param = trimSpaces(param_raw);
            if (param.len == 0) continue;

            if (std.mem.eql(u8, param, "...")) {
                fn_info.has_varargs = true;
                continue;
            }

            const type_raw = stripTrailingParamName(param);
            const z_type = try mapCTypeToZType(alloc, type_raw, aliases) orelse {
                fn_info.params.deinit(alloc);
                return null;
            };

            const abi_info = classifyTypeForAbi(z_type, structs);
            const param_name = try std.fmt.allocPrint(alloc, "p{d}", .{index});

            try fn_info.params.append(alloc, .{
                .name = param_name,
                .c_type = utils.dupe(u8, alloc, type_raw),
                .z_type = z_type,
                .abi_type = abi_info.abi_type,
                .conversion = abi_info.conversion,
                .struct_name = abi_info.struct_name,
            });
        }
    }

    return fn_info;
}

fn emitStructDeclarations(writer: anytype, ordered_structs: []const *StructInfo) !void {
    if (ordered_structs.len == 0) return;

    for (ordered_structs) |info| {
        const keyword = if (info.is_union) "union" else "struct";
        try writer.print("{s} {s} {{\n", .{ keyword, info.name });
        for (info.fields.items) |field| {
            try writer.print("    {s} {s},\n", .{ field.name, field.z_type });
        }
        try writer.writeAll("}\n\n");
    }
}

fn collectCustomTypeNamesFromType(alloc: std.mem.Allocator, type_name_in: []const u8, out: *std.StringHashMap(void)) !void {
    const type_name = trimSpaces(type_name_in);
    if (type_name.len == 0) return;
    if (isBuiltinTypeName(type_name)) return;
    if (std.mem.eql(u8, type_name, "void")) return;

    if (std.mem.startsWith(u8, type_name, "ptr<") and std.mem.endsWith(u8, type_name, ">")) {
        try collectCustomTypeNamesFromType(alloc, type_name[4 .. type_name.len - 1], out);
        return;
    }

    if (std.mem.startsWith(u8, type_name, "arr<") and std.mem.endsWith(u8, type_name, ">")) {
        var parts = try splitTopLevel(alloc, type_name[4 .. type_name.len - 1], ',');
        defer parts.deinit(alloc);
        if (parts.items.len > 0) {
            try collectCustomTypeNamesFromType(alloc, parts.items[0], out);
        }
        return;
    }

    if (std.mem.startsWith(u8, type_name, "simd<") and std.mem.endsWith(u8, type_name, ">")) {
        return;
    }

    if (std.ascii.eqlIgnoreCase(type_name, "const") or
        std.ascii.eqlIgnoreCase(type_name, "volatile") or
        std.ascii.eqlIgnoreCase(type_name, "restrict") or
        std.ascii.eqlIgnoreCase(type_name, "register"))
    {
        return;
    }

    try out.put(utils.dupe(u8, alloc, type_name), {});
}

fn emitOpaqueTypeDeclarations(
    writer: anytype,
    alloc: std.mem.Allocator,
    ordered_structs: []const *StructInfo,
    enums: []const EnumInfo,
    functions: []const FunctionInfo,
) !void {
    var known_structs = std.StringHashMap(void).init(alloc);
    defer known_structs.deinit();
    for (ordered_structs) |info| {
        try known_structs.put(info.name, {});
    }

    var known_enums = std.StringHashMap(void).init(alloc);
    defer known_enums.deinit();
    for (enums) |e| {
        try known_enums.put(e.name, {});
    }

    var needed = std.StringHashMap(void).init(alloc);
    defer needed.deinit();

    for (ordered_structs) |info| {
        for (info.fields.items) |field| {
            try collectCustomTypeNamesFromType(alloc, field.z_type, &needed);
        }
    }
    for (functions) |fn_info| {
        try collectCustomTypeNamesFromType(alloc, fn_info.ret_z_type, &needed);
        try collectCustomTypeNamesFromType(alloc, fn_info.ret_abi_type, &needed);
        for (fn_info.params.items) |param| {
            try collectCustomTypeNamesFromType(alloc, param.z_type, &needed);
            try collectCustomTypeNamesFromType(alloc, param.abi_type, &needed);
        }
    }

    var it = needed.iterator();
    var emitted_any = false;
    while (it.next()) |entry| {
        const name = entry.key_ptr.*;
        if (known_structs.contains(name)) continue;
        if (known_enums.contains(name)) continue;
        try writer.print("struct {s} {{\n}}\n\n", .{name});
        emitted_any = true;
    }

    if (emitted_any) {
        return;
    }
}

fn emitPackUnpackHelpers(writer: anytype, ordered_structs: []const *StructInfo, used_helpers: *const std.StringHashMap(void)) !void {
    for (ordered_structs) |info| {
        if (!used_helpers.contains(info.name)) continue;
        if (!info.abi_mapping.hasConversion()) continue;

        switch (info.abi_mapping.conversion) {
            .simd_f32_2, .simd_f32_4 => {
                const abi_type = info.abi_mapping.abi_type.?;
                try writer.print("fun __abi_pack_{s}(value: {s}) >> {s} {{\n", .{ info.name, info.name, abi_type });
                try writer.print("    {s} tmp = {{", .{abi_type});
                for (info.fields.items, 0..) |field, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("value.{s}", .{field.name});
                }
                try writer.writeAll("};\n");
                try writer.writeAll("    return tmp;\n");
                try writer.writeAll("}\n\n");

                try writer.print("fun __abi_unpack_{s}(value: {s}) >> {s} {{\n", .{ info.name, abi_type, info.name });
                try writer.print("    {s} tmp = {s}{{", .{ info.name, info.name });
                for (info.fields.items, 0..) |field, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("{s} = value[{d}]", .{ field.name, i });
                }
                try writer.writeAll("};\n");
                try writer.writeAll("    return tmp;\n");
                try writer.writeAll("}\n\n");
            },
            .packed_rgba_u32 => {
                if (info.fields.items.len < 4) continue;
                const f0 = info.fields.items[0].name;
                const f1 = info.fields.items[1].name;
                const f2 = info.fields.items[2].name;
                const f3 = info.fields.items[3].name;

                try writer.print("fun __abi_pack_{s}(value: {s}) >> u32 {{\n", .{ info.name, info.name });
                try writer.print("    return (value.{s} as u32) |\n", .{f0});
                try writer.print("           ((value.{s} as u32) << 8) |\n", .{f1});
                try writer.print("           ((value.{s} as u32) << 16) |\n", .{f2});
                try writer.print("           ((value.{s} as u32) << 24);\n", .{f3});
                try writer.writeAll("}\n\n");

                try writer.print("fun __abi_unpack_{s}(value: u32) >> {s} {{\n", .{ info.name, info.name });
                try writer.print("    {s} tmp = {s}{{\n", .{ info.name, info.name });
                try writer.print("        {s} = (value & 0xFF) as u8,\n", .{f0});
                try writer.print("        {s} = ((value >> 8) & 0xFF) as u8,\n", .{f1});
                try writer.print("        {s} = ((value >> 16) & 0xFF) as u8,\n", .{f2});
                try writer.print("        {s} = ((value >> 24) & 0xFF) as u8,\n", .{f3});
                try writer.writeAll("    };\n");
                try writer.writeAll("    return tmp;\n");
                try writer.writeAll("}\n\n");
            },
            .none => {},
        }
    }
}

fn emitParamList(writer: anytype, params: []const ParamInfo, abi: bool, has_varargs: bool) !void {
    for (params, 0..) |param, i| {
        if (i > 0) try writer.writeAll(", ");
        const ty = if (abi) param.abi_type else param.z_type;
        try writer.print("{s}: {s}", .{ param.name, ty });
    }
    if (has_varargs) {
        if (params.len > 0) try writer.writeAll(", ");
        try writer.writeAll("__varargs: vararg<_>");
    }
}

fn needsWrapper(fn_info: FunctionInfo) bool {
    if (fn_info.ret_conversion != .none) return true;
    if (!std.mem.eql(u8, fn_info.ret_z_type, fn_info.ret_abi_type)) return true;

    for (fn_info.params.items) |param| {
        if (param.conversion != .none) return true;
        if (!std.mem.eql(u8, param.z_type, param.abi_type)) return true;
    }

    return false;
}

fn isBuiltinTypeName(type_name: []const u8) bool {
    return std.mem.eql(u8, type_name, "void") or
        std.mem.eql(u8, type_name, "bool") or
        std.mem.eql(u8, type_name, "i8") or
        std.mem.eql(u8, type_name, "i16") or
        std.mem.eql(u8, type_name, "i32") or
        std.mem.eql(u8, type_name, "i64") or
        std.mem.eql(u8, type_name, "u8") or
        std.mem.eql(u8, type_name, "u16") or
        std.mem.eql(u8, type_name, "u32") or
        std.mem.eql(u8, type_name, "u64") or
        std.mem.eql(u8, type_name, "f16") or
        std.mem.eql(u8, type_name, "f32") or
        std.mem.eql(u8, type_name, "f64");
}

fn shouldSkipWrapper(fn_info: FunctionInfo, structs: *const StructMap) bool {
    if (!needsWrapper(fn_info)) return false;

    if (fn_info.ret_conversion == .none and !isBuiltinTypeName(fn_info.ret_z_type) and !isPointerType(fn_info.ret_z_type) and structs.contains(fn_info.ret_z_type)) {
        return true;
    }

    for (fn_info.params.items) |param| {
        if (param.conversion != .none) continue;
        if (isBuiltinTypeName(param.z_type)) continue;
        if (isPointerType(param.z_type)) continue;
        if (structs.contains(param.z_type)) return true;
    }

    return false;
}

fn isKnownUnavailableLinkSymbol(name: []const u8) bool {
    return std.mem.eql(u8, name, "DrawLineDashed");
}

fn emitCallArgs(writer: anytype, fn_info: FunctionInfo) !void {
    for (fn_info.params.items, 0..) |param, i| {
        if (i > 0) try writer.writeAll(", ");
        switch (param.conversion) {
            .simd_f32_2, .simd_f32_4, .packed_rgba_u32 => {
                const struct_name = param.struct_name orelse {
                    try writer.print("{s}", .{param.name});
                    continue;
                };
                try writer.print("__abi_pack_{s}({s})", .{ struct_name, param.name });
            },
            .none => {
                if (!std.mem.eql(u8, param.z_type, param.abi_type)) {
                    try writer.print("({s} as _)", .{param.name});
                } else {
                    try writer.print("{s}", .{param.name});
                }
            },
        }
    }
}

fn emitFunctionWrappers(writer: anytype, functions: []const FunctionInfo, used_helpers: *std.StringHashMap(void), structs: *const StructMap) !void {
    for (functions) |fn_info| {
        const wrap_needed = needsWrapper(fn_info);

        if (!wrap_needed) {
            try writer.print("wrap @{s}(", .{fn_info.name});
            try emitParamList(writer, fn_info.params.items, false, fn_info.has_varargs);
            try writer.print(") >> {s};\n", .{fn_info.ret_z_type});
            continue;
        }

        if (isKnownUnavailableLinkSymbol(fn_info.name)) {
            std.debug.print("warning: skipped function {s}: symbol may be unavailable in linked library\n", .{fn_info.name});
            continue;
        }

        try writer.print("fun @{s}(", .{fn_info.name});
        try emitParamList(writer, fn_info.params.items, true, fn_info.has_varargs);
        try writer.print(") >> {s};\n", .{fn_info.ret_abi_type});

        if (shouldSkipWrapper(fn_info, structs)) {
            std.debug.print("warning: skipped high-level wrapper for {s}: complex ABI signature\n", .{fn_info.name});
            try writer.writeAll("\n");
            continue;
        }

        if (fn_info.has_varargs) {
            try writer.writeAll("\n");
            continue;
        }

        if (fn_info.ret_struct_name) |sn| {
            if (fn_info.ret_conversion != .none) {
                try used_helpers.put(sn, {});
            }
        }
        for (fn_info.params.items) |param| {
            if (param.struct_name) |sn| {
                if (param.conversion != .none) {
                    try used_helpers.put(sn, {});
                }
            }
        }

        try writer.print("fun {s}(", .{fn_info.name});
        try emitParamList(writer, fn_info.params.items, false, false);
        try writer.print(") >> {s} {{\n", .{fn_info.ret_z_type});

        if (std.mem.eql(u8, fn_info.ret_z_type, "void")) {
            try writer.print("    @{s}(", .{fn_info.name});
            try emitCallArgs(writer, fn_info);
            try writer.writeAll(");\n");
            try writer.writeAll("}\n\n");
            continue;
        }

        try writer.print("    return ", .{});
        switch (fn_info.ret_conversion) {
            .simd_f32_2, .simd_f32_4, .packed_rgba_u32 => {
                const struct_name = fn_info.ret_struct_name orelse "";
                try writer.print("__abi_unpack_{s}(@{s}(", .{ struct_name, fn_info.name });
                try emitCallArgs(writer, fn_info);
                try writer.writeAll("));\n");
            },
            .none => {
                if (!std.mem.eql(u8, fn_info.ret_z_type, fn_info.ret_abi_type)) {
                    try writer.print("(@{s}(", .{fn_info.name});
                    try emitCallArgs(writer, fn_info);
                    try writer.writeAll(") as _);\n");
                } else {
                    try writer.print("@{s}(", .{fn_info.name});
                    try emitCallArgs(writer, fn_info);
                    try writer.writeAll(");\n");
                }
            },
        }

        try writer.writeAll("}\n\n");
    }
}

fn readFile(alloc: std.mem.Allocator, file_path: []const u8) ![]u8 {
    const cwd = std.fs.cwd();
    const stat = try cwd.statFile(file_path);
    const file = try cwd.openFile(file_path, .{ .mode = .read_only });
    defer file.close();

    const cap = if (stat.size == 0) 1 else stat.size;
    const buffer = utils.alloc(u8, alloc, @intCast(cap));
    const bytes_read = try file.readAll(buffer);
    return buffer[0..bytes_read];
}

const ProbeSignature = struct {
    ret_llvm_type: []const u8,
    param_llvm_types: std.ArrayList([]const u8),
};

fn parseFirstLlvmType(param: []const u8) ?[]const u8 {
    const s = trimSpaces(param);
    if (s.len == 0) return null;

    if (s[0] == '<') {
        var depth: usize = 0;
        var i: usize = 0;
        while (i < s.len) : (i += 1) {
            if (s[i] == '<') depth += 1;
            if (s[i] == '>') {
                depth -= 1;
                if (depth == 0) return trimSpaces(s[0 .. i + 1]);
            }
        }
        return null;
    }

    if (s[0] == '[') {
        var depth: usize = 0;
        var i: usize = 0;
        while (i < s.len) : (i += 1) {
            if (s[i] == '[') depth += 1;
            if (s[i] == ']') {
                depth -= 1;
                if (depth == 0) return trimSpaces(s[0 .. i + 1]);
            }
        }
        return null;
    }

    var end: usize = 0;
    while (end < s.len and !isSpace(s[end]) and s[end] != ',') : (end += 1) {}
    if (end == 0) return null;
    return trimSpaces(s[0..end]);
}

fn parseProbeSignaturesFromLl(alloc: std.mem.Allocator, ll_text: []const u8) !std.StringHashMap(ProbeSignature) {
    var out = std.StringHashMap(ProbeSignature).init(alloc);

    var lines = std.mem.splitScalar(u8, ll_text, '\n');
    while (lines.next()) |line_raw| {
        const line = trimSpaces(line_raw);
        if (!std.mem.startsWith(u8, line, "define ")) continue;

        const marker = "@__zprobe_";
        const at_pos = std.mem.indexOf(u8, line, marker) orelse continue;

        const name_start = at_pos + marker.len;
        const open_pos_rel = std.mem.indexOfScalar(u8, line[name_start..], '(') orelse continue;
        const open_pos = name_start + open_pos_rel;
        const fn_name = line[name_start..open_pos];
        if (fn_name.len == 0) continue;

        var close_pos: ?usize = null;
        var depth: usize = 0;
        var i = open_pos;
        while (i < line.len) : (i += 1) {
            if (line[i] == '(') depth += 1;
            if (line[i] == ')') {
                depth -= 1;
                if (depth == 0) {
                    close_pos = i;
                    break;
                }
            }
        }
        if (close_pos == null) continue;

        var type_end: isize = @as(isize, @intCast(at_pos)) - 1;
        while (type_end >= 0 and isSpace(line[@as(usize, @intCast(type_end))])) : (type_end -= 1) {}
        if (type_end < 0) continue;

        var type_start: isize = type_end;
        var angle_depth: isize = 0;
        var square_depth: isize = 0;
        while (type_start >= 0) : (type_start -= 1) {
            const ch = line[@as(usize, @intCast(type_start))];
            if (ch == '>') angle_depth += 1;
            if (ch == '<') angle_depth -= 1;
            if (ch == ']') square_depth += 1;
            if (ch == '[') square_depth -= 1;
            if (isSpace(ch) and angle_depth == 0 and square_depth == 0) break;
        }
        const ret_type = trimSpaces(line[@as(usize, @intCast(type_start + 1))..@as(usize, @intCast(type_end + 1))]);
        if (ret_type.len == 0) continue;

        const params_chunk = line[open_pos + 1 .. close_pos.?];
        var params = try splitTopLevel(alloc, params_chunk, ',');
        defer params.deinit(alloc);

        var param_types = std.ArrayList([]const u8){};
        for (params.items) |param_raw| {
            const p = trimSpaces(param_raw);
            if (p.len == 0) continue;
            if (std.mem.eql(u8, p, "void")) continue;
            if (parseFirstLlvmType(p)) |t| {
                try param_types.append(alloc, utils.dupe(u8, alloc, t));
            }
        }

        try out.put(utils.dupe(u8, alloc, fn_name), .{
            .ret_llvm_type = utils.dupe(u8, alloc, ret_type),
            .param_llvm_types = param_types,
        });
    }

    return out;
}

fn inferConversionFromLlvmType(struct_info: *const StructInfo, llvm_ty: []const u8) ?struct {
    abi_type: []const u8,
    conversion: c_abi.ConversionKind,
} {
    const t = trimSpaces(llvm_ty);
    if (std.mem.eql(u8, t, "<2 x float>") and struct_info.fields.items.len == 2) {
        return .{ .abi_type = "simd<f32, 2>", .conversion = .simd_f32_2 };
    }
    if (std.mem.eql(u8, t, "<4 x float>") and struct_info.fields.items.len == 4) {
        return .{ .abi_type = "simd<f32, 4>", .conversion = .simd_f32_4 };
    }
    if (std.mem.eql(u8, t, "i32") and struct_info.fields.items.len == 4) {
        const f = struct_info.fields.items;
        if (std.mem.eql(u8, f[0].z_type, "u8") and std.mem.eql(u8, f[1].z_type, "u8") and std.mem.eql(u8, f[2].z_type, "u8") and std.mem.eql(u8, f[3].z_type, "u8")) {
            return .{ .abi_type = "u32", .conversion = .packed_rgba_u32 };
        }
    }
    return null;
}

fn sanitizeForProbe(alloc: std.mem.Allocator, raw: []const u8) ![]const u8 {
    var s = trimSpaces(raw);
    if (std.mem.startsWith(u8, s, "extern ")) s = trimSpaces(s[7..]);
    if (std.mem.startsWith(u8, s, "static ")) s = trimSpaces(s[7..]);
    if (std.mem.startsWith(u8, s, "inline ")) s = trimSpaces(s[7..]);
    return utils.dupe(u8, alloc, s);
}

fn refineAbiUsingClangProbe(alloc: std.mem.Allocator, header_path: []const u8, functions: *std.ArrayList(FunctionInfo), structs: *const StructMap) !void {
    const clang_path = (try llvm_tools.getLLVMToolPath(alloc, .clang)) orelse return;
    defer if (!std.mem.eql(u8, clang_path, "clang")) alloc.free(clang_path);

    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const probe_c = try std.fmt.allocPrint(a, ".zig-cache/wrapgen/probe_{d}.c", .{std.time.nanoTimestamp()});
    const probe_ll = try std.fmt.allocPrint(a, ".zig-cache/wrapgen/probe_{d}.ll", .{std.time.nanoTimestamp()});
    defer std.fs.cwd().deleteFile(probe_c) catch {};
    defer std.fs.cwd().deleteFile(probe_ll) catch {};

    var src = std.ArrayList(u8){};
    defer src.deinit(a);
    const w = src.writer(a);
    try w.print("#include \"{s}\"\n\n", .{header_path});

    for (functions.items) |fn_info| {
        if (fn_info.has_varargs) continue;

        const ret_c = try sanitizeForProbe(a, fn_info.c_ret_type);
        try w.print("{s} __zprobe_{s}(", .{ ret_c, fn_info.name });
        for (fn_info.params.items, 0..) |param, i| {
            if (i > 0) try w.writeAll(", ");
            const pty = try sanitizeForProbe(a, param.c_type);
            try w.print("{s} a{d}", .{ pty, i });
        }
        try w.writeAll(") {\n");

        if (std.mem.eql(u8, trimSpaces(ret_c), "void")) {
            try w.print("    {s}(", .{fn_info.name});
        } else {
            try w.print("    return {s}(", .{fn_info.name});
        }
        for (fn_info.params.items, 0..) |_, i| {
            if (i > 0) try w.writeAll(", ");
            try w.print("a{d}", .{i});
        }
        try w.writeAll(");\n}\n\n");
    }

    var f = try std.fs.cwd().createFile(probe_c, .{ .truncate = true });
    defer f.close();
    try f.writeAll(src.items);

    var argv = std.ArrayList([]const u8){};
    defer argv.deinit(a);
    try argv.append(a, clang_path);
    try argv.append(a, "-S");
    try argv.append(a, "-emit-llvm");
    try argv.append(a, "-O0");
    try argv.append(a, "-x");
    try argv.append(a, "c");
    try argv.append(a, probe_c);
    try argv.append(a, "-o");
    try argv.append(a, probe_ll);

    var child = std.process.Child.init(argv.items, a);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Ignore;
    child.stderr_behavior = .Ignore;
    const term = try child.spawnAndWait();
    switch (term) {
        .Exited => |code| if (code != 0) return,
        else => return,
    }

    const ll = try readFile(a, probe_ll);
    var sigs = try parseProbeSignaturesFromLl(a, ll);

    for (functions.items) |*fn_info| {
        const sig = sigs.get(fn_info.name) orelse continue;

        if (fn_info.ret_struct_name) |sn| {
            if (structs.get(sn)) |info| {
                if (inferConversionFromLlvmType(info, sig.ret_llvm_type)) |conv| {
                    fn_info.ret_abi_type = conv.abi_type;
                    fn_info.ret_conversion = conv.conversion;
                }
            }
        }

        for (fn_info.params.items, 0..) |*param, idx| {
            if (param.struct_name) |sn| {
                if (idx < sig.param_llvm_types.items.len) {
                    if (structs.get(sn)) |info| {
                        if (inferConversionFromLlvmType(info, sig.param_llvm_types.items[idx])) |conv| {
                            param.abi_type = conv.abi_type;
                            param.conversion = conv.conversion;
                        }
                    }
                }
            }
        }
    }
}

fn generateFromHeaderImpl(alloc: std.mem.Allocator, source_header_path: []const u8, parse_input_path: []const u8, out_path: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    const header_src = try readFile(a, parse_input_path);
    const comments_only = try stripCommentsOnly(a, header_src);
    const cleaned = try stripCommentsAndPreproc(a, header_src);
    const normalized = try removeExternCGuards(a, cleaned);

    var statements = try collectStatements(a, normalized);
    defer statements.deinit(a);

    var aliases = AliasMap.init(a);
    var structs = StructMap.init(a);
    var ordered_structs = std.ArrayList(*StructInfo){};
    var enums = std.ArrayList(EnumInfo){};
    var enum_value_names = std.StringHashMap(void).init(a);
    var constants = std.ArrayList(ConstInfo){};
    var functions = std.ArrayList(FunctionInfo){};

    defer {
        aliases.deinit();
        structs.deinit();
        ordered_structs.deinit(a);
        for (enums.items) |*enum_info| {
            enum_info.values.deinit(a);
        }
        enums.deinit(a);
        enum_value_names.deinit();
        constants.deinit(a);
        functions.deinit(a);
    }

    try parseDefineConstants(a, comments_only, &constants);

    for (statements.items) |stmt| {
        try parseTypedefEnum(a, stmt, &enums, &enum_value_names);
    }

    for (statements.items) |stmt| {
        try parseTypedefAlias(a, stmt, &aliases);
    }

    for (statements.items) |stmt| {
        try parseOpaqueStructAlias(a, stmt, &structs, &ordered_structs);
    }

    for (statements.items) |stmt| {
        try parseTypedefStruct(a, stmt, &aliases, &structs, &ordered_structs);
    }

    for (statements.items) |stmt| {
        const parsed = try parseFunctionStatement(a, stmt, &aliases, &structs);
        if (parsed) |fn_info| {
            try functions.append(a, fn_info);
        }
    }

    refineAbiUsingClangProbe(a, source_header_path, &functions, &structs) catch {};

    var used_helpers = std.StringHashMap(void).init(a);
    defer used_helpers.deinit();

    var output = std.ArrayList(u8){};
    defer output.deinit(a);

    const writer = output.writer(a);
    try emitKnownHeaderFlags(writer, a, source_header_path);

    try emitOpaqueTypeDeclarations(writer, a, ordered_structs.items, enums.items, functions.items);

    try emitStructDeclarations(writer, ordered_structs.items);
    try emitEnumDeclarations(writer, enums.items);

    var emitted_const_names = std.StringHashMap(void).init(a);
    defer emitted_const_names.deinit();
    var skipped_const_names = std.StringHashMap(void).init(a);
    defer skipped_const_names.deinit();
    try emitConstants(writer, constants.items, &enum_value_names, &emitted_const_names, &skipped_const_names);
    if (emitted_const_names.count() > 0) {
        try writer.writeAll("\n");
    }

    for (functions.items) |fn_info| {
        if (!needsWrapper(fn_info)) continue;
        if (isKnownUnavailableLinkSymbol(fn_info.name)) continue;
        if (shouldSkipWrapper(fn_info, &structs)) continue;

        if (fn_info.ret_conversion != .none) {
            if (fn_info.ret_struct_name) |sn| {
                try used_helpers.put(sn, {});
            }
        }
        for (fn_info.params.items) |param| {
            if (param.conversion != .none) {
                if (param.struct_name) |sn| {
                    try used_helpers.put(sn, {});
                }
            }
        }
    }

    try emitPackUnpackHelpers(writer, ordered_structs.items, &used_helpers);
    try emitFunctionWrappers(writer, functions.items, &used_helpers, &structs);

    var out_file = try std.fs.cwd().createFile(out_path, .{ .truncate = true });
    defer out_file.close();
    try out_file.writeAll(output.items);
}

pub fn generateFromHeader(alloc: std.mem.Allocator, header_path: []const u8, out_path: []const u8) !void {
    try generateFromHeaderImpl(alloc, header_path, header_path, out_path);
}

pub fn generateFromHeaderWithClang(alloc: std.mem.Allocator, header_path: []const u8, out_path: []const u8, clang_args: []const []const u8) !void {
    const clang_path = (try llvm_tools.getLLVMToolPath(alloc, .clang)) orelse return error.ClangNotFound;
    defer if (!std.mem.eql(u8, clang_path, "clang")) alloc.free(clang_path);

    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const a = arena.allocator();

    try std.fs.cwd().makePath(".zig-cache/wrapgen");
    const tmp_path = try std.fmt.allocPrint(a, ".zig-cache/wrapgen/preprocessed_{d}.h", .{std.time.nanoTimestamp()});
    defer std.fs.cwd().deleteFile(tmp_path) catch {};

    var argv = std.ArrayList([]const u8){};
    defer argv.deinit(a);

    try argv.append(a, clang_path);
    try argv.append(a, "-E");
    try argv.append(a, "-dD");
    try argv.append(a, "-P");
    try argv.append(a, "-x");
    try argv.append(a, "c");

    for (clang_args) |arg| {
        try argv.append(a, arg);
    }

    try argv.append(a, header_path);
    try argv.append(a, "-o");
    try argv.append(a, tmp_path);

    var child = std.process.Child.init(argv.items, a);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    const term = try child.spawnAndWait();
    switch (term) {
        .Exited => |code| {
            if (code != 0) return error.ClangPreprocessFailed;
        },
        else => return error.ClangPreprocessFailed,
    }

    try generateFromHeaderImpl(alloc, header_path, tmp_path, out_path);
}
