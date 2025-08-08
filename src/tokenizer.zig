const std = @import("std");

extern fn zlang_lex_init(scanner: *?*anyopaque) c_int;
extern fn zlang_lex_destroy(scanner: ?*anyopaque) c_int;
extern fn zlang_lex(scanner: ?*anyopaque) c_int;
extern fn zlang_get_text(scanner: ?*anyopaque) [*c]u8;
extern fn zlang_set_in(file: ?*anyopaque, scanner: ?*anyopaque) void;

extern fn fmemopen(buffer: [*c]const u8, size: usize, mode: [*c]const u8) ?*anyopaque;
extern fn fclose(file: ?*anyopaque) c_int;

const TokenType = enum(c_int) {
    eof = 0,
    if_token = 256,
    else_token = 257,
    for_token = 258,
    identifier = 259,
    number = 260,
    plus = 261,
    minus = 262,
    assign = 263,
    equal = 264,
    newline = 265,
    semicolon = 266,

    pub fn toString(self: TokenType) []const u8 {
        return switch (self) {
            .eof => "EOF",
            .if_token => "IF",
            .else_token => "ELSE",
            .for_token => "FOR",
            .identifier => "IDENTIFIER",
            .number => "NUMBER",
            .plus => "PLUS",
            .minus => "MINUS",
            .assign => "ASSIGN",
            .equal => "EQUAL",
            .newline => "NEWLINE",
            .semicolon => "SEMICOLON",
        };
    }
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}('{s}')", .{ self.type.toString(), self.lexeme });
    }
};

pub const TokenizeError = error{
    LexerInitFailed,
    FileOpenFailed,
    OutOfMemory,
};

pub fn tokenize(allocator: std.mem.Allocator, input: []const u8) TokenizeError!std.ArrayList(Token) {
    var tokens = std.ArrayList(Token).init(allocator);
    errdefer tokens.deinit();
    var scanner: ?*anyopaque = null;
    if (zlang_lex_init(&scanner) != 0) {
        return TokenizeError.LexerInitFailed;
    }
    defer _ = zlang_lex_destroy(scanner);
    const null_terminated_input = try allocator.dupeZ(u8, input);
    defer allocator.free(null_terminated_input);
    const file = fmemopen(null_terminated_input.ptr, input.len, "r");
    if (file == null) {
        return TokenizeError.FileOpenFailed;
    }
    defer _ = fclose(file);
    zlang_set_in(file, scanner);
    while (true) {
        const token_type_int = zlang_lex(scanner);
        const token_type = @as(TokenType, @enumFromInt(token_type_int));
        if (token_type == .eof) break;
        const text_ptr = zlang_get_text(scanner);
        const lexeme = std.mem.span(text_ptr);
        const lexeme_copy = try allocator.dupe(u8, lexeme);

        try tokens.append(Token{
            .type = token_type,
            .lexeme = lexeme_copy,
        });
    }

    return tokens;
}

pub fn freeTokens(allocator: std.mem.Allocator, tokens: std.ArrayList(Token)) void {
    for (tokens.items) |token| {
        allocator.free(token.lexeme);
    }
    tokens.deinit();
}

pub fn printTokens(tokens: std.ArrayList(Token)) void {
    std.debug.print("Tokens found: {}\n", .{tokens.items.len});
    std.debug.print("==================================================\n", .{});

    for (tokens.items, 0..) |token, i| {
        std.debug.print("{:3}: {}\n", .{ i + 1, token });
    }

    if (tokens.items.len == 0) {
        std.debug.print("(No tokens)\n", .{});
    }
}
