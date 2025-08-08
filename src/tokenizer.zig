const std = @import("std");
const errors = @import("errors.zig");
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
    fun_token = 259,
    return_token = 260,
    void_token = 261,
    identifier = 262,
    number = 263,
    plus = 264,
    minus = 265,
    assign = 266,
    equal = 267,
    newline = 268,
    semicolon = 269,
    lbrace = 270,
    rbrace = 271,
    lparen = 272,
    rparen = 273,
    lbracket = 274,
    rbracket = 275,
    rshift = 276,
    at = 277,
    comma = 278,
    string = 279,

    pub fn toString(self: TokenType) []const u8 {
        return switch (self) {
            .eof => "EOF",
            .if_token => "IF",
            .else_token => "ELSE",
            .for_token => "FOR",
            .fun_token => "FUN",
            .return_token => "RETURN",
            .void_token => "VOID",
            .identifier => "IDENTIFIER",
            .number => "NUMBER",
            .plus => "PLUS",
            .minus => "MINUS",
            .assign => "ASSIGN",
            .equal => "EQUAL",
            .newline => "NEWLINE",
            .semicolon => "SEMICOLON",
            .lbrace => "LBRACE",
            .rbrace => "RBRACE",
            .lparen => "LPAREN",
            .rparen => "RPAREN",
            .lbracket => "LBRACKET",
            .rbracket => "RBRACKET",
            .rshift => "RSHIFT",
            .at => "AT",
            .comma => "COMMA",
            .string => "STRING",
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

pub fn tokenize(allocator: std.mem.Allocator, input: []const u8) errors.TokenizeError!std.ArrayList(Token) {
    var tokens = std.ArrayList(Token).init(allocator);
    errdefer tokens.deinit();
    var scanner: ?*anyopaque = null;
    if (zlang_lex_init(&scanner) != 0) {
        return errors.TokenizeError.LexerInitFailed;
    }
    defer _ = zlang_lex_destroy(scanner);
    const null_terminated_input = try allocator.dupeZ(u8, input);
    defer allocator.free(null_terminated_input);
    const file = fmemopen(null_terminated_input.ptr, input.len, "r");
    if (file == null) {
        return errors.TokenizeError.FileOpenFailed;
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
