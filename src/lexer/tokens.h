#ifndef TOKENS_H
#define TOKENS_H
typedef enum {
    TOKEN_IF = 256,
    TOKEN_ELSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_RETURN,
    TOKEN_IDENTIFIER,
    TOKEN_FLOAT,
    TOKEN_NUMBER,
    TOKEN_MULTIPLY,
    TOKEN_DIVIDE,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_ASSIGN,
    TOKEN_EQUAL,
    TOKEN_SEMICOLON,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_RSHIFT,
    TOKEN_AT,
    TOKEN_COMMA,
    TOKEN_COLON,
    TOKEN_STRING,
    TOKEN_CHAR,
    TOKEN_BRAINFUCK,
    TOKEN_LESS,
    TOKEN_GREATER,
    TOKEN_EQ_LESS,
    TOKEN_EQ_GREATER,
    TOKEN_NON_EQUAL,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_NOT,
    TOKEN_AMPERSAND,
    TOKEN_INCREMENT,
    TOKEN_DECREMENT,
    TOKEN_USE,
    TOKEN_ENUM,
    TOKEN_DOT,
    TOKEN_EOF = 0
} TokenType;

int zlang_lex_init(void **scanner);
int zlang_lex_destroy(void *scanner);
int zlang_lex(void *scanner);
char *zlang_get_text(void *scanner);

#endif
