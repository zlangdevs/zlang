#ifndef TOKENS_H
#define TOKENS_H
typedef enum {
    TOKEN_IF = 256,
    TOKEN_ELSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IDENTIFIER,
    TOKEN_NUMBER,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_ASSIGN,
    TOKEN_EQUAL,
    TOKEN_NEWLINE,
    TOKEN_SEMICOLON,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_RSHIFT,
    TOKEN_EOF = 0
} TokenType;

int zlang_lex_init(void **scanner);
int zlang_lex_destroy(void *scanner);
int zlang_lex(void *scanner);
//void zlang_set_in(FILE *file, void *scanner);
char *zlang_get_text(void *scanner);

#endif
