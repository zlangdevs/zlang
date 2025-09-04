%{
#include "parser.h"
#include <stdlib.h>
#include <string.h>
extern YYSTYPE yylval;
%}

%option noyywrap
%option reentrant
%option prefix="zlang_"

%x BRAINFUCK

DIGIT    [0-9]
LETTER   [a-zA-Z_]
IDENT    {LETTER}({LETTER}|{DIGIT})*
ARRAY_TYPE arr<[a-zA-Z_][a-zA-Z0-9_]*,[ \t]*[0-9]+>
PTR_TYPE ptr<[^>]*>
STRING   \"([^"\\]|\\.)*\"
CHAR     '([^'\\]|\\.)'
INTEGER  {DIGIT}+
FLOAT_BASIC  {DIGIT}+\.{DIGIT}+
EXPONENT [eE][+-]?{DIGIT}+
FLOAT_EXP_INT  {DIGIT}+{EXPONENT}
FLOAT_EXP_DOT  {DIGIT}+\.{DIGIT}+{EXPONENT}
FLOAT_DOT  \.{DIGIT}+{EXPONENT}?

%%

"if"        { return TOKEN_IF; }
"else"      { return TOKEN_ELSE; }
"for"       { return TOKEN_FOR; }
"fun"       { return TOKEN_FUN; }
"return"    { return TOKEN_RETURN; }
"break"     { return TOKEN_BREAK; }
"continue"  { return TOKEN_CONTINUE; }
"use"       { return TOKEN_USE; }
"enum"      { return TOKEN_ENUM; }
"struct"    { return TOKEN_STRUCT; }
"null"      { return TOKEN_NULL; }
"brainfuck"[ \t\n]*"{" { BEGIN(BRAINFUCK); yylval.string = strdup(""); }
{ARRAY_TYPE} { yylval.string = strdup(yytext); return TOKEN_IDENTIFIER; }
{PTR_TYPE} { yylval.string = strdup(yytext); return TOKEN_IDENTIFIER; }
{IDENT}     { yylval.string = strdup(yytext); return TOKEN_IDENTIFIER; }
{FLOAT_EXP_DOT}  { yylval.string = strdup(yytext); return TOKEN_FLOAT; }
{FLOAT_EXP_INT}  { yylval.string = strdup(yytext); return TOKEN_FLOAT; }
{FLOAT_DOT}      { yylval.string = strdup(yytext); return TOKEN_FLOAT; }
{FLOAT_BASIC}    { yylval.string = strdup(yytext); return TOKEN_FLOAT; }
{INTEGER}        { yylval.string = strdup(yytext); return TOKEN_NUMBER; }
{STRING}    { yylval.string = strndup(yytext+1, yyleng-2); return TOKEN_STRING; }
{CHAR}      {
    char *content = strndup(yytext+1, yyleng-2);
    int len = strlen(content);
    if (len == 0) {
        fprintf(stderr, "Error: Empty character literal\n");
        free(content);
        return 0;
    } else if (len > 1) {
        fprintf(stderr, "Error: Character literal too long: '%s'\n", content);
        free(content);
        return 0;
    }
    char actual_char;
    if (content[0] == '\\') {
        switch (content[1]) {
            case 'n': actual_char = '\n'; break;
            case 't': actual_char = '\t'; break;
            case 'r': actual_char = '\r'; break;
            case '\\': actual_char = '\\'; break;
            case '\'': actual_char = '\''; break;
            case '"': actual_char = '"'; break;
            default:
                fprintf(stderr, "Error: Unknown escape sequence: \\%c\n", content[1]);
                free(content);
                return 0;
        }
    } else {
        actual_char = content[0];
    }
    free(content);
    yylval.number = (int)actual_char;
    return TOKEN_CHAR;
}
"&&"        { return TOKEN_AND; }
"||"        { return TOKEN_OR; }
"++"        { return TOKEN_INCREMENT; }
"--"        { return TOKEN_DECREMENT; }
"+="        { return TOKEN_PLUS_ASSIGN; }
"-="        { return TOKEN_MINUS_ASSIGN; }
"*="        { return TOKEN_MULTIPLY_ASSIGN; }
"/="        { return TOKEN_DIVIDE_ASSIGN; }
"%="        { return TOKEN_MODULUS_ASSIGN; }
"+"         { return TOKEN_PLUS; }
"-"         { return TOKEN_MINUS; }
"*"         { return TOKEN_MULTIPLY; }
"/"         { return TOKEN_DIVIDE; }
"%"         { return TOKEN_MODULUS; }
"<="        { return TOKEN_EQ_LESS; }
">="        { return TOKEN_EQ_GREATER; }
"!="        { return TOKEN_NON_EQUAL; }
"=="        { return TOKEN_EQUAL; }
"<"         { return TOKEN_LESS; }
">"         { return TOKEN_GREATER; }
"!"         { return TOKEN_NOT; }
"&"         { return TOKEN_AMPERSAND; }
"."         { return TOKEN_DOT; }
"="         { return TOKEN_ASSIGN; }
">>"        { return TOKEN_RSHIFT; }
"{"         { return TOKEN_LBRACE; }
"}"         { return TOKEN_RBRACE; }
"("         { return TOKEN_LPAREN; }
")"         { return TOKEN_RPAREN; }
"["         { return TOKEN_LBRACKET; }
"]"         { return TOKEN_RBRACKET; }
"@"         { return TOKEN_AT; }
","         { return TOKEN_COMMA; }
":"         { return TOKEN_COLON; }
";"         { return TOKEN_SEMICOLON; }
[ \t\n]+    { /* skip spaces */ }
"??".*      { /* skip single-line comments */ }
<BRAINFUCK>[^{}]+ {
    yylval.string = realloc(yylval.string, strlen(yylval.string) + yyleng + 1);
    strcat(yylval.string, yytext);
}
<BRAINFUCK>"{"    {
    yylval.string = realloc(yylval.string, strlen(yylval.string) + 2);
    strcat(yylval.string, "{");
}
<BRAINFUCK>"}"    {
    BEGIN(INITIAL);
    return TOKEN_BRAINFUCK;
}
%%
