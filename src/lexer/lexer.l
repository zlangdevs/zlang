%{
#include "parser.h"
#include <stdlib.h>
#include <string.h>
extern YYSTYPE yylval;

static int template_depth = 0;
static int last_was_identifier = 0;
%}

%option noyywrap
%option reentrant
%option prefix="zlang_"

%x BRAINFUCK
%x COMMENT

DIGIT    [0-9]
LETTER   [a-zA-Z_]
IDENT    {LETTER}({LETTER}|{DIGIT})*
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
{IDENT}     { yylval.string = strdup(yytext); last_was_identifier = 1; return TOKEN_IDENTIFIER; }
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
    }
    char actual_char;
    if (content[0] == '\\') {
        if (len != 2) {
            fprintf(stderr, "Error: Invalid escape sequence: '%s'\n", content);
            free(content);
            return 0;
        }
        switch (content[1]) {
            case 'n': actual_char = '\n'; break;
            case 't': actual_char = '\t'; break;
            case 'r': actual_char = '\r'; break;
            case '\\': actual_char = '\\'; break;
            case '\'': actual_char = '\''; break;
            case '"': actual_char = '"'; break;
            case '0': actual_char = '\0'; break;
            default:
                fprintf(stderr, "Error: Unknown escape sequence: \\%c\n", content[1]);
                free(content);
                return 0;
        }
    } else {
        if (len != 1) {
            fprintf(stderr, "Error: Character literal too long: '%s'\n", content);
            free(content);
            return 0;
        }
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
"+"         { last_was_identifier = 0; return TOKEN_PLUS; }
"-"         { last_was_identifier = 0; return TOKEN_MINUS; }
"*"         { last_was_identifier = 0; return TOKEN_MULTIPLY; }
"/"         { last_was_identifier = 0; return TOKEN_DIVIDE; }
"%"         { return TOKEN_MODULUS; }
"<="        { return TOKEN_EQ_LESS; }
">="        { return TOKEN_EQ_GREATER; }
"!="        { return TOKEN_NON_EQUAL; }
"=="        { return TOKEN_EQUAL; }
"<"         {
    if (last_was_identifier) {
        template_depth++;
    }
    last_was_identifier = 0;
    return TOKEN_LESS;
}
">"         {
    if (template_depth > 0) {
        template_depth--;
    }
    last_was_identifier = 0;
    return TOKEN_GREATER;
}
"..."                         { BEGIN(COMMENT); }
<COMMENT>[^.\n]+              { /* consume runs without dot/newline */ }
<COMMENT>\n                   { /* consume newline; */ }
<COMMENT>\.{1,2}              { /* consume 1–2 dots */ }
<COMMENT>"..."                { BEGIN(INITIAL); }
"!"    { return TOKEN_NOT; }
"&"    { return TOKEN_AMPERSAND; }
"."    { return TOKEN_DOT; }
"="    { return TOKEN_ASSIGN; }
">>"        {
    if (template_depth > 0) {
        template_depth--;
        yyless(1);
        last_was_identifier = 0;
        return TOKEN_GREATER;
    }
    last_was_identifier = 0;
    return TOKEN_RSHIFT;
}
"{"         { last_was_identifier = 0; return TOKEN_LBRACE; }
"}"         { return TOKEN_RBRACE; }
"("         { return TOKEN_LPAREN; }
")"         { return TOKEN_RPAREN; }
"["         { return TOKEN_LBRACKET; }
"]"         { return TOKEN_RBRACKET; }
"@"         { return TOKEN_AT; }
","         { last_was_identifier = 0; return TOKEN_COMMA; }
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
