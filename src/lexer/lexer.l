%{
#include "parser.h"
#include <stdlib.h>
#include <string.h>
extern YYSTYPE yylval;

static int template_depth = 0;
static int last_was_identifier = 0;
static int last_was_rparen = 0;
%}

%option noyywrap
%option reentrant
%option prefix="zlang_"
%option yylineno

%x BRAINFUCK
%x COMMENT

DIGIT    [0-9]
LETTER   [a-zA-Z_]
IDENT    {LETTER}({LETTER}|{DIGIT})*
STRING   \"([^"\\]|\\.)*\"
CHAR     '([^'\\]|\\.)'
INTEGER  {DIGIT}+
HEX_LITERAL    0[xX][0-9a-fA-F]+(['][0-9a-fA-F]+)*
BINARY_LITERAL 0[bB][01]+(['][01]+)*
OCTAL_LITERAL  0[0-7]+(['][0-7]+)*
DELIMITED_INTEGER {DIGIT}+(['][0-9]+)+
DIGIT_WITH_DELIM {DIGIT}+(['][0-9]+)*
FLOAT_BASIC  {DIGIT_WITH_DELIM}\.{DIGIT_WITH_DELIM}
EXPONENT [eE][+-]?{DIGIT_WITH_DELIM}
FLOAT_EXP_INT  {DIGIT_WITH_DELIM}{EXPONENT}
FLOAT_EXP_DOT  {DIGIT_WITH_DELIM}\.{DIGIT_WITH_DELIM}{EXPONENT}
FLOAT_DOT  \.{DIGIT_WITH_DELIM}{EXPONENT}?

%%

"if"        { return TOKEN_IF; }
"else"      { return TOKEN_ELSE; }
"for"       { return TOKEN_FOR; }
"fun"       { return TOKEN_FUN; }
"return"    { return TOKEN_RETURN; }
"break"     { return TOKEN_BREAK; }
"continue"  { return TOKEN_CONTINUE; }
"use"       { return TOKEN_USE; }
"wrap"      { return TOKEN_WRAP; }
"enum"      { return TOKEN_ENUM; }
"struct"    { return TOKEN_STRUCT; }
"null"      { return TOKEN_NULL; }
"as"        { return TOKEN_AS; }
"_"         { return TOKEN_UNDERSCORE; }
"brainfuck"[ \t\n]*"{" { BEGIN(BRAINFUCK); yylval.string = strdup(""); }
{IDENT}     { yylval.string = strdup(yytext); last_was_identifier = 1; last_was_rparen = 0; return TOKEN_IDENTIFIER; }
{FLOAT_EXP_DOT}  { yylval.string = strdup(yytext); last_was_rparen = 0; return TOKEN_FLOAT; }
{FLOAT_EXP_INT}  { yylval.string = strdup(yytext); last_was_rparen = 0; return TOKEN_FLOAT; }
{FLOAT_DOT}      { yylval.string = strdup(yytext); last_was_rparen = 0; return TOKEN_FLOAT; }
{FLOAT_BASIC}    { yylval.string = strdup(yytext); last_was_rparen = 0; return TOKEN_FLOAT; }
{HEX_LITERAL}        { yylval.string = strdup(yytext); last_was_rparen = 0; return TOKEN_NUMBER; }
{BINARY_LITERAL}     { yylval.string = strdup(yytext); last_was_rparen = 0; return TOKEN_NUMBER; }
{OCTAL_LITERAL}      { yylval.string = strdup(yytext); last_was_rparen = 0; return TOKEN_NUMBER; }
{DELIMITED_INTEGER}  { yylval.string = strdup(yytext); last_was_rparen = 0; return TOKEN_NUMBER; }
{INTEGER}        { yylval.string = strdup(yytext); last_was_rparen = 0; return TOKEN_NUMBER; }
{STRING}    { yylval.string = strndup(yytext+1, yyleng-2); last_was_rparen = 0; return TOKEN_STRING; }
{CHAR}      {
    char *content = strndup(yytext+1, yyleng-2);
    int len = strlen(content);
    if (len == 0) {
        fprintf(stderr, "Lex error at line %d: Empty character literal\n", yylineno);
        free(content);
        return 0;
    }
    char actual_char;
    if (content[0] == '\\') {
        if (len != 2) {
            fprintf(stderr, "Lex error at line %d: Invalid escape sequence: '%s'\n", yylineno, content);
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
                fprintf(stderr, "Lex error at line %d: Unknown escape sequence: \\%c\n", yylineno, content[1]);
                free(content);
                return 0;
        }
    } else {
        if (len != 1) {
            fprintf(stderr, "Lex error at line %d: Character literal too long: '%s'\n", yylineno, content);
            free(content);
            return 0;
        }
        actual_char = content[0];
    }
    free(content);
    yylval.number = (int)actual_char;
    last_was_rparen = 0;
    return TOKEN_CHAR;
}
"&&"        { last_was_rparen = 0; return TOKEN_AND; }
"||"        { last_was_rparen = 0; return TOKEN_OR; }
"++"        { last_was_rparen = 0; return TOKEN_INCREMENT; }
"--"        { last_was_rparen = 0; return TOKEN_DECREMENT; }
"+="        { yylval.number = '+'; last_was_rparen = 0; return TOKEN_REASSIGN; }
"-="        { yylval.number = '-'; last_was_rparen = 0; return TOKEN_REASSIGN; }
"*="        { yylval.number = '*'; last_was_rparen = 0; return TOKEN_REASSIGN; }
"/="        { yylval.number = '/'; last_was_rparen = 0; return TOKEN_REASSIGN; }
"%="        { yylval.number = '%'; last_was_rparen = 0; return TOKEN_REASSIGN; }
">>="       { yylval.number = '>'; last_was_rparen = 0; return TOKEN_REASSIGN; }
"<<="       { yylval.number = '<'; last_was_rparen = 0; return TOKEN_REASSIGN; }
"&="        { yylval.number = 'A'; last_was_rparen = 0; return TOKEN_REASSIGN; }
"|="        { yylval.number = '$'; last_was_rparen = 0; return TOKEN_REASSIGN; }
"^="        { yylval.number = '^'; last_was_rparen = 0; return TOKEN_REASSIGN; }
"+"         { last_was_identifier = 0; last_was_rparen = 0; return TOKEN_PLUS; }
"-"         { last_was_identifier = 0; last_was_rparen = 0; return TOKEN_MINUS; }
"*"         { last_was_identifier = 0; last_was_rparen = 0; return TOKEN_MULTIPLY; }
"/"         { last_was_identifier = 0; last_was_rparen = 0; return TOKEN_DIVIDE; }
"%"         { last_was_rparen = 0; return TOKEN_MODULUS; }
"<="        { last_was_rparen = 0; return TOKEN_EQ_LESS; }
">="        { last_was_rparen = 0; return TOKEN_EQ_GREATER; }
"!="        { last_was_rparen = 0; return TOKEN_NON_EQUAL; }
"=="        { last_was_rparen = 0; return TOKEN_EQUAL; }
"<"         {
    last_was_identifier = 0;
    last_was_rparen = 0;
    template_depth++;
    return TOKEN_LESS;
}
">"         {
    last_was_identifier = 0;
    last_was_rparen = 0;
    if (template_depth > 0) template_depth--;
    return TOKEN_GREATER;
}
"..."                         { BEGIN(COMMENT); }
<COMMENT>[^.\n]+              { /* consume runs without dot/newline */ }
<COMMENT>\n                   { /* consume newline; */ }
<COMMENT>\.{1,2}              { /* consume 1–2 dots */ }
<COMMENT>"..."                { BEGIN(INITIAL); }
"!"    { last_was_rparen = 0; return TOKEN_NOT; }
"&"    { last_was_rparen = 0; return TOKEN_AMPERSAND; }
"."    { last_was_rparen = 0; return TOKEN_DOT; }
"="    { last_was_rparen = 0; return TOKEN_ASSIGN; }
">>"        {
    if (template_depth > 0) {
        /* Split >> into > and > for nested generics */
        template_depth--;
        last_was_rparen = 0;
        last_was_identifier = 0;
        yyless(1);
        return TOKEN_GREATER;
    }
    if (last_was_rparen || last_was_identifier) {
        last_was_rparen = 0;
        last_was_identifier = 0;
        return TOKEN_RSHIFT;
    }
    /* Default: split into two > tokens */
    last_was_rparen = 0;
    last_was_identifier = 0;
    yyless(1);
    return TOKEN_GREATER;
}
"<<"        { last_was_rparen = 0; return TOKEN_LSHIFT; }
"|"         { last_was_rparen = 0; return TOKEN_BIT_OR; }
"^"         { last_was_rparen = 0; return TOKEN_XOR; }
"~"         { last_was_rparen = 0; return TOKEN_BIT_NOT; }
"{"         { last_was_identifier = 0; last_was_rparen = 0; return TOKEN_LBRACE; }
"}"         { last_was_rparen = 0; return TOKEN_RBRACE; }
"("         { last_was_rparen = 0; return TOKEN_LPAREN; }
")"         { last_was_rparen = 1; return TOKEN_RPAREN; }
"["         { last_was_rparen = 0; return TOKEN_LBRACKET; }
"]"         { last_was_rparen = 0; return TOKEN_RBRACKET; }
"@"         { last_was_rparen = 0; return TOKEN_AT; }
","         { last_was_identifier = 0; last_was_rparen = 0; return TOKEN_COMMA; }
":"         { last_was_rparen = 0; return TOKEN_COLON; }
";"         { last_was_rparen = 0; return TOKEN_SEMICOLON; }
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
.           {
    fprintf(stderr, "Lex error at line %d: unexpected character '%s'\n", yylineno, yytext);
    return 0;
}
%%
