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
"&&"        { return TOKEN_AND; }
"||"        { return TOKEN_OR; }
"*"         { return TOKEN_MULTIPLY; }
"/"         { return TOKEN_DIVIDE; }
"++"        { return TOKEN_INCREMENT; }
"--"        { return TOKEN_DECREMENT; }
"+"         { return TOKEN_PLUS; }
"-"         { return TOKEN_MINUS; }
"<="        { return TOKEN_EQ_LESS; }
">="        { return TOKEN_EQ_GREATER; }
"!="        { return TOKEN_NON_EQUAL; }
"=="        { return TOKEN_EQUAL; }
"<"         { return TOKEN_LESS; }
">"         { return TOKEN_GREATER; }
"!"         { return TOKEN_NOT; }
"&"         { return TOKEN_AMPERSAND; }
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
