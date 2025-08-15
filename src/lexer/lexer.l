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
STRING   \"([^"\\]|\\.)*\"
FLOAT    {DIGIT}+\.{DIGIT}+

%%

"if"        { return TOKEN_IF; }
"else"      { return TOKEN_ELSE; }
"for"       { return TOKEN_FOR; }
"fun"       { return TOKEN_FUN; }
"return"    { return TOKEN_RETURN; }
"brainfuck"[ \t\n]*"{" { BEGIN(BRAINFUCK); yylval.string = strdup(""); }
{IDENT}     { yylval.string = strdup(yytext); return TOKEN_IDENTIFIER; }
{FLOAT}     { yylval.string = strdup(yytext); return TOKEN_FLOAT; }
{DIGIT}+    { yylval.string = strdup(yytext); return TOKEN_NUMBER; }
{STRING}    { yylval.string = strndup(yytext+1, yyleng-2); return TOKEN_STRING; }
"*"         { return TOKEN_MULTIPLY; }
"/"         { return TOKEN_DIVIDE; }
"+"         { return TOKEN_PLUS; }
"-"         { return TOKEN_MINUS; }
"="         { return TOKEN_ASSIGN; }
"=="        { return TOKEN_EQUAL; }
">>"        { return TOKEN_RSHIFT; }
"{"         { return TOKEN_LBRACE; }
"}"         { return TOKEN_RBRACE; }
"("         { return TOKEN_LPAREN; }
")"         { return TOKEN_RPAREN; }
"["         { return TOKEN_LBRACKET; }
"]"         { return TOKEN_RBRACKET; }
"@"         { return TOKEN_AT; }
","         { return TOKEN_COMMA; }
[ \t\n]+    { /* skip spaces */ }
"??".*      { /* skip single-line comments */ }
;           { return TOKEN_SEMICOLON; }
<BRAINFUCK>[^{}]+ { yylval.string = realloc(yylval.string, strlen(yylval.string) + yyleng + 1);
                    strcat(yylval.string, yytext); }
<BRAINFUCK>"{"    { yylval.string = realloc(yylval.string, strlen(yylval.string) + 2);
                    strcat(yylval.string, "{"); }
<BRAINFUCK>"}"    { BEGIN(INITIAL); return TOKEN_BRAINFUCK; }

%%