%{
#include "parser.h"
#include <stdlib.h>
#include <string.h>

extern YYSTYPE yylval;
%}

%option noyywrap
%option reentrant
%option prefix="zlang_"

DIGIT    [0-9]
LETTER   [a-zA-Z_]
IDENT    {LETTER}({LETTER}|{DIGIT})*
STRING   \"([^"\\]|\\.)*\"

%%

"if"        { return TOKEN_IF; }
"else"      { return TOKEN_ELSE; }
"for"       { return TOKEN_FOR; }
"fun"       { return TOKEN_FUN; }
"return"    { return TOKEN_RETURN; }
"void"      { return TOKEN_VOID; }
{IDENT}     { yylval.string = strdup(yytext); return TOKEN_IDENTIFIER; }
{DIGIT}+    { yylval.string = strdup(yytext); return TOKEN_NUMBER; }
{STRING}    { yylval.string = strndup(yytext+1, yyleng-2); return TOKEN_STRING; }
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
[ \t\n]+      { /* skip spaces */ }
"??".*      { /* skip single-line comments */ }
;           { return TOKEN_SEMICOLON; }
%%
