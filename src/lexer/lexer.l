%{
#include "tokens.h"
%}

%option noyywrap
%option reentrant
%option prefix="zlang_"

DIGIT    [0-9]
LETTER   [a-zA-Z_]
IDENT    {LETTER}({LETTER}|{DIGIT})*

%%

"if"        { return TOKEN_IF; }
"else"      { return TOKEN_ELSE; }
"for"       { return TOKEN_FOR; }
"fun"       { return TOKEN_FUN; }
{IDENT}     { return TOKEN_IDENTIFIER; }
{DIGIT}+    { return TOKEN_NUMBER; }
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
[ \t]+      { /* skip spaces */ }
"??".*      { /* skip single-line comments */ }
\n          { return TOKEN_NEWLINE; }
;           { return TOKEN_SEMICOLON; }
%%
