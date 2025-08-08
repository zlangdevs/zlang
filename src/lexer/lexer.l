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
{IDENT}     { return TOKEN_IDENTIFIER; }
{DIGIT}+    { return TOKEN_NUMBER; }
"+"         { return TOKEN_PLUS; }
"-"         { return TOKEN_MINUS; }
"="         { return TOKEN_ASSIGN; }
"=="        { return TOKEN_EQUAL; }
[ \t]+      { /* skip spaces */ }
\n          { return TOKEN_NEWLINE; }
;           { return TOKEN_SEMICOLON; }
%%
