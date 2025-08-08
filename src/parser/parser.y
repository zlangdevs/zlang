%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define YYDEBUG 1
int yydebug = 0;

// Forward declarations for Zig functions
extern void* zig_create_program(void);
extern void* zig_create_function(const char* name, const char* return_type, void* body);
extern void* zig_create_var_decl(const char* type_name, const char* name, void* initializer);
extern void* zig_create_function_call(const char* name, int is_libc, void* args);
extern void* zig_create_return_stmt(void* expression);
extern void* zig_create_identifier(const char* name);
extern void* zig_create_number_literal(const char* value);
extern void* zig_create_string_literal(const char* value);
extern void* zig_create_stmt_list(void);
extern void* zig_create_arg_list(void);
extern void zig_add_to_program(void* program, void* function);
extern void zig_add_to_stmt_list(void* list, void* stmt);
extern void zig_add_to_arg_list(void* list, void* arg);

void yyerror(const char* s);
int zlang_lex(void* scanner);
int yylex(void);

extern void* current_scanner;
void* ast_root = NULL;

%}

%union {
    char* string;
    void* node;
}

%token <string> TOKEN_IDENTIFIER TOKEN_NUMBER TOKEN_STRING
%token TOKEN_FUN TOKEN_IF TOKEN_ELSE TOKEN_FOR TOKEN_RETURN TOKEN_VOID
%token TOKEN_PLUS TOKEN_MINUS TOKEN_ASSIGN TOKEN_EQUAL
%token TOKEN_LBRACE TOKEN_RBRACE TOKEN_LPAREN TOKEN_RPAREN
%token TOKEN_LBRACKET TOKEN_RBRACKET TOKEN_RSHIFT
%token TOKEN_SEMICOLON TOKEN_NEWLINE TOKEN_AT TOKEN_COMMA

%type <node> program function statement_list statement
%type <node> var_declaration function_call return_statement
%type <node> expression argument_list arguments
%type <string> type_name function_name string_literal

%start program

%%

program:
    optional_newlines function_list {
        $$ = ast_root;
    }
    | optional_newlines function_list TOKEN_NEWLINE {
        $$ = ast_root;
    }
;

optional_newlines:
    /* empty */
    | optional_newlines TOKEN_NEWLINE
;

function_list:
    function {
        if (ast_root == NULL) {
            ast_root = zig_create_program();
        }
        zig_add_to_program(ast_root, $1);
    }
    | function_list optional_newlines function {
        zig_add_to_program(ast_root, $3);
    }
;

function:
    TOKEN_FUN function_name TOKEN_LPAREN TOKEN_RPAREN TOKEN_RSHIFT type_name TOKEN_LBRACE statement_list TOKEN_RBRACE {
        $$ = zig_create_function($2, $6, $8);
        free($2);
        free($6);
    }
;

function_name:
    TOKEN_IDENTIFIER {
        $$ = strdup($1);
    }
;

type_name:
    TOKEN_IDENTIFIER {
        $$ = strdup($1);
    }
    | TOKEN_VOID {
        $$ = strdup("void");
    }
;

statement_list:
    /* empty */ {
        $$ = zig_create_stmt_list();
    }
    | statement_list statement {
        if ($2 != NULL) {
            zig_add_to_stmt_list($1, $2);
        }
        $$ = $1;
    }
    | statement_list TOKEN_NEWLINE {
        $$ = $1;  // Ignore newlines
    }
;

statement:
    var_declaration TOKEN_SEMICOLON {
        $$ = $1;
    }
    | function_call TOKEN_SEMICOLON {
        $$ = $1;
    }
    | return_statement TOKEN_SEMICOLON {
        $$ = $1;
    }
;

var_declaration:
    type_name TOKEN_IDENTIFIER TOKEN_ASSIGN expression {
        void* initializer = $4;
        $$ = zig_create_var_decl($1, $2, initializer);
        free($1);
    }
    | type_name TOKEN_IDENTIFIER {
        $$ = zig_create_var_decl($1, $2, NULL);
        free($1);
    }
;

function_call:
    TOKEN_AT TOKEN_IDENTIFIER TOKEN_LPAREN argument_list TOKEN_RPAREN {
        $$ = zig_create_function_call($2, 1, $4);
    }
    | TOKEN_IDENTIFIER TOKEN_LPAREN argument_list TOKEN_RPAREN {
        $$ = zig_create_function_call($1, 0, $3);
    }
;

return_statement:
    TOKEN_RETURN expression {
        $$ = zig_create_return_stmt($2);
    }
    | TOKEN_RETURN {
        $$ = zig_create_return_stmt(NULL);
    }
;

argument_list:
    /* empty */ {
        $$ = zig_create_arg_list();
    }
    | arguments {
        $$ = $1;
    }
;

arguments:
    expression {
        void* list = zig_create_arg_list();
        zig_add_to_arg_list(list, $1);
        $$ = list;
    }
    | arguments TOKEN_COMMA expression {
        zig_add_to_arg_list($1, $3);
        $$ = $1;
    }
;

expression:
    TOKEN_IDENTIFIER {
        $$ = zig_create_identifier($1);
    }
    | TOKEN_NUMBER {
        $$ = zig_create_number_literal($1);
    }
    | string_literal {
        $$ = zig_create_string_literal($1);
        free($1);
    }
;

string_literal:
    TOKEN_STRING {
        $$ = strdup($1);
    }
;

%%

int yylex(void) {
    return zlang_lex(current_scanner);
}

void yyerror(const char* s) {
    fprintf(stderr, "Parse error: %s\n", s);
}
