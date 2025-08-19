%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define YYDEBUG 1
int yydebug = 0;

/* Forward declarations for Zig functions (implemented on Zig side) */
extern void* zig_create_program(void);
extern void* zig_create_parameter(const char* name, const char* type_name);
extern void* zig_create_param_list(void);
extern void zig_add_to_param_list(void* list, void* param);
extern void* zig_create_function(const char* name, const char* return_type, void* params, void* body);
extern void* zig_create_var_decl(const char* type_name, const char* name, void* initializer);
extern void* zig_create_function_call(const char* name, int is_libc, void* args);
extern void* zig_create_comparison(char op, void* lhs, void* rhs);
extern void* zig_create_return_stmt(void* expression);
extern void* zig_create_binary_op(char op, void* lhs, void* rhs);
extern void* zig_create_unary_op(char op, void* operand);
extern void* zig_create_assignment(const char* name, void* value);
extern void* zig_create_identifier(const char* name);
extern void* zig_create_float_literal(const char* value);
extern void* zig_create_number_literal(const char* value);
extern void* zig_create_string_literal(const char* value);
extern void* zig_create_bool_literal(int value);
extern void* zig_create_stmt_list(void);
extern void* zig_create_arg_list(void);
extern void* zig_create_brainfuck(const char* code);
extern void zig_add_to_program(void* program, void* function);
extern void zig_add_to_stmt_list(void* list, void* stmt);
extern void zig_add_to_arg_list(void* list, void* arg);

void yyerror(const char* s);
int zlang_lex(void* scanner);
int yylex(void);

extern void* current_scanner;
void* ast_root = NULL;
%}

%define parse.error verbose

%union {
    char* string;
    void* node;
}

%token <string> TOKEN_IDENTIFIER TOKEN_FLOAT TOKEN_NUMBER TOKEN_STRING TOKEN_BRAINFUCK

%token TOKEN_FUN TOKEN_IF TOKEN_ELSE TOKEN_FOR TOKEN_RETURN TOKEN_VOID
%token TOKEN_ASSIGN TOKEN_EQUAL TOKEN_NON_EQUAL TOKEN_LESS TOKEN_GREATER TOKEN_EQ_LESS TOKEN_EQ_GREATER
%token TOKEN_LBRACE TOKEN_RBRACE TOKEN_LPAREN TOKEN_RPAREN
%token TOKEN_LBRACKET TOKEN_RBRACKET TOKEN_RSHIFT
%token TOKEN_COLON TOKEN_SEMICOLON TOKEN_AT TOKEN_COMMA TOKEN_PLUS TOKEN_MINUS TOKEN_MULTIPLY TOKEN_DIVIDE TOKEN_AND TOKEN_OR TOKEN_NOT

%left TOKEN_EQUAL TOKEN_NON_EQUAL
%left TOKEN_LESS TOKEN_GREATER TOKEN_EQ_LESS TOKEN_EQ_GREATER
%left TOKEN_PLUS TOKEN_MINUS
%left TOKEN_MULTIPLY TOKEN_DIVIDE TOKEN_AND TOKEN_OR
%right NOT UMINUS UPLUS

%type <node> parameter_list parameters parameter
%type <node> program function_list function statement_list statement
%type <node> var_declaration function_call return_statement assignment brainfuck_statement
%type <node> expression term factor argument_list arguments
%type <string> type_name function_name string_literal

%start program

%%

program:
    /* empty */ {
        ast_root = zig_create_program();
        $$ = ast_root;
    }
  | function_list {
        $$ = ast_root;
    }
;

function_list:
    function {
        if (ast_root == NULL) {
            ast_root = zig_create_program();
        }
        zig_add_to_program(ast_root, $1);
    }
  | function_list function {
        zig_add_to_program(ast_root, $2);
    }
;

function:
    TOKEN_FUN function_name TOKEN_LPAREN parameter_list TOKEN_RPAREN TOKEN_RSHIFT type_name TOKEN_LBRACE statement_list TOKEN_RBRACE {
        $$ = zig_create_function($2, $7, $4, $9);
        free($2);
        free($7);
    }
;

parameter_list:
    /* empty */ { $$ = zig_create_param_list(); }
  | parameters { $$ = $1; }
;

parameters:
    parameter {
        void* list = zig_create_param_list();
        zig_add_to_param_list(list, $1);
        $$ = list;
    }
  | parameters TOKEN_COMMA parameter {
        zig_add_to_param_list($1, $3);
        $$ = $1;
    }
;

parameter:
    TOKEN_IDENTIFIER TOKEN_COLON type_name {
        $$ = zig_create_parameter($1, $3);
        free($3);
    }
;

function_name:
    TOKEN_IDENTIFIER { $$ = strdup($1); }
;

type_name:
    TOKEN_IDENTIFIER { $$ = strdup($1); }
;

statement_list:
    /* empty */ { $$ = zig_create_stmt_list(); }
  | statement_list statement {
        if ($2 != NULL) zig_add_to_stmt_list($1, $2);
        $$ = $1;
    }
;

statement:
    var_declaration TOKEN_SEMICOLON { $$ = $1; }
  | assignment TOKEN_SEMICOLON { $$ = $1; }
  | function_call TOKEN_SEMICOLON { $$ = $1; }
  | return_statement TOKEN_SEMICOLON { $$ = $1; }
  | brainfuck_statement TOKEN_SEMICOLON { $$ = $1; }
;

brainfuck_statement:
    TOKEN_BRAINFUCK {
        $$ = zig_create_brainfuck($1);
        free($1);
    }
;

assignment:
    TOKEN_IDENTIFIER TOKEN_ASSIGN expression {
        $$ = zig_create_assignment($1, $3);
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
    TOKEN_RETURN expression { $$ = zig_create_return_stmt($2); }
  | TOKEN_RETURN { $$ = zig_create_return_stmt(NULL); }
;

argument_list:
    /* empty */ { $$ = zig_create_arg_list(); }
  | arguments { $$ = $1; }
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
    term { $$ = $1; }
  | expression TOKEN_PLUS term { $$ = zig_create_binary_op('+', $1, $3); }
  | expression TOKEN_MINUS term { $$ = zig_create_binary_op('-', $1, $3); }
  | TOKEN_NOT expression %prec NOT { $$ = zig_create_unary_op('!', $2); }
  | TOKEN_MINUS expression %prec UMINUS { $$ = zig_create_unary_op('-', $2); }
  | TOKEN_PLUS expression %prec UPLUS { $$ = zig_create_unary_op('+', $2); }
  | expression TOKEN_EQUAL expression { $$ = zig_create_comparison('=', $1, $3); }
  | expression TOKEN_NON_EQUAL expression { $$ = zig_create_comparison('!', $1, $3); }
  | expression TOKEN_LESS expression { $$ = zig_create_comparison('<', $1, $3); }
  | expression TOKEN_GREATER expression { $$ = zig_create_comparison('>', $1, $3); }
  | expression TOKEN_EQ_LESS expression { $$ = zig_create_comparison('L', $1, $3); }
  | expression TOKEN_EQ_GREATER expression { $$ = zig_create_comparison('G', $1, $3); }
;

term:
    factor { $$ = $1; }
  | term TOKEN_AND factor { $$ = zig_create_binary_op('&', $1, $3); }
  | term TOKEN_OR factor { $$ = zig_create_binary_op('|', $1, $3); }
  | term TOKEN_MULTIPLY factor { $$ = zig_create_binary_op('*', $1, $3); }
  | term TOKEN_DIVIDE factor { $$ = zig_create_binary_op('/', $1, $3); }
;

factor:
    TOKEN_IDENTIFIER { $$ = zig_create_identifier($1); }
  | TOKEN_FLOAT { $$ = zig_create_float_literal($1); }
  | TOKEN_NUMBER { $$ = zig_create_number_literal($1); }
  | string_literal { $$ = zig_create_string_literal($1); free($1); }
  | function_call { $$ = $1; }
  | TOKEN_LPAREN expression TOKEN_RPAREN { $$ = $2; }
;

string_literal:
    TOKEN_STRING { $$ = strdup($1); }
;

%%

int yylex(void) {
    return zlang_lex(current_scanner);
}

void yyerror(const char* s) {
    fprintf(stderr, "Parse error: %s\n", s);
}