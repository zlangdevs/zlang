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
extern void* zig_create_if_stmt(void* condition, void* then_body, void* else_body);
extern void* zig_create_for_stmt(void* condition, void* body);
extern void* zig_create_c_for_stmt(void* init, void* condition, void* increment, void* body);
extern void* zig_create_break_stmt(void);
extern void* zig_create_continue_stmt(void);
extern void* zig_create_array_initializer(void* elements);
extern void* zig_create_array_index(const char* array_name, void* index);
extern void* zig_create_array_assignment(const char* array_name, void* index, void* value);
extern void* zig_create_c_function_decl(const char* name, const char* return_type, void* params);
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

%token TOKEN_FUN TOKEN_IF TOKEN_ELSE TOKEN_FOR TOKEN_RETURN TOKEN_VOID TOKEN_BREAK TOKEN_CONTINUE
%token TOKEN_ASSIGN TOKEN_EQUAL TOKEN_NON_EQUAL TOKEN_LESS TOKEN_GREATER TOKEN_EQ_LESS TOKEN_EQ_GREATER
%token TOKEN_LBRACE TOKEN_RBRACE TOKEN_LPAREN TOKEN_RPAREN
%token TOKEN_LBRACKET TOKEN_RBRACKET TOKEN_RSHIFT
%token TOKEN_COLON TOKEN_SEMICOLON TOKEN_AT TOKEN_COMMA TOKEN_PLUS TOKEN_MINUS TOKEN_MULTIPLY TOKEN_DIVIDE TOKEN_AND TOKEN_OR TOKEN_NOT TOKEN_AMPERSAND

%type <node> if_statement
%type <node> for_statement
%type <node> break_statement
%type <node> continue_statement

%left TOKEN_EQUAL TOKEN_NON_EQUAL
%left TOKEN_LESS TOKEN_GREATER TOKEN_EQ_LESS TOKEN_EQ_GREATER
%left TOKEN_OR
%left TOKEN_AND
%left TOKEN_PLUS TOKEN_MINUS
%left TOKEN_MULTIPLY TOKEN_DIVIDE
%right NOT UMINUS UPLUS
%right UDEREF

%type <node> parameter_list parameters parameter
%type <node> program function_list function statement_list statement
%type <node> var_declaration function_call return_statement assignment brainfuck_statement
%type <node> expression logical_or_expression logical_and_expression additive_expression multiplicative_expression unary_expression primary_expression argument_list arguments
%type <node> comparison_expression simple_term c_for_statement for_increment
%type <node> array_initializer array_assignment c_function_decl c_function_decl_statement
%type <string> type_name function_name string_literal complex_type_name

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
  | c_function_decl_statement {
        if (ast_root == NULL) {
            ast_root = zig_create_program();
        }
        zig_add_to_program(ast_root, $1);
    }
  | function_list c_function_decl_statement {
        zig_add_to_program(ast_root, $2);
    }
;

c_function_decl:
    TOKEN_FUN TOKEN_AT function_name TOKEN_LPAREN parameter_list TOKEN_RPAREN TOKEN_RSHIFT type_name {
        $$ = zig_create_c_function_decl($3, $8, $5);
        free($3);
        free($8);
    }
;

c_function_decl_statement:
    c_function_decl TOKEN_SEMICOLON { $$ = $1; }
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

complex_type_name:
    TOKEN_IDENTIFIER '<' TOKEN_IDENTIFIER ',' TOKEN_NUMBER '>' {
        // Allocate memory for the combined string "arr<type,size>"
        char* result = malloc(strlen($1) + strlen($3) + strlen($5) + 6);
        sprintf(result, "%s<%s, %s>", $1, $3, $5);
        $$ = result;
    }
  | TOKEN_IDENTIFIER '<' type_name '>' {
        // Allocate memory for the combined string "ptr<type>" where type can be nested
        char* result = malloc(strlen($1) + strlen($3) + 5);
        sprintf(result, "%s<%s>", $1, $3);
        free($3);
        $$ = result;
    }
;

type_name:
    complex_type_name { $$ = $1; }
  | TOKEN_IDENTIFIER { $$ = strdup($1); }
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
  | array_assignment TOKEN_SEMICOLON { $$ = $1; }
  | function_call TOKEN_SEMICOLON { $$ = $1; }
  | return_statement TOKEN_SEMICOLON { $$ = $1; }
  | brainfuck_statement TOKEN_SEMICOLON { $$ = $1; }
  | if_statement { $$ = $1; }
  | for_statement { $$ = $1; }
  | c_for_statement { $$ = $1; }
  | break_statement TOKEN_SEMICOLON { $$ = $1; }
  | continue_statement TOKEN_SEMICOLON { $$ = $1; }
  | c_function_decl TOKEN_SEMICOLON { $$ = $1; }
;

if_statement:
    TOKEN_IF expression TOKEN_LBRACE statement_list TOKEN_RBRACE {
        $$ = zig_create_if_stmt($2, $4, NULL);
    }
  | TOKEN_IF expression TOKEN_LBRACE statement_list TOKEN_RBRACE TOKEN_ELSE TOKEN_LBRACE statement_list TOKEN_RBRACE {
        $$ = zig_create_if_stmt($2, $4, $8);
    }
  | TOKEN_IF expression TOKEN_LBRACE statement_list TOKEN_RBRACE TOKEN_ELSE if_statement {
        // Create a statement list containing the if statement for the else body
        void* else_stmt_list = zig_create_stmt_list();
        zig_add_to_stmt_list(else_stmt_list, $7);
        $$ = zig_create_if_stmt($2, $4, else_stmt_list);
    }
;

for_statement:
    TOKEN_FOR TOKEN_LBRACE statement_list TOKEN_RBRACE {
        $$ = zig_create_for_stmt(NULL, $3);
    }
  | TOKEN_FOR comparison_expression TOKEN_LBRACE statement_list TOKEN_RBRACE {
        $$ = zig_create_for_stmt($2, $4);
    }
;

c_for_statement:
    TOKEN_FOR var_declaration TOKEN_SEMICOLON expression TOKEN_SEMICOLON for_increment TOKEN_LBRACE statement_list TOKEN_RBRACE {
        $$ = zig_create_c_for_stmt($2, $4, $6, $8);
    }
  | TOKEN_FOR assignment TOKEN_SEMICOLON expression TOKEN_SEMICOLON for_increment TOKEN_LBRACE statement_list TOKEN_RBRACE {
        $$ = zig_create_c_for_stmt($2, $4, $6, $8);
    }
;

for_increment:
    assignment { $$ = $1; }
  | /* empty */ { $$ = NULL; }
;

break_statement:
    TOKEN_BREAK {
        $$ = zig_create_break_stmt();
    }
;

continue_statement:
    TOKEN_CONTINUE {
        $$ = zig_create_continue_stmt();
    }
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

array_assignment:
    TOKEN_IDENTIFIER TOKEN_LBRACKET expression TOKEN_RBRACKET TOKEN_ASSIGN expression {
        $$ = zig_create_array_assignment($1, $3, $6);
    }
;

array_initializer:
    TOKEN_LBRACE argument_list TOKEN_RBRACE {
        $$ = zig_create_array_initializer($2);
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

comparison_expression:
    simple_term TOKEN_EQUAL simple_term { $$ = zig_create_comparison('=', $1, $3); }
  | simple_term TOKEN_NON_EQUAL simple_term { $$ = zig_create_comparison('!', $1, $3); }
  | simple_term TOKEN_LESS simple_term { $$ = zig_create_comparison('<', $1, $3); }
  | simple_term TOKEN_GREATER simple_term { $$ = zig_create_comparison('>', $1, $3); }
  | simple_term TOKEN_EQ_LESS simple_term { $$ = zig_create_comparison('L', $1, $3); }
  | simple_term TOKEN_EQ_GREATER simple_term { $$ = zig_create_comparison('G', $1, $3); }
;

simple_term:
    TOKEN_IDENTIFIER { $$ = zig_create_identifier($1); }
  | TOKEN_FLOAT { $$ = zig_create_float_literal($1); }
  | TOKEN_NUMBER { $$ = zig_create_number_literal($1); }
  | TOKEN_LPAREN simple_term TOKEN_RPAREN { $$ = $2; }
;

expression:
    logical_or_expression { $$ = $1; }
  | expression TOKEN_EQUAL logical_or_expression { $$ = zig_create_comparison('=', $1, $3); }
  | expression TOKEN_NON_EQUAL logical_or_expression { $$ = zig_create_comparison('!', $1, $3); }
  | expression TOKEN_LESS logical_or_expression { $$ = zig_create_comparison('<', $1, $3); }
  | expression TOKEN_GREATER logical_or_expression { $$ = zig_create_comparison('>', $1, $3); }
  | expression TOKEN_EQ_LESS logical_or_expression { $$ = zig_create_comparison('L', $1, $3); }
  | expression TOKEN_EQ_GREATER logical_or_expression { $$ = zig_create_comparison('G', $1, $3); }
;

logical_or_expression:
    logical_and_expression { $$ = $1; }
  | logical_or_expression TOKEN_OR logical_and_expression { $$ = zig_create_binary_op('|', $1, $3); }
;

logical_and_expression:
    additive_expression { $$ = $1; }
  | logical_and_expression TOKEN_AND additive_expression { $$ = zig_create_binary_op('&', $1, $3); }
;

additive_expression:
    multiplicative_expression { $$ = $1; }
  | additive_expression TOKEN_PLUS multiplicative_expression { $$ = zig_create_binary_op('+', $1, $3); }
  | additive_expression TOKEN_MINUS multiplicative_expression { $$ = zig_create_binary_op('-', $1, $3); }
;

multiplicative_expression:
    unary_expression { $$ = $1; }
  | multiplicative_expression TOKEN_MULTIPLY unary_expression { $$ = zig_create_binary_op('*', $1, $3); }
  | multiplicative_expression TOKEN_DIVIDE unary_expression { $$ = zig_create_binary_op('/', $1, $3); }
;

unary_expression:
    primary_expression { $$ = $1; }
  | TOKEN_NOT unary_expression { $$ = zig_create_unary_op('!', $2); }
  | TOKEN_MINUS unary_expression { $$ = zig_create_unary_op('-', $2); }
  | TOKEN_PLUS unary_expression { $$ = zig_create_unary_op('+', $2); }
  | TOKEN_AMPERSAND unary_expression { $$ = zig_create_unary_op('&', $2); }
  | TOKEN_MULTIPLY unary_expression { $$ = zig_create_unary_op('*', $2); }
;

primary_expression:
    TOKEN_IDENTIFIER { $$ = zig_create_identifier($1); }
  | TOKEN_IDENTIFIER TOKEN_LBRACKET expression TOKEN_RBRACKET { $$ = zig_create_array_index($1, $3); }
  | array_initializer { $$ = $1; }
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
