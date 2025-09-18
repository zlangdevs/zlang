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
extern void* zig_create_method_call(void* object, const char* method_name, void* args);
extern void* zig_create_comparison(char op, void* lhs, void* rhs);
extern void* zig_create_return_stmt(void* expression);
extern void* zig_create_binary_op(char op, void* lhs, void* rhs);
extern void* zig_create_unary_op(char op, void* operand);
extern void* zig_create_assignment(void* target, void* value);
extern void* zig_create_identifier(const char* name);
extern void* zig_create_float_literal(const char* value);
extern void* zig_create_number_literal(const char* value);
extern void* zig_create_string_literal(const char* value);
extern void* zig_create_char_literal(int value);
extern void* zig_create_bool_literal(int value);
extern void* zig_create_null_literal(void);
extern void* zig_create_stmt_list(void);
extern void* zig_create_arg_list(void);
extern void* zig_create_brainfuck(const char* code);
extern void* zig_create_if_stmt(void* condition, void* then_body, void* else_body);
extern void* zig_create_for_stmt(void* condition, void* body);
extern void* zig_create_c_for_stmt(void* init, void* condition, void* increment, void* body);
extern void* zig_create_break_stmt(void);
extern void* zig_create_continue_stmt(void);
extern void* zig_create_array_initializer(void* elements);
extern void* zig_create_array_index(void* array, void* index);
extern void* zig_create_array_assignment(void* array, void* index, void* value);
extern void* zig_create_array_compound_assignment(void* array, void* index, void* value, int op);
extern void* zig_create_c_function_decl(const char* name, const char* return_type, void* params);
extern void* zig_create_wrapper_function(const char* name, const char* return_type, void* params);
extern void* zig_create_use_stmt(const char* module_path);
extern void* zig_create_enum_decl(const char* name, void* values);
extern void* zig_create_struct_decl(const char* name, void* fields);
extern void* zig_create_qualified_identifier(void* base, const char* field);
extern void* zig_create_enum_value_list(void);
extern void zig_add_enum_value(void* list, const char* name, void* value);
extern void* zig_create_struct_field_list(void);
extern void zig_add_struct_field(void* list, const char* name, const char* type_name);
extern void zig_add_struct_field_with_default(void* list, const char* name, const char* type_name, void* default_value);
extern void* zig_create_struct_initializer(const char* struct_name, void* field_values);
extern void* zig_create_struct_field_value_list(void);
extern void zig_add_struct_field_value(void* list, const char* field_name, void* value);
extern void zig_add_to_program(void* program, void* function);
extern void zig_add_global_to_program(void* program, void* global);
extern void zig_add_to_stmt_list(void* list, void* stmt);
extern void zig_add_to_arg_list(void* list, void* arg);

void yyerror(const char* s);
int zlang_lex(void* scanner);
int yylex(void);

extern int zlang_get_lineno(void* scanner);

extern void* current_scanner;
void* ast_root = NULL;
%}

%expect 7
%expect-rr 0

%define parse.error verbose

%glr-parser

%union {
    char* string;
    void* node;
    int number;
}

%token <string> TOKEN_IDENTIFIER TOKEN_FLOAT TOKEN_NUMBER TOKEN_STRING TOKEN_BRAINFUCK
%token <number> TOKEN_CHAR
%token <number> TOKEN_REASSIGN

%token TOKEN_FUN TOKEN_IF TOKEN_ELSE TOKEN_FOR TOKEN_RETURN TOKEN_VOID TOKEN_BREAK TOKEN_CONTINUE TOKEN_USE TOKEN_WRAP TOKEN_ENUM TOKEN_STRUCT TOKEN_DOT TOKEN_NULL
%token TOKEN_ASSIGN TOKEN_EQUAL TOKEN_NON_EQUAL TOKEN_LESS TOKEN_GREATER TOKEN_EQ_LESS TOKEN_EQ_GREATER
%token TOKEN_LBRACE TOKEN_RBRACE TOKEN_LPAREN TOKEN_RPAREN
%token TOKEN_LBRACKET TOKEN_RBRACKET TOKEN_RSHIFT TOKEN_DECREMENT TOKEN_INCREMENT
%token TOKEN_COLON TOKEN_SEMICOLON TOKEN_AT TOKEN_COMMA TOKEN_PLUS TOKEN_MINUS TOKEN_MULTIPLY TOKEN_DIVIDE TOKEN_MODULUS TOKEN_AND TOKEN_OR TOKEN_NOT TOKEN_AMPERSAND

%type <node> if_statement
%type <node> for_statement
%type <node> break_statement
%type <node> continue_statement

%left TOKEN_OR
%left TOKEN_AND
%left TOKEN_EQUAL TOKEN_NON_EQUAL
%left TOKEN_LESS TOKEN_GREATER TOKEN_EQ_LESS TOKEN_EQ_GREATER
%left TOKEN_PLUS TOKEN_MINUS
%left TOKEN_MULTIPLY TOKEN_DIVIDE
%right TOKEN_ASSIGN TOKEN_REASSIGN
%left TOKEN_INCREMENT TOKEN_DECREMENT
%left TOKEN_LBRACKET TOKEN_LPAREN TOKEN_DOT
%left TOKEN_AT
%right NOT UMINUS UPLUS UAMPERSAND UDEREF

%type <node> parameter_list parameters parameter
%type <node> program function_list function statement_list statement
%type <node> var_declaration global_variable_declaration function_call return_statement assignment brainfuck_statement
%type <node> expression logical_or_expression logical_and_expression equality_expression relational_expression additive_expression multiplicative_expression unary_expression primary_expression postfix_expression argument_list arguments
%type <node> comparison_expression simple_term c_for_statement for_increment
%type <node> array_initializer array_assignment c_function_decl c_function_decl_statement use_statement enum_declaration struct_declaration wrap_statement
%type <node> enum_values enum_value_list struct_fields struct_field_list qualified_identifier
%type <node> struct_initializer struct_field_values struct_field_value_list initializer_expression
%type <string> type_name function_name string_literal complex_type_name module_path

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
   | global_variable_declaration {
        if (ast_root == NULL) {
            ast_root = zig_create_program();
        }
        zig_add_global_to_program(ast_root, $1);
    }
   | function_list global_variable_declaration {
        zig_add_global_to_program(ast_root, $2);
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
   | use_statement {
        if (ast_root == NULL) {
            ast_root = zig_create_program();
        }
        zig_add_to_program(ast_root, $1);
    }
   | function_list use_statement {
       zig_add_to_program(ast_root, $2);
   }
   | enum_declaration {
       if (ast_root == NULL) {
           ast_root = zig_create_program();
       }
       zig_add_to_program(ast_root, $1);
   }
   | function_list enum_declaration {
       zig_add_to_program(ast_root, $2);
   }
   | struct_declaration {
       if (ast_root == NULL) {
           ast_root = zig_create_program();
       }
       zig_add_to_program(ast_root, $1);
   }
   | function_list struct_declaration {
       zig_add_to_program(ast_root, $2);
   }
   | wrap_statement {
        if (ast_root == NULL) {
            ast_root = zig_create_program();
        }
        zig_add_to_program(ast_root, $1);
   }
   | function_list wrap_statement {
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

wrap_statement:
    TOKEN_WRAP TOKEN_AT function_name TOKEN_LPAREN parameter_list TOKEN_RPAREN TOKEN_RSHIFT type_name TOKEN_SEMICOLON {
        $$ = zig_create_wrapper_function($3, $8, $5);
        free($3);
        free($8);
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

complex_type_name:
    TOKEN_IDENTIFIER TOKEN_LESS type_name TOKEN_COMMA TOKEN_NUMBER TOKEN_GREATER %prec TOKEN_LESS {
        char* result = malloc(strlen($1) + strlen($3) + strlen($5) + 6);
        sprintf(result, "%s<%s, %s>", $1, $3, $5);
        free($3);
        $$ = result;
    }
  | TOKEN_IDENTIFIER TOKEN_LESS type_name TOKEN_GREATER %prec TOKEN_LESS {
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
   | return_statement TOKEN_SEMICOLON { $$ = $1; }
   | brainfuck_statement TOKEN_SEMICOLON { $$ = $1; }
   | if_statement { $$ = $1; }
   | for_statement { $$ = $1; }
   | c_for_statement { $$ = $1; }
   | break_statement TOKEN_SEMICOLON { $$ = $1; }
   | continue_statement TOKEN_SEMICOLON { $$ = $1; }
   | c_function_decl TOKEN_SEMICOLON { $$ = $1; }
   | use_statement { $$ = $1; }
   | expression TOKEN_SEMICOLON { $$ = $1; }
;

if_statement:
    TOKEN_IF expression TOKEN_LBRACE statement_list TOKEN_RBRACE {
        $$ = zig_create_if_stmt($2, $4, NULL);
    }
  | TOKEN_IF expression TOKEN_LBRACE statement_list TOKEN_RBRACE TOKEN_ELSE TOKEN_LBRACE statement_list TOKEN_RBRACE {
        $$ = zig_create_if_stmt($2, $4, $8);
    }
  | TOKEN_IF expression TOKEN_LBRACE statement_list TOKEN_RBRACE TOKEN_ELSE if_statement {
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
  | unary_expression { $$ = $1; }
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
    TOKEN_IDENTIFIER TOKEN_ASSIGN initializer_expression {
        void* target = zig_create_identifier($1);
        $$ = zig_create_assignment(target, $3);
    }
    | qualified_identifier TOKEN_ASSIGN initializer_expression {
        $$ = zig_create_assignment($1, $3);
    }
    | TOKEN_IDENTIFIER TOKEN_REASSIGN expression {
        void* target = zig_create_identifier($1);
        void* read_id = zig_create_identifier($1);
        $$ = zig_create_assignment(target, zig_create_binary_op((char)$2, read_id, $3));
    }
;

array_assignment:
    postfix_expression TOKEN_LBRACKET expression TOKEN_RBRACKET TOKEN_ASSIGN expression {
        $$ = zig_create_array_assignment($1, $3, $6);
    }
  | postfix_expression TOKEN_LBRACKET expression TOKEN_RBRACKET TOKEN_REASSIGN expression {
        $$ = zig_create_array_compound_assignment($1, $3, $6, $5);
    }
;

array_initializer:
    TOKEN_LBRACE argument_list TOKEN_RBRACE {
        $$ = zig_create_array_initializer($2);
    }
;

var_declaration:
    type_name TOKEN_IDENTIFIER TOKEN_ASSIGN initializer_expression {
        void* initializer = $4;
        $$ = zig_create_var_decl($1, $2, initializer);
        free($1);
    }
    | type_name TOKEN_IDENTIFIER {
        $$ = zig_create_var_decl($1, $2, NULL);
        free($1);
    }
;

global_variable_declaration:
    type_name TOKEN_IDENTIFIER TOKEN_ASSIGN initializer_expression TOKEN_SEMICOLON {
        $$ = zig_create_var_decl($1, $2, $4);
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
    TOKEN_RETURN initializer_expression { $$ = zig_create_return_stmt($2); }
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
;

logical_or_expression:
    logical_and_expression { $$ = $1; }
  | logical_or_expression TOKEN_OR logical_and_expression { $$ = zig_create_binary_op('|', $1, $3); }
;

logical_and_expression:
    equality_expression { $$ = $1; }
  | logical_and_expression TOKEN_AND equality_expression { $$ = zig_create_binary_op('&', $1, $3); }
;

equality_expression:
    relational_expression { $$ = $1; }
  | equality_expression TOKEN_EQUAL relational_expression { $$ = zig_create_comparison('=', $1, $3); }
  | equality_expression TOKEN_NON_EQUAL relational_expression { $$ = zig_create_comparison('!', $1, $3); }
;

relational_expression:
    additive_expression { $$ = $1; }
  | relational_expression TOKEN_LESS additive_expression { $$ = zig_create_comparison('<', $1, $3); }
  | relational_expression TOKEN_GREATER additive_expression { $$ = zig_create_comparison('>', $1, $3); }
  | relational_expression TOKEN_EQ_LESS additive_expression { $$ = zig_create_comparison('L', $1, $3); }
  | relational_expression TOKEN_EQ_GREATER additive_expression { $$ = zig_create_comparison('G', $1, $3); }
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
  | multiplicative_expression TOKEN_MODULUS unary_expression { $$ = zig_create_binary_op('%', $1, $3); }
;

unary_expression:
    postfix_expression { $$ = $1; }
  | TOKEN_NOT unary_expression %prec NOT { $$ = zig_create_unary_op('!', $2); }
  | TOKEN_MINUS unary_expression %prec UMINUS { $$ = zig_create_unary_op('-', $2); }
  | TOKEN_PLUS unary_expression %prec UPLUS { $$ = zig_create_unary_op('+', $2); }
  | TOKEN_AMPERSAND unary_expression %prec UAMPERSAND { $$ = zig_create_unary_op('&', $2); }
  | TOKEN_MULTIPLY unary_expression %prec UDEREF { $$ = zig_create_unary_op('*', $2); }
;

postfix_expression:
    primary_expression { $$ = $1; }
  | postfix_expression TOKEN_LBRACKET expression TOKEN_RBRACKET {
       $$ = zig_create_array_index($1, $3);
   }
  | postfix_expression TOKEN_DOT TOKEN_IDENTIFIER {
       const char* field_copy = strdup($3);
       $$ = zig_create_qualified_identifier($1, field_copy);
       free($3);
   }
  | postfix_expression TOKEN_DOT TOKEN_IDENTIFIER TOKEN_LPAREN argument_list TOKEN_RPAREN {
       const char* method_name_copy = strdup($3);
       $$ = zig_create_method_call($1, method_name_copy, $5);
       free($3);
   }
  | postfix_expression TOKEN_INCREMENT { $$ = zig_create_unary_op('I', $1); }
  | postfix_expression TOKEN_DECREMENT { $$ = zig_create_unary_op('D', $1); }
;

primary_expression:
     TOKEN_IDENTIFIER { $$ = zig_create_identifier($1); }
    | array_initializer { $$ = $1; }
    | TOKEN_FLOAT { $$ = zig_create_float_literal($1); }
    | TOKEN_NUMBER { $$ = zig_create_number_literal($1); }
    | TOKEN_CHAR { $$ = zig_create_char_literal($1); }
    | string_literal { $$ = zig_create_string_literal($1); free($1); }
    | function_call { $$ = $1; }
    | TOKEN_LPAREN expression TOKEN_RPAREN { $$ = $2; }
    | TOKEN_NULL { $$ = zig_create_null_literal(); }
;

qualified_identifier:
    TOKEN_IDENTIFIER TOKEN_DOT TOKEN_IDENTIFIER %prec TOKEN_ASSIGN {
        void* base_node = zig_create_identifier($1);
        const char* field_copy = strdup($3);
        $$ = zig_create_qualified_identifier(base_node, field_copy);
        free($1); free($3);
    }
    | TOKEN_IDENTIFIER TOKEN_LBRACKET expression TOKEN_RBRACKET TOKEN_DOT TOKEN_IDENTIFIER %prec TOKEN_ASSIGN {
        void* arr = zig_create_identifier($1);
        void* array_index = zig_create_array_index(arr, $3);
        const char* field_copy = strdup($6);
        $$ = zig_create_qualified_identifier(array_index, field_copy);
        free($6);
    }
    | qualified_identifier TOKEN_DOT TOKEN_IDENTIFIER %prec TOKEN_ASSIGN {
        const char* field_copy = strdup($3);
        $$ = zig_create_qualified_identifier($1, field_copy);
        free($3);
    }
;

string_literal:
    TOKEN_STRING { $$ = strdup($1); }
;

use_statement:
    TOKEN_USE module_path {
        $$ = zig_create_use_stmt($2);
        free($2);
    }
;

enum_declaration:
    TOKEN_ENUM TOKEN_IDENTIFIER TOKEN_LBRACE enum_values TOKEN_RBRACE {
        $$ = zig_create_enum_decl($2, $4);
        free($2);
    }
;

struct_declaration:
    TOKEN_STRUCT TOKEN_IDENTIFIER TOKEN_LBRACE struct_fields TOKEN_RBRACE {
        $$ = zig_create_struct_decl($2, $4);
        free($2);
    }
;

struct_initializer:
    TOKEN_IDENTIFIER TOKEN_LBRACE struct_field_values TOKEN_RBRACE {
        $$ = zig_create_struct_initializer($1, $3);
        free($1);
    }
;

enum_values:
   /* empty */ { $$ = zig_create_enum_value_list(); }
   | enum_value_list { $$ = $1; }
;

enum_value_list:
    TOKEN_IDENTIFIER {
        $$ = zig_create_enum_value_list();
        zig_add_enum_value($$, $1, NULL);
        free($1);
    }
    | TOKEN_IDENTIFIER TOKEN_ASSIGN expression {
        $$ = zig_create_enum_value_list();
        zig_add_enum_value($$, $1, $3);
        free($1);
    }
    | enum_value_list TOKEN_COMMA TOKEN_IDENTIFIER {
        zig_add_enum_value($1, $3, NULL);
        free($3);
        $$ = $1;
    }
    | enum_value_list TOKEN_COMMA TOKEN_IDENTIFIER TOKEN_ASSIGN expression {
        zig_add_enum_value($1, $3, $5);
        free($3);
        $$ = $1;
    }
    | enum_value_list TOKEN_COMMA {
        /* Allow trailing comma */
        $$ = $1;
    }
;

struct_fields:
   /* empty */ { $$ = zig_create_struct_field_list(); }
   | struct_field_list { $$ = $1; }
;

struct_field_list:
    TOKEN_IDENTIFIER type_name {
        $$ = zig_create_struct_field_list();
        zig_add_struct_field($$, $1, $2);
        free($1);
        free($2);
    }
    | TOKEN_IDENTIFIER type_name TOKEN_ASSIGN expression {
        $$ = zig_create_struct_field_list();
        zig_add_struct_field_with_default($$, $1, $2, $4);
        free($1);
        free($2);
    }
    | struct_field_list TOKEN_COMMA TOKEN_IDENTIFIER type_name {
        zig_add_struct_field($1, $3, $4);
        free($3);
        free($4);
        $$ = $1;
    }
    | struct_field_list TOKEN_COMMA TOKEN_IDENTIFIER type_name TOKEN_ASSIGN expression {
        zig_add_struct_field_with_default($1, $3, $4, $6);
        free($3);
        free($4);
        $$ = $1;
    }
    | struct_field_list TOKEN_COMMA {
        /* Allow trailing comma */
        $$ = $1;
    }
;

struct_field_values:
    /* empty */ { $$ = zig_create_struct_field_value_list(); }
    | struct_field_value_list { $$ = $1; }
;

struct_field_value_list:
    TOKEN_IDENTIFIER TOKEN_ASSIGN expression {
        $$ = zig_create_struct_field_value_list();
        zig_add_struct_field_value($$, $1, $3);
        free($1);
    }
    | struct_field_value_list TOKEN_COMMA TOKEN_IDENTIFIER TOKEN_ASSIGN expression {
        zig_add_struct_field_value($1, $3, $5);
        free($3);
        $$ = $1;
    }
    | struct_field_value_list TOKEN_COMMA {
        /* Allow trailing comma */
        $$ = $1;
    }
;

initializer_expression:
    expression { $$ = $1; }
   | struct_initializer { $$ = $1; }
;

module_path:
    TOKEN_IDENTIFIER { $$ = strdup($1); }
;

%%

int yylex(void) {
    return zlang_lex(current_scanner);
}

void yyerror(const char* s) {
    int line = zlang_get_lineno(current_scanner);
    fprintf(stderr, "Parse error at line %d: %s\n", line, s);
}
