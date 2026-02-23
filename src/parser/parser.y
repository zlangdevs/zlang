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
extern void* zig_create_function(const char* name, const char* return_type, void* params, void* guard, void* body);
extern void* zig_create_var_decl(const char* type_name, const char* name, void* initializer, int is_const);
extern void* zig_create_function_call(const char* name, int is_libc, void* args);
extern void* zig_create_method_call(void* object, const char* method_name, void* args);
extern void* zig_create_comparison(char op, void* lhs, void* rhs);
extern void* zig_create_return_stmt(void* expression);
extern void* zig_create_binary_op(char op, void* lhs, void* rhs);
extern void* zig_create_unary_op(char op, void* operand);
extern void* zig_create_assignment(void* target, void* value);
extern void* zig_create_compound_assignment(void* target, void* value, int op);
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
extern void* zig_create_goto_stmt(const char* label);
extern void* zig_create_label_stmt(const char* label);
extern void* zig_create_match_stmt(void* condition, void* cases);
extern void* zig_create_match_case_list(void);
extern void* zig_create_match_case(void* values, void* body);
extern void zig_add_match_case(void* list, void* match_case);
extern void* zig_create_array_initializer(void* elements);
extern void* zig_create_array_index(void* array, void* index);
extern void* zig_create_array_assignment(void* array, void* index, void* value);
extern void* zig_create_array_compound_assignment(void* array, void* index, void* value, int op);
extern void* zig_create_c_function_decl(const char* name, const char* return_type, void* params);
extern void* zig_create_wrapper_function(const char* name, const char* return_type, void* params);
extern void* zig_create_use_stmt(const char* module_path);
extern void* zig_create_enum_decl(const char* name, void* values);
extern void* zig_create_struct_decl(const char* name, void* fields);
extern void* zig_create_union_decl(const char* name, void* fields);
extern void* zig_create_qualified_identifier(void* base, const char* field);
extern void* zig_create_enum_value_list(void);
extern void zig_add_enum_value(void* list, const char* name, void* value);
extern void* zig_create_struct_field_list(void);
extern void zig_add_struct_field(void* list, const char* name, const char* type_name);
extern void zig_add_struct_field_with_default(void* list, const char* name, const char* type_name, void* default_value);
extern void* zig_create_struct_initializer(const char* struct_name, void* field_values);
extern void* zig_create_struct_field_value_list(void);
extern void zig_add_struct_field_value(void* list, const char* field_name, void* value);
extern void zig_add_struct_positional_value(void* list, void* value);
extern void zig_add_to_program(void* program, void* function);
extern void zig_add_global_to_program(void* program, void* global);
extern void zig_add_to_stmt_list(void* list, void* stmt);
extern void zig_add_to_arg_list(void* list, void* arg);
extern void* zig_create_cast(void* expr, const char* type_name, int auto_flag);
extern void* zig_create_expression_block(const char* type_name, void* statements, void* result);
extern void zlang_set_location(int line, int col);
extern void zig_record_parse_error(int line, int col, const char* msg);

void yyerror(const char* s);
int zlang_lex(void* scanner);
int yylex(void);

extern int zlang_get_lineno(void* scanner);

extern void* current_scanner;
void* ast_root = NULL;
%}

/* Expected shift/reduce conflicts (benign, resolved by GLR):
   - State 1: TOKEN_IDENTIFIER as type_name vs start of complex_type_name
   - State ~68: TOKEN_IDENTIFIER as ref_base vs function_call vs struct_initializer
   - State ~236: TOKEN_IDENTIFIER in multiple contexts (label, type, ref_base)
   These ambiguities are inherent to the language syntax and correctly resolved. */
%expect 4
%expect-rr 0

%define parse.error verbose

%glr-parser
%locations

%union {
    char* string;
    void* node;
    int number;
}

%token <string> TOKEN_IDENTIFIER TOKEN_FLOAT TOKEN_NUMBER TOKEN_STRING TOKEN_BRAINFUCK
%token <number> TOKEN_CHAR
%token <number> TOKEN_REASSIGN

%token TOKEN_FUN TOKEN_IF TOKEN_ELSE TOKEN_FOR TOKEN_RETURN TOKEN_VOID TOKEN_BREAK TOKEN_CONTINUE TOKEN_GOTO TOKEN_USE TOKEN_WRAP TOKEN_ENUM TOKEN_STRUCT TOKEN_UNION TOKEN_DOT TOKEN_NULL TOKEN_CONST TOKEN_WHEN
%token TOKEN_AS TOKEN_UNDERSCORE TOKEN_MATCH
%token TOKEN_ASSIGN TOKEN_EQUAL TOKEN_NON_EQUAL TOKEN_LESS TOKEN_GREATER TOKEN_EQ_LESS TOKEN_EQ_GREATER
%token TOKEN_LBRACE TOKEN_RBRACE TOKEN_LPAREN TOKEN_RPAREN
%token TOKEN_LBRACKET TOKEN_RBRACKET TOKEN_RSHIFT TOKEN_LSHIFT TOKEN_DECREMENT TOKEN_INCREMENT
%token TOKEN_COLON TOKEN_SEMICOLON TOKEN_AT TOKEN_COMMA TOKEN_PLUS TOKEN_MINUS TOKEN_MULTIPLY TOKEN_DIVIDE TOKEN_MODULUS TOKEN_AND TOKEN_OR TOKEN_NOT TOKEN_AMPERSAND TOKEN_BIT_OR TOKEN_XOR TOKEN_BIT_NOT

%type <node> if_statement
%type <node> for_statement
%type <node> break_statement
%type <node> continue_statement
%type <node> goto_statement
%type <node> label_statement
%type <node> match_statement match_case_list match_case match_value_list

%left TOKEN_OR
%left TOKEN_AND
%left TOKEN_BIT_OR
%left TOKEN_XOR
%left TOKEN_AMPERSAND
%left TOKEN_EQUAL TOKEN_NON_EQUAL
%left TOKEN_LESS TOKEN_GREATER TOKEN_EQ_LESS TOKEN_EQ_GREATER
%left TOKEN_LSHIFT TOKEN_RSHIFT
%left TOKEN_PLUS TOKEN_MINUS
%left TOKEN_MULTIPLY TOKEN_DIVIDE
%right TOKEN_ASSIGN TOKEN_REASSIGN
%left TOKEN_INCREMENT TOKEN_DECREMENT
%left TOKEN_LBRACKET TOKEN_LPAREN TOKEN_DOT
%left TOKEN_AT
%right TOKEN_AS
%right NOT UMINUS UPLUS UAMPERSAND UDEREF UBIT_NOT

%type <node> parameter_list parameters parameter
%type <node> program function_list function statement_list statement
%type <node> var_declaration global_variable_declaration function_call return_statement assignment brainfuck_statement
%type <node> expression logical_or_expression logical_and_expression bitwise_or_expression bitwise_xor_expression bitwise_and_expression shift_expression equality_expression relational_expression additive_expression multiplicative_expression unary_expression primary_expression postfix_expression argument_list arguments
%type <node> cast_expression expression_block
%type <node> c_for_statement for_increment
%type <node> array_initializer c_function_decl c_function_decl_statement use_statement enum_declaration struct_declaration union_declaration wrap_statement
%type <node> enum_values enum_value_list struct_fields struct_field_list
%type <node> struct_initializer struct_field_values struct_field_value_list initializer_expression ref_expression ref_base
%type <string> type_name function_name string_literal complex_type_name module_path function_type_core function_type_param_list type_list template_params

%start program

%%

/* ========== TOP-LEVEL PROGRAM STRUCTURE ========== */

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
   | union_declaration {
       if (ast_root == NULL) {
           ast_root = zig_create_program();
       }
       zig_add_to_program(ast_root, $1);
   }
   | function_list union_declaration {
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

/* ========== FUNCTION DECLARATIONS ========== */

c_function_decl:
    TOKEN_FUN TOKEN_AT function_name TOKEN_LPAREN parameter_list TOKEN_RPAREN TOKEN_RSHIFT type_name {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_c_function_decl($3, $8, $5);
        free($3);
        free($8);
    }
;

c_function_decl_statement:
    c_function_decl TOKEN_SEMICOLON { $$ = $1; }
;

wrap_statement:
    TOKEN_WRAP TOKEN_AT function_name TOKEN_LPAREN parameter_list TOKEN_RPAREN TOKEN_RSHIFT type_name TOKEN_SEMICOLON {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_wrapper_function($3, $8, $5);
        free($3);
        free($8);
    }
;

template_params:
    /* empty */ { $$ = NULL; }
  | TOKEN_LBRACKET type_list TOKEN_RBRACKET {
        char* result = malloc(strlen($2) + 3);
        sprintf(result, "[%s]", $2);
        free($2);
        $$ = result;
    }
;

function:
    TOKEN_FUN function_name template_params TOKEN_LPAREN parameter_list TOKEN_RPAREN TOKEN_RSHIFT type_name TOKEN_LBRACE statement_list TOKEN_RBRACE {
        char* return_type_str;
        if ($3) {
            return_type_str = malloc(strlen($3) + strlen($8) + 3);
            sprintf(return_type_str, "%s>>%s", $3, $8);
            free($3);
        } else {
            return_type_str = strdup($8);
        }
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_function($2, return_type_str, $5, NULL, $10);
        free($2);
        free($8);
        free(return_type_str);
    }
  | TOKEN_FUN function_name template_params TOKEN_LPAREN parameter_list TOKEN_RPAREN TOKEN_WHEN TOKEN_LPAREN expression TOKEN_RPAREN TOKEN_RSHIFT type_name TOKEN_LBRACE statement_list TOKEN_RBRACE {
        char* return_type_str;
        if ($3) {
            return_type_str = malloc(strlen($3) + strlen($12) + 3);
            sprintf(return_type_str, "%s>>%s", $3, $12);
            free($3);
        } else {
            return_type_str = strdup($12);
        }
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_function($2, return_type_str, $5, $9, $14);
        free($2);
        free($12);
        free(return_type_str);
    }
;

parameter_list:
    /* empty */ { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_param_list(); }
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
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_parameter($1, $3);
        free($3);
    }
;

/* ========== TYPE SYSTEM ========== */

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
  | TOKEN_IDENTIFIER TOKEN_LESS type_name TOKEN_COMMA TOKEN_UNDERSCORE TOKEN_GREATER %prec TOKEN_LESS {
        char* result = malloc(strlen($1) + strlen($3) + 6);
        sprintf(result, "%s<%s, _>", $1, $3);
        free($3);
        $$ = result;
    }
  | TOKEN_IDENTIFIER TOKEN_LESS TOKEN_UNDERSCORE TOKEN_GREATER %prec TOKEN_LESS {
        char* result = malloc(strlen($1) + 4);
        sprintf(result, "%s<_>", $1);
        $$ = result;
    }
  | TOKEN_IDENTIFIER TOKEN_LESS TOKEN_CONST type_name TOKEN_GREATER %prec TOKEN_LESS {
        char* result = malloc(strlen($1) + strlen($4) + 12);
        sprintf(result, "%s<const %s>", $1, $4);
        free($4);
        $$ = result;
    }
  | TOKEN_IDENTIFIER TOKEN_LESS type_name TOKEN_GREATER %prec TOKEN_LESS {
        char* result = malloc(strlen($1) + strlen($3) + 5);
        sprintf(result, "%s<%s>", $1, $3);
        free($3);
        $$ = result;
    }
  | TOKEN_IDENTIFIER TOKEN_LESS function_type_core TOKEN_GREATER %prec TOKEN_LESS {
        char* result = malloc(strlen($1) + strlen($3) + 5);
        sprintf(result, "%s<%s>", $1, $3);
        free($3);
        $$ = result;
    }
;
function_type_core:
    type_name TOKEN_LPAREN TOKEN_RPAREN {
        const char* ret = $1;
        size_t len = strlen(ret) + 3;
        char* result = malloc(len);
        result[0] = '\0';
        strcat(result, ret);
        strcat(result, "()");
        free($1);
        $$ = result;
    }
  | type_name TOKEN_LPAREN function_type_param_list TOKEN_RPAREN {
        const char* ret = $1;
        const char* params = $3;
        size_t len = strlen(ret) + strlen(params) + 3;
        char* result = malloc(len);
        result[0] = '\0';
        strcat(result, ret);
        strcat(result, "(");
        strcat(result, params);
        strcat(result, ")");
        free($1);
        free($3);
        $$ = result;
    }
;

function_type_param_list:
    type_name { $$ = $1; }
  | function_type_param_list TOKEN_COMMA type_name {
        const char* lhs = $1;
        const char* rhs = $3;
        size_t len = strlen(lhs) + strlen(rhs) + 2;
        char* result = malloc(len);
        result[0] = '\0';
        strcat(result, lhs);
        strcat(result, ",");
        strcat(result, rhs);
        free($1);
        free($3);
        $$ = result;
    }
;

type_list:
    type_name { $$ = $1; }
  | type_list TOKEN_COMMA type_name {
        const char* lhs = $1;
        const char* rhs = $3;
        size_t len = strlen(lhs) + strlen(rhs) + 3; // +2 for ", " and +1 for null
        char* result = malloc(len);
        result[0] = '\0';
        strcat(result, lhs);
        strcat(result, ", ");
        strcat(result, rhs);
        free($1);
        free($3);
        $$ = result;
    }
;

 type_name:
    complex_type_name { $$ = $1; }
  | TOKEN_IDENTIFIER { $$ = strdup($1); }
  | TOKEN_LBRACKET type_list TOKEN_RBRACKET {
        const char* list = $2;
        size_t len = strlen(list) + 3; // +2 for [] and +1 for null
        char* result = malloc(len);
        result[0] = '\0';
        strcat(result, "[");
        strcat(result, list);
        strcat(result, "]");
        free($2);
        $$ = result;
    }
  | TOKEN_LBRACKET TOKEN_RBRACKET type_name {
        const char* inner = $3;
        size_t len = strlen(inner) + 3; // +2 for [] and +1 for null
        char* result = malloc(len);
        sprintf(result, "[]%s", inner);
        free($3);
        $$ = result;
    }
 ;

/* ========== STATEMENTS ========== */

statement_list:
    /* empty */ { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_stmt_list(); }
  | statement_list statement {
        if ($2 != NULL) zig_add_to_stmt_list($1, $2);
        $$ = $1;
    }
;

statement:
    var_declaration TOKEN_SEMICOLON { $$ = $1; }
   | assignment TOKEN_SEMICOLON { $$ = $1; }
   | return_statement TOKEN_SEMICOLON { $$ = $1; }
   | brainfuck_statement TOKEN_SEMICOLON { $$ = $1; }
   | if_statement { $$ = $1; }
   | for_statement { $$ = $1; }
   | c_for_statement { $$ = $1; }
   | break_statement TOKEN_SEMICOLON { $$ = $1; }
   | continue_statement TOKEN_SEMICOLON { $$ = $1; }
   | goto_statement TOKEN_SEMICOLON { $$ = $1; }
   | label_statement { $$ = $1; }
   | match_statement { $$ = $1; }
   | c_function_decl TOKEN_SEMICOLON { $$ = $1; }
   | use_statement { $$ = $1; }
   | expression TOKEN_SEMICOLON { $$ = $1; }
;

if_statement:
    TOKEN_IF expression TOKEN_LBRACE statement_list TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_if_stmt($2, $4, NULL);
    }
  | TOKEN_IF expression TOKEN_LBRACE statement_list TOKEN_RBRACE TOKEN_ELSE TOKEN_LBRACE statement_list TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_if_stmt($2, $4, $8);
    }
  | TOKEN_IF expression TOKEN_LBRACE statement_list TOKEN_RBRACE TOKEN_ELSE if_statement {
        void* else_stmt_list = zig_create_stmt_list();
        zig_add_to_stmt_list(else_stmt_list, $7);
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_if_stmt($2, $4, else_stmt_list);
    }
;

for_statement:
    TOKEN_FOR TOKEN_LBRACE statement_list TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_for_stmt(NULL, $3);
    }
  | TOKEN_FOR TOKEN_LPAREN expression TOKEN_RPAREN TOKEN_LBRACE statement_list TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_for_stmt($3, $6);
    }
  | TOKEN_FOR TOKEN_IDENTIFIER TOKEN_LBRACE statement_list TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_for_stmt(zig_create_identifier($2), $4);
    }
;

c_for_statement:
    TOKEN_FOR var_declaration TOKEN_SEMICOLON expression TOKEN_SEMICOLON for_increment TOKEN_LBRACE statement_list TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_c_for_stmt($2, $4, $6, $8);
    }
  | TOKEN_FOR assignment TOKEN_SEMICOLON expression TOKEN_SEMICOLON for_increment TOKEN_LBRACE statement_list TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_c_for_stmt($2, $4, $6, $8);
    }
;

match_statement:
    TOKEN_MATCH expression TOKEN_LBRACE match_case_list TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_match_stmt($2, $4);
    }
;

match_case_list:
    match_case {
        void* list = zig_create_match_case_list();
        zig_add_match_case(list, $1);
        $$ = list;
    }
  | match_case_list match_case {
        zig_add_match_case($1, $2);
        $$ = $1;
    }
;

match_case:
    match_value_list TOKEN_LBRACE statement_list TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_match_case($1, $3);
    }
;

match_value_list:
    expression {
        void* list = zig_create_arg_list();
        zig_add_to_arg_list(list, $1);
        $$ = list;
    }
  | match_value_list TOKEN_COMMA expression {
        zig_add_to_arg_list($1, $3);
        $$ = $1;
    }
;

for_increment:
    assignment { $$ = $1; }
  | unary_expression { $$ = $1; }
;

break_statement:
    TOKEN_BREAK {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_break_stmt();
    }
;

continue_statement:
    TOKEN_CONTINUE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_continue_stmt();
    }
;

goto_statement:
    TOKEN_GOTO TOKEN_IDENTIFIER {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_goto_stmt($2);
        free($2);
    }
;

label_statement:
    TOKEN_IDENTIFIER TOKEN_COLON {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_label_stmt($1);
        free($1);
    }
;

brainfuck_statement:
    TOKEN_BRAINFUCK {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_brainfuck($1);
        free($1);
    }
;

/* ========== EXPRESSIONS ========== */

ref_base:
    TOKEN_IDENTIFIER { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_identifier($1); }
;

ref_expression:
    ref_base { $$ = $1; }
  | ref_expression TOKEN_LBRACKET expression TOKEN_RBRACKET {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_array_index($1, $3);
    }
  | ref_expression TOKEN_DOT TOKEN_IDENTIFIER {
        const char* field_copy = strdup($3);
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_qualified_identifier($1, field_copy);
        free($3);
    }
;

assignment:
    ref_expression TOKEN_ASSIGN initializer_expression {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_assignment($1, $3);
    }
  | ref_expression TOKEN_REASSIGN expression {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_compound_assignment($1, $3, $2);
    }
  | TOKEN_MULTIPLY ref_expression TOKEN_ASSIGN expression {
        void* deref = zig_create_unary_op('*', $2);
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_assignment(deref, $4);
    }
  | TOKEN_MULTIPLY ref_expression TOKEN_REASSIGN expression {
        void* deref = zig_create_unary_op('*', $2);
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_compound_assignment(deref, $4, $3);
    }
  | TOKEN_MULTIPLY TOKEN_LPAREN expression TOKEN_RPAREN TOKEN_ASSIGN expression {
        void* inner_expr = $3;
        void* deref = zig_create_unary_op('*', inner_expr);
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_assignment(deref, $6);
    }
  | TOKEN_MULTIPLY TOKEN_LPAREN expression TOKEN_RPAREN TOKEN_REASSIGN expression {
        void* inner_expr = $3;
        void* deref = zig_create_unary_op('*', inner_expr);
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_compound_assignment(deref, $6, $5);
    }
;

array_initializer:
    TOKEN_LBRACE argument_list TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_array_initializer($2);
    }
;

var_declaration:
    type_name TOKEN_IDENTIFIER TOKEN_ASSIGN initializer_expression {
        void* initializer = $4;
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_var_decl($1, $2, initializer, 0);
        free($1);
    }
    | type_name TOKEN_IDENTIFIER {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_var_decl($1, $2, NULL, 0);
        free($1);
    }
    | TOKEN_CONST type_name TOKEN_IDENTIFIER TOKEN_ASSIGN initializer_expression {
        void* initializer = $5;
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_var_decl($2, $3, initializer, 1);
        free($2);
    }
;

global_variable_declaration:
    type_name TOKEN_IDENTIFIER TOKEN_ASSIGN initializer_expression TOKEN_SEMICOLON {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_var_decl($1, $2, $4, 0);
        free($1);
    }
    | TOKEN_CONST type_name TOKEN_IDENTIFIER TOKEN_ASSIGN initializer_expression TOKEN_SEMICOLON {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_var_decl($2, $3, $5, 1);
        free($2);
    }
;

function_call:
    TOKEN_AT TOKEN_IDENTIFIER TOKEN_LPAREN argument_list TOKEN_RPAREN {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_function_call($2, 1, $4);
    }
  | TOKEN_IDENTIFIER TOKEN_LPAREN argument_list TOKEN_RPAREN {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_function_call($1, 0, $3);
    }
;

return_statement:
    TOKEN_RETURN initializer_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_return_stmt($2); }
   | TOKEN_RETURN { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_return_stmt(NULL); }
;

argument_list:
    /* empty */ { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_arg_list(); }
  | arguments { $$ = $1; }
;

arguments:
    initializer_expression {
        void* list = zig_create_arg_list();
        zig_add_to_arg_list(list, $1);
        $$ = list;
    }
  | arguments TOKEN_COMMA initializer_expression {
        zig_add_to_arg_list($1, $3);
        $$ = $1;
    }
;

expression:
    logical_or_expression { $$ = $1; }
;

logical_or_expression:
    logical_and_expression { $$ = $1; }
  | logical_or_expression TOKEN_OR logical_and_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_binary_op('|', $1, $3); }
;

logical_and_expression:
    bitwise_or_expression { $$ = $1; }
  | logical_and_expression TOKEN_AND bitwise_or_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_binary_op('&', $1, $3); }
;

bitwise_or_expression:
    bitwise_xor_expression { $$ = $1; }
  | bitwise_or_expression TOKEN_BIT_OR bitwise_xor_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_binary_op('$', $1, $3); }
;

bitwise_xor_expression:
    bitwise_and_expression { $$ = $1; }
  | bitwise_xor_expression TOKEN_XOR bitwise_and_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_binary_op('^', $1, $3); }
;

bitwise_and_expression:
    shift_expression { $$ = $1; }
  | bitwise_and_expression TOKEN_AMPERSAND shift_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_binary_op('A', $1, $3); }
;

shift_expression:
    equality_expression { $$ = $1; }
  | shift_expression TOKEN_LSHIFT equality_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_binary_op('<', $1, $3); }
  | shift_expression TOKEN_RSHIFT equality_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_binary_op('>', $1, $3); }
;

equality_expression:
    relational_expression { $$ = $1; }
  | equality_expression TOKEN_EQUAL relational_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_comparison('=', $1, $3); }
  | equality_expression TOKEN_NON_EQUAL relational_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_comparison('!', $1, $3); }
;

relational_expression:
    additive_expression { $$ = $1; }
  | relational_expression TOKEN_LESS additive_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_comparison('<', $1, $3); }
  | relational_expression TOKEN_GREATER additive_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_comparison('>', $1, $3); }
  | relational_expression TOKEN_EQ_LESS additive_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_comparison('L', $1, $3); }
  | relational_expression TOKEN_EQ_GREATER additive_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_comparison('G', $1, $3); }
;

additive_expression:
    multiplicative_expression { $$ = $1; }
  | additive_expression TOKEN_PLUS multiplicative_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_binary_op('+', $1, $3); }
  | additive_expression TOKEN_MINUS multiplicative_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_binary_op('-', $1, $3); }
;

multiplicative_expression:
    unary_expression { $$ = $1; }
  | multiplicative_expression TOKEN_MULTIPLY unary_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_binary_op('*', $1, $3); }
  | multiplicative_expression TOKEN_DIVIDE unary_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_binary_op('/', $1, $3); }
  | multiplicative_expression TOKEN_MODULUS unary_expression { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_binary_op('%', $1, $3); }
;

unary_expression:
    cast_expression { $$ = $1; }
  | TOKEN_NOT unary_expression %prec NOT { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_unary_op('!', $2); }
  | TOKEN_BIT_NOT unary_expression %prec UBIT_NOT { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_unary_op('~', $2); }
  | TOKEN_MINUS unary_expression %prec UMINUS { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_unary_op('-', $2); }
  | TOKEN_PLUS unary_expression %prec UPLUS { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_unary_op('+', $2); }
  | TOKEN_AMPERSAND unary_expression %prec UAMPERSAND { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_unary_op('&', $2); }
  | TOKEN_MULTIPLY unary_expression %prec UDEREF { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_unary_op('*', $2); }
;

postfix_expression:
    ref_expression { $$ = $1; }
  | primary_expression { $$ = $1; }
  | ref_expression TOKEN_DOT TOKEN_IDENTIFIER TOKEN_LPAREN argument_list TOKEN_RPAREN {
       const char* method_name_copy = strdup($3);
       zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_method_call($1, method_name_copy, $5);
       free($3);
   }
  | postfix_expression TOKEN_INCREMENT { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_unary_op('I', $1); }
  | postfix_expression TOKEN_DECREMENT { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_unary_op('D', $1); }
;

cast_expression:
    postfix_expression { $$ = $1; }
  | cast_expression TOKEN_AS type_name {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_cast($1, $3, 0);
        free($3);
    }
  | cast_expression TOKEN_AS TOKEN_UNDERSCORE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_cast($1, "", 1);
    }
;

expression_block:
    TOKEN_LESS type_name TOKEN_GREATER TOKEN_LBRACE statement_list initializer_expression TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_expression_block($2, $5, $6);
        free($2);
    }
;

primary_expression:
      array_initializer { $$ = $1; }
    | TOKEN_FLOAT { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_float_literal($1); }
    | TOKEN_NUMBER { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_number_literal($1); }
    | TOKEN_CHAR { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_char_literal($1); }
    | string_literal { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_string_literal($1); free($1); }
    | function_call { $$ = $1; }
    | TOKEN_LPAREN expression TOKEN_RPAREN { $$ = $2; }
    | expression_block { $$ = $1; }
    | TOKEN_NULL { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_null_literal(); }
;

/* ========== LITERALS AND IDENTIFIERS ========== */

string_literal:
    TOKEN_STRING { $$ = strdup($1); }
;

/* ========== DECLARATIONS (use, enum, struct) ========== */

use_statement:
    TOKEN_USE module_path {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_use_stmt($2);
        free($2);
    }
;

enum_declaration:
    TOKEN_ENUM TOKEN_IDENTIFIER TOKEN_LBRACE enum_values TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_enum_decl($2, $4);
        free($2);
    }
;

struct_declaration:
    TOKEN_STRUCT TOKEN_IDENTIFIER TOKEN_LBRACE struct_fields TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_struct_decl($2, $4);
        free($2);
    }
;

union_declaration:
    TOKEN_UNION TOKEN_IDENTIFIER TOKEN_LBRACE struct_fields TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_union_decl($2, $4);
        free($2);
    }
;

struct_initializer:
    TOKEN_IDENTIFIER TOKEN_LBRACE struct_field_values TOKEN_RBRACE {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_struct_initializer($1, $3);
        free($1);
    }
;

enum_values:
   /* empty */ { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_enum_value_list(); }
   | enum_value_list { $$ = $1; }
;

enum_value_list:
    TOKEN_IDENTIFIER {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_enum_value_list();
        zig_add_enum_value($$, $1, NULL);
        free($1);
    }
    | TOKEN_IDENTIFIER TOKEN_ASSIGN expression {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_enum_value_list();
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
   /* empty */ { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_struct_field_list(); }
   | struct_field_list { $$ = $1; }
;

struct_field_list:
    TOKEN_IDENTIFIER type_name {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_struct_field_list();
        zig_add_struct_field($$, $1, $2);
        free($1);
        free($2);
    }
    | TOKEN_IDENTIFIER type_name TOKEN_ASSIGN expression {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_struct_field_list();
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
    /* empty */ { zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_struct_field_value_list(); }
    | struct_field_value_list { $$ = $1; }
;

struct_field_value_list:
    TOKEN_IDENTIFIER TOKEN_ASSIGN expression {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_struct_field_value_list();
        zig_add_struct_field_value($$, $1, $3);
        free($1);
    }
    | struct_field_value_list TOKEN_COMMA TOKEN_IDENTIFIER TOKEN_ASSIGN expression {
        zig_add_struct_field_value($1, $3, $5);
        free($3);
        $$ = $1;
    }
    | expression {
        zlang_set_location(@$.first_line, @$.first_column); $$ = zig_create_struct_field_value_list();
        zig_add_struct_positional_value($$, $1);
    }
    | struct_field_value_list TOKEN_COMMA expression {
        zig_add_struct_positional_value($1, $3);
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
    | module_path TOKEN_DOT TOKEN_IDENTIFIER {
        size_t len = strlen($1) + strlen($3) + 2; // dot + null terminator
        $$ = malloc(len);
        snprintf($$, len, "%s.%s", $1, $3);
        free($1);
    }
;

%%

int yylex(void) {
    return zlang_lex(current_scanner);
}

void yyerror(const char* s) {
    int line = zlang_get_lineno(current_scanner);
    zig_record_parse_error(line, 0, s);
}
