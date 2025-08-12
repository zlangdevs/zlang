/* parser.y */
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define YYDEBUG 1
int yydebug = 0;

/* Forward declarations for Zig functions (implemented on Zig side) */
extern void* zig_create_program(void);
extern void* zig_create_function(const char* name, const char* return_type, void* body);
extern void* zig_create_var_decl(const char* type_name, const char* name, void* initializer);
extern void* zig_create_function_call(const char* name, int is_libc, void* args);
extern void* zig_create_return_stmt(void* expression);
extern void* zig_create_identifier(const char* name);
extern void* zig_create_float_literal(const char* value);
extern void* zig_create_number_literal(const char* value);
extern void* zig_create_string_literal(const char* value);
extern void* zig_create_bool_literal(int value);
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

/* produce more helpful parse errors */
%define parse.error verbose

/* semantic value union */
%union {
    char* string;
    void* node;
}

/* tokens with semantic values */
%token <string> TOKEN_IDENTIFIER TOKEN_FLOAT TOKEN_NUMBER TOKEN_STRING

/* plain tokens (declared before the grammar) */
%token TOKEN_FUN TOKEN_IF TOKEN_ELSE TOKEN_FOR TOKEN_RETURN TOKEN_VOID
%token TOKEN_TRUE TOKEN_FALSE TOKEN_BOOL
%token TOKEN_PLUS TOKEN_MINUS TOKEN_ASSIGN TOKEN_EQUAL
%token TOKEN_LBRACE TOKEN_RBRACE TOKEN_LPAREN TOKEN_RPAREN
%token TOKEN_LBRACKET TOKEN_RBRACKET TOKEN_RSHIFT
%token TOKEN_SEMICOLON TOKEN_AT TOKEN_COMMA

/* nonterminals with types */
%type <node> program function_list function statement_list statement
%type <node> var_declaration function_call return_statement
%type <node> expression argument_list arguments
%type <string> type_name function_name string_literal

%start program

%%

/* Top-level: empty program or one-or-more functions */
program:
    /* empty */ {
        /* make an empty program node for empty files */
        ast_root = zig_create_program();
        $$ = ast_root;
    }
  | function_list {
        $$ = ast_root;
    }
;

/* list of functions (one or more) */
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

/* function: fun name() >> type { statements } */
function:
    TOKEN_FUN function_name TOKEN_LPAREN TOKEN_RPAREN TOKEN_RSHIFT type_name TOKEN_LBRACE statement_list TOKEN_RBRACE {
        $$ = zig_create_function($2, $6, $8);
        free($2);
        free($6);
    }
;

/* helpers for strings */
function_name:
    TOKEN_IDENTIFIER { $$ = strdup($1); }
;

type_name:
    TOKEN_IDENTIFIER { $$ = strdup($1); }
  | TOKEN_VOID        { $$ = strdup("void"); }
  | TOKEN_BOOL        { $$ = strdup("bool"); }
;

/* statement list (zero or more statements) */
statement_list:
    /* empty */ { $$ = zig_create_stmt_list(); }
  | statement_list statement {
        if ($2 != NULL) zig_add_to_stmt_list($1, $2);
        $$ = $1;
    }
;

/* statements end with semicolons */
statement:
    var_declaration TOKEN_SEMICOLON { $$ = $1; }
  | function_call TOKEN_SEMICOLON    { $$ = $1; }
  | return_statement TOKEN_SEMICOLON{ $$ = $1; }
;

/* var decl: type name (= expr)? */
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

/* function calls: @ident(...) (libc) or ident(...) */
function_call:
    TOKEN_AT TOKEN_IDENTIFIER TOKEN_LPAREN argument_list TOKEN_RPAREN {
        $$ = zig_create_function_call($2, 1, $4);
    }
  | TOKEN_IDENTIFIER TOKEN_LPAREN argument_list TOKEN_RPAREN {
        $$ = zig_create_function_call($1, 0, $3);
    }
;

/* return with optional expression */
return_statement:
    TOKEN_RETURN expression { $$ = zig_create_return_stmt($2); }
  | TOKEN_RETURN           { $$ = zig_create_return_stmt(NULL); }
;

/* argument list (empty or comma-separated) */
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

/* expressions:
   - allow function_call as an expression (important for RHS of var_decl)
   - then identifiers, numbers, strings
   (You can later extend with binary/unary ops, parens, etc.)
*/
expression:
    function_call                { $$ = $1; }
  | TOKEN_IDENTIFIER            { $$ = zig_create_identifier($1); }
  | TOKEN_FLOAT                 { $$ = zig_create_float_literal($1); }
  | TOKEN_NUMBER                { $$ = zig_create_number_literal($1); }
  | string_literal              {
        $$ = zig_create_string_literal($1);
        free($1);
    }
  | TOKEN_TRUE                  { $$ = zig_create_bool_literal(1); }
  | TOKEN_FALSE                 { $$ = zig_create_bool_literal(0); }
;

/* string literal wrapper */
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
