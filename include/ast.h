#ifndef CBC_AST_H
#define CBC_AST_H

#include "lexer.h"
#include <stdbool.h>

// AST Node Type Enum
// Defines all possible types of nodes in the Abstract Syntax Tree.
typedef enum {
  // Expressions
  AST_NUMBER,      // A numeric literal, e.g., 123, 'a', 'ab'
  AST_STRING,      // A string literal, e.g., "hello"
  AST_IDENT,       // An identifier, e.g., my_var, my_func
  AST_ASSIGN,      // An assignment, e.g., x = y, x =+ 1
  AST_BINARY_OP,   // A binary operation, e.g., a + b
  AST_UNARY_OP,    // A unary prefix operation, e.g., -x, ++x, &x
  AST_POSTFIX_OP,  // A unary postfix operation, e.g., x++, x--
  AST_FUNC_CALL,   // A function call, e.g., printf("hello")
  AST_INDIRECTION, // Pointer indirection, e.g., *p
  AST_ADDRESS_OF,  // Address-of operator, e.g., &x
  AST_TERNARY,     // Ternary conditional, e.g., a ? b : c
  AST_SUBSCRIPT,   // Array subscripting, e.g., v[i]

  // Statements
  AST_FUNC_DECL,  // A function definition/declaration
  AST_VAR_DECL,   // A variable declaration (auto or global)
  AST_EXTRN_DECL, // An external variable declaration
  AST_IF,         // An if-else statement
  AST_WHILE,      // A while loop
  AST_RETURN,     // A return statement
  AST_BLOCK,      // A compound statement { ... }
  AST_GOTO,       // A goto statement
  AST_SWITCH,     // A switch statement
  AST_CASE,       // A case label within a switch
  AST_DEFAULT,    // A default label within a switch
  AST_BREAK,      // A break statement
  AST_LABEL,      // A named label for goto
  AST_ASM_STMT,   // An inline assembly block
} AstNodeType;

typedef struct AstNode AstNode;

// AST Node Data Structs
// Each struct holds the specific data for one type of AST node.

typedef struct {
  long value;
} AstNumber;
typedef struct {
  char *value;
} AstString;
typedef struct {
  char *name;
} AstIdent;
typedef struct {
  TokenType op;
  AstNode *lhs;
  AstNode *rhs;
} AstAssign;
typedef struct {
  TokenType op;
  AstNode *left;
  AstNode *right;
} AstBinaryOp;
typedef struct {
  TokenType op;
  AstNode *expr;
} AstUnaryOp;
typedef struct {
  TokenType op;
  AstNode *expr;
} AstPostfixOp;
typedef struct {
  AstNode *expr;
} AstIndirection;
typedef struct {
  AstNode *lvalue;
} AstAddressOf;
typedef struct {
  AstNode *cond;
  AstNode *then_expr;
  AstNode *else_expr;
} AstTernary;
typedef struct {
  AstNode *array;
  AstNode *index;
} AstSubscript;
typedef struct {
  AstNode *func_expr;
  AstNode **args;
  int arg_count;
} AstFuncCall;
typedef struct {
  char *name;
  AstNode **params;
  int param_count;
  AstNode *body;
  bool has_varargs;
} AstFuncDecl;
typedef struct {
  char *name;
  AstNode **init_list;
  int init_count;
  AstNode *size_expr;
  bool is_vector;
} AstVarDecl;
typedef struct {
  char *name;
} AstExtrnDecl;
typedef struct {
  AstNode *cond;
  AstNode *then_body;
  AstNode *else_body;
} AstIf;
typedef struct {
  AstNode *cond;
  AstNode *body;
} AstWhile;
typedef struct {
  AstNode *expr;
} AstReturn;
typedef struct {
  AstNode **stmts;
  int stmt_count;
  bool is_synthetic;
} AstBlock;
typedef struct {
  char *label;
} AstGoto;
typedef struct {
  long value;
  char *label_name;
} AstCaseLabel;
typedef struct {
  AstNode *expr;
  AstNode *body;
  AstCaseLabel *case_labels;
  int case_count;
  char *default_label_name;
} AstSwitch;
typedef struct {
  AstNode *value;
  AstNode *body;
  char *qbe_label;
} AstCase;
typedef struct {
  AstNode *body;
  char *qbe_label;
} AstDefault;
typedef struct { /* No data needed */
} AstBreak;
typedef struct {
  char *name;
  AstNode *stmt;
} AstLabel;
typedef struct {
  char *code;
} AstAsmStmt;

// Main AST Node struct
struct AstNode {
  AstNodeType type;
  int line;
  AstNode *parent; // Parent node pointer for contextual analysis
  union {
    AstNumber number;
    AstString string;
    AstIdent ident;
    AstAssign assign;
    AstBinaryOp binary_op;
    AstUnaryOp unary_op;
    AstPostfixOp postfix_op;
    AstIndirection indirection;
    AstAddressOf address_of;
    AstTernary ternary;
    AstSubscript subscript;
    AstFuncCall func_call;
    AstFuncDecl func_decl;
    AstVarDecl var_decl;
    AstExtrnDecl extrn_decl;
    AstIf if_stmt;
    AstWhile while_stmt;
    AstReturn return_stmt;
    AstBlock block;
    AstGoto goto_stmt;
    AstSwitch switch_stmt;
    AstCase case_stmt;
    AstDefault default_stmt;
    AstBreak break_stmt;
    AstLabel label;
    AstAsmStmt asm_stmt;
  } data;
};

// Fp

// Constructors for creating new AST nodes
AstNode *ast_number(long value, int line);
AstNode *ast_string(char *value, int line);
AstNode *ast_ident(char *name, int line);
AstNode *ast_assign(TokenType op, AstNode *lhs, AstNode *rhs, int line);
AstNode *ast_binary_op(TokenType op, AstNode *left, AstNode *right, int line);
AstNode *ast_unary_op(TokenType op, AstNode *expr, int line);
AstNode *ast_postfix_op(TokenType op, AstNode *expr, int line);
AstNode *ast_indirection(AstNode *expr, int line);
AstNode *ast_address_of(AstNode *lvalue, int line);
AstNode *ast_ternary(AstNode *cond, AstNode *then_expr, AstNode *else_expr,
                     int line);
AstNode *ast_subscript(AstNode *array, AstNode *index, int line);
AstNode *ast_func_call(AstNode *func_expr, AstNode **args, int arg_count,
                       int line);
AstNode *ast_func_decl(char *name, AstNode **params, int param_count,
                       AstNode *body, bool has_varargs, int line);
AstNode *ast_var_decl(char *name, AstNode **init_list, int init_count,
                      AstNode *size_expr, bool is_vector, int line);
AstNode *ast_extrn_decl(char *name, int line);
AstNode *ast_if(AstNode *cond, AstNode *then_body, AstNode *else_body,
                int line);
AstNode *ast_while(AstNode *cond, AstNode *body, int line);
AstNode *ast_return(AstNode *expr, int line);
AstNode *ast_block(AstNode **stmts, int stmt_count, int line,
                   bool is_synthetic);
AstNode *ast_goto(char *label, int line);
AstNode *ast_switch(AstNode *expr, AstNode *body, int line);
AstNode *ast_case(AstNode *value, AstNode *body, int line);
AstNode *ast_default(AstNode *body, int line);
AstNode *ast_break(int line);
AstNode *ast_label(char *name, AstNode *stmt, int line);
AstNode *ast_asm_stmt(char *code, int line);

// Recursively frees an AST node and all its children
void ast_free(AstNode *node);

// Performs compile-time constant folding on an AST
AstNode *ast_fold_constants(AstNode *node);

#endif // CBC_AST_H
