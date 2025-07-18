#ifndef CBC_AST_H
#define CBC_AST_H

#include "lexer.h"

typedef enum {
    // Expressions
    AST_NUMBER,
    AST_STRING,
    AST_IDENT,
    AST_ASSIGN,
    AST_BINARY_OP,
    AST_UNARY_OP,
    AST_FUNC_CALL,
    AST_ARRAY_INDEX,

    // Statements
    AST_FUNC_DECL,
    AST_VAR_DECL,
    AST_EXTRN_DECL,
    AST_IF,
    AST_WHILE,
    AST_RETURN,
    AST_BLOCK
} AstNodeType;

typedef struct AstNode AstNode;

typedef struct {
    int value;
} AstNumber;

typedef struct {
    char* value;
} AstString;

typedef struct {
    char* name;
} AstIdent;

typedef struct {
    AstNode* lhs; // L-value (identifier or array index)
    AstNode* rhs; // R-value
} AstAssign;

typedef struct {
    TokenType op;
    AstNode* left;
    AstNode* right;
} AstBinaryOp;

typedef struct {
    TokenType op;
    AstNode* expr;
} AstUnaryOp;

typedef struct {
    char* name;
    AstNode** args;
    int arg_count;
} AstFuncCall;

typedef struct {
    AstNode* array;
    AstNode* index;
} AstArrayIndex;

typedef struct {
    char* name;
    AstNode** params;
    int param_count;
    AstNode* body;
} AstFuncDecl;

typedef struct {
    char* name;
    AstNode* init; // NULLable
} AstVarDecl;

typedef struct {
    char* name;
} AstExtrnDecl;

typedef struct {
    AstNode* cond;
    AstNode* then_body;
    AstNode* else_body; // NULLable
} AstIf;

typedef struct {
    AstNode* cond;
    AstNode* body;
} AstWhile;

typedef struct {
    AstNode* expr; // NULLable
} AstReturn;

typedef struct {
    AstNode** stmts;
    int stmt_count;
} AstBlock;

struct AstNode {
    AstNodeType type;
    int line;
    union {
        AstNumber number;
        AstString string;
        AstIdent ident;
        AstAssign assign;
        AstBinaryOp binary_op;
        AstUnaryOp unary_op;
        AstFuncCall func_call;
        AstArrayIndex array_index;
        AstFuncDecl func_decl;
        AstVarDecl var_decl;
        AstExtrnDecl extrn_decl;
        AstIf if_stmt;
        AstWhile while_stmt;
        AstReturn return_stmt;
        AstBlock block;
    } data;
};

// Constructors
AstNode* ast_number(int value, int line);
AstNode* ast_string(char* value, int line);
AstNode* ast_ident(char* name, int line);
AstNode* ast_assign(AstNode* lhs, AstNode* rhs, int line);
AstNode* ast_binary_op(TokenType op, AstNode* left, AstNode* right, int line);
AstNode* ast_unary_op(TokenType op, AstNode* expr, int line);
AstNode* ast_func_call(char* name, AstNode** args, int arg_count, int line);
AstNode* ast_array_index(AstNode* array, AstNode* index, int line);
AstNode* ast_func_decl(char* name, AstNode** params, int param_count, AstNode* body, int line);
AstNode* ast_var_decl(char* name, AstNode* init, int line);
AstNode* ast_extrn_decl(char* name, int line);
AstNode* ast_if(AstNode* cond, AstNode* then_body, AstNode* else_body, int line);
AstNode* ast_while(AstNode* cond, AstNode* body, int line);
AstNode* ast_return(AstNode* expr, int line);
AstNode* ast_block(AstNode** stmts, int stmt_count, int line);

// mem mgmt
void ast_free(AstNode* node);

#endif // CBC_AST_H

