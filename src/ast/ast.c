#include "ast.h"
#include <stdlib.h>
#include <string.h>

#define ALLOC_NODE(type_enum, line_num) \
    AstNode* node = malloc(sizeof(AstNode)); \
    node->type = type_enum; \
    node->line = line_num;

AstNode* ast_number(int value, int line) {
    ALLOC_NODE(AST_NUMBER, line);
    node->data.number.value = value;
    return node;
}

AstNode* ast_string(char* value, int line) {
    ALLOC_NODE(AST_STRING, line);
    node->data.string.value = value;
    return node;
}

AstNode* ast_ident(char* name, int line) {
    ALLOC_NODE(AST_IDENT, line);
    node->data.ident.name = name;
    return node;
}

AstNode* ast_assign(AstNode* lhs, AstNode* rhs, int line) {
    ALLOC_NODE(AST_ASSIGN, line);
    node->data.assign.lhs = lhs;
    node->data.assign.rhs = rhs;
    return node;
}

AstNode* ast_binary_op(TokenType op, AstNode* left, AstNode* right, int line) {
    ALLOC_NODE(AST_BINARY_OP, line);
    node->data.binary_op.op = op;
    node->data.binary_op.left = left;
    node->data.binary_op.right = right;
    return node;
}

AstNode* ast_unary_op(TokenType op, AstNode* expr, int line) {
    ALLOC_NODE(AST_UNARY_OP, line);
    node->data.unary_op.op = op;
    node->data.unary_op.expr = expr;
    return node;
}

AstNode* ast_func_call(char* name, AstNode** args, int arg_count, int line) {
    ALLOC_NODE(AST_FUNC_CALL, line);
    node->data.func_call.name = name;
    node->data.func_call.args = args;
    node->data.func_call.arg_count = arg_count;
    return node;
}

AstNode* ast_array_index(AstNode* array, AstNode* index, int line) {
    ALLOC_NODE(AST_ARRAY_INDEX, line);
    node->data.array_index.array = array;
    node->data.array_index.index = index;
    return node;
}

AstNode* ast_func_decl(char* name, AstNode** params, int param_count, AstNode* body, int line) {
    ALLOC_NODE(AST_FUNC_DECL, line);
    node->data.func_decl.name = name;
    node->data.func_decl.params = params;
    node->data.func_decl.param_count = param_count;
    node->data.func_decl.body = body;
    return node;
}

AstNode* ast_var_decl(char* name, AstNode* init, int line) {
    ALLOC_NODE(AST_VAR_DECL, line);
    node->data.var_decl.name = name;
    node->data.var_decl.init = init;
    return node;
}

AstNode* ast_extrn_decl(char* name, int line) {
    ALLOC_NODE(AST_EXTRN_DECL, line);
    node->data.extrn_decl.name = name;
    return node;
}

AstNode* ast_if(AstNode* cond, AstNode* then_body, AstNode* else_body, int line) {
    ALLOC_NODE(AST_IF, line);
    node->data.if_stmt.cond = cond;
    node->data.if_stmt.then_body = then_body;
    node->data.if_stmt.else_body = else_body;
    return node;
}

AstNode* ast_while(AstNode* cond, AstNode* body, int line) {
    ALLOC_NODE(AST_WHILE, line);
    node->data.while_stmt.cond = cond;
    node->data.while_stmt.body = body;
    return node;
}

AstNode* ast_return(AstNode* expr, int line) {
    ALLOC_NODE(AST_RETURN, line);
    node->data.return_stmt.expr = expr;
    return node;
}

AstNode* ast_block(AstNode** stmts, int stmt_count, int line) {
    ALLOC_NODE(AST_BLOCK, line);
    node->data.block.stmts = stmts;
    node->data.block.stmt_count = stmt_count;
    return node;
}

void ast_free(AstNode* node) {
    if (!node) return;

    switch (node->type) {
        case AST_STRING: free(node->data.string.value); break;
        case AST_IDENT: free(node->data.ident.name); break;
        case AST_ASSIGN:
            ast_free(node->data.assign.lhs);
            ast_free(node->data.assign.rhs);
            break;
        case AST_BINARY_OP:
            ast_free(node->data.binary_op.left);
            ast_free(node->data.binary_op.right);
            break;
        case AST_UNARY_OP: ast_free(node->data.unary_op.expr); break;
        case AST_FUNC_CALL:
            free(node->data.func_call.name);
            for (int i = 0; i < node->data.func_call.arg_count; i++) {
                ast_free(node->data.func_call.args[i]);
            }
            free(node->data.func_call.args);
            break;
        case AST_ARRAY_INDEX:
            ast_free(node->data.array_index.array);
            ast_free(node->data.array_index.index);
            break;
        case AST_FUNC_DECL:
            free(node->data.func_decl.name);
            for (int i = 0; i < node->data.func_decl.param_count; i++) {
                ast_free(node->data.func_decl.params[i]);
            }
            free(node->data.func_decl.params);
            ast_free(node->data.func_decl.body);
            break;
        case AST_VAR_DECL:
            free(node->data.var_decl.name);
            ast_free(node->data.var_decl.init);
            break;
        case AST_EXTRN_DECL: free(node->data.extrn_decl.name); break;
        case AST_IF:
            ast_free(node->data.if_stmt.cond);
            ast_free(node->data.if_stmt.then_body);
            ast_free(node->data.if_stmt.else_body);
            break;
        case AST_WHILE:
            ast_free(node->data.while_stmt.cond);
            ast_free(node->data.while_stmt.body);
            break;
        case AST_RETURN: ast_free(node->data.return_stmt.expr); break;
        case AST_BLOCK:
            for (int i = 0; i < node->data.block.stmt_count; i++) {
                ast_free(node->data.block.stmts[i]);
            }
            free(node->data.block.stmts);
            break;
        case AST_NUMBER: // No heap-allocated members
            break;
    }
    free(node);
}

