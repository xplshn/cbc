#include "ast.h"
#include "util.h"
#include <stdlib.h>
#include <string.h>

// Mem-handling macros for Node allocation
#define ALLOC_NODE(type_enum, line_num)                                        \
  AstNode *node = malloc(sizeof(AstNode));                                     \
  if (!node)                                                                   \
    error(line_num, 0, 0, "Memory allocation failed for AST node");            \
  node->type = type_enum;                                                      \
  node->line = line_num;                                                       \
  node->parent = NULL;

// Constructor Funcs
AstNode *ast_number(long value, int line) {
  ALLOC_NODE(AST_NUMBER, line);
  node->data.number.value = value;
  return node;
}
AstNode *ast_string(char *value, int line) {
  ALLOC_NODE(AST_STRING, line);
  node->data.string.value = value;
  return node;
}
AstNode *ast_ident(char *name, int line) {
  ALLOC_NODE(AST_IDENT, line);
  node->data.ident.name = name;
  return node;
}
AstNode *ast_assign(TokenType op, AstNode *lhs, AstNode *rhs, int line) {
  ALLOC_NODE(AST_ASSIGN, line);
  node->data.assign = (AstAssign){op, lhs, rhs};
  lhs->parent = node;
  rhs->parent = node;
  return node;
}
AstNode *ast_binary_op(TokenType op, AstNode *left, AstNode *right, int line) {
  ALLOC_NODE(AST_BINARY_OP, line);
  node->data.binary_op = (AstBinaryOp){op, left, right};
  left->parent = node;
  right->parent = node;
  return node;
}
AstNode *ast_unary_op(TokenType op, AstNode *expr, int line) {
  ALLOC_NODE(AST_UNARY_OP, line);
  node->data.unary_op = (AstUnaryOp){op, expr};
  expr->parent = node;
  return node;
}
AstNode *ast_postfix_op(TokenType op, AstNode *expr, int line) {
  ALLOC_NODE(AST_POSTFIX_OP, line);
  node->data.postfix_op = (AstPostfixOp){op, expr};
  expr->parent = node;
  return node;
}
AstNode *ast_indirection(AstNode *expr, int line) {
  ALLOC_NODE(AST_INDIRECTION, line);
  node->data.indirection.expr = expr;
  expr->parent = node;
  return node;
}
AstNode *ast_address_of(AstNode *lvalue, int line) {
  ALLOC_NODE(AST_ADDRESS_OF, line);
  node->data.address_of.lvalue = lvalue;
  lvalue->parent = node;
  return node;
}
AstNode *ast_ternary(AstNode *cond, AstNode *then_expr, AstNode *else_expr,
                     int line) {
  ALLOC_NODE(AST_TERNARY, line);
  node->data.ternary = (AstTernary){cond, then_expr, else_expr};
  cond->parent = node;
  then_expr->parent = node;
  else_expr->parent = node;
  return node;
}
AstNode *ast_subscript(AstNode *array, AstNode *index, int line) {
  ALLOC_NODE(AST_SUBSCRIPT, line);
  node->data.subscript = (AstSubscript){array, index};
  array->parent = node;
  index->parent = node;
  return node;
}
AstNode *ast_func_call(AstNode *func_expr, AstNode **args, int arg_count,
                       int line) {
  ALLOC_NODE(AST_FUNC_CALL, line);
  node->data.func_call = (AstFuncCall){func_expr, args, arg_count};
  func_expr->parent = node;
  for (int i = 0; i < arg_count; ++i)
    args[i]->parent = node;
  return node;
}
AstNode *ast_func_decl(char *name, AstNode **params, int param_count,
                       AstNode *body, bool has_varargs, int line) {
  ALLOC_NODE(AST_FUNC_DECL, line);
  node->data.func_decl =
      (AstFuncDecl){name, params, param_count, body, has_varargs};
  if (body)
    body->parent = node;
  for (int i = 0; i < param_count; ++i)
    params[i]->parent = node;
  return node;
}
AstNode *ast_var_decl(char *name, AstNode **init_list, int init_count,
                      AstNode *size_expr, bool is_vector, int line) {
  ALLOC_NODE(AST_VAR_DECL, line);
  node->data.var_decl =
      (AstVarDecl){name, init_list, init_count, size_expr, is_vector};
  for (int i = 0; i < init_count; i++)
    init_list[i]->parent = node;
  if (size_expr)
    size_expr->parent = node;
  return node;
}
AstNode *ast_extrn_decl(char *name, int line) {
  ALLOC_NODE(AST_EXTRN_DECL, line);
  node->data.extrn_decl.name = name;
  return node;
}
AstNode *ast_if(AstNode *cond, AstNode *then_body, AstNode *else_body,
                int line) {
  ALLOC_NODE(AST_IF, line);
  node->data.if_stmt = (AstIf){cond, then_body, else_body};
  cond->parent = node;
  then_body->parent = node;
  if (else_body)
    else_body->parent = node;
  return node;
}
AstNode *ast_while(AstNode *cond, AstNode *body, int line) {
  ALLOC_NODE(AST_WHILE, line);
  node->data.while_stmt = (AstWhile){cond, body};
  cond->parent = node;
  body->parent = node;
  return node;
}
AstNode *ast_return(AstNode *expr, int line) {
  ALLOC_NODE(AST_RETURN, line);
  node->data.return_stmt.expr = expr;
  if (expr)
    expr->parent = node;
  return node;
}
AstNode *ast_block(AstNode **stmts, int stmt_count, int line,
                   bool is_synthetic) {
  ALLOC_NODE(AST_BLOCK, line);
  node->data.block = (AstBlock){stmts, stmt_count, is_synthetic};
  for (int i = 0; i < stmt_count; ++i)
    stmts[i]->parent = node;
  return node;
}
AstNode *ast_goto(char *label, int line) {
  ALLOC_NODE(AST_GOTO, line);
  node->data.goto_stmt.label = label;
  return node;
}
AstNode *ast_switch(AstNode *expr, AstNode *body, int line) {
  ALLOC_NODE(AST_SWITCH, line);
  node->data.switch_stmt = (AstSwitch){expr, body, NULL, 0, NULL};
  expr->parent = node;
  body->parent = node;
  return node;
}
AstNode *ast_case(AstNode *value, AstNode *body, int line) {
  ALLOC_NODE(AST_CASE, line);
  node->data.case_stmt = (AstCase){value, body, NULL};
  value->parent = node;
  body->parent = node;
  return node;
}
AstNode *ast_default(AstNode *body, int line) {
  ALLOC_NODE(AST_DEFAULT, line);
  node->data.default_stmt = (AstDefault){body, NULL};
  body->parent = node;
  return node;
}
AstNode *ast_break(int line) {
  ALLOC_NODE(AST_BREAK, line);
  return node;
}
AstNode *ast_label(char *name, AstNode *stmt, int line) {
  ALLOC_NODE(AST_LABEL, line);
  node->data.label = (AstLabel){name, stmt};
  stmt->parent = node;
  return node;
}
AstNode *ast_asm_stmt(char *code, int line) {
  ALLOC_NODE(AST_ASM_STMT, line);
  node->data.asm_stmt.code = code;
  return node;
}

// Mem mgmt
void ast_free(AstNode *node) {
  if (!node)
    return;

  // Free the children first
  switch (node->type) {
  case AST_ASSIGN:
    ast_free(node->data.assign.lhs);
    ast_free(node->data.assign.rhs);
    break;
  case AST_BINARY_OP:
    ast_free(node->data.binary_op.left);
    ast_free(node->data.binary_op.right);
    break;
  case AST_UNARY_OP:
    ast_free(node->data.unary_op.expr);
    break;
  case AST_POSTFIX_OP:
    ast_free(node->data.postfix_op.expr);
    break;
  case AST_INDIRECTION:
    ast_free(node->data.indirection.expr);
    break;
  case AST_ADDRESS_OF:
    ast_free(node->data.address_of.lvalue);
    break;
  case AST_TERNARY:
    ast_free(node->data.ternary.cond);
    ast_free(node->data.ternary.then_expr);
    ast_free(node->data.ternary.else_expr);
    break;
  case AST_SUBSCRIPT:
    ast_free(node->data.subscript.array);
    ast_free(node->data.subscript.index);
    break;
  case AST_FUNC_CALL:
    ast_free(node->data.func_call.func_expr);
    for (int i = 0; i < node->data.func_call.arg_count; i++)
      ast_free(node->data.func_call.args[i]);
    free(node->data.func_call.args);
    break;
  case AST_FUNC_DECL:
    for (int i = 0; i < node->data.func_decl.param_count; i++)
      ast_free(node->data.func_decl.params[i]);
    free(node->data.func_decl.params);
    ast_free(node->data.func_decl.body);
    break;
  case AST_VAR_DECL:
    for (int i = 0; i < node->data.var_decl.init_count; i++)
      ast_free(node->data.var_decl.init_list[i]);
    free(node->data.var_decl.init_list);
    ast_free(node->data.var_decl.size_expr);
    break;
  case AST_IF:
    ast_free(node->data.if_stmt.cond);
    ast_free(node->data.if_stmt.then_body);
    ast_free(node->data.if_stmt.else_body);
    break;
  case AST_WHILE:
    ast_free(node->data.while_stmt.cond);
    ast_free(node->data.while_stmt.body);
    break;
  case AST_RETURN:
    ast_free(node->data.return_stmt.expr);
    break;
  case AST_BLOCK:
    for (int i = 0; i < node->data.block.stmt_count; i++)
      ast_free(node->data.block.stmts[i]);
    free(node->data.block.stmts);
    break;
  case AST_SWITCH:
    ast_free(node->data.switch_stmt.expr);
    ast_free(node->data.switch_stmt.body);
    for (int i = 0; i < node->data.switch_stmt.case_count; ++i)
      free(node->data.switch_stmt.case_labels[i].label_name);
    free(node->data.switch_stmt.case_labels);
    free(node->data.switch_stmt.default_label_name);
    break;
  case AST_CASE:
    ast_free(node->data.case_stmt.value);
    ast_free(node->data.case_stmt.body);
    free(node->data.case_stmt.qbe_label);
    break;
  case AST_DEFAULT:
    ast_free(node->data.default_stmt.body);
    free(node->data.default_stmt.qbe_label);
    break;
  case AST_LABEL:
    ast_free(node->data.label.stmt);
    break;
  default:
    break; // No children to free
  }

  // Free the node's own data
  switch (node->type) {
  case AST_STRING:
    free(node->data.string.value);
    break;
  case AST_IDENT:
    free(node->data.ident.name);
    break;
  case AST_FUNC_DECL:
    free(node->data.func_decl.name);
    break;
  case AST_VAR_DECL:
    free(node->data.var_decl.name);
    break;
  case AST_EXTRN_DECL:
    free(node->data.extrn_decl.name);
    break;
  case AST_GOTO:
    free(node->data.goto_stmt.label);
    break;
  case AST_LABEL:
    free(node->data.label.name);
    break;
  case AST_ASM_STMT:
    free(node->data.asm_stmt.code);
    break;
  default:
    break; // No dynamic data in the node itself
  }

  free(node);
}

// Constant Folding
AstNode *ast_fold_constants(AstNode *node) {
  if (!node)
    return NULL;

  // Recurse first to fold children
  switch (node->type) {
  case AST_ASSIGN:
    node->data.assign.rhs = ast_fold_constants(node->data.assign.rhs);
    break;
  case AST_BINARY_OP:
    node->data.binary_op.left = ast_fold_constants(node->data.binary_op.left);
    node->data.binary_op.right = ast_fold_constants(node->data.binary_op.right);
    break;
  case AST_UNARY_OP:
    node->data.unary_op.expr = ast_fold_constants(node->data.unary_op.expr);
    break;
  case AST_POSTFIX_OP:
    node->data.postfix_op.expr = ast_fold_constants(node->data.postfix_op.expr);
    break;
  case AST_INDIRECTION:
    node->data.indirection.expr =
        ast_fold_constants(node->data.indirection.expr);
    break;
  case AST_TERNARY:
    node->data.ternary.cond = ast_fold_constants(node->data.ternary.cond);
    node->data.ternary.then_expr =
        ast_fold_constants(node->data.ternary.then_expr);
    node->data.ternary.else_expr =
        ast_fold_constants(node->data.ternary.else_expr);
    break;
  case AST_SUBSCRIPT:
    node->data.subscript.index = ast_fold_constants(node->data.subscript.index);
    break;
  case AST_FUNC_CALL:
    for (int i = 0; i < node->data.func_call.arg_count; i++)
      node->data.func_call.args[i] =
          ast_fold_constants(node->data.func_call.args[i]);
    break;
  case AST_VAR_DECL:
    for (int i = 0; i < node->data.var_decl.init_count; i++)
      node->data.var_decl.init_list[i] =
          ast_fold_constants(node->data.var_decl.init_list[i]);
    node->data.var_decl.size_expr =
        ast_fold_constants(node->data.var_decl.size_expr);
    break;
  case AST_IF:
    node->data.if_stmt.cond = ast_fold_constants(node->data.if_stmt.cond);
    node->data.if_stmt.then_body =
        ast_fold_constants(node->data.if_stmt.then_body);
    node->data.if_stmt.else_body =
        ast_fold_constants(node->data.if_stmt.else_body);
    break;
  case AST_WHILE:
    node->data.while_stmt.cond = ast_fold_constants(node->data.while_stmt.cond);
    node->data.while_stmt.body = ast_fold_constants(node->data.while_stmt.body);
    break;
  case AST_RETURN:
    node->data.return_stmt.expr =
        ast_fold_constants(node->data.return_stmt.expr);
    break;
  case AST_BLOCK:
    for (int i = 0; i < node->data.block.stmt_count; i++)
      node->data.block.stmts[i] = ast_fold_constants(node->data.block.stmts[i]);
    break;
  case AST_SWITCH:
    node->data.switch_stmt.expr =
        ast_fold_constants(node->data.switch_stmt.expr);
    node->data.switch_stmt.body =
        ast_fold_constants(node->data.switch_stmt.body);
    break;
  case AST_CASE:
    node->data.case_stmt.value = ast_fold_constants(node->data.case_stmt.value);
    node->data.case_stmt.body = ast_fold_constants(node->data.case_stmt.body);
    break;
  case AST_DEFAULT:
    node->data.default_stmt.body =
        ast_fold_constants(node->data.default_stmt.body);
    break;
  case AST_LABEL:
    node->data.label.stmt = ast_fold_constants(node->data.label.stmt);
    break;
  default:
    break;
  }

  // Then fold the current node
  if (node->type == AST_BINARY_OP) {
    AstNode *left = node->data.binary_op.left;
    AstNode *right = node->data.binary_op.right;
    if (left->type == AST_NUMBER && right->type == AST_NUMBER) {
      long l = left->data.number.value, r = right->data.number.value, res;
      bool folded = true;
      switch (node->data.binary_op.op) {
      case TOK_PLUS:
        res = l + r;
        break;
      case TOK_MINUS:
        res = l - r;
        break;
      case TOK_STAR:
        res = l * r;
        break;
      case TOK_AND:
        res = l & r;
        break;
      case TOK_OR:
        res = l | r;
        break;
      case TOK_XOR:
        res = l ^ r;
        break;
      case TOK_SHL:
        res = l << r;
        break;
      case TOK_SHR:
        res = l >> r;
        break;
      case TOK_EQEQ:
        res = l == r;
        break;
      case TOK_NEQ:
        res = l != r;
        break;
      case TOK_LT:
        res = l < r;
        break;
      case TOK_GT:
        res = l > r;
        break;
      case TOK_LTE:
        res = l <= r;
        break;
      case TOK_GTE:
        res = l >= r;
        break;
      case TOK_SLASH:
        if (r == 0) {
          error(node->line, 0, 0, "Compile-time division by zero.");
          res = 0;
        } else {
          res = l / r;
        }
        break;
      case TOK_REM:
        if (r == 0) {
          error(node->line, 0, 0, "Compile-time modulo by zero.");
          res = 0;
        } else {
          res = l % r;
        }
        break;
      default:
        folded = false;
        break;
      }
      if (folded) {
        AstNode *new_node = ast_number(res, node->line);
        ast_free(node);
        return new_node;
      }
    }
  } else if (node->type == AST_UNARY_OP) {
    AstNode *expr = node->data.unary_op.expr;
    if (expr->type == AST_NUMBER) {
      long val = expr->data.number.value, res;
      bool folded = true;
      switch (node->data.unary_op.op) {
      case TOK_MINUS:
        res = -val;
        break;
      case TOK_COMPLEMENT:
        res = ~val;
        break;
      case TOK_NOT:
        res = !val;
        break;
      default:
        folded = false;
        break;
      }
      if (folded) {
        AstNode *new_node = ast_number(res, node->line);
        ast_free(node);
        return new_node;
      }
    }
  } else if (node->type == AST_TERNARY) {
    AstNode *cond = node->data.ternary.cond;
    if (cond->type == AST_NUMBER) {
      AstNode *result_node = (cond->data.number.value != 0)
                                 ? node->data.ternary.then_expr
                                 : node->data.ternary.else_expr;
      AstNode *unused_node = (cond->data.number.value != 0)
                                 ? node->data.ternary.else_expr
                                 : node->data.ternary.then_expr;
      // Detach nodes before freeing the ternary node to avoid double-free
      node->data.ternary.cond = NULL;
      node->data.ternary.then_expr = NULL;
      node->data.ternary.else_expr = NULL;
      ast_free(node);
      ast_free(unused_node);
      return result_node;
    }
  }
  return node;
}
