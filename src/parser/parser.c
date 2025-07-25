#include "parser.h"
#include "util.h"
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

// Static Fwd Declarations
static void advance(Parser *parser);
static AstNode *parse_stmt(Parser *parser);
static AstNode *parse_expr(Parser *parser);
static AstNode *parse_assignment_expr(Parser *parser);
static AstNode *parse_ternary_expr(Parser *parser);
static AstNode *parse_global_definition(Parser *parser);
static AstNode *parse_func_decl(Parser *parser);
static AstNode *parse_asm_func_def(Parser *parser);
static void find_cases_recursive(AstNode *node, AstNode *switch_node);
static AstNode *parse_primary_expr(Parser *parser);
static AstNode *parse_binary_expr(Parser *parser, int min_prec);

Parser parser_init(Lexer *lexer) {
  Parser parser = {
      .lexer = lexer,
  };
  parser.current = (Token){.type = TOK_EOF, .value = NULL};
  parser.previous = (Token){.type = TOK_EOF, .value = NULL};
  advance(&parser);
  return parser;
}

void parser_free(Parser *parser) {
  token_free(parser->current);
  token_free(parser->previous);
}

// Parser Helper Functions
static void advance(Parser *parser) {
  token_free(parser->previous);
  parser->previous = parser->current;
  parser->current = lexer_next(parser->lexer);
}

static bool check(Parser *parser, TokenType type) {
  return parser->current.type == type;
}
static Token peek(Parser *parser) { return lexer_peek(parser->lexer); }

static bool match(Parser *parser, TokenType type) {
  if (!check(parser, type))
    return false;
  advance(parser);
  return true;
}

static void expect(Parser *parser, TokenType type, const char *message) {
  if (check(parser, type)) {
    advance(parser);
    return;
  }
  error(parser->current.line, parser->current.column, parser->current.len, "%s",
        message);
}

static bool is_lvalue(AstNode *node) {
  if (!node)
    return false;
  return node->type == AST_IDENT || node->type == AST_INDIRECTION ||
         node->type == AST_SUBSCRIPT;
}

// Expression Parsing
static AstNode *parse_primary_expr(Parser *parser) {
  int line = parser->current.line;
  if (match(parser, TOK_NUMBER)) {
    char *endptr;
    unsigned long long value = strtoull(parser->previous.value, &endptr, 0);
    if (*endptr != '\0') {
      error(line, parser->previous.column, parser->previous.len,
            "Invalid number literal format from lexer.");
    }
    // The warning for overflow is already handled in the lexer.
    // We cast to long, which is what AstNumber holds. The bit pattern is
    // preserved.
    return ast_number((long)value, line);
  }
  if (match(parser, TOK_STRING))
    return ast_string(str_dup(parser->previous.value), line);
  if (match(parser, TOK_IDENT))
    return ast_ident(str_dup(parser->previous.value), line);
  if (match(parser, TOK_LPAREN)) {
    AstNode *expr = parse_expr(parser);
    expect(parser, TOK_RPAREN, "Expected ')' after expression.");
    return expr;
  }
  error(line, parser->current.column, parser->current.len,
        "Expected an expression.");
  return NULL; // Unreachable
}

static AstNode *parse_postfix_expr(Parser *parser) {
  AstNode *expr = parse_primary_expr(parser);
  while (true) {
    int line = parser->current.line;
    if (match(parser, TOK_LPAREN)) {
      AstNode **args = NULL;
      int arg_count = 0, capacity = 0;
      if (!check(parser, TOK_RPAREN)) {
        capacity = 8;
        args = malloc(sizeof(AstNode *) * capacity);
        do {
          if (arg_count >= capacity) {
            capacity *= 2;
            args = realloc(args, sizeof(AstNode *) * capacity);
          }
          args[arg_count++] = parse_assignment_expr(parser);
        } while (match(parser, TOK_COMMA));
      }
      expect(parser, TOK_RPAREN, "Expected ')' after function arguments.");
      expr = ast_func_call(expr, args, arg_count, line);
    } else if (match(parser, TOK_LBRACKET)) {
      AstNode *index = parse_expr(parser);
      expect(parser, TOK_RBRACKET, "Expected ']' after array index.");
      expr = ast_subscript(expr, index, line);
    } else if (match(parser, TOK_INC) || match(parser, TOK_DEC)) {
      if (!is_lvalue(expr))
        error(line, 0, 0, "Postfix '++' or '--' requires an l-value.");
      expr = ast_postfix_op(parser->previous.type, expr, line);
    } else {
      break;
    }
  }
  return expr;
}

static AstNode *parse_unary_expr(Parser *parser) {
  int line = parser->current.line;
  if (match(parser, TOK_NOT) || match(parser, TOK_COMPLEMENT) ||
      match(parser, TOK_MINUS) || match(parser, TOK_PLUS) ||
      match(parser, TOK_INC) || match(parser, TOK_DEC) ||
      match(parser, TOK_STAR) || match(parser, TOK_AND)) {
    TokenType op = parser->previous.type;
    AstNode *operand = parse_unary_expr(parser);

    if (op == TOK_STAR) {
      return ast_indirection(operand, line);
    }
    if (op == TOK_AND) {
      if (!is_lvalue(operand))
        error(line, 0, 0, "Address-of operator '&' requires an l-value.");
      return ast_address_of(operand, line);
    }
    if ((op == TOK_INC || op == TOK_DEC) && !is_lvalue(operand)) {
      error(line, 0, 0, "Prefix '++' or '--' requires an l-value.");
    }
    return ast_unary_op(op, operand, line);
  }
  return parse_postfix_expr(parser);
}

static int get_binary_op_precedence(TokenType op) {
  switch (op) {
  case TOK_STAR:
  case TOK_SLASH:
  case TOK_REM:
    return 12;
  case TOK_PLUS:
  case TOK_MINUS:
    return 11;
  case TOK_SHL:
  case TOK_SHR:
    return 10;
  case TOK_LT:
  case TOK_GT:
  case TOK_LTE:
  case TOK_GTE:
    return 9;
  case TOK_EQEQ:
  case TOK_NEQ:
    return 8;
  case TOK_AND:
    return 7;
  case TOK_XOR:
    return 6;
  case TOK_OR:
    return 5;
  default:
    return -1;
  }
}

static AstNode *parse_binary_expr(Parser *parser, int min_prec) {
  AstNode *left = parse_unary_expr(parser);
  while (true) {
    TokenType op = parser->current.type;
    int prec = get_binary_op_precedence(op);
    if (prec < min_prec)
      break;
    advance(parser);
    AstNode *right = parse_binary_expr(parser, prec + 1);
    left = ast_binary_op(op, left, right, parser->previous.line);
  }
  return left;
}

static AstNode *parse_ternary_expr(Parser *parser) {
  AstNode *cond = parse_binary_expr(parser, 0);
  if (match(parser, TOK_QUESTION)) {
    int line = parser->previous.line;
    AstNode *then_expr = parse_expr(parser);
    expect(parser, TOK_COLON, "Expected ':' for ternary operator.");
    AstNode *else_expr = parse_assignment_expr(parser);
    return ast_ternary(cond, then_expr, else_expr, line);
  }
  return cond;
}

static bool is_assignment_op(TokenType op) {
  return op >= TOK_EQ && op <= TOK_EQ_SHR;
}

static AstNode *parse_assignment_expr(Parser *parser) {
  AstNode *left = parse_ternary_expr(parser);
  if (is_assignment_op(parser->current.type)) {
    if (!is_lvalue(left)) {
      error(parser->current.line, parser->current.column, parser->current.len,
            "Invalid target for assignment.");
    }
    TokenType op = parser->current.type;
    advance(parser);
    AstNode *right = parse_assignment_expr(parser);
    return ast_assign(op, left, right, parser->previous.line);
  }
  return left;
}

static AstNode *parse_expr(Parser *parser) {
  return parse_assignment_expr(parser);
}

// Statement and Declaration Parsing
static AstNode *parse_block_stmt(Parser *parser) {
  int line = parser->current.line;
  expect(parser, TOK_LBRACE, "Expected '{' to start a block.");
  AstNode **stmts = NULL;
  int count = 0, capacity = 0;
  while (!check(parser, TOK_RBRACE) && !check(parser, TOK_EOF)) {
    if (count >= capacity) {
      capacity = capacity == 0 ? 8 : capacity * 2;
      stmts = realloc(stmts, sizeof(AstNode *) * capacity);
    }
    stmts[count++] = parse_stmt(parser);
  }
  expect(parser, TOK_RBRACE, "Expected '}' after block.");
  return ast_block(stmts, count, line, false);
}

static AstNode *parse_declaration_list(Parser *parser) {
  TokenType decl_type = parser->previous.type;
  int line = parser->previous.line;
  AstNode **decls = NULL;
  int count = 0, capacity = 0;
  do {
    if (count >= capacity) {
      capacity = capacity == 0 ? 4 : capacity * 2;
      decls = realloc(decls, sizeof(AstNode *) * capacity);
    }
    expect(parser, TOK_IDENT, "Expected identifier in declaration.");
    char *name = str_dup(parser->previous.value);
    int item_line = parser->previous.line;
    if (decl_type == TOK_EXTRN) {
      if (!is_feature_enabled(FEAT_EXTRN)) {
        error(item_line, 0, 0,
              "'extrn' is forbidden by the current feature set (-Fno-extrn).");
      }
      decls[count++] = ast_extrn_decl(name, item_line);
    } else { // TOK_AUTO
      AstNode *size_expr = NULL;
      bool is_vector = false;
      if (match(parser, TOK_LBRACKET)) {
        is_vector = true;
        if (!check(parser, TOK_RBRACKET)) {
          size_expr = parse_expr(parser);
          AstNode *folded_size = ast_fold_constants(size_expr);
          if (folded_size->type != AST_NUMBER ||
              folded_size->data.number.value <= 0) {
            error(item_line, parser->current.column, parser->current.len,
                  "Vector size must be a positive constant integer.");
          }
          if (folded_size != size_expr)
            ast_free(folded_size);
        }
        expect(parser, TOK_RBRACKET, "Expected ']' after array size.");
      } else if (check(parser, TOK_NUMBER)) {
        is_vector = true;
        size_expr =
            ast_number(atol(parser->current.value), parser->current.line);
        if (size_expr->data.number.value <= 0) {
          error(item_line, parser->current.column, parser->current.len,
                "Vector size must be a positive constant integer.");
        }
        advance(parser);
      }
      AstNode **init_list = NULL;
      int init_count = 0;
      if (match(parser, TOK_EQ)) {
        init_list = malloc(sizeof(AstNode *));
        init_list[0] = parse_assignment_expr(parser);
        init_count = 1;
      }
      decls[count++] = ast_var_decl(name, init_list, init_count, size_expr,
                                    is_vector, item_line);
    }
  } while (match(parser, TOK_COMMA));
  expect(parser, TOK_SEMI, "Expected ';' after declaration list.");
  return ast_block(decls, count, line, true);
}

static void find_cases_recursive(AstNode *node, AstNode *switch_node) {
  if (!node)
    return;
  if (node->type == AST_CASE) {
    AstNode *folded_value = ast_fold_constants(node->data.case_stmt.value);
    if (folded_value->type != AST_NUMBER) {
      error(node->line, 0, 0, "Case value must be a constant integer.");
    }
    node->data.case_stmt.value = folded_value;
    int case_count = switch_node->data.switch_stmt.case_count;
    switch_node->data.switch_stmt.case_labels =
        realloc(switch_node->data.switch_stmt.case_labels,
                sizeof(AstCaseLabel) * (case_count + 1));
    long case_val = folded_value->data.number.value;
    char *label_name = malloc(32);
    snprintf(label_name, 32, "@case_%ld_%d", case_val, node->line);
    // Sanitize label name
    for (char *p = label_name; *p; p++) {
      if (*p == '-')
        *p = '_';
    }
    switch_node->data.switch_stmt.case_labels[case_count] =
        (AstCaseLabel){.value = case_val, .label_name = str_dup(label_name)};
    switch_node->data.switch_stmt.case_count++;
    node->data.case_stmt.qbe_label = str_dup(label_name); // Ensure unique copy
    free(label_name);
  } else if (node->type == AST_DEFAULT) {
    if (switch_node->data.switch_stmt.default_label_name) {
      error(node->line, 0, 0,
            "Multiple 'default' labels in one switch statement.");
    }
    char *label_name = malloc(32);
    snprintf(label_name, 32, "@default_%d", node->line);
    switch_node->data.switch_stmt.default_label_name = str_dup(label_name);
    node->data.default_stmt.qbe_label =
        str_dup(label_name); // Ensure unique copy
    free(label_name);
  } else if (node->type == AST_SWITCH) {
    return;
  }
  switch (node->type) {
  case AST_IF:
    find_cases_recursive(node->data.if_stmt.then_body, switch_node);
    find_cases_recursive(node->data.if_stmt.else_body, switch_node);
    break;
  case AST_WHILE:
    find_cases_recursive(node->data.while_stmt.body, switch_node);
    break;
  case AST_BLOCK:
    for (int i = 0; i < node->data.block.stmt_count; i++)
      find_cases_recursive(node->data.block.stmts[i], switch_node);
    break;
  case AST_LABEL:
    find_cases_recursive(node->data.label.stmt, switch_node);
    break;
  case AST_CASE:
    find_cases_recursive(node->data.case_stmt.body, switch_node);
    break;
  case AST_DEFAULT:
    find_cases_recursive(node->data.default_stmt.body, switch_node);
    break;
  default:
    break;
  }
}

static void build_switch_jump_table(AstNode *switch_node) {
  if (!switch_node || switch_node->type != AST_SWITCH)
    return;
  switch_node->data.switch_stmt.case_labels = NULL;
  switch_node->data.switch_stmt.case_count = 0;
  switch_node->data.switch_stmt.default_label_name = NULL;
  find_cases_recursive(switch_node->data.switch_stmt.body, switch_node);
}

static char *parse_asm_block(Parser *parser) {
  if (!is_feature_enabled(FEAT_ASM)) {
    error(parser->current.line, 0, 0,
          "'__asm__' is forbidden by the current feature set (-Fno-asm).");
  }
  expect(parser, TOK_LPAREN, "Expected '(' after '__asm__'.");
  size_t total_len = 0, capacity = 1024;
  char *code = malloc(capacity);
  code[0] = '\0';
  bool first = true;
  while (!check(parser, TOK_RPAREN) && !check(parser, TOK_EOF)) {
    if (!first)
      match(parser, TOK_COMMA);
    expect(parser, TOK_STRING, "Expected string literal in '__asm__' block.");
    const char *part = parser->previous.value;
    size_t part_len = strlen(part), sep_len = first ? 0 : 1;
    if (total_len + part_len + sep_len + 1 > capacity) {
      capacity = (total_len + part_len + sep_len + 1) * 2;
      code = realloc(code, capacity);
    }
    if (!first)
      strcat(code, "\n");
    strcat(code, part);
    total_len += part_len + sep_len;
    first = false;
  }
  expect(parser, TOK_RPAREN, "Expected ')' to close '__asm__' block.");
  return code;
}

static AstNode *parse_stmt(Parser *parser) {
  int line = parser->current.line;
  if (check(parser, TOK_RBRACE) || check(parser, TOK_EOF)) {
    return ast_block(NULL, 0, line, true); // Null statement
  }
  if (check(parser, TOK_IDENT) && peek(parser).type == TOK_COLON) {
    char *label_name = str_dup(parser->current.value);
    advance(parser);
    advance(parser);
    if (check(parser, TOK_RBRACE) || check(parser, TOK_EOF)) {
      return ast_label(label_name, ast_block(NULL, 0, line, true), line);
    }
    return ast_label(label_name, parse_stmt(parser), line);
  }
  if (match(parser, TOK_IF)) {
    expect(parser, TOK_LPAREN, "Expected '(' after 'if'.");
    AstNode *cond = parse_expr(parser);
    expect(parser, TOK_RPAREN, "Expected ')' after if condition.");
    AstNode *then_body = parse_stmt(parser);
    AstNode *else_body = match(parser, TOK_ELSE) ? parse_stmt(parser) : NULL;
    return ast_if(cond, then_body, else_body, line);
  }
  if (match(parser, TOK_WHILE)) {
    expect(parser, TOK_LPAREN, "Expected '(' after 'while'.");
    AstNode *cond = parse_expr(parser);
    expect(parser, TOK_RPAREN, "Expected ')' after while condition.");
    return ast_while(cond, parse_stmt(parser), line);
  }
  if (match(parser, TOK_SWITCH)) {
    bool has_paren = match(parser, TOK_LPAREN);
    AstNode *expr = parse_expr(parser);
    if (has_paren)
      expect(parser, TOK_RPAREN, "Expected ')' after switch expression.");
    AstNode *body = parse_stmt(parser);
    AstNode *switch_node = ast_switch(expr, body, line);
    build_switch_jump_table(switch_node);
    return switch_node;
  }
  if (check(parser, TOK_LBRACE))
    return parse_block_stmt(parser);
  if (match(parser, TOK_AUTO))
    return parse_declaration_list(parser);
  if (match(parser, TOK_EXTRN))
    return parse_declaration_list(parser);
  if (match(parser, TOK_CASE)) {
    AstNode *value = parse_expr(parser);
    expect(parser, TOK_COLON, "Expected ':' after case value.");
    return ast_case(value, parse_stmt(parser), line);
  }
  if (match(parser, TOK_DEFAULT)) {
    expect(parser, TOK_COLON, "Expected ':' after 'default'.");
    return ast_default(parse_stmt(parser), line);
  }
  if (match(parser, TOK_GOTO)) {
    expect(parser, TOK_IDENT, "Expected label name after 'goto'.");
    AstNode *node = ast_goto(str_dup(parser->previous.value), line);
    expect(parser, TOK_SEMI, "Expected ';' after goto statement.");
    return node;
  }
  if (match(parser, TOK_RETURN)) {
    AstNode *expr = check(parser, TOK_SEMI) ? NULL : parse_expr(parser);
    expect(parser, TOK_SEMI, "Expected ';' after return statement.");
    return ast_return(expr, line);
  }
  if (match(parser, TOK_BREAK)) {
    expect(parser, TOK_SEMI, "Expected ';' after 'break'.");
    return ast_break(line);
  }
  if (match(parser, TOK_SEMI))
    return ast_block(NULL, 0, line, true); // Null statement
  AstNode *expr = parse_expr(parser);
  expect(parser, TOK_SEMI, "Expected ';' after expression statement.");
  return expr;
}

static AstNode *parse_asm_func_def(Parser *parser) {
  int line = parser->current.line;
  expect(parser, TOK_IDENT, "Expected identifier for asm function definition.");
  char *name = str_dup(parser->previous.value);
  if (!is_feature_enabled(FEAT_ASM)) {
    error(parser->current.line, 0, 0,
          "'__asm__' is forbidden by the current feature set (-Fno-asm).");
  }
  expect(parser, TOK___ASM__, "Expected '__asm__' keyword.");
  char *asm_code = parse_asm_block(parser);
  AstNode *body = ast_asm_stmt(asm_code, line);
  if (!check(parser, TOK_LBRACE)) {
    expect(parser, TOK_SEMI, "Expected ';' or '{' after '__asm__' definition.");
  }
  return ast_func_decl(name, NULL, 0, body, false, line);
}

static AstNode *parse_func_decl(Parser *parser) {
  expect(parser, TOK_IDENT, "Expected function name.");
  char *name = str_dup(parser->previous.value);
  int fn_line = parser->previous.line;
  expect(parser, TOK_LPAREN, "Expected '(' after function name.");
  AstNode **params = NULL;
  int param_count = 0, param_cap = 0;
  bool has_varargs = false;
  if (!check(parser, TOK_RPAREN)) {
    param_cap = 8;
    params = malloc(sizeof(AstNode *) * param_cap);
    do {
      if (match(parser, TOK_DOTS)) {
        has_varargs = true;
        break;
      }
      expect(parser, TOK_IDENT, "Expected parameter name or '...'.");
      if (param_count >= param_cap) {
        param_cap *= 2;
        params = realloc(params, sizeof(AstNode *) * param_cap);
      }
      params[param_count++] =
          ast_ident(str_dup(parser->previous.value), parser->previous.line);
    } while (match(parser, TOK_COMMA));
  }
  expect(parser, TOK_RPAREN, "Expected ')' after parameters.");

  AstNode **decl_stmts = NULL;
  int decl_count = 0;
  while (match(parser, TOK_AUTO) || match(parser, TOK_EXTRN)) {
    AstNode *decl_block = parse_declaration_list(parser);
    decl_stmts = realloc(decl_stmts,
                         sizeof(AstNode *) *
                             (decl_count + decl_block->data.block.stmt_count));
    for (int i = 0; i < decl_block->data.block.stmt_count; ++i) {
      decl_stmts[decl_count++] = decl_block->data.block.stmts[i];
    }
    free(decl_block->data.block.stmts);
    free(decl_block);
  }

  AstNode *body = parse_stmt(parser);

  if (decl_count > 0) {
    if (body->type == AST_BLOCK && !body->data.block.is_synthetic) {
      int old_count = body->data.block.stmt_count;
      body->data.block.stmts = realloc(
          body->data.block.stmts, sizeof(AstNode *) * (decl_count + old_count));
      memmove(&body->data.block.stmts[decl_count], &body->data.block.stmts[0],
              old_count * sizeof(AstNode *));
      memcpy(&body->data.block.stmts[0], decl_stmts,
             decl_count * sizeof(AstNode *));
      body->data.block.stmt_count += decl_count;
      free(decl_stmts);
    } else {
      int body_count =
          (body->type == AST_BLOCK && body->data.block.stmt_count == 0 &&
           body->data.block.stmts == NULL)
              ? 0
              : 1;
      AstNode **new_stmts =
          malloc(sizeof(AstNode *) * (decl_count + body_count));
      memcpy(new_stmts, decl_stmts, decl_count * sizeof(AstNode *));
      if (body_count > 0)
        new_stmts[decl_count] = body;
      free(decl_stmts);
      body = ast_block(new_stmts, decl_count + body_count, body->line, false);
    }
  }

  return ast_func_decl(name, params, param_count, body, has_varargs, fn_line);
}

static AstNode *parse_global_definition(Parser *parser) {
  expect(parser, TOK_IDENT, "Expected identifier for global definition.");
  char *name = str_dup(parser->previous.value);
  int line = parser->previous.line;
  AstNode *size_expr = NULL;
  bool is_vector = false;

  if (match(parser, TOK_LBRACKET)) {
    is_vector = true;
    if (!check(parser, TOK_RBRACKET)) {
      size_expr = parse_expr(parser);
      // handle the pointer returned by the folding function.
      size_expr = ast_fold_constants(size_expr);
      if (size_expr->type != AST_NUMBER || size_expr->data.number.value <= 0) {
        error(line, parser->current.column, parser->current.len,
              "Vector size must be a positive constant integer.");
      }
    }
    expect(parser, TOK_RBRACKET, "Expected ']' for vector definition.");
  }

  AstNode **init_list = NULL;
  int init_count = 0;
  int capacity = 0;

  if (!check(parser, TOK_SEMI)) {
    capacity = 8;
    init_list = malloc(sizeof(AstNode *) * capacity);
    do {
      if (init_count >= capacity) {
        capacity *= 2;
        init_list = realloc(init_list, sizeof(AstNode *) * capacity);
      }
      init_list[init_count++] = parse_assignment_expr(parser);
    } while (match(parser, TOK_COMMA) && !check(parser, TOK_SEMI));

    if (init_count > 1) {
      is_vector = true;
    }
  }

  expect(parser, TOK_SEMI, "Expected ';' after global definition.");
  return ast_var_decl(name, init_list, init_count, size_expr, is_vector, line);
}

AstNode *parser_parse(Parser *parser) {
  AstNode **stmts = NULL;
  int stmt_count = 0, capacity = 0;
  int line = parser->current.line;
  while (!check(parser, TOK_EOF)) {
    if (stmt_count >= capacity) {
      capacity = capacity == 0 ? 16 : capacity * 2;
      stmts = realloc(stmts, sizeof(AstNode *) * capacity);
    }
    if (check(parser, TOK_IDENT) && peek(parser).type == TOK___ASM__) {
      stmts[stmt_count++] = parse_asm_func_def(parser);
    } else if (check(parser, TOK_IDENT) && peek(parser).type == TOK_LPAREN) {
      stmts[stmt_count++] = parse_func_decl(parser);
    } else if (check(parser, TOK_IDENT)) {
      stmts[stmt_count++] = parse_global_definition(parser);
    } else {
      stmts[stmt_count++] = parse_stmt(parser);
    }
  }
  return ast_block(stmts, stmt_count, line, true);
}
