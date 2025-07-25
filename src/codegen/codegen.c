#include "codegen.h"
#include "ast.h"
#include "lexer.h"
#include "util.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Context and Symbol Table strucs
typedef struct {
  char *value;
  char *label;
} StringEntry;
typedef struct {
  StringEntry *entries;
  int count;
  int capacity;
} StringTable;
typedef enum { SYM_VAR, SYM_FUNC, SYM_LABEL } SymbolType;
typedef struct Symbol {
  char *name;
  SymbolType type;
  char *qbe_name;
  bool is_vector;
  long stack_offset; // For auto variables
  struct Symbol *next;
} Symbol;
typedef struct Scope {
  Symbol *symbols;
  struct Scope *parent;
} Scope;

// New structure to hold pre-calculated layout for auto variables
typedef struct AutoVarInfo {
  char *name;
  long offset;
  bool is_vector;
  struct AutoVarInfo *next;
} AutoVarInfo;

typedef struct AutoVarList {
  AstNode **vars;
  int count;
  int capacity;
} AutoVarList;

typedef struct CodegenContext {
  FILE *out;
  FILE *asm_out;
  StringTable strings;
  int temp_count;
  int label_count;
  Scope *current_scope;
  char *current_function_name;
  char *current_function_frame; // Base pointer for auto variables
  AutoVarInfo *auto_var_info;   // Map for auto var offsets
  char *break_label;
  char word_type; // 'l' for 64-bit, 'w' for 32-bit
  unsigned short int word_size;
} CodegenContext;

// Fwd Dec.
static int codegen_expr(CodegenContext *ctx, AstNode *node, char **result,
                        char *result_type);
static int codegen_lvalue(CodegenContext *ctx, AstNode *node,
                          char **result_addr);
static int codegen_stmt(CodegenContext *ctx, AstNode *node);
static void enter_scope(CodegenContext *ctx);
static void exit_scope(CodegenContext *ctx);
static void collect_strings_recursive(CodegenContext *ctx, AstNode *node);
static void find_all_autos_in_func(AstNode *node, AutoVarList *list);
static bool is_string_derived_expr(AstNode *node);

// String mgt table
static void init_string_table(StringTable *table) {
  table->entries = malloc(sizeof(StringEntry) * 10);
  table->capacity = 10;
  table->count = 0;
}

static void free_string_table(StringTable *table) {
  for (int i = 0; i < table->count; i++) {
    free(table->entries[i].value);
    free(table->entries[i].label);
  }
  free(table->entries);
}

static char *add_string(CodegenContext *ctx, const char *value) {
  for (int i = 0; i < ctx->strings.count; i++) {
    if (strcmp(ctx->strings.entries[i].value, value) == 0) {
      return str_dup(ctx->strings.entries[i].label);
    }
  }
  if (ctx->strings.count >= ctx->strings.capacity) {
    ctx->strings.capacity *= 2;
    ctx->strings.entries = realloc(ctx->strings.entries,
                                   sizeof(StringEntry) * ctx->strings.capacity);
  }
  char *label = malloc(16);
  snprintf(label, 16, "str%d", ctx->strings.count);
  ctx->strings.entries[ctx->strings.count].value = str_dup(value);
  ctx->strings.entries[ctx->strings.count].label = str_dup(label);
  ctx->strings.count++;
  return label;
}

static void emit_data_section(CodegenContext *ctx) {
  if (ctx->strings.count == 0)
    return;
  fprintf(ctx->out, "\n# --- Data Section ---\n");
  for (int i = 0; i < ctx->strings.count; i++) {
    fprintf(ctx->out, "data $%s = { ", ctx->strings.entries[i].label);
    const unsigned char *p =
        (const unsigned char *)ctx->strings.entries[i].value;
    bool first_item = true;
    if (*p == '\0') {
      fprintf(ctx->out, "b 0 }\n");
      continue;
    }

    while (*p) {
      if (!first_item)
        fprintf(ctx->out, ", ");
      const unsigned char *start = p;
      while (*p >= 32 && *p <= 126 && *p != '"' && *p != '\\')
        p++;
      if (p > start) {
        fprintf(ctx->out, "b \"%.*s\"", (int)(p - start), start);
      } else {
        fprintf(ctx->out, "b %d", *p);
        p++;
      }
      first_item = false;
    }
    if (!first_item)
      fprintf(ctx->out, ", ");
    fprintf(ctx->out, "b 0 }\n");
  }
  fprintf(ctx->out, "\n");
}

// Symbol Table and Scope Management
static Scope *scope_create(Scope *parent) {
  Scope *scope = malloc(sizeof(Scope));
  scope->symbols = NULL;
  scope->parent = parent;
  return scope;
}

static void scope_free(Scope *scope) {
  if (!scope)
    return;
  Symbol *sym = scope->symbols;
  while (sym) {
    Symbol *next = sym->next;
    free(sym->name);
    free(sym->qbe_name);
    free(sym);
    sym = next;
  }
  free(scope);
}

static void enter_scope(CodegenContext *ctx) {
  ctx->current_scope = scope_create(ctx->current_scope);
}

static void exit_scope(CodegenContext *ctx) {
  Scope *scope_to_free = ctx->current_scope;
  ctx->current_scope = ctx->current_scope->parent;
  scope_free(scope_to_free);
}

static Symbol *scope_find_symbol(Scope *scope, const char *name) {
  if (!scope || !name)
    return NULL;
  for (Scope *s = scope; s; s = s->parent) {
    for (Symbol *sym = s->symbols; sym; sym = sym->next) {
      if (strcmp(sym->name, name) == 0) {
        return sym;
      }
    }
  }
  return NULL;
}

static Symbol *scope_find_symbol_local(Scope *scope, const char *name) {
  if (!scope || !name)
    return NULL;
  for (Symbol *sym = scope->symbols; sym; sym = sym->next) {
    if (strcmp(sym->name, name) == 0) {
      return sym;
    }
  }
  return NULL;
}

static Symbol *scope_add_symbol(CodegenContext *ctx, const char *name,
                                SymbolType type, bool is_vector, int line) {
  Symbol *existing = scope_find_symbol_local(ctx->current_scope, name);
  if (existing) {
    if (type == SYM_VAR && existing->type == SYM_FUNC &&
        ctx->current_scope->parent == NULL) {
      existing->type = SYM_VAR;
      existing->is_vector = is_vector;
      free(existing->qbe_name);
      existing->qbe_name = malloc(strlen(name) + 32);
      sprintf(existing->qbe_name, "$%s", name);
      return existing;
    }
    if (type == SYM_FUNC && existing->type == SYM_FUNC) {
      return existing;
    }
    error(line, 0, 0, "Redefinition of '%s' in the same scope.", name);
  }

  Symbol *new_sym = malloc(sizeof(Symbol));
  new_sym->name = str_dup(name);
  new_sym->type = type;
  new_sym->is_vector = is_vector;
  new_sym->stack_offset = -1;
  new_sym->qbe_name = malloc(strlen(name) + 32);
  switch (type) {
  case SYM_VAR:
    if (ctx->current_scope->parent == NULL) { // Global scope
      sprintf(new_sym->qbe_name, "$%s", name);
    } else { // Local scope - generate unique name for QBE temp
      sprintf(new_sym->qbe_name, "%%.%s_%d", name, ctx->temp_count++);
    }
    break;
  case SYM_FUNC:
    sprintf(new_sym->qbe_name, "$%s", name);
    break;
  case SYM_LABEL:
    sprintf(new_sym->qbe_name, "@%s", name);
    break;
  }

  new_sym->next = ctx->current_scope->symbols;
  ctx->current_scope->symbols = new_sym;
  return new_sym;
}

// QBE helpers
static char *new_temp(CodegenContext *ctx) {
  char *temp = malloc(16);
  snprintf(temp, 16, "%%.t%d", ctx->temp_count++);
  return temp;
}

static char *new_label(CodegenContext *ctx) {
  char *label = malloc(16);
  snprintf(label, 16, "@L%d", ctx->label_count++);
  return label;
}

static const char *get_op_str(TokenType op) {
  switch (op) {
  case TOK_PLUS:
  case TOK_PLUS_EQ:
  case TOK_EQ_PLUS:
    return "add";
  case TOK_MINUS:
  case TOK_MINUS_EQ:
  case TOK_EQ_MINUS:
    return "sub";
  case TOK_STAR:
  case TOK_STAR_EQ:
  case TOK_EQ_STAR:
    return "mul";
  case TOK_SLASH:
  case TOK_SLASH_EQ:
  case TOK_EQ_SLASH:
    return "div";
  case TOK_REM:
  case TOK_REM_EQ:
  case TOK_EQ_REM:
    return "rem";
  case TOK_AND:
  case TOK_AND_EQ:
  case TOK_EQ_AND:
    return "and";
  case TOK_OR:
  case TOK_OR_EQ:
  case TOK_EQ_OR:
    return "or";
  case TOK_XOR:
  case TOK_XOR_EQ:
  case TOK_EQ_XOR:
    return "xor";
  case TOK_SHL:
  case TOK_SHL_EQ:
  case TOK_EQ_SHL:
    return "shl";
  case TOK_SHR:
  case TOK_SHR_EQ:
  case TOK_EQ_SHR:
    return "sar";
  default:
    return NULL;
  }
}

static const char *get_cmp_op_str(TokenType op, char type) {
  switch (op) {
  case TOK_EQEQ:
    return (type == 'l') ? "ceql" : "ceqw";
  case TOK_NEQ:
    return (type == 'l') ? "cnel" : "cnew";
  case TOK_LT:
    return (type == 'l') ? "csltl" : "csltw";
  case TOK_GT:
    return (type == 'l') ? "csgtl" : "csgtw";
  case TOK_LTE:
    return (type == 'l') ? "cslel" : "cslew";
  case TOK_GTE:
    return (type == 'l') ? "csgel" : "csgew";
  default:
    return NULL;
  }
}

// === Codegen Implementation ===
// : --------------------------- :
// ==============================

// Recursively checks if an AST node represents an address derived from a string
// literal. This is a heuristic to guess if a pointer is a `char*`.
static bool is_string_derived_expr(AstNode *node) {
  if (!node) {
    return false;
  }
  if (node->type == AST_STRING) {
    return true;
  }
  if (node->type == AST_BINARY_OP && node->data.binary_op.op == TOK_PLUS) {
    // If either side of a '+' is string-derived, the result is too
    return is_string_derived_expr(node->data.binary_op.left) ||
           is_string_derived_expr(node->data.binary_op.right);
  }
  return false;
}

static int codegen_lvalue(CodegenContext *ctx, AstNode *node,
                          char **result_addr) {
  if (!node)
    error(0, 0, 0, "Null l-value node in codegen");
  switch (node->type) {
  case AST_IDENT: {
    Symbol *sym = scope_find_symbol(ctx->current_scope, node->data.ident.name);
    if (!sym) {
      // Allow forward-declared functions to be treated as variables before
      // definition.
      sym = scope_add_symbol(ctx, node->data.ident.name, SYM_VAR, false,
                             node->line);
    }
    if (sym->type == SYM_FUNC &&
        !scope_find_symbol_local(ctx->current_scope, node->data.ident.name)) {
      error(node->line, 0, 0, "Cannot use function name '%s' as an l-value.",
            node->data.ident.name);
    }
    *result_addr = str_dup(sym->qbe_name);
    break;
  }
  case AST_INDIRECTION: {
    char expr_type;
    return codegen_expr(ctx, node->data.indirection.expr, result_addr,
                        &expr_type);
  }
  case AST_SUBSCRIPT: {
    AstNode *index_node = node->data.subscript.index;

    if (index_node->type == AST_POSTFIX_OP) {
      // Special handling for v[i++] or v[i--] to sequence the side effect
      // correctly.
      // 1. Get the address of the array base.
      char *array_addr;
      char array_type;
      codegen_expr(ctx, node->data.subscript.array, &array_addr, &array_type);

      // 2. Get the address of the variable being incremented/decremented (e.g.,
      // 'i').
      char *var_lval;
      codegen_lvalue(ctx, index_node->data.postfix_op.expr, &var_lval);

      // 3. Load the *current* value of 'i' to use as the index.
      char *index_val = new_temp(ctx);
      fprintf(ctx->out, "\t%s =%c load%c %s\n", index_val, ctx->word_type,
              ctx->word_type, var_lval);

      // 4. Calculate the final address of the element: array_base + (index_val
      // * word_size). This is the "use it" part.
      char *scaled_index = new_temp(ctx);
      fprintf(ctx->out, "\t%s =%c mul %s, %hu\n", scaled_index, ctx->word_type,
              index_val, ctx->word_size);
      *result_addr = new_temp(ctx);
      fprintf(ctx->out, "\t%s =%c add %s, %s\n", *result_addr, ctx->word_type,
              array_addr, scaled_index);

      // 5. NOW, after the address has been calculated and used, perform the
      // increment/decrement.
      char *new_var_val = new_temp(ctx);
      const char *op =
          (index_node->data.postfix_op.op == TOK_INC) ? "add" : "sub";
      fprintf(ctx->out, "\t%s =%c %s %s, 1\n", new_var_val, ctx->word_type, op,
              index_val);
      fprintf(ctx->out, "\tstore%c %s, %s\n", ctx->word_type, new_var_val,
              var_lval);

      free(array_addr);
      free(var_lval);
      free(index_val);
      free(scaled_index);
      free(new_var_val);
    } else {
      // Behavior for simple cases like v[i]
      char *array_addr;
      char *index_val;
      char array_type, index_type;

      codegen_expr(ctx, node->data.subscript.array, &array_addr, &array_type);
      codegen_expr(ctx, node->data.subscript.index, &index_val, &index_type);

      char *scaled_index = new_temp(ctx);
      fprintf(ctx->out, "\t%s =%c mul %s, %hu\n", scaled_index, ctx->word_type,
              index_val, ctx->word_size);

      *result_addr = new_temp(ctx);
      fprintf(ctx->out, "\t%s =%c add %s, %s\n", *result_addr, ctx->word_type,
              array_addr, scaled_index);

      free(array_addr);
      free(index_val);
      free(scaled_index);
    }
    break;
  }
  default:
    error(node->line, 0, 0, "Expression is not a valid l-value.");
    return 0;
  }
  return 0;
}

static int codegen_expr(CodegenContext *ctx, AstNode *node, char **result,
                        char *result_type) {
  if (!node)
    error(0, 0, 0, "Null expression node in codegen");
  *result_type = ctx->word_type;
  int terminates = 0;

  switch (node->type) {
  case AST_NUMBER:
    *result = malloc(32);
    snprintf(*result, 32, "%ld", node->data.number.value);
    break;
  case AST_STRING: {
    char *label = add_string(ctx, node->data.string.value);
    *result = malloc(strlen(label) + 2);
    snprintf(*result, strlen(label) + 2, "$%s", label);
    free(label);
    break;
  }
  case AST_IDENT: {
    Symbol *sym = scope_find_symbol(ctx->current_scope, node->data.ident.name);
    if (!sym) {
      // Implicitly declare as a function if not found.
      sym = scope_add_symbol(ctx, node->data.ident.name, SYM_FUNC, false,
                             node->line);
    }

    // An identifier's name evaluates to an address (a pointer) if it's a
    // function or a vector. Otherwise, it's a scalar variable, and its value
    // should be loaded. The context of a function call is special: `f` in `f()`
    // should be an address.
    bool is_function_name_in_call =
        (node->parent && node->parent->type == AST_FUNC_CALL &&
         node == node->parent->data.func_call.func_expr);

    if (sym->is_vector || (sym->type == SYM_FUNC && is_function_name_in_call)) {
      // Array names and function names used in calls decay to pointers
      // (addresses).
      *result = str_dup(sym->qbe_name);
    } else {
      // All other identifiers (scalars, or functions not in a call context like
      // `n`) are treated as variables whose values should be loaded.
      char *lvalue_addr;
      codegen_lvalue(ctx, node, &lvalue_addr);
      *result = new_temp(ctx);
      fprintf(ctx->out, "\t%s =%c load%c %s\n", *result, ctx->word_type,
              ctx->word_type, lvalue_addr);
      free(lvalue_addr);
    }
    break;
  }
  case AST_INDIRECTION: {
    AstNode *operand = node->data.indirection.expr;
    char *addr;
    char addr_type;

    // Recursively check if the address expression originates from a string
    // literal.
    bool is_char_ptr = is_string_derived_expr(operand);

    terminates = codegen_expr(ctx, operand, &addr, &addr_type);
    *result = new_temp(ctx);

    if (is_char_ptr) {
      // It's a character pointer. Load a single signed byte and extend it.
      fprintf(ctx->out, "\t%s =%c loadsb %s\n", *result, ctx->word_type, addr);
    } else {
      // Default behavior: assume it's a word pointer and load a full word.
      fprintf(ctx->out, "\t%s =%c load%c %s\n", *result, ctx->word_type,
              ctx->word_type, addr);
    }
    free(addr);
    break;
  }
  case AST_ADDRESS_OF: {
    AstNode *lval_node = node->data.address_of.lvalue;
    if (lval_node->type == AST_IDENT) {
      Symbol *sym =
          scope_find_symbol(ctx->current_scope, lval_node->data.ident.name);
      if (!sym) {
        sym = scope_add_symbol(ctx, lval_node->data.ident.name, SYM_FUNC, false,
                               lval_node->line);
      }
      if (sym->type == SYM_FUNC) {
        *result = str_dup(sym->qbe_name);
        return 0;
      }
    }
    return codegen_lvalue(ctx, lval_node, result);
  }
  case AST_SUBSCRIPT: {
    char *addr;
    terminates = codegen_lvalue(ctx, node, &addr);
    *result = new_temp(ctx);
    fprintf(ctx->out, "\t%s =%c load%c %s\n", *result, ctx->word_type,
            ctx->word_type, addr);
    free(addr);
    break;
  }
  case AST_ASSIGN: {
    char *lvalue_addr;
    codegen_lvalue(ctx, node->data.assign.lhs, &lvalue_addr);

    char *rvalue;
    char rvalue_type;
    if (node->data.assign.op != TOK_EQ) {
      char *current_lvalue_val = new_temp(ctx);
      fprintf(ctx->out, "\t%s =%c load%c %s\n", current_lvalue_val,
              ctx->word_type, ctx->word_type, lvalue_addr);

      char *rhs_val;
      char rhs_type;
      codegen_expr(ctx, node->data.assign.rhs, &rhs_val, &rhs_type);
      const char *op_str = get_op_str(node->data.assign.op);
      rvalue = new_temp(ctx);
      fprintf(ctx->out, "\t%s =%c %s %s, %s\n", rvalue, ctx->word_type, op_str,
              current_lvalue_val, rhs_val);

      free(rhs_val);
      free(current_lvalue_val);
    } else {
      codegen_expr(ctx, node->data.assign.rhs, &rvalue, &rvalue_type);
    }

    fprintf(ctx->out, "\tstore%c %s, %s\n", ctx->word_type, rvalue,
            lvalue_addr);
    *result = rvalue;
    free(lvalue_addr);
    break;
  }
  case AST_BINARY_OP: {
    char *left;
    char *right;
    char left_type, right_type;
    codegen_expr(ctx, node->data.binary_op.left, &left, &left_type);
    codegen_expr(ctx, node->data.binary_op.right, &right, &right_type);

    const char *op_str = get_op_str(node->data.binary_op.op);
    const char *cmp_op_str =
        get_cmp_op_str(node->data.binary_op.op, ctx->word_type);

    *result = new_temp(ctx);
    if (cmp_op_str) {
      fprintf(ctx->out, "\t%s =%c %s %s, %s\n", *result, ctx->word_type,
              cmp_op_str, left, right);
    } else if (op_str) {
      fprintf(ctx->out, "\t%s =%c %s %s, %s\n", *result, ctx->word_type, op_str,
              left, right);
    } else {
      error(node->line, 0, 0, "Invalid binary operator token %d",
            node->data.binary_op.op);
    }

    free(left);
    free(right);
    break;
  }
  case AST_UNARY_OP: {
    char *expr_val;
    char expr_type;
    codegen_expr(ctx, node->data.unary_op.expr, &expr_val, &expr_type);

    *result = new_temp(ctx);
    switch (node->data.unary_op.op) {
    case TOK_MINUS:
      fprintf(ctx->out, "\t%s =%c sub 0, %s\n", *result, ctx->word_type,
              expr_val);
      break;
    case TOK_PLUS:
      fprintf(ctx->out, "\t%s =%c copy %s\n", *result, ctx->word_type,
              expr_val);
      break;
    case TOK_NOT:
      fprintf(ctx->out, "\t%s =%c ceq%c %s, 0\n", *result, ctx->word_type,
              ctx->word_type, expr_val);
      break;
    case TOK_COMPLEMENT:
      fprintf(ctx->out, "\t%s =%c xor %s, -1\n", *result, ctx->word_type,
              expr_val);
      break;
    case TOK_INC:
    case TOK_DEC: {
      char *lvalue_addr;
      codegen_lvalue(ctx, node->data.unary_op.expr, &lvalue_addr);
      const char *op = (node->data.unary_op.op == TOK_INC) ? "add" : "sub";
      fprintf(ctx->out, "\t%s =%c %s %s, 1\n", *result, ctx->word_type, op,
              expr_val);
      fprintf(ctx->out, "\tstore%c %s, %s\n", ctx->word_type, *result,
              lvalue_addr);
      free(lvalue_addr);
      break;
    }
    default:
      error(node->line, 0, 0, "Unsupported unary operator");
    }
    free(expr_val);
    break;
  }
  case AST_POSTFIX_OP: {
    char *lvalue_addr;
    codegen_lvalue(ctx, node->data.postfix_op.expr, &lvalue_addr);
    *result = new_temp(ctx);
    fprintf(ctx->out, "\t%s =%c load%c %s\n", *result, ctx->word_type,
            ctx->word_type, lvalue_addr);

    char *new_val = new_temp(ctx);
    const char *op = (node->data.postfix_op.op == TOK_INC) ? "add" : "sub";
    fprintf(ctx->out, "\t%s =%c %s %s, 1\n", new_val, ctx->word_type, op,
            *result);
    fprintf(ctx->out, "\tstore%c %s, %s\n", ctx->word_type, new_val,
            lvalue_addr);

    free(lvalue_addr);
    free(new_val);
    break;
  }
  case AST_TERNARY: {
    char *result_addr = new_temp(ctx);
    fprintf(ctx->out, "\t%s =%c alloc%hu %hu\n", result_addr, ctx->word_type,
            ctx->word_size, ctx->word_size);

    char *cond_val;
    char cond_type;
    codegen_expr(ctx, node->data.ternary.cond, &cond_val, &cond_type);

    char *then_label = new_label(ctx);
    char *else_label = new_label(ctx);
    char *end_label = new_label(ctx);
    fprintf(ctx->out, "\tjnz %s, %s, %s\n", cond_val, then_label, else_label);
    free(cond_val);

    fprintf(ctx->out, "%s\n", then_label);
    char *then_val;
    char then_type;
    int then_terminates =
        codegen_expr(ctx, node->data.ternary.then_expr, &then_val, &then_type);
    if (!then_terminates) {
      fprintf(ctx->out, "\tstore%c %s, %s\n", ctx->word_type, then_val,
              result_addr);
      fprintf(ctx->out, "\tjmp %s\n", end_label);
    }
    free(then_val);

    fprintf(ctx->out, "%s\n", else_label);
    char *else_val;
    char else_type;
    int else_terminates =
        codegen_expr(ctx, node->data.ternary.else_expr, &else_val, &else_type);
    if (!else_terminates) {
      fprintf(ctx->out, "\tstore%c %s, %s\n", ctx->word_type, else_val,
              result_addr);
      fprintf(ctx->out, "\tjmp %s\n", end_label);
    }
    free(else_val);

    fprintf(ctx->out, "%s\n", end_label);
    *result = new_temp(ctx);
    fprintf(ctx->out, "\t%s =%c load%c %s\n", *result, ctx->word_type,
            ctx->word_type, result_addr);

    terminates = then_terminates && else_terminates;

    free(result_addr);
    free(then_label);
    free(else_label);
    free(end_label);
    break;
  }
  case AST_FUNC_CALL: {
    int arg_count = node->data.func_call.arg_count;
    char **arg_vals = malloc(arg_count * sizeof(char *));

    // Evaluate arguments from right to left (same as tsoding/b)
    for (int i = arg_count - 1; i >= 0; i--) {
      char arg_type;
      codegen_expr(ctx, node->data.func_call.args[i], &arg_vals[i], &arg_type);
    }

    char *func_val;
    char func_type;
    if (node->data.func_call.func_expr->type == AST_SUBSCRIPT) {
      char *func_addr;
      codegen_lvalue(ctx, node->data.func_call.func_expr, &func_addr);
      func_val = new_temp(ctx);
      fprintf(ctx->out, "\t%s =%c load%c %s\n", func_val, ctx->word_type,
              ctx->word_type, func_addr);
      free(func_addr);
    } else {
      codegen_expr(ctx, node->data.func_call.func_expr, &func_val, &func_type);
    }

    bool needs_return = true;
    if (node->parent &&
        (node->parent->type == AST_BLOCK || node->parent->type == AST_IF ||
         node->parent->type == AST_WHILE ||
         node->parent->type == AST_FUNC_DECL)) {
      needs_return = false;
    }

    if (needs_return) {
      *result = new_temp(ctx);
      fprintf(ctx->out, "\t%s =%c call %s(", *result, ctx->word_type, func_val);
    } else {
      *result = str_dup("0"); // Dummy value
      fprintf(ctx->out, "\tcall %s(", func_val);
    }

    // Print arguments in the correct (left-to-right) order for the call
    for (int i = 0; i < arg_count; i++) {
      fprintf(ctx->out, "%c %s", ctx->word_type, arg_vals[i]);
      if (i < arg_count - 1)
        fprintf(ctx->out, ", ");
      free(arg_vals[i]);
    }
    fprintf(ctx->out, ")\n");
    free(arg_vals);
    free(func_val);
    break;
  }
  default:
    error(node->line, 0, 0, "Unknown expression type %d in codegen",
          node->type);
  }
  return terminates;
}

static int codegen_stmt(CodegenContext *ctx, AstNode *node) {
  if (!node)
    return 0;
  int terminates = 0;

  switch (node->type) {
  case AST_ASSIGN:
  case AST_FUNC_CALL:
  case AST_UNARY_OP:
  case AST_POSTFIX_OP: {
    char *result;
    char result_type;
    terminates = codegen_expr(ctx, node, &result, &result_type);
    free(result);
    break;
  }
  case AST_FUNC_DECL: {
    if (node->data.func_decl.body &&
        node->data.func_decl.body->type == AST_ASM_STMT) {
      char *name = node->data.func_decl.name;
      fprintf(ctx->asm_out, ".globl %s\n", name);
      fprintf(ctx->asm_out, "%s:\n", name);
      char *code = node->data.func_decl.body->data.asm_stmt.code;
      char *code_copy = str_dup(code);
      if (!code_copy)
        error(node->line, 0, 0, "malloc failed for asm codegen");
      char *line = strtok(code_copy, "\n");
      while (line != NULL) {
        fprintf(ctx->asm_out, "\t%s\n", line);
        line = strtok(NULL, "\n");
      }
      fprintf(ctx->asm_out, "\n");
      free(code_copy);
    } else if (node->data.func_decl.body) {
      enter_scope(ctx); // Function scope
      ctx->current_function_name = node->data.func_decl.name;
      ctx->temp_count = 0;
      ctx->break_label = NULL;
      ctx->auto_var_info = NULL;

      Symbol *func_sym =
          scope_find_symbol(ctx->current_scope, node->data.func_decl.name);
      fprintf(ctx->out, "export function %c %s(", ctx->word_type,
              func_sym->qbe_name);
      for (int i = 0; i < node->data.func_decl.param_count; i++) {
        fprintf(ctx->out, "%c %%p%d", ctx->word_type, i);
        if (i < node->data.func_decl.param_count - 1 ||
            node->data.func_decl.has_varargs)
          fprintf(ctx->out, ", ");
      }
      if (node->data.func_decl.has_varargs) {
        fprintf(ctx->out, "...");
      }
      fprintf(ctx->out, ") {\n@start\n");

      AutoVarList all_locals = {0};
      all_locals.capacity = node->data.func_decl.param_count;
      all_locals.vars = malloc(sizeof(AstNode *) * all_locals.capacity);

      for (int i = 0; i < node->data.func_decl.param_count; i++) {
        all_locals.vars[all_locals.count++] = node->data.func_decl.params[i];
      }

      find_all_autos_in_func(node->data.func_decl.body, &all_locals);
      long total_frame_size = 0;
      long *sizes =
          all_locals.count > 0 ? calloc(all_locals.count, sizeof(long)) : NULL;
      if (all_locals.count > 0 && !sizes)
        error(node->line, 0, 0, "malloc failed");

      for (int i = 0; i < all_locals.count; i++) {
        AstNode *var_node = all_locals.vars[i];
        long size_in_words = 1;
        if (var_node->type == AST_VAR_DECL &&
            var_node->data.var_decl.is_vector) {
          if (var_node->data.var_decl.size_expr) {
            AstNode *size_node = var_node->data.var_decl.size_expr;
            if (size_node->type != AST_NUMBER)
              error(var_node->line, 0, 0,
                    "Local array size must be a constant integer.");
            // According to the B reference, `name[N]` allocates N+1 words.
            size_in_words = size_node->data.number.value + 1;
          } else if (var_node->data.var_decl.init_count > 0) {
            size_in_words = var_node->data.var_decl.init_count;
          }
        }
        sizes[i] = size_in_words * ctx->word_size;
        total_frame_size += sizes[i];
      }

      if (total_frame_size > 0) {
        ctx->current_function_frame = new_temp(ctx);
        fprintf(ctx->out, "\t%s =%c alloc%hu %ld\n",
                ctx->current_function_frame, ctx->word_type, ctx->word_size,
                total_frame_size);
      } else {
        ctx->current_function_frame = NULL;
      }

      long current_offset = 0;
      for (int i = all_locals.count - 1; i >= 0; i--) {
        AstNode *var_node = all_locals.vars[i];
        AutoVarInfo *info = malloc(sizeof(AutoVarInfo));
        info->name = str_dup(var_node->type == AST_VAR_DECL
                                 ? var_node->data.var_decl.name
                                 : var_node->data.ident.name);
        info->offset = current_offset;
        info->is_vector = (var_node->type == AST_VAR_DECL)
                              ? var_node->data.var_decl.is_vector
                              : false;
        info->next = ctx->auto_var_info;
        ctx->auto_var_info = info;
        current_offset += sizes[i];
      }
      if (sizes)
        free(sizes);

      enter_scope(ctx); // Parameter and top-level auto scope
      for (int i = 0; i < node->data.func_decl.param_count; i++) {
        AstNode *param_node = node->data.func_decl.params[i];
        AutoVarInfo *info = ctx->auto_var_info;
        while (info && strcmp(info->name, param_node->data.ident.name) != 0)
          info = info->next;

        Symbol *sym = scope_add_symbol(ctx, param_node->data.ident.name,
                                       SYM_VAR, false, param_node->line);
        sym->stack_offset = info->offset;
        fprintf(ctx->out, "\t%s =%c add %s, %ld\n", sym->qbe_name,
                ctx->word_type, ctx->current_function_frame, sym->stack_offset);
        fprintf(ctx->out, "\tstore%c %%p%d, %s\n", ctx->word_type, i,
                sym->qbe_name);
      }

      int body_terminates = codegen_stmt(ctx, node->data.func_decl.body);
      if (!body_terminates) {
        fprintf(ctx->out, "\tret 0\n");
      }
      fprintf(ctx->out, "}\n\n");

      exit_scope(ctx); // Param/auto scope
      exit_scope(ctx); // Function scope

      free(all_locals.vars);
      if (ctx->current_function_frame)
        free(ctx->current_function_frame);
      while (ctx->auto_var_info) {
        AutoVarInfo *next = ctx->auto_var_info->next;
        free(ctx->auto_var_info->name);
        free(ctx->auto_var_info);
        ctx->auto_var_info = next;
      }
      ctx->current_function_name = NULL;
    }
    break;
  }
  case AST_VAR_DECL: {
    if (ctx->current_scope->parent == NULL) { // Global
      Symbol *sym =
          scope_find_symbol(ctx->current_scope, node->data.var_decl.name);
      long size_in_words = 1;
      if (node->data.var_decl.is_vector) {
        long size_from_expr = 0;
        if (node->data.var_decl.size_expr) {
          AstNode *folded_size =
              ast_fold_constants(node->data.var_decl.size_expr);
          if (folded_size->type != AST_NUMBER)
            error(node->line, 0, 0,
                  "Global array size must be a constant integer.");
          // According to the B reference, `name[N]` allocates N+1 words.
          size_from_expr = folded_size->data.number.value + 1;
          if (folded_size != node->data.var_decl.size_expr)
            ast_free(folded_size);
        }
        long size_from_inits = node->data.var_decl.init_count;
        size_in_words = (size_from_expr > size_from_inits) ? size_from_expr
                                                           : size_from_inits;
        if (size_in_words == 0)
          size_in_words = 1; // Cannot be size 0
      }

      fprintf(ctx->out, "data %s = align %hu { ", sym->qbe_name,
              ctx->word_size);
      if (node->data.var_decl.init_count > 0) {
        for (int i = 0; i < node->data.var_decl.init_count; i++) {
          AstNode *init_node = node->data.var_decl.init_list[i];
          fprintf(ctx->out, "%c ", ctx->word_type);
          if (init_node->type == AST_NUMBER) {
            fprintf(ctx->out, "%ld", init_node->data.number.value);
          } else if (init_node->type == AST_STRING) {
            char *label = add_string(ctx, init_node->data.string.value);
            fprintf(ctx->out, "$%s", label);
            free(label);
          } else if (init_node->type == AST_IDENT) {
            Symbol *init_sym = scope_find_symbol(ctx->current_scope,
                                                 init_node->data.ident.name);
            if (!init_sym)
              init_sym = scope_add_symbol(ctx, init_node->data.ident.name,
                                          SYM_FUNC, false, init_node->line);
            fprintf(ctx->out, "%s", init_sym->qbe_name);
          } else {
            error(node->line, 0, 0,
                  "Global initializer must be a constant or identifier.");
          }
          if (i < node->data.var_decl.init_count - 1)
            fprintf(ctx->out, ", ");
        }
      }

      long remaining_bytes =
          (size_in_words - node->data.var_decl.init_count) * ctx->word_size;
      if (remaining_bytes > 0) {
        if (node->data.var_decl.init_count > 0)
          fprintf(ctx->out, ", ");
        fprintf(ctx->out, "z %ld", remaining_bytes);
      } else if (node->data.var_decl.init_count == 0 && size_in_words > 0) {
        fprintf(ctx->out, "z %ld", size_in_words * ctx->word_size);
      }

      fprintf(ctx->out, " }\n");
    } else { // Local auto
      AutoVarInfo *info = ctx->auto_var_info;
      while (info && strcmp(info->name, node->data.var_decl.name) != 0) {
        info = info->next;
      }
      if (!info)
        error(node->line, 0, 0,
              "Internal error: auto var '%s' has no offset info.",
              node->data.var_decl.name);

      Symbol *sym = scope_add_symbol(ctx, node->data.var_decl.name, SYM_VAR,
                                     info->is_vector, node->line);
      sym->stack_offset = info->offset;
      fprintf(ctx->out, "\t%s =%c add %s, %ld\n", sym->qbe_name, ctx->word_type,
              ctx->current_function_frame, sym->stack_offset);

      if (node->data.var_decl.init_count > 0) {
        AstNode *ident_node =
            ast_ident(str_dup(node->data.var_decl.name), node->line);
        char *lvalue_addr_base;
        codegen_lvalue(ctx, ident_node, &lvalue_addr_base);
        ast_free(ident_node);

        for (int i = 0; i < node->data.var_decl.init_count; ++i) {
          char *init_val;
          char init_type;
          codegen_expr(ctx, node->data.var_decl.init_list[i], &init_val,
                       &init_type);
          char *current_addr = lvalue_addr_base;
          if (i > 0) {
            current_addr = new_temp(ctx);
            fprintf(ctx->out, "\t%s =%c add %s, %ld\n", current_addr,
                    ctx->word_type, lvalue_addr_base, (long)i * ctx->word_size);
          }
          fprintf(ctx->out, "\tstore%c %s, %s\n", ctx->word_type, init_val,
                  current_addr);
          if (i > 0)
            free(current_addr);
          free(init_val);
        }
        free(lvalue_addr_base);
      }
    }
    break;
  }
  case AST_EXTRN_DECL:
    // If a symbol for this name doesn't exist anywhere, add it to the CURRENT
    // scope. This handles true externs like printf. If it already exists (e.g.,
    // from a global definition parsed in pass 1), do nothing.
    if (!scope_find_symbol(ctx->current_scope, node->data.extrn_decl.name)) {
      scope_add_symbol(ctx, node->data.extrn_decl.name, SYM_FUNC, false,
                       node->line);
    }
    break;
  case AST_IF: {
    char *cond_val;
    char cond_type;
    codegen_expr(ctx, node->data.if_stmt.cond, &cond_val, &cond_type);

    char *then_label = new_label(ctx);
    char *end_label = new_label(ctx);
    char *else_label =
        node->data.if_stmt.else_body ? new_label(ctx) : end_label;
    fprintf(ctx->out, "\tjnz %s, %s, %s\n", cond_val, then_label, else_label);
    free(cond_val);

    fprintf(ctx->out, "%s\n", then_label);
    int then_terminates = codegen_stmt(ctx, node->data.if_stmt.then_body);
    if (!then_terminates)
      fprintf(ctx->out, "\tjmp %s\n", end_label);

    int else_terminates = 0;
    if (node->data.if_stmt.else_body) {
      fprintf(ctx->out, "%s\n", else_label);
      else_terminates = codegen_stmt(ctx, node->data.if_stmt.else_body);
      if (!else_terminates)
        fprintf(ctx->out, "\tjmp %s\n", end_label);
      if (else_label != end_label)
        free(else_label);
    }
    fprintf(ctx->out, "%s\n", end_label);
    free(then_label);
    free(end_label);
    terminates = then_terminates &&
                 (node->data.if_stmt.else_body != NULL && else_terminates);
    break;
  }
  case AST_WHILE: {
    char *start_label = new_label(ctx);
    char *body_label = new_label(ctx);
    char *end_label = new_label(ctx);
    char *old_break_label = ctx->break_label;
    ctx->break_label = end_label;

    fprintf(ctx->out, "\tjmp %s\n", start_label);
    fprintf(ctx->out, "%s\n", body_label);
    codegen_stmt(ctx, node->data.while_stmt.body);

    fprintf(ctx->out, "%s\n", start_label);
    char *cond_val;
    char cond_type;
    codegen_expr(ctx, node->data.while_stmt.cond, &cond_val, &cond_type);
    fprintf(ctx->out, "\tjnz %s, %s, %s\n", cond_val, body_label, end_label);
    free(cond_val);

    fprintf(ctx->out, "%s\n", end_label);
    free(start_label);
    free(body_label);
    free(end_label);
    ctx->break_label = old_break_label;
    terminates = 0;
    break;
  }
  case AST_SWITCH: {
    char *expr_val;
    char expr_type;
    codegen_expr(ctx, node->data.switch_stmt.expr, &expr_val, &expr_type);

    char *old_break_label = ctx->break_label;
    char *end_label = new_label(ctx);
    ctx->break_label = end_label;
    char *default_target =
        node->data.switch_stmt.default_label_name
            ? str_dup(node->data.switch_stmt.default_label_name)
            : str_dup(end_label);

    // Generate a chain of comparisons, like in the reference implementation.
    if (node->data.switch_stmt.case_count > 0) {
      for (int i = 0; i < node->data.switch_stmt.case_count; i++) {
        AstCaseLabel *case_label = &node->data.switch_stmt.case_labels[i];
        char *cmp_res = new_temp(ctx);
        char *next_check_label;

        bool is_last_case = (i == node->data.switch_stmt.case_count - 1);
        if (is_last_case) {
          // On the last case, if it doesn't match, jump to the default target.
          next_check_label = str_dup(default_target);
        } else {
          // Otherwise, jump to the next comparison check.
          next_check_label = new_label(ctx);
        }

        fprintf(ctx->out, "\t%s =%c ceq%c %s, %ld\n", cmp_res, ctx->word_type,
                ctx->word_type, expr_val, case_label->value);
        fprintf(ctx->out, "\tjnz %s, %s, %s\n", cmp_res, case_label->label_name,
                next_check_label);

        if (!is_last_case) {
          // Define the label for the next check.
          fprintf(ctx->out, "%s\n", next_check_label);
        }

        free(cmp_res);
        free(next_check_label);
      }
    } else {
      // If there are no cases, jump straight to the default target.
      fprintf(ctx->out, "\tjmp %s\n", default_target);
    }
    free(expr_val);

    // The block containing the jump table is now terminated by the last `jnz`
    // or the `jmp`. The next thing generated will start a new block, which
    // should be the case bodies. Any statements before the first case are
    // unreachable and will create an unterminated block. We will manually
    // iterate the body's statements and skip code until the first label.
    AstNode *body = node->data.switch_stmt.body;
    bool start_codegen = false;
    if (body->type == AST_BLOCK) {
      for (int i = 0; i < body->data.block.stmt_count; i++) {
        AstNode *stmt = body->data.block.stmts[i];
        if (!start_codegen &&
            (stmt->type == AST_CASE || stmt->type == AST_DEFAULT ||
             stmt->type == AST_LABEL)) {
          start_codegen = true;
        }
        if (start_codegen) {
          codegen_stmt(ctx, stmt);
        }
      }
    } else {
      if (body->type == AST_CASE || body->type == AST_DEFAULT ||
          body->type == AST_LABEL) {
        codegen_stmt(ctx, body);
      }
    }

    fprintf(ctx->out, "%s\n", end_label);

    free(end_label);
    free(default_target);
    ctx->break_label = old_break_label;
    terminates =
        0; // A switch statement itself doesn't terminate unless all paths do.
    break;
  }
  case AST_CASE:
    if (!node->data.case_stmt.qbe_label)
      error(node->line, 0, 0, "Internal error: case has no QBE label.");
    fprintf(ctx->out, "%s\n", node->data.case_stmt.qbe_label);
    terminates = codegen_stmt(ctx, node->data.case_stmt.body);
    break;
  case AST_DEFAULT:
    if (!node->data.default_stmt.qbe_label)
      error(node->line, 0, 0, "Internal error: default has no QBE label.");
    fprintf(ctx->out, "%s\n", node->data.default_stmt.qbe_label);
    terminates = codegen_stmt(ctx, node->data.default_stmt.body);
    break;
  case AST_RETURN: {
    if (node->data.return_stmt.expr) {
      char *expr_val;
      char expr_type;
      codegen_expr(ctx, node->data.return_stmt.expr, &expr_val, &expr_type);
      fprintf(ctx->out, "\tret %s\n", expr_val);
      free(expr_val);
    } else {
      fprintf(ctx->out, "\tret\n");
    }
    terminates = 1;
    break;
  }
  case AST_BLOCK: {
    bool is_real_block = !node->data.block.is_synthetic;
    if (is_real_block)
      enter_scope(ctx);

    for (int i = 0; i < node->data.block.stmt_count; i++) {
      AstNode *current_stmt = node->data.block.stmts[i];
      if (terminates) {
        bool is_label = (current_stmt->type == AST_LABEL ||
                         current_stmt->type == AST_CASE ||
                         current_stmt->type == AST_DEFAULT);
        if (!is_label) {
          warning(WARN_UNREACHABLE_CODE, current_stmt->line, 0, 0,
                  "Unreachable code.");
          continue;
        }
        // A label makes the following code reachable again.
        terminates = 0;
      }
      if (codegen_stmt(ctx, current_stmt)) {
        terminates = 1;
      }
    }
    if (is_real_block)
      exit_scope(ctx);
    break;
  }
  case AST_LABEL:
    fprintf(ctx->out, "@%s\n", node->data.label.name);
    terminates = codegen_stmt(ctx, node->data.label.stmt);
    break;
  case AST_GOTO:
    fprintf(ctx->out, "\tjmp @%s\n", node->data.goto_stmt.label);
    terminates = 1;
    break;
  case AST_BREAK:
    if (!ctx->break_label)
      error(node->line, 0, 0, "'break' statement not in loop or switch");
    fprintf(ctx->out, "\tjmp %s\n", ctx->break_label);
    terminates = 1;
    break;
  case AST_ASM_STMT:
    error(node->line, 0, 0, "Standalone '__asm__' statement is not supported.");
    break;
  default:
    if (node->type < AST_FUNC_DECL) {
      char *result;
      char result_type;
      codegen_expr(ctx, node, &result, &result_type);
      free(result);
    } else if (node->type != AST_VAR_DECL) {
      error(node->line, 0, 0, "Unknown statement type %d in codegen",
            node->type);
    }
  }
  return terminates;
}

static void collect_strings_recursive(CodegenContext *ctx, AstNode *node) {
  if (!node)
    return;
  switch (node->type) {
  case AST_STRING:
    add_string(ctx, node->data.string.value);
    break;
  case AST_ASSIGN:
    collect_strings_recursive(ctx, node->data.assign.lhs);
    collect_strings_recursive(ctx, node->data.assign.rhs);
    break;
  case AST_BINARY_OP:
    collect_strings_recursive(ctx, node->data.binary_op.left);
    collect_strings_recursive(ctx, node->data.binary_op.right);
    break;
  case AST_UNARY_OP:
    collect_strings_recursive(ctx, node->data.unary_op.expr);
    break;
  case AST_POSTFIX_OP:
    collect_strings_recursive(ctx, node->data.postfix_op.expr);
    break;
  case AST_INDIRECTION:
    collect_strings_recursive(ctx, node->data.indirection.expr);
    break;
  case AST_ADDRESS_OF:
    collect_strings_recursive(ctx, node->data.address_of.lvalue);
    break;
  case AST_TERNARY:
    collect_strings_recursive(ctx, node->data.ternary.cond);
    collect_strings_recursive(ctx, node->data.ternary.then_expr);
    collect_strings_recursive(ctx, node->data.ternary.else_expr);
    break;
  case AST_SUBSCRIPT:
    collect_strings_recursive(ctx, node->data.subscript.array);
    collect_strings_recursive(ctx, node->data.subscript.index);
    break;
  case AST_FUNC_CALL:
    collect_strings_recursive(ctx, node->data.func_call.func_expr);
    for (int i = 0; i < node->data.func_call.arg_count; i++) {
      collect_strings_recursive(ctx, node->data.func_call.args[i]);
    }
    break;
  case AST_FUNC_DECL:
    collect_strings_recursive(ctx, node->data.func_decl.body);
    break;
  case AST_VAR_DECL:
    for (int i = 0; i < node->data.var_decl.init_count; ++i) {
      collect_strings_recursive(ctx, node->data.var_decl.init_list[i]);
    }
    break;
  case AST_IF:
    collect_strings_recursive(ctx, node->data.if_stmt.cond);
    collect_strings_recursive(ctx, node->data.if_stmt.then_body);
    collect_strings_recursive(ctx, node->data.if_stmt.else_body);
    break;
  case AST_WHILE:
    collect_strings_recursive(ctx, node->data.while_stmt.cond);
    collect_strings_recursive(ctx, node->data.while_stmt.body);
    break;
  case AST_RETURN:
    collect_strings_recursive(ctx, node->data.return_stmt.expr);
    break;
  case AST_BLOCK:
    for (int i = 0; i < node->data.block.stmt_count; i++) {
      collect_strings_recursive(ctx, node->data.block.stmts[i]);
    }
    break;
  case AST_SWITCH:
    collect_strings_recursive(ctx, node->data.switch_stmt.expr);
    collect_strings_recursive(ctx, node->data.switch_stmt.body);
    break;
  case AST_CASE:
    collect_strings_recursive(ctx, node->data.case_stmt.value);
    collect_strings_recursive(ctx, node->data.case_stmt.body);
    break;
  case AST_DEFAULT:
    collect_strings_recursive(ctx, node->data.default_stmt.body);
    break;
  case AST_LABEL:
    collect_strings_recursive(ctx, node->data.label.stmt);
    break;
  default:
    break;
  }
}

static void find_all_autos_in_func(AstNode *node, AutoVarList *list) {
  if (!node)
    return;
  if (node->type == AST_FUNC_DECL) {
    find_all_autos_in_func(node->data.func_decl.body, list);
    return;
  }

  if (node->type == AST_VAR_DECL) {
    if (list->count >= list->capacity) {
      list->capacity = list->capacity == 0 ? 8 : list->capacity * 2;
      list->vars = realloc(list->vars, sizeof(AstNode *) * list->capacity);
    }
    list->vars[list->count++] = node;
  }

  switch (node->type) {
  case AST_BLOCK:
    for (int i = 0; i < node->data.block.stmt_count; i++) {
      find_all_autos_in_func(node->data.block.stmts[i], list);
    }
    break;
  case AST_IF:
    find_all_autos_in_func(node->data.if_stmt.then_body, list);
    find_all_autos_in_func(node->data.if_stmt.else_body, list);
    break;
  case AST_WHILE:
    find_all_autos_in_func(node->data.while_stmt.body, list);
    break;
  case AST_SWITCH:
    find_all_autos_in_func(node->data.switch_stmt.body, list);
    break;
  case AST_CASE:
    find_all_autos_in_func(node->data.case_stmt.body, list);
    break;
  case AST_DEFAULT:
    find_all_autos_in_func(node->data.default_stmt.body, list);
    break;
  case AST_LABEL:
    find_all_autos_in_func(node->data.label.stmt, list);
    break;
  default:
    break;
  }
}

void codegen_generate(AstNode *node, const char *output_file,
                      const char *asm_output_file,
                      unsigned short int word_size) {
  CodegenContext ctx = {
      .out = fopen(output_file, "w"),
      .asm_out = fopen(asm_output_file, "w"),
      .temp_count = 0,
      .label_count = 0,
      .current_scope = scope_create(NULL),
      .current_function_name = NULL,
      .current_function_frame = NULL,
      .break_label = NULL,
      .word_size = word_size,
      .word_type = (word_size == 8) ? 'l' : 'w',
  };
  init_string_table(&ctx.strings);
  if (!ctx.out)
    error(0, 0, 0, "Failed to open output file '%s'", output_file);
  if (!ctx.asm_out)
    error(0, 0, 0, "Failed to open output file '%s'", asm_output_file);

  fprintf(ctx.asm_out, "# Inline assembly from source\n.section .text\n\n");

  // PASS 1: Find all global definitions and populate the global symbol table.
  if (node->type == AST_BLOCK) {
    for (int i = 0; i < node->data.block.stmt_count; i++) {
      AstNode *stmt = node->data.block.stmts[i];
      if (stmt->type == AST_VAR_DECL) {
        scope_add_symbol(&ctx, stmt->data.var_decl.name, SYM_VAR,
                         stmt->data.var_decl.is_vector, stmt->line);
      } else if (stmt->type == AST_FUNC_DECL) {
        scope_add_symbol(&ctx, stmt->data.func_decl.name, SYM_FUNC, false,
                         stmt->line);
      }
    }
  }

  // Also collect all string literals before generating code.
  collect_strings_recursive(&ctx, node);

  // PASS 2: Generate code for all statements.
  codegen_stmt(&ctx, node);

  emit_data_section(&ctx);

  fclose(ctx.out);
  fclose(ctx.asm_out);
  free_string_table(&ctx.strings);
  scope_free(ctx.current_scope);
}
