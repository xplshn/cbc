#include "codegen.h"
#include "ast.h"
#include "lexer.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Global data definitions
typedef struct {
    char* value;
    char* label;
} StringEntry;

typedef struct {
    StringEntry* entries;
    int count;
    int capacity;
} StringTable;

// Sym. Table for Variable scoping
typedef struct Symbol {
    char* name;
    int is_param; // 1 for parameter, 0 for local
    int is_initialized;
    int is_used;  // usage is tracked for warnings
    struct Symbol* next;
} Symbol;

typedef struct Scope {
    Symbol* symbols;
    struct Scope* parent;
} Scope;

typedef struct {
    FILE* out;
    StringTable strings;
    int temp_count;
    int label_count;
    Scope* current_scope;
    char* current_function;
    bool has_return; // to know if function has return statement
} CodegenContext;

// fwd declarations
static void init_string_table(StringTable* table);
static void free_string_table(StringTable* table);
static char* add_string(CodegenContext* ctx, const char* value, int line);
static void emit_data_section(CodegenContext* ctx);
static Symbol* symbol_create(char* name, int is_param);
static void symbol_free(Symbol* sym);
static Scope* scope_create(Scope* parent);
static void scope_free(Scope* scope);
static void enter_scope(CodegenContext* ctx);
static void exit_scope(CodegenContext* ctx);
static void scope_add_symbol(Scope* scope, char* name, int is_param, int line);
static int scope_find_symbol(Scope* scope, char* name, int* is_param, int* is_initialized, int* is_used);
static void scope_mark_initialized(Scope* scope, char* name, int line);
static void scope_mark_used(Scope* scope, char* name, int line);
static void check_unused_symbols(Scope* scope, int line);
static char* new_temp(CodegenContext* ctx);
static char* new_label(CodegenContext* ctx);
static void codegen_expr(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type);
static void codegen_lvalue(CodegenContext* ctx, AstNode* node, char** result);
static void codegen_stmt(CodegenContext* ctx, AstNode* node);
static bool is_valid_lvalue(AstNode* node);

// str table management
static void init_string_table(StringTable* table) {
    table->entries = malloc(sizeof(StringEntry) * 10);
    table->capacity = 10;
    table->count = 0;
}

static void free_string_table(StringTable* table) {
    for (int i = 0; i < table->count; i++) {
        free(table->entries[i].value);
        free(table->entries[i].label);
    }
    free(table->entries);
}

static char* add_string(CodegenContext* ctx, const char* value, int line) {
    // validate
    if (!value || value[0] == '\0') {
        fprintf(stderr, "\033[33mWarning\033[0m at line %d: Empty or null string literal\n", line);
        return str_dup("empty_str");
    }
    // check for existing string
    for (int i = 0; i < ctx->strings.count; i++) {
        if (strcmp(ctx->strings.entries[i].value, value) == 0) {
            return str_dup(ctx->strings.entries[i].label);
        }
    }
    // add new string
    if (ctx->strings.count >= ctx->strings.capacity) {
        ctx->strings.capacity *= 2;
        ctx->strings.entries = realloc(ctx->strings.entries, sizeof(StringEntry) * ctx->strings.capacity);
    }
    char* label = malloc(16);
    snprintf(label, 16, "str%d", ctx->strings.count);
    ctx->strings.entries[ctx->strings.count].value = str_dup(value);
    ctx->strings.entries[ctx->strings.count].label = str_dup(label);
    ctx->strings.count++;
    return label;
}

// TODO: Moronic handling of escape sequences should be removed.
//       - Stop being lazy and look at cproc's codegen, maybe I can learn something
static void emit_data_section(CodegenContext* ctx) {
    fprintf(ctx->out, "# Data section\n");
    for (int i = 0; i < ctx->strings.count; i++) {
        fprintf(ctx->out, "data $%s = { b \"", ctx->strings.entries[i].label);
        // handle escape sequences
        for (char* p = ctx->strings.entries[i].value; *p; p++) {
            if (*p == '\n') fprintf(ctx->out, "\\n");
            else if (*p == '"') fprintf(ctx->out, "\\\"");
            else if (*p == '\\') fprintf(ctx->out, "\\\\");
            else fputc(*p, ctx->out);
        }
        fprintf(ctx->out, "\", b 0 }\n");
    }
    fprintf(ctx->out, "\n");
}

static Symbol* symbol_create(char* name, int is_param) {
    Symbol* sym = malloc(sizeof(Symbol));
    sym->name = str_dup(name);
    sym->is_param = is_param;
    sym->is_initialized = is_param;
    sym->is_used = 0;
    sym->next = NULL;
    return sym;
}

static void symbol_free(Symbol* sym) {
    if (sym) {
        free(sym->name);
        free(sym);
    }
}

static Scope* scope_create(Scope* parent) {
    Scope* scope = malloc(sizeof(Scope));
    scope->symbols = NULL;
    scope->parent = parent;
    return scope;
}

static void scope_free(Scope* scope) {
    Symbol* sym = scope->symbols;
    while (sym) {
        Symbol* next = sym->next;
        symbol_free(sym);
        sym = next;
    }
    free(scope);
}

static void enter_scope(CodegenContext* ctx) {
    ctx->current_scope = scope_create(ctx->current_scope);
    DEBUG("Entered new scope");
}

static void exit_scope(CodegenContext* ctx) {
    if (!ctx->current_scope) {
        error(0, "Attempt to exit non-existent scope");
    }
    Scope* scope_to_free = ctx->current_scope;
    ctx->current_scope = ctx->current_scope->parent;
    scope_free(scope_to_free);
    DEBUG("Exited scope");
}

static void scope_add_symbol(Scope* scope, char* name, int is_param, int line) {
    Symbol* sym = scope->symbols;
    while (sym) {
        if (strcmp(sym->name, name) == 0) {
            error(line, "Redefinition of variable '%s' in same scope", name);
        }
        sym = sym->next;
    }
    Symbol* new_sym = symbol_create(name, is_param);
    new_sym->next = scope->symbols;
    scope->symbols = new_sym;
    DEBUG("Added symbol '%s' (param=%d) to scope", name, is_param);
}

static int scope_find_symbol(Scope* scope, char* name, int* is_param, int* is_initialized, int* is_used) {
    while (scope) {
        for (Symbol* sym = scope->symbols; sym; sym = sym->next) {
            if (strcmp(sym->name, name) == 0) {
                *is_param = sym->is_param;
                *is_initialized = sym->is_initialized;
                *is_used = sym->is_used;
                DEBUG("Found symbol '%s' (param=%d, initialized=%d, used=%d)", name, *is_param, *is_initialized, *is_used);
                return 1;
            }
        }
        scope = scope->parent;
    }
    DEBUG("Symbol '%s' not found in any scope", name);
    return 0;
}

static void scope_mark_initialized(Scope* scope, char* name, int line) {
    while (scope) {
        for (Symbol* sym = scope->symbols; sym; sym = sym->next) {
            if (strcmp(sym->name, name) == 0) {
                sym->is_initialized = 1;
                DEBUG("Marked symbol '%s' as initialized", name);
                return;
            }
        }
        scope = scope->parent;
    }
    error(line, "Cannot mark undefined variable '%s' as initialized", name);
}

static void scope_mark_used(Scope* scope, char* name, int line) {
    while (scope) {
        for (Symbol* sym = scope->symbols; sym; sym = sym->next) {
            if (strcmp(sym->name, name) == 0) {
                sym->is_used = 1;
                DEBUG("Marked symbol '%s' as used", name);
                return;
            }
        }
        scope = scope->parent;
    }
    error(line, "Cannot mark undefined variable '%s' as used", name);
}

static void check_unused_symbols(Scope* scope, int line) {
    for (Symbol* sym = scope->symbols; sym; sym = sym->next) {
        if (!sym->is_used && !sym->is_param) {
            fprintf(stderr, "\033[33mWarning\033[0m at line %d: Variable '%s' declared but not used\n",
                    line, sym->name);
        }
    }
}

static char* new_temp(CodegenContext* ctx) {
    char* temp = malloc(16);
    snprintf(temp, 16, "%%t%d", ctx->temp_count++);
    return temp;
}

static char* new_label(CodegenContext* ctx) {
    char* label = malloc(16);
    snprintf(label, 16, "L%d", ctx->label_count++);
    return label;
}

static bool is_valid_lvalue(AstNode* node) {
    return node->type == AST_IDENT || node->type == AST_ARRAY_INDEX;
}

static void codegen_number(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    *result = malloc(16);
    snprintf(*result, 16, "%d", node->data.number.value);
    if (strcmp(expected_type, "w") != 0) {
        fprintf(stderr, "\033[33mWarning\033[0m at line %d: Number used in %s context, assuming word\n",
                node->line, expected_type);
    }
}

static void codegen_string(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    char* label = add_string(ctx, node->data.string.value, node->line);
    *result = malloc(32);
    snprintf(*result, 32, "$%s", label);
    free(label);
    if (strcmp(expected_type, "l") != 0) {
        fprintf(stderr, "\033[33mWarning\033[0m at line %d: String used in %s context, assuming pointer\n",
                node->line, expected_type);
    }
}

static void codegen_ident(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    int is_param, is_initialized, is_used;
    if (!scope_find_symbol(ctx->current_scope, node->data.ident.name, &is_param, &is_initialized, &is_used)) {
        error(node->line, "Undefined variable '%s'", node->data.ident.name);
    }
    if (!is_initialized) {
        fprintf(stderr, "\033[33mWarning\033[0m at line %d: Variable '%s' used before initialization\n",
                node->line, node->data.ident.name);
    }
    scope_mark_used(ctx->current_scope, node->data.ident.name, node->line);
    *result = new_temp(ctx);
    if (is_param) {
        fprintf(ctx->out, "\t%s =%s copy %%%s\n", *result, expected_type, node->data.ident.name);
    } else {
        fprintf(ctx->out, "\t%s =%s loadsw %%%s\n", *result, expected_type, node->data.ident.name);
    }
}

static void codegen_array_index(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    char* addr;
    codegen_lvalue(ctx, node, &addr);
    *result = new_temp(ctx);
    fprintf(ctx->out, "\t%s =l loadl %s\n", *result, addr); // Always load pointer as l
    free(addr);
}

static void codegen_lvalue(CodegenContext* ctx, AstNode* node, char** result) {
    if (!is_valid_lvalue(node)) {
        error(node->line, "Invalid l-value expression");
    }
    switch (node->type) {
        case AST_IDENT: {
            int is_param, is_initialized, is_used;
            if (!scope_find_symbol(ctx->current_scope, node->data.ident.name, &is_param, &is_initialized, &is_used)) {
                error(node->line, "Undefined variable '%s' used as l-value", node->data.ident.name);
            }
            *result = malloc(strlen(node->data.ident.name) + 2);
            sprintf(*result, "%%%s", node->data.ident.name);
            scope_mark_initialized(ctx->current_scope, node->data.ident.name, node->line);
            break;
        }
        case AST_ARRAY_INDEX: {
            char* base;
            char* index;
            char* offset = new_temp(ctx);
            codegen_expr(ctx, node->data.array_index.array, &base, "l");
            codegen_expr(ctx, node->data.array_index.index, &index, "w");
            *result = new_temp(ctx);
            char* index_l = new_temp(ctx);
            fprintf(ctx->out, "\t%s =l extsw %s\n", index_l, index); // Sign-extend w to l
            fprintf(ctx->out, "\t%s =l mul %s, 8\n", offset, index_l); // Use l for pointer arithmetic
            fprintf(ctx->out, "\t%s =l add %s, %s\n", *result, base, offset);
            free(base);
            free(index);
            free(offset);
            free(index_l);
            break;
        }
        default:
            error(node->line, "Invalid l-value expression");
    }
}

static void codegen_assign(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    if (!is_valid_lvalue(node->data.assign.lhs)) {
        error(node->line, "Assignment to non-l-value");
    }
    char* lvalue;
    char* rvalue;
    codegen_lvalue(ctx, node->data.assign.lhs, &lvalue);
    codegen_expr(ctx, node->data.assign.rhs, &rvalue, expected_type);
    fprintf(ctx->out, "\tstore%s %s, %s\n", expected_type, rvalue, lvalue);
    *result = rvalue;
    free(lvalue);
}

static void codegen_binary_op(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    char* left;
    char* right;
    codegen_expr(ctx, node->data.binary_op.left, &left, expected_type);
    codegen_expr(ctx, node->data.binary_op.right, &right, expected_type);
    *result = new_temp(ctx);
    const char* op_str;
    switch (node->data.binary_op.op) {
        case TOK_PLUS: op_str = "add"; break;
        case TOK_MINUS: op_str = "sub"; break;
        case TOK_STAR: op_str = "mul"; break;
        case TOK_SLASH: op_str = "div"; break;
        case TOK_AND: op_str = "and"; break;
        case TOK_OR: op_str = "or"; break;
        case TOK_XOR: op_str = "xor"; break;
        case TOK_SHL: op_str = "shl"; break;
        case TOK_SHR: op_str = "shr"; break;
        case TOK_EQEQ: op_str = "ceqw"; break;
        case TOK_NEQ: op_str = "cnew"; break;
        case TOK_LT: op_str = "csltw"; break;
        case TOK_GT: op_str = "csgtw"; break;
        case TOK_LTE: op_str = "cslew"; break;
        case TOK_GTE: op_str = "csgew"; break;
        default:
            error(node->line, "Unsupported binary operator");
            op_str = "unknown";
            break;
    }
    fprintf(ctx->out, "\t%s =%s %s %s, %s\n", *result, expected_type, op_str, left, right);
    free(left);
    free(right);
}

static void codegen_unary_op(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    char* expr_val;
    codegen_expr(ctx, node->data.unary_op.expr, &expr_val, expected_type);
    *result = new_temp(ctx);

    switch (node->data.unary_op.op) {
        case TOK_NOT:
            fprintf(ctx->out, "\t%s =%s ceqw %s, 0\n", *result, expected_type, expr_val);
            break;
        case TOK_COMPLEMENT:
            fprintf(ctx->out, "\t%s =%s not %s\n", *result, expected_type, expr_val);
            break;
        case TOK_INC: {
            if (!is_valid_lvalue(node->data.unary_op.expr)) {
                error(node->line, "Increment operator applied to non-l-value");
            }
            char* lvalue;
            char* new_val = new_temp(ctx);
            codegen_lvalue(ctx, node->data.unary_op.expr, &lvalue);
            fprintf(ctx->out, "\t%s =%s add %s, 1\n", new_val, expected_type, expr_val);
            fprintf(ctx->out, "\tstore%s %s, %s\n", expected_type, new_val, lvalue);
            fprintf(ctx->out, "\t%s =%s copy %s\n", *result, expected_type, expr_val);
            free(lvalue);
            free(new_val);
            break;
        }
        default:
            error(node->line, "Unsupported unary operator");
    }
    free(expr_val);
}

static void codegen_func_call(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    char** args = malloc(node->data.func_call.arg_count * sizeof(char*));
    const char** arg_types = malloc(node->data.func_call.arg_count * sizeof(char*));
    for (int i = 0; i < node->data.func_call.arg_count; i++) {
        const char* arg_type = (node->data.func_call.args[i]->type == AST_STRING || node->data.func_call.args[i]->type == AST_ARRAY_INDEX) ? "l" : "w";
        codegen_expr(ctx, node->data.func_call.args[i], &args[i], arg_type);
        arg_types[i] = arg_type;
    }
    *result = new_temp(ctx);
    fprintf(ctx->out, "\t%s =%s call $%s(", *result, expected_type, node->data.func_call.name);
    for (int i = 0; i < node->data.func_call.arg_count; i++) {
        fprintf(ctx->out, "%s %s", arg_types[i], args[i]);
        if (i < node->data.func_call.arg_count - 1) fprintf(ctx->out, ", ");
        free(args[i]);
    }
    fprintf(ctx->out, ")\n");
    free(args);
    free(arg_types);
}

static void codegen_expr(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    if (!node) {
        error(0, "Null expression node in codegen");
    }
    switch (node->type) {
        case AST_NUMBER: codegen_number(ctx, node, result, expected_type); break;
        case AST_STRING: codegen_string(ctx, node, result, expected_type); break;
        case AST_IDENT: codegen_ident(ctx, node, result, expected_type); break;
        case AST_ARRAY_INDEX: codegen_array_index(ctx, node, result, expected_type); break;
        case AST_ASSIGN: codegen_assign(ctx, node, result, expected_type); break;
        case AST_BINARY_OP: codegen_binary_op(ctx, node, result, expected_type); break;
        case AST_UNARY_OP: codegen_unary_op(ctx, node, result, expected_type); break;
        case AST_FUNC_CALL: codegen_func_call(ctx, node, result, expected_type); break;
        default:
            error(node->line, "Unknown expression type %d in codegen", node->type);
    }
}

static bool is_var_decl_block(AstNode* node) {
    if (node->type != AST_BLOCK) {
        DEBUG("Not a block node, type=%d", node->type);
        return false;
    }
    DEBUG("Checking block with %d statements", node->data.block.stmt_count);
    for (int i = 0; i < node->data.block.stmt_count; i++) {
        AstNode* stmt = node->data.block.stmts[i];
        DEBUG("Statement %d type=%d", i, stmt->type);
        if (stmt->type != AST_VAR_DECL) {
            DEBUG("Non-variable declaration found, type=%d", stmt->type);
            return false;
        }
    }
    DEBUG("Block contains only variable declarations");
    return true;
}

static void codegen_stmt(CodegenContext* ctx, AstNode* node) {
    if (!node) return;
    DEBUG("Processing statement type=%d at line %d", node->type, node->line);
    switch (node->type) {
        case AST_ASSIGN:
        case AST_FUNC_CALL:
        case AST_UNARY_OP: {
            char* result;
            codegen_expr(ctx, node, &result, "w");
            free(result);
            break;
        }
        case AST_FUNC_DECL: {
            ctx->current_function = node->data.func_decl.name;
            ctx->temp_count = 0; // Reset temporaries per function
            ctx->label_count = 0; // Reset labels per function
            ctx->has_return = false;
            fprintf(ctx->out, "export function w $%s(", node->data.func_decl.name);
            enter_scope(ctx);
            for (int i = 0; i < node->data.func_decl.param_count; i++) {
                AstNode* param = node->data.func_decl.params[i];
                if (param->type != AST_IDENT) {
                    error(param->line, "Function parameter must be an identifier");
                }
                scope_add_symbol(ctx->current_scope, param->data.ident.name, 1, param->line);
                fprintf(ctx->out, "%s %%%s", (i == 0) ? "w" : "l", param->data.ident.name);
                if (i < node->data.func_decl.param_count - 1) fprintf(ctx->out, ", ");
            }
            fprintf(ctx->out, ") {\n@start\n");
            codegen_stmt(ctx, node->data.func_decl.body);
            if (!ctx->has_return && strcmp(node->data.func_decl.name, "main") != 0) {
                fprintf(stderr, "\033[33mWarning\033[0m at line %d: Function '%s' may not return a value\n",
                        node->line, node->data.func_decl.name);
                fprintf(ctx->out, "\tret 0\n");
            } else if (!ctx->has_return) {
                fprintf(ctx->out, "\tret 0\n");
            }
            fprintf(ctx->out, "}\n\n");
            check_unused_symbols(ctx->current_scope, node->line);
            exit_scope(ctx);
            ctx->current_function = NULL;
            break;
        }
        case AST_VAR_DECL: {
            scope_add_symbol(ctx->current_scope, node->data.var_decl.name, 0, node->line);
            fprintf(ctx->out, "\t%%%s =l alloc4 4\n", node->data.var_decl.name);
            if (node->data.var_decl.init) {
                char* value;
                codegen_expr(ctx, node->data.var_decl.init, &value, "w");
                fprintf(ctx->out, "\tstorew %s, %%%s\n", value, node->data.var_decl.name);
                scope_mark_initialized(ctx->current_scope, node->data.var_decl.name, node->line);
                free(value);
            }
            break;
        }
        case AST_EXTRN_DECL:
            // No-op: external symbols are resolved by the linker, I think..., extrn in still untested
            break;
        case AST_IF: {
            char* cond;
            char* then_label = new_label(ctx);
            char* else_label = new_label(ctx);
            char* end_label = new_label(ctx);
            codegen_expr(ctx, node->data.if_stmt.cond, &cond, "w");
            fprintf(ctx->out, "\tjnz %s, @%s, @%s\n", cond, then_label, else_label);
            free(cond);

            fprintf(ctx->out, "@%s\n", then_label);
            enter_scope(ctx);
            codegen_stmt(ctx, node->data.if_stmt.then_body);
            exit_scope(ctx);
            check_unused_symbols(ctx->current_scope, node->line);
            fprintf(ctx->out, "\tjmp @%s\n", end_label);

            fprintf(ctx->out, "@%s\n", else_label);
            if (node->data.if_stmt.else_body) {
                enter_scope(ctx);
                codegen_stmt(ctx, node->data.if_stmt.else_body);
                exit_scope(ctx);
                check_unused_symbols(ctx->current_scope, node->line);
            }
            fprintf(ctx->out, "\tjmp @%s\n", end_label);

            fprintf(ctx->out, "@%s\n", end_label);
            free(then_label);
            free(else_label);
            free(end_label);
            break;
        }
        case AST_WHILE: {
            char* start_label = new_label(ctx);
            char* body_label = new_label(ctx);
            char* end_label = new_label(ctx);
            fprintf(ctx->out, "@%s\n", start_label);
            char* cond;
            codegen_expr(ctx, node->data.while_stmt.cond, &cond, "w");
            fprintf(ctx->out, "\tjnz %s, @%s, @%s\n", cond, body_label, end_label);
            free(cond);
            fprintf(ctx->out, "@%s\n", body_label);
            enter_scope(ctx);
            codegen_stmt(ctx, node->data.while_stmt.body);
            exit_scope(ctx);
            check_unused_symbols(ctx->current_scope, node->line);
            fprintf(ctx->out, "\tjmp @%s\n", start_label);
            fprintf(ctx->out, "@%s\n", end_label);
            free(start_label);
            free(body_label);
            free(end_label);
            break;
        }
        case AST_RETURN: {
            ctx->has_return = true;
            if (node->data.return_stmt.expr) {
                char* expr;
                codegen_expr(ctx, node->data.return_stmt.expr, &expr, "w");
                fprintf(ctx->out, "\tret %s\n", expr);
                free(expr);
            } else {
                fprintf(ctx->out, "\tret 0\n");
            }
            break;
        }
        case AST_BLOCK: {
            bool is_var_block = is_var_decl_block(node);
            DEBUG("Block is_var_decl_block=%d", is_var_block);
            if (!is_var_block) {
                enter_scope(ctx);
            }
            for (int i = 0; i < node->data.block.stmt_count; i++) {
                codegen_stmt(ctx, node->data.block.stmts[i]);
                DEBUG("Processed statement %d of %d in block", i + 1, node->data.block.stmt_count);
            }
            if (!is_var_block) {
                check_unused_symbols(ctx->current_scope, node->line);
                exit_scope(ctx);
            }
            break;
        }
        default:
            error(node->line, "Unknown statement type %d in codegen", node->type);
    }
}

void codegen_generate(AstNode* node, const char* output_file) {
    CodegenContext ctx = {
        .out = fopen(output_file, "w"),
        .temp_count = 0,
        .label_count = 0,
        .current_scope = scope_create(NULL),
        .current_function = NULL,
        .has_return = false
    };
    init_string_table(&ctx.strings);
    if (!ctx.out) {
        error(0, "Failed to open output file '%s'", output_file);
    }

    // Process all statements to collect strings
    enter_scope(&ctx);
    for (int i = 0; i < node->data.block.stmt_count; i++) {
        codegen_stmt(&ctx, node->data.block.stmts[i]);
        DEBUG("Processed top-level statement %d of %d", i + 1, node->data.block.stmt_count);
    }
    exit_scope(&ctx);

    // Rewind and emit data section
    rewind(ctx.out);
    emit_data_section(&ctx);

    // Reprocess statements for function codegen
    enter_scope(&ctx);
    for (int i = 0; i < node->data.block.stmt_count; i++) {
        codegen_stmt(&ctx, node->data.block.stmts[i]);
    }
    exit_scope(&ctx);

    fclose(ctx.out);
    free_string_table(&ctx.strings);
    scope_free(ctx.current_scope);
}
