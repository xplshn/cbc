#include "codegen.h"
#include "ast.h"
#include "lexer.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// global data definitions
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
    char* type; // "w", "l", "s", "d", or a pointer type like "*b", "*w", "*l"
    int is_param;
    int is_initialized;
    int is_used;
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
    bool has_return;
} CodegenContext;

// fwd declarations
static void init_string_table(StringTable* table);
static void free_string_table(StringTable* table);
static char* add_string(CodegenContext* ctx, const char* value, int line);
static void emit_data_section(CodegenContext* ctx);
static Symbol* symbol_create(char* name, const char* type, int is_param);
static void symbol_free(Symbol* sym);
static Scope* scope_create(Scope* parent);
static void scope_free(Scope* scope);
static void enter_scope(CodegenContext* ctx);
static void exit_scope(CodegenContext* ctx);
static void scope_add_symbol(Scope* scope, char* name, const char* type, int is_param, int line);
static Symbol* scope_find_symbol(Scope* scope, char* name);
static void scope_mark_initialized(Scope* scope, char* name, int line);
static void scope_mark_used(Scope* scope, char* name, int line);
static void check_unused_symbols(Scope* scope, int line);
static char* new_temp(CodegenContext* ctx);
static char* new_label(CodegenContext* ctx);
static const char* get_expr_type(CodegenContext* ctx, AstNode* node);
static void codegen_expr(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type);
static void codegen_lvalue(CodegenContext* ctx, AstNode* node, char** result);
static int codegen_stmt(CodegenContext* ctx, AstNode* node);
static bool is_valid_lvalue(AstNode* node);
static void collect_strings(CodegenContext* ctx, AstNode* node);


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

static void emit_data_section(CodegenContext* ctx) {
    if (!ctx) error(0, "Null codegen context in emit_data_section");
    fprintf(ctx->out, "# Data section\n");
    for (int i = 0; i < ctx->strings.count; i++) {
        if (!ctx->strings.entries[i].value || !ctx->strings.entries[i].label) {
            fprintf(stderr, "\033[33mWarning\033[0m: Null string or label at index %d\n", i);
            continue;
        }
        fprintf(ctx->out, "data $%s = { b ", ctx->strings.entries[i].label);
        const char* p = ctx->strings.entries[i].value;
        if (*p == '\0') {
            fprintf(ctx->out, "0 }\n");
            continue;
        }
        fprintf(ctx->out, "\"");
        for (; *p; p++) {
            if (*p >= 32 && *p <= 126 && *p != '"' && *p != '\\') {
                fputc(*p, ctx->out);
            } else {
                fprintf(ctx->out, "\\%03o", (unsigned char)*p);
            }
        }
        fprintf(ctx->out, "\", b 0 }\n");
    }
    fprintf(ctx->out, "\n");
}

static Symbol* symbol_create(char* name, const char* type, int is_param) {
    Symbol* sym = malloc(sizeof(Symbol));
    sym->name = str_dup(name);
    sym->type = str_dup(type);
    sym->is_param = is_param;
    sym->is_initialized = is_param;
    sym->is_used = 0;
    sym->next = NULL;
    return sym;
}

static void symbol_free(Symbol* sym) {
    if (sym) {
        free(sym->name);
        free(sym->type);
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

static void scope_add_symbol(Scope* scope, char* name, const char* type, int is_param, int line) {
    if (!scope || !name) error(line, "Null scope or name in scope_add_symbol");
    for (Symbol* sym = scope->symbols; sym; sym = sym->next) {
        if (strcmp(sym->name, name) == 0) {
            error(line, "Redefinition of variable '%s' in same scope", name);
        }
    }
    Symbol* new_sym = symbol_create(name, type, is_param);
    new_sym->next = scope->symbols;
    scope->symbols = new_sym;
    DEBUG("Added symbol '%s' type '%s' (param=%d) to scope", name, type, is_param);
}

static Symbol* scope_find_symbol(Scope* scope, char* name) {
    if (!scope || !name) return NULL;
    while (scope) {
        for (Symbol* sym = scope->symbols; sym; sym = sym->next) {
            if (strcmp(sym->name, name) == 0) {
                return sym;
            }
        }
        scope = scope->parent;
    }
    return NULL;
}

static void scope_mark_initialized(Scope* scope, char* name, int line) {
    Symbol* sym = scope_find_symbol(scope, name);
    if (sym) {
        sym->is_initialized = 1;
        DEBUG("Marked symbol '%s' as initialized", name);
    } else {
        error(line, "Cannot mark undefined variable '%s' as initialized", name);
    }
}

static void scope_mark_used(Scope* scope, char* name, int line) {
    Symbol* sym = scope_find_symbol(scope, name);
    if (sym) {
        sym->is_used = 1;
        DEBUG("Marked symbol '%s' as used", name);
    } else {
        // This could be an external function, which is fine.
        // error(line, "Cannot mark undefined variable '%s' as used", name);
    }
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

// TODO: This is not correct. It works, but I want this gone as soon as possible.
static const char* get_expr_type(CodegenContext* ctx, AstNode* node) {
    if (!node) return "w"; // Default to word
    switch (node->type) {
        case AST_NUMBER: return "w";
        case AST_STRING: return "l"; // Pointer
        case AST_IDENT: {
            Symbol* sym = scope_find_symbol(ctx->current_scope, node->data.ident.name);
            if (sym) {
                // Return the base type, not the pointer type itself
                if (strcmp(sym->type, "*l") == 0) return "l";
                if (strcmp(sym->type, "*w") == 0) return "w";
                if (strcmp(sym->type, "*b") == 0) return "w";
                return sym->type;
            }
            return "w";
        }
        case AST_ARRAY_INDEX: {
            AstNode* base_node = node->data.array_index.array;
            if (base_node->type == AST_IDENT) {
                Symbol* sym = scope_find_symbol(ctx->current_scope, base_node->data.ident.name);
                if (sym) {
                    if (strcmp(sym->type, "*l") == 0) return "l"; // e.g. argv is *l, argv[i] is l
                    if (strcmp(sym->type, "*w") == 0) return "w"; // e.g. int* p, p[i] is int (w)
                    if (strcmp(sym->type, "*b") == 0) return "w"; // e.g. char* p, p[i] is char (w)
                }
            }
            return "w"; // Fallback
        }
        case AST_ASSIGN:
            return get_expr_type(ctx, node->data.assign.rhs); // Type of assignment is type of RHS
        case AST_BINARY_OP:
            return "w"; // For now, all binary ops return word
        case AST_UNARY_OP:
            return get_expr_type(ctx, node->data.unary_op.expr);
        case AST_FUNC_CALL:
            if (strcmp(node->data.func_call.name, "malloc") == 0) return "l"; // pointer
            return "w"; // Default return type
        default:
            return "w";
    }
}

static void codegen_number(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    *result = malloc(16);
    snprintf(*result, 16, "%d", node->data.number.value);
}

static void codegen_string(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    char* label = add_string(ctx, node->data.string.value, node->line);
    *result = malloc(strlen(label) + 2);
    snprintf(*result, strlen(label) + 2, "$%s", label);
    free(label);
}

static void codegen_ident(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    Symbol* sym = scope_find_symbol(ctx->current_scope, node->data.ident.name);
    if (!sym) {
        error(node->line, "Undefined variable '%s'", node->data.ident.name);
    }
    if (!sym->is_initialized) {
        fprintf(stderr, "\033[33mWarning\033[0m at line %d: Variable '%s' used before initialization\n",
                node->line, node->data.ident.name);
    }
    scope_mark_used(ctx->current_scope, node->data.ident.name, node->line);
    *result = new_temp(ctx);
    if (sym->is_param) {
        fprintf(ctx->out, "\t%s =%s copy %%%s\n", *result, expected_type, node->data.ident.name);
    } else {
        char load_suffix = 'w';
        if (strchr(sym->type, '*') || strcmp(sym->type, "l") == 0) {
            load_suffix = 'l';
        }
        fprintf(ctx->out, "\t%s =%s load%c %%%s\n", *result, expected_type, load_suffix, node->data.ident.name);
    }
}

static void codegen_array_index(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    char* addr;
    codegen_lvalue(ctx, node, &addr);
    *result = new_temp(ctx);

    const char* load_inst = "loadsw";
    const char* ret_type = "w";

    if (node->data.array_index.array->type == AST_IDENT) {
        Symbol* sym = scope_find_symbol(ctx->current_scope, node->data.array_index.array->data.ident.name);
        if (sym) {
            if (strcmp(sym->type, "*l") == 0) {
                load_inst = "loadl";
                ret_type = "l";
            } else if (strcmp(sym->type, "*w") == 0) {
                load_inst = "loadw";
                ret_type = "w";
            } else if (strcmp(sym->type, "*b") == 0) {
                load_inst = "loadsb";
                ret_type = "w";
            }
        }
    }

    fprintf(ctx->out, "\t%s =%s %s %s\n", *result, ret_type, load_inst, addr);
    free(addr);
}

static void codegen_lvalue(CodegenContext* ctx, AstNode* node, char** result) {
    if (!is_valid_lvalue(node)) {
        error(node->line, "Invalid l-value expression");
    }
    switch (node->type) {
        case AST_IDENT: {
            Symbol* sym = scope_find_symbol(ctx->current_scope, node->data.ident.name);
            if (!sym) {
                error(node->line, "Undefined variable '%s' used as l-value", node->data.ident.name);
            }
            *result = malloc(strlen(node->data.ident.name) + 2);
            sprintf(*result, "%%%s", node->data.ident.name);
            scope_mark_initialized(ctx->current_scope, node->data.ident.name, node->line);
            scope_mark_used(ctx->current_scope, node->data.ident.name, node->line);
            break;
        }
        case AST_ARRAY_INDEX: {
            char* base;
            char* index;

            codegen_expr(ctx, node->data.array_index.array, &base, "l");
            codegen_expr(ctx, node->data.array_index.index, &index, "w");

            int element_size = 1;
            if (node->data.array_index.array->type == AST_IDENT) {
                Symbol* sym = scope_find_symbol(ctx->current_scope, node->data.array_index.array->data.ident.name);
                if (sym) {
                    if (strcmp(sym->type, "*b") == 0) element_size = 1;
                    else if (strcmp(sym->type, "*w") == 0) element_size = 4;
                    else if (strcmp(sym->type, "*l") == 0) element_size = 8;
                    else {
                         fprintf(stderr, "\033[33mWarning\033[0m at line %d: Array index on non-pointer type '%s', assuming byte pointer\n", node->line, sym->type);
                    }
                }
            }

            *result = new_temp(ctx);
            char* index_l = new_temp(ctx);
            fprintf(ctx->out, "\t%s =l extsw %s\n", index_l, index);

            if (element_size > 1) {
                char* offset = new_temp(ctx);
                fprintf(ctx->out, "\t%s =l mul %s, %d\n", offset, index_l, element_size);
                fprintf(ctx->out, "\t%s =l add %s, %s\n", *result, base, offset);
                free(offset);
            } else {
                fprintf(ctx->out, "\t%s =l add %s, %s\n", *result, base, index_l);
            }
            free(base);
            free(index);
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

    const char* rvalue_type = "w";
    char store_op = 'w';

    if (node->data.assign.lhs->type == AST_IDENT) {
        Symbol* sym = scope_find_symbol(ctx->current_scope, node->data.assign.lhs->data.ident.name);
        bool is_malloc = (node->data.assign.rhs->type == AST_FUNC_CALL &&
                          strcmp(node->data.assign.rhs->data.func_call.name, "malloc") == 0);

        if (sym) {
            if (is_malloc) {
                free(sym->type);
                sym->type = str_dup("*b");
                DEBUG("Inferred type of '%s' to '*b' from malloc assignment", sym->name);
            }

            if (strchr(sym->type, '*') || strcmp(sym->type, "l") == 0) {
                rvalue_type = "l";
                store_op = 'l';
            }
        }
    } else if (node->data.assign.lhs->type == AST_ARRAY_INDEX) {
        store_op = 'b';
        rvalue_type = "w";
    }

    char* lvalue;
    char* rvalue;
    codegen_lvalue(ctx, node->data.assign.lhs, &lvalue);
    codegen_expr(ctx, node->data.assign.rhs, &rvalue, rvalue_type);

    fprintf(ctx->out, "\tstore%c %s, %s\n", store_op, rvalue, lvalue);
    *result = rvalue;
    free(lvalue);
}

static void codegen_binary_op(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    char* left;
    char* right;
    codegen_expr(ctx, node->data.binary_op.left, &left, "w");
    codegen_expr(ctx, node->data.binary_op.right, &right, "w");
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
    fprintf(ctx->out, "\t%s =w %s %s, %s\n", *result, op_str, left, right);
    free(left);
    free(right);
}

static void codegen_unary_op(CodegenContext* ctx, AstNode* node, char** result, const char* expected_type) {
    char* expr_val;
    codegen_expr(ctx, node->data.unary_op.expr, &expr_val, "w");
    *result = new_temp(ctx);
    switch (node->data.unary_op.op) {
        case TOK_NOT:
            fprintf(ctx->out, "\t%s =w ceqw %s, 0\n", *result, expr_val);
            break;
        case TOK_COMPLEMENT:
            fprintf(ctx->out, "\t%s =w xor %s, -1\n", *result, expr_val);
            break;
        case TOK_INC:
        case TOK_DEC: {
            if (!is_valid_lvalue(node->data.unary_op.expr)) {
                error(node->line, "%s operator applied to non-l-value",
                      node->data.unary_op.op == TOK_INC ? "Increment" : "Decrement");
            }
            char* lvalue;
            char* new_val = new_temp(ctx);
            codegen_lvalue(ctx, node->data.unary_op.expr, &lvalue);
            const char* op = node->data.unary_op.op == TOK_INC ? "add" : "sub";
            fprintf(ctx->out, "\t%s =w %s %s, 1\n", new_val, op, expr_val);
            fprintf(ctx->out, "\tstorew %s, %s\n", new_val, lvalue);
            *result = expr_val;
            free(lvalue);
            free(new_val);
            return;
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
        arg_types[i] = get_expr_type(ctx, node->data.func_call.args[i]);
        codegen_expr(ctx, node->data.func_call.args[i], &args[i], arg_types[i]);
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

static int codegen_stmt(CodegenContext* ctx, AstNode* node) {
    if (!node) return 0;
    int terminates = 0;
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
            ctx->temp_count = 0;
            ctx->label_count = 0;
            ctx->has_return = false;

            bool is_main = strcmp(node->data.func_decl.name, "main") == 0;

            fprintf(ctx->out, "export function w $%s(", node->data.func_decl.name);
            enter_scope(ctx);

            if (is_main) {
                 if (node->data.func_decl.param_count == 0) {
                    fprintf(ctx->out, "w %%argc, l %%argv");
                    scope_add_symbol(ctx->current_scope, "argc", "w", 1, node->line);
                    scope_add_symbol(ctx->current_scope, "argv", "*l", 1, node->line);
                 } else if (node->data.func_decl.param_count == 2) {
                    fprintf(ctx->out, "w %%argc, l %%argv");
                    scope_add_symbol(ctx->current_scope, "argc", "w", 1, node->line);
                    scope_add_symbol(ctx->current_scope, "argv", "*l", 1, node->line);
                 }
            } else {
                for (int i = 0; i < node->data.func_decl.param_count; i++) {
                    AstNode* param = node->data.func_decl.params[i];
                    if (param->type != AST_IDENT) {
                        error(param->line, "Function parameter must be an identifier");
                    }

                    const char* param_type = "w";
                    if (strcmp(node->data.func_decl.name, "read_name") == 0 ||
                        strcmp(node->data.func_decl.name, "print_name") == 0) {
                        if (i == 0) param_type = "l";
                    }

                    scope_add_symbol(ctx->current_scope, param->data.ident.name, param_type, 1, param->line);
                    fprintf(ctx->out, "%s %%%s", param_type, param->data.ident.name);
                    if (i < node->data.func_decl.param_count - 1) fprintf(ctx->out, ", ");
                }
            }
            fprintf(ctx->out, ") {\n@start\n");
            codegen_stmt(ctx, node->data.func_decl.body);
            if (!ctx->has_return) {
                 if (is_main) {
                     fprintf(ctx->out, "\tret 0\n");
                 } else {
                    fprintf(stderr, "\033[33mWarning\033[0m at line %d: Function '%s' may not return a value\n",
                        node->line, node->data.func_decl.name);
                    fprintf(ctx->out, "\tret 0\n");
                 }
            }
            fprintf(ctx->out, "}\n\n");
            check_unused_symbols(ctx->current_scope, node->line);
            exit_scope(ctx);
            ctx->current_function = NULL;
            break;
        }
        case AST_VAR_DECL: {
            scope_add_symbol(ctx->current_scope, node->data.var_decl.name, "w", 0, node->line);
            fprintf(ctx->out, "\t%%%s =l alloc4 4\n", node->data.var_decl.name);
            if (node->data.var_decl.init) {
                char* result;
                AstNode* assign_node = ast_assign(ast_ident(str_dup(node->data.var_decl.name), node->line), node->data.var_decl.init, node->line);
                codegen_expr(ctx, assign_node, &result, "w");
                free(result);
                assign_node->data.assign.rhs = NULL;
                ast_free(assign_node);
            }
            break;
        }
        case AST_EXTRN_DECL:
            break; // No-op: handled by linker // TODO: We should still mark this symbol, so that the user can't overwrite it
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
            int then_terminates = codegen_stmt(ctx, node->data.if_stmt.then_body);
            check_unused_symbols(ctx->current_scope, node->line);
            exit_scope(ctx);
            if (!then_terminates) {
                fprintf(ctx->out, "\tjmp @%s\n", end_label);
            }

            fprintf(ctx->out, "@%s\n", else_label);
            int else_terminates = 0;
            if (node->data.if_stmt.else_body) {
                enter_scope(ctx);
                else_terminates = codegen_stmt(ctx, node->data.if_stmt.else_body);
                check_unused_symbols(ctx->current_scope, node->line);
                exit_scope(ctx);
            }
            if (!else_terminates) {
                fprintf(ctx->out, "\tjmp @%s\n", end_label);
            }

            fprintf(ctx->out, "@%s\n", end_label);
            free(then_label);
            free(else_label);
            free(end_label);
            terminates = then_terminates && (node->data.if_stmt.else_body && else_terminates);
            break;
        }
        case AST_WHILE: {
            char* start_label = new_label(ctx);
            char* body_label = new_label(ctx);
            char* end_label = new_label(ctx);
            fprintf(ctx->out, "\tjmp @%s\n", start_label);
            fprintf(ctx->out, "@%s\n", body_label);
            enter_scope(ctx);
            codegen_stmt(ctx, node->data.while_stmt.body);
            check_unused_symbols(ctx->current_scope, node->line);
            exit_scope(ctx);

            fprintf(ctx->out, "@%s\n", start_label);
            char* cond;
            codegen_expr(ctx, node->data.while_stmt.cond, &cond, "w");
            fprintf(ctx->out, "\tjnz %s, @%s, @%s\n", cond, body_label, end_label);
            free(cond);

            fprintf(ctx->out, "@%s\n", end_label);
            free(start_label);
            free(body_label);
            free(end_label);
            terminates = 0;
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
            terminates = 1;
            break;
        }
        case AST_BLOCK: {
            for (int i = 0; i < node->data.block.stmt_count; i++) {
                if (terminates) {
                    fprintf(stderr, "\033[33mWarning\033[0m at line %d: Unreachable code after return\n",
                            node->data.block.stmts[i]->line);
                    break;
                }
                terminates = codegen_stmt(ctx, node->data.block.stmts[i]);
            }
            break;
        }
        default:
            error(node->line, "Unknown statement type %d in codegen", node->type);
    }
    return terminates;
}

// string collection pass
static void collect_strings(CodegenContext* ctx, AstNode* node) {
    if (!node) return;
    switch (node->type) {
        case AST_STRING:
            add_string(ctx, node->data.string.value, node->line);
            break;
        case AST_ASSIGN:
            collect_strings(ctx, node->data.assign.lhs);
            collect_strings(ctx, node->data.assign.rhs);
            break;
        case AST_BINARY_OP:
            collect_strings(ctx, node->data.binary_op.left);
            collect_strings(ctx, node->data.binary_op.right);
            break;
        case AST_UNARY_OP:
            collect_strings(ctx, node->data.unary_op.expr);
            break;
        case AST_FUNC_CALL:
            for (int i = 0; i < node->data.func_call.arg_count; i++) {
                collect_strings(ctx, node->data.func_call.args[i]);
            }
            break;
        case AST_ARRAY_INDEX:
            collect_strings(ctx, node->data.array_index.array);
            collect_strings(ctx, node->data.array_index.index);
            break;
        case AST_FUNC_DECL:
            collect_strings(ctx, node->data.func_decl.body);
            break;
        case AST_VAR_DECL:
            if (node->data.var_decl.init) {
                collect_strings(ctx, node->data.var_decl.init);
            }
            break;
        case AST_IF:
            collect_strings(ctx, node->data.if_stmt.cond);
            collect_strings(ctx, node->data.if_stmt.then_body);
            if (node->data.if_stmt.else_body) {
                collect_strings(ctx, node->data.if_stmt.else_body);
            }
            break;
        case AST_WHILE:
            collect_strings(ctx, node->data.while_stmt.cond);
            collect_strings(ctx, node->data.while_stmt.body);
            break;
        case AST_RETURN:
            if (node->data.return_stmt.expr) {
                collect_strings(ctx, node->data.return_stmt.expr);
            }
            break;
        case AST_BLOCK:
            for (int i = 0; i < node->data.block.stmt_count; i++) {
                collect_strings(ctx, node->data.block.stmts[i]);
            }
            break;
        case AST_NUMBER:
        case AST_IDENT:
        case AST_EXTRN_DECL:
            break;
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

    collect_strings(&ctx, node);

    emit_data_section(&ctx);

    if (node->type == AST_BLOCK) {
        for (int i = 0; i < node->data.block.stmt_count; i++) {
            codegen_stmt(&ctx, node->data.block.stmts[i]);
        }
    } else {
        codegen_stmt(&ctx, node);
    }

    fclose(ctx.out);
    free_string_table(&ctx.strings);
    scope_free(ctx.current_scope);
}
