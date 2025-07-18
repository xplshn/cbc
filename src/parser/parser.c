#include "parser.h"
#include "util.h"
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// fwd declarations
static void advance(Parser* parser);
static AstNode* parse_expr(Parser* parser);
static AstNode* parse_stmt(Parser* parser);

Parser parser_init(Lexer* lexer) {
    Parser parser = { .lexer = lexer };
    parser.current = (Token){ .type = TOK_EOF, .value = NULL, .line = 0, .column = 0 };
    parser.previous = (Token){ .type = TOK_EOF, .value = NULL, .line = 0, .column = 0 };
    advance(&parser);
    return parser;
}

static void advance(Parser* parser) {
    token_free(parser->previous);
    parser->previous = parser->current;
    parser->current = lexer_next(parser->lexer);
    DEBUG("Advanced to token: type=%d, value='%s'", parser->current.type, parser->current.value ? parser->current.value : "");
}

static bool check(Parser* parser, TokenType type) {
    return parser->current.type == type;
}

static bool match(Parser* parser, TokenType type) {
    if (!check(parser, type)) return false;
    advance(parser);
    return true;
}

static void expect(Parser* parser, TokenType type, const char* message) {
    if (parser->current.type == type) {
        advance(parser);
        return;
    }
    error(parser->current.line, "%s. Expected token type %d, got %d", message, type, parser->current.type);
}

static AstNode* parse_primary(Parser* parser) {
    DEBUG("Parsing primary");
    if (match(parser, TOK_NUMBER)) {
        return ast_number(atoi(parser->previous.value), parser->previous.line);
    }
    if (match(parser, TOK_STRING)) {
        return ast_string(str_dup(parser->previous.value), parser->previous.line);
    }
    if (match(parser, TOK_IDENT)) {
        char* name = str_dup(parser->previous.value);
        int line = parser->previous.line;

        if (match(parser, TOK_LPAREN)) {
            AstNode** args = NULL;
            int arg_count = 0;
            if (!check(parser, TOK_RPAREN)) {
                args = malloc(sizeof(AstNode*) * 10);
                do {
                    args[arg_count++] = parse_expr(parser);
                } while (match(parser, TOK_COMMA));
            }
            expect(parser, TOK_RPAREN, "Expected ')' after function arguments");
            return ast_func_call(name, args, arg_count, line);
        }
        return ast_ident(name, line);
    }
    if (match(parser, TOK_LPAREN)) {
        AstNode* expr = parse_expr(parser);
        expect(parser, TOK_RPAREN, "Expected ')' after expression");
        return expr;
    }
    if (match(parser, TOK_NOT)) {
        return ast_unary_op(TOK_NOT, parse_primary(parser), parser->previous.line);
    }

    error(parser->current.line, "Expected an expression.");
    return NULL;
}

static AstNode* parse_postfix(Parser* parser) {
    AstNode* expr = parse_primary(parser);

    while (true) {
        if (match(parser, TOK_LBRACKET)) {
            AstNode* index = parse_expr(parser);
            expect(parser, TOK_RBRACKET, "Expected ']' after array index");
            expr = ast_array_index(expr, index, parser->previous.line);
        } else if (match(parser, TOK_INC)) {
            expr = ast_unary_op(TOK_INC, expr, parser->previous.line);
        } else {
            break;
        }
    }
    return expr;
}

static int get_precedence(TokenType op) {
    switch (op) {
        case TOK_EQ: return 1;
        case TOK_LT: return 2;
        case TOK_PLUS:
        case TOK_MINUS: return 3;
        case TOK_STAR:
        case TOK_SLASH: return 4;
        default: return 0;
    }
}

static AstNode* parse_binary_op(Parser* parser, int prec) {
    AstNode* left = parse_postfix(parser);

    while (true) {
        TokenType op = parser->current.type;
        int op_prec = get_precedence(op);

        if (op_prec == 0 || op_prec < prec) break;

        bool right_assoc = (op == TOK_EQ);
        int next_prec = right_assoc ? op_prec : op_prec + 1;

        advance(parser);
        AstNode* right = parse_binary_op(parser, next_prec);

        if (op == TOK_EQ) {
            left = ast_assign(left, right, parser->previous.line);
        } else {
            left = ast_binary_op(op, left, right, parser->previous.line);
        }
    }
    return left;
}

static AstNode* parse_expr(Parser* parser) {
    DEBUG("Parsing expression");
    return parse_binary_op(parser, 1);
}

static AstNode* parse_block(Parser* parser) {
    int line = parser->current.line;
    AstNode** stmts = malloc(sizeof(AstNode*) * 10);
    int stmt_count = 0;
    while (!check(parser, TOK_RBRACE) && !check(parser, TOK_EOF)) {
        stmts[stmt_count++] = parse_stmt(parser);
    }
    expect(parser, TOK_RBRACE, "Expected '}' after block");
    return ast_block(stmts, stmt_count, line);
}

static AstNode* parse_stmt(Parser* parser) {
    DEBUG("Parsing statement");
    if (match(parser, TOK_IF)) {
        int line = parser->previous.line;
        expect(parser, TOK_LPAREN, "Expected '(' after 'if'");
        AstNode* cond = parse_expr(parser);
        expect(parser, TOK_RPAREN, "Expected ')' after if condition");
        AstNode* then_body = parse_stmt(parser);
        AstNode* else_body = NULL;
        if (match(parser, TOK_ELSE)) {
            else_body = parse_stmt(parser);
        }
        return ast_if(cond, then_body, else_body, line);
    }
    if (match(parser, TOK_WHILE)) {
        int line = parser->previous.line;
        expect(parser, TOK_LPAREN, "Expected '(' after 'while'");
        AstNode* cond = parse_expr(parser);
        expect(parser, TOK_RPAREN, "Expected ')' after while condition");
        AstNode* body = parse_stmt(parser);
        return ast_while(cond, body, line);
    }
    if (match(parser, TOK_RETURN)) {
        int line = parser->previous.line;
        AstNode* expr = NULL;
        if (!check(parser, TOK_SEMI)) {
            expr = parse_expr(parser);
        }
        expect(parser, TOK_SEMI, "Expected ';' after return value");
        return ast_return(expr, line);
    }
    if (match(parser, TOK_LBRACE)) {
        return parse_block(parser);
    }
    if (match(parser, TOK_EXTRN)) {
        int line = parser->previous.line;
        expect(parser, TOK_IDENT, "Expected identifier after 'extrn'");
        char* name = str_dup(parser->previous.value);
        expect(parser, TOK_SEMI, "Expected ';' after extrn declaration");
        return ast_extrn_decl(name, line);
    }
    if (match(parser, TOK_AUTO)) {
        int line = parser->previous.line;
        AstNode** stmts = malloc(sizeof(AstNode*) * 10);
        int stmt_count = 0;
        do {
            expect(parser, TOK_IDENT, "Expected identifier after 'auto' or ','");
            char* name = str_dup(parser->previous.value);
            AstNode* init = NULL;
            if (match(parser, TOK_EQ)) {
                init = parse_expr(parser);
            }
            stmts[stmt_count++] = ast_var_decl(name, init, line);
        } while (match(parser, TOK_COMMA));
        expect(parser, TOK_SEMI, "Expected ';' after auto declaration");
        if (stmt_count == 1) {
            AstNode* single_stmt = stmts[0];
            free(stmts);
            return single_stmt;
        }
        return ast_block(stmts, stmt_count, line);
    }

    AstNode* expr = parse_expr(parser);
    expect(parser, TOK_SEMI, "Expected ';' after expression statement");
    return expr;
}

AstNode* parser_parse(Parser* parser) {
    AstNode** stmts = malloc(sizeof(AstNode*) * 20);
    int stmt_count = 0;
    int line = parser->current.line;

    while (!check(parser, TOK_EOF)) {
        if (check(parser, TOK_IDENT) && parser->lexer->source[parser->lexer->pos] == '(') {
            int fn_line = parser->current.line;
            expect(parser, TOK_IDENT, "Expected function name");
            char* name = str_dup(parser->previous.value);
            expect(parser, TOK_LPAREN, "Expected '(' after function name");

            AstNode** params = NULL;
            int param_count = 0;
            if (!check(parser, TOK_RPAREN)) {
                params = malloc(sizeof(AstNode*) * 10);
                do {
                    expect(parser, TOK_IDENT, "Expected parameter name");
                    params[param_count++] = ast_ident(str_dup(parser->previous.value), parser->previous.line);
                } while (match(parser, TOK_COMMA));
            }
            expect(parser, TOK_RPAREN, "Expected ')' after parameters");

            expect(parser, TOK_LBRACE, "Expected '{' to start function body");
            AstNode* body = parse_block(parser);
            stmts[stmt_count++] = ast_func_decl(name, params, param_count, body, fn_line);
        } else {
            stmts[stmt_count++] = parse_stmt(parser);
        }
    }
    return ast_block(stmts, stmt_count, line);
}

void parser_free(Parser* parser) {
    token_free(parser->current);
    token_free(parser->previous);
}




