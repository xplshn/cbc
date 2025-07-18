#include "lexer.h"
#include "util.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

Lexer lexer_init(const char* source) {
    return (Lexer){
        .source = source,
        .pos = 0,
        .line = 1,
        .column = 1
    };
}

static char peek(Lexer* lexer) {
    return lexer->source[lexer->pos];
}

static char advance(Lexer* lexer) {
    char c = lexer->source[lexer->pos];
    if (c != '\0') {
        lexer->pos++;
        if (c == '\n') {
            lexer->line++;
            lexer->column = 1;
        } else {
            lexer->column++;
        }
    }
    return c;
}

static void skip_whitespace(Lexer* lexer) {
    while (isspace(peek(lexer))) {
        advance(lexer);
    }
}

static Token make_token(Lexer* lexer, TokenType type, char* value) {
    return (Token){
        .type = type,
        .value = value,
        .line = lexer->line,
        .column = lexer->column
    };
}

static Token identifier(Lexer* lexer) {
    int start = lexer->pos;
    while (isalnum(peek(lexer)) || peek(lexer) == '_') {
        advance(lexer);
    }
    int len = lexer->pos - start;
    char* value = malloc(len + 1);
    strncpy(value, lexer->source + start, len);
    value[len] = '\0';

    TokenType type = TOK_IDENT;
    if (strcmp(value, "auto") == 0) type = TOK_AUTO;
    else if (strcmp(value, "extrn") == 0) type = TOK_EXTRN;
    else if (strcmp(value, "if") == 0) type = TOK_IF;
    else if (strcmp(value, "else") == 0) type = TOK_ELSE;
    else if (strcmp(value, "while") == 0) type = TOK_WHILE;
    else if (strcmp(value, "return") == 0) type = TOK_RETURN;

    if (type != TOK_IDENT) {
        free(value);
        value = NULL;
    }
    return make_token(lexer, type, value);
}

static Token number(Lexer* lexer) {
    int start = lexer->pos;
    while (isdigit(peek(lexer))) {
        advance(lexer);
    }
    int len = lexer->pos - start;
    char* value = malloc(len + 1);
    strncpy(value, lexer->source + start, len);
    value[len] = '\0';
    return make_token(lexer, TOK_NUMBER, value);
}

static Token string(Lexer* lexer) {
    int start = lexer->pos;
    while (peek(lexer) != '"' && peek(lexer) != '\0') {
        advance(lexer);
    }

    if (peek(lexer) == '\0') {
        error(lexer->line, "Unterminated string literal.");
    }

    int len = lexer->pos - start;
    char* value = malloc(len + 1);
    strncpy(value, lexer->source + start, len);
    value[len] = '\0';

    advance(lexer); // Consume closing '"'
    return make_token(lexer, TOK_STRING, value);
}

Token lexer_next(Lexer* lexer) {
    skip_whitespace(lexer);

    char c = peek(lexer);
    if (c == '\0') return make_token(lexer, TOK_EOF, NULL);
    if (isalpha(c) || c == '_') return identifier(lexer);
    if (isdigit(c)) return number(lexer);

    advance(lexer);
    switch (c) {
        case '(': return make_token(lexer, TOK_LPAREN, NULL);
        case ')': return make_token(lexer, TOK_RPAREN, NULL);
        case '{': return make_token(lexer, TOK_LBRACE, NULL);
        case '}': return make_token(lexer, TOK_RBRACE, NULL);
        case '[': return make_token(lexer, TOK_LBRACKET, NULL);
        case ']': return make_token(lexer, TOK_RBRACKET, NULL);
        case ';': return make_token(lexer, TOK_SEMI, NULL);
        case ',': return make_token(lexer, TOK_COMMA, NULL);
        case '=': return make_token(lexer, TOK_EQ, NULL);
        case '<': return make_token(lexer, TOK_LT, NULL);
        case '*': return make_token(lexer, TOK_STAR, NULL);
        case '/': return make_token(lexer, TOK_SLASH, NULL);
        case '!': return make_token(lexer, TOK_NOT, NULL);
        case '+':
            if (peek(lexer) == '+') {
                advance(lexer);
                return make_token(lexer, TOK_INC, NULL);
            }
            return make_token(lexer, TOK_PLUS, NULL);
        case '-': return make_token(lexer, TOK_MINUS, NULL);
        case '"': return string(lexer);
    }

    error(lexer->line, "Unexpected character: %c", c);
    return make_token(lexer, TOK_EOF, NULL); // Unreachable
}

void token_free(Token token) {
    if (token.value) free(token.value);
}

