#include "lexer.h"
#include "util.h"
#include "utf8.h"
#include <ctype.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

Lexer lexer_init(const char* source) {
    if (!source) {
        error(0, "Null source input to lexer");
    }
    return (Lexer){
        .source = source,
        .pos = 0,
        .line = 1,
        .column = 1
    };
}

static char peek(Lexer* lexer) {
    if (!lexer->source) {
        error(lexer->line, "Null source in lexer");
    }
    return lexer->source[lexer->pos];
}

static char advance(Lexer* lexer) {
    if (!lexer->source) {
        error(lexer->line, "Null source in lexer");
    }
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
    while (true) {
        char c = peek(lexer);
        if (isspace(c)) {
            advance(lexer);
            continue;
        }
        if (c == '/' && lexer->source[lexer->pos + 1] == '/') {
            advance(lexer); // Consume first '/'
            advance(lexer); // Consume second '/'
            while (peek(lexer) != '\n' && peek(lexer) != '\0') {
                advance(lexer);
            }
            continue;
        }
        break;
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

static size_t decodechar(Lexer* lexer, uint32_t* chr, int line) {
    const char* s = lexer->source + lexer->pos;
    const unsigned char* p = (const unsigned char*)s;
    uint32_t c;

    if (*p == '\0') {
        error(line, "Unexpected EOF in character literal");
    }

    if (*p == '\\') {
        ++p;
        advance(lexer); // Consume '\'
        if (*p == '\0') {
            error(line, "Unexpected EOF after escape character");
        }
        size_t consumed = 1; // Count '\'
        switch (*p) {
            case '\'': c = '\''; ++p; advance(lexer); consumed++; break;
            case '"':  c = '"';  ++p; advance(lexer); consumed++; break;
            case '?':  c = '?';  ++p; advance(lexer); consumed++; break;
            case '\\': c = '\\'; ++p; advance(lexer); consumed++; break;
            case 'a':  c = '\a'; ++p; advance(lexer); consumed++; break;
            case 'b':  c = '\b'; ++p; advance(lexer); consumed++; break;
            case 'f':  c = '\f'; ++p; advance(lexer); consumed++; break;
            case 'n':  c = '\n'; ++p; advance(lexer); consumed++; break;
            case 'r':  c = '\r'; ++p; advance(lexer); consumed++; break;
            case 't':  c = '\t'; ++p; advance(lexer); consumed++; break;
            case 'v':  c = '\v'; ++p; advance(lexer); consumed++; break;
            case 'x':
                ++p; advance(lexer); consumed++;
                if (*p == '\0' || !isxdigit(*p)) {
                    error(line, "Invalid hex escape sequence");
                }
                c = 0;
                do {
                    c = c * 16 + (*p > '9' ? 10 + tolower(*p) - 'a' : *p - '0');
                    ++p;
                    advance(lexer);
                    consumed++;
                } while (*p && isxdigit(*p));
                break;
            default:
                if (!(*p >= '0' && *p <= '7')) {
                    error(line, "Invalid escape sequence '\\%c'", *p);
                }
                c = 0;
                int i = 0;
                do {
                    c = c * 8 + (*p - '0');
                    ++p;
                    advance(lexer);
                    consumed++;
                } while (i++ < 2 && *p && (*p >= '0' && *p <= '7'));
                if (c > 255) {
                    error(line, "Octal escape sequence out of range");
                }
                break;
        }
        *chr = c;
        return consumed;
    } else {
        int e;
        const unsigned char* next = (const unsigned char*)utf8_decode((void*)p, &c, &e);
        if (e) {
            error(line, "Invalid UTF-8 sequence");
        }
        if (c > 255) {
            error(line, "Unicode character out of range for B (use ASCII)");
        }
        size_t n = next - p;
        for (size_t i = 0; i < n; i++) {
            if (peek(lexer) == '\0') {
                error(line, "Unexpected EOF in UTF-8 sequence");
            }
            advance(lexer);
        }
        *chr = c;
        return n;
    }
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
    advance(lexer); // Consume opening '"'
    char* buffer = malloc(256); // Dynamic resizing could be added
    int len = 0;
    int line = lexer->line;
    while (peek(lexer) != '"' && peek(lexer) != '\0') {
        if (len >= 255) error(lexer->line, "String literal too long");
        uint32_t c;
        decodechar(lexer, &c, line);
        if (c > 255) error(line, "Character out of range in string");
        buffer[len++] = (char)c;
    }
    if (peek(lexer) == '\0') {
        free(buffer);
        error(lexer->line, "Unterminated string literal");
    }
    buffer[len] = '\0';
    char* value = str_dup(buffer);
    free(buffer);
    advance(lexer); // Consume closing '"'
    return make_token(lexer, TOK_STRING, value);
}

static Token character(Lexer* lexer) {
    advance(lexer); // Consume opening "'"
    int line = lexer->line;
    if (peek(lexer) == '\0' || peek(lexer) == '\'') {
        error(line, "Empty character literal");
    }
    uint32_t c;
    decodechar(lexer, &c, line);
    if (c > 255) error(line, "Character out of range");
    if (peek(lexer) != '\'') {
        error(line, "Expected closing single quote for character literal");
    }
    advance(lexer); // Consume closing "'"
    char* str = malloc(16);
    snprintf(str, 16, "%d", (unsigned char)c);
    return make_token(lexer, TOK_CHAR, str);
}

static Token make_operator(Lexer* lexer, TokenType single, TokenType double1, char c1, TokenType double2, char c2) {
    if (peek(lexer) == c1) {
        advance(lexer);
        return make_token(lexer, double1, NULL);
    }
    if (c2 && peek(lexer) == c2) {
        advance(lexer);
        return make_token(lexer, double2, NULL);
    }
    return make_token(lexer, single, NULL);
}

Token lexer_next(Lexer* lexer) {
    skip_whitespace(lexer);

    char c = peek(lexer);
    if (c == '\0') {
        Token tok = make_token(lexer, TOK_EOF, NULL);
        printf("Token: type=%d, value=%s, line=%d, col=%d\n",
       tok.type, tok.value ? tok.value : "null", tok.line, tok.column);
        if (tok.type == TOK_CHAR) {
            printf("debug: Character value (ASCII %d)\n", tok.value ? atoi(tok.value) : 0);
        }
        return tok;
    }

    Token tok;
    if (isalpha(c) || c == '_') {
        tok = identifier(lexer);
    } else if (isdigit(c)) {
        tok = number(lexer);
    } else if (c == '"') {
        tok = string(lexer);
    } else if (c == '\'') {
        tok = character(lexer);
    } else {
        advance(lexer);
        switch (c) {
            case '(': tok = make_token(lexer, TOK_LPAREN, NULL); break;
            case ')': tok = make_token(lexer, TOK_RPAREN, NULL); break;
            case '{': tok = make_token(lexer, TOK_LBRACE, NULL); break;
            case '}': tok = make_token(lexer, TOK_RBRACE, NULL); break;
            case '[': tok = make_token(lexer, TOK_LBRACKET, NULL); break;
            case ']': tok = make_token(lexer, TOK_RBRACKET, NULL); break;
            case ';': tok = make_token(lexer, TOK_SEMI, NULL); break;
            case ',': tok = make_token(lexer, TOK_COMMA, NULL); break;
            case '*': tok = make_token(lexer, TOK_STAR, NULL); break;
            case '/': tok = make_token(lexer, TOK_SLASH, NULL); break;
            case '~': tok = make_token(lexer, TOK_COMPLEMENT, NULL); break;
            case '&': tok = make_token(lexer, TOK_AND, NULL); break;
            case '|': tok = make_token(lexer, TOK_OR, NULL); break;
            case '^': tok = make_token(lexer, TOK_XOR, NULL); break;
            case '=': tok = make_operator(lexer, TOK_EQ, TOK_EQEQ, '=', 0, 0); break;
            case '<': tok = make_operator(lexer, TOK_LT, TOK_SHL, '<', TOK_LTE, '='); break;
            case '>': tok = make_operator(lexer, TOK_GT, TOK_SHR, '>', TOK_GTE, '='); break;
            case '!': tok = make_operator(lexer, TOK_NOT, TOK_NEQ, '=', 0, 0); break;
            case '+': tok = make_operator(lexer, TOK_PLUS, TOK_INC, '+', 0, 0); break;
            case '-': tok = make_operator(lexer, TOK_MINUS, TOK_DEC, '-', 0, 0); break;
            default:
                error(lexer->line, "Unexpected character: %c", c);
                tok = make_token(lexer, TOK_EOF, NULL);
                break;
        }
    }

    printf("Token: type=%d, value=%s, line=%d, col=%d\n",
           tok.type, tok.value ? tok.value : "null", tok.line, tok.column);
    return tok;
}

void token_free(Token token) {
    if (token.value) {
        free(token.value);
        token.value = NULL; // Prevent double-free
    }
}

