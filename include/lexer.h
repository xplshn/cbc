#ifndef CBC_LEXER_H
#define CBC_LEXER_H

typedef enum {
    // TXT
    TOK_EOF = 0,
    TOK_IDENT,
    TOK_NUMBER,
    TOK_STRING,

    // Keywords
    TOK_AUTO,
    TOK_EXTRN,
    TOK_IF,
    TOK_ELSE,
    TOK_WHILE,
    TOK_RETURN,

    // Punctuation
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LBRACE,
    TOK_RBRACE,
    TOK_LBRACKET,
    TOK_RBRACKET,
    TOK_SEMI,
    TOK_COMMA,

    // Operators
    TOK_EQ,       // =
    TOK_LT,       // <
    TOK_PLUS,     // +
    TOK_MINUS,    // -
    TOK_STAR,     // *
    TOK_SLASH,    // /
    TOK_NOT,      // !
    TOK_INC,      // ++

} TokenType;

typedef struct {
    TokenType type;
    char* value;
    int line;
    int column;
} Token;

typedef struct {
    const char* source;
    int pos;
    int line;
    int column;
} Lexer;

Lexer lexer_init(const char* source);
Token lexer_next(Lexer* lexer);
void token_free(Token token);

#endif // CBC_LEXER_H

