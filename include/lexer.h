#ifndef CBC_LEXER_H
#define CBC_LEXER_H

typedef enum {
    // TXT
    TOK_EOF = 0,
    TOK_IDENT,
    TOK_NUMBER,
    TOK_STRING,
    TOK_QUOTE, // '
    TOK_CHAR, // 'c', where c an arbitrary is a character

    // TODO: Implement a simple macro-system using comments
    TOK_COMMENT,

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
    TOK_EQ,         // =
    TOK_LT,         // <
    TOK_PLUS,       // +
    TOK_MINUS,      // -
    TOK_STAR,       // *
    TOK_SLASH,      // /
    TOK_NOT,        // !
    TOK_COMPLEMENT, // ~
    TOK_INC,        // ++
    TOK_DEC,        // --
    TOK_AND,        // &
    TOK_OR,         // |
    TOK_XOR,        // ^
    TOK_SHL,        // <<
    TOK_SHR,        // >>
    TOK_EQEQ,       // ==
    TOK_NEQ,        // !=
    TOK_GT,         // >
    TOK_GTE,        // >=
    TOK_LTE,        // <=
} TokenType;

typedef struct {
    TokenType type;
    char* value;
    int line;
    int pos;
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

