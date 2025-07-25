#ifndef CBC_LEXER_H
#define CBC_LEXER_H

#include <stdbool.h>

// Token Type Enum
// Defines all possible token types the lexer can produce.
typedef enum {
  // Meta
  TOK_EOF = 0, // End of file/input

  // Literals
  TOK_IDENT,  // Identifier, e.g., my_var
  TOK_NUMBER, // Numeric literal, e.g., 123, 0x1A, 'a'
  TOK_STRING, // String literal, e.g., "hello"

  // Keywords
  TOK_AUTO,
  TOK_EXTRN,
  TOK_IF,
  TOK_ELSE,
  TOK_WHILE,
  TOK_RETURN,
  TOK_GOTO,
  TOK_SWITCH,
  TOK_CASE,
  TOK_DEFAULT,
  TOK_BREAK,
  TOK___ASM__,

  // Punctuation
  TOK_LPAREN,
  TOK_RPAREN,
  TOK_LBRACE,
  TOK_RBRACE,
  TOK_LBRACKET,
  TOK_RBRACKET,
  TOK_SEMI,
  TOK_COMMA,
  TOK_COLON,
  TOK_QUESTION,
  TOK_DOTS,

  // === OPERATOR GROUPS ===
  // Note: The order helps in classifying operators.
  // Assignment Operators (Lowest Precedence in this group)
  TOK_EQ, // =
  // C-style Compound Assignment
  TOK_PLUS_EQ,  // +=
  TOK_MINUS_EQ, // -=
  TOK_STAR_EQ,  // *=
  TOK_SLASH_EQ, // /=
  TOK_REM_EQ,   // %=
  TOK_AND_EQ,   // &=
  TOK_OR_EQ,    // |=
  TOK_XOR_EQ,   // ^=
  TOK_SHL_EQ,   // <<=
  TOK_SHR_EQ,   // >>=
  // Historical B-style Compound Assignment
  TOK_EQ_PLUS,  // =+
  TOK_EQ_MINUS, // =-
  TOK_EQ_STAR,  // =*
  TOK_EQ_SLASH, // =/
  TOK_EQ_REM,   // =%
  TOK_EQ_AND,   // =&
  TOK_EQ_OR,    // =|
  TOK_EQ_XOR,   // =^
  TOK_EQ_SHL,   // =<<
  TOK_EQ_SHR,   // =>>

  // Binary Operators
  TOK_PLUS,
  TOK_MINUS,
  TOK_STAR,
  TOK_SLASH,
  TOK_REM,
  TOK_AND,
  TOK_OR,
  TOK_XOR,
  TOK_SHL,
  TOK_SHR,
  TOK_EQEQ,
  TOK_NEQ,
  TOK_LT,
  TOK_GT,
  TOK_GTE,
  TOK_LTE,

  // Unary & Postfix Operators
  TOK_NOT,        // !
  TOK_COMPLEMENT, // ~
  TOK_INC,        // ++
  TOK_DEC,        // --

} TokenType;
// Represents a single token from the source code.
typedef struct {
  TokenType type;
  char *value; // Value for literals (string, number, ident)
  int line;    // Line number where the token starts
  int column;  // Column number where the token starts
  int len;     // Length of the token text in the source
} Token;

// The lexer state, holding the source, position, and configuration.
typedef struct {
  const char *source;
  int pos;
  int line;
  int column;
  Token peeked;
  bool has_peeked;
  // Configuration flags set by the driver based on features/std.
  bool use_c_escapes; // true: `\n`, false: `*n`
} Lexer;

// Fp

// Inits a new lexer
Lexer lexer_init(const char *source);
// Consumes and returns the next token from the lexer
Token lexer_next(Lexer *lexer);
// Peeks at the next token without consuming it
Token lexer_peek(Lexer *lexer);
// Frees the memory allocated for a token's value, if any
void token_free(Token token);
#endif // CBC_LEXER_H
