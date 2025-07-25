#ifndef CBC_PARSER_H
#define CBC_PARSER_H

#include "ast.h"
#include "lexer.h"

// Parser state
typedef struct {
  Lexer *lexer;
  Token current;
  Token previous;
} Parser;

// Fp
Parser parser_init(Lexer *lexer);
AstNode *parser_parse(Parser *parser);
void parser_free(Parser *parser);

#endif // CBC_PARSER_H
