#ifndef CBC_CODEGEN_H
#define CBC_CODEGEN_H

#include "ast.h"

void codegen_generate(AstNode *node, const char *output_file,
                      const char *asm_output_file,
                      unsigned short int word_size);

#endif // CBC_CODEGEN_H
