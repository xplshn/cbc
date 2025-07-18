#ifndef CBC_CODEGEN_H
#define CBC_CODEGEN_H

#include "ast.h"

void codegen_generate(AstNode* node, const char* output_file);

#endif // CBC_CODEGEN_H
