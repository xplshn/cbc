#include "lexer.h"
#include "parser.h"
#include "codegen.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input.b>\n", argv[0]);
        return 1;
    }

    // Read input file
    FILE* file = fopen(argv[1], "r");
    if (!file) {
        fprintf(stderr, "Cannot open file %s\n", argv[1]);
        return 1;
    }
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    char* source = malloc(size + 1);
    if (!source) {
        fprintf(stderr, "Could not allocate memory for source file\n");
        return 1;
    }
    size_t read_size = fread(source, 1, size, file);
    if (read_size != (size_t)size) {
        fprintf(stderr, "Error reading file\n");
        free(source);
        fclose(file);
        return 1;
    }
    source[size] = '\0';
    fclose(file);

    // Lex and parse
    DEBUG("--- LEXING & PARSING ---");
    Lexer lexer = lexer_init(source);
    Parser parser = parser_init(&lexer);
    AstNode* ast = parser_parse(&parser);
    DEBUG("--- PARSING COMPLETE ---");

    // Generate QBE IR
    DEBUG("--- CODE GENERATION ---");
    codegen_generate(ast, "output.qbe");
    DEBUG("--- CODEGEN COMPLETE ---");

    // Clean up
    parser_free(&parser);
    ast_free(ast);
    free(source);

    // Run QBE and linker
    DEBUG("--- ASSEMBLING & LINKING ---");
    if (system("qbe -o output.s output.qbe") != 0) {
        error(0, "QBE failed to assemble the intermediate representation.");
    }
    if (system("cc -o output output.s") != 0) {
        error(0, "C compiler failed to link the final executable.");
    }
    DEBUG("--- BUILD COMPLETE: ./output ---");

    return 0;
}


