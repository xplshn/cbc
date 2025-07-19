#include "util.h"
#include "lexer.h"
#include "parser.h"
#include "codegen.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input.b>\n", argv[0]);
        return 1;
    }

    printf("debug: Opening file '%s'\n", argv[1]);
    FILE* file = fopen(argv[1], "r");
    if (!file) {
        fprintf(stderr, "Error: Cannot open file '%s'\n", argv[1]);
        return 1;
    }

    printf("debug: Getting file size\n");
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    if (file_size < 0) {
        fprintf(stderr, "Error: Cannot determine file size for '%s'\n", argv[1]);
        fclose(file);
        return 1;
    }
    fseek(file, 0, SEEK_SET);

    printf("debug: Allocating buffer for %ld bytes\n", file_size);
    char* source = malloc(file_size + 4);
    if (!source) {
        fprintf(stderr, "Error: Memory allocation failed for '%s'\n", argv[1]);
        fclose(file);
        return 1;
    }

    printf("debug: Reading file\n");
    size_t read = fread(source, 1, file_size, file);
    if (read != (size_t)file_size) {
        fprintf(stderr, "Error: Failed to read entire file '%s' (read %zu of %ld bytes)\n",
                argv[1], read, file_size);
        free(source);
        fclose(file);
        return 1;
    }
    source[read] = '\0';
    source[read + 1] = '\0';
    source[read + 2] = '\0';
    source[read + 3] = '\0'; // Padding for utf8_decode
    fclose(file);

    printf("debug: --- LEXING & PARSING ---\n");
    Lexer lexer = lexer_init(source);
    Parser parser = parser_init(&lexer);
    AstNode* ast = parser_parse(&parser);
    printf("debug: --- PARSING COMPLETE ---\n");

    printf("debug: --- CODE GENERATION ---\n");
    codegen_generate(ast, "output.qbe");
    printf("debug: --- CODEGEN COMPLETE ---\n");

    // Clean up
    parser_free(&parser);
    ast_free(ast);
    free(source);

    // Run QBE and linker
    printf("debug: --- ASSEMBLING & LINKING ---\n");
    if (system("qbe -o output.s output.qbe") != 0) {
        error(0, "QBE failed to assemble the intermediate representation.");
    }
    if (system("O output.s -o ./output.os") != 0) {
        error(0, "Peephole optimizer failed to optimize the generated assembly.");
    }
    if (system("mv output.os output.s") != 0) {
        error(0, "Could not rename ./output.os to ./output.s");
    }
    if (system("cc -s -static-pie -o output output.s") != 0) {
        error(0, "C compiler failed to link the final executable.");
    }
    //if (system("sstrip output") != 0) {
    //    error(0, "Couldn't strip the output binary using sstrip.");
    //}
    printf("debug: --- BUILD COMPLETE: ./output ---\n");

    return 0;
}

