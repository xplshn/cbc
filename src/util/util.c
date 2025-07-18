#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void error(int line, const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    fprintf(stderr, "\033[31mError\033[0m at line %d: ", line);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    va_end(args);
    exit(1);
}

char* str_dup(const char* src) {
    if (!src) return NULL;
    char* dst = malloc(strlen(src) + 1);
    if (!dst) { error(0, "Memory allocation failed for string duplication."); }
    strcpy(dst, src);
    return dst;
}
