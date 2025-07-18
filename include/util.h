#ifndef CBC_UTIL_H
#define CBC_UTIL_H

#include <stdarg.h>
#include <stdio.h>

void error(int line, const char* fmt, ...);
char* str_dup(const char* src);

#ifdef DBG
#define DEBUG(fmt, ...) do { \
    fprintf(stderr, "\033[35mdebug:\033[0m " fmt "\n", ##__VA_ARGS__); \
} while (0)
#else
#define DEBUG(fmt, ...) do { } while (0)
#endif

#endif // CBC_UTIL_H


