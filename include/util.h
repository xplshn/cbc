#ifndef CBC_UTIL_H
#define CBC_UTIL_H

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

#define CRESET "\e[0m"
#define RED "\e[0;31m"
#define GRN "\e[0;32m"
#define YEL "\e[0;33m"
#define MAG "\e[0;35m"

// Tagged Debugging
#ifdef DBG_TAGS
bool is_debug_tag_enabled(const char *tag);
#define DEBUG(tag, fmt, ...)                                                   \
  do {                                                                         \
    if (is_debug_tag_enabled(tag))                                             \
      fprintf(stderr, "\033[35mdebug[%s]:\033[0m " fmt "\n", tag,              \
              ##__VA_ARGS__);                                                  \
  } while (0)
#else
#define DEBUG(tag, fmt, ...)                                                   \
  do {                                                                         \
  } while (0)
#endif

// Feature Flags
// X-Macro list of all features, their names, default states, and descriptions.
#define FOR_ALL_FEATURES(X)                                                   \
  X(EXTRN, "extrn", true, "Allow the 'extrn' keyword")                        \
  X(ASM, "asm", true, "Allow the '__asm__' keyword and blocks")               \
  X(B_ESCAPES, "b-escapes", true, "Recognize B-style '*' character escapes")  \
  X(C_ESCAPES, "c-escapes", true, "Recognize C-style '\\' character escapes") \
  X(B_COMPOUND_ASSIGN, "b-compound-assign", true,                             \
    "Recognize B-style assignment operators like '=+'")

// Error Reporting & Help
void set_source_for_errors(const char *source, const char *filename);
void error(int line, int col, int len, const char *fmt, ...);
// misc
char *str_dup(const char *src);

// Warnings system

// X-Macro list of all warnings, their names, default states, and descriptions.
#define FOR_ALL_WARNINGS(X)                                                    \
  X(C_ESCAPES, "c-escapes", true,                                              \
    "Using C-style '\\' escapes instead of B's '*'")                           \
  X(B_ESCAPES, "b-escapes", true,                                              \
    "Using historical B-style '*' escapes instead of C's '\\'")                \
  X(B_OPS, "b-ops", true, "Using historical B assignment operators like '=+'") \
  X(C_OPS, "c-ops", true,                                                      \
    "Using C-style assignment operators like '+=' in -std=B mode")             \
  X(UNRECOGNIZED_ESCAPE, "unrecognized-escape", true,                          \
    "Using unrecognized escape sequences")                                     \
  X(TRUNCATED_CHAR, "truncated-char", true,                                    \
    "Character escape value is too large for a byte and has been truncated")   \
  X(LONG_CHAR_CONST, "long-char-const", true,                                  \
    "Multi-character constant is too long for a word")                         \
  X(C_COMMENTS, "c-comments", false,                                           \
    "Using non-standard C-style '//' comments")                                \
  X(OVERFLOW, "overflow", true,                                                \
    "Integer constant is out of range for its type")                           \
  X(PEDANTIC, "pedantic", false,                                               \
    "Issues that violate the current strict -std=")                            \
  X(UNREACHABLE_CODE, "unreachable-code", true, "Unreachable code")            \
  X(EXTRA, "extra", true,                                                      \
    "Extra warnings (e.g., poor choices, unrecognized flags)")

// Generate the WarningType enum from the X-Macro list.
typedef enum {
#define X(EnumName, ...) WARN_##EnumName,
  FOR_ALL_WARNINGS(X)
#undef X
      WARN_COUNT
} WarningType;

// --- Structs for Warnings and Features ---
typedef struct {
  const char *name;
  bool enabled;
  const char *description;
} WarningInfo;

// Global array of warning information, defined in util.c.
extern WarningInfo warning_info[WARN_COUNT];

// Generate the FeatureType enum from the X-Macro list.
typedef enum {
#define X(EnumName, ...) FEAT_##EnumName,
  FOR_ALL_FEATURES(X)
#undef X
      FEAT_COUNT
} FeatureType;

typedef struct {
  const char *name;
  bool enabled;
  const char *description;
} FeatureInfo;

// Global array of feature information, defined in util.c.
extern FeatureInfo feature_info[FEAT_COUNT];

// Functions to control and issue warnings.
void set_warning_all(bool enabled);
void set_warning(WarningType type, bool enabled);
bool is_warning_enabled(WarningType type);
void warning(WarningType type, int line, int col, int len, const char *fmt,
             ...);

// Functions to control features.
void set_feature(FeatureType type, bool enabled);
bool is_feature_enabled(FeatureType type);
#endif // CBC_UTIL_H
