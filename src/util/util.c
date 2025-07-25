#include "util.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Globals for holding source information for error reporting.
static const char *source_code = NULL;
static const char *source_filename = NULL;

// Define the global warning info array using the X-Macro.
WarningInfo warning_info[WARN_COUNT] = {
#define X(EnumName, cli_name, default_enabled, desc)                           \
  [WARN_##EnumName] = {                                                        \
      .name = cli_name, .enabled = default_enabled, .description = desc},
    FOR_ALL_WARNINGS(X)
#undef X
};

// Define the global feature info array using the X-Macro.
FeatureInfo feature_info[FEAT_COUNT] = {
#define X(EnumName, cli_name, default_enabled, desc)                           \
  [FEAT_##EnumName] = {                                                        \
      .name = cli_name, .enabled = default_enabled, .description = desc},
    FOR_ALL_FEATURES(X)
#undef X
};

void set_source_for_errors(const char *source, const char *filename) {
  source_code = source;
  source_filename = filename;
}

static void print_error_line(FILE *stream, int line, int col, int len) {
  if (!source_code)
    return;
  const char *line_start = source_code;
  const char *line_end = strchr(line_start, '\n');
  int current_line = 1;
  while (current_line < line && line_end != NULL) {
    line_start = line_end + 1;
    line_end = strchr(line_start, '\n');
    current_line++;
  }

  if (current_line != line)
    return;

  if (line_end == NULL) {
    line_end = line_start + strlen(line_start);
  }

  fprintf(stream, "  %.*s\n", (int)(line_end - line_start), line_start);
  fprintf(stream, "  ");
  for (int i = 1; i < col; i++) {
    if (i > 0 && line_start[i - 1] == '\t') {
      fputc('\t', stream);
    } else {
      fputc(' ', stream);
    }
  }
  fprintf(stream, "\033[32m^");
  for (int i = 1; i < len; i++) {
    fputc('~', stream);
  }
  fprintf(stream, "\033[0m\n");
}

void error(int line, int col, int len, const char *fmt, ...) {
  fprintf(stderr, "%s:%d:%d: \033[31merror:\033[0m ",
          source_filename ? source_filename : "source", line, col);
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  va_end(args);

  print_error_line(stderr, line, col, len);
  exit(1);
}

void print_help(const char *prog_name) {
  fprintf(stderr, "Usage: %s [options] <input.b> ...\n", prog_name);
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  -o <file>                Place the output into <file>.\n");
  fprintf(stderr, "  -h, --help               Display this information.\n");
  fprintf(stderr, "  -std=<std>               Specify language standard (B, "
                  "Bx). Default: Bx\n");
  fprintf(stderr, "  -Wall                    Enable most warnings.\n");
  fprintf(stderr, "  -Wno-all                 Disable all warnings.\n");
  fprintf(stderr, "  -pedantic                Issue all warnings demanded by "
                  "the current B std.\n");
  fprintf(stderr,
          "  -fno-extrn               Forbid use of 'extrn' keyword.\n");
  fprintf(stderr,
          "  -fno-asm                 Forbid use of '__asm__' keyword.\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "  -W<warning>              Enable a specific warning.\n");
  fprintf(stderr, "  -Wno-<warning>           Disable a specific warning.\n");
  fprintf(stderr, "  Available warnings:\n");
  for (int j = 0; j < WARN_COUNT; j++) {
    fprintf(stderr, "    %-22s %s\n", warning_info[j].name,
            warning_info[j].description);
  }
  fprintf(stderr, "\n");
  fprintf(stderr, "  -F<feature>              Enable a specific feature.\n");
  fprintf(stderr, "  -Fno-<feature>           Disable a specific feature.\n");
  fprintf(stderr, "  Available features:\n");
  for (int j = 0; j < FEAT_COUNT; j++) {
    fprintf(stderr, "    %-22s %s\n", feature_info[j].name,
            feature_info[j].description);
  }
}

char *str_dup(const char *src) {
  if (!src)
    return NULL;
  char *dst = malloc(strlen(src) + 1);
  if (!dst) {
    error(0, 0, 0, "Memory allocation failed for string duplication.");
  }
  strcpy(dst, src);
  return dst;
}

#ifdef DBG_TAGS
bool is_debug_tag_enabled(const char *tag) {
  static char *tags_to_search = NULL;
  static bool initialized = false;
  static bool all_enabled = false;

  if (!initialized) {
    initialized = true;
    const char *tags_from_env = getenv("DBG_TAGS");
    if (tags_from_env) {
      if (strcmp(tags_from_env, "all") == 0) {
        all_enabled = true;
      } else {
        size_t len = strlen(tags_from_env);
        tags_to_search = malloc(len + 3);
        if (!tags_to_search)
          error(0, 0, 0, "malloc failed in is_debug_tag_enabled");
        tags_to_search[0] = ',';
        strcpy(tags_to_search + 1, tags_from_env);
        tags_to_search[len + 1] = ',';
        tags_to_search[len + 2] = '\0';
      }
    }
  }

  if (all_enabled)
    return true;
  if (!tags_to_search)
    return false;

  char tag_to_find[256];
  snprintf(tag_to_find, sizeof(tag_to_find), ",%s,", tag);

  return strstr(tags_to_search, tag_to_find) != NULL;
}
#endif

void set_warning_all(bool enabled) {
  for (int i = 0; i < WARN_COUNT; i++) {
    // Don't enable pedantic with -Wall, it must be explicit
    if (i == WARN_PEDANTIC && enabled)
      continue;
    warning_info[i].enabled = enabled;
  }
}

void set_warning(WarningType type, bool enabled) {
  if (type < WARN_COUNT) {
    warning_info[type].enabled = enabled;
  }
}

bool is_warning_enabled(WarningType type) {
  if (type < WARN_COUNT) {
    return warning_info[type].enabled;
  }
  return false;
}

void warning(WarningType type, int line, int col, int len, const char *fmt,
             ...) {
  if (!is_warning_enabled(type)) {
    return;
  }
  fprintf(stderr, "%s:%d:%d: \033[33mwarning:\033[0m ",
          source_filename ? source_filename : "source", line, col);
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fprintf(stderr, " [-W%s]\n", warning_info[type].name);
  print_error_line(stderr, line, col, len);
}

void set_feature(FeatureType type, bool enabled) {
  if (type < FEAT_COUNT) {
    feature_info[type].enabled = enabled;
  }
}

bool is_feature_enabled(FeatureType type) {
  if (type < FEAT_COUNT)
    return feature_info[type].enabled;
  return false;
}
