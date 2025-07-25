#include "ast.h"
#include "codegen.h"
#include "lexer.h"
#include "parser.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// CLi
typedef struct {
  const char **input_files;
  int input_file_count;
  const char *output_filename;
  const char *std_str;
} CliOptions;
// Fp
static void parse_arguments(int argc, char *argv[], CliOptions *opts);
static char *read_file_to_string(const char *path);
static void compile_and_link(const CliOptions *opts, const char *qbe_file,
                             const char *asm_file);
static void cleanup(const char **files, int count);
static void apply_std(const char *std_name);
static void print_help(const char *prog_name);

int main(int argc, char *argv[]) {
  if (argc < 2) {
    print_help(argv[0]);
    return 1;
  }


  CliOptions opts = {.input_files = malloc(argc * sizeof(char *)),
                     .input_file_count = 0,
                     .output_filename = "a.out",
                     .std_str = "Bx"
  };
  if (!opts.input_files)
    error(0, 0, 0, "Failed to allocate memory for input files.");

  for (int i = 1; i < argc; i++) {
    if (strncmp(argv[i], "-std=", 5) == 0) {
      opts.std_str = argv[i] + 5;
      break;
    }
  }

  // Apply the determined standard's settings.
  apply_std(opts.std_str);

  // Now, parse all arguments. This will apply user-specified -W/-F flags,
  // correctly overriding the defaults we just set from the standard.
  parse_arguments(argc, argv, &opts);

  if (opts.input_file_count == 0) {
    error(0, 0, 0, "No input files specified.");
  }

  size_t total_size = 0;
  char *combined_source = malloc(1);
  if (!combined_source)
    error(0, 0, 0, "Failed to allocate memory for combined source.");
  combined_source[0] = '\0';
  for (int i = 0; i < opts.input_file_count; i++) {
    char *file_content = read_file_to_string(opts.input_files[i]);
    size_t file_size = strlen(file_content);
    char *new_combined_source =
        realloc(combined_source,
                total_size + file_size + 2); // +1 for newline, +1 for null
    if (!new_combined_source)
      error(0, 0, 0, "Failed to reallocate memory for combined source.");
    combined_source = new_combined_source;
    memcpy(combined_source + total_size, file_content, file_size);
    combined_source[total_size + file_size] = '\n';
    combined_source[total_size + file_size + 1] = '\0';
    total_size += file_size + 1;
    free(file_content);
  }

  set_source_for_errors(combined_source, "combined source");
  // --- Compilation Pipeline ---
  Lexer lexer = lexer_init(combined_source);
  Parser parser = parser_init(&lexer);

  // After all flags are processed, configure the lexer.
  lexer.use_c_escapes = is_feature_enabled(FEAT_C_ESCAPES);

  AstNode *ast = parser_parse(&parser);
  ast = ast_fold_constants(ast);

  const char *qbe_output_file = "output.qbe";
  const char *asm_output_file = "output_asm.s";
  codegen_generate(ast, qbe_output_file, asm_output_file, sizeof(void *));

  compile_and_link(&opts, qbe_output_file, asm_output_file);

  parser_free(&parser);
  ast_free(ast);
  free(combined_source);
  free((void *)opts.input_files);
  //const char *temp_files[] = {"output.s", "output_asm.s", "output.qbe"};
  //cleanup(temp_files, 3);
  const char *temp_files[] = {"output.s", "output_asm.s"};
  cleanup(temp_files, 2);

  return 0;
}

// misc cli stuff (--help/-h, alignment, etc)
#define HELP_SECTION(title) fprintf(stderr, "\n  %s\n", title)
#define HELP_ITEM(key, desc) fprintf(stderr, "    %-22s %s\n", key, desc)
#define HELP_LIST_HEADER(name) fprintf(stderr, "    Available %s:\n", name)
#define HELP_LIST_ITEM(name, desc)                                             \
  fprintf(stderr, "      %-20s %s\n", name, desc)

static void print_help(const char *prog_name) {
  fprintf(stderr, "\nCopyright (c) 2025: xplshn and contributors\n");
  fprintf(stderr, "For more details refer to <https://github.com/xplshn/cbc "
                  "|| cbc.xplshn.com.ar>\n");

  HELP_SECTION("Synopsis");
  fprintf(stderr, "    %s [options] <input.b> ...\n", prog_name);

  HELP_SECTION("Description");
  fprintf(stderr,
          "    A compiler for the B programming language and its extensions.\n");

  HELP_SECTION("Options");
  HELP_ITEM("-o <file>", "Place the output into <file>.");
  HELP_ITEM("-h, --help", "Display this information.");
  HELP_ITEM("-std=<std>", "Specify language standard (B, Bx). Default: Bx");
  HELP_ITEM("-pedantic", "Issue all warnings demanded by the current B std.");

  HELP_SECTION("Warning Flags");
  HELP_ITEM("-Wall", "Enable most warnings.");
  HELP_ITEM("-Wno-all", "Disable all warnings.");
  HELP_ITEM("-W<warning>", "Enable a specific warning.");
  HELP_ITEM("-Wno-<warning>", "Disable a specific warning.");
  HELP_LIST_HEADER("warnings");
  for (int j = 0; j < WARN_COUNT; j++) {
    fprintf(stderr, "      %-22s %-75s %s\n", warning_info[j].name,
            warning_info[j].description,
            is_warning_enabled((WarningType)j) ? "[x]" : "[-]");
  }

  HELP_SECTION("Feature Flags");
  HELP_ITEM("-F<feature>", "Enable a specific feature.");
  HELP_ITEM("-Fno-<feature>", "Disable a specific feature.");
  HELP_LIST_HEADER("features");
  for (int j = 0; j < FEAT_COUNT; j++) {
    fprintf(stderr, "      %-22s %-75s %s\n", feature_info[j].name,
            feature_info[j].description,
            is_feature_enabled((FeatureType)j) ? "[x]" : "[-]");
  }

  fprintf(stderr, "\n");
}

static void parse_arguments(int argc, char *argv[], CliOptions *opts) {
  for (int i = 1; i < argc; ++i) {
    const char *arg = argv[i];
    if (arg[0] != '-') {
      opts->input_files[opts->input_file_count++] = (char *)arg;
      continue;
    }

    if (strcmp(arg, "--help") == 0 || strcmp(arg, "-h") == 0) {
      print_help(argv[0]);
      exit(0);
    } else if (strcmp(arg, "-o") == 0) {
      if (++i < argc)
        opts->output_filename = argv[i];
      else
        error(0, 0, 0, "-o requires a filename.");
    } else if (strncmp(arg, "-std=", 5) == 0) {
      // This is already handled in the first pass, but we keep it here
      // to avoid it being treated as an unknown option.
    } else if (strcmp(arg, "-Wall") == 0) {
      set_warning_all(true);
    } else if (strcmp(arg, "-Wno-all") == 0) {
      set_warning_all(false);
    } else if (strcmp(arg, "-pedantic") == 0) {
      set_warning(WARN_PEDANTIC, true);
    } else if (strncmp(arg, "-Wno-", 5) == 0) {
      const char *name = arg + 5;
      bool found = false;
      for (int j = 0; j < WARN_COUNT; ++j) {
        if (strcmp(warning_info[j].name, name) == 0) {
          set_warning((WarningType)j, false);
          found = true;
          break;
        }
      }
      if (!found)
        warning(WARN_EXTRA, 0, 0, 0, "Unrecognized warning option: %s", arg);
    } else if (strncmp(arg, "-W", 2) == 0) {
      const char *name = arg + 2;
      bool found = false;
      for (int j = 0; j < WARN_COUNT; ++j) {
        if (strcmp(warning_info[j].name, name) == 0) {
          set_warning((WarningType)j, true);
          found = true;
          break;
        }
      }
      if (!found)
        warning(WARN_EXTRA, 0, 0, 0, "Unrecognized warning option: %s", arg);
    } else if (strcmp(arg, "-fno-extrn") == 0) {
      set_feature(FEAT_EXTRN, false);
    } else if (strcmp(arg, "-fno-asm") == 0) {
      set_feature(FEAT_ASM, false);
    } else if (strncmp(arg, "-Fno-", 5) == 0) {
      const char *name = arg + 5;
      bool found = false;
      for (int j = 0; j < FEAT_COUNT; ++j) {
        if (strcmp(feature_info[j].name, name) == 0) {
          set_feature((FeatureType)j, false);
          found = true;
          break;
        }
      }
      if (!found)
        warning(WARN_EXTRA, 0, 0, 0, "Unrecognized feature option: %s", arg);
    } else if (strncmp(arg, "-F", 2) == 0) {
      const char *name = arg + 2;
      bool found = false;
      for (int j = 0; j < FEAT_COUNT; ++j) {
        if (strcmp(feature_info[j].name, name) == 0) {
          set_feature((FeatureType)j, true);
          found = true;
          break;
        }
      }
      if (!found)
        warning(WARN_EXTRA, 0, 0, 0, "Unrecognized feature option: %s", arg);
    } else {
      // not an option, prolly an input file
      opts->input_files[opts->input_file_count++] = (char *)arg;
    }
  }
}

static void apply_std(const char *std_name) {
  bool is_pedantic = is_warning_enabled(WARN_PEDANTIC);
  if (strcmp(std_name, "B") == 0) {
    set_feature(FEAT_B_COMPOUND_ASSIGN, true);
    set_feature(FEAT_C_ESCAPES, !is_pedantic);
    set_warning(WARN_C_OPS, true);
    set_warning(WARN_B_OPS, false);
    if (is_pedantic) {
      set_feature(FEAT_EXTRN, false);
      set_feature(FEAT_ASM, false);
      set_warning(WARN_C_COMMENTS, true);
    }
  } else if (strcmp(std_name, "Bx") == 0) {
    set_feature(FEAT_B_COMPOUND_ASSIGN, false);
    set_feature(FEAT_B_ESCAPES, !is_pedantic);
    set_warning(WARN_C_OPS, false);
    set_warning(WARN_B_OPS, !is_pedantic);
  } else {
    error(0, 0, 0, "Unsupported standard '%s'. Supported: 'B', 'Bx'.",
          std_name);
  }
}

static char *read_file_to_string(const char *path) {
  FILE *file = fopen(path, "rb");
  if (!file)
    error(0, 0, 0, "Cannot open file '%s'", path);

  fseek(file, 0, SEEK_END);
  long size = ftell(file);
  fseek(file, 0, SEEK_SET);
  char *buffer = malloc(size + 4); // +4 UTF-8 padding
  if (!buffer)
    error(0, 0, 0, "Could not allocate memory to read file '%s'", path);
  if (fread(buffer, 1, size, file) != (size_t)size)
    error(0, 0, 0, "Could not read the entire file '%s'", path);
  buffer[size] = '\0';
  buffer[size + 1] = '\0';
  buffer[size + 2] = '\0';
  buffer[size + 3] = '\0';

  fclose(file);
  return buffer;
}

static void compile_and_link(const CliOptions *opts, const char *qbe_file,
                             const char *asm_file) {
  char command[2048];
  // 1. Compile IR to Assembly
  snprintf(command, sizeof(command), "qbe -o output.s %s", qbe_file);
  if (system(command) != 0) {
    error(0, 0, 0, "QBE failed to process the intermediate representation");
  }

  // 2. Use the C compiler to assemble and link our assembly files
  snprintf(command, sizeof(command), "cc -static-pie -o %s output.s %s",
           opts->output_filename, asm_file);
  if (system(command) != 0) {
    error(0, 0, 0, "C compiler failed to link the final executable");
  }
}

static void cleanup(const char **files, int count) {
  for (int i = 0; i < count; ++i) {
    // will silently fail if the file doesn't exist. Which is OK
    unlink(files[i]);
  }
}
