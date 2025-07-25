#include "lexer.h"
#include "util.h"
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

// Fwd Dec.
static Token lexer_get_token(Lexer *lexer);
static long decode_escape(Lexer *lexer, char escape_char);

// lexer api
Lexer lexer_init(const char *source) {
  if (!source)
    error(0, 0, 0, "Null source input to lexer");
  return (Lexer){.source = source, .line = 1, .column = 1};
}

Token lexer_next(Lexer *lexer) {
  if (lexer->has_peeked) {
    lexer->has_peeked = false;
    return lexer->peeked;
  }
  return lexer_get_token(lexer);
}

Token lexer_peek(Lexer *lexer) {
  if (!lexer->has_peeked) {
    lexer->peeked = lexer_get_token(lexer);
    lexer->has_peeked = true;
  }
  return lexer->peeked;
}

void token_free(Token token) {
  if (token.value)
    free(token.value);
}

// lexer internals (Character Handling)

static char peek(const Lexer *lexer) { return lexer->source[lexer->pos]; }
static char peek_next(const Lexer *lexer) {
  return lexer->source[lexer->pos + 1];
}

static char advance(Lexer *lexer) {
  char c = lexer->source[lexer->pos];
  if (c != '\0') {
    lexer->pos++;
    if (c == '\n') {
      lexer->line++;
      lexer->column = 1;
    } else {
      lexer->column++;
    }
  }
  return c;
}

// lexer internals (Token Creation)

static Token make_token(const Lexer *lexer, TokenType type, char *value,
                        int start_col, int len) {
  return (Token){.type = type,
                 .value = value,
                 .line = lexer->line,
                 .column = start_col,
                 .len = len};
}

static Token make_simple_token(Lexer *lexer, TokenType type, int len) {
  Token t = make_token(lexer, type, NULL, lexer->column, len);
  for (int i = 0; i < len; ++i)
    advance(lexer);
  return t;
}

// lexer internals (Skipping Whitespace & Comments)

static void skip_whitespace(Lexer *lexer) {
  while (true) {
    char c = peek(lexer);
    if (isspace(c)) {
      advance(lexer);
      continue;
    }
    if (c == '/' && peek_next(lexer) == '*') {
      int line = lexer->line, col = lexer->column;
      advance(lexer);
      advance(lexer);
      while (peek(lexer) != '\0' &&
             (peek(lexer) != '*' || peek_next(lexer) != '/')) {
        advance(lexer);
      }
      if (peek(lexer) == '\0') {
        error(line, col, 2, "Unterminated block comment");
      }
      advance(lexer);
      advance(lexer);
      continue;
    }
    if (c == '/' && peek_next(lexer) == '/') {
      warning(WARN_C_COMMENTS, lexer->line, lexer->column, 2,
              "Using non-standard C-style '//' comment.");
      while (peek(lexer) != '\n' && peek(lexer) != '\0') {
        advance(lexer);
      }
      continue;
    }
    break;
  }
}

// lexer internals (Literals)

static Token number_literal(Lexer *lexer) {
  int start_col = lexer->column;
  const char *start_ptr = lexer->source + lexer->pos;
  char *end_ptr;
  errno = 0;
  // parse the complete range of valid 64-bit unsigned integers
  unsigned long long value = strtoull(start_ptr, &end_ptr, 0);
  if (errno == ERANGE) {
    warning(WARN_OVERFLOW, lexer->line, start_col, end_ptr - start_ptr,
            "Integer constant is out of range.");
  }

  int len = end_ptr - start_ptr;
  for (int i = 0; i < len; ++i)
    advance(lexer);
  char *str_val = malloc(32);
  // Store as a signed long long, which can represent the full unsigned 64-bit
  // range
  snprintf(str_val, 32, "%lld", (long long)value);
  return make_token(lexer, TOK_NUMBER, str_val, start_col, len);
}

static Token char_literal(Lexer *lexer) {
  int start_col = lexer->column, start_pos = lexer->pos;
  advance(lexer); // Consume opening '

  long word = 0;
  int char_count = 0;
  while (peek(lexer) != '\'' && peek(lexer) != '\0') {
    char current_char = peek(lexer);
    long val;

    if (current_char == '\\' && is_feature_enabled(FEAT_C_ESCAPES)) {
      advance(lexer); // consume '\'
      if (!lexer->use_c_escapes) {
        warning(WARN_C_ESCAPES, lexer->line, lexer->column - 1, 1,
                "Using C-style '\\' escape in character literal");
      }
      val = decode_escape(lexer, '\\');
    } else if (current_char == '*' && is_feature_enabled(FEAT_B_ESCAPES)) {
      advance(lexer); // consume '*'
      if (lexer->use_c_escapes) {
        warning(WARN_B_ESCAPES, lexer->line, lexer->column - 1, 1,
                "Using B-style '*' escape in character literal");
      }
      long decoded = decode_escape(lexer, '*');
      if (decoded != -1) {
        val = decoded;
      } else {
        // Not a valid B escape, treat '*' as literal.
        // We already advanced past it, so just set the value.
        val = current_char;
      }
    } else {
      advance(lexer); // consume the character
      val = current_char;
    }

    if (++char_count > sizeof(long)) {
      warning(WARN_LONG_CHAR_CONST, lexer->line, start_col, 1,
              "Multi-character constant may overflow word size");
    }
    word = (word << 8) | (val & 0xFF);
  }

  if (peek(lexer) != '\'')
    error(lexer->line, start_col, lexer->pos - start_pos,
          "Unterminated character literal");
  advance(lexer); // Consume closing '

  char *str = malloc(32);
  snprintf(str, 32, "%ld", word);
  return make_token(lexer, TOK_NUMBER, str, start_col, lexer->pos - start_pos);
}

static Token string_literal(Lexer *lexer) {
  int start_col = lexer->column, start_pos = lexer->pos;
  advance(lexer); // Consume opening "

  int cap = 256;
  char *buffer = malloc(cap);
  int len = 0;

  while (peek(lexer) != '"' && peek(lexer) != '\0') {
    if (len >= cap - 1)
      buffer = realloc(buffer, cap *= 2);
    char c = advance(lexer);

    if (c == '\\') {
      if (is_feature_enabled(FEAT_C_ESCAPES)) {
        if (!lexer->use_c_escapes) { // In B-mode, warn on C-escape
          warning(WARN_C_ESCAPES, lexer->line, lexer->column - 1, 1,
                  "Using C-style '\\' escape in string literal");
        }
        long val = decode_escape(lexer, '\\');
        if (val < 0 || val > 255)
          warning(WARN_TRUNCATED_CHAR, lexer->line, lexer->column, 1,
                  "Character escape out of range, truncating");
        buffer[len++] = (char)val;
      } else {
        buffer[len++] = c;
      }
    } else if (c == '*') {
      if (is_feature_enabled(FEAT_B_ESCAPES)) {
        if (lexer->use_c_escapes) { // In C-mode, warn on B-escape
          warning(WARN_B_ESCAPES, lexer->line, lexer->column - 1, 1,
                  "Using B-style '*' escape in string literal");
        }
        long val = decode_escape(lexer, '*');
        if (val != -1) { // Successful B escape
          if (val < 0 || val > 255)
            warning(WARN_TRUNCATED_CHAR, lexer->line, lexer->column, 1,
                    "Character escape out of range, truncating");
          buffer[len++] = (char)val;
        } else { // Invalid B escape, treat '*' as literal
          buffer[len++] = c;
        }
      } else {
        buffer[len++] = c;
      }
    } else {
      buffer[len++] = c;
    }
  }

  if (peek(lexer) == '\0')
    error(lexer->line, start_col, lexer->pos - start_pos,
          "Unterminated string literal");
  advance(lexer); // Consume closing "
  buffer[len] = '\0';
  return make_token(lexer, TOK_STRING, buffer, start_col,
                    lexer->pos - start_pos);
}

static long decode_escape(Lexer *lexer, char escape_char) {
  char c = peek(lexer);
  if (c == '\0')
    error(lexer->line, lexer->column, 1, "Unterminated escape sequence");

  advance(lexer); // Consume the character after the escape initiator

  if (escape_char == '\\') {
    if (c == 'x') { // Hex escape
      const char *start = lexer->source + lexer->pos;
      char *end;
      long val = strtol(start, &end, 16);
      if (start == end)
        error(lexer->line, lexer->column - 2, 2,
              "Invalid hex escape: \\x requires hex digits");
      lexer->pos += end - start;
      lexer->column += end - start;
      return val;
    }
    if (c >= '0' && c <= '7') { // Octal escape
      // Backtrack one char to include the first digit
      lexer->pos--;
      lexer->column--;
      const char *start = lexer->source + lexer->pos;
      char *end;
      long val = strtol(start, &end, 8);
      lexer->pos += end - start;
      lexer->column += end - start;
      return val;
    }
    switch (c) {
    case 'n':
      return '\n';
    case 't':
      return '\t';
    case 'v':
      return '\v';
    case 'b':
      return '\b';
    case 'r':
      return '\r';
    case 'f':
      return '\f';
    case 'a':
      return '\a';
    case '\\':
      return '\\';
    case '\'':
      return '\'';
    case '"':
      return '"';
    case '?':
      return '?';
    default:
      warning(WARN_UNRECOGNIZED_ESCAPE, lexer->line, lexer->column - 1, 1,
              "Unrecognized escape sequence '\\%c'", c);
      return c;
    }
  } else { // B-style escapes
    switch (c) {
    case 'n':
      return '\n';
    case 't':
      return '\t';
    case 'e':
      return 4; /* EOT */
    case 'b':
      return '\b';
    case 'r':
      return '\r';
    case '0':
      return '\0';
    case '(':
      return '{';
    case ')':
      return '}';
    case '*':
      return '*';
    case '\'':
      return '\'';
    default:
      warning(WARN_UNRECOGNIZED_ESCAPE, lexer->line, lexer->column - 1, 1,
              "Unrecognized escape sequence '\\%c'", c);
      return c;
    }
  }
}

static Token identifier_or_keyword(Lexer *lexer) {
  int start_pos = lexer->pos, start_col = lexer->column;
  while (isalnum(peek(lexer)) || peek(lexer) == '_')
    advance(lexer);

  int len = lexer->pos - start_pos;
  const char *value_ptr = lexer->source + start_pos;
  struct {
    const char *name;
    TokenType type;
  } keywords[] = {{"auto", TOK_AUTO},   {"extrn", TOK_EXTRN},
                  {"if", TOK_IF},       {"else", TOK_ELSE},
                  {"while", TOK_WHILE}, {"return", TOK_RETURN},
                  {"goto", TOK_GOTO},   {"switch", TOK_SWITCH},
                  {"case", TOK_CASE},   {"default", TOK_DEFAULT},
                  {"break", TOK_BREAK}, {"__asm__", TOK___ASM__}};
  for (size_t i = 0; i < sizeof(keywords) / sizeof(keywords[0]); ++i) {
    if (strlen(keywords[i].name) == len &&
        strncmp(keywords[i].name, value_ptr, len) == 0) {
      return make_token(lexer, keywords[i].type, NULL, start_col, len);
    }
  }

  char *value = malloc(len + 1);
  strncpy(value, value_ptr, len);
  value[len] = '\0';
  return make_token(lexer, TOK_IDENT, value, start_col, len);
}

// Main Dispatch

static Token lexer_get_token(Lexer *lexer) {
  skip_whitespace(lexer);
  int start_col = lexer->column;
  char c = peek(lexer);
  char next = peek_next(lexer);
  if (c == '\0')
    return make_token(lexer, TOK_EOF, NULL, start_col, 0);
  if (isalpha(c) || c == '_')
    return identifier_or_keyword(lexer);
  if (isdigit(c))
    return number_literal(lexer);
  if (c == '"')
    return string_literal(lexer);
  if (c == '\'')
    return char_literal(lexer);

  switch (c) {
  case '(':
    return make_simple_token(lexer, TOK_LPAREN, 1);
  case ')':
    return make_simple_token(lexer, TOK_RPAREN, 1);
  case '{':
    return make_simple_token(lexer, TOK_LBRACE, 1);
  case '}':
    return make_simple_token(lexer, TOK_RBRACE, 1);
  case '[':
    return make_simple_token(lexer, TOK_LBRACKET, 1);
  case ']':
    return make_simple_token(lexer, TOK_RBRACKET, 1);
  case ';':
    return make_simple_token(lexer, TOK_SEMI, 1);
  case ',':
    return make_simple_token(lexer, TOK_COMMA, 1);
  case ':':
    return make_simple_token(lexer, TOK_COLON, 1);
  case '?':
    return make_simple_token(lexer, TOK_QUESTION, 1);
  case '~':
    return make_simple_token(lexer, TOK_COMPLEMENT, 1);
  case '.':
    if (next == '.' && lexer->source[lexer->pos + 2] == '.')
      return make_simple_token(lexer, TOK_DOTS, 3);
    break;
  case '+':
    if (next == '+')
      return make_simple_token(lexer, TOK_INC, 2);
    if (next == '=') {
      warning(WARN_C_OPS, lexer->line, start_col, 2,
              "C-style operator '+=' used in -std=B mode");
      return make_simple_token(lexer, TOK_PLUS_EQ, 2);
    }
    return make_simple_token(lexer, TOK_PLUS, 1);
  case '-':
    if (next == '-')
      return make_simple_token(lexer, TOK_DEC, 2);
    if (next == '=') {
      warning(WARN_C_OPS, lexer->line, start_col, 2,
              "C-style operator '-=' used in -std=B mode");
      return make_simple_token(lexer, TOK_MINUS_EQ, 2);
    }
    return make_simple_token(lexer, TOK_MINUS, 1);
  case '*':
    if (next == '=') {
      warning(WARN_C_OPS, lexer->line, start_col, 2,
              "C-style operator '*=' used in -std=B mode");
      return make_simple_token(lexer, TOK_STAR_EQ, 2);
    }
    return make_simple_token(lexer, TOK_STAR, 1);
  case '/':
    if (next == '=') {
      warning(WARN_C_OPS, lexer->line, start_col, 2,
              "C-style operator '/=' used in -std=B mode");
      return make_simple_token(lexer, TOK_SLASH_EQ, 2);
    }
    return make_simple_token(lexer, TOK_SLASH, 1);
  case '%':
    if (next == '=') {
      warning(WARN_C_OPS, lexer->line, start_col, 2,
              "C-style operator '%%=' used in -std=B mode");
      return make_simple_token(lexer, TOK_REM_EQ, 2);
    }
    return make_simple_token(lexer, TOK_REM, 1);
  case '&':
    if (next == '=') {
      warning(WARN_C_OPS, lexer->line, start_col, 2,
              "C-style operator '&=' used in -std=B mode");
      return make_simple_token(lexer, TOK_AND_EQ, 2);
    }
    return make_simple_token(lexer, TOK_AND, 1);
  case '|':
    if (next == '=') {
      warning(WARN_C_OPS, lexer->line, start_col, 2,
              "C-style operator '|=' used in -std=B mode");
      return make_simple_token(lexer, TOK_OR_EQ, 2);
    }
    return make_simple_token(lexer, TOK_OR, 1);
  case '^':
    if (next == '=') {
      warning(WARN_C_OPS, lexer->line, start_col, 2,
              "C-style operator '^=' used in -std=B mode");
      return make_simple_token(lexer, TOK_XOR_EQ, 2);
    }
    return make_simple_token(lexer, TOK_XOR, 1);
  case '!':
    if (next == '=')
      return make_simple_token(lexer, TOK_NEQ, 2);
    return make_simple_token(lexer, TOK_NOT, 1);
  case '<':
    if (next == '<') {
      if (lexer->source[lexer->pos + 2] == '=') {
        warning(WARN_C_OPS, lexer->line, start_col, 3,
                "C-style operator '<<=' used in -std=B mode");
        return make_simple_token(lexer, TOK_SHL_EQ, 3);
      }
      return make_simple_token(lexer, TOK_SHL, 2);
    }
    if (next == '=')
      return make_simple_token(lexer, TOK_LTE, 2);
    return make_simple_token(lexer, TOK_LT, 1);
  case '>':
    if (next == '>') {
      if (lexer->source[lexer->pos + 2] == '=') {
        warning(WARN_C_OPS, lexer->line, start_col, 3,
                "C-style operator '>>=' used in -std=B mode");
        return make_simple_token(lexer, TOK_SHR_EQ, 3);
      }
      return make_simple_token(lexer, TOK_SHR, 2);
    }
    if (next == '=')
      return make_simple_token(lexer, TOK_GTE, 2);
    return make_simple_token(lexer, TOK_GT, 1);
  case '=':
    if (next == '=')
      return make_simple_token(lexer, TOK_EQEQ, 2);

    if (is_feature_enabled(FEAT_B_COMPOUND_ASSIGN)) {
      if (next == '<' && lexer->source[lexer->pos + 2] == '<') {
        warning(WARN_B_OPS, lexer->line, start_col, 3,
                "B-style operator '=<<' used in non-B mode");
        return make_simple_token(lexer, TOK_EQ_SHL, 3);
      }
      if (next == '>' && lexer->source[lexer->pos + 2] == '>') {
        warning(WARN_B_OPS, lexer->line, start_col, 3,
                "B-style operator '=>>' used in non-B mode");
        return make_simple_token(lexer, TOK_EQ_SHR, 3);
      }
      if (strchr("+-*/%&|^", next)) {
        warning(WARN_B_OPS, lexer->line, start_col, 2,
                "B-style assignment operator used in non-B mode");
        switch (next) {
        case '+':
          return make_simple_token(lexer, TOK_EQ_PLUS, 2);
        case '-':
          return make_simple_token(lexer, TOK_EQ_MINUS, 2);
        case '*':
          return make_simple_token(lexer, TOK_EQ_STAR, 2);
        case '/':
          return make_simple_token(lexer, TOK_EQ_SLASH, 2);
        case '%':
          return make_simple_token(lexer, TOK_EQ_REM, 2);
        case '&':
          return make_simple_token(lexer, TOK_EQ_AND, 2);
        case '|':
          return make_simple_token(lexer, TOK_EQ_OR, 2);
        case '^':
          return make_simple_token(lexer, TOK_EQ_XOR, 2);
        }
      }
    }
    return make_simple_token(lexer, TOK_EQ, 1);
  }

  error(lexer->line, start_col, 1, "Unexpected character: '%c' (ASCII %d)", c,
        c);
  return make_token(lexer, TOK_EOF, NULL, start_col, 0); // Unreachable
}
