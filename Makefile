CC = cc
CFLAGS = -std=c11 -Wall -Iinclude -DDBG
SRC = src/main.c src/lexer/lexer.c src/parser/parser.c src/ast/ast.c src/util/util.c src/codegen/codegen.c
OBJ = $(SRC:.c=.o)
OUT = cbc

all: $(OUT)

$(OUT): $(OBJ)
	$(CC) $(OBJ) -o $(OUT)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(OUT) output.qbe output.s output.o output

.PHONY: all clean
