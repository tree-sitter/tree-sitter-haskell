# build parser.c
src/parser.c: grammar.js
	npx tree-sitter generate

# build web version of tree-sitter-haskell
tree-sitter-haskell.wasm: src/parser.c src/scanner.c
	npx tree-sitter build-wasm

CC := cc
OURCFLAGS := -shared -fPIC -g -O0 -I src

clean:
	rm -f debug *.o *.a

debug.so: src/parser.c src/scanner.c
	$(CC) $(OURCFLAGS) $(CFLAGS) -o parser.o src/parser.c
	$(CC) $(OURCFLAGS) $(CFLAGS) -o scanner.o src/scanner.c
	$(CC) $(OURCFLAGS) $(CFLAGS) -o debug.so $(PWD)/scanner.o $(PWD)/parser.o
	@echo ""
	@echo "-----------"
	@echo ""
	@echo "To use the debug build with tree-sitter on linux, run:"
	@echo "cp debug.so $HOME/.cache/tree-sitter/lib/haskell.so"
