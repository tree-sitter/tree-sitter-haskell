WEB_TREE_SITTER_FILES=README.md package.json tree-sitter-web.d.ts tree-sitter.js tree-sitter.wasm
TREE_SITTER_VERSION=v0.20.4

# build parser.c
src/parser.c: grammar.js
	npx tree-sitter generate

# build web version of tree-sitter with --debug
.PHONY: debug-wasm
debug-wasm:
	@rm -rf tmp/tree-sitter
	@git clone                                       \
		-c advice.detachedHead=false --quiet           \
		--depth=1 --branch=$(TREE_SITTER_VERSION)      \
		https://github.com/tree-sitter/tree-sitter.git \
		tmp/tree-sitter
	@(cd tmp/tree-sitter && ./script/build-wasm --debug)
	@mkdir -p node_modules/web-tree-sitter
	@cp tmp/tree-sitter/LICENSE node_modules/web-tree-sitter
	@cp $(addprefix tmp/tree-sitter/lib/binding_web/,$(WEB_TREE_SITTER_FILES)) node_modules/web-tree-sitter
	@rm -rf tmp/tree-sitter

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
