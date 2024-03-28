#!/usr/bin/env node

const fs = require('fs');
const Parser = require('web-tree-sitter');

if (process.argv.length < 2) {
  console.log('Usage: test/tree-sitter-parse.js <haskell-file..>')
  process.exit(1)
}

Parser.init().then(() => {
  Parser.Language.load('tree-sitter-haskell.wasm').then((Haskell) => {
    const parser = new Parser;
    parser.setLanguage(Haskell);
    for (let i = 1; i < process.argv.length; i++) {
      const fileName = process.argv[i]
      const sourceCode = fs.readFileSync(fileName, 'utf8')
      const tree = parser.parse(sourceCode);
    }
  });
});
