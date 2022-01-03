#!/usr/bin/env node

const fs = require('fs');
const assert = require('assert');
const Parser = require('web-tree-sitter');

if (process.argv.length < 3) {
  console.log('Usage: script/tree-sitter-parse.js <haskell-file..>')
  process.exit(1)
}

async function main() {
  await Parser.init()
  Haskell = await Parser.Language.load('tree-sitter-haskell.wasm')
  const parser = new Parser()
  parser.setLanguage(Haskell)
  for (let i = 2; i < process.argv.length; i++) {
    const fileName = process.argv[i]
    const sourceCode = fs.readFileSync(fileName, 'utf8')
    const tree = parser.parse(sourceCode)
    assert.equal(tree.rootNode.type, 'haskell')
  }
}

main()