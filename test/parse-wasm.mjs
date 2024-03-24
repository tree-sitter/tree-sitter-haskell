#!/usr/bin/env node

import {readFileSync} from 'fs'
import Parser from 'web-tree-sitter'

const base = 'TREE_SITTER_LIBDIR' in process.env ? process.env.TREE_SITTER_LIBDIR : '.'
let files = []
let quiet = false
let errors = false

for (const arg of process.argv.slice(2)) {
  if (arg == '-q' || arg == '--quiet') quiet = true
  else files.push(arg)
}

if (files.length == 0) {
  console.log('Usage: parse-wasm.mjs [-q|--quiet] haskell-file ...')
  process.exit(1)
}

function node_range(node) {
  let start = node.startPosition
  let end = node.endPosition
  return `[${start.row}, ${start.column}] - [${end.row}, ${end.column}]`
}

function print_tree(tree) {
  const cursor = tree.walk()
  let needs_newline = false
  let indent_level = 0
  let did_visit_children = false
  while (true) {
    let node = cursor.currentNode()
    let is_named = node.isNamed()
    if (did_visit_children) {
      if (is_named) {
        process.stdout.write(')')
        needs_newline = true
      }
      if (cursor.gotoNextSibling()) did_visit_children = false
      else if (cursor.gotoParent()) {
          did_visit_children = true
          indent_level -= 1
      } else break
    }
    else {
      if (is_named) {
        if (needs_newline) process.stdout.write('\n')
        for (let i = 0; i < indent_level; i++) process.stdout.write('  ')
        if (cursor.fieldName) process.stdout.write(`${cursor.fieldName}: `)
        process.stdout.write(`(${node.type} ${node_range(node)}`)
        needs_newline = true
      }
      if (cursor.gotoFirstChild()) {
        did_visit_children = false
        indent_level += 1
      }
      else did_visit_children = true
    }
  }
  process.stdout.write('\n')
}

function find_error(tree) {
  const cursor = tree.walk()
  while (true) {
    let node = cursor.currentNode()
    if (node.hasError()) {
      if (node.isError() || node.isMissing()) return node
      else if (!cursor.gotoFirstChild()) return null
    } else if (!cursor.gotoNextSibling()) return null
  }
}

function handle_error(file, node) {
  errors = true
  process.stdout.write(`${file} (`)
  if (node.isMissing()) {
    process.stdout.write('MISSING ')
    if (node.isNamed()) process.stdout.write(node.type)
    else process.stdout.write(`"${node.type.replace('\n', '\\n')}"`)
  }
  else process.stdout.write(`${node.type} `)
  process.stdout.write(node_range(node))
  process.stdout.write(')\n')
}

(async () => {
  await Parser.init()
  let lang = await Parser.Language.load(base + '/haskell.wasm')
  const parser = new Parser
  parser.setLanguage(lang)
  for (const file of files) {
    const sourceCode = readFileSync(file, 'utf8')
    const tree = parser.parse(sourceCode)
    const errorNode = find_error(tree)
    if (!quiet) print_tree(tree)
    if (errorNode != null) handle_error(file, errorNode)
  }
  if (errors) process.exit(1)
})()
