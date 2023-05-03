;; Copyright 2022 nvim-treesitter
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; ----------------------------------------------------------------------------
;; Literals and comments

(integer) @number
(exp_negation) @number
(exp_literal (float)) @number
(char) @string
(string) @string

(con_unit) @punctuation.special  ; unit, as in ()

(comment) @comment


;; ----------------------------------------------------------------------------
;; Punctuation

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  (comma)
  ";"
] @punctuation.delimiter


;; ----------------------------------------------------------------------------
;; Keywords, operators, includes

[
  "forall"
  "∀"
] @keyword

(pragma) @attribute

[
  "if"
  "then"
  "else"
  "case"
  "of"
] @keyword

[
  "import"
  "qualified"
  "module"
] @keyword

[
  (operator)
  (constructor_operator)
  (type_operator)
  (tycon_arrow)
  (all_names)
  (wildcard)
  "="
  "|"
  "::"
  "=>"
  "->"
  "<-"
  "\\"
  "`"
  "@"
] @operator

(module) @module

[
  (where)
  "let"
  "in"
  "class"
  "instance"
  "data"
  "newtype"
  "family"
  "type"
  "as"
  "hiding"
  "deriving"
  "via"
  "stock"
  "anyclass"
  "do"
  "mdo"
  "rec"
  "infix"
  "infixl"
  "infixr"
] @keyword


;; ----------------------------------------------------------------------------
;; Functions and variables

(variable) @variable
(pat_wildcard) @variable

(signature name: (variable) @type)
(function
  name: (variable) @function
  patterns: (patterns))
((signature (fun)) . (function (variable) @function))
((signature (context (fun))) . (function (variable) @function))
((signature (forall (context (fun)))) . (function (variable) @function))

(exp_infix (variable) @operator)  ; consider infix functions as operators



;; ----------------------------------------------------------------------------
;; Types

(type) @type
(type_variable) @type

(constructor) @tag

; True or False
((constructor) @_bool (#match? @_bool "(True|False)")) @variable.builtin


;; ----------------------------------------------------------------------------
;; Quasi-quotes

(quoter) @function
; Highlighting of quasiquote_body is handled by injections.scm
