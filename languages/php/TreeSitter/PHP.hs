module TreeSitter.PHP
( tree_sitter_php
) where

import Foreign.Ptr
import TreeSitter.Language

foreign import ccall unsafe "vendor/tree-sitter-php/src/parser.c tree_sitter_php" tree_sitter_php :: Ptr Language
