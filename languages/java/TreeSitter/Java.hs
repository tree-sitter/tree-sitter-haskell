module TreeSitter.Java
( tree_sitter_java
) where

import Foreign.Ptr
import TreeSitter.Language

foreign import ccall unsafe "vendor/tree-sitter-java/src/parser.c tree_sitter_java" tree_sitter_java :: Ptr Language
