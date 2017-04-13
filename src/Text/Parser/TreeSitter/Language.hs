{-# LANGUAGE TemplateHaskell #-}
module Text.Parser.TreeSitter.Language where

import Data.Char
import Data.Traversable (for)
import Data.List.Split
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import Language.Haskell.TH

newtype Language = Language ()
  deriving (Show, Eq)

type TSSymbol = Word16

data SymbolType = Regular | Anonymous | Auxiliary
  deriving (Enum, Eq, Ord, Show)

foreign import ccall unsafe "vendor/tree-sitter/include/tree_sitter/runtime.h ts_language_symbol_count" ts_language_symbol_count :: Ptr Language -> Word32
foreign import ccall unsafe "vendor/tree-sitter/include/tree_sitter/runtime.h ts_language_symbol_name" ts_language_symbol_name :: Ptr Language -> TSSymbol -> CString
foreign import ccall unsafe "vendor/tree-sitter/include/tree_sitter/runtime.h ts_language_symbol_type" ts_language_symbol_type :: Ptr Language -> TSSymbol -> Int


languageSymbols :: Ptr Language -> IO [(SymbolType, String)]
languageSymbols language = for [0..fromIntegral (pred count)] $ \ symbol -> do
  name <- peekCString (ts_language_symbol_name language symbol)
  pure (toEnum (ts_language_symbol_type language symbol), name)
  where count = ts_language_symbol_count language


-- | TemplateHaskell construction of a datatype for the referenced Language.
mkSymbolDatatype :: Name -> Ptr Language -> Q [Dec]
mkSymbolDatatype name language = do
  symbols <- runIO $ languageSymbols language

  pure [ DataD [] name [] Nothing (flip NormalC [] . uncurry symbolToName <$> symbols) [ ConT ''Show, ConT ''Eq, ConT ''Enum, ConT ''Ord ] ]

symbolToName :: SymbolType -> String -> Name
symbolToName ty = mkName . (prefix ++) . (>>= initUpper) . map (>>= toDescription) . filter (not . all (== '_')) . toWords . prefixHidden
  where toWords = split (condense (whenElt (not . isAlpha)))

        prefixHidden s@('_':_) = "Hidden" ++ s
        prefixHidden s = s

        initUpper (c:cs) = toUpper c : cs
        initUpper "" = ""

        toDescription '{' = "LBrace"
        toDescription '}' = "RBrace"
        toDescription '(' = "LParen"
        toDescription ')' = "RParen"
        toDescription '.' = "Dot"
        toDescription ':' = "Colon"
        toDescription ',' = "Comma"
        toDescription '|' = "Pipe"
        toDescription ';' = "Semicolon"
        toDescription '*' = "Star"
        toDescription '&' = "Ampersand"
        toDescription '=' = "Equal"
        toDescription '<' = "LAngle"
        toDescription '>' = "RAngle"
        toDescription '[' = "LBracket"
        toDescription ']' = "RBracket"
        toDescription '+' = "Plus"
        toDescription '-' = "Minus"
        toDescription '/' = "Slash"
        toDescription '\\' = "Backslash"
        toDescription '^' = "Caret"
        toDescription '!' = "Bang"
        toDescription '%' = "Percent"
        toDescription '@' = "At"
        toDescription '~' = "Tilde"
        toDescription '?' = "Question"
        toDescription '`' = "Backtick"
        toDescription '#' = "Hash"
        toDescription '$' = "Dollar"
        toDescription '"' = "DQuote"
        toDescription '\'' = "SQuote"
        toDescription '\t' = "Tab"
        toDescription '\n' = "LF"
        toDescription '\r' = "CR"
        toDescription c = [c]

        prefix = case ty of
          Regular -> ""
          Anonymous -> "Anon"
          Auxiliary -> "Aux"
