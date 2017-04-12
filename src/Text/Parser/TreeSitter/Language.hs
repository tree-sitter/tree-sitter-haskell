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

data TSSymbolType = Regular | Anonymous | Auxiliary
  deriving (Enum, Eq, Ord, Show)

foreign import ccall unsafe "vendor/tree-sitter/include/tree_sitter/runtime.h ts_language_symbol_count" ts_language_symbol_count :: Ptr Language -> Word32
foreign import ccall unsafe "vendor/tree-sitter/include/tree_sitter/runtime.h ts_language_symbol_name" ts_language_symbol_name :: Ptr Language -> TSSymbol -> CString
foreign import ccall unsafe "vendor/tree-sitter/include/tree_sitter/runtime.h ts_language_symbol_type" ts_language_symbol_type :: Ptr Language -> TSSymbol -> Int


languageSymbols :: Ptr Language -> IO [(TSSymbolType, String)]
languageSymbols language = for [0..fromIntegral (pred count)] $ \ symbol -> do
  name <- peekCString (ts_language_symbol_name language symbol)
  pure (toEnum (ts_language_symbol_type language symbol), name)
  where count = ts_language_symbol_count language


-- | TemplateHaskell construction of a datatype for the referenced Language.
mkSymbolDatatype :: Name -> Ptr Language -> Q [Dec]
mkSymbolDatatype name language = do
  symbols <- runIO $ languageSymbols language

  pure [ DataD [] name [] Nothing (flip NormalC [] . uncurry symbolToName <$> symbols) [ ConT ''Show, ConT ''Eq, ConT ''Enum, ConT ''Ord ] ]

symbolToName :: TSSymbolType -> String -> Name
symbolToName ty = mkName . (>>= initUpper) . map (>>= toDescription) . toWords
  where toWords = split (condense (whenElt (not . isAlpha)))
        initUpper (c:cs) = toUpper c : cs
        initUpper "" = ""
        toDescription '{' = "OpenBrace"
        toDescription '}' = "CloseBrace"
        toDescription '(' = "OpenParen"
        toDescription ')' = "CloseParen"
        toDescription '.' = "Period"
        toDescription ':' = "Colon"
        toDescription ',' = "Comma"
        toDescription '|' = "VerticalLine"
        toDescription ';' = "Semicolon"
        toDescription '*' = "Asterisk"
        toDescription '&' = "Ampersand"
        toDescription '=' = "Equal"
        toDescription '<' = "LessThan"
        toDescription '>' = "GreaterThan"
        toDescription '[' = "OpenBracket"
        toDescription ']' = "CloseBracket"
        toDescription '+' = "Plus"
        toDescription '-' = "Minus"
        toDescription '/' = "ForwardSlash"
        toDescription '\\' = "Backslash"
        toDescription '^' = "Caret"
        toDescription '!' = "ExclamationPoint"
        toDescription '%' = "Percent"
        toDescription '@' = "At"
        toDescription '~' = "Tilde"
        toDescription '?' = "QuestionMark"
        toDescription '`' = "Backtick"
        toDescription '#' = "Pound"
        toDescription '$' = "Dollar"
        toDescription '"' = "DoubleQuote"
        toDescription '\'' = "SingleQuote"
        toDescription '\t' = "Tab"
        toDescription '\n' = "LF"
        toDescription '\r' = "CR"
        toDescription '_' = "Underscore"
        toDescription c = [c]
