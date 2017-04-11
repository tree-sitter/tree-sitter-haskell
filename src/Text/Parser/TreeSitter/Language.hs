{-# LANGUAGE TemplateHaskell #-}
module Text.Parser.TreeSitter.Language where

import Data.Char
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import Language.Haskell.TH

newtype Language = Language ()
  deriving (Show, Eq)

type TSSymbol = Word16

foreign import ccall unsafe "vendor/tree-sitter/include/tree_sitter/runtime.h ts_language_symbol_count" ts_language_symbol_count :: Ptr Language -> Word32
foreign import ccall unsafe "vendor/tree-sitter/include/tree_sitter/runtime.h ts_language_symbol_name" ts_language_symbol_name :: Ptr Language -> TSSymbol -> CString


-- | TemplateHaskell construction of a datatype for the referenced Language.
mkSymbolDatatype :: Name -> Ptr Language -> Q Dec
mkSymbolDatatype name language = do
  let symbolCount = ts_language_symbol_count language
  symbolNames <- runIO $ traverse (peekCString . ts_language_symbol_name language) [0..fromIntegral (pred symbolCount)]

  pure $! DataD [] name [] Nothing [  ] [ ConT (mkName "Show"), ConT (mkName "Eq"), ConT (mkName "Enum") ]

toTitleCase :: String -> String
toTitleCase s = case s of
  c:rest -> toUpper c : go rest
  _ -> go s
  where go "" = ""
        go ('_':c:rest) = toUpper c : go rest
        go (c:rest) = c : go rest
