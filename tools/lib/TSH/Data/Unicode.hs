module TSH.Data.Unicode where

import qualified Data.Text as Text
import Data.Text (toLower)
import Exon (exon)

newtype CatAbbr =
  CatAbbr Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

newtype CatLong =
  CatLong Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

data Cat =
  Cat {
    abbr :: CatAbbr,
    long :: CatLong
  }
  deriving stock (Eq, Show, Generic)

data Codepoint =
  Codepoint {
    num :: Word64,
    name :: Text,
    cat :: CatAbbr
  }
  deriving stock (Eq, Show, Generic)

prettyPoint :: Codepoint -> Text
prettyPoint Codepoint {..} =
  [exon|#{pad 5 (show num)}    #{pad 2 (coerce cat)}    #{toLower name}|]
  where
    pad n s = Text.replicate (clamp (n - Text.length s)) " " <> s
    clamp n | n > 0 = n
            | otherwise = 0

data CodepointBlock =
  CodepointBlock {
    points :: NonEmpty Codepoint,
    minPoint :: Word64,
    maxPoint :: Word64
  }
  deriving stock (Eq, Show, Generic)
