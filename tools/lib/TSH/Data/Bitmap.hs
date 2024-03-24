module TSH.Data.Bitmap where

data Bitmap =
  Bitmap {
    minPoint :: Word64,
    maxPoint :: Word64,
    bytes :: [Word8]
  }
  deriving stock (Eq, Show, Generic)

data IdentifierBitmaps =
  IdentifierBitmaps {
    name :: Text,
    bitmaps :: NonEmpty Bitmap
  }
  deriving stock (Eq, Show, Generic)
