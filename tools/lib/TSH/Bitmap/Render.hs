module TSH.Bitmap.Render where

import qualified Data.Text as Text
import Exon (exon)
import Numeric (showHex)

import qualified TSH.Data.Bitmap
import TSH.Data.Bitmap (Bitmap (Bitmap), IdentifierBitmaps (IdentifierBitmaps))

catLines :: [Text] -> Text
catLines = Text.intercalate "\n"

indent :: Int -> [Text] -> Text
indent n =
  catLines . fmap (Text.replicate n "  " <>)

bitmapIndexName :: Maybe Word -> Text -> Text
bitmapIndexName index name
  | Just i <- index = [exon|#{name}_#{show i}|]
  | otherwise = name

showByte :: Integral a => a -> Text
showByte b
  | b < 10
  = show (fromIntegral @_ @Int b)
  | otherwise
  = toText ([exon|0x#{showHex b}|] "")

condenseBytes :: [Text] -> [Text]
condenseBytes =
  unfoldr (check . splitAt 20)
  where
    check = \case
      ([], _) -> Nothing
      (chunk, rest) -> Just (mconcat chunk, rest)

renderBitmap :: Text -> Maybe Word -> Bitmap -> Text
renderBitmap name index Bitmap {minPoint, maxPoint, bytes} =
  [exon|static uint8_t #{prefix}[] = {
#{bytesEnc}
};

static int32_t #{prefix}_min_codepoint = #{show minPoint};

static int32_t #{prefix}_max_codepoint = #{show maxPoint};

static bool is_#{suffixedName}_char(int32_t c) {
#{indent 1 cond}
}
|]
  where
    cond = maxGuard ++ [offset, op]
    maxGuard
      | isNothing index
      = [[exon|if (c < #{prefix}_min_codepoint || c > #{prefix}_max_codepoint) return false;|]]
      | otherwise
      = []
    offset = [exon|int32_t offset = c - bitmap_#{suffixedName}_min_codepoint;|]
    op = [exon|return (bitmap_#{suffixedName}[offset >> 3] & (1u << (offset & 7))) > 0;|]
    bytesEnc = catLines (condenseBytes (appendComma . showByte <$> bytes))
    appendComma = (<> ",")
    suffixedName = bitmapIndexName index name

    prefix = [exon|bitmap_#{suffixedName}|]

intervalSelector :: Text -> [Word] -> Text
intervalSelector name indexes =
  [exon|static bool is_#{name}_char(int32_t c) {
  return
#{indent 2 (concatMap one indexes)}
    false;
}
|]
  where
    one index
      | let n = bitmapIndexName (Just index) name
      = [
        [exon|c < bitmap_#{n}_min_codepoint ? false :|],
        [exon|c <= bitmap_#{n}_max_codepoint ? is_#{n}_char(c) :|]
      ]

renderBitmaps :: IdentifierBitmaps -> [Text]
renderBitmaps IdentifierBitmaps {name, bitmaps}
  | [single] <- bitmaps
  = [renderBitmap name Nothing single]
  | otherwise
  = multi ++ [intervalSelector name (fst <$> indexed)]
  where
    multi = [renderBitmap name (Just i) b | (i, b) <- indexed]
    indexed = zip [1..] (toList bitmaps)

renderFile :: NonEmpty IdentifierBitmaps -> Text
renderFile bitmaps =
  [exon|#include <stdbool.h>
#include <stdint.h>

#{catLines (concatMap renderBitmaps bitmaps)}|]
