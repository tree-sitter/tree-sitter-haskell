module TSH.Bitmap where

import Control.Monad.Trans.Except (throwE)
import Data.Bits (setBit)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.IO as Text
import Path (Abs, Dir, File, Path, toFilePath)
import Path.IO (getCurrentDir)

import TSH.Bitmap.Render (renderFile, showByte)
import qualified TSH.Data.Bitmap
import TSH.Data.Bitmap (Bitmap (Bitmap), IdentifierBitmaps (IdentifierBitmaps))
import qualified TSH.Data.Options
import TSH.Data.Options (BitmapOptions (BitmapOptions), BitmapPreset (..), presetSlug, OutputOptions (..), BitmapsOptions (..))
import qualified TSH.Data.Unicode
import TSH.Data.Unicode (Codepoint (..), CodepointBlock (CodepointBlock))
import TSH.Unicode (pointBlocks, pointsInCategories, readCodepoints, targetCategories, codepointMap)
import TSH.Util (M, tryIO, runM)
import Exon (exon)
import Data.Map.Strict ((!?))

mkByte :: Word64 -> [Word64] -> Word8
mkByte base =
  foldl' step (0 :: Word8)
  where
    step z entry = setBit z (fromIntegral (entry - base))

createBitmap ::
  CodepointBlock ->
  Bitmap
createBitmap CodepointBlock {minPoint, maxPoint, points} =
  Bitmap {minPoint, maxPoint, bytes}
  where
    (_, bytes) = step minPoint [] ((.num) <$> toList points)

    step byte acc [] = (byte, reverse acc)
    step byte acc pts =
      step next (mkByte byte cur : acc) rest
      where
        (cur, rest) = span (< next) pts
        next = byte + 8

writeFile :: Path Abs File -> Text -> M ()
writeFile file content =
  tryIO (Text.writeFile (toFilePath file) content)

targetBlocks ::
  BitmapOptions ->
  [Codepoint] ->
  M (NonEmpty CodepointBlock)
targetBlocks options allPoints = do
  targetCats <- targetCategories options.preset options.categories
  targetPoints <- maybe noMatch pure (pointsInCategories targetCats options.output.limit allPoints)
  pure (maybe single pointBlocks options.output.gapSize targetPoints)
  where
    single points =
      pure CodepointBlock {points, minPoint = (NonEmpty.head points).num, maxPoint = (NonEmpty.last points).num}
    noMatch = throwE "No points matched the specified categories"

idBitmaps ::
  BitmapOptions ->
  [Codepoint] ->
  M IdentifierBitmaps
idBitmaps options points = do
  blocks <- targetBlocks options points
  pure IdentifierBitmaps {name, bitmaps = createBitmap <$> blocks}
  where
    name = fromMaybe "identifier" (options.output.name <|> (presetSlug <$> options.preset))

genBitmaps ::
  Path Abs Dir ->
  BitmapOptions ->
  M IdentifierBitmaps
genBitmaps cwd options = do
  points <- readCodepoints cwd options.output.unicodeData
  idBitmaps options points

genMultiBitmaps ::
  Path Abs Dir ->
  BitmapsOptions ->
  M (NonEmpty IdentifierBitmaps)
genMultiBitmaps cwd options = do
  points <- readCodepoints cwd options.output.unicodeData
  for options.presets \ p -> idBitmaps BitmapOptions {preset = Just p, categories = Nothing, output} points
  where
    BitmapsOptions {output} = options

writeOutput ::
  OutputOptions ->
  Text ->
  M ()
writeOutput options content =
  maybe (liftIO . Text.putStrLn) writeFile options.file content

writeBitmaps ::
  Path Abs Dir ->
  BitmapOptions ->
  M ()
writeBitmaps cwd options = do
  bitmaps <- genBitmaps cwd options
  writeOutput options.output (renderFile [bitmaps])

writeMultiBitmaps ::
  Path Abs Dir ->
  BitmapsOptions ->
  M ()
writeMultiBitmaps cwd options = do
  bitmaps <- genMultiBitmaps cwd options
  writeOutput options.output (renderFile bitmaps)

test_cCode :: IO ()
test_cCode = do
  cwd <- getCurrentDir
  runM (writeBitmaps cwd options)
  where
    options =
      BitmapOptions {
        preset = Just Id,
        categories = Nothing,
        output
      }

    output =
      OutputOptions {
        limit = Just 100,
        file = Nothing,
        gapSize = Just 5,
        unicodeData = Nothing,
        name = Nothing
      }

reserved :: [Int]
reserved =
  [
    0x2237,
    0x21D2,
    0x2192,
    0x2190,
    0x291a,
    0x2919,
    0x291C,
    0x291B,
    0x2605,
    0x2200,
    0x2987,
    0x2988,
    0x27E6,
    0x27E7
  ]

test_reserved :: IO ()
test_reserved =
  for_ (ord '\\' : reserved) \ p ->
    putStrLn [exon|#{toString (showByte p)} | #{[chr p]}|]

test_points :: IO ()
test_points =
  runM do
    cwd <- getCurrentDir
    points <- codepointMap cwd Nothing
    dbgs (points !? '-')
