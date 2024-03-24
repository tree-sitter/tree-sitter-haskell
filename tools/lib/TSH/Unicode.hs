module TSH.Unicode where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.List.NonEmpty ((<|))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Numeric (readHex)
import Path (Abs, Dir, File, Path, reldir, relfile, toFilePath, (</>))
import Path.IO (doesFileExist)

import TSH.Data.Options (BitmapPreset (Id))
import TSH.Data.Unicode (CatAbbr (..), Codepoint (..), CodepointBlock (..))
import TSH.Unicode.Category (catMap, presetCats)
import TSH.Util (M, pathText, tryIO)
import qualified Data.List.NonEmpty as NonEmpty

splitLine :: Text -> M Codepoint
splitLine l =
  case Text.split (== ';') l of
    [point, newName, CatAbbr -> cat, _, _, _, _, _, _, _, oldName, _, _, _, _] ->
      case readHex (toString point) of
        [(num, "")] -> pure Codepoint {num, name = chooseName newName oldName, cat}
        _ -> bad
    _ -> bad
  where
    chooseName n o | Text.take 1 n == "<" = o
                   | otherwise = n
    bad = throwE [exon|Bad line in unicode file: #{l}|]

dataFile ::
  Path Abs Dir ->
  Maybe (Path Abs File) ->
  M (Path Abs File)
dataFile cwd specified =
  maybe fromCwd fromSpecified specified
  where
    fromSpecified path =
      ifM (doesFileExist path) (pure path) $
      throwE [exon|Specified path #{pathText path} does not exist|]

    fromCwd =
      ifM (doesFileExist here) (pure here) $
      ifM (doesFileExist sub) (pure sub) $
      throwE [exon|Can't find '#{pathText name}' in #{pathText cwd} or the subdir 'tools'|]

    here = cwd </> name
    sub = cwd </> [reldir|tools|] </> name
    name = [relfile|UnicodeData.txt|]

parsePoints ::
  [Text] ->
  ExceptT Text IO [Codepoint]
parsePoints lns =
  traverse splitLine lns

readCodepoints ::
  Path Abs Dir ->
  Maybe (Path Abs File) ->
  M [Codepoint]
readCodepoints cwd specified = do
  file <- dataFile cwd specified
  lns <- Text.lines <$> tryIO (Text.readFile (toFilePath file))
  parsePoints lns

codepointMap ::
  Path Abs Dir ->
  Maybe (Path Abs File) ->
  M (Map Char Codepoint)
codepointMap cwd specified = do
  points <- readCodepoints cwd specified
  pure (Map.fromList (points <&> \ p -> (chr (fromIntegral p.num), p)))

targetCategories ::
  Maybe BitmapPreset ->
  Maybe (NonEmpty CatAbbr) ->
  M (Set CatAbbr)
targetCategories preset categories
  | isNothing preset
  , isNothing categories
  = pure (presetCats Id)
  | otherwise
  = do
    extra <- traverse validate (foldMap toList categories)
    pure (foldMap presetCats preset <> Set.fromList extra)
  where
    validate cat | Map.member cat catMap = pure cat
                 | otherwise = throwE [exon|Invalid category: ##{cat}|]

catFilter :: Set CatAbbr -> Codepoint -> Bool
catFilter targets Codepoint {cat} =
  Set.member cat targets

limitItems :: Maybe Word64 -> [a] -> [a]
limitItems =
  maybe id (take . fromIntegral)

pointsInCategories ::
  Set CatAbbr ->
  Maybe Word64 ->
  [Codepoint] ->
  Maybe (NonEmpty Codepoint)
pointsInCategories targets limit points =
  nonEmpty (limitItems limit (filter (catFilter targets) points))

pointBlocks ::
  Word64 ->
  NonEmpty Codepoint ->
  NonEmpty CodepointBlock
pointBlocks minGapSize (NonEmpty.reverse -> (h :| t)) =
  lastBlock :| prevBlocks
  where
    (lastBlock, prevBlocks) = foldl' step (mkBlock h, []) t

    step (current, blocks) p
      | current.minPoint - p.num > minGapSize
      = (mkBlock p, current : blocks)
      | otherwise
      = (current {points = p <| current.points, minPoint = p.num}, blocks)

    mkBlock point = CodepointBlock {points = [point], minPoint = point.num, maxPoint = point.num}
