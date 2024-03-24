module TSH.BitmapTest where

import qualified Data.Set as Set
import Exon (exon)
import Path (Abs, Dir, Path)

import TSH.Bitmap (idBitmaps, writeOutput)
import TSH.Bitmap.Render (catLines, indent, renderBitmaps, showByte)
import TSH.Data.Options (BitmapOptions (..), BitmapPreset (..), OutputOptions (..))
import qualified TSH.Data.Unicode
import TSH.Data.Unicode (CatAbbr (..), Codepoint (Codepoint))
import TSH.Unicode (limitItems, readCodepoints)
import TSH.Unicode.Category (presetCats)
import TSH.Util (M)

includes :: Text
includes =
  [exon|#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <wctype.h>
#include <wchar.h>
#include <locale.h>
#include <stdint.h>
#include <stdlib.h>
|]

check :: Text -> Set CatAbbr -> Codepoint -> Text
check slug targets Codepoint {num, cat} =
  [exon|if (is_#{slug}_char(#{c}) != #{accept}) { return failure(#{c}, #{accept}, "##{cat}"); }|]
  where
    c = showByte num
    accept | Set.member cat targets = "true"
           | otherwise = "false"

checks :: Text -> Set CatAbbr -> [Codepoint] -> Text
checks slug positive =
  indent 1 . fmap (check slug positive)

genTest_id ::
  [Codepoint] ->
  Text
genTest_id points =
  [exon|int failure(int32_t c, bool accept, char * restrict cat) {
  wchar_t wc[2] = {c, 0};
  fwprintf(stderr, L"Mismatch: '%ls' (%s): %s\n", wc, cat, accept ? "rejected" : "accepted");
  return 1;
}

int main() {
  setlocale(LC_ALL, "C.UTF-8");
#{checks "identifier" (presetCats Id) points}
  fwprintf(stderr, L"All code points have been classified correctly.\n");
}
|]

check_wslower :: Text -> Codepoint -> Text
check_wslower slug Codepoint {num, cat} =
  [exon|if (is_#{slug}_char(#{c}) != !!iswlower(#{c})) { return failure(#{c}, is_#{slug}_char(#{c}), !!iswlower(#{c}), "##{cat}"); }|]
  where
    c = showByte num

checks_wslower :: Text -> [Codepoint] -> Text
checks_wslower slug =
  indent 1 . fmap (check_wslower slug)

genTest_lower ::
  Text ->
  [Codepoint] ->
  Text
genTest_lower slug points =
  [exon|int failure(int32_t c, bool bm, bool wl, char * restrict cat) {
  wchar_t wc[2] = {c, 0};
  fwprintf(stderr, L"Mismatch with wslower: '%ls' (%s) %d / %d\n", wc, cat, bm, wl);
  return 1;
}

int main() {
  setlocale(LC_ALL, "C.UTF-8");
#{checks_wslower slug points}
  fwprintf(stderr, L"All code points have been classified correctly.\n");
  return 0;
}
|]

writeBitmapTest ::
  Path Abs Dir ->
  BitmapOptions ->
  M ()
writeBitmapTest cwd baseOptions = do
  points <- readCodepoints cwd baseOptions.output.unicodeData
  bitmaps_id <- idBitmaps options_id points
  let test_id = genTest_id (limitItems baseOptions.output.limit points)
  writeOutput options_id.output (catLines (includes : renderBitmaps bitmaps_id ++ [test_id]))
  -- bitmaps_lower <- idBitmaps options_lower points
  -- let test_lower = genTest_lower "lower" (limitItems baseOptions.limit points)
  -- writeOutput options_lower (catLines (includes : renderBitmaps bitmaps_lower ++ [test_lower]))
  where
    options_id = baseOptions {preset = Just Id, categories = Nothing, output = baseOptions.output {name = Nothing}}
    -- options_lower = baseOptions {preset = Nothing, categories = Just ["Ll", "Lo"], name = Just "lower"}
