module TSH where

import Control.Monad.Trans.Except (ExceptT)
import Path (Abs, Dir, Path)

import TSH.Bitmap (writeBitmaps, writeMultiBitmaps)
import TSH.BitmapTest (writeBitmapTest)
import qualified TSH.Data.Options
import TSH.Data.Options (Command (..), Options (Options))
import TSH.Options (parseCli)
import TSH.Util (runM)

runCommand ::
  Path Abs Dir ->
  Command ->
  ExceptT Text IO ()
runCommand cwd = \case
  GenBitmap options -> writeBitmaps cwd options
  GenBitmaps options -> writeMultiBitmaps cwd options
  GenBitmapTest options -> writeBitmapTest cwd options

main :: IO ()
main = do
  Options {cwd, command} <- parseCli
  runM (runCommand cwd command)
