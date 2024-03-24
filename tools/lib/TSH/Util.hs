module TSH.Util where

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Text.IO as Text
import Path (Path, toFilePath)
import System.IO (stderr)
import System.IO.Error (tryIOError)

type M a = ExceptT Text IO a

runM :: M () -> IO ()
runM m = leftA (Text.hPutStrLn stderr) =<< runExceptT m

tryIO ::
  IO a ->
  ExceptT Text IO a
tryIO ma =
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left err -> throwE (show err)

pathText :: Path b t -> Text
pathText =
  toText . toFilePath
