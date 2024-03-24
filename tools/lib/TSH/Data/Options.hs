module TSH.Data.Options where

import Path (Abs, Dir, File, Path)

import TSH.Data.Unicode (CatAbbr)

data BitmapPreset =
  Id
  |
  VaridStart
  |
  VaridNonStart
  |
  ConidStart
  |
  Symop
  |
  Space
  deriving stock (Eq, Show, Generic)

presetSlug :: BitmapPreset -> Text
presetSlug = \case
  Id -> "identifier"
  VaridStart -> "varid_start"
  VaridNonStart -> "varid_non_start"
  ConidStart -> "conid_start"
  Symop -> "symop"
  Space -> "space"

data OutputOptions =
  OutputOptions {
    limit :: Maybe Word64,
    file :: Maybe (Path Abs File),
    gapSize :: Maybe Word64,
    unicodeData :: Maybe (Path Abs File),
    name :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

data BitmapOptions =
  BitmapOptions {
    preset :: Maybe BitmapPreset,
    categories :: Maybe (NonEmpty CatAbbr),
    output :: OutputOptions
  }
  deriving stock (Eq, Show, Generic)

data BitmapsOptions =
  BitmapsOptions {
    presets :: NonEmpty BitmapPreset,
    output :: OutputOptions
  }
  deriving stock (Eq, Show, Generic)

data Command =
  GenBitmap BitmapOptions
  |
  GenBitmaps BitmapsOptions
  |
  GenBitmapTest BitmapOptions
  deriving stock (Eq, Show, Generic)

data Options =
  Options {
    cwd :: Path Abs Dir,
    command :: Command
  }
  deriving stock (Eq, Show, Generic)
