module TSH.Options where

import Exon (exon)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ReadM,
  auto,
  command,
  customExecParser,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  long,
  option,
  prefs,
  progDesc,
  readerError,
  short,
  showHelpOnEmpty,
  showHelpOnError,
  strOption,
  subparserInline,
  )
import Options.Applicative.Types (readerAsk)
import Path (Abs, Dir, File, Path, parseAbsFile)
import Path.IO (getCurrentDir)
import Prelude hiding (Mod)

import TSH.Data.Options (
  BitmapOptions (..),
  BitmapPreset (..),
  BitmapsOptions (..),
  Command (..),
  Options (..),
  OutputOptions (..),
  )

pathOption ::
  String ->
  (String -> Either e a) ->
  ReadM a
pathOption desc parse = do
  raw <- readerAsk
  leftA (const (readerError [exon|not a valid #{desc} path: #{raw}|])) (parse raw)

absFileOption :: ReadM (Path Abs File)
absFileOption = pathOption "absolute file" parseAbsFile

someNE :: Parser a -> Parser (NonEmpty a)
someNE p = (:|) <$> p <*> many p

presetValues :: ReadM BitmapPreset
presetValues =
  readerAsk >>= \case
    "id" -> pure Id
    "varid-start" -> pure VaridStart
    "varid-non-start" -> pure VaridNonStart
    "conid-start" -> pure ConidStart
    "symop" -> pure Symop
    "space" -> pure Space
    v -> fail [exon|Invalid category preset: #{v}|]

outputOptionsParser :: Parser OutputOptions
outputOptionsParser = do
  limit <- optional (option auto (long "limit" <> short 'l' <> help "Only use a limited number of code points"))
  file <- optional (option absFileOption (long "file" <> short 'f' <> help "Write to this absolute file path"))
  gapSize <- optional (option auto (long "gap-size" <> help "Minimum size of empty gaps to delete from the bitmap"))
  unicodeData <- optional (option absFileOption (long "unicode-data" <> help "Path to Unicode.txt"))
  name <- optional (strOption (long "name" <> short 'n' <> help "Name for the bitmap used in C functions"))
  pure OutputOptions {limit, file, gapSize, unicodeData, name}

presetParser :: Parser BitmapPreset
presetParser =
  option presetValues (long "preset" <> short 'p' <> help presetHelp)
  where
    presetHelp = "Optional category preset: varid, varid-start, varid-non-start, conid-start, symop"

bitmapCommand :: Parser BitmapOptions
bitmapCommand = do
  preset <- optional presetParser
  categories <- nonEmpty <$> many categoryOption
  output <- outputOptionsParser
  pure BitmapOptions {preset, categories, output}
  where
    categoryOption = strOption (long "category" <> short 'c' <> help "Categories to add to the preset")

bitmapsCommand :: Parser BitmapsOptions
bitmapsCommand = do
  presets <- someNE presetParser
  output <- outputOptionsParser
  pure BitmapsOptions {presets, output}

commands :: Mod CommandFields Command
commands =
  command "bitmap" (GenBitmap <$> info bitmapCommand (progDesc bitmapDesc))
  <>
  command "bitmaps" (GenBitmaps <$> info bitmapsCommand (progDesc bitmapsDesc))
  <>
  command "bitmap-test" (GenBitmapTest <$> info bitmapCommand (progDesc "Generate a test for bitmap"))
  where
    bitmapDesc =
      "Generate C code that checks whether a character belongs to some unicode categories"
    bitmapsDesc =
      "Generate multiple bitmaps"

optionsParser ::
  Path Abs Dir ->
  Parser Options
optionsParser cwd = do
  cmd <- hsubparser commands
  pure Options {cwd, command = cmd}

parseCli ::
  IO Options
parseCli = do
  cwd <- getCurrentDir
  customExecParser parserPrefs (info (optionsParser cwd <**> helper) desc)
  where
    parserPrefs =
      prefs (showHelpOnEmpty <> showHelpOnError <> subparserInline)
    desc =
      fullDesc <> header "Tools for tree-sitter-haskell development"
