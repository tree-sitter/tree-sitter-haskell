module TSH.Unicode.Category where

import qualified Data.Map.Strict as Map

import TSH.Data.Options (BitmapPreset (..))
import TSH.Data.Unicode (Cat (Cat), CatAbbr (..), CatLong)

catMap :: Map CatAbbr CatLong
catMap =
  [
    -- ("C", "Other"),
    ("Cc", "Control"),
    ("Cf", "Format"),
    ("Cn", "Unassigned"),
    ("Co", "Private_Use"),
    ("Cs", "Surrogate"),

    -- ("L", "Letter"),
    ("LC", "Cased_Letter"),
    ("Ll", "Lowercase_Letter"),
    ("Lm", "Modifier_Letter"),
    ("Lo", "Other_Letter"),
    ("Lt", "Titlecase_Letter"),
    ("Lu", "Uppercase_Letter"),

    -- ("M", "Mark"),
    ("Mc", "Spacing_Mark"),
    ("Me", "Enclosing_Mark"),
    ("Mn", "Nonspacing_Mark"),

    -- ("N", "Number"),
    ("Nd", "Decimal_Number"),
    ("Nl", "Letter_Number"),
    ("No", "Other_Number"),

    -- ("P", "Punctuation"),
    ("Pc", "Connector_Punctuation"),
    ("Pd", "Dash_Punctuation"),
    ("Pe", "Close_Punctuation"),
    ("Pf", "Final_Punctuation"),
    ("Pi", "Initial_Punctuation"),
    ("Po", "Other_Punctuation"),
    ("Ps", "Open_Punctuation"),

    -- ("S", "Symbol"),
    ("Sc", "Currency_Symbol"),
    ("Sk", "Modifier_Symbol"),
    ("Sm", "Math_Symbol"),
    ("So", "Other_Symbol"),

    -- ("Z", "Separator"),
    ("Zl", "Line_Separator"),
    ("Zp", "Paragraph_Separator"),
    ("Zs", "Space_Separator")
  ]

cats :: Map CatAbbr Cat
cats = Map.mapWithKey Cat catMap

presetCats :: BitmapPreset -> Set CatAbbr
presetCats = \case
  Id -> ["LC", "Ll", "Lm", "Lo", "Lt", "Lu", "Mn", "Nd", "Nl", "No"]
  VaridStart -> ["Ll", "Lo"]
  VaridNonStart -> ["LC", "Lm", "Lt", "Lu", "Mn", "Nd", "Nl", "No"]
  ConidStart -> ["Lu", "Lt"]
  Symop -> ["Pc", "Pd", "Po", "Sc", "Sk", "Sm", "So"]
  Space -> ["Zs"]
