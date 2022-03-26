module Language.Sculpt.Metadata where

import Prelude

data Metadata
  = Universe {}
  | Pi { indented :: Boolean }
  | Lambda { indented :: Boolean }
  | Let { indented :: Boolean }
  | Hole {}
  | Neutral { indented :: Boolean }
