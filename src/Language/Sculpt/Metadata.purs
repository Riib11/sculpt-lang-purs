module Language.Sculpt.Metadata where

import Prelude
import Undefined (undefined)

data Metadata
  = Universe {}
  | Pi { indented :: Boolean }
  | Lambda { indented :: Boolean }
  | Let { indented :: Boolean }
  | Hole {}
  | Neutral { indented :: Boolean }
