module Language.Sculpt.Level where

import Data.Natural
import Prelude

import Data.Enum (class Enum)

newtype Level
  = Level Natural

derive newtype instance Eq Level
derive newtype instance Ord Level
derive newtype instance Enum Level
derive newtype instance Semiring Level

to :: Int -> Level
to = Level <<< intToNat

from :: Level -> Int
from (Level n) = natToInt n

meet :: Level -> Level -> Level
meet (Level m) (Level n) = Level (m `min` n)

join :: Level -> Level -> Level
join (Level m) (Level n) = Level (m `max` n)

infix 6 join as ⨆
infix 6 meet as ⨅
