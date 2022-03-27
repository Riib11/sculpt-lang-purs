module Language.Sculpt.Level where

import Data.Natural
import Prelude
import Data.Enum (class Enum, pred, succ)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

data Level
  = Level Natural
  | LevelInfinity

derive instance Eq Level
derive instance Ord Level

instance Semiring Level where 
  zero = Level zero 

  one = Level one

  add (Level m) (Level n) = Level (m + n)
  add LevelInfinity _ = LevelInfinity
  add _ LevelInfinity = LevelInfinity

  mul (Level m) (Level n) = Level (m * n)
  mul LevelInfinity _ = LevelInfinity
  mul _ LevelInfinity = LevelInfinity


to :: Int -> Level
to = Level <<< intToNat

from :: Level -> Int
from (Level n) = natToInt n
from LevelInfinity = unsafeCrashWith "from: LevelInfinity"

meet :: Level -> Level -> Level
meet = min

join :: Level -> Level -> Level
join = max

infix 6 meet as ⨅
infix 6 join as ⊔
