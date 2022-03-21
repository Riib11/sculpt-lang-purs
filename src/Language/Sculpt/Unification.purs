module Language.Sculpt.Unification where

import Language.Sculpt.Syntax
import Prelude
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Undefined (undefined)

type HoleSubstitution
  = Map HoleId Term

unify :: Term -> Term -> Maybe HoleSubstitution
unify _ _ = undefined
