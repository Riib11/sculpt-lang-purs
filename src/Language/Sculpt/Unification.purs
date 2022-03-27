module Language.Sculpt.Unification where

import Language.Sculpt.Syntax
import Prelude
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Undefined (undefined)

type HoleSubstitution meta
  = Map HoleId (Term meta)

unify :: forall meta. Term meta -> Term meta -> Maybe (HoleSubstitution meta)
unify _ _ = undefined

applyHoleSubstitution :: forall meta. HoleSubstitution meta -> Term meta -> Term meta
applyHoleSubstitution = undefined
