module Language.Sculpt.Typing where

import Language.Sculpt.Level
import Language.Sculpt.Syntax
import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Language.Sculpt.Level (Level)
import Language.Sculpt.Level as Level
import Undefined (undefined)

type Context
  = Map TermId Term

infer :: Context -> Term -> Term
infer context = case _ of
  Universe universe -> Universe { level: zero, meta: Nothing }
  Pi pi -> Universe { level: levelTerm pi.type1 ⨆ levelTerm pi.type2, meta: Nothing }
  Lambda lambda -> Pi {}
  NeutralTerm neutral -> ?a

inferNeutral :: Context -> Neutral -> Term
inferNeutral = undefined

levelTerm :: Term -> Level
levelTerm = case _ of
  Universe { level } -> level + one
  _ -> undefined

-- levelNeutral :: Neutral -> Level 
-- levelNeutral = case _ of 
