module Language.Sculpt.Recurse.Template where

import Language.Sculpt.Syntax
import Prelude
import Prim.Row
import Record
import Language.Sculpt.Recurse.Base as Rec

type RecUniverse row
  = Rec.RecUniverse row

type RecPi row
  = Rec.RecPi row

type RecLambda row
  = Rec.RecLambda row

type RecNeutralTerm row
  = Rec.RecNeutralTerm row

type RecTerm row
  = Rec.RecTerm row

type RecTermFun row universe pi lambda neutral term a
  = Rec.RecTermFun row universe pi lambda neutral term a

recTerm :: forall row a. RecTermFun row RecUniverse RecPi RecLambda RecNeutral RecTerm a
recTerm rec =
  Rec.recTerm
    { universe: rec.universe
    , pi: rec.pi
    , lambda: rec.lambda
    , neutral: rec.neutral
    }

type RecHole row
  = Rec.RecHole row

type RecVariable row
  = Rec.RecVariable row

type RecApplication row
  = Rec.RecApplication row

type RecLet row
  = Rec.RecLet row

type RecNeutral row
  = Rec.RecNeutral row

type RecNeutralFun row hole variable application let_ neutral a
  = Rec.RecNeutralFun row hole variable application let_ neutral a

recNeutral :: forall row a. RecNeutralFun row RecHole RecVariable RecApplication RecLet RecNeutral a
recNeutral rec =
  Rec.recNeutral
    { hole: rec.hole
    , variable: rec.variable
    , application: rec.application
    , let_: rec.let_
    }
