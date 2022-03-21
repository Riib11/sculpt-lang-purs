module Language.Sculpt.Recurse.Base where

import Language.Sculpt.Syntax
import Prelude
import Prim.Row
import Record

type RecUniverse row
  = ( universe :: Universe | row )

type RecPi row
  = ( pi :: Pi | row )

type RecLambda row
  = ( lambda :: Lambda | row )

type RecNeutralTerm row
  = ( neutral :: Neutral | row )

type RecTerm row
  = ( term :: Term | row )

type RecTermFun (row :: Row Type) universe pi lambda neutral term a
  = Lacks "term" row =>
    { universe :: Record (universe row) -> a
    , pi :: Record (pi row) -> a
    , lambda :: Record (lambda row) -> a
    , neutral :: Record (neutral row) -> a
    } ->
    Record (term row) -> a

recTerm :: forall row a. RecTermFun row RecUniverse RecPi RecLambda RecNeutralTerm RecTerm a
recTerm rec arg = case arg.term of
  Universe universe -> rec.universe $ union { universe } $ delete _term arg
  Pi pi -> rec.pi $ union { pi } $ delete _term arg
  Lambda lambda -> rec.lambda $ union { lambda } $ delete _term arg
  NeutralTerm neutral -> rec.neutral $ union { neutral } $ delete _term arg

type RecHole row
  = ( hole :: Hole | row )

type RecVariable row
  = ( variable :: Variable | row )

type RecApplication row
  = ( application :: Application | row )

type RecLet row
  = ( let_ :: Let | row )

type RecNeutral row
  = ( neutral :: Neutral | row )

type RecNeutralFun (row :: Row Type) hole variable application let_ neutral a
  = Lacks "neutral" row =>
    { hole :: Record (hole row) -> a
    , variable :: Record (variable row) -> a
    , application :: Record (application row) -> a
    , let_ :: Record (let_ row) -> a
    } ->
    Record (neutral row) -> a

recNeutral :: forall row a. RecNeutralFun row RecHole RecVariable RecApplication RecLet RecNeutral a
recNeutral rec arg = case arg.neutral of
  Hole hole -> rec.hole $ union { hole } $ delete _neutral arg
  Variable variable -> rec.variable $ union { variable } $ delete _neutral arg
  Application application -> rec.application $ union { application } $ delete _neutral arg
  Let let_ -> rec.let_ $ union { let_ } $ delete _neutral arg
