module Language.Sculpt.Recurse.Base where

import Language.Sculpt.Syntax
import Prelude
import Prim.Row
import Record
import Data.List (List(..))
import Language.Sculpt.Level (Level(..))
import Type.Proxy (Proxy(..))

_universe = Proxy :: Proxy "universe"

_pi = Proxy :: Proxy "pi"

_lambda = Proxy :: Proxy "lambda"

_hole = Proxy :: Proxy "hole"

_let_ = Proxy :: Proxy "let_"

type RecUniverse meta row
  = ( universe :: Record (Universe meta) | row )

type RecPi meta row
  = ( pi :: Record (Pi meta) | row )

type RecLambda meta row
  = ( lambda :: Record (Lambda meta) | row )

type RecHole meta row
  = ( hole :: Record (Hole meta) | row )

type RecNeutral meta row
  = ( neutral :: Record (Neutral meta) | row )

type RecLet meta row
  = ( let_ :: Record (Let meta) | row )

type RecArgumentsNil meta row
  = ( | row )

type RecArgumentsCons meta row
  = ( term :: Term meta, terms :: List (Term meta) | row )

type RecTerm meta row
  = ( term :: Term meta | row )

type RecArguments meta row
  = ( terms :: List (Term meta) | row )

-- recTerm
type RecTermFun meta row universe pi lambda neutral let_ hole term a
  = Lacks "term" row =>
    { universe :: Record (universe meta row) -> a
    , pi :: Record (pi meta row) -> a
    , lambda :: Record (lambda meta row) -> a
    , neutral :: Record (neutral meta row) -> a
    , let_ :: Record (let_ meta row) -> a
    , hole :: Record (hole meta row) -> a
    } ->
    Record (term meta row) -> a

recTerm :: forall meta row a. RecTermFun meta row RecUniverse RecPi RecLambda RecNeutral RecLet RecHole RecTerm a
recTerm rec arg = case arg.term of
  Universe universe -> rec.universe $ union { universe } $ delete _term arg
  Pi pi -> rec.pi $ union { pi } $ delete _term arg
  Lambda lambda -> rec.lambda $ union { lambda } $ delete _term arg
  Neutral neutral -> rec.neutral $ union { neutral } $ delete _term arg
  Let let_ -> rec.let_ $ union { let_ } $ delete _term arg
  Hole hole -> rec.hole $ union { hole } $ delete _term arg

-- recArguments
type RecArgumentsFun meta row nil cons arguments a
  = Lacks "term" row =>
    Lacks "terms" row =>
    { nil :: Record (nil meta row) -> a
    , cons :: Record (cons meta row) -> a
    } ->
    Record (arguments meta row) -> a

recArguments :: forall meta row a. RecArgumentsFun meta row RecArgumentsNil RecArgumentsCons RecArguments a
recArguments rec arg = case arg.terms of
  Nil -> rec.nil $ delete _terms arg
  Cons term terms -> rec.cons $ union { term, terms } $ delete _terms arg
