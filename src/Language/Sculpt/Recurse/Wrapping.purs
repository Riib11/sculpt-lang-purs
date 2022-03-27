module Language.Sculpt.Recurse.Wrapping where

import Language.Sculpt.Syntax
import Prelude
import Record
import Data.List (List)
import Language.Sculpt.Recurse.Typing as Rec
import Prim.Row (class Lacks)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

_wrap_term = Proxy :: Proxy "wrap_term"

_wrap_term1 = Proxy :: Proxy "wrap_term1"

_wrap_term2 = Proxy :: Proxy "wrap_term2"

_wrap_terms = Proxy :: Proxy "wrap_terms"

_wrap_type1 = Proxy :: Proxy "wrap_term1"

_wrap_type2 = Proxy :: Proxy "wrap_term2"

_wrap_parent = Proxy :: Proxy "wrap_parent"

-- Wrap
type Wrap meta a
  = a -> Term meta

type WrapTerm meta
  = Wrap meta (Term meta)

type WrapTerms meta
  = Wrap meta (List (Term meta))

-- Rec*
type RecUniverse meta row
  = Rec.RecUniverse meta ( wrap_parent :: WrapTerm meta | row )

type RecPi meta row
  = Rec.RecPi meta ( wrap_type1 :: WrapTerm meta, wrap_type2 :: WrapTerm meta, wrap_parent :: WrapTerm meta | row )

type RecLambda meta row
  = Rec.RecLambda meta ( wrap_term :: WrapTerm meta, wrap_parent :: WrapTerm meta | row )

type RecHole meta row
  = Rec.RecHole meta ( wrap_parent :: WrapTerm meta | row )

type RecNeutral meta row
  = Rec.RecNeutral meta ( wrap_terms :: WrapTerms meta, wrap_parent :: WrapTerm meta | row )

type RecLet meta row
  = Rec.RecLet meta ( wrap_type :: WrapTerm meta, wrap_term1 :: WrapTerm meta, wrap_term2 :: WrapTerm meta, wrap_parent :: WrapTerm meta | row )

type RecArgumentsNil meta row
  = Rec.RecArgumentsNil meta ( wrap_parent :: WrapTerm meta | row )

type RecArgumentsCons meta row
  = Rec.RecArgumentsCons meta ( wrap_term :: WrapTerm meta, wrap_terms :: Wrap meta (List (Term meta)), wrap_parent :: WrapTerm meta | row )

type RecArguments meta row
  = Rec.RecArguments meta ( wrap_terms :: WrapTerms meta, wrap_parent :: WrapTerm meta | row )

type RecTerm meta row
  = Rec.RecTerm meta ( wrap_term :: WrapTerm meta | row )

-- recTerm
type RecTermFun meta row universe pi lambda neutral let_ hole term a
  = Lacks "wrap_term" row =>
    Rec.RecTermFun meta row universe pi lambda neutral let_ hole term a

recTerm :: forall meta row a. RecTermFun meta row RecUniverse RecPi RecLambda RecNeutral RecLet RecHole RecTerm a
recTerm rec =
  Rec.recTerm
    { universe:
        \arg ->
          rec.universe
            $ union { wrap_parent: arg.wrap_term }
            $ delete _wrap_term arg
    , pi:
        \arg ->
          rec.pi
            $ union
                { wrap_type1: \type1 -> arg.wrap_term $ Pi (arg.pi { type1 = type1 })
                , wrap_type2: \type2 -> arg.wrap_term $ Pi (arg.pi { type2 = type2 })
                , wrap_parent: arg.wrap_term
                }
            $ delete _wrap_term arg
    , lambda:
        \arg ->
          rec.lambda
            $ union
                { wrap_term: \term -> arg.wrap_term $ Lambda (arg.lambda { term = term })
                , wrap_parent: arg.wrap_term
                }
            $ delete _wrap_term arg
    , neutral:
        \arg ->
          rec.neutral
            $ union
                { wrap_terms: \terms -> arg.wrap_term $ Neutral (arg.neutral { terms = terms })
                , wrap_parent: arg.wrap_term
                }
            $ delete _wrap_term arg
    , let_:
        \arg ->
          rec.let_
            $ union
                { wrap_type: \type_ -> arg.wrap_term $ Let (arg.let_ { type_ = type_ })
                , wrap_term1: \term1 -> arg.wrap_term $ Let (arg.let_ { term1 = term1 })
                , wrap_term2: \term2 -> arg.wrap_term $ Let (arg.let_ { term2 = term2 })
                , wrap_parent: arg.wrap_term
                }
            $ delete _wrap_term arg
    , hole:
        \arg ->
          rec.hole
            $ union { wrap_parent: arg.wrap_term }
            $ delete _wrap_term arg
    }
