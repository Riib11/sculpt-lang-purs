module Language.Sculpt.Recurse.Wrapping where

import Language.Sculpt.Syntax
import Prelude
import Record
import Data.List (List)
import Data.List as List
import Data.Maybe (fromJust)
import Language.Sculpt.Recurse.Typing as Rec
import Partial.Unsafe (unsafePartial)
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

-- Wrap*
type Wrap meta a
  = a -> Term meta

type WrapTerm meta
  = Wrap meta (Term meta)

type WrapTerms meta
  = List (WrapTerm meta)

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
                { wrap_terms:
                    List.mapWithIndex
                      (\i _ term -> Neutral (arg.neutral { terms = unsafePartial $ fromJust $ List.updateAt i term arg.neutral.terms }))
                      arg.neutral.terms
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

type WrapSigTerm meta
  = Wrap meta ({ type_ :: Term meta, term :: Term meta, neutral :: Record (Neutral meta), iArg :: Int })

type WrapSigTerms meta
  = List (WrapTerm meta)

-- Rec*
type RecSigUniverse meta row
  = Rec.RecUniverse meta ( wrap_parent :: WrapSigTerm meta | row )

type RecSigPi meta row
  = Rec.RecPi meta ( wrap_type1 :: WrapSigTerm meta, wrap_type2 :: WrapSigTerm meta, wrap_parent :: WrapSigTerm meta | row )

type RecSigLambda meta row
  = Rec.RecLambda meta ( wrap_term :: WrapSigTerm meta, wrap_parent :: WrapSigTerm meta | row )

type RecSigHole meta row
  = Rec.RecHole meta ( wrap_parent :: WrapSigTerm meta | row )

type RecSigNeutral meta row
  = Rec.RecNeutral meta ( wrap_terms :: WrapSigTerms meta, wrap_parent :: WrapSigTerm meta | row )

type RecSigLet meta row
  = Rec.RecLet meta ( wrap_type :: WrapSigTerm meta, wrap_term1 :: WrapSigTerm meta, wrap_term2 :: WrapSigTerm meta, wrap_parent :: WrapSigTerm meta | row )

type RecSigArgumentsNil meta row
  = Rec.RecArgumentsNil meta ( wrap_parent :: WrapSigTerm meta | row )

type RecSigArgumentsCons meta row
  = Rec.RecArgumentsCons meta ( wrap_term :: WrapSigTerm meta, wrap_terms :: Wrap meta (List (Term meta)), wrap_parent :: WrapSigTerm meta | row )

type RecSigArguments meta row
  = Rec.RecArguments meta ( wrap_terms :: WrapSigTerms meta, wrap_parent :: WrapSigTerm meta | row )

type RecSigTerm meta row
  = Rec.RecTerm meta ( wrap_term :: WrapSigTerm meta | row )

-- recTerm
type RecSigTermFun meta row universe pi lambda neutral let_ hole term a
  = Lacks "wrap_term" row =>
    Rec.RecTermFun meta row universe pi lambda neutral let_ hole term a

-- input must be normalized
recSig :: forall meta row a. RecSigTermFun meta row RecSigUniverse RecSigPi RecSigLambda RecSigNeutral RecSigLet RecSigHole RecSigTerm a
recSig rec =
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
                { wrap_type1: \{ type_, term, neutral, iArg } -> arg.wrap_term ?a
                , wrap_type2: \type2 -> undefined
                , wrap_parent: arg.wrap_term
                }
            $ delete _wrap_term arg
    , lambda: -- impossible
        \arg ->
          rec.lambda
            $ union
                { wrap_term: \arg -> undefined
                , wrap_parent: arg.wrap_term
                }
            $ delete _wrap_term arg
    , neutral:
        \arg ->
          rec.neutral
            $ union
                { wrap_terms: List.mapWithIndex (\i _ term -> undefined) arg.neutral.terms
                , wrap_parent: arg.wrap_term
                }
            $ delete _wrap_term arg
    , let_: -- impossible
        \arg ->
          rec.let_
            $ union
                { wrap_type: \arg -> undefined
                , wrap_term1: \arg -> undefined
                , wrap_term2: \arg -> undefined
                , wrap_parent: arg.wrap_term
                }
            $ delete _wrap_term arg
    , hole:
        \arg ->
          rec.hole
            $ union { wrap_parent: arg.wrap_term }
            $ delete _wrap_term arg
    }
