module Language.Sculpt.Recurse.Changing where

import Language.Sculpt.Syntax
import Prelude
import Control.Alternative (guard)
import Data.Default (class Default, class DefaultS, default, defaultS)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Language.Sculpt.Normalization (normalize)
import Language.Sculpt.Recurse.Typing (Context)
import Language.Sculpt.Recurse.Wrapping as Rec
import Language.Sculpt.Unification (applyHoleSubstitution, unify)
import Record (union)
import Undefined (undefined)

-- Given an input, determines if the change is valid.
-- If valid, yields a (lazy) computation that performs the change.
type Change meta a
  = a -> Maybe (Unit -> Term meta)

-- Changes that are common to all `Term`s.
type TermChanges meta row
  = ( dig :: Change meta Unit
    , enLambda :: Change meta Unit -- encapsulate in the `term` of a Lambda
    , enPi :: Change meta Unit -- encapsulate in the `type2` of a Pi 
    , enLet :: Change meta Unit -- encapsulate in the `term2` of a Let 
    | row
    )

-- Rec*
type RecUniverse meta row
  = Rec.RecUniverse meta
      ( TermChanges meta
          ( | row )
      )

type RecPi meta row
  = Rec.RecPi meta
      ( TermChanges meta
          ( | row )
      )

type RecLambda meta row
  = Rec.RecLambda meta
      ( TermChanges meta
          ( | row )
      )

type RecHole meta row
  = Rec.RecHole meta
      ( TermChanges meta
          ( fill :: Change meta ({ type_ :: Term meta, term :: Term meta })
          , apply :: Change meta Id
          | row
          )
      )

type RecNeutral meta row
  = Rec.RecNeutral meta
      ( TermChanges meta
          ( | row )
      )

type RecLet meta row
  = Rec.RecLet meta
      ( TermChanges meta
          ( | row )
      )

type RecArgumentsNil meta row
  = Rec.RecArgumentsNil meta ( | row )

type RecArgumentsCons meta row
  = Rec.RecArgumentsCons meta ( | row )

type RecArguments meta row
  = Rec.RecArguments meta ( | row )

type RecTerm meta row
  = Rec.RecTerm meta ( | row )

-- recTerm
type RecTermFun meta row universe pi lambda neutral let_ hole term a
  = Rec.RecTermFun meta row universe pi lambda neutral let_ hole term a

recTerm :: forall meta row a. DefaultS meta => RecTermFun meta row RecUniverse RecPi RecLambda RecNeutral RecLet RecHole RecTerm a
recTerm rec =
  Rec.recTerm
    { universe: undefined
    , pi: undefined
    , lambda: undefined
    , neutral: undefined
    , let_: undefined
    , hole:
        \arg ->
          rec.hole
            $ union
                { dig: \_ -> Nothing -- can't dig at a hole 
                , enLambda:
                    \_ -> case normalize arg.type_ of
                      Pi pi ->
                        Just \_ ->
                          arg.wrap_parent
                            $ Lambda
                                { binding: { id: freshId unit, name: pi.binding.name }
                                , term: Hole (freshHole unit)
                                , meta: defaultS SProxy
                                }
                      Hole hole ->
                        let
                          pi =
                            { binding: freshBinding unit
                            , type1: Hole (freshHole unit)
                            , type2: Hole (freshHole unit)
                            , meta: defaultS SProxy
                            }
                        in
                          do
                            holeSub <- unify (normalize arg.type_) (Pi pi)
                            pure \_ ->
                              applyHoleSubstitution holeSub $ arg.wrap_parent
                                $ Lambda
                                    { binding: freshBinding unit
                                    , term: Hole (freshHole unit)
                                    , meta: defaultS SProxy
                                    }
                      _ -> Nothing
                -- --
                -- holeSub <- unify (normalize arg.type_) ?a
                -- --
                -- pure \_ ->
                --   applyHoleSubstitution holeSub $ arg.wrap_parent (Lambda { binding: ?a, term: ?a, meta: default })
                , enPi: \_ -> undefined -- TODO
                , enLet: \_ -> undefined -- TODO
                , fill:
                    \({ type_, term }) -> do
                      -- well-scoped
                      guard $ checkWellScoped arg.context term
                      -- unifies type 
                      -- TODO: are these types already normalized?
                      holeSub <- unify (normalize arg.type_) (normalize type_)
                      --
                      pure \_ ->
                        applyHoleSubstitution holeSub $ arg.wrap_parent term
                , apply:
                    \id -> do
                      -- well-scoped
                      -- compatible output-type
                      --
                      undefined :: Maybe (Unit -> Term meta)
                }
                arg
    }

checkWellScoped :: forall meta. Context meta -> Term meta -> Boolean
checkWellScoped context term = undefined
