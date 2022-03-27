module Language.Sculpt.Recurse.Typing where

import Language.Sculpt.Level
import Language.Sculpt.Syntax
import Prelude
import Prim.Row
import Record
import Data.Array.NonEmpty (fromArray)
import Data.List (List(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, maybe, maybe')
import Data.Semigroup.Foldable (maximum)
import Language.Sculpt.Normalization (normalize)
import Language.Sculpt.Recurse.Base as Rec
import Partial (crashWith)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type Context meta
  = Map Id ({ type_ :: Term meta, binding :: Binding })

insertContext :: forall meta. Binding -> Term meta -> Context meta -> Context meta
insertContext binding@{ id, name } type_ = Map.insert id { type_, binding }

_context = Proxy :: Proxy "context"

type RecUniverse meta row
  = Rec.RecUniverse meta ( context :: Context meta, level :: Level | row )

type RecPi meta row
  = Rec.RecPi meta ( context1 :: Context meta, level1 :: Level, context2 :: Context meta, level2 :: Level | row )

type RecLambda meta row
  = Rec.RecLambda meta ( context1 :: Context meta, type1 :: Term meta, context2 :: Context meta, type2 :: Term meta | row )

type RecHole meta row
  = Rec.RecHole meta ( context :: Context meta, type_ :: Term meta | row )

type RecNeutral meta row
  = Rec.RecNeutral meta ( context :: Context meta, type_ :: Term meta | row )

type RecLet meta row
  = Rec.RecLet meta ( context1 :: Context meta, type1 :: Term meta, context2 :: Context meta, type2 :: Term meta | row )

type RecArgumentsNil meta row
  = Rec.RecArgumentsNil meta ( context :: Context meta, type_ :: Term meta | row )

type RecArgumentsCons meta row
  = Rec.RecArgumentsCons meta ( context :: Context meta, type1 :: Term meta, type2 :: Term meta | row )

type RecArguments meta row
  = Rec.RecArguments meta ( context :: Context meta, type_ :: Term meta | row )

type RecTerm meta row
  = Rec.RecTerm meta ( context :: Context meta, type_ :: Term meta | row )

-- recTerm
type RecTermFun meta row universe pi lambda neutral let_ hole term a
  = Lacks "context" row =>
    Lacks "context1" row =>
    Lacks "context2" row =>
    Lacks "type_" row =>
    Lacks "type1" row =>
    Lacks "type2" row =>
    Rec.RecTermFun meta row universe pi lambda neutral let_ hole term a

recTerm :: forall meta row a. RecTermFun meta row RecUniverse RecPi RecLambda RecNeutral RecLet RecHole RecTerm a
recTerm rec =
  Rec.recTerm
    { universe:
        \arg -> case arg.type_ of
          Universe { level } -> rec.universe $ union { level } (delete _type_ arg)
          _ -> unsafeCrashWith "recTerm:universe: badly-typed"
    , pi:
        \arg ->
          let
            context2 = insertContext arg.pi.binding (normalize arg.pi.type1) arg.context
          in
            case arg.type_ of
              Universe { level } ->
                rec.pi
                  $ union
                      { context1: arg.context
                      , level1: inferLevel arg.context arg.pi.type1 level
                      , context2
                      , level2: inferLevel context2 arg.pi.type2 level
                      }
                      (delete _context $ delete _type_ arg)
              _ -> unsafeCrashWith "recTerm:pi: badly-typed"
    , lambda:
        \arg -> case arg.type_ of
          Pi pi ->
            rec.lambda
              $ union
                  { context1: arg.context
                  , type1: pi.type1
                  , context2: insertContext arg.lambda.binding (normalize pi.type1) arg.context
                  , type2: pi.type2
                  }
              $ (delete _context $ delete _type_ arg)
          _ -> unsafeCrashWith "badly-typed"
    , neutral: rec.neutral
    , let_:
        \arg ->
          rec.let_
            $ union
                { context1: arg.context
                , type1: arg.let_.type_
                , context2: insertContext arg.let_.binding (normalize arg.let_.type_) arg.context
                , type2: arg.type_
                }
            $ (delete _context $ delete _type_ arg)
    , hole: rec.hole
    }

-- recArguments
type RecArgumentsFun meta row nil cons arguments a
  = Lacks "context" row =>
    Lacks "type_" row =>
    Lacks "type1" row =>
    Lacks "type2" row =>
    Lacks "term" row =>
    Lacks "terms" row =>
    Rec.RecArgumentsFun meta row nil cons arguments a

recArguments :: forall meta row a. RecArgumentsFun meta row RecArgumentsNil RecArgumentsCons RecArguments a
recArguments rec =
  Rec.recArguments
    { nil: rec.nil
    , cons:
        \arg -> case arg.type_ of
          Pi pi -> rec.cons $ union { type1: pi.type1, type2: pi.type2 } (delete _type_ arg)
          _ -> unsafeCrashWith "recArguments: badly-typed"
    }

-- normalizes the inferred type
lookupTerm :: forall meta. Id -> Context meta -> Term meta
lookupTerm id context =
  maybe'
    (\_ -> unsafeCrashWith "lookupTerm: id not in context")
    (_.type_)
    (Map.lookup id context)

lookupName :: forall meta. Id -> Context meta -> Name
lookupName id context =
  maybe'
    (\_ -> unsafeCrashWith "lookupName: id not in context")
    (_.binding.name)
    (Map.lookup id context)

inferLevel :: forall meta. Context meta -> Term meta -> Level -> Level
inferLevel context type_ level = case type_ of
  Universe universe -> universe.level + one
  Pi pi ->
    inferLevel context pi.type1 level
      ⊔ inferLevel (insertContext pi.binding pi.type1 context) pi.type2 level
  Neutral neutral ->
    inferLevel context
      (inferTerm_Arguments { terms: neutral.terms, context: context, type_: lookupTerm neutral.id context })
      level
  -- undefined -- inferTerm_Arguments context neutral level
  Hole _ -> level
  Lambda _ -> unsafeCrashWith "inferLevel: abnormal"
  Let _ -> unsafeCrashWith "inferLevel: abnormal"

inferTerm_Arguments :: forall meta. { terms :: List (Term meta), context :: Context meta, type_ :: Term meta } -> Term meta
inferTerm_Arguments =
  recArguments
    { nil: \arg -> arg.type_
    , cons: \arg -> inferTerm_Arguments { terms: arg.terms, context: arg.context, type_: arg.type2 }
    }
