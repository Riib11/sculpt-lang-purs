module Language.Sculpt.Recurse.Typing where

import Language.Sculpt.Syntax
import Prelude
import Prim.Row
import Record
import Data.Map as Map
import Language.Sculpt.Recurse.Base as Rec
import Language.Sculpt.Typing (Context, infer, inferNeutral)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

_context = Proxy :: Proxy "context"

_type_ = Proxy :: Proxy "type_"

_beta = Proxy :: Proxy "beta"

type RecUniverse row
  = Rec.RecUniverse ( context :: Context, type_ :: Term | row )

type RecPi row
  = Rec.RecPi ( context1 :: Context, type1 :: Term, context2 :: Context, type2 :: Term | row )

type RecLambda row
  = Rec.RecLambda ( context1 :: Context, type1 :: Term, context2 :: Context, type2 :: Term | row )

type RecNeutralTerm row
  = Rec.RecNeutralTerm ( context :: Context, type_ :: Term | row )

type RecTerm row
  = Rec.RecTerm ( context :: Context, type_ :: Term | row )

type RecTermFun row universe pi lambda neutral term a
  = Lacks "context" row =>
    Lacks "context1" row =>
    Lacks "context2" row =>
    Lacks "type_" row =>
    Lacks "type1" row =>
    Lacks "type2" row =>
    Rec.RecTermFun row universe pi lambda neutral term a

recTerm :: forall row a. RecTermFun row RecUniverse RecPi RecLambda RecNeutral RecTerm a
recTerm rec =
  Rec.recTerm
    { universe: rec.universe
    , pi:
        \arg -> case arg.type_ of
          Universe universe ->
            rec.pi
              $ union
                  { context1: arg.context
                  , type1: arg.type_
                  , context2: Map.insert arg.pi.termId arg.pi.type1 arg.context
                  , type2: arg.type_
                  }
              $ (delete _context $ delete _type_ arg)
          _ -> unsafeCrashWith "badly-typed"
    , lambda:
        \arg -> case arg.type_ of
          Pi pi ->
            rec.lambda
              $ union
                  { context1: arg.context
                  , type1: pi.type1
                  , context2: Map.insert arg.lambda.termId pi.type1 arg.context
                  , type2: pi.type2
                  }
              $ (delete _context $ delete _type_ arg)
          _ -> unsafeCrashWith "badly-typed"
    , neutral: rec.neutral
    }

type RecHole row
  = Rec.RecHole ( context :: Context, type_ :: Term | row )

type RecVariable row
  = Rec.RecVariable ( context :: Context, type_ :: Term | row )

type RecApplication row
  = Rec.RecApplication ( context :: Context, type1 :: Term, type2 :: Term | row )

type RecLet row
  = Rec.RecLet ( context1 :: Context, type1 :: Term, context2 :: Context, type2 :: Term | row )

type RecNeutral row
  = Rec.RecNeutral ( context :: Context, type_ :: Term | row )

type RecNeutralFun row hole variable application let_ neutral a
  = Lacks "context" row =>
    Lacks "type_" row =>
    Rec.RecNeutralFun row hole variable application let_ neutral a

recNeutral :: forall row a. RecNeutralFun row RecHole RecVariable RecApplication RecLet RecNeutral a
recNeutral rec =
  Rec.recNeutral
    { hole: rec.hole
    , variable: rec.variable
    , application:
        \arg ->
          let
            type1 = inferNeutral arg.context arg.application.neutral
          in
            case type1 of
              Pi pi ->
                rec.application
                  $ union
                      { context: arg.context
                      , type1: type1
                      , type2: pi.type1
                      }
                  $ (delete _context $ delete _type_ arg)
              _ -> unsafeCrashWith "badly-typed"
    , let_:
        \arg ->
          let
            type1 = infer arg.context arg.let_.term1
          in
            rec.let_
              $ union
                  { context1: arg.context
                  , type1: type1
                  , context2: Map.insert arg.let_.termId type1 arg.context
                  , type2: arg.type_
                  }
              $ (delete _context $ delete _type_ arg)
    }
