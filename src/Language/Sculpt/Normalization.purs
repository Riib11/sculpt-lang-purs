module Language.Sculpt.Normalization where

import Language.Sculpt.Recurse.Base
import Language.Sculpt.Syntax
import Prelude
import Record
import Data.List (List(..))
import Partial.Unsafe (unsafeCrashWith)
import Undefined (undefined)

-- normalize
normalize :: forall meta. Term meta -> Term meta
normalize term = go { term }
  where
  go =
    recTerm
      { universe: \arg -> Universe arg.universe
      , pi: \arg -> Pi (arg.pi { type1 = normalize arg.pi.type1, type2 = normalize arg.pi.type2 })
      , lambda: \arg -> Lambda (arg.lambda { term = normalize arg.lambda.term })
      , neutral: \arg -> Neutral (arg.neutral { terms = normalize <$> arg.neutral.terms })
      , let_: \arg -> substitute arg.let_.binding (normalize arg.let_.term1) arg.let_.term2
      , hole: \arg -> Hole arg.hole
      }

-- substitute
-- [binding |-> termSub] term
substitute :: forall meta. Binding -> Term meta -> Term meta -> Term meta
substitute binding termSub term = go { binding, termSub, term }
  where
  go =
    recTerm
      { universe: \arg -> Universe arg.universe
      , pi: \arg -> Pi (arg.pi { type1 = substitute arg.binding arg.termSub arg.pi.type1, type2 = substitute binding arg.termSub arg.pi.type2 })
      , lambda: \arg -> Lambda (arg.lambda { term = substitute binding arg.termSub arg.lambda.term })
      , neutral:
          \arg ->
            if arg.neutral.id == arg.binding.id then
              apply arg.termSub arg.neutral.terms
            else
              Neutral (arg.neutral { terms = substitute binding arg.termSub <$> arg.neutral.terms })
      , let_:
          \arg ->
            Let
              ( arg.let_
                  { term1 = substitute binding arg.termSub arg.let_.term1
                  , type_ = substitute binding arg.termSub arg.let_.type_
                  , term2 = substitute binding arg.termSub arg.let_.term2
                  }
              )
      , hole: \arg -> Hole arg.hole
      }

-- apply
-- (λ x1 ... λ xM . b) a1 ... aN
apply :: forall meta. Term meta -> List (Term meta) -> Term meta
apply term terms = case terms of
  Nil -> term
  Cons termSub terms' -> case term of
    Lambda lambda -> apply (substitute lambda.binding termSub lambda.term) terms'
    _ -> unsafeCrashWith "apply: badly-typed"
