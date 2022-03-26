module Language.Sculpt.Syntax where

import Data.Natural
import Language.Sculpt.Level
import Prelude
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.UUID (UUID, genUUID)
import Effect.Unsafe (unsafePerformEffect)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type Universe meta
  = ( level :: Level
    , meta :: meta
    )

type Pi meta
  = ( termName :: TermName
    , termId :: TermId
    , type1 :: Term meta
    , type2 :: Term meta
    , meta :: meta
    )

type Lambda meta
  = ( termName :: TermName
    , termId :: TermId
    , term :: Term meta
    , meta :: meta
    )

type Let meta
  = ( termName :: TermName
    , termId :: TermId
    , type_ :: Term meta
    , term1 :: Term meta
    , term2 :: Term meta
    , meta :: meta
    )

type Hole meta
  = ( holeId :: HoleId
    , weakening :: Set TermId
    , substitution :: Substitution meta
    , meta :: meta
    )

type Neutral meta
  = ( termId :: TermId
    , terms :: List (Term meta)
    , meta :: meta
    )

data Term meta
  = Universe (Record (Universe meta))
  | Pi (Record (Pi meta))
  | Lambda (Record (Lambda meta))
  | Neutral (Record (Neutral meta))
  | Let (Record (Let meta))
  | Hole (Record (Hole meta))

-- TermId
newtype TermId
  = TermId UUID

derive instance eqTermId :: Eq TermId

derive instance ordTermId :: Ord TermId

freshTermId :: Unit -> TermId
freshTermId _ = unsafePerformEffect do TermId <$> genUUID

data TermName
  = TermName String
  | IgnoreTermName

derive instance eqTermName :: Eq TermName

derive instance ordTermName :: Ord TermName

-- HoleId
newtype HoleId
  = HoleId UUID

freshHoleId :: Unit -> HoleId
freshHoleId _ = unsafePerformEffect do HoleId <$> genUUID

derive newtype instance eqHoleId :: Eq HoleId

derive newtype instance ordHoleId :: Ord HoleId

-- Substitution
type Substitution meta
  = Map TermId (Term meta)

-- Proxies
_level = Proxy :: Proxy "level"

_meta = Proxy :: Proxy "meta"

_termName = Proxy :: Proxy "termName"

_termId = Proxy :: Proxy "termId"

_type_ = Proxy :: Proxy "type_"

_type1 = Proxy :: Proxy "type1"

_type2 = Proxy :: Proxy "type2"

_term = Proxy :: Proxy "term"

_terms = Proxy :: Proxy "terms"

_term1 = Proxy :: Proxy "term"

_term2 = Proxy :: Proxy "term"

_holeId = Proxy :: Proxy "holeId"

_neutral = Proxy :: Proxy "neutral"
