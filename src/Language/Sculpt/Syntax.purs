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

type Universe metaUniverse metaPi metaLambda metaNeutral metaLet metaHole
  = ( level :: Level | metaUniverse )

type Pi metaUniverse metaPi metaLambda metaNeutral metaLet metaHole
  = ( termName :: TermName, termId :: TermId, type1 :: Term metaUniverse metaPi metaLambda metaNeutral metaLet metaHole, type2 :: Term metaUniverse metaPi metaLambda metaNeutral metaLet metaHole | metaPi )

type Lambda metaUniverse metaPi metaLambda metaNeutral metaLet metaHole
  = ( termName :: TermName, termId :: TermId, type :: Term metaUniverse metaPi metaLambda metaNeutral metaLet metaHole, term :: Term metaUniverse metaPi metaLambda metaNeutral metaLet metaHole | metaLambda )

type Let metaUniverse metaPi metaLambda metaNeutral metaLet metaHole
  = ( termName :: TermName, termId :: TermId, term1 :: Term metaUniverse metaPi metaLambda metaNeutral metaLet metaHole, term2 :: Term metaUniverse metaPi metaLambda metaNeutral metaLet metaHole | metaLet )

type Hole metaUniverse metaPi metaLambda metaNeutral metaLet metaHole
  = ( holeId :: HoleId, weakening :: Set TermId, substitution :: Substitution metaUniverse metaPi metaLambda metaNeutral metaLet metaHole | metaHole )

type Neutral metaUniverse metaPi metaLambda metaNeutral metaLet metaHole
  = ( termId :: TermId, terms :: List (Term metaUniverse metaPi metaLambda metaNeutral metaLet metaHole) | metaNeutral )

data Term metaUniverse metaPi metaLambda metaNeutral metaLet metaHole
  = Universe (Record (Universe metaUniverse metaPi metaLambda metaNeutral metaLet metaHole))
  | Pi (Record (Pi metaUniverse metaPi metaLambda metaNeutral metaLet metaHole))
  | Lambda (Record (Lambda metaUniverse metaPi metaLambda metaNeutral metaLet metaHole))
  | Neutral (Record (Neutral metaUniverse metaPi metaLambda metaNeutral metaLet metaHole))
  | Let (Record (Let metaUniverse metaPi metaLambda metaNeutral metaLet metaHole))
  | Hole (Record (Hole metaUniverse metaPi metaLambda metaNeutral metaLet metaHole))

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
type Substitution metaUniverse metaPi metaLambda metaNeutral metaLet metaHole
  = Map TermId (Term metaUniverse metaPi metaLambda metaNeutral metaLet metaHole)

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
