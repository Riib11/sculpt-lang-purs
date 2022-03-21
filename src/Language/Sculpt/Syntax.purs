module Language.Sculpt.Syntax where

import Data.Natural
import Language.Sculpt.Level
import Prelude
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.UUID (UUID)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type Universe
  = { level :: Level, meta :: Maybe {} }

type Pi
  = { termId :: TermId, type1 :: Term, type2 :: Term, meta :: Maybe { termName :: TermName } }

type Lambda
  = { termId :: TermId, type1 :: Term, term :: Term, meta :: Maybe { termName :: TermName } }

type Let
  = { termId :: TermId, term1 :: Term, term2 :: Term, meta :: Maybe { termName :: TermName } }

type Hole
  = { holeId :: HoleId, type_ :: Term, weakening :: Set TermId, meta :: Maybe {} }

type Variable
  = { termId :: TermId, meta :: Maybe {} }

type Application
  = { term1 :: Term, term2 :: Term, meta :: Maybe {} }

data Term
  = Universe Universe
  | Pi Pi
  | Lambda Lambda
  | Variable Variable
  | Application Application
  | Let Let
  | Hole Hole

newtype TermId
  = TermId UUID

derive instance eqTermId :: Eq TermId

derive instance ordTermId :: Ord TermId

data TermName
  = TermName String
  | IgnoreTermName

derive instance eqTermName :: Eq TermName

derive instance ordTermName :: Ord TermName

newtype HoleId
  = HoleId UUID

derive newtype instance eqHoleId :: Eq HoleId

derive newtype instance ordHoleId :: Ord HoleId

_level = Proxy :: Proxy "level"

_meta = Proxy :: Proxy "meta"

_termName = Proxy :: Proxy "termName"

_termId = Proxy :: Proxy "termId"

_type_ = Proxy :: Proxy "type_"

_type1 = Proxy :: Proxy "type1"

_type2 = Proxy :: Proxy "type2"

_term = Proxy :: Proxy "term"

_term1 = Proxy :: Proxy "term"

_term2 = Proxy :: Proxy "term"

_holeId = Proxy :: Proxy "holeId"

_neutral = Proxy :: Proxy "neutral"
