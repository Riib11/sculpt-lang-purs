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

data Syntax
  = SyntaxUniverse
  | SyntaxPi
  | SyntaxLambda
  | SyntaxLet
  | SyntaxHole
  | SyntaxNeutral

type Universe meta
  = ( level :: Level, meta :: Maybe meta )

type Pi meta
  = ( binding :: Binding, type1 :: Term meta, type2 :: Term meta, meta :: Maybe meta )

type Lambda meta
  = ( binding :: Binding, term :: Term meta, meta :: Maybe meta )

type Let meta
  = ( binding :: Binding, type_ :: Term meta, term1 :: Term meta, term2 :: Term meta, meta :: Maybe meta )

type Hole meta
  = ( holeId :: HoleId, weakening :: Set Id, substitution :: Substitution meta, meta :: Maybe meta )

type Neutral meta
  = ( id :: Id, terms :: List (Term meta), meta :: Maybe meta )

data Term meta
  = Universe (Record (Universe meta))
  | Pi (Record (Pi meta))
  | Lambda (Record (Lambda meta))
  | Neutral (Record (Neutral meta))
  | Let (Record (Let meta))
  | Hole (Record (Hole meta))

-- Binding
type Binding
  = { id :: Id, name :: Name }

-- Id
newtype Id
  = Id UUID

derive instance eqId :: Eq Id

derive instance ordId :: Ord Id

freshId :: Unit -> Id
freshId _ = unsafePerformEffect do Id <$> genUUID

data Name
  = Name String
  | IgnoreName

derive instance eqName :: Eq Name

derive instance ordName :: Ord Name

-- HoleId
newtype HoleId
  = HoleId UUID

freshHoleId :: Unit -> HoleId
freshHoleId _ = unsafePerformEffect do HoleId <$> genUUID

derive newtype instance eqHoleId :: Eq HoleId

derive newtype instance ordHoleId :: Ord HoleId

-- Substitution
type Substitution meta
  = Map Id (Term meta)

-- Proxies
_level = Proxy :: Proxy "level"

_meta = Proxy :: Proxy "meta"

_name = Proxy :: Proxy "name"

_id = Proxy :: Proxy "id"

_type_ = Proxy :: Proxy "type_"

_type1 = Proxy :: Proxy "type1"

_type2 = Proxy :: Proxy "type2"

_term = Proxy :: Proxy "term"

_terms = Proxy :: Proxy "terms"

_term1 = Proxy :: Proxy "term"

_term2 = Proxy :: Proxy "term"

_holeId = Proxy :: Proxy "holeId"

_neutral = Proxy :: Proxy "neutral"
