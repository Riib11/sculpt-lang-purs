module Data.Default where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Type.Proxy (Proxy)

class Default a where
  default :: a

instance Default (Maybe a) where 
  default = Nothing

instance Default Unit where 
  default = unit

class DefaultT f where 
  defaultT :: forall a. Proxy a -> f a

class DefaultS f where 
  defaultS :: forall s. IsSymbol s => SProxy s -> f s 