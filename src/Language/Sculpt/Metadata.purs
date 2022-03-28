module Language.Sculpt.Metadata where

import Data.Leibniz
import Prelude
import Data.Default (class DefaultS, class DefaultT)
import Data.Symbol (reflectSymbol)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

type Universe
  = {}

type Pi
  = { indented :: Boolean }

type Lambda
  = { indented :: Boolean }

type Let
  = { indented :: Boolean }

type Hole
  = {}

type Neutral
  = { indented :: Boolean }

data Metadata (label :: Symbol)
  = Universe Universe (label ~ "Universe")
  | Pi Pi (label ~ "Pi")
  | Lambda Lambda (label ~ "Lambda")
  | Let Let (label ~ "Let")
  | Hole Hole (label ~ "Hole")
  | Neutral Neutral (label ~ "Neutral")

toUniverse :: Metadata "Universe" -> Universe
toUniverse = case _ of
  Universe meta _ -> meta
  _ -> unsafeCrashWith "impossible"

instance defaultSMetadata :: DefaultS Metadata where
  defaultS sproxy = case reflectSymbol sproxy of
    "Universe" -> Universe {} (unsafeCoerce unit)
    "Pi" -> Pi { indented: true } (unsafeCoerce unit)
    "Lambda" -> Lambda { indented: true } (unsafeCoerce unit)
    "Let" -> Let { indented: true } (unsafeCoerce unit)
    "Hole" -> Hole {} (unsafeCoerce unit)
    "Neutral" -> Neutral { indented: true } (unsafeCoerce unit)
    _ -> unsafeCrashWith "defaultSMetadata:defaultS: impossible"

example :: Metadata "Universe"
example = Universe {} (Leibniz identity)
