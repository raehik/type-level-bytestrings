{-# LANGUAGE UndecidableInstances #-}

module Tmp where

import GHC.TypeLits
import Data.Type.Symbol.Natural

type family FromRight eab where
    FromRight ('Left  a) = TypeError a
    FromRight ('Right b) = b

class X a where type Asdf a :: Natural
instance X (sym :: Symbol) where
    type Asdf sym = FromRightParseResult sym (ParseHexSymbol sym)
