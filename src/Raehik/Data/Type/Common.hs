module Raehik.Data.Type.Common where

import GHC.TypeLits
import Data.Type.Ord ( OrdCond )

-- | Simplified common type-level conditional.
type IfNatLte n m fThen fElse = OrdCond (CmpNat n m) fThen fThen fElse

-- | Append two type-level lists.
type family l ++ r where
    (a ': l) ++ r = a ': l ++ r
    '[] ++ r = r
