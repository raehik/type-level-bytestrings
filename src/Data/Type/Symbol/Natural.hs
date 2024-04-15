{-# LANGUAGE UndecidableInstances #-}

{- | Parsing digit strings to 'Natural's from 'Symbol's.

May come in handy for sum-type generics. For example, say you wish to extract
some number from the constructor name. With these type functions, you may do so
on the type-level, instead of reifying immediately and performing term-level
operations. This way, we get good performance without relying on optimizations,
and problems with constructor names are caught at compile time.
-}

module Data.Type.Symbol.Natural where

import GHC.TypeLits
import DeFun.Core ( type (~>), type App, type (@@) )

type BinarySymbolToNat  sym = SymbolToNat  2 AsciiBinaryDigitValueSym  sym
type OctalSymbolToNat   sym = SymbolToNat  8 AsciiOctalDigitValueSym   sym
type DecimalSymbolToNat sym = SymbolToNat 10 AsciiDecimalDigitValueSym sym
type HexSymbolToNat     sym = SymbolToNat 16 AsciiHexDigitValueSym     sym

-- | The length of a symbol
type SymbolLength sym = SymbolLength' 0 (UnconsSymbol sym)

type family SymbolLength' len mchsym where
    SymbolLength' len 'Nothing = len
    SymbolLength' len ('Just '(_, sym)) =
        SymbolLength' (len+1) (UnconsSymbol sym)

type SymbolToNat base tfDigitValue sym =
    SymbolToNat' base tfDigitValue 0 (SymbolLength sym - 1) (UnconsSymbol sym)

type family SymbolToNat' base tfDigitValue n expo (mchsym :: Maybe (Char, Symbol)) where
    SymbolToNat' base tfDigitValue n expo 'Nothing = n
    SymbolToNat' base tfDigitValue n expo ('Just '(ch, sym)) =
        SymbolToNat' base tfDigitValue (n + (tfDigitValue @@ ch) * base^expo) (expo-1) (UnconsSymbol sym)

type AsciiBinaryDigitValueSym :: Char ~> Natural
data AsciiBinaryDigitValueSym a
type instance App AsciiBinaryDigitValueSym a = AsciiBinaryDigitValue a

type family AsciiBinaryDigitValue (ch :: Char) :: Natural where
    AsciiBinaryDigitValue '0' = 0
    AsciiBinaryDigitValue '1' = 1

type AsciiOctalDigitValueSym :: Char ~> Natural
data AsciiOctalDigitValueSym a
type instance App AsciiOctalDigitValueSym a = AsciiOctalDigitValue a

type family AsciiOctalDigitValue (ch :: Char) :: Natural where
    AsciiOctalDigitValue '0' = 0
    AsciiOctalDigitValue '1' = 1
    AsciiOctalDigitValue '2' = 2
    AsciiOctalDigitValue '3' = 3
    AsciiOctalDigitValue '4' = 4
    AsciiOctalDigitValue '5' = 5
    AsciiOctalDigitValue '6' = 6
    AsciiOctalDigitValue '7' = 7

type AsciiDecimalDigitValueSym :: Char ~> Natural
data AsciiDecimalDigitValueSym a
type instance App AsciiDecimalDigitValueSym a = AsciiDecimalDigitValue a

type family AsciiDecimalDigitValue (ch :: Char) :: Natural where
    AsciiDecimalDigitValue '0' = 0
    AsciiDecimalDigitValue '1' = 1
    AsciiDecimalDigitValue '2' = 2
    AsciiDecimalDigitValue '3' = 3
    AsciiDecimalDigitValue '4' = 4
    AsciiDecimalDigitValue '5' = 5
    AsciiDecimalDigitValue '6' = 6
    AsciiDecimalDigitValue '7' = 7
    AsciiDecimalDigitValue '8' = 8
    AsciiDecimalDigitValue '9' = 9

type AsciiHexDigitValueSym :: Char ~> Natural
data AsciiHexDigitValueSym a
type instance App AsciiHexDigitValueSym a = AsciiHexDigitValue a

type family AsciiHexDigitValue (ch :: Char) :: Natural where
    AsciiHexDigitValue '0' = 0
    AsciiHexDigitValue '1' = 1
    AsciiHexDigitValue '2' = 2
    AsciiHexDigitValue '3' = 3
    AsciiHexDigitValue '4' = 4
    AsciiHexDigitValue '5' = 5
    AsciiHexDigitValue '6' = 6
    AsciiHexDigitValue '7' = 7
    AsciiHexDigitValue '8' = 8
    AsciiHexDigitValue '9' = 9
    AsciiHexDigitValue 'a' = 10
    AsciiHexDigitValue 'A' = 10
    AsciiHexDigitValue 'b' = 11
    AsciiHexDigitValue 'B' = 11
    AsciiHexDigitValue 'c' = 12
    AsciiHexDigitValue 'C' = 12
    AsciiHexDigitValue 'd' = 13
    AsciiHexDigitValue 'D' = 13
    AsciiHexDigitValue 'e' = 14
    AsciiHexDigitValue 'E' = 14
    AsciiHexDigitValue 'f' = 15
    AsciiHexDigitValue 'F' = 15
