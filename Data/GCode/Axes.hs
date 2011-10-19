{-# LANGUAGE
    DeriveDataTypeable #-}
module Data.GCode.Axes
    ( -- * Types
      Axis(..), Position(..)
      -- * Compact syntax for @'Position'@s
    , a, b, c, i, j, k, u, v, w, x, y, z
    ) where

import Data.Typeable ( Typeable )
import Data.Data     ( Data     )

-- | An axis of machine motion.
data Axis = A | B | C | I | J | K | U | V | W | X | Y | Z
    deriving (Eq, Ord, Read, Show, Typeable, Data, Enum, Bounded)

-- | Positioning of an axis.
data Position = Pos Axis Rational
    deriving (Eq, Ord, Read, Show, Typeable, Data)

a, b, c, i, j, k, u, v, w, x, y, z :: Rational -> Position
a = Pos A
b = Pos B
c = Pos C
i = Pos I
j = Pos J
k = Pos K
u = Pos U
v = Pos V
w = Pos W
x = Pos X
y = Pos Y
z = Pos Z
