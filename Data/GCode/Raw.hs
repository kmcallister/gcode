{-# LANGUAGE
    DeriveDataTypeable
  , GeneralizedNewtypeDeriving #-}
module Data.GCode.Raw
    ( Word(..), Token(..), Line(..), File(..)
    , render
    ) where

import Data.Typeable ( Typeable )
import Data.Data     ( Data     )
import Data.Monoid
import Text.PrettyPrint hiding ( render )
import Numeric


-- | A G-code word.
data Word = Word Char Rational
    deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | A component of a line.
data Token
    -- | A G-code word.
    = TWord    Word

    -- | A G-code comment.  Should not contain '@)@'.  Certain comments may be
    -- interpreted specially by the implementation.
    | TComment String
    deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | A single line.
newtype Line = Line [Token]
    deriving (Eq, Ord, Read, Show, Typeable, Data, Monoid)

-- | An entire file.
newtype File = File [Line]
    deriving (Eq, Ord, Read, Show, Typeable, Data, Monoid)


-- | Convert a @'File'@ to the concrete G-code syntax.
--
-- Calling @'show'@ provides the @'String'@ syntax.
render :: File -> Doc
render (File lns) = vcat (map rLine lns) where
    rLine (Line xs) = hsep (map rTok xs)
    rTok (TWord (Word c r)) = char c <> fmt (fromRational r)
    rTok (TComment xs) = parens (text xs)

    fmt :: Double -> Doc
    fmt x = text (showFFloat Nothing x "")
