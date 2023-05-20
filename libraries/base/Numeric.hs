{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Odds and ends, mostly functions for reading and showing
-- 'RealFloat'-like kind of values.
--
-----------------------------------------------------------------------------

module Numeric (

        -- * Showing

        showSigned,

        showIntAtBase,
        showInt,
        showBin,
        showHex,
        showOct,

        showEFloat,
        showFFloat,
        showGFloat,
        showFFloatAlt,
        showGFloatAlt,
        showFloat,
        showHFloat,

        floatToDigits,

        -- * Reading

        -- | /NB:/ 'readInt' is the \'dual\' of 'showIntAtBase',
        -- and 'readDec' is the \`dual\' of 'showInt'.
        -- The inconsistent naming is a historical accident.

        readSigned,

        readInt,
        readBin,
        readDec,
        readOct,
        readHex,

        readFloat,

        lexDigits,

        -- * Miscellaneous

        fromRat,
        Floating(..)

        ) where

import GHC.Base
import GHC.Err
import GHC.Float
import GHC.Maybe
import GHC.Num
import GHC.Read
import GHC.Real
import GHC.Show
import GHC.Show.Integer
import Text.ParserCombinators.ReadP( ReadP, readP_to_S, pfail )
import qualified Text.Read.Lex as L

-- $setup
-- >>> import Prelude

-- -----------------------------------------------------------------------------
-- Reading

-- | Reads an /unsigned/ integral value in an arbitrary base.
readInt :: Num a
  => a                  -- ^ the base
  -> (Char -> Bool)     -- ^ a predicate distinguishing valid digits in this base
  -> (Char -> Int)      -- ^ a function converting a valid digit character to an 'Int'
  -> ReadS a
readInt base isDigit valDigit = readP_to_S (L.readIntP base isDigit valDigit)

-- | Read an unsigned number in binary notation.
--
-- >>> readBin "10011"
-- [(19,"")]
readBin :: (Eq a, Num a) => ReadS a
readBin = readP_to_S L.readBinP

-- | Read an unsigned number in octal notation.
--
-- >>> readOct "0644"
-- [(420,"")]
readOct :: (Eq a, Num a) => ReadS a
readOct = readP_to_S L.readOctP

-- | Read an unsigned number in decimal notation.
--
-- >>> readDec "0644"
-- [(644,"")]
readDec :: (Eq a, Num a) => ReadS a
readDec = readP_to_S L.readDecP

-- | Read an unsigned number in hexadecimal notation.
-- Both upper or lower case letters are allowed.
--
-- >>> readHex "deadbeef"
-- [(3735928559,"")]
readHex :: (Eq a, Num a) => ReadS a
readHex = readP_to_S L.readHexP

-- | Reads an /unsigned/ 'RealFrac' value,
-- expressed in decimal scientific notation.
readFloat :: RealFrac a => ReadS a
readFloat = readP_to_S readFloatP

readFloatP :: RealFrac a => ReadP a
readFloatP =
  do tok <- L.lex
     case tok of
       L.Number n -> return $ fromRational $ L.numberToRational n
       _          -> pfail

-- It's turgid to have readSigned work using list comprehensions,
-- but it's specified as a ReadS to ReadS transformer
-- With a bit of luck no one will use it.

-- | Reads a /signed/ 'Real' value, given a reader for an unsigned value.
readSigned :: (Real a) => ReadS a -> ReadS a
readSigned readPos = readParen False read'
                     where read' r  = read'' r ++
                                      (do
                                        ("-",s) <- lex r
                                        (x,t)   <- read'' s
                                        return (-x,t))
                           read'' r = do
                               (str,s) <- lex r
                               (n,"")  <- readPos str
                               return (n,s)

-- -----------------------------------------------------------------------------
-- Showing

-- Controlling the format and precision of floats. The code that
-- implements the formatting itself is in @PrelNum@ to avoid
-- mutual module deps.

{-# SPECIALIZE showEFloat ::
        Maybe Int -> Float  -> ShowS,
        Maybe Int -> Double -> ShowS #-}
{-# SPECIALIZE showFFloat ::
        Maybe Int -> Float  -> ShowS,
        Maybe Int -> Double -> ShowS #-}
{-# SPECIALIZE showGFloat ::
        Maybe Int -> Float  -> ShowS,
        Maybe Int -> Double -> ShowS #-}

-- | Show a signed 'RealFloat' value
-- using scientific (exponential) notation (e.g. @2.45e2@, @1.5e-3@).
--
-- In the call @'showEFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then at most @d@ digits after the decimal point are shown.
showEFloat    :: (RealFloat a) => Maybe Int -> a -> ShowS

-- | Show a signed 'RealFloat' value
-- using standard decimal notation (e.g. @245000@, @0.0015@).
--
-- In the call @'showFFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then at most @d@ digits after the decimal point are shown.
showFFloat    :: (RealFloat a) => Maybe Int -> a -> ShowS

-- | Show a signed 'RealFloat' value
-- using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
--
-- In the call @'showGFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then at most @d@ digits after the decimal point are shown.
showGFloat    :: (RealFloat a) => Maybe Int -> a -> ShowS

showEFloat d x =  showString (formatRealFloat FFExponent d x)
showFFloat d x =  showString (formatRealFloat FFFixed d x)
showGFloat d x =  showString (formatRealFloat FFGeneric d x)

-- | Show a signed 'RealFloat' value
-- using standard decimal notation (e.g. @245000@, @0.0015@).
--
-- This behaves as 'showFFloat', except that a decimal point
-- is always guaranteed, even if not needed.
--
-- @since 4.7.0.0
showFFloatAlt    :: (RealFloat a) => Maybe Int -> a -> ShowS

-- | Show a signed 'RealFloat' value
-- using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
--
-- This behaves as 'showFFloat', except that a decimal point
-- is always guaranteed, even if not needed.
--
-- @since 4.7.0.0
showGFloatAlt    :: (RealFloat a) => Maybe Int -> a -> ShowS

showFFloatAlt d x =  showString (formatRealFloatAlt FFFixed d True x)
showGFloatAlt d x =  showString (formatRealFloatAlt FFGeneric d True x)

{- | Show a floating-point value in the hexadecimal format,
similar to the @%a@ specifier in C's printf.

  >>> showHFloat (212.21 :: Double) ""
  "0x1.a86b851eb851fp7"
  >>> showHFloat (-12.76 :: Float) ""
  "-0x1.9851ecp3"
  >>> showHFloat (-0 :: Double) ""
  "-0x0p+0"
-}
showHFloat :: RealFloat a => a -> ShowS
showHFloat = showString . fmt
  where
  fmt x
    | isNaN x                   = "NaN"
    | isInfinite x              = (if x < 0 then "-" else "") ++ "Infinity"
    | x < 0 || isNegativeZero x = '-' : cvt (-x)
    | otherwise                 = cvt x

  cvt x
    | x == 0 = "0x0p+0"
    | otherwise =
      case floatToDigits 2 x of
        r@([], _) -> error $ "Impossible happened: showHFloat: " ++ show r
        (d:ds, e) -> "0x" ++ show d ++ frac ds ++ "p" ++ show (e-1)

  -- Given binary digits, convert them to hex in blocks of 4
  -- Special case: If all 0's, just drop it.
  frac digits
    | allZ digits = ""
    | otherwise   = "." ++ hex digits
    where
    hex ds =
      case ds of
        []                -> ""
        [a]               -> hexDigit a 0 0 0 ""
        [a,b]             -> hexDigit a b 0 0 ""
        [a,b,c]           -> hexDigit a b c 0 ""
        a : b : c : d : r -> hexDigit a b c d (hex r)

  hexDigit a b c d = showHex (8*a + 4*b + 2*c + d)

  allZ xs = case xs of
              x : more -> x == 0 && allZ more
              []       -> True

