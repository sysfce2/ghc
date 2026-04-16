{-# LANGUAGE MagicHash, UnboxedTuples, LexicalNegation, ExtendedLiterals #-}

import GHC.Prim
import GHC.Int

-- Cmm constant folding tests for vector operations

data IntX2 = IX2# Int64X2#
data IntX4 = IX4# Int32X4#

instance Show IntX2 where
  show (IX2# d) = case (unpackInt64X2# d) of
    (# a, b #) -> show ((I64# a), (I64# b))

instance Show IntX4 where
  show (IX4# v) = case (unpackInt32X4# v) of
    (# a, b, c, d #) -> show ((I32# a), (I32# b), (I32# c), (I32# d))

testFoldPlus = do
  let v1    = packInt64X2# (# 111111#Int64,  121212#Int64 #)
  let v2    = packInt64X2# (# 121212#Int64,  131313#Int64 #)
  print $ IX2# $ plusInt64X2# v1 v2 -- expect to see 232323 and 252525 here,
                                    -- and not 111111, 121212, or 131313

testFoldMax = do
  let v1    = broadcastInt32X4# 333333#Int32
  let v2    = broadcastInt32X4# 333332#Int32
  print $ IX4# $ maxInt32X4# v1 v2 -- expect to see 333333 here and not 333332

testFoldMin = do
  let v1 = broadcastInt32X4# 474747#Int32
  let v2 = broadcastInt32X4# 474748#Int32
  print $ IX4# $ minInt32X4# v1 v2 -- expect to see 474747 here and not 474748

testNeg = do
  let v1 = broadcastInt32X4# 343434#Int32
  print $ IX4# $ negateInt32X4# v1 -- expect to see -343434 here, not positive 343434


testInserts = do
  let v1 = broadcastInt32X4# 353535#Int32
  let v2 = insertInt32X4# v1 363636#Int32 0#
  let (# a, _, _, _ #) = unpackInt32X4# v2
  print $ (I32# a) -- expect to see 363636 here, not 353535


testInserts2 = do
  let v1 = broadcastInt32X4# 373737#Int32
  let v2 = insertInt32X4# v1 383838#Int32 0#
  let v3 = plusInt32X4# v2 (broadcastInt32X4# 393939#Int32)
  let (# a, _, _, _ #) = unpackInt32X4# v3
  print $ (I32# a) -- expect to see 777777 == 383838+393939 here, and not 373737, 383838, or 393939

{-# INLINE testOverwrite #-}
testOverwrite :: Int64X2# -> IO ()
testOverwrite v = do
  let v1 = insertInt64X2# v 404040#Int64 0#
  let v2 = insertInt64X2# v1 404041#Int64 1#
  print $ IX2# v2 -- expect <404040, 404041> to appear in the cmm as a single assignment,
                  -- rather than a series of inserts

{-# NOINLINE testExtractFromInsert #-}
testExtractFromInsert :: Int32X4# -> IO ()
testExtractFromInsert v = do
  let v2 = insertInt32X4# v 454545#Int32 3#
  let (# _, _, _, d #) = unpackInt32X4# v2
  print (I32# d) -- 454545 should fold as a constant even though v is a runtime value


main = do
  testFoldPlus
  testFoldMax
  testFoldMin
  testNeg
  testInserts
  testInserts2
  testOverwrite (broadcastInt64X2# 414141#Int64)
  testExtractFromInsert (broadcastInt32X4# 464646#Int32)

