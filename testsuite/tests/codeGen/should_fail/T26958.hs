{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Main where
import GHC.Exts
import GHC.IO (IO(..))

-- Test that -fcheck-prim-bounds catches OOB access in copySmallArray#
-- when the length argument is a non-literal (variable). See #26958.
main :: IO ()
main = IO $ \s0 ->
  case newSmallArray# 1# () s0 of { (# s1, srcm #) ->
  case unsafeFreezeSmallArray# srcm s1 of { (# s2, src #) ->
  case sizeofSmallArray# src of { n# ->
  case newSmallArray# 1# () s2 of { (# s3, dst #) ->
  case copySmallArray# src 0# dst 5# n# s3 of
    s4 -> (# s4, () #) }}}}
