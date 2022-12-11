module Main where

import Control.Concurrent.STM
import Control.Exception
import GHC.Conc
import GHC.Exts.Heap.Closures
import GHC.Exts.DecodeStack
import GHC.Stack.CloneStack
import TestUtils

main :: IO ()
main = do
  (stackSnapshot, decodedStack) <-
    atomically $
      catchSTM @SomeException (unsafeIOToSTM getDecodedStack) throwSTM

  assertStackInvariants stackSnapshot decodedStack
  assertThat
    "Stack contains one catch stm frame"
    (== 1)
    (length $ filter isCatchStmFrame decodedStack)
  assertThat
    "Stack contains one atomically frame"
    (== 1)
    (length $ filter isAtomicallyFrame decodedStack)

getDecodedStack :: IO (StackSnapshot, [Closure])
getDecodedStack = do
  s <-cloneMyStack
  fs <- decodeStack' s
  pure (s, fs)

isCatchStmFrame :: Closure -> Bool
isCatchStmFrame (CatchStmFrame _ _) = True
isCatchStmFrame _ = False

isAtomicallyFrame :: Closure -> Bool
isAtomicallyFrame (AtomicallyFrame _ _) = True
isAtomicallyFrame _ = False
