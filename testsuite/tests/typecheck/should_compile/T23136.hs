{-# LANGUAGE TypeFamilies, MonomorphismRestriction #-}

module T23136 where

type family F a where
  F Int = Bool

class C a where
  op :: a -> ()

instance C Int where
  op _ = ()

f :: a -> F a
f _ = undefined

-- this requires the monomorphism restriction to be interesting
g = \x -> (f x :: Bool, op x)