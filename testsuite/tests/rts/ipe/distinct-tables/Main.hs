module Main where

import GHC.InfoProv
import qualified X

main = do
  print =<< whereFrom cafA1
  print =<< whereFrom cafA2
  print =<< whereFrom cafB1
  print =<< whereFrom cafB2
  print =<< whereFrom cafC1
  print =<< whereFrom cafC2
  print =<< whereFrom (ACon ())
  print =<< whereFrom cafXA
  print =<< whereFrom X.cafXA1
  print =<< whereFrom X.cafXA2
  print =<< whereFrom (X.ACon ())
  print =<< whereFrom (BCon cafA1)
  print =<< whereFrom (CCon (cafA1, BCon (ACon ())))

data A = ACon ()
data B = BCon A
data C = CCon (A, B)

cafA1 = ACon ()
cafA2 = ACon ()
cafB1 = BCon cafA1
cafB2 = BCon cafA2
cafC1 = CCon (cafA1, cafB1)
cafC2 = CCon (cafA2, cafB2)

cafXA = X.ACon ()
