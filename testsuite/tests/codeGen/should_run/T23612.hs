module Main where

data X = A1

foo :: X -> ()
foo A1 = ()

bar :: X -> ()
bar y = foo y

main = seq (map bar [A1]) (return ())
