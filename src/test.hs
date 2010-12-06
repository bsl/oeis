module Main where

import qualified Math.OEIS as OEIS

main :: IO ()
main = print $ OEIS.getSequenceByID "A000040"
