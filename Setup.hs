module Main where

import System.Process (system)

import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, runTests)

main :: IO ()
main =
    defaultMainWithHooks $ simpleUserHooks { runTests = runTests' }
  where
    runTests' _ _ _ _ = do
        system "runhaskell -Wall -i./src src/test.hs"
        return ()
