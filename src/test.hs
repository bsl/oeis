module Main where

import qualified Math.OEIS as OEIS

main :: IO ()
main =
    case OEIS.lookupSequenceByID "A000040" of
      Just s -> do
          print $ OEIS.catalogNums s
          print $ OEIS.description s
          print $ OEIS.keywords    s

          let sd = OEIS.sequenceData s
          if sd == OEIS.extendSequence (take 10 sd)
            then putStrLn "lookupSequence ok"
            else putStrLn "lookupSequence failed"

      Nothing -> print "unable to look up A000040"
