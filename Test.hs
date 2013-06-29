import Test.HUnit ((@?=))

import qualified Test.Framework                 as TF
import qualified Test.Framework.Providers.HUnit as TF
import qualified Test.HUnit                     as HU

import qualified Math.OEIS as OEIS

--------------------------------------------------------------------------------

main :: IO ()
main =
    TF.defaultMain tests

--------------------------------------------------------------------------------

tests :: [TF.Test]
tests =
    [ TF.testGroup "main"
      [ TF.testCase "lookupSequenceByID" test_lookupSequenceByID
      ]
    ]

--------------------------------------------------------------------------------

test_lookupSequenceByID :: IO ()
test_lookupSequenceByID = do
    let m = OEIS.lookupSequenceByID "A000040"
    case m of
      Nothing -> HU.assertFailure ""
      Just r -> do
          OEIS.catalogNums r @?= ["A000040", "M0652", "N0241"]
          OEIS.description r @?= "The prime numbers."
          OEIS.keywords    r @?= [OEIS.Core, OEIS.Nonn, OEIS.Nice, OEIS.Easy, OEIS.Changed]

{-# ANN module "HLint: ignore Use camelCase" #-}
