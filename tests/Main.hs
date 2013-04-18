module Main where

import           Test.QuickCheck
import           Test.Framework (defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

main = defaultMain tests

tests =
    [ testGroup "boring" boringTests
    ]



-- boring tests!
boring :: Int -> Bool
boring x = const "hello" x == "hello"

boringTests = [ testProperty "boring" boring ]
