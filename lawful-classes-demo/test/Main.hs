{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.Trans.Class (lift)
import Hedgehog (forAll)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Lawful.Demo (evalDemoT, monadDemoLaws, monadDemoLaws')
import qualified Test.Lawful.Hedgehog as H
import qualified Test.Lawful.QuickCheck as Q
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Monadic (pick)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "DemoT"
    [ testGroup
        "monadDemoLaws"
        [ H.testLaws "using Hedgehog" evalDemoT (monadDemoLaws genHedgehog),
          Q.testLaws "using QuickCheck" evalDemoT (monadDemoLaws genQuickCheck)
        ],
      testGroup
        "monadDemoLaws'"
        [ H.testLaws "using Hedgehog" evalDemoT (monadDemoLaws' genHedgehog),
          Q.testLaws "using QuickCheck" evalDemoT (monadDemoLaws' genQuickCheck)
        ]
    ]
  where
    genHedgehog = lift $ forAll $ Gen.integral $ Range.linearBounded @Int
    genQuickCheck = lift $ pick $ arbitrary @Int
