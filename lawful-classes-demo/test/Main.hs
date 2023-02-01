{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Control.Monad.Trans.Class (lift)
import Hedgehog (forAll)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TastyUtils (mayFail)
import Test.Lawful.Demo (evalDemoT, evalUnlawfulDemoT, monadDemoLaws, monadDemoLaws')
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
    "lawful-classes-demo"
    [ testGroup
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
        ],
      testGroup
        "UnlawfulDemoT"
        [ mayFail $
            testGroup
              "monadDemoLaws"
              [ H.testLaws "using Hedgehog" evalUnlawfulDemoT (monadDemoLaws genHedgehog),
                Q.testLaws "using QuickCheck" evalUnlawfulDemoT (monadDemoLaws genQuickCheck)
              ]
        ]
    ]
  where
    genHedgehog = lift $ forAll $ Gen.integral $ Range.linearBounded @Int
    genQuickCheck = lift $ pick $ arbitrary @Int
