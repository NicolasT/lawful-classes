{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Hedgehog (withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import TastyUtils (mayFail)
import Test.Lawful.Demo
  ( evalDemo,
    evalDemoT,
    evalUnlawfulDemo,
    evalUnlawfulDemoT,
    monadDemoLaws,
    monadDemoLaws',
  )
import qualified Test.Lawful.Hedgehog as H
import qualified Test.Lawful.QuickCheck as Q
import Test.QuickCheck (arbitrary, once)
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
            [ H.testLaws "using Hedgehog" evalDemoT (monadDemoLaws (H.forAll genHedgehog)),
              Q.testLaws "using QuickCheck" evalDemoT (monadDemoLaws (Q.forAll genQuickCheck))
            ],
          testGroup
            "monadDemoLaws'"
            [ H.testLaws "using Hedgehog" evalDemoT (monadDemoLaws' (H.forAll genHedgehog)),
              Q.testLaws "using QuickCheck" evalDemoT (monadDemoLaws' (Q.forAll genQuickCheck))
            ]
        ],
      testGroup
        "Demo"
        [ testGroup
            "monadDemoLaws"
            [ -- 'Demo' is not a transformer, so there's not really a way to
              -- generate arbitrary values: all we can do is pass in a single
              -- test exemplar (lifted using 'pure'), a bit like a unit-test.
              -- Given we'll check the laws with only a single value, it
              -- doesn't make sense to run the (same) test 100 (or more) times,
              -- hence we can use the @testLawsWith@ functions to modify the
              -- properties, telling Hedgehog or QuickCheck to run the test
              -- only once.
              H.testLawsWith (withTests 1) "using Hedgehog" (pure . evalDemo) (monadDemoLaws _11),
              Q.testLawsWith once "using QuickCheck" (pure . evalDemo) (monadDemoLaws _11)
            ]
        ],
      testGroup
        "UnlawfulDemoT"
        [ mayFail $
            testGroup
              "monadDemoLaws"
              [ H.testLaws "using Hedgehog" evalUnlawfulDemoT (monadDemoLaws (H.forAll genHedgehog)),
                Q.testLaws "using QuickCheck" evalUnlawfulDemoT (monadDemoLaws (Q.forAll genQuickCheck))
              ]
        ],
      testGroup
        "UnlawfulDemo"
        [ mayFail $
            testGroup
              "monadDemoLaws"
              [ H.testLawsWith (withTests 1) "using Hedgehog" (pure . evalUnlawfulDemo) (monadDemoLaws _11),
                Q.testLawsWith once "using QuickCheck" (pure . evalUnlawfulDemo) (monadDemoLaws _11)
              ]
        ]
    ]
  where
    genHedgehog = Gen.integral (Range.linearBounded @Int)
    genQuickCheck = arbitrary @Int
    _11 :: (Applicative m) => m Int
    _11 = pure 11
