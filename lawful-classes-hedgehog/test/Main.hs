module Main (main) where

import Control.Monad.State (evalState)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Trans.State (evalStateT)
import Hedgehog (withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Lawful.Hedgehog (forAll, forAllShow, testLaws, testLawsWith)
import Test.Lawful.Types (Laws, assert, discard)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)

main :: IO ()
main = defaultMain tests

monadStateLaws :: (MonadState s m, Eq s) => m s -> Laws m
monadStateLaws gen =
  [ ( "get returns what was put",
      do
        a0 <- get
        a <- gen

        if a == a0
          then discard
          else do
            put a
            a' <- get
            assert $ a' == a
    )
  ]

newtype NoShow = NoShow Int
  deriving (Eq)

tests :: TestTree
tests =
  testGroup
    "lawful-classes-hedgehog"
    [ testGroup
        "StateT Int"
        [ testLaws
            "monadStateLaws"
            (`evalStateT` (0 :: Int))
            (monadStateLaws $ forAll $ Gen.integral Range.linearBounded)
        ],
      testGroup
        "StateT NoShow"
        [ testLaws
            "monadStateLaws"
            (`evalStateT` NoShow 0)
            (monadStateLaws (forAllShow (\(NoShow _) -> "NoShow") (NoShow <$> Gen.integral Range.linearBounded)))
        ],
      testGroup
        "StateT ()"
        [ expectFailBecause "() has only one possible value" $
            testLaws
              "monadStateLaws"
              (`evalStateT` ())
              (monadStateLaws $ forAll $ pure ())
        ],
      testGroup
        "State Int"
        [ testLawsWith
            (withTests 1)
            "monadStateLaws"
            (pure . flip evalState (0 :: Int))
            (monadStateLaws $ pure 1)
        ]
    ]
