{-# LANGUAGE RankNTypes #-}

-- |
-- Module:      Test.Lawful.Hedgehog
-- Description: Hedgehog support for lawful-classes
-- Copyright:   (c) 2023, Nicolas Trangez
-- License:     Apache-2.0
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
--
-- Support code to check @lawful-classes@ laws using Hedgehog and, optionally,
-- Tasty.
module Test.Lawful.Hedgehog
  ( testLaws,
    toProperty,
  )
where

import Hedgehog (Property, PropertyT, assert, discard, evalM, property)
import Test.Lawful.Types (Law, Laws)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Given a way to evaluate an @m a@ into a base 'Monad', turn a 'Law' into a 'Property'.
toProperty :: (forall a. m a -> PropertyT IO a) -> Law m -> Property
toProperty run law = property $ maybe discard assert =<< evalM (run law)

-- | Given 'Laws', create a @tasty@ 'TestTree'.
testLaws :: TestName -> (forall a. m a -> PropertyT IO a) -> Laws m -> TestTree
testLaws name run laws = testGroup name [testProperty n (toProperty run l) | (n, l) <- laws]
