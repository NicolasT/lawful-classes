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
    testLawsWith,
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
testLaws = testLawsWith id

-- | Given 'Laws', create a @tasty@ 'TestTree', modifying all created
-- 'Property's with the given function.
--
-- As an example, 'Hedgehog.withTests' could be used to reduce or increase
-- the number of times tests are executed.
--
-- @since 0.1.1.0
testLawsWith :: (Property -> Property) -> TestName -> (forall a. m a -> PropertyT IO a) -> Laws m -> TestTree
testLawsWith fn name run laws = testGroup name [testProperty n (fn $ toProperty run l) | (n, l) <- laws]
