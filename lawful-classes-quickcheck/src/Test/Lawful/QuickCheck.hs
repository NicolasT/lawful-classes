{-# LANGUAGE RankNTypes #-}

-- |
-- Module:      Test.Lawful.QuickCheck
-- Description: QuickCheck support for lawful-classes
-- Copyright:   (c) 2023, Nicolas Trangez
-- License:     Apache-2.0
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
--
-- Support code to check @lawful-classes@ laws using QuickCheck and,
-- optionally, Tasty.
module Test.Lawful.QuickCheck
  ( testLaws,
    testLawsWith,
    toProperty,
  )
where

import Test.Lawful.Types (Law, Laws)
import Test.QuickCheck (Property, discard)
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- | Given a way to evaluate an @m a@ into a base 'Monad', turn a 'Law' into a 'Property'.
toProperty :: (forall a. m a -> PropertyM IO a) -> Law m -> Property
toProperty run law = monadicIO $ maybe discard assert =<< run law

-- | Given 'Laws', create a @tasty@ 'TestTree'.
testLaws :: TestName -> (forall a. m a -> PropertyM IO a) -> Laws m -> TestTree
testLaws = testLawsWith id

-- | Given 'Laws', create a @tasty@ 'TestTree', modifying all created
-- 'Property's with the given function.
--
-- As an example, 'Test.QuickCheck.once' could be used to run every test only
-- once.
--
-- @since 0.1.1.0
testLawsWith :: (Property -> Property) -> TestName -> (forall a. m a -> PropertyM IO a) -> Laws m -> TestTree
testLawsWith fn name run laws = testGroup name [testProperty n (fn $ toProperty run l) | (n, l) <- laws]
