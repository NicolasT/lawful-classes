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
  ( -- * Tasty integration
    testLaws,
    testLawsWith,

    -- * Utilities
    forAll,
    forAllShow,

    -- * Plumbing
    toProperty,
  )
where

import Control.Monad.Trans.Class (MonadTrans, lift)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog (Gen, Property, PropertyT, assert, discard, evalM, property)
import qualified Hedgehog as H
import Test.Lawful.Types (Law, Laws)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Given a way to evaluate an @m a@ into a base 'Monad', turn a 'Law' into a 'Property'.
toProperty :: (forall a. m a -> PropertyT IO a) -> Law m -> Property
toProperty run law = property $ maybe discard assert =<< evalM (run law)

-- | Given 'Laws' for @m@ and a way to evaluate an @m a@ in @'PropertyT' IO@,
-- create a @tasty@ 'TestTree'.
testLaws :: TestName -> (forall a. m a -> PropertyT IO a) -> Laws m -> TestTree
testLaws = testLawsWith id

-- | Given 'Laws' for @m@ and a way to evaluate an @m a@ in @'PropertyT' IO@,
-- create a @tasty@ 'TestTree', modifying all created 'Property's with the
-- given function.
--
-- As an example, 'Hedgehog.withTests' could be used to reduce or increase
-- the number of times tests are executed, e.g., because 'm' is not a
-- transformer so there's no way to generate multiple test exemplars using
-- some generator, except for the trivial constant generator.
--
-- @since 0.1.1.0
testLawsWith :: (Property -> Property) -> TestName -> (forall a. m a -> PropertyT IO a) -> Laws m -> TestTree
testLawsWith fn name run laws = testGroup name [testProperty n (fn $ toProperty run l) | (n, l) <- laws]

-- | Lifted version of 'H.forAll'.
--
-- This can be used to easily create generators for laws which need them.
--
-- @since 0.1.2.0
forAll :: (MonadTrans t, Monad m, Show a, HasCallStack) => Gen a -> t (PropertyT m) a
forAll gen = withFrozenCallStack $ lift (H.forAll gen)

-- | Lifted version of 'H.forAllWith'.
--
-- Like 'forAll', but for types without a 'Show' instance (or, for which
-- another stringification functions but 'show' should be used).
--
-- This can be used to earily create generators for laws which need them.
--
-- @since 0.1.2.0
forAllShow :: (MonadTrans t, Monad m, HasCallStack) => (a -> String) -> Gen a -> t (PropertyT m) a
forAllShow shw gen = withFrozenCallStack $ lift (H.forAllWith shw gen)
