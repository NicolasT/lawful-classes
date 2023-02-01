{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module:      Test.Lawful.Demo
-- Description: Demo library for @lawful-classes@
-- Copyright:   (c) 2023, Nicolas Trangez
-- License:     Apache-2.0
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
--
-- A demo library for @lawful-classes@ exposing a class with some laws and
-- an instance of said class. The corresponding test-suite validates the
-- instance obeys to the laws, both using Hedgehog and QuickCheck.
--
-- Note, in a real-world project, one would use either Hedgehog or QuickCheck
-- to check the laws, and the class and instance(s) are likely found in
-- different packages.
module Test.Lawful.Demo
  ( -- * Demo class
    MonadDemo,

    -- ** Laws
    monadDemoLaws,
    monadDemoLaws',

    -- * Demo instances
    DemoT,
    evalDemoT,
    Demo,
    evalDemo,
    UnlawfulDemoT,
    evalUnlawfulDemoT,
    UnlawfulDemo,
    evalUnlawfulDemo,
  )
where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State.Class (get, put)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.State (StateT, evalStateT)
import Test.Lawful.Types (Laws, assert, discard)

-- | A demo class of effects.
--
-- Laws:
-- - 'retrieve' after 'store' yields the just-stored value.
class (Monad m) => MonadDemo a m where
  store :: a -> m ()
  retrieve :: m a

-- | Laws, using the type aliases and utilities provided in "Test.Lawful".
monadDemoLaws ::
  (MonadDemo a m, Eq a) =>
  -- | Action which yields values of type @a@ upon request.
  m a ->
  Laws m
monadDemoLaws gen =
  [ ( "retrieve yields what's stored",
      do
        a0 <- retrieve
        a <- gen

        if a == a0
          then discard
          else do
            store a
            a' <- retrieve
            assert $ a' == a
    )
  ]

-- | Laws, not using the type aliases and utilities provided in "Test.Lawful".
--
-- Since these are simple, one may not want a dependency on @lawful-classes-types@,
-- in which case this works just as well.
monadDemoLaws' ::
  (MonadDemo a m, Eq a) =>
  -- | Action which yields values of type @a@ upon request.
  m a ->
  [(String, m (Maybe Bool))]
monadDemoLaws' gen =
  [ ( "retrieve yields what's stored",
      do
        a0 <- retrieve
        a <- gen

        if a == a0
          then pure Nothing
          else do
            store a
            a' <- retrieve
            pure $ Just $ a' == a
    )
  ]

-- | A demonstration 'Monad', instance of 'MonadDemo'.
newtype DemoT m a = DemoT (StateT Int m a)
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadTrans)

instance (Monad m) => MonadDemo Int (DemoT m) where
  store = DemoT . put
  retrieve = DemoT get

-- | Evaluate a 'DemoT' into its base 'Monad'.
evalDemoT :: (Monad m) => DemoT m a -> m a
evalDemoT (DemoT act) = evalStateT act 0

-- | A non-transformer version of 'DemoT'.
type Demo = DemoT Identity

-- | Evaluate a 'Demo'.
evalDemo :: Demo a -> a
evalDemo = runIdentity . evalDemoT

-- | A demonstration unlawful 'Monad', instance of 'MonadDemo'.
--
-- Its instance of 'MonadDemo' is such that any value greater than 10 is not
-- actually stored, hence breaking the @retrieve yields what's stored@ law.
newtype UnlawfulDemoT m a = UnlawfulDemoT (StateT Int m a)
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadTrans)

instance (Monad m) => MonadDemo Int (UnlawfulDemoT m) where
  store v
    | v <= 10 = UnlawfulDemoT (put v)
    | otherwise = return ()
  retrieve = UnlawfulDemoT get

-- | Evaluate an 'UnlawfulDemoT' into its base 'Monad'.
evalUnlawfulDemoT :: (Monad m) => UnlawfulDemoT m a -> m a
evalUnlawfulDemoT (UnlawfulDemoT act) = evalStateT act 0

-- | A non-transformer version of 'UnlawfulDemoT'.
type UnlawfulDemo = UnlawfulDemoT Identity

-- | Evaluate an 'UnlawfulDemo'.
evalUnlawfulDemo :: UnlawfulDemo a -> a
evalUnlawfulDemo = runIdentity . evalUnlawfulDemoT
