# lawful-classes

The `lawful-classes` packages provide a simple framework to define laws for
classes (e.g., some `MonadState` class and laws related to the behaviour of
actions defined in `MonadState`) without incurring a dependency on libraries
like [Tasty][tasty] or [Hspec][hspec], or [QuickCheck][quickcheck] or
[Hedgehog][hedgehog], within the library where said class and its related
laws are defined.

Law definitions can make use of generators of values, as provided by
QuickCheck or Hedgehog, but can request a test iteration to be discarded if
some generated value doesn't meet some criteria (alike QuickCheck's `==>`
implication arrow), without a dependency on QuickCheck or other libraries:
in essence, every test-case returns a value of type `Maybe Bool` (in some
monadic context), where `Nothing` implies a test-case should be discarded,
and `Just b` represents an (un)successful test-case/assertion.

Hence, `Law`s are computations of type `m (Maybe Bool)`, and are grouped in
`Laws`, a list of `(String, Law)` pairs where the first member provides some
human-readable description of the law being checked.

These type aliases, and some trivial helper functions, are provided in the
`lawful-classes-types` package. This package only depends on `base`, but it's
not strictly required as a dependency to define laws: everything can be
achieved by only using `base`.

[tasty]: https://hackage.haskell.org/package/tasty
[hspec]: https://hackage.haskell.org/package/hspec
[quickcheck]: https://hackage.haskell.org/package/QuickCheck
[hedgehog]: https://hackage.haskell.org/package/hedgehog

## Packages
[lawful-classes-types][lawful-classes-types] provides common type aliases and
some trivial helper functions. A dependency on this library is not required
to define laws.

[lawful-classes-hedgehog][lawful-classes-hedgehog] provides code to integrate
law-checking in a Tasty environment, when using Hedgehog to generate exemplars.
It also provides some plumbing functions for integration in frameworks other
than Tasty.

[lawful-classes-quickcheck][lawful-classes-quickcheck] provides code to
integrate law-checking in a Tasty environment, when using QuickCheck to
generate exemplars. It also provides some plumbing functions for integration 
in frameworks other than Tasty.

[lawful-classes-types]: https://hackage.haskell.org/package/lawful-classes-types
[lawful-classes-hedgehog]: https://hackage.haskell.org/package/lawful-classes-hedgehog
[lawful-classes-quickcheck]: https://hackage.haskell.org/package/lawful-classes-quickcheck

## Example

Here's an example, using Literate Haskell.

### Boilerplate
First, some boilerplate, only required for the code below but in no way
required to use the framework:

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
```

In this example, we'll define some implementations of a class, using `State`
and `StateT` under the hood:

```haskell
import Control.Monad.State (State, evalState)
import Control.Monad.State.Class (get, put)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.State (StateT, evalStateT)
```

Checking the laws can be done using any testing framework. Using
`lawful-classes-hedgehog` or `lawful-classes-quickcheck`, integration with
either Hedgehog or QuickCheck becomes easier, especially when using Tasty as
a test runner. Generally, you'll use either Hedgehog or QuickCheck. In this
example, we'll showcase both:

```haskell
import Hedgehog (withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.QuickCheck (arbitrary, once)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
```

Finally, import the code from `lawful-classes-types`, `lawful-classes-hedgehog`
and `lawful-classes-quickcheck`:

```haskell
import Test.Lawful.Types (Laws, assert, discard)
import qualified Test.Lawful.Hedgehog as LH
import qualified Test.Lawful.QuickCheck as LQ
```

### A class and its laws
In our library, we define a class, `MonadStore`, which exposes two actions,
`store` and `retrieve`:

```haskell
-- | An environment which allows to store some value, and retrieve it later.
class Monad m => MonadStore a m | m -> a where
  -- | Store the given value.
  store :: a -> m ()
  -- | Retrieve a previously stored value, if any.
  retrieve :: m (Maybe a)
```

Given this, we can define some `Laws` to which any valid instance of
`MonadStore` should obey:

```haskell
-- Remember, `Laws m` is just `[(String, m (Maybe Bool)]`
monadStoreLaws :: (MonadStore a m, Eq a) => m a -> Laws m
monadStoreLaws gen = [
  ("When nothing is stored, nothing can be retrieved", do
      a0 <- retrieve
      assert $ a0 == Nothing  -- assert = pure . Just
  ),

  ("Retrieve yields what was stored", do
      -- Retrieve the currently stored value
      a0 <- retrieve
      -- Generate some arbitrary value
      a <- gen

      if Just a == a0
        -- Debatable: if the generated value equals the currently stored one, this test is void
        then discard  -- discard = pure Nothing
        else do
          store a
          a' <- retrieve
          assert $ a' == Just a
  )
  ]
```

### Instances
We define two instances of `MonadStore`, one which is correct and obeys the
laws, one which (intentionally) doesn't. First, `LockerT`:

```haskell
newtype LockerT a m b = LockerT (StateT (Maybe a) m b)
  deriving (Functor, Applicative, Monad, MonadTrans)

instance Monad m => MonadStore a (LockerT a m) where
  store = LockerT . put . Just
  retrieve = LockerT get

evalLockerT :: Monad m => LockerT a m b -> m b
evalLockerT (LockerT act) = evalStateT act Nothing
```

Given the above, we can check whether the `monadStoreLaws` hold for this
implementation as follows, first using Hedgehog:

```haskell
lockerTTestsHedgehog :: TestTree
lockerTTestsHedgehog = 
  LH.testLaws "monadStoreLaws (Hedgehog)" evalLockerT $
    monadStoreLaws (LH.forAll (Gen.int Range.linearBounded))
```

Then, using QuickCheck:

```haskell
lockerTTestsQuickCheck :: TestTree
lockerTTestsQuickCheck =
  LQ.testLaws "monadStoreLaws (QuickCheck)" evalLockerT $
    monadStoreLaws (LQ.forAll (arbitrary @Int))
```

A second instance of `MonadStore` doesn't play by the rules:

```haskell
newtype UnlawfulLockerT a m b = UnlawfulLockerT (StateT (Maybe a) m b)
  deriving (Functor, Applicative, Monad, MonadTrans)

instance (Monad m, Eq a, Num a) => MonadStore a (UnlawfulLockerT a m) where
  store v = do
    let v' = if v == 0 then v else v + 1
    UnlawfulLockerT (put $ Just v')
  retrieve = UnlawfulLockerT get

evalUnlawfulLockerT :: (Monad m, Num a) => UnlawfulLockerT a m b -> m b
evalUnlawfulLockerT (UnlawfulLockerT act) = evalStateT act (Just 1)
```

Similarly, test its law-(non-)abiding using Hedgehog and QuickCheck as exemplar
generators (note, in this case we wrap them with `expectFail`, since this
document is tested in CI, and we actually want the following tests to fail, but
not CI to fail):

```haskell
unlawfulLockerTTests :: TestTree
unlawfulLockerTTests = expectFail $ testGroup "UnlawfulLockerT" [
  LH.testLaws "monadStoreLaws (Hedgehog)" evalUnlawfulLockerT $
    monadStoreLaws (LH.forAll (Gen.int Range.linearBounded)),
  LQ.testLaws "monadStoreLaws (QuickCheck)" evalUnlawfulLockerT $
    monadStoreLaws (LQ.forAll (arbitrary @Int))
  ]
```

#### Plain monads
In the above, we use a monad transformer, since the laws are defined such that
a generator of values can be used. This generator is, then, lifted into a
monad lower in the stack, e.g., `PropertyT` when using Hedgehog or `PropertyM`
when using QuickCheck.

We can, of course, check the laws against a plain monad as well, but in this
case no values can be generated: there's no lower monad in the stack. We can,
however, provide a generator which yields exactly one (constant) value. In this
case, it doesn't make sense to run the test, say, 100 times. Hence, the
`testLaws` functions have a counterpart which allow to modify the tested
properties: `testLawsWith`.

Let's first define `Locker`, which wraps `State` (it could, of course, reuse
the definition of `LockerT` and use `Identity` as the base monad):

```haskell
newtype Locker a b = Locker (State (Maybe a) b)
  deriving (Functor, Applicative, Monad)

instance MonadStore a (Locker a) where
  store = Locker . put . Just
  retrieve = Locker get

evalLocker :: Locker a b -> b
evalLocker (Locker act) = evalState act Nothing
```

Now, define the tests, and ensure they run only once:

```haskell
lockerTests :: TestTree
lockerTests = testGroup "Locker" [
  LH.testLawsWith (withTests 1) "monadStoreLaws (Hedgehog)"
    (pure . evalLocker)
    (monadStoreLaws $ pure (1 :: Int)),
  LQ.testLawsWith once "monadStoreLaws (Hedgehog)"
    (pure . evalLocker)
    (monadStoreLaws $ pure (1 :: Int))
  ]
```

Hence, we'll run the tests with only a single generated value, being `1`.

### Running tests
Finally, we can hook everything together and run the tests, using Tasty:

```haskell
main :: IO ()
main = defaultMain $ testGroup "lawful-classes-readme" [
    testGroup "LockerT" [
      lockerTTestsHedgehog,
      lockerTTestsQuickCheck
      ],
    unlawfulLockerTTests,
    lockerTests
  ]
```

And indeed:

```
lawful-classes-readme
  LockerT
    monadStoreLaws (Hedgehog)
      When nothing is stored, nothing can be retrieved: OK
          ✓ <interactive> passed 100 tests.
      Retrieve yields what was stored:                  OK
          ✓ <interactive> passed 100 tests.
    monadStoreLaws (QuickCheck)
      When nothing is stored, nothing can be retrieved: OK
        +++ OK, passed 100 tests.
      Retrieve yields what was stored:                  OK
        +++ OK, passed 100 tests.
  UnlawfulLockerT
    monadStoreLaws (Hedgehog)
      When nothing is stored, nothing can be retrieved: FAIL (expected)
          ✗ <interactive> failed at src/Test/Lawful/Hedgehog.hs:37:47
            after 1 test.
            shrink path: 1:

            This failure can be reproduced by running:
            > recheckAt (Seed 15866264305184189776 9458969182257889837) "1:" <property>

        Use ...
         (expected failure)
      Retrieve yields what was stored:                  FAIL (expected)
          ✗ <interactive> failed at src/Test/Lawful/Hedgehog.hs:37:47
            after 2 tests and 56 shrinks.
            shrink path: 2:bA55

                ┏━━ README.lhs ━━━
            172 ┃ unlawfulLockerTTests :: TestTree
            173 ┃ unlawfulLockerTTests = expectFail $ testGroup "UnlawfulLockerT" [
            174 ┃   LH.testLaws "monadStoreLaws (Hedgehog)" evalUnlawfulLockerT $
            175 ┃     monadStoreLaws (LH.forAll (Gen.int Range.linearBounded)),
                ┃     │ 2
            176 ┃   LQ.testLaws "monadStoreLaws (QuickCheck)" evalUnlawfulLockerT $
            177 ┃     monadStoreLaws (LQ.forAll (arbitrary @Int))
            178 ┃   ]
            179 ┃ ```
            180 ┃
            181 ┃ ### Running tests
            182 ┃ Finally, we can hook everything together and run the tests, using Tasty:
            183 ┃
            184 ┃ ```haskell

            This failure can be reproduced by running:
            > recheckAt (Seed 9014603521135969515 646347982343338801) "2:bA55" <property>

        Use ...
         (expected failure)
    monadStoreLaws (QuickCheck)
      When nothing is stored, nothing can be retrieved: FAIL (expected)
        *** Failed! Assertion failed (after 1 test):
        Use --quickcheck-replay=181324 to reproduce. (expected failure)
      Retrieve yields what was stored:                  FAIL (expected)
        *** Failed! Assertion failed (after 4 tests):
        3
        Use --quickcheck-replay=404272 to reproduce. (expected failure)
  Locker
    monadStoreLaws (Hedgehog)
      When nothing is stored, nothing can be retrieved: OK
          ✓ <interactive> passed 1 test.
      Retrieve yields what was stored:                  OK
          ✓ <interactive> passed 1 test.
    monadStoreLaws (Hedgehog)
      When nothing is stored, nothing can be retrieved: OK
        +++ OK, passed 1 test.
      Retrieve yields what was stored:                  OK
        +++ OK, passed 1 test.

All 12 tests passed (0.01s)
```
