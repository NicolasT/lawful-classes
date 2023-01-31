-- |
-- Module:      Test.Lawful.Types
-- Description: Types for lawful-classes
-- Copyright:   (c) 2023, Nicolas Trangez
-- License:     Apache-2.0
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
--
-- Type definitions (aliases) and basic utility functions used in the
-- @lawful-classes@ ecosystem.
module Test.Lawful.Types
  ( -- * Core types
    Laws,
    Law,

    -- * Utilities
    discard,
    assert,
  )
where

-- | Laws that hold for a specific 'Monad' @m@.
--
-- They come with a name, and an action which may yield a 'Bool', denoting
-- whether or not the law holds. It yield 'Nothing' if the test should be
-- discarded for the given input (when a generator is used).
type Laws m = [(String, Law m)]

-- | A single law.
--
-- See 'Laws'.
type Law m = m (Maybe Bool)

-- | Discard the current test-case.
discard :: (Applicative m) => Law m
discard = pure Nothing

-- | Assert a given 'Bool' value is 'True'.
assert :: (Applicative m) => Bool -> Law m
assert = pure . Just
