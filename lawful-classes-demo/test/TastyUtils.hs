module TastyUtils (mayFail) where

import Test.Tasty (TestTree)
import Test.Tasty.ExpectedFailure (wrapTest)
import Test.Tasty.Runners (FailureReason (..), Outcome (..), Result (..))

-- | Wrap tests so they're allowed to fail, i.e., either success or
-- failure is success.
mayFail :: TestTree -> TestTree
mayFail = wrapTest $ fmap $ \r -> case resultOutcome r of
  Success -> r
  Failure f -> case f of
    TestFailed ->
      r
        { resultOutcome = Success,
          resultShortDescription = resultShortDescription r <> " (expected)"
        }
    TestThrewException {} -> r
    TestTimedOut {} -> r
    TestDepFailed -> r
