module Review.Test.ExpectationExtra exposing (onFail)

import Expect exposing (Expectation)
import Test.Runner


onFail : (() -> String) -> Expectation -> Expectation
onFail message expectation =
    case Test.Runner.getFailureReason expectation of
        Just _ ->
            expectation |> Expect.onFail (message ())

        Nothing ->
            expectation
