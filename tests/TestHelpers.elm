module TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults, whenNotExpectingNaN)

import Expect exposing (Expectation)
import Review.Rule exposing (Rule)
import Review.Test
import Simplify
import Test.Runner


ruleWithDefaults : Rule
ruleWithDefaults =
    Simplify.rule Simplify.defaults


ruleExpectingNaN : Rule
ruleExpectingNaN =
    Simplify.rule (Simplify.expectNaN Simplify.defaults)


{-| Runs the rule twice with both `ruleWithDefaults` and `ruleExpectingNaN`.
It will `expectErrors` the given errors when run with `ruleWithDefaults`
but `expectNoErrors` when run with `ruleExpectingNaN`.

    test "should replace Dict.fromList (List.repeat n a) by if n >= 1 then Dict.fromList [ a ] else Dict.empty" <|
        """module A exposing (..)
    a = Dict.fromList (List.repeat n b)
    """
            |> whenNotExpectingNaN Review.Test.run
                [ Review.Test.error
                    { message = "..."
                    , details = [ "..." ]
                    , under = "Dict.fromList"
                    }
                ]

Can also be used with `Review.Test.runWithProjectData` by passing that to `whenNotExpectingNaN`.

    input
        |> whenNotExpectingNaN (Review.Test.runWithProjectData project)
            [{- expected errors -}]

-}
whenNotExpectingNaN :
    (Rule -> input -> Review.Test.ReviewResult)
    -> List Review.Test.ExpectedError
    -> input
    -> Expect.Expectation
whenNotExpectingNaN runner expectedErrors input =
    Expect.all
        [ \() ->
            input
                |> runner ruleWithDefaults
                |> Review.Test.expectErrors expectedErrors
                |> onFail "expectNaN is DISABLED"
        , \() ->
            input
                |> runner ruleExpectingNaN
                |> Review.Test.expectNoErrors
                |> onFail "expectNaN is ENABLED"
        ]
        ()


onFail : String -> Expectation -> Expectation
onFail when expectation =
    case Test.Runner.getFailureReason expectation of
        Just reason ->
            Expect.fail (reason.description ++ "\n\n\u{001B}[33m**When " ++ when ++ "**\u{001B}[39m")

        Nothing ->
            expectation
