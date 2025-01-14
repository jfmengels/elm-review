module Review.Test.FailureMessageHelper exposing (expectFailure)

import Expect exposing (Expectation)
import Review.Test.ExpectationExtra exposing (onFail)
import Test.Runner


expectFailure : String -> Expectation -> Expectation
expectFailure expectedFailureMessage actualResult =
    expectFailureModifiedBy (String.trim expectedFailureMessage) actualResult


expectFailureModifiedBy : String -> Expectation -> Expectation
expectFailureModifiedBy expectedFailureMessage actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.fail "Expected a failure, but got a pass"

        Just actualInfo ->
            Expect.all
                [ \receivedMessage ->
                    receivedMessage
                        |> Expect.equal (removeColors (String.trim expectedFailureMessage))
                , \receivedMessage ->
                    Expect.all
                        (String.lines receivedMessage
                            |> List.map
                                (\line () ->
                                    String.length line
                                        |> Expect.atMost 76
                                        |> onFail (\() -> "Message has line longer than 76 characters:\n\n" ++ line)
                                )
                        )
                        ()
                ]
                (removeColors actualInfo.description)


removeColors : String -> String
removeColors str =
    str
        |> String.replace "\u{001B}[0m" ""
        |> String.replace "\u{001B}[1m" ""
        |> String.replace "\u{001B}[2m" ""
        |> String.replace "\u{001B}[22m" ""
        |> String.replace "\u{001B}[31m" ""
        |> String.replace "\u{001B}[32m" ""
        |> String.replace "\u{001B}[37m" ""
        |> String.replace "\u{001B}[39m" ""
