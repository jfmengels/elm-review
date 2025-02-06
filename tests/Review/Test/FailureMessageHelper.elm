module Review.Test.FailureMessageHelper exposing (expectFailure, expectFailureNoLengthCheck)

import Expect exposing (Expectation)
import Review.Test.ExpectationExtra exposing (onFail)
import Test.Runner


expectFailure : String -> Expectation -> Expectation
expectFailure expectedFailureMessage actualResult =
    expectFailureModifiedBy True (String.trim expectedFailureMessage) actualResult


expectFailureNoLengthCheck : String -> Expectation -> Expectation
expectFailureNoLengthCheck expectedFailureMessage actualResult =
    expectFailureModifiedBy False (String.trim expectedFailureMessage) actualResult


expectFailureModifiedBy : Bool -> String -> Expectation -> Expectation
expectFailureModifiedBy checkLength expectedFailureMessage actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.fail "Expected a failure, but got a pass"

        Just actualInfo ->
            Expect.all
                [ \receivedMessage ->
                    receivedMessage
                        |> Expect.equal (removeColors (String.trim expectedFailureMessage))
                , \receivedMessage ->
                    if checkLength then
                        Expect.all
                            (String.lines receivedMessage
                                |> List.map
                                    (\line () ->
                                        if String.startsWith "  " line then
                                            Expect.pass

                                        else
                                            String.length line
                                                |> Expect.atMost 76
                                                |> onFail (\() -> "Message has line longer than 76 characters:\n\n" ++ line)
                                    )
                            )
                            ()

                    else
                        Expect.pass
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
