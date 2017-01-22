port module NoWarningCommentsTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import NoWarningComments exposing (rule)
import Types exposing (Error)


error : String -> Error
error word =
    Error "NoWarningComments" ("Unexpected " ++ word ++ " comment")


wordsToLookFor : List String
wordsToLookFor =
    [ "TODO", "todo", "FIXME", "fixme", "XXX", "xxx" ]


failingTestCases : List Test
failingTestCases =
    List.concatMap
        (\word ->
            [ test ("should report single-line comments containing only " ++ word) <|
                \() ->
                    ("-- " ++ word)
                        |> rule
                        |> Expect.equal [ error word ]
            , test ("should report single-line comments containing " ++ word) <|
                \() ->
                    ("-- " ++ word ++ ": Do this")
                        |> rule
                        |> Expect.equal [ error word ]
            , test ("should report single-line comments containing " ++ word ++ " (no space between comment and word)") <|
                \() ->
                    ("--" ++ word ++ ": Do this")
                        |> rule
                        |> Expect.equal [ error word ]
            , test ("should report multi-line comments containing " ++ word) <|
                \() ->
                    ("{-" ++ word ++ " \n -}")
                        |> rule
                        |> Expect.equal [ error word ]
            ]
        )
        wordsToLookFor


tests : List Test
tests =
    [ test "should not report regular single-line comments" <|
        \() ->
            rule "-- foo bar"
                |> Expect.equal []
    , test "should not report regular multi-line comments" <|
        \() ->
            rule "{- foo bar \n -}"
                |> Expect.equal []
    ]
        ++ failingTestCases


all : Test
all =
    describe "NoWarningComments" tests
