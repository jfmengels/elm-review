port module NoWarningCommentsTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.NoWarningComments exposing (rule)
import Lint.Types exposing (LintRule, LintError)
import TestUtil exposing (expectErrors)


error : String -> LintError
error word =
    LintError "NoWarningComments" ("Unexpected " ++ word ++ " comment")


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
                        |> expectErrors [ error word ]
            , test ("should report single-line comments containing " ++ word) <|
                \() ->
                    ("-- " ++ word ++ ": Do this")
                        |> rule
                        |> expectErrors [ error word ]
            , test ("should report single-line comments containing " ++ word ++ " (no space between comment and word)") <|
                \() ->
                    ("--" ++ word ++ ": Do this")
                        |> rule
                        |> expectErrors [ error word ]
            , test ("should report multi-line comments containing " ++ word) <|
                \() ->
                    ("{-" ++ word ++ " \n -}")
                        |> rule
                        |> expectErrors [ error word ]
            ]
        )
        wordsToLookFor


tests : List Test
tests =
    [ test "should not report regular single-line comments" <|
        \() ->
            rule "-- foo bar"
                |> expectErrors []
    , test "should not report regular multi-line comments" <|
        \() ->
            rule "{- foo bar \n -}"
                |> expectErrors []
    ]
        ++ failingTestCases


all : Test
all =
    describe "NoWarningComments" tests
