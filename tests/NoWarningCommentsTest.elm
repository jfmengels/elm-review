module NoWarningCommentsTest exposing (all)

import Lint.Rules.NoWarningComments exposing (rule)
import Lint.Types exposing (LintError, LintResult, LintRule)
import Test exposing (Test, describe, test)
import TestUtil exposing (expectErrors, ruleTester)


testRule : String -> LintResult
testRule =
    ruleTester rule


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
                    testRule ("-- " ++ word)
                        |> expectErrors [ error word ]
            , test ("should report single-line comments containing " ++ word) <|
                \() ->
                    testRule ("-- " ++ word ++ ": Do this")
                        |> expectErrors [ error word ]
            , test ("should report single-line comments containing " ++ word ++ " (no space between comment and word)") <|
                \() ->
                    testRule ("--" ++ word ++ ": Do this")
                        |> expectErrors [ error word ]
            , test ("should report multi-line comments containing " ++ word) <|
                \() ->
                    testRule ("{-" ++ word ++ " \n -}")
                        |> expectErrors [ error word ]
            ]
        )
        wordsToLookFor


tests : List Test
tests =
    [ test "should not report regular single-line comments" <|
        \() ->
            testRule "-- foo bar"
                |> expectErrors []
    , test "should not report regular multi-line comments" <|
        \() ->
            testRule "{- foo bar \n -}"
                |> expectErrors []
    ]
        ++ failingTestCases


all : Test
all =
    describe "NoWarningComments" tests
