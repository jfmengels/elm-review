port module DefaultPatternPositionTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.DefaultPatternPosition exposing (rule, PatternPosition(First, Last))
import Lint.Types exposing (LintRule, LintError)
import TestUtil exposing (expectErrors)


error : String -> LintError
error position =
    LintError "DefaultPatternPosition" ("Expected default pattern to appear " ++ position ++ " in the list of patterns")


tests : List Test
tests =
    [ test "should not report when default pattern is at the expected position (first)" <|
        \() ->
            """a = case b of
              _ -> 1
              Bar -> 1
              Foo -> 1
            """
                |> rule { position = First }
                |> expectErrors []
    , test "should not report when default pattern is at the expected position (last)" <|
        \() ->
            """a = case b of
              Foo -> 1
              Bar -> 1
              _ -> 1
            """
                |> rule { position = Last }
                |> expectErrors []
    , test "should not report when there is no default pattern (first)" <|
        \() ->
            """a = case b of
              Foo -> 1
              Bar -> 1
            """
                |> rule { position = First }
                |> expectErrors []
    , test "should not report when there is no default pattern (last)" <|
        \() ->
            """a = case b of
              Foo -> 1
              Bar -> 1
            """
                |> rule { position = Last }
                |> expectErrors []
    , test "should report an error when the default pattern is not at the expected position (first) (opposite expected position)" <|
        \() ->
            """a = case b of
              Foo -> 1
              Bar -> 1
              _ -> 1
            """
                |> rule { position = First }
                |> expectErrors [ error "first" ]
    , test "should report an error when the default pattern is not at the expected position (first) (somewhere in the middle)" <|
        \() ->
            """a = case b of
              Foo -> 1
              _ -> 1
              Bar -> 1
            """
                |> rule { position = First }
                |> expectErrors [ error "first" ]
    , test "should report an error when the default pattern is not at the expected position (last) (opposite expected position)" <|
        \() ->
            """a = case b of
              _ -> 1
              Foo -> 1
              Bar -> 1
            """
                |> rule { position = Last }
                |> expectErrors [ error "last" ]
    , test "should report an error when the default pattern is not at the expected position (last) (somewhere in the middle)" <|
        \() ->
            """a = case b of
              Foo -> 1
              _ -> 1
              Bar -> 1
            """
                |> rule { position = Last }
                |> expectErrors [ error "last" ]
    ]


all : Test
all =
    describe "DefaultPatternPosition" tests
