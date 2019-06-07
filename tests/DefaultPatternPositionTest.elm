module DefaultPatternPositionTest exposing (all)

import Elm.Syntax.Range exposing (Location, Range)
import Lint exposing (Rule)
import Lint.Error exposing (Error)
import Lint.Rule exposing (LintResult)
import Lint.Rule.DefaultPatternPosition exposing (Configuration, PatternPosition(..), rule)
import Test exposing (Test, describe, test)
import TestUtil exposing (expectErrorsWithoutRange, location, ruleTester)


testRule : Configuration -> String -> LintResult
testRule options =
    ruleTester (rule options)


error : String -> Error
error position =
    Error
        "DefaultPatternPosition"
        ("Expected default pattern to appear " ++ position ++ " in the list of patterns")
        (location ( 0, 0 ) ( 0, 0 ))


tests : List Test
tests =
    [ test "should not report when default pattern is at the expected position (first)" <|
        \() ->
            """module A exposing(..)
a = case b of
  _ -> 1
  Bar -> 1
  Foo -> 1
"""
                |> testRule { position = First }
                |> expectErrorsWithoutRange []
    , test "should not report when default pattern is at the expected position (last)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  Bar -> 1
  _ -> 1
"""
                |> testRule { position = Last }
                |> expectErrorsWithoutRange []
    , test "should not report when there is no default pattern (first)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  Bar -> 1
"""
                |> testRule { position = First }
                |> expectErrorsWithoutRange []
    , test "should not report when there is no default pattern (last)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  Bar -> 1
"""
                |> testRule { position = Last }
                |> expectErrorsWithoutRange []
    , test "should report an error when the default pattern is not at the expected position (first) (opposite expected position)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  Bar -> 1
  _ -> 1
"""
                |> testRule { position = First }
                |> expectErrorsWithoutRange [ error "first" ]
    , test "should report an error when the default pattern is not at the expected position (first) (somewhere in the middle)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  _ -> 1
  Bar -> 1
"""
                |> testRule { position = First }
                |> expectErrorsWithoutRange [ error "first" ]
    , test "should report an error when the default pattern is not at the expected position (last) (opposite expected position)" <|
        \() ->
            """module A exposing(..)
a = case b of
  _ -> 1
  Foo -> 1
  Bar -> 1
"""
                |> testRule { position = Last }
                |> expectErrorsWithoutRange [ error "last" ]
    , test "should report an error when the default pattern is not at the expected position (last) (somewhere in the middle)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  _ -> 1
  Bar -> 1
"""
                |> testRule { position = Last }
                |> expectErrorsWithoutRange [ error "last" ]
    ]


all : Test
all =
    describe "DefaultPatternPosition" tests
