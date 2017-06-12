port module NoUselessPatternMatchingTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.NoUselessPatternMatching exposing (rule)
import Lint.Types exposing (LintRule, Error)
import TestUtil exposing (expectErrors)


uselessError : Error
uselessError =
    Error "NoUselessPatternMatching" "Useless case expression: It will always evaluate to the same value"


uselessPatternError : Error
uselessPatternError =
    Error "NoUselessPatternMatching" "Useless patterns: Some will always evaluate to the same value as the default pattern"


tests : List Test
tests =
    [ test "should not report cases with different results" <|
        \() ->
            rule """a = case b of
              Just c -> a
              Nothing -> 1
            """
                |> expectErrors []
    , test "should report case expression where all pattern end up with the same result" <|
        \() ->
            rule """a = case b of
              Just c -> 1
              Nothing -> 1
            """
                |> expectErrors [ uselessError ]
    , test "should report case expression where all pattern have the same expression" <|
        \() ->
            rule """a = case b of
              Just c -> b
              Nothing -> b
            """
                |> expectErrors [ uselessError ]
    , test "should not report case expression where all pattern end up with the same body, but introduce and use variables with pattern matching" <|
        \() ->
            rule """a = case b of
              Just b -> b
              Nothing -> b
            """
                |> expectErrors []
    , test "should report case expression where all pattern end up with the same body, and introduce but don't use variables with pattern matching" <|
        \() ->
            rule """a = case b of
              Just g c D e -> b
              Nothing -> b
            """
                |> expectErrors [ uselessError ]
    , test "should report patterns that have the same value as the default pattern and do not introduce variables" <|
        \() ->
            rule """a = case b of
              Foo -> c
              Bar -> b
              a -> b
            """
                |> expectErrors [ uselessPatternError ]
    , test "should report patterns that have the same value as the default pattern and do not introduce variables (with _)" <|
        \() ->
            rule """a = case foo of
              Nothing -> 2
              Just -> 1
              _ -> 1
            """
                |> expectErrors [ uselessPatternError ]
    , test "should not report patterns that have the same value as the default pattern but introduce and use variables" <|
        \() ->
            rule """a = case b of
              Foo -> c
              Bar b -> b
              a -> b
            """
                |> expectErrors []
    , test "should report patterns that have the same value as the default pattern, even if they introduce and don't use variables" <|
        \() ->
            rule """a = case b of
              Foo -> c
              Bar d -> b
              a -> b
            """
                |> expectErrors [ uselessPatternError ]
    , test "should not report patterns where there is no default pattern" <|
        \() ->
            rule """a = case b of
              Foo -> c
              Bar -> b
              Baz -> b
            """
                |> expectErrors []
    , test "should not report case where there is only one pattern (which introduces and uses variables)" <|
        \() ->
            rule """a = case b of
              Foo d -> d
            """
                |> expectErrors []
    , test "should report case where there is only one pattern (which doesn't have variables)" <|
        \() ->
            rule """a = case b of
              Foo -> d
            """
                |> expectErrors [ uselessError ]
    , test "should report case where there is only the default pattern" <|
        \() ->
            rule """a = case b of
              _ -> d
            """
                |> expectErrors [ uselessError ]
    ]


all : Test
all =
    describe "NoUselessPatternMatching" tests
