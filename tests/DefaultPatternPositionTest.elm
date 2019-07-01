module DefaultPatternPositionTest exposing (all)

import Lint.Rule.DefaultPatternPosition exposing (PatternPosition(..), rule)
import Lint.Test exposing (LintResult)
import Test exposing (Test, describe, test)


testRule : PatternPosition -> String -> LintResult
testRule patternPosition =
    Lint.Test.run (rule patternPosition)


message : String -> String
message position =
    "Expected default pattern to appear " ++ position ++ " in the list of patterns"


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
                |> testRule ShouldBeFirst
                |> Lint.Test.expectNoErrors
    , test "should not report when default pattern is at the expected position (last)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  Bar -> 1
  _ -> 1
"""
                |> testRule ShouldBeLast
                |> Lint.Test.expectNoErrors
    , test "should not report when there is no default pattern (first)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  Bar -> 1
"""
                |> testRule ShouldBeFirst
                |> Lint.Test.expectNoErrors
    , test "should not report when there is no default pattern (last)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  Bar -> 1
"""
                |> testRule ShouldBeLast
                |> Lint.Test.expectNoErrors
    , test "should report an error when the default pattern is not at the expected position (first) (opposite expected position)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  Bar -> 1
  _ -> 1
"""
                |> testRule ShouldBeFirst
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message "first"
                        , under = "_"
                        }
                    ]
    , test "should report an error when the default pattern is not at the expected position (first) (somewhere in the middle)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  _ -> 1
  Bar -> 1
"""
                |> testRule ShouldBeFirst
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message "first"
                        , under = "_"
                        }
                    ]
    , test "should report an error when the default pattern is not at the expected position (last) (opposite expected position)" <|
        \() ->
            """module A exposing(..)
a = case b of
  _ -> 1
  Foo -> 1
  Bar -> 1
"""
                |> testRule ShouldBeLast
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message "last"
                        , under = "_"
                        }
                    ]
    , test "should report an error when the default pattern is not at the expected position (last) (somewhere in the middle)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  _ -> 1
  Bar -> 1
"""
                |> testRule ShouldBeLast
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message "last"
                        , under = "_"
                        }
                    ]
    ]


all : Test
all =
    describe "DefaultPatternPosition" tests
