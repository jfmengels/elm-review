module NoUnusedTypeConstructorsTest exposing (all)

import Lint.Rule.NoUnusedTypeConstructors exposing (rule)
import Lint.Test exposing (LintResult)
import Test exposing (Test, describe, test)


testRule : String -> LintResult
testRule =
    Lint.Test.run rule


tests : List Test
tests =
    [ test "should not report non-exposed variables" <|
        \() ->
            testRule """module A exposing (b)
a = 1"""
                |> Lint.Test.expectNoErrors
    , test "should not report used type constructors" <|
        \() ->
            testRule """module A exposing (b)
type Foo = Bar | Baz
a = Bar
b = Baz"""
                |> Lint.Test.expectNoErrors
    , test "should not report unused type constructors when module is exposing all" <|
        \() ->
            testRule """module A exposing (..)
type Foo = Bar | Baz
"""
                |> Lint.Test.expectNoErrors
    , test "should not report unused type constructors when module is exposing the constructors of that type" <|
        \() ->
            testRule """module A exposing (Foo(..))
type Foo = Bar | Baz
"""
                |> Lint.Test.expectNoErrors
    , test "should report unused type constructors" <|
        \() ->
            testRule """module A exposing (b)
type Foo = Bar | Baz"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Type constructor `Bar` is not used."
                        , under = "Bar"
                        }
                    , Lint.Test.error
                        { message = "Type constructor `Baz` is not used."
                        , under = "Baz"
                        }
                    ]
    , test "should report unused type constructors, even if the type is exposed" <|
        \() ->
            testRule """module A exposing (Foo)
type Foo = Bar | Baz"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Type constructor `Bar` is not used."
                        , under = "Bar"
                        }
                    , Lint.Test.error
                        { message = "Type constructor `Baz` is not used."
                        , under = "Baz"
                        }
                    ]
    ]


all : Test
all =
    describe "NoUnusedTypeConstructors" tests
