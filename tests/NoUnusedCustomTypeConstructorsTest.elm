module NoUnusedCustomTypeConstructorsTest exposing (all)

import NoUnused.CustomTypeConstructors exposing (rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : String -> ReviewResult
testRule =
    Review.Test.run rule


details : List String
details =
    [ "This type constructor is never used. It might be handled everywhere it might appear, but there is no location where this value actually gets created."
    , "You should either use this value somewhere, or remove it at the location I pointed at."
    , "If you remove it, you may find that other pieces of code are never used, and can themselves be removed too. This could end up simplifying your code a lot."
    ]


tests : List Test
tests =
    [ test "should not report non-exposed variables" <|
        \() ->
            testRule """module A exposing (b)
a = 1"""
                |> Review.Test.expectNoErrors
    , test "should not report used type constructors" <|
        \() ->
            testRule """module A exposing (b)
type Foo = Bar | Baz
a = Bar
b = Baz"""
                |> Review.Test.expectNoErrors
    , test "should not report unused type constructors when module is exposing all" <|
        \() ->
            testRule """module A exposing (..)
type Foo = Bar | Baz
"""
                |> Review.Test.expectNoErrors
    , test "should not report unused type constructors when module is exposing the constructors of that type" <|
        \() ->
            testRule """module A exposing (Foo(..))
type Foo = Bar | Baz
"""
                |> Review.Test.expectNoErrors
    , test "should report unused type constructors" <|
        \() ->
            testRule """module A exposing (b)
type Foo = Bar | Baz"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Type constructor `Bar` is not used."
                        , details = details
                        , under = "Bar"
                        }
                    , Review.Test.error
                        { message = "Type constructor `Baz` is not used."
                        , details = details
                        , under = "Baz"
                        }
                    ]
    , test "should report unused type constructors, even if the type is exposed" <|
        \() ->
            testRule """module A exposing (Foo)
type Foo = Bar | Baz"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Type constructor `Bar` is not used."
                        , details = details
                        , under = "Bar"
                        }
                    , Review.Test.error
                        { message = "Type constructor `Baz` is not used."
                        , details = details
                        , under = "Baz"
                        }
                    ]
    ]


all : Test
all =
    describe "NoUnusedCustomTypeConstructors" tests
