module MiscRules.NoUnusedTypeConstructorsTest exposing (all)

import MiscRules.NoUnusedTypeConstructors exposing (rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : String -> ReviewResult
testRule =
    Review.Test.run rule


details : List String
details =
    [ "Since it is not being used, I recommend removing it. It should make the code clearer to read for other people."
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
    describe "NoUnusedTypeConstructors" tests
