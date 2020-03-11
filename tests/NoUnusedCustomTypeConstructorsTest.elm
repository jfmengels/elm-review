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


all : Test
all =
    describe "NoUnusedCustomTypeConstructors"
        [ unusedTests, phantomTypeTests ]


unusedTests : Test
unusedTests =
    describe "Unused variables"
        [ test "should not report non-exposed variables" <|
            \() ->
                testRule """module MyModule exposing (b)
a = 1"""
                    |> Review.Test.expectNoErrors
        , test "should not report used type constructors" <|
            \() ->
                testRule """module MyModule exposing (b)
type Foo = Bar | Baz
a = Bar
b = Baz"""
                    |> Review.Test.expectNoErrors
        , test "should not report unused type constructors when module is exposing all" <|
            \() ->
                testRule """module MyModule exposing (..)
type Foo = Bar | Baz
"""
                    |> Review.Test.expectNoErrors
        , test "should not report unused type constructors when module is exposing the constructors of that type" <|
            \() ->
                testRule """module MyModule exposing (Foo(..))
type Foo = Bar | Baz
"""
                    |> Review.Test.expectNoErrors
        , test "should report unused type constructors" <|
            \() ->
                testRule """module MyModule exposing (b)
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
                testRule """module MyModule exposing (Foo)
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


phantomTypeTests : Test
phantomTypeTests =
    describe "Phantom type"
        [ test "should not report a custom type with one constructor, when it is used in the stead of a phantom variable" <|
            \() ->
                testRule """module MyModule exposing (id)
type User = User
type Id a = Id

id : Id User
id = Id
"""
                    |> Review.Test.expectNoErrors
        , test "should not report a custom type with one constructor, when it is used in the stead of a phantom variable in a let variable" <|
            \() ->
                testRule """module MyModule exposing (id)
type User = User
type Id a = Id


id =
  let
    a : Id User
    a = Id
  in
  a
"""
                    |> Review.Test.expectNoErrors
        , test "should report a custom type with multiple constructors, when it is used in the stead of a phantom variable" <|
            \() ->
                testRule """module MyModule exposing (id)
type Something = A | B
type Id a = Id

id : Id Something
id = Id
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `A` is not used."
                            , details = details
                            , under = "A"
                            }
                        , Review.Test.error
                            { message = "Type constructor `B` is not used."
                            , details = details
                            , under = "B"
                            }
                        ]
        , test "should report a custom type with one constructor, when there is a phantom type available but it isn't used" <|
            \() ->
                testRule """module MyModule exposing (id)
type User = User
type Id a = Id
id = Id
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = details
                            , under = "User"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 13 }, end = { row = 2, column = 17 } }
                        ]
        , test "should report a custom type with one constructor when the constructor is named differently than the type, even when it is used in the stead of a phantom variable" <|
            \() ->
                testRule """module MyModule exposing (id)
type User = UserConstructor
type Id a = Id

id : Id User
id = Id
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `UserConstructor` is not used."
                            , details = details
                            , under = "UserConstructor"
                            }
                        ]
        , test "should report a custom type with one constructor, when it is used in the stead of a non-phantom variable" <|
            \() ->
                testRule """module MyModule exposing (id)
type User = User
type Id a = Id a

id : Id User
id = Id
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = details
                            , under = "User"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 13 }, end = { row = 2, column = 17 } }
                        ]
        , test "should report a custom type with a type variable, when it is used in the stead of a phantom variable" <|
            \() ->
                testRule """module MyModule exposing (id)
type User something = User
type Id a = Id a

id : Id (User otherThing)
id = Id
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = details
                            , under = "User"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 27 } }
                        ]
        , test "should report a custom type with one constructor that has arguments, when it is used in the stead of a phantom variable" <|
            \() ->
                testRule """module MyModule exposing (id)
type User = User Something
type Id a = Id a

id : Id User
id = Id
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Type constructor `User` is not used."
                            , details = details
                            , under = "User"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 13 }, end = { row = 2, column = 17 } }
                        ]

        -- TODO Handle phantom types from other modules
        ]
