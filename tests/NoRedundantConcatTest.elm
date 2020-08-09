module NoRedundantConcatTest exposing (..)

import NoRedundantConcat
import Review.Test
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "NoRedundantConcat"
        [ test "Simple one" <|
            \_ ->
                """module A exposing (..)

a = [ foo ] ++ b
"""
                    |> Review.Test.run NoRedundantConcat.rule
                    |> Review.Test.expectErrors
                        [ useConsError
                            """[ foo ] ++ b"""
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo :: b
"""
                        ]
        , test "Multiple" <|
            \_ ->
                """module A exposing (..)

a = [ foo, bar ] ++ b
"""
                    |> Review.Test.run NoRedundantConcat.rule
                    |> Review.Test.expectErrors
                        [ useConsError
                            """[ foo, bar ] ++ b"""
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo :: bar :: b
"""
                        ]
        , test "Does not mention concat of non-list literals" <|
            \_ ->
                """module A exposing (..)

a = foo ++ bar ++ b
"""
                    |> Review.Test.run NoRedundantConcat.rule
                    |> Review.Test.expectNoErrors
        , test "Concat of list literals can be single list" <|
            \_ ->
                """module A exposing (..)

a = [ foo ] ++ [ bar ]
"""
                    |> Review.Test.run NoRedundantConcat.rule
                    |> Review.Test.expectErrors
                        [ useSingleListError
                            """[ foo ] ++ [ bar ]"""
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = [foo, bar]
"""
                        ]
        , test "List.concat of literal lists can be single list" <|
            \_ ->
                """module A exposing (..)

a = List.concat [ [ a ], [ b ] ]
"""
                    |> Review.Test.run NoRedundantConcat.rule
                    |> Review.Test.expectErrors
                        [ redundantListConcatError
                            """List.concat [ [ a ], [ b ] ]"""
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = [a, b]
"""
                        ]
        , test "List.concat of multiple literal lists can be single list" <|
            \_ ->
                """module A exposing (..)

a = List.concat [ [ a, b ], [ c, d ], [ e, f ] ]
"""
                    |> Review.Test.run NoRedundantConcat.rule
                    |> Review.Test.expectErrors
                        [ redundantListConcatError
                            """List.concat [ [ a, b ], [ c, d ], [ e, f ] ]"""
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = [a, b, c, d, e, f]
"""
                        ]
        , test "Concatenation of literal strings can be single string" <|
            \_ ->
                """module A exposing (..)

a = "hello" ++ " world"
"""
                    |> Review.Test.run NoRedundantConcat.rule
                    |> Review.Test.expectErrors
                        [ redundantStringConcatError
                            """"hello" ++ " world\""""
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = "hello world"
"""
                        ]
        ]


redundantStringConcatError : String -> Review.Test.ExpectedError
redundantStringConcatError under =
    Review.Test.error
        { message = "Concatenating a literal string with another literal string is redundant"
        , details =
            [ "Expressions like `\"foo\" ++ \"bar\"` are harder to read than `\"foobar\"`. Consider simplifying this expression."
            ]
        , under = under
        }


redundantListConcatError : String -> Review.Test.ExpectedError
redundantListConcatError under =
    Review.Test.error
        { message = "Using List.concat to concatenate list literals is redundant"
        , details =
            [ "Rather than using `List.concat`, consider putting the elements into a single list literal"
            ]
        , under = under
        }


useConsError : String -> Review.Test.ExpectedError
useConsError under =
    Review.Test.error
        { message = "Concatenating a literal list with something else can be written using cons operators"
        , details =
            [ "Expressions like `[ foo ] ++ b` can be written as `foo :: b`."
            , "This preserves the mental model that `List` is a linked list, with the performance considerations associated with those."
            ]
        , under = under
        }


useSingleListError : String -> Review.Test.ExpectedError
useSingleListError under =
    Review.Test.error
        { message = "Concatenating a literal list with another literal list can be written as a single list literal"
        , details =
            [ "Expressions like `[ foo ] ++ [ bar ]` can be written as `[ foo, bar ]`."
            , "Using 'complex' expressions when not necessary can make code look a lot more complex than it really is. When you need to put two literal lists together, you can just put them together! No need to have that happen at runtime."
            ]
        , under = under
        }
