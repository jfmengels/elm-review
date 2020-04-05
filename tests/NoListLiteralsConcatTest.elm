module NoListLiteralsConcatTest exposing (all)

import NoListLiteralsConcat exposing (rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : String -> ReviewResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Review.Test.run rule


all : Test
all =
    describe "NoListLiteralsConcat"
        [ usingPlusPlusTests
        ]


message : String
message =
    "Expression could be simplified to be a single List"


details : List String
details =
    [ "Try moving all the elements into a single list." ]


usingPlusPlusTests : Test
usingPlusPlusTests =
    describe "Using (++)"
        [ test "should not report a single list literal" <|
            \() ->
                testRule """
a = []
b = [1]
c = [ "string", "foo", "bar" ]
"""
                    |> Review.Test.expectNoErrors
        , test "should report concatenating two list literals" <|
            \() ->
                testRule """
a = [ 1 ] ++ [ 2, 3 ]
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "[ 1 ] ++ [ 2, 3 ]"
                            }
                        ]
        , test "should report concatenating two list literals, even they contain variables" <|
            \() ->
                testRule """
a = [ a, 1 ] ++ [ b, 2 ]
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "[ a, 1 ] ++ [ b, 2 ]"
                            }
                        ]
        , test "should report concatenating an empty list and something" <|
            \() ->
                testRule """
a = [] ++ something
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "[]"
                            }
                        ]
        , test "should report concatenating something and an empty list" <|
            \() ->
                testRule """
a = something ++ []
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "[]"
                            }
                        ]
        , test "should not report using :: to a variable or expression" <|
            \() ->
                testRule """
a = 1 :: list
b = 1 :: foo bar
"""
                    |> Review.Test.expectNoErrors
        , test "should report using :: to a list literal" <|
            \() ->
                testRule """
a = 1 :: [ 2, 3]
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "1 :: [ 2, 3]"
                            }
                        ]
        , test "should not report List.concat that contains a variable or expression" <|
            \() ->
                testRule """
a = List.concat [ foo, bar ]
b = List.concat [ [ 1 ], foo ]
c = List.concat [ foo, [ 1 ] ]
"""
                    |> Review.Test.expectNoErrors
        , test "should report List.concat with no items" <|
            \() ->
                testRule """
a = List.concat []
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "List.concat []"
                            }
                        ]
        , test "should report List.concat with a single item" <|
            \() ->
                testRule """
a = List.concat [ a ]
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "List.concat [ a ]"
                            }
                        ]
        , test "should report List.concat that only contains list literals" <|
            \() ->
                testRule """
a = List.concat [ [ 1, 2, 3 ], [ 4, 5, 6] ]
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "List.concat [ [ 1, 2, 3 ], [ 4, 5, 6] ]"
                            }
                        ]
        ]
