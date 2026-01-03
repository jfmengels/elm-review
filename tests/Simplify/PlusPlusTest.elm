module Simplify.PlusPlusTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "(++)"
        [ test "should not report a single list literal" <|
            \() ->
                """module A exposing (..)
a = []
b = [1]
c = [ "string", "foo", "bar" ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report simple strings" <|
            \() ->
                """module A exposing (..)
a = "abc" ++ value
b = \"\"\"123\"\"\"
c = \"\"\"multi
line
string
\"\"\"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace "a" ++ "" by "a\"""" <|
            \() ->
                """module A exposing (..)
a = "a" ++ ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending \"\""
                            , details = [ "You can replace this operation by the left string." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "a"
"""
                        ]
        , test """should replace "a" ++ "" ++ x by "a" ++ x""" <|
            \() ->
                """module A exposing (..)
a = "a" ++ "" ++ x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending \"\""
                            , details = [ "You can replace this operation by the left string." ]
                            , under = "++"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 9 }, end = { row = 2, column = 11 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "a" ++ x
"""
                        ]
        , test """should replace x ++ "a" ++ "" by x ++ "a\"""" <|
            \() ->
                """module A exposing (..)
a = x ++ "a" ++ ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending \"\""
                            , details = [ "You can replace this operation by the left string." ]
                            , under = "++"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 14 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x ++ "a"
"""
                        ]
        , test """should replace x ++ "" ++ "a" by x ++ "a\"""" <|
            \() ->
                """module A exposing (..)
a = x ++ "" ++ "a"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending \"\""
                            , details = [ "You can replace this operation by the right string." ]
                            , under = "++"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 13 }, end = { row = 2, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x ++ "a"
"""
                        ]
        , test """should replace "" ++ x ++ "a" ++ "" by x ++ "a\"""" <|
            \() ->
                """module A exposing (..)
a = "" ++ x ++ "a"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending \"\""
                            , details = [ "You can replace this operation by the right string." ]
                            , under = "++"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 8 }, end = { row = 2, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x ++ "a"
"""
                        ]
        , test """should not report x ++ "" (because this can lead to better performance)""" <|
            \() ->
                """module A exposing (..)
a = x ++ ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace "" ++ "a" by "a\"""" <|
            \() ->
                """module A exposing (..)
a = "" ++ "a"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending \"\""
                            , details = [ "You can replace this operation by the right string." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "a"
"""
                        ]
        , test """should replace "a" ++ "b" by "ab" if they're on the same line""" <|
            \() ->
                """module A exposing (..)
a = "a" ++ "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on string literals can be turned into a single string literal"
                            , details = [ "Try moving all the elements into a single string literal." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "ab"
"""
                        ]
        , test """should replace "a" ++ "b" ++ x by "ab" ++ x""" <|
            \() ->
                """module A exposing (..)
a = "a" ++ "b" ++ x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on string literals can be turned into a single string literal"
                            , details = [ "Try moving all the elements into a single string literal." ]
                            , under = "++"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 9 }, end = { row = 2, column = 11 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "ab" ++ x
"""
                        ]
        , test """should not replace "a" ++ x ++ "b\"""" <|
            \() ->
                """module A exposing (..)
a = "a" ++ x ++ "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace x ++ "a" ++ "b" by x ++ "ab\"""" <|
            \() ->
                """module A exposing (..)
a = x ++ "a" ++ "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on string literals can be turned into a single string literal"
                            , details = [ "Try moving all the elements into a single string literal." ]
                            , under = "++"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 14 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x ++ "ab"
"""
                        ]
        , test """should not replace "a" ++ "b" if they're on distinct lines (single quotes)""" <|
            \() ->
                """module A exposing (..)
a = "a"
    ++ "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should not replace "a" ++ "b" if they're on distinct lines (triple quotes)""" <|
            \() ->
                """module A exposing (..)
a = \"\"\"a\"\"\"
    ++ \"\"\"b\"\"\"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace \"\"\"a\"\"\" ++ \"\"\"b\"\"\" by \"\"\"ab\"\"\" if they're on the same line""" <|
            \() ->
                """module A exposing (..)
a = \"\"\"a\"\"\" ++ \"\"\"b\"\"\"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on string literals can be turned into a single string literal"
                            , details = [ "Try moving all the elements into a single string literal." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = \"\"\"ab\"\"\"
"""
                        ]
        , test """should replace "a" ++ \"\"\"b\"\"\" by \"\"\"ab\"\"\" if they're on the same line""" <|
            \() ->
                """module A exposing (..)
a = "a" ++ \"\"\"b\"\"\"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on string literals can be turned into a single string literal"
                            , details = [ "Try moving all the elements into a single string literal." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = \"\"\"ab\"\"\"
"""
                        ]
        , test """should replace \"\"\"a\"\"\" ++ "b" by \"\"\"ab\"\"\" if they're on the same line""" <|
            \() ->
                """module A exposing (..)
a = \"\"\"a\"\"\" ++ "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on string literals can be turned into a single string literal"
                            , details = [ "Try moving all the elements into a single string literal." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = \"\"\"ab\"\"\"
"""
                        ]
        , test """should replace "a" ++ "b" if they share a line ("a" uses triple quotes)""" <|
            \() ->
                """module A exposing (..)
a = \"\"\"a
    \"\"\" ++ "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on string literals can be turned into a single string literal"
                            , details = [ "Try moving all the elements into a single string literal." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = \"\"\"a
    b\"\"\"
"""
                        ]
        , test """should replace "a" ++ "b" if they share a line ("b" uses triple quotes)""" <|
            \() ->
                """module A exposing (..)
a = "a" ++ \"\"\"b
            \"\"\"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on string literals can be turned into a single string literal"
                            , details = [ "Try moving all the elements into a single string literal." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = \"\"\"ab
            \"\"\"
"""
                        ]
        , test """should replace "a" ++ "b" if they share a line (both use triple quotes)""" <|
            \() ->
                """module A exposing (..)
a = \"\"\"a
    \"\"\" ++ \"\"\"b
            \"\"\"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on string literals can be turned into a single string literal"
                            , details = [ "Try moving all the elements into a single string literal." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = \"\"\"a
    b
            \"\"\"
"""
                        ]
        , test "should report concatenating two list literals if they're one the same line" <|
            \() ->
                """module A exposing (..)
a = [ 1 ] ++ [ 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1 , 2, 3 ]
"""
                        ]
        , test "should report concatenating two list literals if all list elements are on different lines" <|
            \() ->
                """module A exposing (..)
a = [ 0
    , 1 ]
    ++ [ 2
       , 3
       ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = [ 0
    , 1 , 2
       , 3
       ]
"""
                        ]
        , test "should not report concatenating two list literals if two lists are on different lines" <|
            \() ->
                """module A exposing (..)
a = [ 0, 1 ]
    ++ [ 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report concatenating two list literals, even if they contain variables" <|
            \() ->
                """module A exposing (..)
a = [ a, 1 ] ++ [ b, 2 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ a, 1 , b, 2 ]
"""
                        ]
        , test "should report concatenating [] and something" <|
            \() ->
                """module A exposing (..)
a = [] ++ something
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending []"
                            , details = [ "You can replace this operation by the right list." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = something
"""
                        ]
        , test "should report concatenating something and []" <|
            \() ->
                """module A exposing (..)
a = something ++ []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending []"
                            , details = [ "You can replace this operation by the left list." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = something
"""
                        ]
        , test "should report concatenating something and [] even when something appears afterwards" <|
            \() ->
                """module A exposing (..)
a = something ++ [] ++ x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending []"
                            , details = [ "You can replace this operation by the right list." ]
                            , under = "++"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 21 }, end = { row = 2, column = 23 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = something ++ x
"""
                        ]
        , test "should replace [b] ++ c by b :: c" <|
            \() ->
                """module A exposing (..)
a = [ b ] ++ c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending a singleton list to the beginning is the same as using (::) with the value inside"
                            , details = [ "You can replace this (++) operation by using (::) with the value inside the left singleton list on the right list." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b :: c
"""
                        ]
        , test "should replace [ f n ] ++ c by (f n) :: c" <|
            \() ->
                """module A exposing (..)
a = [ f n ] ++ c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending a singleton list to the beginning is the same as using (::) with the value inside"
                            , details = [ "You can replace this (++) operation by using (::) with the value inside the left singleton list on the right list." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f n) :: c
"""
                        ]
        , test "should not replace [b] ++ c when on the right of a ++ operator" <|
            \() ->
                """module A exposing (..)
a = left ++ [ b ] ++ c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace [b] ++ c when on the right of a ++ operator but inside parens" <|
            \() ->
                """module A exposing (..)
a = left ++ ([ b ] ++ c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.fromList [ b, c ] ++ String.fromList [ d, e ] by String.fromList [ b, c , d, e ]" <|
            \() ->
                """module A exposing (..)
a = String.fromList [ b, c ] ++ String.fromList [ d, e ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromList [ b, c , d, e ]
"""
                        ]
        ]
