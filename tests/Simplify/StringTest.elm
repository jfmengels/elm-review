module Simplify.StringTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "String"
        [ stringToListTests
        , stringFromListTests
        , stringFromIntTests
        , stringFromFloatTests
        , stringIsEmptyTests
        , stringLengthTests
        , stringConcatTests
        , stringJoinTests
        , stringRepeatTests
        , stringReplaceTests
        , stringWordsTests
        , stringLinesTests
        , stringAppendTests
        , stringToLowerTests
        , stringToUpperTests
        , stringReverseTests
        , stringTrimLeftTests
        , stringTrimRightTests
        , stringTrimTests
        , stringSliceTests
        , stringRightTests
        , stringLeftTests
        , stringDropRightTests
        , stringDropLeftTests
        , stringFilterTests
        , stringUnconsTests
        , stringMapTests
        , stringAnyTests
        , stringAllTests
        , stringFoldlTests
        , stringFoldrTests
        ]


stringIsEmptyTests : Test
stringIsEmptyTests =
    describe "String.isEmpty"
        [ test "should not report String.concat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty
b = String.isEmpty value
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.isEmpty \"\" by True" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on \"\" will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace String.isEmpty \"a\" by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty "a"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on this string will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace String.isEmpty (String.fromChar c) by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty (String.fromChar c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on this string will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace String.isEmpty (String.fromInt n) by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty (String.fromInt n)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on this string will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace String.isEmpty (String.fromFloat n) by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty (String.fromFloat n)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on this string will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace String.isEmpty (String.cons h t) by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty (String.cons h t)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on this string will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace String.isEmpty (String.fromList (h :: t)) by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty (String.fromList (h :: t))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on this string will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace String.isEmpty (String.repeat 2 (String.fromChar 'x')) by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty (String.repeat 2 (String.fromChar 'x'))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on this string will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace String.isEmpty (str ++ String.fromChar 'x') by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty (str ++ String.fromChar 'x')
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on this string will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace String.isEmpty (String.fromChar 'x' ++ str) by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty (String.fromChar 'x' ++ str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on this string will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace String.isEmpty (String.append str <| String.fromChar 'x') by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty (String.append str <| String.fromChar 'x')
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on this string will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace String.isEmpty (String.reverse string) by String.isEmpty string" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty (String.reverse string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.reverse before String.isEmpty"
                            , details = [ "Reordering the chars in a string does not affect its length. You can replace the String.reverse call by the unchanged string." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.isEmpty string
"""
                        ]
        , test "should replace String.isEmpty (String.map f string) by String.isEmpty string" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty (String.map f string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.map before String.isEmpty"
                            , details = [ "Changing each char in a string to another char can never make a non-empty string empty or an empty string non-empty. You can replace the String.map call by the unchanged string." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.isEmpty string
"""
                        ]
        , test "should replace String.isEmpty (String.fromList list) by List.isEmpty list" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty (String.fromList list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.isEmpty can be combined into List.isEmpty"
                            , details = [ "You can replace this call by List.isEmpty with the same argument given to String.fromList which is meant for this exact purpose." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.isEmpty list)
"""
                        ]
        , test "should replace String.isEmpty << String.fromList by List.isEmpty" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty << String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.isEmpty can be combined into List.isEmpty"
                            , details = [ "You can replace this composition by List.isEmpty which is meant for this exact purpose." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty
"""
                        ]
        ]


stringLengthTests : Test
stringLengthTests =
    describe "String.length"
        [ test "should not report String.length that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.length
b = String.length str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report String.length (String.fromList list) because String.length measures UTF-16 parts, not code points" <|
            \() ->
                """module A exposing (..)
a = String.length (String.fromList list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report String.length (String.fromChar c) because c could have 2 UTF-16 parts and therefore length 2"
            (\() ->
                """module A exposing (..)
a = String.length (String.fromChar c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
            )
        , test "should not report String.length (String.fromList [ c0, c1 ]) because any Char could have 2 UTF-16 parts and therefore length 2"
            (\() ->
                """module A exposing (..)
a = String.length (String.fromList [ c0, c1 ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
            )
        , test "should replace String.length \"\" by 0" <|
            \() ->
                """module A exposing (..)
a = String.length ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 0"
                            , details = [ "The length of the string can be determined by looking at the code." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace String.length \"abc\" by 3" <|
            \() ->
                """module A exposing (..)
a = String.length "abc"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 3"
                            , details = [ "The length of the string can be determined by looking at the code." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should replace String.length \"a\\t🚀b\\c🇲🇻\\u{000D}\\r\" by 13" <|
            \() ->
                """module A exposing (..)
a = String.length "a\\t🚀b\\\\c🇲🇻\\u{000D}\\r"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 13"
                            , details = [ "The length of the string can be determined by looking at the code." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = 13
"""
                        ]
        , test "should replace String.length \"\"\"a\\t🚀b\\c🇲🇻\\u{000D}\\r\"\"\" by 13" <|
            \() ->
                """module A exposing (..)
a = String.length \"\"\"a\\t🚀b\\\\c🇲🇻\\u{000D}\\r\"\"\"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 13"
                            , details = [ "The length of the string can be determined by looking at the code." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = 13
"""
                        ]
        , test "should replace if l == \"\" then String.length l else 1 by if l == \"\" then 0 else 1" <|
            \() ->
                """module A exposing (..)
a = if l == "" then String.length l else 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 0"
                            , details =
                                [ "The length of the string can be determined by looking at the code."
                                ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if l == "" then 0 else 1
"""
                        ]
        , test "should replace String.length (String.reverse string) by String.length string" <|
            \() ->
                """module A exposing (..)
a = String.length (String.reverse string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.reverse before String.length"
                            , details = [ "Reordering the chars in a string does not affect its length. You can replace the String.reverse call by the unchanged string." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.length string
"""
                        ]
        ]


stringConcatTests : Test
stringConcatTests =
    describe "String.concat"
        [ test "should not report String.concat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.concat list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.concat [] by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.concat []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.concat on [] will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should report String.concat with a single item" <|
            \() ->
                """module A exposing (..)
a = [ string ] |> String.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.concat on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = string
"""
                        ]
        , test "should replace String.concat << List.singleton by identity" <|
            \() ->
                """module A exposing (..)
a = String.concat << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.concat on a singleton list will always result in the value inside"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should remove empty list literals passed to List.concat (last item)" <|
            \() ->
                """module A exposing (..)
a = String.concat [ a, "" ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.concat on a list containing an irrelevant \"\""
                            , details = [ "Including \"\" in the list does not change the result of this call. You can remove the \"\" element." ]
                            , under = "\"\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.concat [ a ]
"""
                        ]
        , test "should replace String.concat [ string0, String.concat [ string1, string2 ], string3, String.concat [ string4, string5 ] ] by String.concat [ string0, string1, string2, string3, string4, string5 ]" <|
            \() ->
                """module A exposing (..)
a = String.concat [ string0, String.concat [ string1, string2 ], string3, String.concat [ string4, string5 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Nested String.concat calls can be spread"
                            , details = [ "You can move the elements from the inner String.concat calls to inside this outer String.concat call." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 18 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.concat [ string0,  string1, string2 , string3,  string4, string5  ]
"""
                        ]
        , test "should replace String.concat (List.repeat n str) by (String.repeat n str)" <|
            \() ->
                """module A exposing (..)
a = String.concat (List.repeat n str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat, then String.concat can be combined into String.repeat"
                            , details = [ "You can replace this call by String.repeat with the same arguments given to List.repeat which is meant for this exact purpose." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.repeat n str)
"""
                        ]
        , test "should replace str |> List.repeat n |> String.concat by str |> String.repeat n" <|
            \() ->
                """module A exposing (..)
a = str |> List.repeat n |> String.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat, then String.concat can be combined into String.repeat"
                            , details = [ "You can replace this call by String.repeat with the same arguments given to List.repeat which is meant for this exact purpose." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = str |> String.repeat n
"""
                        ]
        , test "should replace String.concat << List.repeat n by String.repeat n" <|
            \() ->
                """module A exposing (..)
a = String.concat << List.repeat n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat, then String.concat can be combined into String.repeat"
                            , details = [ "You can replace this composition by String.repeat with the same argument given to List.repeat which is meant for this exact purpose." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.repeat n
"""
                        ]
        , test "should replace String.concat (List.intersperse str strings) by (String.join str strings)" <|
            \() ->
                """module A exposing (..)
a = String.concat (List.intersperse str strings)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse, then String.concat can be combined into String.join"
                            , details = [ "You can replace this call by String.join with the same arguments given to List.intersperse which is meant for this exact purpose." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.join str strings)
"""
                        ]
        , test "should replace str |> List.intersperse str |> String.concat by str |> String.join str" <|
            \() ->
                """module A exposing (..)
a = str |> List.intersperse str |> String.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse, then String.concat can be combined into String.join"
                            , details = [ "You can replace this call by String.join with the same arguments given to List.intersperse which is meant for this exact purpose." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = str |> String.join str
"""
                        ]
        , test "should replace String.concat << List.intersperse str by String.join str" <|
            \() ->
                """module A exposing (..)
a = String.concat << List.intersperse str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse, then String.concat can be combined into String.join"
                            , details = [ "You can replace this composition by String.join with the same argument given to List.intersperse which is meant for this exact purpose." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.join str
"""
                        ]
        ]


stringJoinTests : Test
stringJoinTests =
    describe "String.join"
        [ test "should not report String.join that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.join b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.join b [] by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.join b []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.join on [] will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test """should replace String.join "" list by String.concat list""" <|
            \() ->
                """module A exposing (..)
a = String.join "" list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.join with separator \"\" is the same as String.concat"
                            , details = [ "You can replace this call by String.concat." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.concat list
"""
                        ]
        , test """should replace String.join "" by String.concat""" <|
            \() ->
                """module A exposing (..)
a = String.join ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.join with separator \"\" is the same as String.concat"
                            , details = [ "You can replace this call by String.concat." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.concat
"""
                        ]
        , test """should replace list |> String.join "" by list |> String.concat""" <|
            \() ->
                """module A exposing (..)
a = list |> String.join ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.join with separator \"\" is the same as String.concat"
                            , details = [ "You can replace this call by String.concat." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> String.concat
"""
                        ]
        ]


stringRepeatTests : Test
stringRepeatTests =
    describe "String.repeat"
        [ test "should not report String.repeat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.repeat n str
b = String.repeat 5 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace String.repeat n "" by \"\"""" <|
            \() ->
                """module A exposing (..)
a = String.repeat n ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat with \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.repeat 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.repeat 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat with length 0 will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test """should replace String.repeat 0 by (always "")""" <|
            \() ->
                """module A exposing (..)
a = String.repeat 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat with length 0 will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.repeat -5 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.repeat -5 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat with negative length will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.repeat 1 str by str" <|
            \() ->
                """module A exposing (..)
a = String.repeat 1 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat 1 will always return the same given string to repeat"
                            , details = [ "You can replace this call by the string to repeat itself." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = str
"""
                        ]
        , test "should replace String.repeat 1 by identity" <|
            \() ->
                """module A exposing (..)
a = String.repeat 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat 1 will always return the same given string to repeat"
                            , details = [ "You can replace this call by identity." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


stringReplaceTests : Test
stringReplaceTests =
    describe "String.replace"
        [ test "should not report String.replace that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a0 = String.replace
a1 = String.replace from
a2 = String.replace from to
a3 = String.replace from to str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report String.replace on String.replace with same arguments because the replaced string could contain a pattern that would get replaced" <|
            \() ->
                """module A exposing (..)
a = String.replace from to (String.replace from to str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.replace n n by identity" <|
            \() ->
                """module A exposing (..)
a = String.replace n n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.replace where the pattern to replace and the replacement are equal will always return the same given string"
                            , details = [ "You can replace this call by identity." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace String.replace n n x by x" <|
            \() ->
                """module A exposing (..)
a = String.replace n n x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.replace where the pattern to replace and the replacement are equal will always return the same given string"
                            , details = [ "You can replace this call by the string itself." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace String.replace x y \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.replace x y ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.replace on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.replace x y z by z when we know what the value will be and that it is unchanged" <|
            \() ->
                """module A exposing (..)
a = String.replace "x" "y" "z"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.replace with a pattern not present in the given string will result in the given string"
                            , details = [ "You can replace this call by the given string itself." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "z"
"""
                        ]
        , test "should replace z |> String.replace x y z by z when we know what the value will be and that it is unchanged" <|
            \() ->
                """module A exposing (..)
a = "z" |> String.replace "x" "y"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.replace with a pattern not present in the given string will result in the given string"
                            , details = [ "You can replace this call by the given string itself." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "z"
"""
                        ]
        , test "should not replace String.replace x y z by z when we know what the value will be but it will be different" <|
            \() ->
                """module A exposing (..)
a = String.replace "x" "y" "xz"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not remove String.replace call when the replacement is \u{000D}" <|
            \() ->
                """module A exposing (..)
a = \"\"\"
foo
bar
\"\"\"
                    |> String.replace "\\r" ""

b = \"\"\"
foo
bar
\"\"\"
                    |> String.replace "\\u{000D}" ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


stringWordsTests : Test
stringWordsTests =
    describe "String.words"
        [ test "should not report String.words that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.words
b = String.words str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace String.words "" by [ "" ]""" <|
            \() ->
                """module A exposing (..)
a = String.words ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.words on \"\" will result in [ \"\" ]"
                            , details = [ "You can replace this call by [ \"\" ]." ]
                            , under = "String.words"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ "" ]
"""
                        ]
        ]


stringLinesTests : Test
stringLinesTests =
    describe "String.lines"
        [ test "should not report String.lines that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.lines
b = String.lines str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace String.lines "" by []""" <|
            \() ->
                """module A exposing (..)
a = String.lines ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.lines on \"\" will result in [ \"\" ]"
                            , details = [ "You can replace this call by [ \"\" ]." ]
                            , under = "String.lines"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ "" ]
"""
                        ]
        ]


stringToListTests : Test
stringToListTests =
    describe "String.toList"
        [ test "should not report String.toList that contains a variable" <|
            \() ->
                """module A exposing (..)
a = String.toList
b = String.toList str
c = String.toList << f << String.fromList
d = (String.toList << f) << String.fromList
e = String.toList << (f << String.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace x |> f |> String.fromList |> String.toList by x |> f" <|
            \() ->
                """module A exposing (..)
a = x |> f |> String.fromList |> String.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can replace this call by the argument given to String.fromList." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f
"""
                        ]
        , test "should replace String.toList << String.fromList by identity" <|
            \() ->
                """module A exposing (..)
a = String.toList << String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace String.toList << (String.fromList << f) by (f)" <|
            \() ->
                """module A exposing (..)
a = String.toList << (String.fromList << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace String.toList << (String.fromList << g << f) by (g << f)" <|
            \() ->
                """module A exposing (..)
a = String.toList << (String.fromList << g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (g << f)
"""
                        ]
        , test "should replace String.toList << (f >> String.fromList) by (f)" <|
            \() ->
                """module A exposing (..)
a = String.toList << (f >> String.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace String.toList << (f >> g >> String.fromList) by (f >> g)" <|
            \() ->
                """module A exposing (..)
a = String.toList << (f >> g >> String.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f >> g)
"""
                        ]
        , test "should replace (f << String.toList) << String.fromList by (f)" <|
            \() ->
                """module A exposing (..)
a = (f << String.toList) << String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace (g << f << String.toList) << String.fromList by (g << f)" <|
            \() ->
                """module A exposing (..)
a = (g << f << String.toList) << String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (g << f)
"""
                        ]
        , test "should replace (String.toList >> f) << String.fromList by (f)" <|
            \() ->
                """module A exposing (..)
a = (String.toList >> f) << String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace (String.toList >> f >> g) << String.fromList by (f >> g)" <|
            \() ->
                """module A exposing (..)
a = (String.toList >> f >> g) << String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f >> g)
"""
                        ]
        ]


stringFromListTests : Test
stringFromListTests =
    describe "String.fromList"
        [ test "should not report String.fromList that contains a variable" <|
            \() ->
                """module A exposing (..)
a = String.fromList
b = String.fromList list
c = String.fromList << f << String.toList
d = (String.fromList << f) << String.toList
e = String.fromList << (f << String.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.fromList [] by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.fromList []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList on [] will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.fromList [ char ] by String.fromChar char" <|
            \() ->
                """module A exposing (..)
a = String.fromList [ char ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList on a singleton list will result in String.fromChar with the value inside"
                            , details = [ "You can replace this call by String.fromChar with the value inside the singleton list." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar char
"""
                        ]
        , test "should replace String.fromList [ f a ] by String.fromChar (f a)" <|
            \() ->
                """module A exposing (..)
a = String.fromList [ f b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList on a singleton list will result in String.fromChar with the value inside"
                            , details = [ "You can replace this call by String.fromChar with the value inside the singleton list." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar (f b)
"""
                        ]
        , test "should replace String.fromList (List.singleton char) by String.fromChar char" <|
            \() ->
                """module A exposing (..)
a = String.fromList (List.singleton char)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList on a singleton list will result in String.fromChar with the value inside"
                            , details = [ "You can replace this call by String.fromChar with the value inside the singleton list." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar char
"""
                        ]
        , test "should replace List.singleton >> String.fromList by String.fromChar" <|
            \() ->
                """module A exposing (..)
a = List.singleton >> String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList on a singleton list will result in String.fromChar with the value inside"
                            , details = [ "You can replace this call by String.fromChar." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar
"""
                        ]
        , test "should replace x |> f |> String.toList |> String.fromList by x |> f" <|
            \() ->
                """module A exposing (..)
a = x |> f |> String.toList |> String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can replace this call by the argument given to String.toList." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f
"""
                        ]
        , test "should replace String.fromList << String.toList by identity" <|
            \() ->
                """module A exposing (..)
a = String.fromList << String.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace String.fromList << (String.toList << f) by (f)" <|
            \() ->
                """module A exposing (..)
a = String.fromList << (String.toList << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace String.fromList << (String.toList << g << f) by (g << f)" <|
            \() ->
                """module A exposing (..)
a = String.fromList << (String.toList << g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (g << f)
"""
                        ]
        , test "should replace (i << h << String.fromList) << (String.toList << g << f) by (i << h) << (g << f)" <|
            \() ->
                """module A exposing (..)
a = (i << h << String.fromList) << (String.toList << g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (i << h) << (g << f)
"""
                        ]
        , test "should replace (String.fromList >> h >> i) << (f >> g >> String.toList) by (h >> i) << (f >> g)" <|
            \() ->
                """module A exposing (..)
a = (String.fromList >> h >> i) << (f >> g >> String.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (h >> i) << (f >> g)
"""
                        ]
        , test "should replace (i << (h << String.fromList)) << ((String.toList << g) << f) by (i << (h)) << ((g) << f)" <|
            \() ->
                """module A exposing (..)
a = (i << (h << String.fromList)) << ((String.toList << g) << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (i << (h)) << ((g) << f)
"""
                        ]
        , test "should replace String.fromList << (f >> String.toList) by (f)" <|
            \() ->
                """module A exposing (..)
a = String.fromList << (f >> String.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace (f << String.fromList) << String.toList by (f)" <|
            \() ->
                """module A exposing (..)
a = (f << String.fromList) << String.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace (String.fromList >> f) << String.toList by (f)" <|
            \() ->
                """module A exposing (..)
a = (String.fromList >> f) << String.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace (String.fromList >> f >> g) << String.toList by (f >> g)" <|
            \() ->
                """module A exposing (..)
a = (String.fromList >> f >> g) << String.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f >> g)
"""
                        ]
        ]


stringFromIntTests : Test
stringFromIntTests =
    describe "String.fromInt"
        [ test "should not report String.fromInt that contains a variable" <|
            \() ->
                """module A exposing (..)
a = String.fromInt
b = String.fromInt q
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.fromInt 123 by \"123\"" <|
            \() ->
                """module A exposing (..)
a = String.fromInt 123
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromInt on 123 will result in \"123\""
                            , details = [ "You can replace this call by \"123\"." ]
                            , under = "String.fromInt"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "123"
"""
                        ]
        ]


stringFromFloatTests : Test
stringFromFloatTests =
    describe "String.fromFloat"
        [ test "should not report String.fromFloat that contains a variable" <|
            \() ->
                """module A exposing (..)
a = String.fromFloat
b = String.fromFloat q
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.fromFloat 1.23 by \"1.23\"" <|
            \() ->
                """module A exposing (..)
a = String.fromFloat 1.23
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromFloat on 1.23 will result in \"1.23\""
                            , details = [ "You can replace this call by \"1.23\"." ]
                            , under = "String.fromFloat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "1.23"
"""
                        ]
        ]


stringAppendTests : Test
stringAppendTests =
    describe "String.append"
        [ test "should not report String.append used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = String.append string1 string2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.append \"\" string by string" <|
            \() ->
                """module A exposing (..)
a = String.append "" string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append \"\" will always return the same given string"
                            , details = [ "You can replace this call by the string itself." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = string
"""
                        ]
        , test "should replace String.append string \"\" by string" <|
            \() ->
                """module A exposing (..)
a = String.append string ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.append with \"\""
                            , details = [ "You can replace this call by the given first string." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = string
"""
                        ]
        , test "should replace \"\" |> String.append string by string" <|
            \() ->
                """module A exposing (..)
a = "" |> String.append string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.append with \"\""
                            , details = [ "You can replace this call by the given first string." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = string
"""
                        ]
        , test "should replace string |> String.append \"\" by string" <|
            \() ->
                """module A exposing (..)
a = "" |> String.append string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.append with \"\""
                            , details = [ "You can replace this call by the given first string." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = string
"""
                        ]
        , test "should report String.append applied on two string literals" <|
            \() ->
                """module A exposing (..)
a = String.append (String.fromList [b,c]) (String.fromList [d,e])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.fromList [b,c,d,e])
"""
                        ]
        , test "should report String.append applied on two string literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
a = String.append (String.fromList [ b, z ]) (String.fromList [c,d,0])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.fromList [ b, z ,c,d,0])
"""
                        ]
        , test "should report String.append <| on two string literals" <|
            \() ->
                """module A exposing (..)
a = String.append (String.fromList [b, c]) <| String.fromList [d,e]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromList [b, c,d,e]
"""
                        ]
        , test "should report String.append |> on two string literals" <|
            \() ->
                """module A exposing (..)
a = String.fromList [d,e] |> String.append (String.fromList [b,c])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromList [b,c,d,e]
"""
                        ]
        , test "should report String.append |> on two string literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
a = String.fromList [c,d,0] |> String.append (String.fromList [ b, z ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromList [ b, z ,c,d,0]
"""
                        ]
        , test "should replace String.append ([ b, c ] |> String.fromList) (String.fromList [ d, e ]) by (String.fromList [ b, c, d, e ])" <|
            \() ->
                """module A exposing (..)
a = String.append ([ b, c ] |> String.fromList) (String.fromList [ d, e ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.fromList [ b, c , d, e ])
"""
                        ]
        , test "should replace String.append ([ b, c ] |> String.fromList) (String.fromList <| [ d, e ]) by (String.fromList <| [ b, c, d, e ])" <|
            \() ->
                """module A exposing (..)
a = String.append ([ b, c ] |> String.fromList) (String.fromList <| [ d, e ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.fromList <| [ b, c , d, e ])
"""
                        ]
        , test "should replace String.append (String.fromList <| [ b, c ]) ([ d, e ] |> String.fromList) by ([ b, c , d, e ] |> String.fromList)" <|
            \() ->
                """module A exposing (..)
a = String.append (String.fromList <| [ b, c ]) ([ d, e ] |> String.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ([ b, c , d, e ] |> String.fromList)
"""
                        ]
        , test "should replace [ d, e ] |> String.fromList |> String.append (String.fromList <| [ b, c ]) by [ b, c , d, e ] |> String.fromList" <|
            \() ->
                """module A exposing (..)
a = [ d, e ] |> String.fromList |> String.append (String.fromList <| [ b, c ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ b, c , d, e ] |> String.fromList
"""
                        ]
        ]


stringToLowerTests : Test
stringToLowerTests =
    describe "String.toLower"
        [ test "should not report String.toLower with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = String.toLower
a1 = String.toLower str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report String.toLower on String.toUpper because for example ß splits into SS on toUpper" <|
            \() ->
                """module A exposing (..)
a0 = String.toLower (String.toUpper str)
a1 = String.toLower << String.toUpper
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.toLower (String.toLower str) to String.toLower str" <|
            \() ->
                """module A exposing (..)
a = String.toLower (String.toLower str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.toLower after String.toLower"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.toLower"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 19 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.toLower str
"""
                        ]
        , test "should replace String.toLower << String.toLower to String.toLower" <|
            \() ->
                """module A exposing (..)
a = String.toLower << String.toLower
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.toLower after String.toLower"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.toLower"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 19 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.toLower
"""
                        ]
        ]


stringToUpperTests : Test
stringToUpperTests =
    describe "String.toUpper"
        [ test "should not report String.toUpper with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = String.toUpper
a1 = String.toUpper str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.toUpper (String.toUpper str) to String.toUpper str" <|
            \() ->
                """module A exposing (..)
a = String.toUpper (String.toUpper str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.toUpper after String.toUpper"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.toUpper"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 19 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.toUpper str
"""
                        ]
        , test "should replace String.toUpper << String.toUpper to String.toUpper" <|
            \() ->
                """module A exposing (..)
a = String.toUpper << String.toUpper
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.toUpper after String.toUpper"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.toUpper"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 19 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.toUpper
"""
                        ]
        , test "should not report String.toUpper (String.toLower str), see https://github.com/jfmengels/elm-review-simplify/pull/429#issuecomment-3746681750" <|
            \() ->
                """module A exposing (..)
a = String.toUpper (String.toLower str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report String.toUpper << String.toLower, see https://github.com/jfmengels/elm-review-simplify/pull/429#issuecomment-3746681750" <|
            \() ->
                """module A exposing (..)
a = String.toUpper << String.toLower
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


stringReverseTests : Test
stringReverseTests =
    describe "String.reverse"
        [ test "should not report String.reverse with okay arguments" <|
            \() ->
                """module A exposing (..)
a = String.reverse
b = String.reverse str
c = (String.reverse << f) << String.reverse
d = String.reverse << (f << String.reverse)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.reverse \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.reverse ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.reverse (String.fromChar a) by (String.fromChar a)" <|
            \() ->
                """module A exposing (..)
a = String.reverse (String.fromChar b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse on a single-char string will result in the given single-char string"
                            , details = [ "You can replace this call by the given single-char string." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar b
"""
                        ]
        , test "should replace a |> String.fromChar |> String.reverse by a |> String.fromChar" <|
            \() ->
                """module A exposing (..)
a = b |> String.fromChar |> String.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse on a single-char string will result in the given single-char string"
                            , details = [ "You can replace this call by the given single-char string." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> String.fromChar
"""
                        ]
        , test "should replace String.reverse << String.fromChar by String.fromChar" <|
            \() ->
                """module A exposing (..)
a = String.reverse << String.fromChar
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse on a single-char string will result in the unchanged single-char string"
                            , details = [ "You can replace this composition by String.fromChar." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar
"""
                        ]
        , test "should replace String.fromChar >> String.reverse by String.fromChar" <|
            \() ->
                """module A exposing (..)
a = String.fromChar >> String.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse on a single-char string will result in the unchanged single-char string"
                            , details = [ "You can replace this composition by String.fromChar." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar
"""
                        ]
        , test "should replace String.reverse <| String.reverse <| x by x" <|
            \() ->
                """module A exposing (..)
a = String.reverse <| String.reverse <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse, then String.reverse cancels each other out"
                            , details = [ "You can replace this call by the argument given to String.reverse." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 19 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify String.reverse >> String.reverse to identity" <|
            \() ->
                """module A exposing (..)
a = String.reverse >> String.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse, then String.reverse cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 37 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify (f << String.reverse) << String.reverse to (f)" <|
            \() ->
                """module A exposing (..)
a = (f << String.reverse) << String.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse, then String.reverse cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 25 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should simplify String.reverse << (String.reverse << f) to (f)" <|
            \() ->
                """module A exposing (..)
a = String.reverse << (String.reverse << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse, then String.reverse cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 19 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        ]


stringTrimLeftTests : Test
stringTrimLeftTests =
    describe "String.trimLeft"
        [ test "should not report String.trimLeft with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = String.trimLeft
a1 = String.trimLeft str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.trimLeft (String.trimLeft str) by String.trimLeft str" <|
            \() ->
                """module A exposing (..)
a = String.trimLeft (String.trimLeft str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.trimLeft after String.trimLeft"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.trimLeft"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 20 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.trimLeft str
"""
                        ]
        , test "should replace String.trimLeft (String.trimRight str) by String.trim str" <|
            \() ->
                """module A exposing (..)
a = String.trimLeft (String.trimRight str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.trimRight, then String.trimLeft can be combined into String.trim"
                            , details = [ "You can replace this call by String.trim with the same argument given to String.trimRight which is meant for this exact purpose." ]
                            , under = "String.trimLeft"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 20 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.trim str)
"""
                        ]
        , test "should replace String.trimLeft (String.trim str) by String.trim str" <|
            \() ->
                """module A exposing (..)
a = String.trimLeft (String.trim str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.trimLeft on a String.trim call"
                            , details = [ "You can replace this call by the given String.trim call." ]
                            , under = "String.trimLeft"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 20 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.trim str
"""
                        ]
        ]


stringTrimRightTests : Test
stringTrimRightTests =
    describe "String.trimRight"
        [ test "should not report String.trimRight with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = String.trimRight
a1 = String.trimRight str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.trimRight (String.trimRight str) by String.trimRight str" <|
            \() ->
                """module A exposing (..)
a = String.trimRight (String.trimRight str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.trimRight after String.trimRight"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.trimRight"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 21 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.trimRight str
"""
                        ]
        , test "should replace String.trimRight (String.trimLeft str) by String.trim str" <|
            \() ->
                """module A exposing (..)
a = String.trimRight (String.trimLeft str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.trimLeft, then String.trimRight can be combined into String.trim"
                            , details = [ "You can replace this call by String.trim with the same argument given to String.trimLeft which is meant for this exact purpose." ]
                            , under = "String.trimRight"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 21 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.trim str)
"""
                        ]
        , test "should replace String.trimRight (String.trim str) by String.trim str" <|
            \() ->
                """module A exposing (..)
a = String.trimRight (String.trim str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.trimRight on a String.trim call"
                            , details = [ "You can replace this call by the given String.trim call." ]
                            , under = "String.trimRight"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 21 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.trim str
"""
                        ]
        ]


stringTrimTests : Test
stringTrimTests =
    describe "String.trim"
        [ test "should not report String.trim with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = String.trim
a1 = String.trim str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.trim (String.trim str) by String.trim str" <|
            \() ->
                """module A exposing (..)
a = String.trim (String.trim str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.trim after String.trim"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.trim"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.trim str
"""
                        ]
        , test "should replace String.trim (String.trimLeft str) by String.trim str" <|
            \() ->
                """module A exposing (..)
a = String.trim (String.trimLeft str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.trimLeft before String.trim"
                            , details = [ "Trimming from the start is already covered by the final String.trim. You can replace the String.trimLeft call by the unchanged string." ]
                            , under = "String.trim"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.trim str
"""
                        ]
        , test "should replace String.trim (String.trimRight str) by String.trim str" <|
            \() ->
                """module A exposing (..)
a = String.trim (String.trimRight str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.trimRight before String.trim"
                            , details = [ "Trimming from the end is already covered by the final String.trim. You can replace the String.trimRight call by the unchanged string." ]
                            , under = "String.trim"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.trim str
"""
                        ]
        ]


stringSliceTests : Test
stringSliceTests =
    describe "String.slice"
        [ test "should not report String.slice that contains variables or expressions" <|
            \() ->
                """module A exposing (..)
a = String.slice b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report String.slice 0 n" <|
            \() ->
                """module A exposing (..)
a = String.slice 0
b = String.slice 0 n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report String.slice on String.map because the given function could change the UTF-16 length (String.slice is not unicode-aware)" <|
            \() ->
                """module A exposing (..)
a = String.slice start end (String.map f string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.slice start end \"\" by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice start end ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.slice b 0 by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice b 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice with end index 0 will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.slice b 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice b 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice with end index 0 will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.slice n n by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice n n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice with equal start and end index will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.slice n n str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice n n str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice with equal start and end index will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.slice a z \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice a z ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.slice with natural start >= natural end by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice 2 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice with a start index greater than the end index will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.slice with negative start >= negative end by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice -1 -2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice with a negative start index closer to the right than the negative end index will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should not report String.slice with negative start, natural end" <|
            \() ->
                """module A exposing (..)
a = String.slice -1 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        []
        , test "should not report String.slice with natural start, negative end" <|
            \() ->
                """module A exposing (..)
a = String.slice 1 -2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        []
        ]


stringLeftTests : Test
stringLeftTests =
    describe "String.left"
        [ test "should not report String.left that contains variables or expressions" <|
            \() ->
                """module A exposing (..)
a = String.left n string
b = String.left n0 (String.left n1 string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report String.left on String.map because the given function could change the UTF-16 length (String.left is not unicode-aware)" <|
            \() ->
                """module A exposing (..)
a = String.left n (String.map f string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.left 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.left 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.left with length 0 will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.left"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.left 0 by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.left 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.left with length 0 will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.left"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.left -literal by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.left -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.left with negative length will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.left"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.left n \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.left n ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.left on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.left"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.left n (String.left n string) by String.left n string" <|
            \() ->
                """module A exposing (..)
a = String.left n (String.left n string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.left after equivalent String.left"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.left"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.left n string
"""
                        ]
        , test "should replace String.left n >> String.left n by String.left n" <|
            \() ->
                """module A exposing (..)
a = String.left n >> String.left n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.left after equivalent String.left"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.left"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 22 }, end = { row = 2, column = 33 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.left n
"""
                        ]
        , test "should replace String.left n << String.left n by String.left n" <|
            \() ->
                """module A exposing (..)
a = String.left n << String.left n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.left after equivalent String.left"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.left"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.left n
"""
                        ]
        ]


stringRightTests : Test
stringRightTests =
    describe "String.right"
        [ test "should not report String.right that contains variables or expressions" <|
            \() ->
                """module A exposing (..)
a = String.right n string
b = String.right n0 (String.right n1 string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report String.right on String.map because the given function could change the UTF-16 length (String.right is not unicode-aware)" <|
            \() ->
                """module A exposing (..)
a = String.right n (String.map f string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.right n (String.right n string) by String.right n string" <|
            \() ->
                """module A exposing (..)
a = String.right n (String.right n string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.right after equivalent String.right"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.right"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.right n string
"""
                        ]
        , test "should replace String.right n >> String.right n by String.right n" <|
            \() ->
                """module A exposing (..)
a = String.right n >> String.right n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.right after equivalent String.right"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.right"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 35 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.right n
"""
                        ]
        , test "should replace String.right n << String.right n by String.right n" <|
            \() ->
                """module A exposing (..)
a = String.right n << String.right n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.right after equivalent String.right"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.right"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.right n
"""
                        ]
        , test "should replace String.right 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.right with length 0 will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.right 0 by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.right with length 0 will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.right -literal str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right -1 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.right with negative length will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.right -literal by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.right with negative length will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.right n \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right n ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.right on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        ]


stringDropLeftTests : Test
stringDropLeftTests =
    describe "String.dropLeft"
        [ test "should not report String.dropLeft with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = String.dropLeft
a1 = String.dropLeft n
a2 = String.dropLeft n str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.dropLeft n \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.dropLeft n ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.dropLeft on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.dropLeft"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.dropLeft 0 str by str" <|
            \() ->
                """module A exposing (..)
a = String.dropLeft 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.dropLeft with count 0 will always return the same given string"
                            , details = [ "You can replace this call by the string itself." ]
                            , under = "String.dropLeft"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = str
"""
                        ]
        , test "should replace String.dropLeft -1 str by str" <|
            \() ->
                """module A exposing (..)
a = String.dropLeft -1 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.dropLeft with negative count will always return the same given string"
                            , details = [ "You can replace this call by the string itself." ]
                            , under = "String.dropLeft"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = str
"""
                        ]
        , test "should replace String.dropLeft 10 \"Hello\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.dropLeft 10 "Hello"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.dropLeft with a count greater than or equal to the given string's length will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.dropLeft"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should not report String.dropLeft on String.map because the given function could change the UTF-16 length (String.dropLeft is not unicode-aware)" <|
            \() ->
                """module A exposing (..)
a = String.dropLeft n (String.map f string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


stringUnconsTests : Test
stringUnconsTests =
    describe "String.uncons"
        [ test "should not report String.uncons with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = String.uncons
a1 = String.uncons str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.uncons \"\" by Nothing" <|
            \() ->
                """module A exposing (..)
a = String.uncons ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.uncons on \"\" will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "String.uncons"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        ]


stringDropRightTests : Test
stringDropRightTests =
    describe "String.dropRight"
        [ test "should not report String.dropRight with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = String.dropRight
a1 = String.dropRight n
a2 = String.dropRight n str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.dropRight n \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.dropRight n ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.dropRight on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.dropRight"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.dropRight 0 str by str" <|
            \() ->
                """module A exposing (..)
a = String.dropRight 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.dropRight with count 0 will always return the same given string"
                            , details = [ "You can replace this call by the string itself." ]
                            , under = "String.dropRight"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = str
"""
                        ]
        , test "should replace String.dropRight -1 str by str" <|
            \() ->
                """module A exposing (..)
a = String.dropRight -1 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.dropRight with negative count will always return the same given string"
                            , details = [ "You can replace this call by the string itself." ]
                            , under = "String.dropRight"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = str
"""
                        ]
        , test "should replace String.dropRight 10 \"Hello\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.dropRight 10 "Hello"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.dropRight with a count greater than or equal to the given string's length will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.dropRight"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should not report String.dropRight on String.map because the given function could change the UTF-16 length (String.dropRight is not unicode-aware)" <|
            \() ->
                """module A exposing (..)
a = String.dropRight n (String.map f string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


stringFilterTests : Test
stringFilterTests =
    describe "String.filter"
        [ test "should not report String.filter used with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = String.filter f x
a1 = String.filter f (String.filter g x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.filter f \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.filter f ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.filter on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.filter f (String.filter f string) by String.filter f string" <|
            \() ->
                """module A exposing (..)
a = String.filter f (String.filter f string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.filter after equivalent String.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 18 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.filter f string
"""
                        ]
        , test "should replace String.filter f << String.filter f by String.filter f" <|
            \() ->
                """module A exposing (..)
a = String.filter f << String.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.filter after equivalent String.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "String.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 18 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.filter f
"""
                        ]
        , test "should replace String.filter (always True) x by x" <|
            \() ->
                """module A exposing (..)
a = String.filter (always True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.filter with a function that will always return True will always return the same given string"
                            , details = [ "You can replace this call by the string itself." ]
                            , under = "String.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace String.filter (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = String.filter (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.filter with a function that will always return True will always return the same given string"
                            , details = [ "You can replace this call by identity." ]
                            , under = "String.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace String.filter (always False) x by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.filter (always False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.filter with a function that will always return False will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.filter (always False) by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.filter with a function that will always return False will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.filter f (String.reverse string) by String.reverse (String.filter f string)" <|
            \() ->
                """module A exposing (..)
a = String.filter f (String.reverse string)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.filter on String.reverse can be optimized to String.reverse on String.filter"
                            , details = [ "You can replace this call by String.reverse, on String.filter with the function given to the original String.filter." ]
                            , under = "String.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.reverse (String.filter f string)
"""
                        ]
        , test "should replace String.filter f << String.reverse by String.reverse << String.filter f" <|
            \() ->
                """module A exposing (..)
a = String.filter f << String.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.filter on String.reverse can be optimized to String.reverse on String.filter"
                            , details = [ "You can replace this composition by String.filter with the function given to the original String.filter, then String.reverse." ]
                            , under = "String.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.reverse << String.filter f)
"""
                        ]
        ]


stringMapTests : Test
stringMapTests =
    describe "String.map"
        [ test "should not report String.map with okay arguments"
            (\() ->
                """module A exposing (..)
a0 = String.map
a1 = String.map f
a2 = String.map f str
a3 = String.map f (String.repeat n str)
a4 = String.repeat n (String.map f str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
            )
        , test "should replace String.map f \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.map f ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.map on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.map identity str by str" <|
            \() ->
                """module A exposing (..)
a = String.map identity str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.map with an identity function will always return the same given string"
                            , details = [ "You can replace this call by the string itself." ]
                            , under = "String.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = str
"""
                        ]
        , test "should replace String.map identity by identity" <|
            \() ->
                """module A exposing (..)
a = String.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.map with an identity function will always return the same given string"
                            , details = [ "You can replace this call by identity." ]
                            , under = "String.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace String.map f (String.repeat n (String.fromChar c)) by String.repeat n (String.fromChar (f c))" <|
            \() ->
                """module A exposing (..)
a = String.map f (String.repeat n (String.fromChar c))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.map on String.repeat on String.fromChar is the same as String.repeat on String.fromChar with the mapped char"
                            , details = [ "You can replace this call by the String.repeat on String.fromChar operation but with the function given to the String.map operation applied to the original char." ]
                            , under = "String.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.repeat n (String.fromChar (f c))
"""
                        ]
        , test "should replace String.map (f ; x) (String.repeat n <| String.fromChar (g ; y)) by String.repeat n <| String.fromChar ((f ; x) ; (g ; y))" <|
            \() ->
                """module A exposing (..)
a = String.map (f
                x) (String.repeat n<|String.fromChar (g
                                                        y))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.map on String.repeat on String.fromChar is the same as String.repeat on String.fromChar with the mapped char"
                            , details = [ "You can replace this call by the String.repeat on String.fromChar operation but with the function given to the String.map operation applied to the original char." ]
                            , under = "String.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.repeat n<|String.fromChar ((f
                x)
                                                     (g
                                                        y)))
"""
                        ]
        ]


stringAnyTests : Test
stringAnyTests =
    describe "String.any"
        [ test "should not report String.any with okay arguments"
            (\() ->
                """module A exposing (..)
a0 = String.any
a1 = String.any f
a2 = String.any f str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
            )
        , test "should replace String.any f \"\" by False" <|
            \() ->
                """module A exposing (..)
a = String.any f ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.any on \"\" will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace String.any (always False) str by False" <|
            \() ->
                """module A exposing (..)
a = String.any (always False) str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.any with a function that will always return False will always result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace String.any (always False) by False" <|
            \() ->
                """module A exposing (..)
a = String.any (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.any with a function that will always return False will always result in False"
                            , details = [ "You can replace this call by always False." ]
                            , under = "String.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always False
"""
                        ]
        , test "should replace String.any (always True) string by not (String.isEmpty string)" <|
            \() ->
                """module A exposing (..)
a = String.any (always True) string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.any with a function that will always return True is the same as Basics.not on String.isEmpty"
                            , details = [ "You can replace this call by Basics.not on String.isEmpty on the string given to the String.any call." ]
                            , under = "String.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (String.isEmpty string)
"""
                        ]
        , test "should replace String.any (always True) by not << String.isEmpty" <|
            \() ->
                """module A exposing (..)
a = String.any (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.any with a function that will always return True is the same as Basics.not on String.isEmpty"
                            , details = [ "You can replace this call by String.isEmpty, then Basics.not." ]
                            , under = "String.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (not << String.isEmpty)
"""
                        ]
        ]


stringAllTests : Test
stringAllTests =
    describe "String.all"
        [ test "should not report String.all with okay arguments"
            (\() ->
                """module A exposing (..)
a0 = String.all
a1 = String.all f
a2 = String.all f str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
            )
        , test "should replace String.all f \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.all f ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.all on \"\" will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "String.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace String.all (always True) str by True" <|
            \() ->
                """module A exposing (..)
a = String.all (always True) str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.all with a function that will always return True will always result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "String.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace String.all (always True) by True" <|
            \() ->
                """module A exposing (..)
a = String.all (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.all with a function that will always return True will always result in True"
                            , details = [ "You can replace this call by always True." ]
                            , under = "String.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always True
"""
                        ]
        , test "should replace String.all (always False) string by String.isEmpty string" <|
            \() ->
                """module A exposing (..)
a = String.all (always False) string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.all with a function that will always return False is the same as String.isEmpty"
                            , details = [ "You can replace this call by String.isEmpty on the string given to the String.all call." ]
                            , under = "String.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.isEmpty string
"""
                        ]
        , test "should replace String.all (always False) by String.isEmpty" <|
            \() ->
                """module A exposing (..)
a = String.all (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.all with a function that will always return False is the same as String.isEmpty"
                            , details = [ "You can replace this call by String.isEmpty." ]
                            , under = "String.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.isEmpty
"""
                        ]
        ]


stringFoldlTests : Test
stringFoldlTests =
    describe "String.foldl"
        [ test "should not report String.foldl used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = String.foldl
b = String.foldl (\\el soFar -> soFar - el)
c = String.foldl (\\el soFar -> soFar - el) 20
d = String.foldl (\\el soFar -> soFar - el) 20 string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.foldl f initial \"\" by initial" <|
            \() ->
                """module A exposing (..)
a = String.foldl f initial ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldl on \"\" will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "String.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial
"""
                        ]
        , test "should replace String.foldl (always identity) initial string by initial" <|
            \() ->
                """module A exposing (..)
a = String.foldl (always identity) initial string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "String.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial
"""
                        ]
        , test "should replace String.foldl (always identity) initial by always initial" <|
            \() ->
                """module A exposing (..)
a = String.foldl (always identity) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "String.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always initial
"""
                        ]
        , test "should replace String.foldl (always identity) by always" <|
            \() ->
                """module A exposing (..)
a = String.foldl (always identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which string is supplied next." ]
                            , under = "String.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always
"""
                        ]
        ]


stringFoldrTests : Test
stringFoldrTests =
    describe "String.foldr"
        [ test "should not report String.foldr used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = String.foldr
b = String.foldr (\\el soFar -> soFar - el)
c = String.foldr (\\el soFar -> soFar - el) 20
d = String.foldr (\\el soFar -> soFar - el) 20 string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.foldr f initial \"\" by initial" <|
            \() ->
                """module A exposing (..)
a = String.foldr f initial ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldr on \"\" will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "String.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial
"""
                        ]
        , test "should replace String.foldr (always identity) initial string by initial" <|
            \() ->
                """module A exposing (..)
a = String.foldr (always identity) initial string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "String.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial
"""
                        ]
        , test "should replace String.foldr (always identity) initial by always initial" <|
            \() ->
                """module A exposing (..)
a = String.foldr (always identity) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "String.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always initial
"""
                        ]
        , test "should replace String.foldr (always identity) by always" <|
            \() ->
                """module A exposing (..)
a = String.foldr (always identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which string is supplied next." ]
                            , under = "String.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always
"""
                        ]
        ]
