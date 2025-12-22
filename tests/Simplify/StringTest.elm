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
        , stringReverseTests
        , stringSliceTests
        , stringRightTests
        , stringLeftTests
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
                            , details = [ "You can replace this composition by String.repeat with the same arguments given to List.repeat which is meant for this exact purpose." ]
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
                            , details = [ "You can replace this composition by String.join with the same arguments given to List.intersperse which is meant for this exact purpose." ]
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
a = (String.fromChar b)
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
a = (String.left n string)
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
a = String.right n0 (String.right n1 string)
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
a = (String.right n string)
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
