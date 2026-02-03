module Simplify.PipelineTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Pipelines"
        [ test "should replace >> when used directly in a |> pipeline" <|
            \() ->
                """module A exposing (..)
a =
    b
        |> f
        >> g
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use |> instead of >>"
                            , details =
                                [ "Because of the precedence of operators, using >> at this location is the same as using |>."
                                , "To make it more idiomatic in Elm and generally easier to read, please use |> instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: data |> fn1 |> (fn2 >> fn3)
After:  data |> fn1 |>  fn2 |> fn3"""
                                ]
                            , under = ">>"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    b
        |> f
        |> g
"""
                        ]
        , test "should replace >> when used directly in a |> pipeline (multiple)" <|
            \() ->
                """module A exposing (..)
a =
    b
        |> f
        >> g
        >> h
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use |> instead of >>"
                            , details =
                                [ "Because of the precedence of operators, using >> at this location is the same as using |>."
                                , "To make it more idiomatic in Elm and generally easier to read, please use |> instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: data |> fn1 |> (fn2 >> fn3)
After:  data |> fn1 |>  fn2 |> fn3"""
                                ]
                            , under = ">>"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 9 }, end = { row = 5, column = 11 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    b
        |> f
        |> g
        |> h
"""
                        ]
        , test "should replace >> when used directly in a |> pipeline (right argument >> inside parentheses)" <|
            \() ->
                """module A exposing (..)
a =
    b
        |> (f >> g)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use |> instead of >>"
                            , details =
                                [ "Because of the precedence of operators, using >> at this location is the same as using |>."
                                , "To make it more idiomatic in Elm and generally easier to read, please use |> instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: data |> fn1 |> (fn2 >> fn3)
After:  data |> fn1 |>  fn2 |> fn3"""
                                ]
                            , under = ">>"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    b
        |> f |> g
"""
                        ]
        , test "should replace >> when used directly in a |> pipeline (right argument |> >> inside parentheses)" <|
            \() ->
                """module A exposing (..)
a =
    b
        |> (f |> g >> h)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use |> instead of >>"
                            , details =
                                [ "Because of the precedence of operators, using >> at this location is the same as using |>."
                                , "To make it more idiomatic in Elm and generally easier to read, please use |> instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: data |> fn1 |> (fn2 >> fn3)
After:  data |> fn1 |>  fn2 |> fn3"""
                                ]
                            , under = ">>"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    b
        |> (f |> g |> h)
"""
                        ]
        , test "should replace << when used directly in a <| pipeline" <|
            \() ->
                """module A exposing (..)
a =
    g <<
    f <|
        b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of <<"
                            , details =
                                [ "Because of the precedence of operators, using << at this location is the same as using <|."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: (fn3 << fn2) <| fn1 <| data
After:   fn3 <| fn2  <| fn1 <| data"""
                                ]
                            , under = "<<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    g <|
    f <|
        b
"""
                        ]
        , test "should replace << when used directly in a <| pipeline (multiple)" <|
            \() ->
                """module A exposing (..)
a =
    h << g << f <| b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of <<"
                            , details =
                                [ "Because of the precedence of operators, using << at this location is the same as using <|."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: (fn3 << fn2) <| fn1 <| data
After:   fn3 <| fn2  <| fn1 <| data"""
                                ]
                            , under = "<<"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    h <| g <| f <| b
"""
                        ]
        , test "should replace << when used directly in a <| pipeline (left argument << inside parentheses)" <|
            \() ->
                """module A exposing (..)
a =
    (f << g)
        <| b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of <<"
                            , details =
                                [ "Because of the precedence of operators, using << at this location is the same as using <|."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: (fn3 << fn2) <| fn1 <| data
After:   fn3 <| fn2  <| fn1 <| data"""
                                ]
                            , under = "<<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    f <| g
        <| b
"""
                        ]
        , test "should replace << when used directly in a <| pipeline (left argument << <| inside parentheses)" <|
            \() ->
                """module A exposing (..)
a =
    (f << g <| h) <|
        b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of <<"
                            , details =
                                [ "Because of the precedence of operators, using << at this location is the same as using <|."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: (fn3 << fn2) <| fn1 <| data
After:   fn3 <| fn2  <| fn1 <| data"""
                                ]
                            , under = "<<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    (f <| g <| h) <|
        b
"""
                        ]
        , test "should reverse >> when used in a <| pipeline (with parentheses)" <|
            \() ->
                """module A exposing (..)
a = (f >> g) <| b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of >>"
                            , details =
                                [ "Mixing chains of functions with >> in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: data |> (fn1 >> fn2)
After:  data |> fn1 |> fn2"""
                                ]
                            , under = ">>"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = g <| f <| b
"""
                        ]
        , test "should reverse >> when used in a <| pipeline (without parentheses)" <|
            \() ->
                """module A exposing (..)
a = f >> g <| b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of >>"
                            , details =
                                [ "Mixing chains of functions with >> in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: data |> (fn1 >> fn2)
After:  data |> fn1 |> fn2"""
                                ]
                            , under = ">>"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = g <| f <| b
"""
                        ]
        , test "should reverse >> when used in a <| pipeline (many elements)" <|
            \() ->
                """module A exposing (..)
a = f >> g >> h >> i <| b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of >>"
                            , details =
                                [ "Mixing chains of functions with >> in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: data |> (fn1 >> fn2)
After:  data |> fn1 |> fn2"""
                                ]
                            , under = ">>"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 17 }, end = { row = 2, column = 19 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = i <| h <| g <| f <| b
"""
                        ]
        , test "should reverse >> when used in a <| pipeline (many elements, including parens)" <|
            \() ->
                """module A exposing (..)
a = f >> g >> (h >> i) <| b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of >>"
                            , details =
                                [ "Mixing chains of functions with >> in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: data |> (fn1 >> fn2)
After:  data |> fn1 |> fn2"""
                                ]
                            , under = ">>"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 18 }, end = { row = 2, column = 20 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = i <| h <| g <| f <| b
"""
                        ]
        , test "should not reverse << sub-compositions in a <| pipeline" <|
            \() ->
                """module A exposing (..)
a = f >> g >> (i << h) <| b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of >>"
                            , details =
                                [ "Mixing chains of functions with >> in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: data |> (fn1 >> fn2)
After:  data |> fn1 |> fn2"""
                                ]
                            , under = ">>"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = i << h <| g <| f <| b
"""
                        ]
        , test "should reverse << when used in a |> pipeline (with parentheses)" <|
            \() ->
                """module A exposing (..)
a = b |> (g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use |> instead of <<"
                            , details =
                                [ "Mixing chains of functions with << in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use |> instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: (fn2 << fn1) data
After:   fn2 <| fn1  data"""
                                ]
                            , under = "<<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> f |> g
"""
                        ]
        , test "should reverse << when used in a |> pipeline (without parentheses)" <|
            \() ->
                """module A exposing (..)
a = b |> g << f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use |> instead of <<"
                            , details =
                                [ "Mixing chains of functions with << in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use |> instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: (fn2 << fn1) data
After:   fn2 <| fn1  data"""
                                ]
                            , under = "<<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> f |> g
"""
                        ]
        , test "should reverse << when used in a |> pipeline (many elements)" <|
            \() ->
                """module A exposing (..)
a = b |> i << h << g << f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use |> instead of <<"
                            , details =
                                [ "Mixing chains of functions with << in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use |> instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: (fn2 << fn1) data
After:   fn2 <| fn1  data"""
                                ]
                            , under = "<<"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> f |> g |> h |> i
"""
                        ]
        , test "should reverse << when used in a |> pipeline (many elements, including parens)" <|
            \() ->
                """module A exposing (..)
a = b |> i << h << (g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use |> instead of <<"
                            , details =
                                [ "Mixing chains of functions with << in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use |> instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: (fn2 << fn1) data
After:   fn2 <| fn1  data"""
                                ]
                            , under = "<<"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> f |> g |> h |> i
"""
                        ]
        , test "should not reverse >> sub-compositions in a |> pipeline" <|
            \() ->
                """module A exposing (..)
a = b |> i << h << (f >> g)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use |> instead of <<"
                            , details =
                                [ "Mixing chains of functions with << in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use |> instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: (fn2 << fn1) data
After:   fn2 <| fn1  data"""
                                ]
                            , under = "<<"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> f >> g |> h |> i
"""
                        ]
        , test "should reverse >> when used in a plain application call" <|
            \() ->
                """module A exposing (..)
a = (f >> g) b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of >>"
                            , details =
                                [ "Mixing chains of functions with >> in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: data |> (fn1 >> fn2)
After:  data |> fn1 |> fn2"""
                                ]
                            , under = ">>"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (g <| f b)
"""
                        ]
        , test "should reverse >> when used in a plain application call, and add parens if there are more arguments" <|
            \() ->
                """module A exposing (..)
a = (f >> g) b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of >>"
                            , details =
                                [ "Mixing chains of functions with >> in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: data |> (fn1 >> fn2)
After:  data |> fn1 |> fn2"""
                                ]
                            , under = ">>"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (g <| f b) c
"""
                        ]
        , test "should convert application from >> to parenthesized <| to avoid precedence issues" <|
            \() ->
                """module A exposing (..)
a = (f >> g) b == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of >>"
                            , details =
                                [ "Mixing chains of functions with >> in a direct function call with arguments positioned at the other end is confusing."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: data |> (fn1 >> fn2)
After:  data |> fn1 |> fn2"""
                                ]
                            , under = ">>"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (g <| f b) == 0
"""
                        ]
        , test "should convert from << to <| when used in a plain application call" <|
            \() ->
                """module A exposing (..)
a = (g << f) b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of <<"
                            , details =
                                [ "Because of the precedence of operators, using << at this location is the same as using <|."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: (fn3 << fn2) <| fn1 <| data
After:   fn3 <| fn2  <| fn1 <| data"""
                                ]
                            , under = "<<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (g <| f b)
"""
                        ]
        , test "should convert application from << to parenthesized <| to avoid precedence issues" <|
            \() ->
                """module A exposing (..)
a = (g << f) b == 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of <<"
                            , details =
                                [ "Because of the precedence of operators, using << at this location is the same as using <|."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: (fn3 << fn2) <| fn1 <| data
After:   fn3 <| fn2  <| fn1 <| data"""
                                ]
                            , under = "<<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (g <| f b) == 0
"""
                        ]
        , test "should convert from << to <| when used in a plain application call, and add parens if there are more arguments" <|
            \() ->
                """module A exposing (..)
a = (g << f) b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use <| instead of <<"
                            , details =
                                [ "Because of the precedence of operators, using << at this location is the same as using <|."
                                , "To make it more idiomatic in Elm and generally easier to read, please use <| instead. You may need to remove some parentheses to do this."
                                , """Here is an example:
Before: (fn3 << fn2) <| fn1 <| data
After:   fn3 <| fn2  <| fn1 <| data"""
                                ]
                            , under = "<<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (g <| f b) c
"""
                        ]
        ]
