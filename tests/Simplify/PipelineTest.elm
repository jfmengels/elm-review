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
        ]
