module NoSimpleLetBodyTest exposing (all)

import NoSimpleLetBody exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoSimpleLetBody"
        [ rootLevelTests
        , notRootLevelTests
        ]


rootLevelTests : Test
rootLevelTests =
    describe "At root level"
        [ test "should report an error when let body is a simple function or value" <|
            \() ->
                """module A exposing (..)
a = let b = 1
    in b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The referenced value should be inlined."
                            , details =
                                [ "The name of the value is redundant with the surrounding expression."
                                , "If you believe that the expression needs a name because it is too complex, consider splitting the expression up more or extracting it to a new function."
                                ]
                            , under = "b"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should report an error when let body is a simple function or value and is the last declaration" <|
            \() ->
                """module A exposing (..)
a = let
        c = 2
        b = c
    in b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The referenced value should be inlined."
                            , details =
                                [ "The name of the value is redundant with the surrounding expression."
                                , "If you believe that the expression needs a name because it is too complex, consider splitting the expression up more or extracting it to a new function."
                                ]
                            , under = "b"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = let
        c = 2
    in
    c
"""
                        ]
        , test "should report an error but not suggest a fix when value is not the last declaration" <|
            \() ->
                """module A exposing (..)
a = let
        b = c
        c = 2
    in b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The referenced value should be inlined."
                            , details =
                                [ "The name of the value is redundant with the surrounding expression."
                                , "If you believe that the expression needs a name because it is too complex, consider splitting the expression up more or extracting it to a new function."
                                ]
                            , under = "b"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 9 } }
                        ]
        , test "should report an error but not suggest a fix when value is not the last declaration (last is destructuring)" <|
            \() ->
                """module A exposing (..)
a = let
        b = c
        {c} = 2
    in b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The referenced value should be inlined."
                            , details =
                                [ "The name of the value is redundant with the surrounding expression."
                                , "If you believe that the expression needs a name because it is too complex, consider splitting the expression up more or extracting it to a new function."
                                ]
                            , under = "b"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 9 } }
                        ]
        , test "should report an error but not suggest a fix when value is a function that takes arguments" <|
            \() ->
                """module A exposing (..)
a = let
        c = 2
        b n = c
    in b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The referenced value should be inlined."
                            , details =
                                [ "The name of the value is redundant with the surrounding expression."
                                , "If you believe that the expression needs a name because it is too complex, consider splitting the expression up more or extracting it to a new function."
                                ]
                            , under = "b"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 9 } }
                        ]
        , test "should not report an error when let body is a function call" <|
            \() ->
                """module A exposing (..)
a = let b = 1
    in b ()
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error if the return value was not declared in the let" <|
            \() ->
                """module A exposing (..)
a = let b = 1
    in c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error if the return value was destructured in the let" <|
            \() ->
                """module A exposing (..)
a1 = let (b, _) = 1
     in b
a2 = let {b} = 1
     in b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error if the return value is a reference to another module's value with the same name as something declared" <|
            \() ->
                """module A exposing (..)
a = let b = 1
    in A.b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


notRootLevelTests : Test
notRootLevelTests =
    describe "Not at root-level"
        [ test "should report an error when let is deep in the function" <|
            \() ->
                """module A exposing (..)
a =
  if condition then
    let b = 1
    in b
  else
    2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The referenced value should be inlined."
                            , details =
                                [ "The name of the value is redundant with the surrounding expression."
                                , "If you believe that the expression needs a name because it is too complex, consider splitting the expression up more or extracting it to a new function."
                                ]
                            , under = "b"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if condition then
    1
  else
    2
"""
                        ]
        ]
