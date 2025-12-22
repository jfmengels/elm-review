module Simplify.DuplicatedIfsTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Duplicated if conditions"
        [ test "should not remove nested conditions if they're not duplicate" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if y then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should remove duplicate nested conditions (x inside the then)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    1
  else
    3
"""
                        ]
        , test "should remove duplicate nested conditions (x inside the then, with parens)" <|
            \() ->
                """module A exposing (..)
a =
  if (x) then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if (x) then
    1
  else
    3
"""
                        ]
        , test "should remove duplicate nested conditions (not x inside the top condition)" <|
            \() ->
                """module A exposing (..)
a =
  if not x then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if not x then
    2
  else
    3
"""
                        ]
        , test "should remove duplicate nested conditions (not <| x inside the top condition)" <|
            \() ->
                """module A exposing (..)
a =
  if not <| x then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if not <| x then
    2
  else
    3
"""
                        ]
        , test "should remove opposite nested conditions (not x inside the then)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if not x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be True can be replaced by False"
                            , details = [ "You can replace this call by False." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    if False then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove duplicate nested conditions (x inside the else)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    1
  else
    if x then
      2
    else
      3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    1
  else
    3
"""
                        ]
        , test "should remove opposite nested conditions (not x inside the else)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    1
  else
    if not x then
      2
    else
      3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be False can be replaced by True"
                            , details = [ "You can replace this call by True." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    1
  else
    if True then
      2
    else
      3
"""
                        ]
        , test "should remove duplicate nested conditions (x part of a nested condition)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if x && y then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary && True"
                            , details = [ "You can replace this operation by the right bool." ]
                            , under = "x &&"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    if y then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove duplicate deeply nested conditions" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if y then
      if x then
        1
      else
        2
    else
      3
  else
    4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    if y then
      1
    else
      3
  else
    4
"""
                        ]
        , test "should remove duplicate nested conditions (x part of the top && condition)" <|
            \() ->
                """module A exposing (..)
a =
  if x && y then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x && y then
    1
  else
    3
"""
                        ]
        , test "should remove opposite nested conditions (x part of the top || condition)" <|
            \() ->
                """module A exposing (..)
a =
  if x || y then
    1
  else
    if x then
      2
    else
      3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x || y then
    1
  else
    3
"""
                        ]
        , test "should not remove condition when we don't have enough information (x part of the top && condition, other condition in the else)" <|
            \() ->
                """module A exposing (..)
a =
  if x && y then
    1
  else
    if x then
      2
    else
      3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not remove condition when we don't have enough information (x part of the top || condition, other condition in the then)" <|
            \() ->
                """module A exposing (..)
a =
  if x || y then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should remove branches where the condition always matches" <|
            \() ->
                """module A exposing (..)
a =
  if x == 1 then
    if x == 1 then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "x == 1"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 8 }, end = { row = 4, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x == 1 then
    if True then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (strings == with different values)" <|
            \() ->
                """module A exposing (..)
a =
  if x == "a" then
    if x == "b" then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "x == \"b\""
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 8 }, end = { row = 4, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x == "a" then
    if False then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (not function or value)" <|
            \() ->
                """module A exposing (..)
a =
  if item.name == "Aged Brie" then
    if item.name == "Sulfuras, Hand of Ragnaros" then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "item.name == \"Sulfuras, Hand of Ragnaros\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if item.name == "Aged Brie" then
    if False then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches" <|
            \() ->
                """module A exposing (..)
a =
  if x == 1 then
    if x == 2 then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "x == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x == 1 then
    if False then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (strings)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= "a" then
    if x == "a" then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= "a" then
    2
  else
    3
"""
                        ]
        , test "should not spread inferred things from one branch to another" <|
            \() ->
                """module A exposing (..)
a =
  if x == 1 then
    1
  else if x == 2 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should remove branches where the condition always matches (/=)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= 1 then
    if x /= 1 then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= 1 then
    1
  else
    3
"""
                        ]
        , test "should remove branches where the condition always matches (/= in else)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= 1 then
    1
  else if x /= 1 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(/=) comparison will result in False"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by False." ]
                            , under = "x /= 1"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 11 }, end = { row = 5, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= 1 then
    1
  else if False then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition always matches (== in else)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= 1 then
    1
  else if x == 1 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "x == 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= 1 then
    1
  else if True then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition always matches (/= <then> == in else)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= 1 then
    if x == 1 then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= 1 then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (literal on the left using ==, second if)" <|
            \() ->
                """module A exposing (..)
a =
  if x == 1 then
    1
  else if 1 == x then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x == 1 then
    1
  else 3
"""
                        ]
        , test "should remove branches where the condition never matches (literal on the left using ==, first if)" <|
            \() ->
                """module A exposing (..)
a =
  if 1 == x then
    1
  else if x == 1 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if 1 == x then
    1
  else 3
"""
                        ]
        , test "should remove branches where the condition always matches (literal on the left using /=)" <|
            \() ->
                """module A exposing (..)
a =
  if 1 /= x then
    1
  else if x == 1 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = [ "Based on the values and/or the context, we can determine the result. You can replace this operation by True." ]
                            , under = "x == 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if 1 /= x then
    1
  else if True then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (&&)" <|
            \() ->
                """module A exposing (..)
a =
  if a && b then
    1
  else if a && b then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a && b then
    1
  else 3
"""
                        ]
        , test "should remove branches where the condition may not match (a || b --> a)" <|
            \() ->
                """module A exposing (..)
a =
  if a || b then
    1
  else if a then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    1
  else 3
"""
                        ]
        , test "should remove branches where the condition may not match (a || b --> a && b)" <|
            \() ->
                """module A exposing (..)
a =
  if a || b then
    1
  else if a && b then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(&&) with any side being False will result in False"
                            , details =
                                [ "You can replace this operation by False."
                                , "Maybe you have hardcoded a value or mistyped a condition?"
                                ]
                            , under = "a && b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    1
  else if a then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition may not match (a && b --> a --> b)" <|
            \() ->
                """module A exposing (..)
a =
  if a && b then
    1
  else if a then
    if b then
      2
    else
      3
  else
    4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a && b then
    1
  else if a then
    3
  else
    4
"""
                        ]
        , test "should remove branches where the condition may not match (a || b --> <then> a --> b)" <|
            \() ->
                """module A exposing (..)
a =
  if a || b then
    if not a then
      if b then
        1
      else
        2
    else
      3
  else
    4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    if not a then
      1
    else
      3
  else
    4
"""
                        ]
        , test "should remove branches where the condition may not match (a || b --> not a)" <|
            \() ->
                """module A exposing (..)
a =
  if a || b then
    1
  else if not a then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be False can be replaced by True"
                            , details = [ "You can replace this call by True." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    1
  else if True then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition may not match (not (a || b) --> not a --> not b)" <|
            \() ->
                -- TODO Probably best to normalize inside Evaluate.getBoolean?
                """module A exposing (..)
a =
  if not (a || b) then
    1
  else if not a then
    if b then
      2
    else
      3
  else
    4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if not (a || b) then
    1
  else if not a then
    2
  else
    4
"""
                        ]

        --        ,   test "should not lose information as more conditions add up" <|
        --                \() ->
        --                    """module A exposing (..)
        --a =
        --  if a == 1 then
        --    if a == f b then
        --      if a == 1 then
        --        1
        --      else
        --        2
        --    else
        --      3
        --  else
        --    4
        --"""
        --                        |> Review.Test.run ruleWithDefaults
        --                        |> Review.Test.expectErrors
        --                            [ Review.Test.error
        --                                { message = "The condition will always evaluate to True"
        --                                , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
        --                                , under = "if"
        --                                }
        --                                |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
        --                                |> Review.Test.whenFixed """module A exposing (..)
        --a =
        --  if a == 1 then
        --    if a == 1 then
        --        1
        --      else
        --        2
        --  else
        --    4
        --"""
        --                            , Review.Test.error
        --                                { message = "The condition will always evaluate to True"
        --                                , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
        --                                , under = "if"
        --                                }
        --                                |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 9 } }
        --                                |> Review.Test.whenFixed """module A exposing (..)
        --a =
        --  if a == 1 then
        --    if a /= 2 then
        --      1
        --    else
        --      3
        --  else
        --    4
        --"""
        --                            ]
        -- TODO
        -- Unhappy && and || cases:
        --   if a && b then ... else <not a || not b>
        --   if a || b then ... else <not a && not b>
        ]
