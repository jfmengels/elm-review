module Simplify.CaseOfTest exposing (all)

import Review.Test
import Simplify
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Case of"
        [ regularCaseOfTests
        , booleanCaseOfTests
        , caseOfWithUnnecessaryCasesTests
        ]


regularCaseOfTests : Test
regularCaseOfTests =
    describe "Regular case of"
        [ test "should not report case of when the body of the branches are different" <|
            \() ->
                """module A exposing (..)
a = case value of
      A -> 1
      B -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace case of with a single wildcard case by the body of the case" <|
            \() ->
                """module A exposing (..)
a = case value of
      _ -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not replace case of with a single case by the body of the case" <|
            \() ->
                """module A exposing (..)
type B = C
a = case value of
      C -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors

        -- TODO Create a project with a union with a single constructor
        --        , test "should not replace case of with a single case when the constructor is ignored" <|
        --            \() ->
        --                """module A exposing (..)
        --type B = C
        --a = case value of
        --      C -> x
        --"""
        --                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "A.B" ] <| defaults)
        --                    |> Review.Test.expectNoErrors
        , test "should replace case of with multiple cases that have the same body" <|
            \() ->
                """module A exposing (..)
a = case value of
      Just _ -> x
      Nothing -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not replace case of with a single case when the constructor from a dependency is ignored" <|
            \() ->
                """module A exposing (..)
a = case value of
      Just _ -> x
      Nothing -> x
"""
                    |> Review.Test.run (Simplify.rule <| Simplify.ignoreCaseOfForTypes [ "Maybe.Maybe" ] <| Simplify.defaults)
                    |> Review.Test.expectNoErrors
        , test "should not replace case of with multiple cases when all constructors of ignored type are used" <|
            \() ->
                """module A exposing (..)
a = case value of
      Just _ -> x
      Nothing -> x
"""
                    |> Review.Test.run (Simplify.rule <| Simplify.ignoreCaseOfForTypes [ "Maybe.Maybe" ] <| Simplify.defaults)
                    |> Review.Test.expectNoErrors
        , test "should replace case of with multiple cases when not all constructors of ignored type are used" <|
            \() ->
                """module A exposing (..)
a = case value of
      Just _ -> x
      _ -> x
"""
                    |> Review.Test.run (Simplify.rule <| Simplify.ignoreCaseOfForTypes [ "Maybe.Maybe" ] <| Simplify.defaults)
                    |> Review.Test.expectNoErrors
        , test "should not replace case of with a single case with ignored arguments by the body of the case" <|
            \() ->
                """module A exposing (..)
a = case value of
      A (_) (B C) -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace case of where a pattern introduces a variable" <|
            \() ->
                """module A exposing (..)
a = case value of
      A (_) (B c) -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace boolean case of with the same body by that body" <|
            \() ->
                """module A exposing (..)
a = case value of
      True -> x
      False -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace case expression that destructures a tuple by a let declaration" <|
            \() ->
                """module A exposing (..)
a =
    case value of
        ( x, y ) ->
            1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use a let expression to destructure data"
                            , details = [ "It is more idiomatic in Elm to use a let expression to define a new variable rather than to use pattern matching. This will also make the code less indented, therefore easier to read." ]
                            , under = "( x, y )"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let ( x, y ) = value
    in
            1
"""
                        ]
        , test "should replace case expression that destructures a record by a let declaration" <|
            \() ->
                """module A exposing (..)
a =
    case value of
        { x, y } ->
            1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use a let expression to destructure data"
                            , details = [ "It is more idiomatic in Elm to use a let expression to define a new variable rather than to use pattern matching. This will also make the code less indented, therefore easier to read." ]
                            , under = "{ x, y }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let { x, y } = value
    in
            1
"""
                        ]
        , test "should replace case expression that destructures a variable by a let declaration" <|
            \() ->
                """module A exposing (..)
a =
    case value of
        var ->
            1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use a let expression to destructure data"
                            , details = [ "It is more idiomatic in Elm to use a let expression to define a new variable rather than to use pattern matching. This will also make the code less indented, therefore easier to read." ]
                            , under = "var"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let var = value
    in
            1
"""
                        ]
        ]


caseOfWithUnnecessaryCasesTests : Test
caseOfWithUnnecessaryCasesTests =
    describe "unnecessary cases"
        [ test "should remove unnecessary case of dependency variant when all cases are variant patterns" <|
            \() ->
                """module A exposing (..)
a =
    case Ok value of
        Err _ ->
            0

        Ok True ->
            1
        
        Ok False ->
            2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unreachable case branches"
                            , details = [ "The value between case ... of is a known Ok variant. However, the 1st case matches on a different variant which means you can remove it." ]
                            , under = "Err _"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    case value of

        True ->
            1
        
        False ->
            2
"""
                        ]
        , test "should remove multiple unnecessary cases of module-local variant with multiple attachments when all cases are variant patterns" <|
            \() ->
                """module A exposing (..)
a =
    case A b c d of
        B False ->
            1
        
        C ->
            0
        
        B True ->
            2

        A True _ _ ->
            3
        
        A _ False _ ->
            4

type AOrB
  = A Bool Bool Bool
  | B Bool
  | C
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unreachable case branches"
                            , details = [ "The value between case ... of is a known A variant. However, the 1st and 2nd and 3rd case matches on a different variant which means you can remove it." ]
                            , under = "B False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    case (b, (c, d)) of

        (True, (_, _)) ->
            3
        
        (_, (False, _)) ->
            4

type AOrB
  = A Bool Bool Bool
  | B Bool
  | C
"""
                        ]
        , test "should report case of on variant from single-variant type with variant argument" <|
            \() ->
                """module A exposing (..)
a =
    case Toop4 (Just a0) a1 a2 a3 of
        Toop4 (Just 0) _ _ _ ->
            0
        
        Toop4 (Just _) _ _ _ ->
            1
        
        Toop4 Nothing _ _ _ ->
            2

type Toop4 a0 a1 a2 a3
  = Toop4 a0 a1 a2 a3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unreachable case branches"
                            , details = [ "The value between case ... of is a known Just variant. However, the 3rd case matches on a different variant which means you can remove it." ]
                            , under = "Nothing"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    case (a0, (a1, (a2, a3))) of
        (0, (_, (_, _))) ->
            0
        
        (_, (_, (_, _))) ->
            1

type Toop4 a0 a1 a2 a3
  = Toop4 a0 a1 a2 a3
"""
                        ]
        , test "should remove multiple unnecessary cases of project-local variant with multiple attachments when all cases are variant patterns" <|
            \() ->
                [ """module A exposing (..)
import AOrB exposing (AOrB(..))

a =
    case A b c d of
        B False ->
            1
        
        C ->
            0
        
        B True ->
            2

        A True _ _ ->
            3
        
        A _ False _ ->
            4
"""
                , """module AOrB exposing (AOrB(..))

type AOrB
  = A Bool Bool Bool
  | B Bool
  | C
"""
                ]
                    |> Review.Test.runOnModules ruleWithDefaults
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Unreachable case branches"
                                , details = [ "The value between case ... of is a known AOrB.A variant. However, the 1st and 2nd and 3rd case matches on a different variant which means you can remove it." ]
                                , under = "B False"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import AOrB exposing (AOrB(..))

a =
    case (b, (c, d)) of

        (True, (_, _)) ->
            3
        
        (_, (False, _)) ->
            4
"""
                            ]
                          )
                        ]
        , test "should remove unnecessary case of empty list when all cases are list patterns" <|
            \() ->
                """module A exposing (..)
a =
    case [] of
        _ :: _ :: _ ->
            0
        
        [ _ ] ->
            1

        _ :: _ ->
            2
        
        [] ->
            3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unreachable case branches"
                            , details =
                                [ "The value between case ... of is a known list of length 0. However, the 1st and 2nd and 3rd case matches on a list with a different length which means you can remove it."
                                , "Since only a single case branch will remain after removing the unreachable ones, you can replace this case-of by the result of the 4th case."
                                ]
                            , under = "_ :: _ :: _"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    3
"""
                        ]
        , test "should remove unnecessary case of filled list literal when all cases are list patterns" <|
            \() ->
                """module A exposing (..)
a =
    case [ 0, 0 ] of
        _ :: _ :: _ ->
            0
        
        [ _ ] ->
            1
        
        [ _, _ ] ->
            2
        
        [ _, _, _ ] ->
            3

        _ :: _ ->
            4
        
        [] ->
            5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unreachable case branches"
                            , details = [ "The value between case ... of is a known list of length 2. However, the 2nd and 4th and 6th case matches on a list with a different length which means you can remove it." ]
                            , under = "[ _ ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    case (0, [ 0 ]) of
        (_, _ :: _) ->
            0

        (_, [ _ ]) ->
            2

        (_, _) ->
            4
"""
                        ]
        , test "should remove unnecessary case of filled list literal when all cases are list patterns with length >= 3" <|
            \() ->
                """module A exposing (..)
a =
    case [ 0, 0, 0, 0 ] of
        [ _, _, _, _ ] ->
            0

        _ :: _ :: _ :: _ ->
            1
        
        [] ->
            2
        
        [ _ ] ->
            3
        
        [ _, _ ] ->
            4
        
        [ _, _, _ ] ->
            5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unreachable case branches"
                            , details = [ "The value between case ... of is a known list of length 4. However, the 3rd and 4th and 5th and 6th case matches on a list with a different length which means you can remove it." ]
                            , under = "[]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    case (0, (0, (0, [ 0 ]))) of
        (_, (_, (_, [ _ ]))) ->
            0

        (_, (_, (_, _))) ->
            1
"""
                        ]
        , test "should remove unnecessary case of cons when all cases are list patterns" <|
            \() ->
                """module A exposing (..)
a =
    case 0 :: 0 :: tail of
        _ :: _ :: _ :: _ ->
            0
        
        [ _, _, _ ] ->
            1
        
        _ :: _ :: _ ->
            2
        
        [ _, _ ] ->
            3

        _ :: _ ->
            4
        
        [] ->
            5
        
        [ _ ] ->
            6
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unreachable case branches"
                            , details = [ "The value between case ... of is a list of length >= 2. However, the 6th and 7th case matches on a shorter list which means you can remove it." ]
                            , under = "[]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    case (0, 0 :: tail) of
        (_, _ :: _ :: _) ->
            0
        
        (_, [ _, _ ]) ->
            1
        
        (_, _ :: _) ->
            2
        
        (_, [ _ ]) ->
            3

        (_, _) ->
            4
"""
                        ]
        , test "should remove unnecessary case of dependency variant when all cases of nested tuple part are variant patterns" <|
            \() ->
                """module A exposing (..)
a =
    case ( b, ( c, d, Ok value ) ) of
        ( b_, ( c_, d_, Err _ ) ) ->
            0

        ( b_, ( c_, d_, Ok True ) ) ->
            1
        
        ( b_, ( c_, d_, Ok False ) ) ->
            2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unreachable case branches"
                            , details = [ "The value between case ... of is a known Ok variant. However, the 1st case matches on a different variant which means you can remove it." ]
                            , under = "Err _"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    case ( b, ( c, d, value ) ) of

        ( b_, ( c_, d_, True ) ) ->
            1
        
        ( b_, ( c_, d_, False ) ) ->
            2
"""
                        ]
        ]


booleanCaseOfMessage : String
booleanCaseOfMessage =
    "Replace `case..of` by an `if` condition"


booleanCaseOfDetails : List String
booleanCaseOfDetails =
    [ "The idiomatic way to check for a condition is to use an `if` expression."
    , "Read more about it at: https://guide.elm-lang.org/core_language.html#if-expressions"
    ]


booleanCaseOfTests : Test
booleanCaseOfTests =
    describe "Boolean case of"
        [ test "should not report pattern matches for non-boolean values" <|
            \() ->
                """module A exposing (..)
a = case thing of
      Thing -> 1
      Bar -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report pattern matches when the evaluated expression is a tuple of with a boolean" <|
            \() ->
                """module A exposing (..)
a = case ( bool1, bool2 ) of
      ( True, True ) -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report pattern matches when one of the patterns is a bool constructor (True and False)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      True -> 1
      False -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if bool then 1
      else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (on multiple lines)" <|
            \() ->
                """module A exposing (..)
a =
    case bool of
        True ->
            1
        False ->
            2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    if bool then 1
        else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (False and True)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      False -> 1
      True -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if not (bool) then 1
      else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (True and wildcard)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      True -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if bool then 1
      else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (False and wildcard)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      False -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if not (bool) then 1
      else 2
"""
                        ]
        , test "should report pattern matches for booleans even when one of the patterns starts with `Basics.`" <|
            \() ->
                """module A exposing (..)
a = case bool of
      Basics.True -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "Basics.True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if bool then 1
      else 2
"""
                        ]
        , test "should report pattern matches for booleans even when the constructor seems to be for booleans but comes from an unknown module" <|
            \() ->
                """module A exposing (..)
a = case bool of
      OtherModule.True -> 1
      _ -> 2

b = case bool of
      OtherModule.False -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]
