module NoUnoptimizedRecursionTest exposing (all)

import NoUnoptimizedRecursion exposing (optInWithComment, optOutWithComment, rule)
import Review.Test
import Test exposing (Test, describe, test)


message : String
message =
    "This function call cannot be tail-call optimized"


all : Test
all =
    describe "NoUnoptimizedRecursion"
        [ test "should not report non-recursive functions" <|
            \() ->
                """module A exposing (..)
a = 1
fun x = a + x
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report self-referential values without arguments" <|
            \() ->
                """module A exposing (..)
commentDecoder =
  map2 Comment
    (field "message" string)
    (field "responses" (map Responses (list (lazy (\\_ -> commentDecoder)))))
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report self-referential let values without arguments" <|
            \() ->
                """module A exposing (..)
a =
  let
    commentDecoder =
      map2 Comment
        (field "message" string)
        (field "responses" (map Responses (list (lazy (\\_ -> commentDecoder)))))
  in
  commentDecoder
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should report an error when a function is recursive but applies operations on the result" <|
            \() ->
                """module A exposing (..)
fun x =
  fun x + 1
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are applying operations on the result of recursive call. The recursive call should be the last thing to happen in this branch."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 3 }, end = { row = 3, column = 6 } }
                        ]
        , test "should not report an error when a function is properly TCO (if then branch)" <|
            \() ->
                """module A exposing (..)
fun x =
  if condition x then
    fun (x - 1)
  else
    x
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error when a function is properly TCO (if else branch)" <|
            \() ->
                """module A exposing (..)
fun x =
  if condition x then
    x
  else
    fun (x - 1)
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should report recursive call in the arguments of a function call" <|
            \() ->
                """module A exposing (..)
fun x =
  other (fun (x - 1))
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are applying operations on the result of recursive call. The recursive call should be the last thing to happen in this branch."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 10 }, end = { row = 3, column = 13 } }
                        ]
        , test "should report recursive call in the condition of an if block" <|
            \() ->
                """module A exposing (..)
fun x =
  if fun (x - 1) then
    1
  else
    x
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, the recursive call should not appear inside an if condition."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 6 }, end = { row = 3, column = 9 } }
                        ]
        , test "should report recursive call in a negation operation" <|
            \() ->
                """module A exposing (..)
fun x =
  -(fun (x - 1))
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are applying operations on the result of recursive call. The recursive call should be the last thing to happen in this branch."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                        ]
        , test "should report recursive call in a record access operation" <|
            \() ->
                """module A exposing (..)
fun x =
  (fun (x - 1)).field
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are accessing a field on the result of recursive call. The recursive call should be the last thing to happen in this branch."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 4 }, end = { row = 3, column = 7 } }
                        ]
        , test "should report recursive call from inside a tuple" <|
            \() ->
                """module A exposing (..)
fun x =
  ( fun (x - 1), 1 )
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are storing the result of recursive call inside a tuple. The recursive call should be the last thing to happen in this branch."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                        ]
        , test "should report recursive call from inside a list" <|
            \() ->
                """module A exposing (..)
fun x =
  [ fun (x - 1), 1 ]
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are storing the result of recursive call inside a list. The recursive call should be the last thing to happen in this branch."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                        ]
        , test "should report recursive call from inside a record expression" <|
            \() ->
                """module A exposing (..)
fun x =
  { result = fun (x - 1) }
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are storing the result of recursive call inside a record. The recursive call should be the last thing to happen in this branch."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 14 }, end = { row = 3, column = 17 } }
                        ]
        , test "should report recursive call from inside a record update expression" <|
            \() ->
                """module A exposing (..)
fun x =
  { y | result = fun (x - 1) }
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are storing the result of recursive call inside a record. The recursive call should be the last thing to happen in this branch."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 18 }, end = { row = 3, column = 21 } }
                        ]
        , test "should report recursive call in the case of pattern to evaluate" <|
            \() ->
                """module A exposing (..)
fun x =
  case fun (x - 1) of
    _ -> 1
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, the recursive call should not appear in the pattern to evaluate for a case expression."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 11 } }
                        ]
        , test "should not report an error when a function is properly TCO (case branch)" <|
            \() ->
                """module A exposing (..)
fun x =
  case x of
    _ -> fun (x - 1)
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error when a function is properly TCO (let body)" <|
            \() ->
                """module A exposing (..)
fun x =
  let
    y = x - 1
  in
  fun y
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should report an error when a function is called recursively from inside one of its let functions" <|
            \() ->
                """module A exposing (..)
fun x =
  let
    fun2 y =
      fun x
  in
  fun2 x
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, the recursive call should not appear inside a let declaration."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 10 } }
                        ]
        , test "should not report an error when a function is properly TCO (parentheses)" <|
            \() ->
                """module A exposing (..)
fun x =
  (fun (x - 1))
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should report recursive call in a lambda" <|
            \() ->
                """module A exposing (..)
fun n =
    \\x -> (fun n x)
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, the recursive call should not appear inside an anonymous function."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 15 } }
                        ]
        , test "should report recursive call using |>" <|
            \() ->
                """module A exposing (..)
fun x n =
    if x <= 0 then
        n

    else
        n
            |> fun (x - 1)
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are applying operations on the result of recursive call. The recursive call should be the last thing to happen in this branch."
                                , "Removing the usage of `|>` may fix the issue here."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 8, column = 16 }, end = { row = 8, column = 19 } }
                        ]
        , test "should report recursive call using |> (simple reference)" <|
            \() ->
                """module A exposing (..)
fun x =
    n
        |> fun
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are applying operations on the result of recursive call. The recursive call should be the last thing to happen in this branch."
                                , "Removing the usage of `|>` may fix the issue here."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 12 }, end = { row = 4, column = 15 } }
                        ]
        , test "should report recursive call using <|" <|
            \() ->
                """module A exposing (..)
fun x n =
    if x <= 0 then
        n

    else
        fun (x - 1) <| n
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are applying operations on the result of recursive call. The recursive call should be the last thing to happen in this branch."
                                , "Removing the usage of `<|` may fix the issue here."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 9 }, end = { row = 7, column = 12 } }
                        ]
        , test "should report an error for non-TCO let functions" <|
            \() ->
                """module A exposing (..)
a n =
  let
    fun x =
      fun x + 1
  in
  fun 2
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are applying operations on the result of recursive call. The recursive call should be the last thing to happen in this branch."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 10 } }
                        ]
        , test "should not report an error for TCO let functions" <|
            \() ->
                """module A exposing (..)
a n =
  let
    fun x =
      fun x
  in
  fun 2
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error for TCO let functions inside a recursive function" <|
            \() ->
                """module A exposing (..)
fun1 n =
  let
    fun2 x =
      fun2 x
  in
  if cond then
    fun1 n
  else
    fun2 2
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the function body contains has the exact opt out comment" <|
            \() ->
                """module A exposing (..)
fun x =
  -- OPT OUT
  fun x + 1
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the function body contains the opt out comment" <|
            \() ->
                """module A exposing (..)
fun x =
  -- SOME TAG, OPT OUT, OTHER TAG
  fun x + 1
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the function body contains the opt out comment when it has a signature" <|
            \() ->
                """module A exposing (..)
fun : Int -> Int
fun x =
  -- OPT OUT
  fun x + 1
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error for let functions when the function body contains the opt out comment" <|
            \() ->
                """module A exposing (..)
a n =
  let
    fun x =
      -- OPT OUT
      fun x + 1
  in
  fun x
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should report an error when the function body contains the opt in comment" <|
            \() ->
                """module A exposing (..)
fun x =
  -- OPT IN
  fun x + 1
"""
                    |> Review.Test.run (rule (optInWithComment "OPT IN"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are applying operations on the result of recursive call. The recursive call should be the last thing to happen in this branch."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 3 }, end = { row = 4, column = 6 } }
                        ]
        , test "should report an error for let functions when the function body contains the opt in comment" <|
            \() ->
                """module A exposing (..)
a n =
  let
    fun x =
      -- OPT IN
      fun x + 1
  in
  fun x
"""
                    |> Review.Test.run (rule (optInWithComment "OPT IN"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details =
                                [ "Among other possible reasons, you are applying operations on the result of recursive call. The recursive call should be the last thing to happen in this branch."
                                , "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail"
                                ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 7 }, end = { row = 6, column = 10 } }
                        ]
        , test "should not report an error when the function body does not contain the opt in comment" <|
            \() ->
                """module A exposing (..)
fun x =
  fun x + 1
"""
                    |> Review.Test.run (rule (optInWithComment "OPT IN"))
                    |> Review.Test.expectNoErrors
        ]
