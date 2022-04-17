module NoRecursiveUpdateTest exposing (all)

import NoRecursiveUpdate exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoRecursiveUpdate"
        [ test "should report when an update function calls itself" <|
            \_ ->
                """
module A exposing (..)
update msg model =
  case msg of
    Foo ->
      { model | foo = True }
    Bar ->
      update Foo { model | bar = True }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`update` shouldn't call itself"
                            , details = [ "If you wish to have the same behavior for different messages, move that behavior into a new function and have it called in the handling of both messages." ]
                            , under = "update"
                            }
                            |> Review.Test.atExactly { start = { row = 8, column = 7 }, end = { row = 8, column = 13 } }
                        ]
        , test "should not report other recursive functions" <|
            \_ ->
                """
module A exposing (..)
recursive list =
   case list of
     [] -> Nothing
     _ :: xs -> recursive xs
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report calls to update functions from other modules" <|
            \_ ->
                """
module A exposing (..)
update msg model =
  case msg of
    Bar subMsg ->
      Bar.update subMsg model.bar
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report references to update functions outside the update functions" <|
            \_ ->
                """
module A exposing (..)

main = Browser.sandbox { init = init, update = update, view = view }

update msg model =
  case msg of
    Bar ->
      1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
