module NoNoOpMsgTest exposing (all)

import NoNoOpMsg exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


message : String
message =
    "Don't use NoOp, give it a better name"


details : List String
details =
    [ "A Msg name should explain what happened. NoOp means that nothing happened."
    , "Even if you don't care about handling the event, give it a name that describes what happened."
    , "@noahzgordon's talk on it: https://www.youtube.com/watch?v=w6OVDBqergc"
    ]


all : Test
all =
    describe "NoNoOpMsg"
        [ test "should report an error when there is a NoOp Msg constructor" <|
            \() ->
                """module A exposing (..)
type Msg
  = NoOp
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "NoOp"
                            }
                        ]
        , test "should report an error when there is a Noop Msg constructor" <|
            \() ->
                """module A exposing (..)
type Msg
 = Noop
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Noop"
                            }
                        ]
        , test "should not report an error when there is no custom type" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the constructor name is not NoOp" <|
            \() ->
                """module A exposing (..)
type Msg
  = Foo
  | Bar
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when we see NoOp in pattern matching" <|
            \() ->
                """module A exposing (..)
update : Msg -> Model -> Model
update msg model =
  case msg of
     NoOp -> model
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
