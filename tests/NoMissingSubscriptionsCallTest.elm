module NoMissingSubscriptionsCallTest exposing (all)

import NoMissingSubscriptionsCall exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoMissingSubscriptionsCall"
        [ test "should report when a module defines when a module exposes a limited set of things" <|
            \_ ->
                [ """
module Main exposing (main)
import Imported

main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

update msg model =
  case msg of
    UsedMsg subMsg ->
      Imported.update subMsg model.used

subscriptions model =
  Sub.none
""", """
module Imported exposing (update, subscriptions)
update = Debug.todo ""
subscriptions = Debug.todo ""
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Main"
                          , [ Review.Test.error
                                { message = "Missing subscriptions call to Imported.subscriptions"
                                , details =
                                    [ "The Imported module defines a `subscriptions` function, which you are not using even though you are using its `update` function. This makes me think that you are not subscribing to all the things you should."
                                    ]
                                , under = "Imported.update"
                                }
                            ]
                          )
                        ]
        , test "should not report anything when the imported module's subscriptions function is used" <|
            \_ ->
                [ """
module Main exposing (main)
import Imported

main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

update msg model =
  case msg of
    UsedMsg subMsg ->
      Imported.update subMsg model.used

subscriptions model =
  Imported.subscriptions
""", """
module Imported exposing (update, subscriptions)
update = Debug.todo ""
subscriptions = Debug.todo ""
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should not report anything when the imported module does not define a subscriptions function" <|
            \_ ->
                [ """
module Main exposing (main)
import Imported

main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

update msg model =
  case msg of
    UsedMsg subMsg ->
      Imported.update subMsg model.used

subscriptions model =
  Sub.none
""", """
module Imported exposing (update)
update = Debug.todo ""
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        ]
