module NoExposingEverythingTest exposing (all)

import NoExposingEverything exposing (rule)
import Review.Test
import Test exposing (..)


all : Test
all =
    describe "NoExposingEverything"
        [ test "should not when a module exposes a limited set of things" <|
            \_ ->
                "module A exposing (B(..), C, d)"
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when a module exposes everything" <|
            \_ ->
                "module A exposing (..)"
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module exposes everything implicitly \"(..)\""
                            , details =
                                [ "Modules should have hidden implementation details with an explicit API so that the module is used in a proper and controlled way. The users of this module should not have to know about what is inside a module it is using, and they shouldn't need to access it's internal details. Therefore, the API should be explicitly defined and ideally as small as possible."
                                ]
                            , under = "(..)"
                            }
                        ]
        ]
