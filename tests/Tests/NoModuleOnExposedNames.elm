module Tests.NoModuleOnExposedNames exposing (all)

import NoModuleOnExposedNames exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoModuleOnExposedNames"
        [ test "reports modules used on exposed values" <|
            \_ ->
                """
module Page exposing (view)
import Html.Attributes as Attr exposing (class)
view children =
    div [ Attr.class "container" ] children
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ moduleOnExposedValueError "Attr.class" "class"
                            |> Review.Test.whenFixed
                                """
module Page exposing (view)
import Html.Attributes as Attr exposing (class)
view children =
    div [ class "container" ] children
"""
                        ]
        , test "reports modules used on exposed types" <|
            \_ ->
                """
module Page exposing (view)
import Html exposing (Html, Attribute)
view : List (Html.Attribute msg) -> Html msg
view children =
    Html.div [] children
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ moduleOnExposedTypeError "Html.Attribute" "Attribute"
                            |> Review.Test.whenFixed
                                """
module Page exposing (view)
import Html exposing (Html, Attribute)
view : List (Attribute msg) -> Html msg
view children =
    Html.div [] children
"""
                        ]
        , test "does not report names not exposed" <|
            \_ ->
                """
module Page exposing (view)
import Html.Attributes as Attr
view children =
    div [ Attr.class "container" ] children
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


moduleOnExposedValueError : String -> String -> Review.Test.ExpectedError
moduleOnExposedValueError call name =
    Review.Test.error
        { message = "Module used on exposed value `" ++ name ++ "`."
        , details =
            [ "It is not necessary to use the module here as `" ++ name ++ "` was exposed on import."
            , "You should remove the module from this call, or remove the name from the import .. exposing list."
            ]
        , under = call
        }


moduleOnExposedTypeError : String -> String -> Review.Test.ExpectedError
moduleOnExposedTypeError call name =
    Review.Test.error
        { message = "Module used on exposed type `" ++ name ++ "`."
        , details =
            [ "It is not necessary to use the module here as `" ++ name ++ "` was exposed on import."
            , "You should remove the module from this call, or remove the name from the import .. exposing list."
            ]
        , under = call
        }
