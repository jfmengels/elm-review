module Text exposing
    ( Text
    , from
    , inBlue, inRed
    , join
    , view
    )

{-| Represents text with some styling applied to it.

    text : List Text
    text =
        [ Text.from "My name is "
        , Text.from "John"
            |> Text.withColor
        , Text.from "."
        ]


# Definition

@docs Text


# Constructors

@docs from


# Modifiers

@docs inBlue, inRed


# Working with lists

@docs join


# ACCESS

@docs length


# Encoding

@docs encode

-}

import Html exposing (Html)
import Html.Attributes as Attr



-- DEFINITION


{-| Represents text with some styling applied to it.
-}
type Text
    = Text
        { str : String
        , color : Maybe ( Int, Int, Int )
        }



-- CONSTRUCTORS


{-| Create an unstyled `Text` from a string.
-}
from : String -> Text
from value =
    Text
        { str = value
        , color = Nothing
        }



-- MODIFIERS


inBlue : Text -> Text
inBlue (Text text) =
    Text { text | color = Just ( 51, 187, 200 ) }


inRed : Text -> Text
inRed (Text text) =
    Text { text | color = Just ( 255, 0, 0 ) }



-- WORKING WITH LISTS


join : String -> List (List Text) -> List Text
join sep chunks =
    List.intersperse [ from sep ] chunks
        |> List.concatMap identity



-- VIEW


view : List Text -> Html msg
view texts =
    Html.div
        []
        (List.map viewPart texts)


viewPart : Text -> Html msg
viewPart (Text text) =
    Html.span
        [ case text.color of
            Just ( red, green, blue ) ->
                Attr.style "color" <| "rgb(" ++ String.fromInt red ++ "," ++ String.fromInt green ++ "," ++ String.fromInt blue ++ ")"

            Nothing ->
                Attr.classList []
        ]
        (text.str
            |> String.lines
            |> List.map Html.text
            |> List.intersperse (Html.br [] [])
        )
