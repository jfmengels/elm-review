module Main exposing (main)

import Ast
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Html exposing (..)
import Html
import Html.Events exposing (..)
import Json.Decode as JD


type Msg
    = Replace String


init : String
init =
    """module Main exposing (..)

f : Int -> Int
f x = x + 1

g : Int -> Int
g x = x * 2

h = f << g
"""


update : Msg -> String -> String
update action model =
    case action of
        Replace m ->
            m


withChild : a -> List (Html Msg) -> Html Msg
withChild title children =
    li []
        [ pre [] [ text <| toString title ]
        , ul [] children
        ]


expression : Expression -> Html Msg
expression e =
    case e of
        List es ->
            withChild e (List.map expression es)

        Application e1 e2 ->
            withChild e
                [ expression e1
                , expression e2
                ]

        e ->
            li [] [ pre [] [ text <| toString e ] ]


statement : Statement -> Html Msg
statement s =
    case s of
        FunctionDeclaration _ _ e ->
            withChild s [ expression e ]

        s ->
            li [] [ pre [] [ text <| toString s ] ]


tree : String -> Html Msg
tree m =
    case Ast.parse m of
        Ok ( _, _, statements ) ->
            ul [] (List.map statement statements)

        err ->
            div [] [ text <| toString err ]


view : String -> Html Msg
view model =
    div []
        [ textarea [ on "input" (JD.map Replace targetValue) ] [ text model ]
        , tree model
        ]


main : Program Never String Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
