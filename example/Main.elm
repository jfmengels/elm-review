module Main exposing (main)

import Result
import Ast
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Html exposing (Html, p, div, li, ul, pre, textarea, text)
import Html.Attributes exposing (id)
import Html.Events exposing (..)
import Json.Decode as JD
import Lint


-- Rules

import FindNoAnnotatedFunction
import NoDebug
import NoExposingEverything


type Msg
    = Replace String


init : String
init =
    """module Main exposing (..)

f : Int -> Int
f x = x Debug.log 1

a : a -> a
a = Debug.log "foo" x

h = f << Debug.log
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


tree2 : ( Result (List String) (List Statement), a ) -> Html Msg
tree2 ast =
    case ast of
        ( Ok statements, _ ) ->
            ul [] (List.map statement statements)

        err ->
            div [] [ text <| toString err ]


tree : Result a ( b, c, List Statement ) -> Html Msg
tree ast =
    case ast of
        Ok ( _, _, statements ) ->
            ul [] (List.map statement statements)

        err ->
            div [] [ text <| toString err ]


lint : Result a ( b, c, List Statement ) -> Html Msg
lint ast =
    let
        statements =
            ast
                |> Result.map (\( _, _, statements ) -> statements)
                |> Result.withDefault []

        lint =
            Lint.lint statements

        errors =
            List.concat
                [ lint FindNoAnnotatedFunction.rule
                , lint NoDebug.rule
                , lint NoExposingEverything.rule
                ]
    in
        div [] (List.map (\x -> p [] [ text x ]) errors)


view : String -> Html Msg
view model =
    let
        ast =
            Ast.parse model
    in
        div [ id "wrapper" ]
            [ textarea
                [ id "left"
                , on "input" (JD.map Replace targetValue)
                ]
                [ text model ]
            , div [ id "right" ]
                [ p [ id "ast" ] [ tree ast ]
                , p [ id "lint" ] [ lint ast ]
                ]
            ]


main : Program Never String Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
