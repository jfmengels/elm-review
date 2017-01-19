module Main exposing (main)

import Result
import Ast
import Ast as L
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Html exposing (Html, p, div, li, ul, pre, textarea, text)
import Html.Attributes exposing (id, class)
import Html.Events exposing (..)
import Json.Decode as JD
import Types


-- Rules

import NoDebug
import NoDuplicateImports
import NoExposingEverything
import NoImportingEverything
import NoUnannotatedFunction
import NoUnusedVariables


type Msg
    = Replace String


init : String
init =
    """module Main exposing (f)

import Html
import Html exposing (..)

f : Int -> Int
f x = x Debug.log 1

g n = n + 1
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


tree : Result a ( b, c, List Statement ) -> Html Msg
tree ast =
    case ast of
        Ok ( _, _, statements ) ->
            ul [] (List.map statement statements)

        err ->
            div [] [ text <| toString err ]


rules : List (String -> List Types.Error)
rules =
    [ NoDebug.rule
    , NoDuplicateImports.rule
    , NoExposingEverything.rule
    , NoImportingEverything.rule
    , NoUnannotatedFunction.rule
    , NoUnusedVariables.rule
    ]


lint : String -> Html Msg
lint source =
    let
        errors =
            List.concatMap (\rule -> rule source) rules

        messages =
            if List.isEmpty errors then
                [ "No issues here." ]
            else
                List.map (\err -> err.rule ++ ": " ++ err.message) errors
    in
        div []
            (List.map
                (\message -> p [] [ text message ])
                messages
            )


view : String -> Html Msg
view model =
    div [ id "wrapper" ]
        [ div [ id "left" ]
            [ p [ class "title" ] [ text "Source code" ]
            , textarea
                [ id "input"
                , on "input" (JD.map Replace targetValue)
                ]
                [ text model ]
            , div []
                [ p [ class "title" ] [ text "Linting errors" ]
                , div [ id "lint" ]
                    [ lint model
                    ]
                ]
            ]
        , div [ id "right" ]
            [ p [ class "title" ] [ text "AST" ]
            , p [ id "ast" ] [ tree <| Ast.parse model ]
            ]
        ]


main : Program Never String Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
