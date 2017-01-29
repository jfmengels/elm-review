module Main exposing (main)

import Result
import Ast
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Html exposing (Html, p, div, li, ul, pre, textarea, text)
import Html.Attributes exposing (id, class)
import Html.Events exposing (..)
import Json.Decode as JD
import Lint exposing (lintSource)
import Lint.Types
import Regex exposing (regex, escape)


-- Rules

import Lint.Rules.DefaultPatternPosition
import Lint.Rules.NoConstantCondition
import Lint.Rules.NoDebug
import Lint.Rules.NoDuplicateImports
import Lint.Rules.NoExposingEverything
import Lint.Rules.NoImportingEverything
import Lint.Rules.NoNestedLet
import Lint.Rules.NoUnannotatedFunction
import Lint.Rules.NoUnusedVariables
import Lint.Rules.NoUselessIf
import Lint.Rules.NoUselessPatternMatching
import Lint.Rules.NoWarningComments
import Lint.Rules.SimplifyPiping


type Msg
    = Replace String


rules : List (String -> List Lint.Types.Error)
rules =
    [ Lint.Rules.DefaultPatternPosition.rule { position = Lint.Rules.DefaultPatternPosition.Last }
    , Lint.Rules.NoConstantCondition.rule
    , Lint.Rules.NoDebug.rule
    , Lint.Rules.NoDuplicateImports.rule
    , Lint.Rules.NoExposingEverything.rule
    , Lint.Rules.NoImportingEverything.rule
    , Lint.Rules.NoNestedLet.rule
    , Lint.Rules.NoUnannotatedFunction.rule
    , Lint.Rules.NoUnusedVariables.rule
    , Lint.Rules.NoUselessIf.rule
    , Lint.Rules.NoUselessPatternMatching.rule
    , Lint.Rules.NoWarningComments.rule
    , Lint.Rules.SimplifyPiping.rule
    ]


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
    elementWithChildren (toString title) children


expression : Expression -> Html Msg
expression e =
    case e of
        List es ->
            withChild e (List.map expression es)

        Application e1 e2 ->
            displayElementAndExpressions e [ e1, e2 ]

        BinOp operator left right ->
            displayElementAndExpressions e [ operator, left, right ]

        e ->
            li [] [ pre [] [ text <| toString e ] ]


replace : String -> String -> String -> String
replace search substitution string =
    string
        |> Regex.replace (Regex.AtMost 1) (regex (escape search)) (\_ -> substitution)


removeSubElement : a -> List b -> String
removeSubElement parent children =
    List.foldl
        (\child res ->
            res
                |> replace ("(" ++ child ++ ")") ""
                |> replace child ""
        )
        (toString parent)
        (List.map toString children)


elementWithChildren : String -> List (Html msg) -> Html msg
elementWithChildren title children =
    li []
        [ pre [] [ text title ]
        , ul [] children
        ]


displayElementAndExpressions : a -> List Expression -> Html Msg
displayElementAndExpressions element expressions =
    elementWithChildren
        (removeSubElement element expressions)
        (List.map expression expressions)


displayElementAndAnyChild : a -> List b -> Html Msg
displayElementAndAnyChild element children =
    elementWithChildren
        (removeSubElement element children)
        [ li [] [ pre [] [ text <| String.join " " <| List.map toString children ] ] ]


statement : Statement -> Html Msg
statement s =
    let
        defaultDisplay =
            li [] [ pre [] [ text <| toString s ] ]
    in
        case s of
            FunctionDeclaration _ _ body ->
                displayElementAndExpressions s [ body ]

            FunctionTypeDeclaration _ body ->
                displayElementAndAnyChild s [ body ]

            ModuleDeclaration _ exportSet ->
                displayElementAndAnyChild s [ exportSet ]

            ImportStatement _ _ exportSet ->
                displayElementAndAnyChild s [ exportSet ]

            s ->
                defaultDisplay


tree : Result a ( b, c, List Statement ) -> Html Msg
tree ast =
    case ast of
        Ok ( _, _, statements ) ->
            ul [] (List.map statement statements)

        err ->
            div [] [ text "Sorry, I could not parse your code. This may be my fault though :/" ]


lint : String -> Html Msg
lint source =
    let
        errors =
            lintSource rules source

        messages =
            if List.isEmpty errors then
                [ "No errors." ]
            else
                errors
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
