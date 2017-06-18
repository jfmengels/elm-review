module Main exposing (main)

import Ast
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Combine
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (on, targetValue)
import Json.Decode as JD
import Lint exposing (lintSource)
import Lint.Types exposing (LintRule, Severity(..))
import Regex exposing (escape, regex)
import Result


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
import Lint.Rules.SimplifyPropertyAccess
import Lint.Rules.ElmTest.NoDuplicateTestBodies


type Msg
    = Replace String


config : List ( Severity, LintRule )
config =
    [ ( Critical, Lint.Rules.DefaultPatternPosition.rule { position = Lint.Rules.DefaultPatternPosition.Last } )
    , ( Critical, Lint.Rules.NoConstantCondition.rule )
    , ( Critical, Lint.Rules.NoDebug.rule )
    , ( Critical, Lint.Rules.NoDuplicateImports.rule )
    , ( Critical, Lint.Rules.NoExposingEverything.rule )
    , ( Critical, Lint.Rules.NoImportingEverything.rule { exceptions = [ "Html" ] } )
    , ( Critical, Lint.Rules.NoNestedLet.rule )
    , ( Critical, Lint.Rules.NoUnannotatedFunction.rule )
    , ( Critical, Lint.Rules.NoUnusedVariables.rule )
    , ( Critical, Lint.Rules.NoUselessIf.rule )
    , ( Critical, Lint.Rules.NoUselessPatternMatching.rule )
    , ( Warning, Lint.Rules.NoWarningComments.rule )
    , ( Critical, Lint.Rules.SimplifyPiping.rule )
    , ( Critical, Lint.Rules.SimplifyPropertyAccess.rule )
    , ( Critical, Lint.Rules.ElmTest.NoDuplicateTestBodies.rule )
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


removeComments : String -> String
removeComments =
    Regex.replace Regex.All (Regex.regex "--.$") (always "")
        >> Regex.replace Regex.All (Regex.regex "\n +\\w+ : .*") (always "")


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

            PortModuleDeclaration _ exportSet ->
                displayElementAndAnyChild s [ exportSet ]

            ImportStatement _ _ exportSet ->
                displayElementAndAnyChild s [ exportSet ]

            s ->
                defaultDisplay


tree : Result (Combine.ParseErr ()) ( b, c, List Statement ) -> Html Msg
tree ast =
    case ast of
        Ok ( _, _, statements ) ->
            ul [] (List.map statement statements)

        Err ( _, _, errors ) ->
            div [] (List.map text errors)


lint : String -> Html Msg
lint source =
    let
        lintResult =
            lintSource config source

        messages =
            case lintResult of
                Err errors ->
                    errors

                Ok errors ->
                    if List.isEmpty errors then
                        [ "No errors." ]
                    else
                        List.map (Tuple.second >> .message) errors
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
            , p [ id "ast" ] [ tree <| Ast.parse <| removeComments model ]
            ]
        ]


main : Program Never String Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
