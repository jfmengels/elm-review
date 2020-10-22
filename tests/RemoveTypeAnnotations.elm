module RemoveTypeAnnotations exposing (rule)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "RemoveTypeAnnotations" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    List Range


initialContext : Context
initialContext =
    []


expressionVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionVisitor node rangesToRemove =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            let
                ranges : List Range
                ranges =
                    List.filterMap
                        (\declaration ->
                            case Node.value declaration of
                                Expression.LetFunction { signature } ->
                                    Maybe.map Node.range signature

                                Expression.LetDestructuring _ _ ->
                                    Nothing
                        )
                        declarations
            in
            ( [], ranges ++ rangesToRemove )

        _ ->
            ( [], rangesToRemove )


finalEvaluation : Context -> List (Error {})
finalEvaluation rangesToRemove =
    if List.isEmpty rangesToRemove then
        []

    else
        [ Rule.errorWithFix
            { message = "Remove type annotations"
            , details = [ "Bad type annotations!" ]
            }
            { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
            (List.map Fix.removeRange rangesToRemove)
        ]
