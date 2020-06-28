module MiscRules.NoBooleanCase exposing (rule)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Rule3 as Rule3


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoBooleanCase" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor (Node.Node range expression) =
    case expression of
        Expression.CaseExpression caseBlock ->
            errorsForCaseBlock range caseBlock

        _ ->
            []


errorsForCaseBlock : Range -> Expression.CaseBlock -> List (Error {})
errorsForCaseBlock range { cases } =
    case cases of
        [ ( Node.Node _ (Pattern.NamedPattern { moduleName, name } []), _ ), _ ] ->
            if moduleName == [] && (name == "True" || name == "False") then
                [ Rule.error
                    { message = "Matching boolean values in a case .. of expression"
                    , details = [ "It's quite silly" ]
                    }
                    range
                ]

            else
                []

        _ ->
            []
