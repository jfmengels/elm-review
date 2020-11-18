module NoUselessCmdNone exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Reports functions that never make use of their power to return Cmds.

    config =
        [ NoUselessCmdNone.rule
        ]


## Fail

    update msg model =
        case msg of
            ClickedIncrement ->
                ( model + 1, Cmd.none )

            ClickedDecrement ->
                ( model - 1, Cmd.none )


## Success

    update msg model =
        case msg of
            ClickedIncrement ->
                model + 1

            ClickedDecrement ->
                model - 1


## When (not) to enable this rule

This rule is not useful when you are writing a package that does not deal with `Cmd`s.


## Try it out

You can try this rule out by running the following command:

```bash
elm - review --template jfmengels/elm-review-noop/preview --rules NoUselessCmdNone
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoUselessCmdNone" initialContext
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , ranges : List Range
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , ranges = []
            }
        )
        |> Rule.withModuleNameLookupTable


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            ( [], findViolations (Node.value function.declaration).expression context )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            ( []
            , List.foldl
                findViolationsForLetDeclaration
                context
                declarations
            )

        _ ->
            ( [], context )


findViolationsForLetDeclaration : Node Expression.LetDeclaration -> Context -> Context
findViolationsForLetDeclaration letDeclaration context =
    case Node.value letDeclaration of
        Expression.LetFunction function ->
            findViolations (Node.value function.declaration).expression context

        Expression.LetDestructuring _ expression ->
            findViolations expression context


findViolations : Node Expression -> Context -> Context
findViolations node context =
    case getBranches node of
        Just expressions ->
            let
                rangesWithViolation : List (Maybe Range)
                rangesWithViolation =
                    List.map (resultsInCmdNone context) expressions
            in
            if List.all ((/=) Nothing) rangesWithViolation then
                { context
                    | ranges =
                        (rangesWithViolation
                            |> List.filterMap identity
                        )
                            ++ context.ranges
                }

            else
                context

        Nothing ->
            context


getBranches : Node Expression -> Maybe (List (Node Expression))
getBranches node =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            Just (List.map Tuple.second cases)

        Expression.IfBlock _ ifExpr elseExpr ->
            case Node.value elseExpr of
                Expression.IfBlock _ _ _ ->
                    Just (ifExpr :: Maybe.withDefault [] (getBranches elseExpr))

                _ ->
                    Just [ ifExpr, elseExpr ]

        Expression.LetExpression { expression } ->
            getBranches expression

        _ ->
            Nothing


resultsInCmdNone : Context -> Node Expression -> Maybe Range
resultsInCmdNone context node =
    case Node.value node of
        Expression.TupledExpression (_ :: (Node range (Expression.FunctionOrValue _ "none")) :: []) ->
            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                Just [ "Platform", "Cmd" ] ->
                    Just range

                _ ->
                    Nothing

        Expression.LetExpression { expression } ->
            resultsInCmdNone context expression

        _ ->
            Nothing


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    context.ranges
        |> uniqueBy rangeToString
        |> List.map error


rangeToString : Range -> String
rangeToString range =
    [ range.start.row
    , range.start.column
    , range.end.row
    , range.end.column
    ]
        |> List.map String.fromInt
        |> String.join "-"


error : Range -> Error {}
error range =
    Rule.error
        { message = "This function always returns Cmd.none"
        , details =
            [ "Since this function returns Cmd.none in all cases, you can simplify it by having it not return a Cmd."
            ]
        }
        range



-- Taken from elm-community/list-extra


uniqueBy : (a -> comparable) -> List a -> List a
uniqueBy f list =
    uniqueHelp f Set.empty list []


uniqueHelp : (a -> comparable) -> Set comparable -> List a -> List a -> List a
uniqueHelp f existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                computedFirst : comparable
                computedFirst =
                    f first
            in
            if Set.member computedFirst existing then
                uniqueHelp f existing rest accumulator

            else
                uniqueHelp f (Set.insert computedFirst existing) rest (first :: accumulator)
