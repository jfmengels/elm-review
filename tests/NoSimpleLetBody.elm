module NoSimpleLetBody exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports when a let expression's body is a simple reference to a value declared in the let expression.

🔧 Running with `--fix` will automatically remove most of the reported errors.

    config =
        [ NoSimpleLetBody.rule
        ]

The reasoning is that it is not necessary to assign a name to the result of a let expression,
since they are redundant with the value or function containing the expression.

If it feels necessary to give a name anyway because it helps clarify the context, then it might be a sign that the computation of that value should be extracted to a function.

Let expressions will be reported regardless of whether they're at the root of a function or deeply nested.


## Fail

    a =
        let
            b =
                1

            c =
                b + 1
        in
        c


## Success

Anything that is not simply a reference to a value declared in the let expression is okay.

    a =
        let
            b =
                1
        in
        b + 1

The rule will not report when the referenced value was destructured in the let expression.

    first tuple =
        let
            ( value, _ ) =
                tuple
        in
        value


## When (not) to enable this rule

This rule resolves a minor style issue, and may not be worth enforcing depending on how strongly you feel about this issue.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-code-style/example --rules NoSimpleLetBody
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoSimpleLetBody" initContext
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { extractSourceCode : Range -> String
    , constructorsToReport : Set String
    }


initContext : Rule.ContextCreator () Context
initContext =
    Rule.initContextCreator
        (\extractSourceCode () ->
            { extractSourceCode = extractSourceCode
            , constructorsToReport = Set.empty
            }
        )
        |> Rule.withSourceCodeExtractor


declarationListVisitor : List (Node Declaration) -> Context -> ( List nothing, Context )
declarationListVisitor declarations context =
    let
        constructorsToReport : Set String
        constructorsToReport =
            List.foldl safelyDestructurableConstructor Set.empty declarations
    in
    ( [], { context | constructorsToReport = constructorsToReport } )


safelyDestructurableConstructor : Node Declaration -> Set String -> Set String
safelyDestructurableConstructor (Node _ node) acc =
    case node of
        Declaration.CustomTypeDeclaration { generics, constructors } ->
            if List.isEmpty generics then
                case constructors of
                    [ Node _ { name } ] ->
                        Set.insert (Node.value name) acc

                    _ ->
                        acc

            else
                acc

        _ ->
            acc


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor (Node range expr) context =
    case expr of
        Expression.LetExpression letBlock ->
            ( visitLetExpression context range letBlock, context )

        _ ->
            ( [], context )


visitLetExpression : Context -> Range -> Expression.LetBlock -> List (Rule.Error {})
visitLetExpression { extractSourceCode, constructorsToReport } nodeRange { declarations, expression } =
    case checkPatternToFind constructorsToReport expression of
        Just patternToFind ->
            let
                maybeResolution : Maybe Resolution
                maybeResolution =
                    findDeclarationToMove patternToFind declarations
            in
            case maybeResolution of
                Just resolution ->
                    let
                        (Node range _) =
                            expression
                    in
                    [ Rule.errorWithFix
                        { message = "The referenced value should be inlined."
                        , details =
                            [ "The name of the value is redundant with the surrounding expression."
                            , "If you believe that the expression needs a name because it is too complex, consider splitting the expression up more or extracting it to a new function."
                            ]
                        }
                        range
                        (fix extractSourceCode nodeRange range resolution)
                    ]

                Nothing ->
                    []

        _ ->
            []


checkPatternToFind : Set String -> Node Expression -> Maybe PatternToFind
checkPatternToFind constructorsToReport (Node _ expression) =
    case expression of
        Expression.ParenthesizedExpression expr ->
            checkPatternToFind constructorsToReport expr

        Expression.FunctionOrValue [] name ->
            Just (Reference name)

        Expression.TupledExpression elements ->
            let
                patternsToFind : List PatternToFind
                patternsToFind =
                    List.filterMap (checkPatternToFind constructorsToReport) elements
            in
            if areLengthsEqual patternsToFind elements then
                Just (TuplePattern patternsToFind)

            else
                Nothing

        Expression.Application ((Node _ (Expression.FunctionOrValue [] name)) :: args) ->
            -- TODO Support constructors from other modules
            if Set.member name constructorsToReport then
                let
                    patternsToFind : List PatternToFind
                    patternsToFind =
                        List.filterMap (checkPatternToFind constructorsToReport) args
                in
                if areLengthsEqual patternsToFind args then
                    Just (NamedPattern { moduleName = [], name = name } patternsToFind)

                else
                    Nothing

            else
                Nothing

        _ ->
            Nothing


type PatternToFind
    = Reference String
    | TuplePattern (List PatternToFind)
    | NamedPattern Pattern.QualifiedNameRef (List PatternToFind)


type Resolution
    = ReportNoFix
    | Move { toRemove : Range, toCopy : Range }
    | RemoveOnly { toCopy : Range }
    | MoveLast { previousEnd : Location, toCopy : Range }


findDeclarationToMove : PatternToFind -> List (Node Expression.LetDeclaration) -> Maybe Resolution
findDeclarationToMove patternToFind declarations =
    findDeclarationToMoveHelp
        patternToFind
        (List.length declarations)
        declarations
        { index = 0
        , previousEnd = Nothing
        , lastEnd = Nothing
        }


findDeclarationToMoveHelp : PatternToFind -> Int -> List (Node Expression.LetDeclaration) -> { index : Int, previousEnd : Maybe Location, lastEnd : Maybe Location } -> Maybe Resolution
findDeclarationToMoveHelp patternToFind nbOfDeclarations declarations { index, previousEnd, lastEnd } =
    case declarations of
        [] ->
            Nothing

        (Node declarationRange declaration) :: rest ->
            case match declaration patternToFind of
                Just matchParams ->
                    Just
                        (createResolution
                            declarationRange
                            matchParams
                            { lastEnd = lastEnd, previousEnd = previousEnd }
                            (index == nbOfDeclarations - 1)
                        )

                Nothing ->
                    findDeclarationToMoveHelp
                        patternToFind
                        nbOfDeclarations
                        rest
                        { index = index + 1
                        , previousEnd = lastEnd
                        , lastEnd = Just declarationRange.end
                        }


match : Expression.LetDeclaration -> PatternToFind -> Maybe { hasArguments : Bool, expressionRange : Range }
match declaration patternToFind =
    case declaration of
        Expression.LetFunction function ->
            let
                functionDeclaration : Expression.FunctionImplementation
                functionDeclaration =
                    Node.value function.declaration
            in
            if Reference (Node.value functionDeclaration.name) == patternToFind then
                Just
                    { hasArguments = not (List.isEmpty functionDeclaration.arguments)
                    , expressionRange = Node.range functionDeclaration.expression
                    }

            else
                Nothing

        Expression.LetDestructuring destructuringPattern (Node expressionRange _) ->
            if matchPatternToFind patternToFind destructuringPattern then
                Just
                    { hasArguments = False
                    , expressionRange = expressionRange
                    }

            else
                Nothing


matchPatternToFind : PatternToFind -> Node Pattern -> Bool
matchPatternToFind patternToFind (Node _ destructuringPattern) =
    case ( patternToFind, destructuringPattern ) of
        ( _, Pattern.ParenthesizedPattern pattern ) ->
            matchPatternToFind patternToFind pattern

        ( Reference refName, Pattern.VarPattern name ) ->
            refName == name

        ( Reference _, _ ) ->
            False

        ( TuplePattern left, Pattern.TuplePattern right ) ->
            map2ThenAll matchPatternToFind left right

        ( TuplePattern _, _ ) ->
            False

        ( NamedPattern refLeft argsLeft, Pattern.NamedPattern refRight argsRight ) ->
            -- TODO Is only safe when there are no value phantom types?
            (refLeft == refRight)
                && map2ThenAll matchPatternToFind argsLeft argsRight

        ( NamedPattern _ _, _ ) ->
            False


areLengthsEqual : List a -> List b -> Bool
areLengthsEqual left right =
    case left of
        [] ->
            case right of
                [] ->
                    True

                _ :: _ ->
                    False

        _ :: leftRest ->
            case right of
                [] ->
                    False

                _ :: rightRest ->
                    areLengthsEqual leftRest rightRest


map2ThenAll : (a -> b -> Bool) -> List a -> List b -> Bool
map2ThenAll fn left right =
    case left of
        [] ->
            case right of
                [] ->
                    True

                _ ->
                    False

        leftFirst :: leftRest ->
            case right of
                [] ->
                    False

                rightFirst :: rightRest ->
                    if fn leftFirst rightFirst then
                        map2ThenAll fn leftRest rightRest

                    else
                        False


createResolution : Range -> { hasArguments : Bool, expressionRange : Range } -> { lastEnd : Maybe Location, previousEnd : Maybe Location } -> Bool -> Resolution
createResolution declarationRange { hasArguments, expressionRange } { lastEnd, previousEnd } isLast =
    if hasArguments then
        ReportNoFix

    else
        case ( lastEnd, isLast ) of
            ( Just lastEnd_, True ) ->
                MoveLast
                    { previousEnd = lastEnd_
                    , toCopy = expressionRange
                    }

            ( Just _, False ) ->
                Move
                    { toRemove =
                        { start = Maybe.withDefault declarationRange.start previousEnd
                        , end = declarationRange.end
                        }
                    , toCopy = expressionRange
                    }

            ( Nothing, True ) ->
                RemoveOnly { toCopy = expressionRange }

            ( Nothing, False ) ->
                Move
                    { toRemove =
                        { start = Maybe.withDefault declarationRange.start previousEnd
                        , end = declarationRange.end
                        }
                    , toCopy = expressionRange
                    }


fix :
    (Range -> String)
    -> Range
    -> Range
    -> Resolution
    -> List Fix
fix extractSourceCode nodeRange letBodyRange resolution =
    case resolution of
        ReportNoFix ->
            []

        RemoveOnly { toCopy } ->
            -- Remove the let/in keywords and the let binding
            [ Fix.removeRange { start = nodeRange.start, end = toCopy.start }
            , Fix.removeRange { start = toCopy.end, end = nodeRange.end }
            ]

        Move { toRemove, toCopy } ->
            [ Fix.removeRange
                { start =
                    if nodeRange.start.row == toRemove.start.row then
                        toRemove.start

                    else
                        { row = toRemove.start.row, column = 1 }
                , end = toRemove.end
                }
            , Fix.replaceRangeBy
                letBodyRange
                (extractSourceCode toCopy)
            ]

        MoveLast { previousEnd, toCopy } ->
            let
                indentation : String
                indentation =
                    String.repeat (nodeRange.start.column - 1) " "
            in
            [ Fix.replaceRangeBy { start = previousEnd, end = toCopy.start } ("\n" ++ indentation ++ "in\n" ++ indentation)
            , Fix.removeRange { start = toCopy.end, end = nodeRange.end }
            ]
