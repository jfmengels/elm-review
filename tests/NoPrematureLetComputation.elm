module NoPrematureLetComputation exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import RangeDict exposing (RangeDict)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports let declarations that are computed earlier than needed.

This rule is useful to prevent unnecessary computations and to group related code together.

    config =
        [ NoPrematureLetComputation.rule
        ]

🔧 Running with `--fix` will automatically fix almost all of the reported errors.


## Fail

In this example, we compute `value` earlier than needed, and we end up not using it in of the branches.

    someFunction n =
        let
            value =
                expensiveComputation n
        in
        if needToCompute then
            value + 1

        else
            0


## Success

If we take the example from above, this would be the suggested (and automatic) fix:

    someFunction n =
        if needToCompute then
            let
                value =
                    expensiveComputation n
            in
            value + 1

        else
            0

A declaration will not be reported if it's used in multiple branches at the same level.
The rule will try to move the declaration as close as possible to the usages.

    someFunction n =
        let
            value =
                expensiveComputation n
        in
        if condition then
            value + 1

        else
            value - 1

Sometimes, when a computation is somewhat expensive, it is done once in a let declaration and then
referenced in a let or anonymous function. This rule does not want to worsen the performance, and therefore
declarations will not be moved to inside a function.

    someFunction items n =
        let
            -- Will stay here
            value =
                expensiveComputation n
        in
        List.map
            (\item ->
                if condition item then
                    value + item.value

                else
                    0
            )
            items

There are some exceptions when we know for sure that an anonymous function will only be computed once,
for instance when it is the argument to `Maybe.map`:

    someFunction maybeItem n =
        let
            -- Will be moved from here...
            value =
                expensiveComputation n
        in
        Maybe.map
            (\item ->
                if condition item then
                    -- ... to here
                    value + item.value

                else
                    0
            )
            maybeItem

The rule will also merge adjacent let declarations together:

    someFunction n =
        let
            y =
                1
        in
        let
            z =
                1
        in
        y + z

    -->
    someFunction n =
        let
            y =
                1

            z =
                1
        in
        y + z


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoPrematureLetComputation
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoPrematureLetComputation" initialContext
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , extractSourceCode : Range -> String
    , scope : Scope
    , branching : Branching
    , functionsThatWillOnlyBeComputedOnce : RangeDict ()
    }


type alias Branching =
    { full : List Range
    , last : Maybe Range
    }


type Scope
    = Scope ScopeType ScopeData


type ScopeType
    = Branch
    | LetScope
    | Function
    | FunctionOkayToMoveInto


type alias ScopeData =
    { letDeclarations : List Declared
    , used : Set String
    , insertionLocation : LetInsertPosition
    , scopes : RangeDict Scope
    }


type LetInsertPosition
    = InsertNewLet Location
    | InsertExistingLet Location


type alias Declared =
    { name : String
    , introducesVariablesInImplementation : Bool
    , reportRange : Range
    , declarationColumn : Int
    , declarationRange : Range
    , removeRange : Range
    }


newBranch : LetInsertPosition -> Scope
newBranch insertionLocation =
    Scope
        Branch
        { letDeclarations = []
        , used = Set.empty
        , insertionLocation = insertionLocation
        , scopes = RangeDict.empty
        }


emptyBranching : Branching
emptyBranching =
    { full = []
    , last = Nothing
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable extractSourceCode () ->
            { lookupTable = lookupTable
            , extractSourceCode = extractSourceCode
            , scope = newBranch (InsertNewLet { row = 0, column = 0 })
            , branching = emptyBranching
            , functionsThatWillOnlyBeComputedOnce = RangeDict.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withSourceCodeExtractor


updateCurrentBranch : (ScopeData -> ScopeData) -> List Range -> Scope -> Scope
updateCurrentBranch updateFn currentBranching (Scope type_ segment) =
    case currentBranching of
        [] ->
            Scope type_ (updateFn segment)

        range :: restOfSegments ->
            Scope
                type_
                { segment
                    | scopes =
                        RangeDict.modify
                            range
                            (updateCurrentBranch updateFn restOfSegments)
                            segment.scopes
                }


updateAllSegmentsOfCurrentBranch : (ScopeData -> ScopeData) -> List Range -> Scope -> Scope
updateAllSegmentsOfCurrentBranch updateFn currentBranching (Scope type_ scope) =
    case currentBranching of
        [] ->
            Scope type_ (updateFn scope)

        range :: restOfSegments ->
            Scope
                type_
                (updateFn
                    { scope
                        | scopes =
                            RangeDict.modify
                                range
                                (updateAllSegmentsOfCurrentBranch updateFn restOfSegments)
                                scope.scopes
                    }
                )


getCurrentBranch : List Range -> Scope -> Maybe Scope
getCurrentBranch currentBranching branch =
    case currentBranching of
        [] ->
            Just branch

        range :: restOfBranching ->
            RangeDict.get range (getScopes branch)
                |> Maybe.andThen (getCurrentBranch restOfBranching)


getScopes : Scope -> RangeDict Scope
getScopes =
    getScopeData >> .scopes


getScopeData : Scope -> ScopeData
getScopeData (Scope _ scope) =
    scope


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            ( []
            , { lookupTable = context.lookupTable
              , extractSourceCode = context.extractSourceCode
              , scope = newBranch (figureOutInsertionLocation (declaration |> Node.value |> .expression))
              , branching = emptyBranching
              , functionsThatWillOnlyBeComputedOnce = RangeDict.empty
              }
            )

        _ ->
            ( [], context )


figureOutInsertionLocation : Node Expression -> LetInsertPosition
figureOutInsertionLocation node =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            case declarations of
                first :: _ ->
                    InsertExistingLet (Node.range first).start

                [] ->
                    -- Should not happen
                    InsertNewLet (Node.range node).start

        _ ->
            InsertNewLet (Node.range node).start


expressionEnterVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitor node context =
    let
        newContext : Context
        newContext =
            case getCurrentBranch context.branching.full context.scope |> Maybe.map getScopes of
                Just scopes ->
                    if RangeDict.member (Node.range node) scopes then
                        { context | branching = addBranching (Node.range node) context.branching }

                    else
                        context

                Nothing ->
                    context
    in
    ( [], expressionEnterVisitorHelp node newContext )


addBranching : Range -> Branching -> Branching
addBranching range branching =
    { full = branching.full ++ [ range ]
    , last = Just range
    }


removeLastBranchIfOnIt : Range -> Branching -> Maybe Branching
removeLastBranchIfOnIt range branching =
    if branching.last == Just range then
        let
            full : List Range
            full =
                List.take (List.length branching.full - 1) branching.full
        in
        Just
            (removeLastBranchIfOnItRetry
                range
                { full = full
                , last = getLastListItem full
                }
            )

    else
        Nothing


removeLastBranchIfOnItRetry : Range -> Branching -> Branching
removeLastBranchIfOnItRetry range branching =
    if branching.last == Just range then
        let
            full : List Range
            full =
                List.take (List.length branching.full - 1) branching.full
        in
        { full = full
        , last = getLastListItem full
        }

    else
        branching


popCurrentNodeFromBranching : Range -> Context -> Context
popCurrentNodeFromBranching range context =
    case removeLastBranchIfOnIt range context.branching of
        Just newBranching ->
            { context | branching = newBranching }

        Nothing ->
            context


expressionEnterVisitorHelp : Node Expression -> Context -> Context
expressionEnterVisitorHelp node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            let
                branch : Scope
                branch =
                    updateCurrentBranch
                        (\b -> { b | used = Set.insert name b.used })
                        context.branching.full
                        context.scope
            in
            { context | scope = branch }

        Expression.RecordUpdateExpression name _ ->
            let
                branch : Scope
                branch =
                    updateCurrentBranch
                        (\b -> { b | used = Set.insert (Node.value name) b.used })
                        context.branching.full
                        context.scope
            in
            { context | scope = branch }

        Expression.LetExpression letBlock ->
            registerLetExpression node letBlock context

        Expression.IfBlock _ then_ else_ ->
            addBranches [ then_, else_ ] context

        Expression.CaseExpression { cases } ->
            registerCaseExpression node cases context

        Expression.LambdaExpression { args, expression } ->
            let
                branch : Scope
                branch =
                    if List.any patternIntroducesVariable args then
                        markLetDeclarationsAsIntroducingVariables (Node.range node) context

                    else
                        context.scope

                newScope : Scope
                newScope =
                    Scope
                        (if RangeDict.member (Node.range node) context.functionsThatWillOnlyBeComputedOnce then
                            FunctionOkayToMoveInto

                         else
                            Function
                        )
                        { letDeclarations = []
                        , used = Set.empty
                        , insertionLocation = figureOutInsertionLocation expression
                        , scopes = RangeDict.empty
                        }

                branchWithAddedScope : Scope
                branchWithAddedScope =
                    updateCurrentBranch
                        (\b ->
                            { b
                                | scopes =
                                    RangeDict.insert
                                        (Node.range node)
                                        newScope
                                        b.scopes
                            }
                        )
                        context.branching.full
                        branch
            in
            { context
                | scope = branchWithAddedScope
                , branching = addBranching (Node.range node) context.branching
            }

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: argumentWithParens :: restOfArguments) ->
            registerApplicationCall
                fnRange
                fnName
                argumentWithParens
                (List.length restOfArguments)
                context

        Expression.OperatorApplication "|>" _ _ (Node _ (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: argumentWithParens :: restOfArguments))) ->
            registerApplicationCall
                fnRange
                fnName
                argumentWithParens
                (List.length restOfArguments + 1)
                context

        Expression.OperatorApplication "<|" _ (Node _ (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: argumentWithParens :: restOfArguments))) _ ->
            registerApplicationCall
                fnRange
                fnName
                argumentWithParens
                (List.length restOfArguments + 1)
                context

        _ ->
            context


registerLetExpression : Node Expression -> Expression.LetBlock -> Context -> Context
registerLetExpression node { declarations, expression } context =
    let
        isDeclarationAlone : Bool
        isDeclarationAlone =
            List.length declarations == 1

        letDeclarations : List Declared
        letDeclarations =
            declarations
                |> List.concatMap collectDeclarations
                |> List.map
                    (\( nameNode, expressionRange, declaration ) ->
                        { name = Node.value nameNode
                        , introducesVariablesInImplementation = False
                        , reportRange = Node.range nameNode
                        , declarationColumn = (Node.range declaration).start.column
                        , declarationRange = fullLines { start = (Node.range declaration).start, end = expressionRange.end }
                        , removeRange =
                            if isDeclarationAlone then
                                { start = (Node.range node).start
                                , end = (Node.range expression).start
                                }

                            else
                                { start = { row = (Node.range declaration).start.row, column = 1 }
                                , end = expressionRange.end
                                }
                        }
                    )

        scopes : RangeDict Scope
        scopes =
            declarations
                |> List.filterMap getLetFunctionRange
                |> List.map (\range -> ( range, functionScope ))
                |> RangeDict.fromList

        newScope : Scope
        newScope =
            Scope
                LetScope
                { letDeclarations = letDeclarations
                , used = Set.empty
                , insertionLocation = figureOutInsertionLocation node
                , scopes = scopes
                }

        contextWithDeclarationsMarked : Context
        contextWithDeclarationsMarked =
            { context | scope = markLetDeclarationsAsIntroducingVariables (Node.range node) context }

        branch : Scope
        branch =
            updateCurrentBranch
                (\b ->
                    { b
                        | scopes =
                            RangeDict.insert
                                (Node.range node)
                                newScope
                                b.scopes
                    }
                )
                contextWithDeclarationsMarked.branching.full
                contextWithDeclarationsMarked.scope
    in
    { contextWithDeclarationsMarked
        | scope = branch
        , branching = addBranching (Node.range node) contextWithDeclarationsMarked.branching
    }


registerCaseExpression : Node Expression -> List ( Node Pattern, Node Expression ) -> Context -> Context
registerCaseExpression node cases context =
    let
        contextWithDeclarationsMarked : Context
        contextWithDeclarationsMarked =
            if List.any (Tuple.first >> patternIntroducesVariable) cases then
                { context | scope = markLetDeclarationsAsIntroducingVariables (Node.range node) context }

            else
                context

        branchNodes : List (Node Expression)
        branchNodes =
            List.map (\( _, exprNode ) -> exprNode) cases
    in
    addBranches branchNodes contextWithDeclarationsMarked


registerApplicationCall : Range -> String -> Node Expression -> Int -> Context -> Context
registerApplicationCall fnRange fnName argumentWithParens nbOfOtherArguments context =
    let
        argument : Node Expression
        argument =
            removeParens argumentWithParens
    in
    case Node.value argument of
        Expression.LambdaExpression _ ->
            case numberOfArgumentsForFunction context.lookupTable fnName fnRange of
                Just expectedNumberOfArguments ->
                    if nbOfOtherArguments == expectedNumberOfArguments - 1 then
                        { context
                            | functionsThatWillOnlyBeComputedOnce =
                                RangeDict.insert (Node.range argument) () context.functionsThatWillOnlyBeComputedOnce
                        }

                    else
                        context

                Nothing ->
                    context

        _ ->
            context


numberOfArgumentsForFunction : ModuleNameLookupTable -> String -> Range -> Maybe number
numberOfArgumentsForFunction lookupTable fnName fnRange =
    case Dict.get fnName knownFunctions of
        Just knownModuleNames ->
            ModuleNameLookupTable.moduleNameAt lookupTable fnRange
                |> Maybe.andThen (\moduleName -> Dict.get moduleName knownModuleNames)

        _ ->
            Nothing


knownFunctions : Dict String (Dict (List String) number)
knownFunctions =
    Dict.fromList
        [ ( "map"
          , Dict.fromList
                [ ( [ "Maybe" ], 2 )
                , ( [ "Html" ], 2 )
                , ( [ "Result" ], 2 )
                , ( [ "Task" ], 2 )
                ]
          )
        , ( "map2"
          , Dict.fromList
                [ ( [ "Maybe" ], 3 )
                , ( [ "Result" ], 3 )
                , ( [ "Task" ], 3 )
                ]
          )
        , ( "map3"
          , Dict.fromList
                [ ( [ "Maybe" ], 4 )
                , ( [ "Result" ], 4 )
                , ( [ "Task" ], 4 )
                ]
          )
        , ( "map4"
          , Dict.fromList
                [ ( [ "Maybe" ], 5 )
                , ( [ "Result" ], 5 )
                , ( [ "Task" ], 5 )
                ]
          )
        , ( "map5"
          , Dict.fromList
                [ ( [ "Maybe" ], 6 )
                , ( [ "Result" ], 6 )
                , ( [ "Task" ], 6 )
                ]
          )
        , ( "mapError"
          , Dict.fromList
                [ ( [ "Result" ], 2 )
                , ( [ "Task" ], 2 )
                ]
          )
        , ( "andThen"
          , Dict.fromList
                [ ( [ "Maybe" ], 2 )
                , ( [ "Result" ], 2 )
                , ( [ "Task" ], 2 )
                ]
          )

        -- TODO Support mapBoth as well
        , ( "mapFirst"
          , Dict.singleton [ "Tuple" ] 2
          )
        , ( "mapSecond"
          , Dict.singleton [ "Tuple" ] 2
          )
        , ( "perform"
          , Dict.singleton [ "Task" ] 2
          )
        , ( "attempt"
          , Dict.singleton [ "Task" ] 2
          )
        , ( "onError"
          , Dict.singleton [ "Task" ] 2
          )
        , ( "update"
          , Dict.singleton [ "Dict" ] 4
          )
        ]


removeParens : Node Expression -> Node Expression
removeParens node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            removeParens expr

        _ ->
            node


functionScope : Scope
functionScope =
    Scope
        Function
        { letDeclarations = []
        , used = Set.empty
        , insertionLocation =
            -- Will not be used
            InsertNewLet { row = 0, column = 0 }
        , scopes = RangeDict.empty
        }


variablesInPattern : Node Pattern -> List (Node String)
variablesInPattern node =
    case Node.value node of
        Pattern.ListPattern patterns ->
            List.concatMap variablesInPattern patterns

        Pattern.TuplePattern patterns ->
            List.concatMap variablesInPattern patterns

        Pattern.RecordPattern fields ->
            fields

        Pattern.UnConsPattern left right ->
            List.concatMap variablesInPattern [ left, right ]

        Pattern.VarPattern name ->
            [ Node (Node.range node) name ]

        Pattern.NamedPattern _ patterns ->
            List.concatMap variablesInPattern patterns

        Pattern.AsPattern pattern name ->
            name :: variablesInPattern pattern

        Pattern.ParenthesizedPattern pattern ->
            variablesInPattern pattern

        _ ->
            []


patternIntroducesVariable : Node Pattern -> Bool
patternIntroducesVariable node =
    case Node.value node of
        Pattern.ListPattern patterns ->
            List.any patternIntroducesVariable patterns

        Pattern.TuplePattern patterns ->
            List.any patternIntroducesVariable patterns

        Pattern.RecordPattern _ ->
            True

        Pattern.UnConsPattern left right ->
            patternIntroducesVariable left
                || patternIntroducesVariable right

        Pattern.VarPattern _ ->
            True

        Pattern.NamedPattern _ patterns ->
            List.any patternIntroducesVariable patterns

        Pattern.AsPattern _ _ ->
            True

        Pattern.ParenthesizedPattern pattern ->
            patternIntroducesVariable pattern

        _ ->
            False


markLetDeclarationsAsIntroducingVariables : Range -> Context -> Scope
markLetDeclarationsAsIntroducingVariables range context =
    updateAllSegmentsOfCurrentBranch
        (markDeclarationsAsUsed range)
        context.branching.full
        context.scope


markDeclarationsAsUsed : Range -> ScopeData -> ScopeData
markDeclarationsAsUsed range branchData =
    { branchData | letDeclarations = List.map (markDeclarationAsUsed range) branchData.letDeclarations }


markDeclarationAsUsed : Range -> Declared -> Declared
markDeclarationAsUsed range declared =
    if isRangeContained { outer = declared.declarationRange, inner = range } then
        { declared | introducesVariablesInImplementation = True }

    else
        declared


isRangeContained : { outer : Range, inner : Range } -> Bool
isRangeContained { outer, inner } =
    (Range.compareLocations outer.start inner.start /= GT)
        && (Range.compareLocations outer.end inner.end /= LT)


fullLines : Range -> Range
fullLines range =
    { start = { row = range.start.row, column = 1 }
    , end = range.end
    }


addBranches : List (Node Expression) -> Context -> Context
addBranches nodes context =
    let
        branch : Scope
        branch =
            updateCurrentBranch
                (\b -> { b | scopes = insertNewBranches nodes b.scopes })
                context.branching.full
                context.scope
    in
    { context | scope = branch }


insertNewBranches : List (Node Expression) -> RangeDict Scope -> RangeDict Scope
insertNewBranches nodes rangeDict =
    case nodes of
        [] ->
            rangeDict

        node :: tail ->
            insertNewBranches tail
                (RangeDict.insert
                    (Node.range node)
                    (newBranch (figureOutInsertionLocation node))
                    rangeDict
                )


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor node context =
    ( expressionExitVisitorHelp node context
    , popCurrentNodeFromBranching (Node.range node) context
    )


expressionExitVisitorHelp : Node Expression -> Context -> List (Rule.Error {})
expressionExitVisitorHelp node context =
    case Node.value node of
        Expression.LetExpression _ ->
            case getCurrentBranch context.branching.full context.scope of
                Just ((Scope LetScope scopeData) as scope) ->
                    List.filterMap
                        (\declaration ->
                            canBeMovedToCloserLocation True declaration.name scope
                                |> List.head
                                |> Maybe.map (createError context declaration)
                        )
                        scopeData.letDeclarations

                _ ->
                    []

        _ ->
            []


collectDeclarations : Node Expression.LetDeclaration -> List ( Node String, Range, Node Expression.LetDeclaration )
collectDeclarations node =
    case Node.value node of
        Expression.LetFunction letFunction ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value letFunction.declaration
            in
            if List.isEmpty declaration.arguments then
                [ ( declaration.name
                  , Node.range declaration.expression
                  , node
                  )
                ]

            else
                []

        Expression.LetDestructuring pattern expression ->
            case variablesInPattern pattern of
                [ name ] ->
                    [ ( name
                      , Node.range expression
                      , node
                      )
                    ]

                _ ->
                    []


getLetFunctionRange : Node Expression.LetDeclaration -> Maybe Range
getLetFunctionRange node =
    case Node.value node of
        Expression.LetFunction { declaration } ->
            if List.isEmpty (Node.value declaration).arguments then
                Nothing

            else
                Just (declaration |> Node.value |> .expression |> Node.range)

        Expression.LetDestructuring _ _ ->
            Nothing


canBeMovedToCloserLocation : Bool -> String -> Scope -> List LetInsertPosition
canBeMovedToCloserLocation isRoot name (Scope type_ scope) =
    let
        closestLocation : List LetInsertPosition
        closestLocation =
            canBeMovedToCloserLocationForBranchData isRoot name scope
    in
    case type_ of
        Branch ->
            closestLocation

        LetScope ->
            closestLocation

        FunctionOkayToMoveInto ->
            closestLocation

        Function ->
            -- Duplicating so that the parent has to use its insert location,
            -- and we don't insert inside the let
            closestLocation ++ closestLocation


canBeMovedToCloserLocationForBranchData : Bool -> String -> ScopeData -> List LetInsertPosition
canBeMovedToCloserLocationForBranchData isRoot name branchData =
    if Set.member name branchData.used then
        emptyIfTrue isRoot [ branchData.insertionLocation ]

    else
        let
            relevantUsages : List LetInsertPosition
            relevantUsages =
                findRelevantUsages name (RangeDict.values branchData.scopes) []
        in
        if List.length relevantUsages > 1 then
            emptyIfTrue isRoot [ branchData.insertionLocation ]

        else
            relevantUsages


findRelevantUsages : String -> List Scope -> List LetInsertPosition -> List LetInsertPosition
findRelevantUsages name branches result =
    if List.length result > 1 then
        -- If we have already found 2 branches with relevant usages, then we don't need to continue
        result

    else
        case branches of
            [] ->
                result

            first :: rest ->
                findRelevantUsages name rest (canBeMovedToCloserLocation False name first ++ result)


emptyIfTrue : Bool -> List a -> List a
emptyIfTrue bool list =
    if bool then
        []

    else
        list


createError : Context -> Declared -> LetInsertPosition -> Rule.Error {}
createError context declared letInsertPosition =
    let
        letInsertLine : Int
        letInsertLine =
            case letInsertPosition of
                InsertNewLet insertLocation ->
                    insertLocation.row

                InsertExistingLet insertLocation ->
                    insertLocation.row
    in
    Rule.errorWithFix
        { message = "Let value was declared prematurely"
        , details =
            [ "This value is only used in some code paths, and it can therefore be computed unnecessarily."
            , "Try moving it closer to where it is needed, I recommend to move it to line " ++ String.fromInt letInsertLine ++ "."
            ]
        }
        declared.reportRange
        (fix context declared letInsertPosition)


fix : Context -> Declared -> LetInsertPosition -> List Fix
fix context declared letInsertPosition =
    if declared.introducesVariablesInImplementation then
        []

    else
        case letInsertPosition of
            InsertNewLet insertLocation ->
                [ Fix.removeRange declared.removeRange
                , context.extractSourceCode declared.declarationRange
                    |> wrapInLet declared.reportRange.start.column insertLocation.column
                    |> Fix.insertAt insertLocation
                ]

            InsertExistingLet insertLocation ->
                [ Fix.removeRange declared.removeRange
                , context.extractSourceCode declared.declarationRange
                    |> insertInLet declared.declarationColumn insertLocation.column
                    |> Fix.insertAt insertLocation
                ]


wrapInLet : Int -> Int -> String -> String
wrapInLet initialPosition column source =
    let
        padding : String
        padding =
            String.repeat (column - 1) " "
    in
    [ [ "let" ]
    , source
        |> String.lines
        |> List.map (\line -> String.repeat (column - initialPosition) " " ++ "    " ++ line)
    , [ padding ++ "in", padding ]
    ]
        |> List.concat
        |> String.join "\n"


insertInLet : Int -> Int -> String -> String
insertInLet initialPosition column source =
    case source |> String.trim |> String.lines of
        [] ->
            ""

        firstLine :: restOfLines ->
            ((firstLine :: List.map (\line -> String.repeat (column - initialPosition) " " ++ line) restOfLines)
                |> String.join "\n"
            )
                ++ "\n"
                ++ String.repeat (column - 1) " "


getLastListItem : List a -> Maybe a
getLastListItem =
    List.reverse >> List.head
