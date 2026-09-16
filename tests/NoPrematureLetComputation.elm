module NoPrematureLetComputation exposing (rule)

{-|

@docs rule

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
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
    , introducedVariables : Set String
    }


type LetInsertPosition
    = InsertNewLet { insert : Location, nextExpr : Location }
    | InsertExistingLet { insert : Location, nextExpr : Location }


type alias Declared =
    { names : List String
    , introducedVariables : Set String
    , reportRange : Range
    , declarationRange : Range
    , letBlock : LetBlockWithRange
    }


type alias LetBlockWithRange =
    { range : Range
    , block : Expression.LetBlock
    }


newBranch : LetInsertPosition -> Set String -> Scope
newBranch insertionLocation introducedVariables =
    Scope
        Branch
        { letDeclarations = []
        , used = Set.empty
        , insertionLocation = insertionLocation
        , scopes = RangeDict.empty
        , introducedVariables = introducedVariables
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
            , scope = newBranch (InsertNewLet { insert = emptyLocation, nextExpr = emptyLocation }) Set.empty
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
                            (\scope_ -> updateCurrentBranch updateFn restOfSegments scope_)
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
                                (\scope_ -> updateAllSegmentsOfCurrentBranch updateFn restOfSegments scope_)
                                scope.scopes
                    }
                )


getCurrentBranch : List Range -> Scope -> Maybe Scope
getCurrentBranch currentBranching initialBranch =
    List.foldl
        (\range maybeBranch ->
            case maybeBranch of
                Nothing ->
                    Nothing

                Just branch ->
                    RangeDict.get range (getScopes branch)
        )
        (Just initialBranch)
        currentBranching


getScopes : Scope -> RangeDict Scope
getScopes (Scope _ { scopes }) =
    scopes


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            let
                introducedVariables : Set String
                introducedVariables =
                    introducedVariablesInPattern (Node.value declaration).arguments Set.empty

                expression : Node Expression
                expression =
                    (Node.value declaration).expression
            in
            ( []
            , { lookupTable = context.lookupTable
              , extractSourceCode = context.extractSourceCode
              , scope =
                    newBranch
                        (insertExistingLetIfPresent expression
                            |> Maybe.withDefault (InsertNewLet { insert = (Node.range expression).start, nextExpr = (Node.range expression).start })
                        )
                        introducedVariables
              , branching = emptyBranching
              , functionsThatWillOnlyBeComputedOnce = RangeDict.empty
              }
            )

        _ ->
            ( [], context )


findKeyword : String -> (Range -> String) -> Range -> Maybe Location
findKeyword keyword extractSourceCode range =
    extractSourceCode range
        |> String.lines
        |> indexedFindMap
            (\lineIndex line ->
                case String.indexes keyword line of
                    [] ->
                        Nothing

                    column :: _ ->
                        if String.contains "--" (String.left column line) then
                            Nothing

                        else if lineIndex == 0 then
                            Just { row = range.start.row, column = range.start.column + column + String.length keyword }

                        else
                            Just { row = range.start.row + lineIndex, column = column + String.length keyword + 1 }
            )


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
                , last = last full
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
        , last = last full
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

        Expression.IfBlock (Node condition _) ((Node then_ _) as thenNode) ((Node else_ _) as elseNode) ->
            let
                thenInsertLocation : LetInsertPosition
                thenInsertLocation =
                    insertExistingLetIfPresent thenNode
                        |> onNothing
                            (\() ->
                                findKeyword "then" context.extractSourceCode { start = condition.end, end = then_.start }
                                    |> Maybe.map (\location -> InsertNewLet { insert = location, nextExpr = then_.start })
                            )
                        |> withDefaultLazy (\() -> InsertNewLet { insert = then_.start, nextExpr = then_.start })

                elseInsertLocation : LetInsertPosition
                elseInsertLocation =
                    insertExistingLetIfPresent elseNode
                        |> onNothing
                            (\() ->
                                findKeyword "else" context.extractSourceCode { start = then_.end, end = else_.start }
                                    |> Maybe.map (\location -> InsertNewLet { insert = location, nextExpr = else_.start })
                            )
                        |> withDefaultLazy (\() -> InsertNewLet { insert = else_.start, nextExpr = else_.start })
            in
            addBranches
                [ ( then_, thenInsertLocation, Set.empty )
                , ( else_, elseInsertLocation, Set.empty )
                ]
                context

        Expression.CaseExpression { cases } ->
            registerCaseExpression node cases context

        Expression.LambdaExpression lambda ->
            registerLambdaExpression node lambda context

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: args) ->
            registerApplicationCall
                fnRange
                fnName
                (Array.fromList args)
                context

        Expression.OperatorApplication "|>" _ pipeArg (Node _ (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: args))) ->
            registerApplicationCall
                fnRange
                fnName
                (Array.push pipeArg (Array.fromList args))
                context

        Expression.OperatorApplication "<|" _ (Node _ (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: args))) pipeArg ->
            registerApplicationCall
                fnRange
                fnName
                (Array.push pipeArg (Array.fromList args))
                context

        _ ->
            context


insertExistingLetIfPresent : Node Expression -> Maybe LetInsertPosition
insertExistingLetIfPresent (Node range expr) =
    case expr of
        Expression.LetExpression { declarations } ->
            case declarations of
                (Node first _) :: _ ->
                    Just
                        (InsertExistingLet
                            { insert =
                                { row = range.start.row
                                , column = range.start.column + 3
                                }
                            , nextExpr = first.start
                            }
                        )

                [] ->
                    -- Should not happen
                    Nothing

        _ ->
            Nothing


registerLambdaExpression : Node a -> Expression.Lambda -> Context -> Context
registerLambdaExpression node { args, expression } context =
    let
        introducedVariables : Set String
        introducedVariables =
            introducedVariablesInPattern args Set.empty

        branch : Scope
        branch =
            if Set.isEmpty introducedVariables then
                context.scope

            else
                markLetDeclarationsAsIntroducingVariables (Node.range node) introducedVariables context

        expressionRangeStart : Location
        expressionRangeStart =
            (Node.range expression).start

        insertLocation : LetInsertPosition
        insertLocation =
            insertExistingLetIfPresent expression
                |> onNothing
                    (\() ->
                        findKeyword "->" context.extractSourceCode { start = (Node.range node).start, end = expressionRangeStart }
                            |> Maybe.map (\location -> InsertNewLet { insert = location, nextExpr = expressionRangeStart })
                    )
                |> withDefaultLazy (\() -> InsertNewLet { insert = expressionRangeStart, nextExpr = expressionRangeStart })

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
                , insertionLocation = insertLocation
                , scopes = RangeDict.empty
                , introducedVariables = introducedVariables
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


registerLetExpression : Node Expression -> Expression.LetBlock -> Context -> Context
registerLetExpression ((Node letNodeRange _) as letNode) letBlock context =
    let
        letDeclarations : List Declared
        letDeclarations =
            List.filterMap
                (collectDeclared { range = letNodeRange, block = letBlock })
                letBlock.declarations

        scopes : RangeDict Scope
        scopes =
            letBlock.declarations
                |> List.filterMap getLetFunctionRange
                |> RangeDict.fromList

        newScope : Scope
        newScope =
            Scope
                LetScope
                { letDeclarations = letDeclarations
                , used = Set.empty
                , insertionLocation =
                    insertExistingLetIfPresent letNode
                        |> Maybe.withDefault (InsertNewLet { insert = (Node.range letNode).start, nextExpr = (Node.range letNode).start })
                , scopes = scopes
                , introducedVariables = Set.empty
                }

        contextWithDeclarationsMarked : Context
        contextWithDeclarationsMarked =
            { context | scope = markLetDeclarationsAsIntroducingVariables letNodeRange Set.empty context }

        branch : Scope
        branch =
            updateCurrentBranch
                (\b ->
                    { b
                        | scopes =
                            RangeDict.insert
                                letNodeRange
                                newScope
                                b.scopes
                    }
                )
                contextWithDeclarationsMarked.branching.full
                contextWithDeclarationsMarked.scope
    in
    { contextWithDeclarationsMarked
        | scope = branch
        , branching = addBranching (Node.range letNode) contextWithDeclarationsMarked.branching
    }


registerCaseExpression : Node Expression -> List ( Node Pattern, Node Expression ) -> Context -> Context
registerCaseExpression node cases context =
    let
        branchNodes : List ( Range, LetInsertPosition, Set String )
        branchNodes =
            List.map
                (\( pattern, (Node exprRange _) as exprNode ) ->
                    let
                        insertPosition : LetInsertPosition
                        insertPosition =
                            insertExistingLetIfPresent exprNode
                                |> onNothing
                                    (\() ->
                                        findKeyword "->" context.extractSourceCode { start = (Node.range pattern).end, end = exprRange.start }
                                            |> Maybe.map (\location -> InsertNewLet { insert = location, nextExpr = exprRange.start })
                                    )
                                |> withDefaultLazy (\() -> InsertNewLet { insert = exprRange.start, nextExpr = exprRange.start })
                    in
                    ( exprRange
                    , insertPosition
                    , introducedVariablesInPattern [ pattern ] Set.empty
                    )
                )
                cases

        introducedVariables : Set String
        introducedVariables =
            List.foldl (\( _, _, introduced ) soFar -> Set.union introduced soFar) Set.empty branchNodes

        contextWithDeclarationsMarked : Context
        contextWithDeclarationsMarked =
            if Set.isEmpty introducedVariables then
                context

            else
                { context | scope = markLetDeclarationsAsIntroducingVariables (Node.range node) introducedVariables context }
    in
    addBranches branchNodes contextWithDeclarationsMarked


registerApplicationCall : Range -> String -> Array (Node Expression) -> Context -> Context
registerApplicationCall fnRange fnName arguments context =
    case numberOfArgumentsForFunction context.lookupTable fnName fnRange of
        Just { numberOfArguments, indexOfFnsCalledOnce } ->
            if Array.length arguments >= numberOfArguments then
                let
                    functionsThatWillOnlyBeComputedOnce : RangeDict ()
                    functionsThatWillOnlyBeComputedOnce =
                        List.foldl
                            (\index acc ->
                                case Array.get index arguments |> Maybe.map removeParens of
                                    Just (Node range (Expression.LambdaExpression _)) ->
                                        RangeDict.insert range () acc

                                    _ ->
                                        acc
                            )
                            context.functionsThatWillOnlyBeComputedOnce
                            indexOfFnsCalledOnce
                in
                { context | functionsThatWillOnlyBeComputedOnce = functionsThatWillOnlyBeComputedOnce }

            else
                context

        Nothing ->
            context


numberOfArgumentsForFunction : ModuleNameLookupTable -> String -> Range -> Maybe FunctionData
numberOfArgumentsForFunction lookupTable fnName fnRange =
    case Dict.get fnName knownFunctions of
        Just knownModuleNames ->
            ModuleNameLookupTable.moduleNameAt lookupTable fnRange
                |> Maybe.andThen (\moduleName -> Dict.get moduleName knownModuleNames)

        Nothing ->
            Nothing


type alias FunctionData =
    { numberOfArguments : Int
    , indexOfFnsCalledOnce : List Int
    }


knownFunctions : Dict String (Dict ModuleName FunctionData)
knownFunctions =
    Dict.fromList
        [ ( "map"
          , Dict.fromList
                [ ( [ "Maybe" ], { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Html" ], { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Result" ], { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Task" ], { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] } )
                ]
          )
        , ( "map2"
          , Dict.fromList
                [ ( [ "Maybe" ], { numberOfArguments = 3, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Result" ], { numberOfArguments = 3, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Task" ], { numberOfArguments = 3, indexOfFnsCalledOnce = [ 0 ] } )
                ]
          )
        , ( "map3"
          , Dict.fromList
                [ ( [ "Maybe" ], { numberOfArguments = 4, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Result" ], { numberOfArguments = 4, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Task" ], { numberOfArguments = 4, indexOfFnsCalledOnce = [ 0 ] } )
                ]
          )
        , ( "map4"
          , Dict.fromList
                [ ( [ "Maybe" ], { numberOfArguments = 5, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Result" ], { numberOfArguments = 5, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Task" ], { numberOfArguments = 5, indexOfFnsCalledOnce = [ 0 ] } )
                ]
          )
        , ( "map5"
          , Dict.fromList
                [ ( [ "Maybe" ], { numberOfArguments = 6, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Result" ], { numberOfArguments = 6, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Task" ], { numberOfArguments = 6, indexOfFnsCalledOnce = [ 0 ] } )
                ]
          )
        , ( "mapError"
          , Dict.fromList
                [ ( [ "Result" ], { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Task" ], { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] } )
                ]
          )
        , ( "andThen"
          , Dict.fromList
                [ ( [ "Maybe" ], { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Result" ], { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] } )
                , ( [ "Task" ], { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] } )
                ]
          )
        , ( "mapFirst"
          , Dict.singleton [ "Tuple" ] { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] }
          )
        , ( "mapSecond"
          , Dict.singleton [ "Tuple" ] { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] }
          )
        , ( "mapBoth"
          , Dict.singleton [ "Tuple" ] { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0, 1 ] }
          )
        , ( "perform"
          , Dict.singleton [ "Task" ] { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] }
          )
        , ( "attempt"
          , Dict.singleton [ "Task" ] { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] }
          )
        , ( "onError"
          , Dict.singleton [ "Task" ] { numberOfArguments = 2, indexOfFnsCalledOnce = [ 0 ] }
          )
        , ( "update"
          , Dict.singleton [ "Dict" ] { numberOfArguments = 4, indexOfFnsCalledOnce = [ 0 ] }
          )
        ]


removeParens : Node Expression -> Node Expression
removeParens node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            removeParens expr

        _ ->
            node


patternRangeWithoutParens : Node Pattern -> Range
patternRangeWithoutParens node =
    case Node.value node of
        Pattern.ParenthesizedPattern expr ->
            patternRangeWithoutParens expr

        _ ->
            Node.range node


functionScope : Set String -> Scope
functionScope introducedVariables =
    Scope
        Function
        { letDeclarations = []
        , used = Set.empty
        , -- Will not be used
          insertionLocation = dummyInsertLocation
        , scopes = RangeDict.empty
        , introducedVariables = introducedVariables
        }


dummyInsertLocation : LetInsertPosition
dummyInsertLocation =
    InsertNewLet { insert = emptyLocation, nextExpr = emptyLocation }


emptyLocation : Location
emptyLocation =
    { row = 0, column = 0 }


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


introducedVariablesInPattern : List (Node Pattern) -> Set String -> Set String
introducedVariablesInPattern nodes names =
    case nodes of
        [] ->
            names

        (Node _ value) :: rest ->
            case value of
                Pattern.ListPattern patterns ->
                    introducedVariablesInPattern (patterns ++ rest) names

                Pattern.TuplePattern patterns ->
                    introducedVariablesInPattern (patterns ++ rest) names

                Pattern.RecordPattern fields ->
                    introducedVariablesInPattern rest
                        (List.foldl (\(Node _ name) acc -> Set.insert name acc) names fields)

                Pattern.UnConsPattern left right ->
                    introducedVariablesInPattern (left :: right :: rest) names

                Pattern.VarPattern name ->
                    introducedVariablesInPattern rest (Set.insert name names)

                Pattern.NamedPattern _ patterns ->
                    introducedVariablesInPattern (patterns ++ rest) names

                Pattern.AsPattern pattern (Node _ name) ->
                    introducedVariablesInPattern (pattern :: rest) (Set.insert name names)

                Pattern.ParenthesizedPattern pattern ->
                    introducedVariablesInPattern (pattern :: rest) names

                _ ->
                    introducedVariablesInPattern rest names


markLetDeclarationsAsIntroducingVariables : Range -> Set String -> Context -> Scope
markLetDeclarationsAsIntroducingVariables range names context =
    updateAllSegmentsOfCurrentBranch
        (markDeclarationsAsIntroducingVariables range names)
        context.branching.full
        context.scope


markDeclarationsAsIntroducingVariables : Range -> Set String -> ScopeData -> ScopeData
markDeclarationsAsIntroducingVariables range names branchData =
    { branchData | letDeclarations = List.map (markDeclarationAsIntroducingVariables range names) branchData.letDeclarations }


markDeclarationAsIntroducingVariables : Range -> Set String -> Declared -> Declared
markDeclarationAsIntroducingVariables range names declared =
    if isRangeContained { outer = declared.declarationRange, inner = range } then
        { declared | introducedVariables = Set.union declared.introducedVariables names }

    else
        declared


isRangeContained : { outer : Range, inner : Range } -> Bool
isRangeContained { outer, inner } =
    (Range.compareLocations outer.start inner.start /= GT)
        && (Range.compareLocations outer.end inner.end /= LT)


untilEndOfLine : Location -> Location
untilEndOfLine { row } =
    { row = row, column = 1000000 }


addBranches : List ( Range, LetInsertPosition, Set String ) -> Context -> Context
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


insertNewBranches : List ( Range, LetInsertPosition, Set String ) -> RangeDict Scope -> RangeDict Scope
insertNewBranches nodes rangeDict =
    case nodes of
        [] ->
            rangeDict

        ( range, letInsertPosition, introducedVariables ) :: tail ->
            insertNewBranches tail
                (RangeDict.insert
                    range
                    (newBranch letInsertPosition introducedVariables)
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
                            canBeMovedToCloserLocation True declaration.names Set.empty scope
                                |> List.head
                                |> Maybe.map (createError context declaration)
                        )
                        scopeData.letDeclarations

                _ ->
                    []

        _ ->
            []


collectDeclared :
    LetBlockWithRange
    -> Node Expression.LetDeclaration
    -> Maybe Declared
collectDeclared letBlock node =
    case Node.value node of
        Expression.LetFunction letFunction ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value letFunction.declaration
            in
            if List.isEmpty declaration.arguments then
                let
                    range : Range
                    range =
                        { start = (Node.range node).start
                        , end = (Node.range declaration.expression).end
                        }
                in
                { names = [ Node.value declaration.name ]
                , introducedVariables = introducedVariablesForNestedLetExpression declaration.expression
                , reportRange = Node.range declaration.name
                , declarationRange = range
                , letBlock = letBlock
                }
                    |> Just

            else
                Nothing

        Expression.LetDestructuring pattern expression ->
            case variablesInPattern pattern of
                first :: rest ->
                    let
                        range : Range
                        range =
                            { start = (Node.range node).start
                            , end = (Node.range expression).end
                            }
                    in
                    { names = Node.value first :: List.map Node.value rest
                    , introducedVariables = Set.empty
                    , reportRange =
                        if List.isEmpty rest then
                            Node.range first

                        else
                            patternRangeWithoutParens pattern
                    , declarationRange = range
                    , letBlock = letBlock
                    }
                        |> Just

                [] ->
                    Nothing


introducedVariablesForNestedLetExpression : Node Expression -> Set String
introducedVariablesForNestedLetExpression node =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            List.foldl
                (\decl acc ->
                    case Node.value decl of
                        Expression.LetFunction fn ->
                            Set.insert (Node.value (Node.value fn.declaration).name) acc

                        Expression.LetDestructuring pattern _ ->
                            List.foldl (\(Node _ name) subAcc -> Set.insert name subAcc) acc (variablesInPattern pattern)
                )
                Set.empty
                declarations

        _ ->
            Set.empty


getLetFunctionRange : Node Expression.LetDeclaration -> Maybe ( Range, Scope )
getLetFunctionRange node =
    case Node.value node of
        Expression.LetFunction { declaration } ->
            if List.isEmpty (Node.value declaration).arguments then
                Nothing

            else
                Just
                    ( declaration |> Node.value |> .expression |> Node.range
                    , functionScope (introducedVariablesInPattern (Node.value declaration).arguments Set.empty)
                    )

        Expression.LetDestructuring _ _ ->
            Nothing


canBeMovedToCloserLocation : Bool -> List String -> Set String -> Scope -> List ( LetInsertPosition, Set String )
canBeMovedToCloserLocation isRoot names introducedVariablesSoFar (Scope type_ scope) =
    let
        closestLocation : List ( LetInsertPosition, Set String )
        closestLocation =
            canBeMovedToCloserLocationForBranchData isRoot names (Set.union introducedVariablesSoFar scope.introducedVariables) scope
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


canBeMovedToCloserLocationForBranchData : Bool -> List String -> Set String -> ScopeData -> List ( LetInsertPosition, Set String )
canBeMovedToCloserLocationForBranchData isRoot names introducedVariablesSoFar branchData =
    if List.any (\name -> Set.member name branchData.used) names then
        if isRoot then
            []

        else
            [ ( branchData.insertionLocation, Set.union branchData.introducedVariables introducedVariablesSoFar ) ]

    else
        let
            relevantUsages : List ( LetInsertPosition, Set String )
            relevantUsages =
                findRelevantUsages names introducedVariablesSoFar (RangeDict.values branchData.scopes) []
        in
        case relevantUsages of
            [] ->
                []

            [ _ ] ->
                relevantUsages

            _ ->
                if isRoot then
                    []

                else
                    [ ( branchData.insertionLocation, branchData.introducedVariables ) ]


findRelevantUsages : List String -> Set String -> List Scope -> List ( LetInsertPosition, Set String ) -> List ( LetInsertPosition, Set String )
findRelevantUsages names introducedVariablesSoFar branches result =
    case result of
        _ :: _ :: _ ->
            -- If we have already found 2 branches with relevant usages, then we don't need to continue
            result

        _ ->
            case branches of
                [] ->
                    result

                first :: rest ->
                    findRelevantUsages names introducedVariablesSoFar rest (canBeMovedToCloserLocation False names introducedVariablesSoFar first ++ result)


createError : Context -> Declared -> ( LetInsertPosition, Set String ) -> Rule.Error {}
createError context declared ( letInsertPosition, introducedVariablesInTargetScope ) =
    let
        letInsertLine : Int
        letInsertLine =
            case letInsertPosition of
                InsertNewLet { insert, nextExpr } ->
                    Basics.min (insert.row + 1) nextExpr.row

                InsertExistingLet { insert, nextExpr } ->
                    Basics.min (insert.row + 1) nextExpr.row
    in
    Rule.errorWithFix
        (case declared.names of
            [ _ ] ->
                { message = "Let value was declared prematurely"
                , details =
                    [ "This value is only used in some code paths, and it can therefore be computed unnecessarily."
                    , "Try moving it closer to where it is needed, I recommend to move it to line " ++ String.fromInt letInsertLine ++ "."
                    ]
                }

            _ ->
                { message = "Let values were declared prematurely"
                , details =
                    [ "These values are only used in some code paths, and can therefore be computed unnecessarily."
                    , "Try moving them closer to where it is needed, I recommend to move them to line " ++ String.fromInt letInsertLine ++ "."
                    ]
                }
        )
        declared.reportRange
        (fix context declared letInsertPosition introducedVariablesInTargetScope)


fix : Context -> Declared -> LetInsertPosition -> Set String -> List Fix
fix context declared letInsertPosition introducedVariablesInTargetScope =
    if not (Set.isEmpty (Set.intersect declared.introducedVariables introducedVariablesInTargetScope)) then
        []

    else
        case letInsertPosition of
            InsertNewLet { insert, nextExpr } ->
                case declared.letBlock.block.declarations of
                    [] ->
                        []

                    [ _ ] ->
                        -- Moving the entire block between `let` and `in`
                        let
                            startColumn : Int
                            startColumn =
                                declared.letBlock.range.start.column

                            range : Range
                            range =
                                rangeFromLetUntilInKeywords context.extractSourceCode declared.letBlock

                            source : String
                            source =
                                (String.repeat (startColumn - 1) " " ++ context.extractSourceCode range)
                                    |> indent (nextExpr.column - startColumn)
                        in
                        [ Fix.removeRange range
                        , Fix.insertAt insert ("\n" ++ source)
                        ]

                    declarations ->
                        let
                            startColumn : Int
                            startColumn =
                                declared.letBlock.range.start.column

                            endOfPrevious : Location
                            endOfPrevious =
                                findMapWithAcc
                                    (\(Node itemRange _) previous ->
                                        if itemRange == declared.declarationRange then
                                            Ok previous

                                        else
                                            Err itemRange.end
                                    )
                                    { row = declared.letBlock.range.start.row
                                    , column = declared.letBlock.range.start.column + 3
                                    }
                                    declarations

                            range : Range
                            range =
                                { start = untilEndOfLine endOfPrevious
                                , end = untilEndOfLine declared.declarationRange.end
                                }

                            spacing : String
                            spacing =
                                if insert.row == nextExpr.row then
                                    " "

                                else
                                    "\n" ++ String.repeat (startColumn - 1) " "
                        in
                        [ Fix.removeRange range
                        , ("\n" ++ String.repeat (startColumn - 1) " " ++ "let" ++ context.extractSourceCode range)
                            ++ spacing
                            ++ "in"
                            |> indent (nextExpr.column - startColumn)
                            |> Fix.insertAt insert
                        ]

            InsertExistingLet { insert, nextExpr } ->
                let
                    startColumn : Int
                    startColumn =
                        declared.declarationRange.start.column
                in
                if nextExpr.column - startColumn < 0 then
                    -- If the source declaration is more indented than the target destination
                    -- then we would have to dedent the declaration, which potentially means removing comments in that space.
                    -- It's better to let elm-format reformat the code, and attempt again later.
                    []

                else if insert.row == nextExpr.row then
                    {- If the first declaration is on the same line as the `let` keyword
                       then skip a fix. It's quite difficult especially if we take leading comments into account

                         let {--} a = 1 in ...

                       would have to be transformed like

                         let      toMove = 1
                             {--} a = 1 in ...

                       It's better to let elm-format reformat the code, and attempt again later.
                    -}
                    []

                else
                    case declared.letBlock.block.declarations of
                        [] ->
                            []

                        [ _ ] ->
                            let
                                rangeToRemove : Range
                                rangeToRemove =
                                    rangeFromLetUntilInKeywords context.extractSourceCode declared.letBlock

                                rangeToCopy : Range
                                rangeToCopy =
                                    { start = { row = rangeToRemove.start.row, column = rangeToRemove.start.column + 3 }
                                    , end = { row = rangeToRemove.end.row, column = rangeToRemove.end.column - 2 }
                                    }
                            in
                            [ Fix.removeRange rangeToRemove
                            , context.extractSourceCode rangeToCopy
                                |> indent (nextExpr.column - startColumn)
                                |> Fix.insertAt insert
                            ]

                        declarations ->
                            let
                                endOfPrevious : Location
                                endOfPrevious =
                                    findMapWithAcc
                                        (\(Node itemRange _) previous ->
                                            if itemRange == declared.declarationRange then
                                                Ok previous

                                            else
                                                Err itemRange.end
                                        )
                                        { row = declared.letBlock.range.start.row
                                        , column = declared.letBlock.range.start.column + 3
                                        }
                                        declarations

                                range : Range
                                range =
                                    { start = untilEndOfLine endOfPrevious
                                    , end = untilEndOfLine declared.declarationRange.end
                                    }
                            in
                            [ Fix.removeRange range
                            , context.extractSourceCode range
                                |> indent (nextExpr.column - startColumn)
                                |> Fix.insertAt insert
                            ]


rangeFromLetUntilInKeywords : (Range -> String) -> LetBlockWithRange -> Range
rangeFromLetUntilInKeywords extractSourceCode { range, block } =
    let
        lastDeclarationEnd : Location
        lastDeclarationEnd =
            case last block.declarations of
                Just (Node lastDeclarationRange _) ->
                    lastDeclarationRange.end

                Nothing ->
                    range.start

        exprStart : Location
        exprStart =
            (Node.range block.expression).start

        end : Location
        end =
            findKeyword "in" extractSourceCode { start = lastDeclarationEnd, end = exprStart }
                |> Maybe.withDefault exprStart
    in
    { start = range.start
    , end = end
    }


indent : Int -> String -> String
indent padding source =
    let
        paddingStr : String
        paddingStr =
            String.repeat padding " "
    in
    source
        |> String.lines
        |> List.map (\line -> paddingStr ++ line ++ "")
        |> String.join "\n"


last : List a -> Maybe a
last items =
    case items of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: rest ->
            last rest


indexedFindMap : (Int -> a -> Maybe b) -> List a -> Maybe b
indexedFindMap fn list =
    indexedFindMapHelp fn list 0


indexedFindMapHelp : (Int -> a -> Maybe b) -> List a -> Int -> Maybe b
indexedFindMapHelp fn list index =
    case list of
        [] ->
            Nothing

        a :: rest ->
            case fn index a of
                (Just _) as just ->
                    just

                Nothing ->
                    indexedFindMapHelp fn rest (index + 1)


findMapWithAcc : (a -> b -> Result b b) -> b -> List a -> b
findMapWithAcc fn acc list =
    case list of
        [] ->
            acc

        item :: rest ->
            case fn item acc of
                Ok b ->
                    b

                Err b ->
                    findMapWithAcc fn b rest


{-| Like `Maybe.andThen` but for the `Nothing` case.
Useful for trying multiple things in order.
-}
onNothing : (() -> Maybe a) -> Maybe a -> Maybe a
onNothing nextTry maybe =
    case maybe of
        (Just _) as just ->
            just

        Nothing ->
            nextTry ()


withDefaultLazy : (() -> a) -> Maybe a -> a
withDefaultLazy default maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            default ()
