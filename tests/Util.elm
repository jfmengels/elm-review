module Util exposing
    ( GluedTo(..)
    , allBindingsInPattern
    , checkSorting
    , checkSortingWithGlue
    , countUsesIn
    , fallbackCompareFor
    , fallbackCompareWithUnsortableFor
    , findAllNamesIn
    , findDependencies
    , makeAccessFunc
    , validate
    )

{-| Utility functions used by other modules but not specific to them.
-}

import Dict exposing (Dict)
import Dict.Extra as DictX
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Graph exposing (AcyclicGraph, Edge, Graph, NodeContext)
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Result.Extra as ResultX
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Error)
import Set exposing (Set)


{-| Get all immediate child expressions of an expression.
-}
subexpressions : Node Expression -> List (Node Expression)
subexpressions expr =
    case Node.value expr of
        LetExpression letBlock ->
            let
                subExprs : Node LetDeclaration -> Node Expression
                subExprs n =
                    case Node.value n of
                        LetFunction { declaration } ->
                            Node.value declaration
                                |> .expression

                        LetDestructuring _ e ->
                            e
            in
            letBlock.expression
                :: List.map subExprs letBlock.declarations

        ListExpr es ->
            es

        TupledExpression es ->
            es

        RecordExpr setters ->
            List.map (Tuple.second << Node.value) setters

        RecordUpdateExpression record updaters ->
            Node.map (FunctionOrValue []) record
                :: List.map (Tuple.second << Node.value) updaters

        Application es ->
            es

        CaseExpression caseBlock ->
            caseBlock.expression
                :: List.map Tuple.second caseBlock.cases

        OperatorApplication _ _ e1 e2 ->
            [ e1, e2 ]

        IfBlock predExpr thenExpr elseExpr ->
            [ predExpr, thenExpr, elseExpr ]

        LambdaExpression { expression } ->
            [ expression ]

        RecordAccess record _ ->
            [ record ]

        ParenthesizedExpression e ->
            [ e ]

        Negation e ->
            [ e ]

        UnitExpr ->
            []

        Integer _ ->
            []

        Hex _ ->
            []

        Floatable _ ->
            []

        Literal _ ->
            []

        CharLiteral _ ->
            []

        GLSLExpression _ ->
            []

        RecordAccessFunction _ ->
            []

        FunctionOrValue _ _ ->
            []

        Operator _ ->
            []

        PrefixOperator _ ->
            []


{-| Recursively find all bindings in a pattern.
-}
allBindingsInPattern : Node Pattern -> List String
allBindingsInPattern pattern =
    let
        go : List (Node Pattern) -> List String
        go =
            List.concatMap allBindingsInPattern
    in
    case Node.value pattern of
        ListPattern ps ->
            go ps

        TuplePattern ps ->
            go ps

        RecordPattern ps ->
            List.map Node.value ps

        NamedPattern _ ps ->
            go ps

        UnConsPattern p ps ->
            go [ p, ps ]

        VarPattern name ->
            [ name ]

        AsPattern p name ->
            Node.value name :: go [ p ]

        ParenthesizedPattern p ->
            go [ p ]

        AllPattern ->
            []

        UnitPattern ->
            []

        CharPattern _ ->
            []

        StringPattern _ ->
            []

        IntPattern _ ->
            []

        HexPattern _ ->
            []

        FloatPattern _ ->
            []


{-| Count the uses of a given name in the scope of an expression.
-}
countUsesIn : Node Expression -> String -> Int
countUsesIn expr name =
    case Node.value expr of
        -- If the name is qualified, it isn't a variable
        FunctionOrValue [] n ->
            if n == name then
                1

            else
                0

        _ ->
            subexpressions expr
                -- Count and sum in one pass
                |> List.foldl (\e -> (+) (countUsesIn e name)) 0


{-| Find all (local) names used in an expression.
-}
findAllNamesIn : Node Expression -> Set String
findAllNamesIn expr =
    case Node.value expr of
        -- If the name is qualified, it isn't a variable
        FunctionOrValue [] n ->
            Set.singleton n

        _ ->
            subexpressions expr
                |> List.foldl (\e -> Set.union (findAllNamesIn e)) Set.empty


{-| Use the first order, or use the second order if the first is `Just EQ`. This
is lazy in the second comparison and written for use in pipeline-style code.
-}
fallbackCompareWithUnsortableFor : Maybe Order -> (() -> Maybe Order) -> Maybe Order
fallbackCompareWithUnsortableFor comp fallback =
    case comp of
        Just EQ ->
            fallback ()

        ltOrGtOrNothing ->
            ltOrGtOrNothing


{-| Use the first order, or use the second order if the first is `EQ`. This is
lazy in the second comparison and written for use in pipeline-style code, e.g.
in implementing a stable sort below:

    (\() -> compare index1 index2)
        |> fallbackCompareFor (compare element1 element2)

-}
fallbackCompareFor : Order -> (() -> Order) -> Order
fallbackCompareFor comp fallback =
    case comp of
        EQ ->
            fallback ()

        ltOrGt ->
            ltOrGt


{-| Given a source code extractor and a sorted list of ranges (with original
indices), create fixes to resort the source code to the list.
-}
createFix : (Range -> String) -> List ( Int, Range ) -> List Fix
createFix extractSource sorted =
    let
        applyFix : Int -> ( Int, Range ) -> List Fix
        applyFix newIndex ( oldIndex, range ) =
            if newIndex == oldIndex then
                []

            else
                ListX.find ((==) newIndex << Tuple.first) sorted
                    |> Maybe.map Tuple.second
                    |> Maybe.map
                        (\oldRange ->
                            extractSource range
                                |> Fix.replaceRangeBy oldRange
                                |> List.singleton
                        )
                    |> Maybe.withDefault []
    in
    List.indexedMap applyFix sorted
        |> List.concat


{-| Given context and a list of ordering functions, check if a list is sorted
and generate errors if it isn't. Ordering functions are applied in order, with
ties being broken by the next function in the list. Earlier ordering functions
will never be invalidated by later ones (i.e. there is no transitivity
assumptions made between ordering functions).
-}
checkSorting : (Range -> String) -> String -> List ({ a | range : Range } -> { a | range : Range } -> Order) -> Range -> List { a | range : Range } -> List (Error {})
checkSorting extractSource errorConcerns orderings errorRange ds =
    let
        indexed : List (Graph.Node { a | range : Range })
        indexed =
            List.indexedMap (\i d -> { id = i, label = d }) ds
    in
    (case orderings of
        [ o ] ->
            -- If there is only one sorting function, we needn't worry about
            -- weird transitivity issues, so simply sort stably.
            List.sortWith
                (\d1 d2 ->
                    (\() -> compare d1.id d2.id)
                        |> fallbackCompareFor (o d1.label d2.label)
                )
                indexed

        os ->
            -- Otherwise, generate all pairwise edges, then eliminate cycles by
            -- ignoring lower priority sorts first.
            genEdges os indexed
                |> Graph.fromNodesAndEdges indexed
                |> eliminateCycles indexed
                |> Graph.topologicalSort
                |> List.map .node
    )
        -- Check if sorted
        |> (\sorted ->
                if List.map .id sorted /= List.map .id indexed then
                    -- Generate a fix if unsorted
                    List.map (\{ id, label } -> ( id, label.range )) sorted
                        |> createFix extractSource
                        |> unsortedError errorConcerns errorRange
                        |> List.singleton

                else
                    []
           )


{-| Given a graph with edges labeled by priority (larger number is lower
priority, convert it to an acyclic graph by removing edges, with the guarantee
that all lower-priority edges will be removed before any of higher priority
are.
-}
eliminateCycles : List (Graph.Node a) -> Graph a Int -> AcyclicGraph a Int
eliminateCycles nodes g =
    -- Check if it is acyclic
    Graph.stronglyConnectedComponents g
        |> ResultX.extract
            (List.map
                -- If not, for each strongly-connected subgraph
                (\graph ->
                    let
                        ns : List (Graph.Node a)
                        ns =
                            Graph.nodes graph

                        ( lowestPriorityEdges, higherPriorityEdges ) =
                            -- Get all edges
                            Graph.edges graph
                                -- Group them by priority
                                |> DictX.groupBy .label
                                |> Dict.values
                                -- Select lowest priority edges
                                |> ListX.unconsLast
                                |> Maybe.withDefault ( [], [] )
                                |> Tuple.mapBoth
                                    (List.map (\{ from, to } -> ( from, to ))
                                        >> Set.fromList
                                    )
                                    List.concat
                    in
                    Set.filter
                        -- Filter low priority edges by removing any that by the insertion of just that edge create a cyclic subgraph
                        (\( from, to ) ->
                            Graph.fromNodesAndEdges ns ({ from = from, to = to, label = -1 } :: higherPriorityEdges)
                                |> Graph.checkAcyclic
                                |> ResultX.isErr
                        )
                        lowestPriorityEdges
                        |> (\s ->
                                if Set.isEmpty s then
                                    -- Don't risk the infinite loop and delete them all if none of them appeared cyclic individually
                                    lowestPriorityEdges

                                else
                                    s
                           )
                )
                >> List.foldl Set.union Set.empty
                -- Eliminate all identified edges and rebuild graph
                >> (\toRemove ->
                        Graph.edges g
                            |> List.filter (\{ from, to } -> not <| Set.member ( from, to ) toRemove)
                            |> Graph.fromNodesAndEdges nodes
                            -- Repeat the process if necessary
                            |> eliminateCycles nodes
                   )
            )


{-| Generate edges for every pairwise combination of nodes, along with their
priority (lower number is higher priority).
-}
genEdges : List ({ a | range : Range } -> { a | range : Range } -> Order) -> List (Graph.Node { a | range : Range }) -> List (Edge Int)
genEdges orderings indexed =
    let
        genEdge : List ( Int, { a | range : Range } -> { a | range : Range } -> Order ) -> ( Graph.Node { a | range : Range }, Graph.Node { a | range : Range } ) -> Maybe (Edge Int)
        genEdge os ( d1, d2 ) =
            case os of
                ( priority, o ) :: os_ ->
                    case o d1.label d2.label of
                        EQ ->
                            genEdge os_ ( d1, d2 )

                        LT ->
                            Just { from = d1.id, to = d2.id, label = priority }

                        GT ->
                            Just { from = d2.id, to = d1.id, label = priority }

                [] ->
                    -- Unsortable, so simply use original order
                    case compare d1.id d2.id of
                        LT ->
                            Just { from = d1.id, to = d2.id, label = 0 }

                        GT ->
                            Just { from = d2.id, to = d1.id, label = 0 }

                        EQ ->
                            -- This should never happen
                            Nothing
    in
    ListX.uniquePairs indexed
        |> List.filterMap (genEdge <| List.indexedMap Tuple.pair orderings)


{-| Given a list of ordering functions for breaking ties, order to things.
-}
compareByOrderings : List (a -> a -> Order) -> a -> a -> Order
compareByOrderings orderings d1 d2 =
    let
        go : List (a -> a -> Order) -> Order
        go os =
            case os of
                [] ->
                    EQ

                o :: os_ ->
                    (\() -> go os_)
                        |> fallbackCompareFor (o d1 d2)
    in
    go orderings


{-| Specify how something is glued to another (by name).
-}
type GluedTo
    = GluedBeforeFirst (Set String)
    | GluedAfterFirst (Set String)
    | GluedBeforeLast (Set String)
    | GluedAfterLast (Set String)


{-| Given context and a list of ordering functions, check if a list is sorted
and generate errors if it isn't.
-}
checkSortingWithGlue : (Range -> String) -> String -> List ({ a | namesBound : Set String, glued : Maybe GluedTo, range : Range } -> { a | namesBound : Set String, glued : Maybe GluedTo, range : Range } -> Order) -> Range -> List { a | namesBound : Set String, glued : Maybe GluedTo, range : Range } -> List (Error {})
checkSortingWithGlue extractSource errorConcerns orderings errorRange ds =
    let
        insertGlued : List (Graph.Node { a | namesBound : Set String, glued : Maybe GluedTo, range : Range }) -> Graph.Node { a | namesBound : Set String, glued : Maybe GluedTo, range : Range } -> { toGlue : List (Graph.Node { a | namesBound : Set String, glued : Maybe GluedTo, range : Range }), inserted : List (Graph.Node { a | namesBound : Set String, glued : Maybe GluedTo, range : Range }) }
        insertGlued glued ({ label } as dec) =
            -- Keep only glued that are glued to this name
            List.partition
                (.label
                    >> .glued
                    >> MaybeX.unwrap False (not << Set.isEmpty << Set.intersect label.namesBound << gluedTo)
                )
                glued
                -- Split into glued before and after
                |> Tuple.mapFirst (List.partition (MaybeX.unwrap False isGluedBefore << .glued << .label))
                -- Flatten
                |> (\( ( gluedBefore, gluedAfter ), toGlue ) -> { toGlue = toGlue, inserted = gluedBefore ++ dec :: gluedAfter })

        indexed : List (Graph.Node { a | namesBound : Set String, glued : Maybe GluedTo, range : Range })
        indexed =
            List.indexedMap (\i d -> { id = i, label = d }) ds

        sort : List (Graph.Node { a | namesBound : Set String, glued : Maybe GluedTo, range : Range }) -> List (Graph.Node { a | namesBound : Set String, glued : Maybe GluedTo, range : Range })
        sort =
            List.sortWith
                (\d1 d2 ->
                    -- Sort stably
                    (\() -> compare d1.id d2.id)
                        |> fallbackCompareFor (compareByOrderings orderings d1.label d2.label)
                )

        glueLevel : List (Graph.Node { a | namesBound : Set String, glued : Maybe GluedTo, range : Range }) -> List (List (NodeContext { a | namesBound : Set String, glued : Maybe GluedTo, range : Range } Int)) -> List (Graph.Node { a | namesBound : Set String, glued : Maybe GluedTo, range : Range })
        glueLevel sorted glued =
            case glued of
                [] ->
                    sorted

                g :: gs ->
                    let
                        asList : List (Graph.Node { a | namesBound : Set String, glued : Maybe GluedTo, range : Range })
                        asList =
                            List.map .node g
                                |> sort
                    in
                    (if List.isEmpty sorted then
                        -- Initial unglued items
                        asList

                     else
                        -- Glue items
                        List.partition (MaybeX.unwrap False isGluedToFirst << .glued << .label) asList
                            |> (\( toFirst, toLast ) ->
                                    List.foldl
                                        (\d { toGlue, inserted } ->
                                            insertGlued toGlue d
                                                |> (\r -> { r | inserted = inserted ++ r.inserted })
                                        )
                                        { toGlue = toFirst, inserted = [] }
                                        sorted
                                        |> .inserted
                                        |> List.foldr
                                            (\d { toGlue, inserted } ->
                                                insertGlued toGlue d
                                                    |> (\r -> { r | inserted = r.inserted ++ inserted })
                                            )
                                            { toGlue = toLast, inserted = [] }
                                        |> .inserted
                               )
                    )
                        |> (\sorted_ -> glueLevel sorted_ gs)
    in
    gluedListToDAG indexed
        -- Sort by dependency (gluing)
        |> Graph.heightLevels
        -- Glue each level
        |> glueLevel []
        -- Check if sorted
        |> (\sorted ->
                if List.map .id sorted /= List.map .id indexed then
                    -- Generate a fix if unsorted
                    List.map (\{ id, label } -> ( id, label.range )) sorted
                        |> createFix extractSource
                        |> unsortedError errorConcerns errorRange
                        |> List.singleton

                else
                    []
           )


{-| Extract what names something is glued to.
-}
gluedTo : GluedTo -> Set String
gluedTo g =
    case g of
        GluedBeforeFirst ns ->
            ns

        GluedAfterFirst ns ->
            ns

        GluedBeforeLast ns ->
            ns

        GluedAfterLast ns ->
            ns


{-| Check if something is glued to the first or last of its glued list.
-}
isGluedToFirst : GluedTo -> Bool
isGluedToFirst g =
    case g of
        GluedBeforeFirst _ ->
            True

        GluedAfterFirst _ ->
            True

        GluedBeforeLast _ ->
            False

        GluedAfterLast _ ->
            False


{-| Check if something is glued before or after that to which it is glued.
-}
isGluedBefore : GluedTo -> Bool
isGluedBefore g =
    case g of
        GluedBeforeFirst _ ->
            True

        GluedAfterFirst _ ->
            False

        GluedBeforeLast _ ->
            True

        GluedAfterLast _ ->
            False


{-| Given a list of glued TLDs, remove any glues that are cyclic by converting
it to a directed acyclic graph, where edges indicate gluing dependencies (i.e.
A -> B means B is glued to A).
-}
gluedListToDAG : List (Graph.Node { a | namesBound : Set String, glued : Maybe GluedTo }) -> AcyclicGraph { a | namesBound : Set String, glued : Maybe GluedTo } Int
gluedListToDAG ds =
    let
        namesToNodeId : Dict String Int
        namesToNodeId =
            List.concatMap (\{ id, label } -> List.map (\n -> ( n, id )) <| Set.toList label.namesBound) ds
                |> Dict.fromList

        edges : List (Edge Int)
        edges =
            -- There can be duplicate edges, but that is fine, since the graph only keeps one
            List.concatMap
                (\{ id, label } ->
                    MaybeX.unwrap [] (Set.toList << gluedTo) label.glued
                        |> List.filterMap
                            (\n ->
                                Dict.get n namesToNodeId
                                    |> Maybe.map (\from -> { from = from, to = id, label = 0 })
                            )
                )
                ds
    in
    Graph.fromNodesAndEdges ds edges
        |> eliminateCycles ds


{-| Find all dependencies of a declaration.
-}
findDependencies : ( Int, { a | dependentOnBindings : Set String, namesBound : Set String } ) -> List { a | dependentOnBindings : Set String, namesBound : Set String } -> ( Set String, Int )
findDependencies ( decI, d ) ds =
    ListX.indexedFoldl
        (\i { dependentOnBindings, namesBound } (( glueAcc, numberUsedIn ) as acc) ->
            -- Cannot glue to itself
            if i == decI || (Set.isEmpty <| Set.intersect d.namesBound dependentOnBindings) then
                acc

            else
                ( Set.union glueAcc namesBound, numberUsedIn + 1 )
        )
        ( Set.empty, 0 )
        ds


{-| Given a range and a fix, create an unsorted case error.
-}
unsortedError : String -> Range -> List Fix -> Error {}
unsortedError errorConcerns =
    Rule.errorWithFix
        { message = errorConcerns ++ " are not sorted."
        , details =
            [ errorConcerns ++ " were found out of order.  They should be sorted as specified in the rule configuration."
            ]
        }


{-| Keep a value only if it passes a predicate. Like `Maybe.Extra.filter`, but
does not take a `Maybe` as input.
-}
validate : (a -> Bool) -> a -> Maybe a
validate pred x =
    if pred x then
        Just x

    else
        Nothing


{-| Work around `elm-syntax` sometimes including a period in record access
functions.
-}
makeAccessFunc : String -> String
makeAccessFunc accessFunc =
    if String.startsWith "." accessFunc then
        -- Work around elm-syntax behavior
        String.dropLeft 1 accessFunc

    else
        accessFunc
