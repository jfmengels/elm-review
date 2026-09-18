module Elm.TypeInference.SCC exposing (stronglyConnectedComponents)

{-| Strongly Connected Components (Tarjan's algorithm)

Gathers graph nodes into groups based on dependencies. In each group, each node
is reachable from every other node.

Used eg. for let-in bindings or mutually recursive declarations (isEven and
isOdd defined in terms of each other).

Each inner group can then be solved in isolation (with access to previously
solved groups).

It also helps let-polymorphism as a type can be generalized only after its
equations are fully collected. Each group is solved monomorphically before any
generalization.

-}

import Dict exposing (Dict)
import Set exposing (Set)


{-| Examples:

Independent declarations each get their own group:

    stronglyConnectedComponents [ "x", "y", "z" ] (\_ -> [])
    --> [ [ "x" ], [ "y" ], [ "z" ] ]

A dependency chain comes back dependencies-first:

    -- main = double x
    -- double x = x + x
    -- x = 1
    edges node =
        case node of
            "main" -> ["double"]
            "double" -> ["x"]
            "x" -> []
            _ -> []

    stronglyConnectedComponents [ "main", "double", "x" ] edges
    --> [ [ "x" ], [ "double" ], [ "main" ] ]

Mutually recursive bindings must be solved together:

    -- isEven n = if n == 0 then True else isOdd n
    -- isOdd n = if n == 0 then False else isEven n
    edges node =
        case node of
            "isEven" ->  ["isOdd"]
            "isOdd" -> ["isEven"]
            _ -> []

    stronglyConnectedComponents [ "isEven", "isOdd" ] edges

--> [ [ "isOdd", "isEven" ] ]

-}
stronglyConnectedComponents :
    List comparable
    -> (comparable -> List comparable)
    -> List (List comparable)
stronglyConnectedComponents nodes edges =
    let
        initAcc : Acc comparable
        initAcc =
            { index = Dict.empty
            , lowlink = Dict.empty
            , onStack = Set.empty
            , nodeStack = []
            , sccs = []
            , counter = 0
            }

        finalAcc : Acc comparable
        finalAcc =
            List.foldl (visit edges) initAcc nodes
    in
    List.reverse finalAcc.sccs


type alias Frame comparable =
    { node : comparable
    , remaining : List comparable
    }


type alias Acc comparable =
    { index : Dict comparable Int
    , lowlink : Dict comparable Int
    , onStack : Set comparable
    , nodeStack : List comparable
    , sccs : List (List comparable)
    , counter : Int
    }


visit : (comparable -> List comparable) -> comparable -> Acc comparable -> Acc comparable
visit edges start acc =
    if Dict.member start acc.index then
        acc

    else
        runFrames
            edges
            [ { node = start, remaining = edges start } ]
            (initNode start acc)


initNode : comparable -> Acc comparable -> Acc comparable
initNode v acc =
    { index = Dict.insert v acc.counter acc.index
    , lowlink = Dict.insert v acc.counter acc.lowlink
    , onStack = Set.insert v acc.onStack
    , nodeStack = v :: acc.nodeStack
    , sccs = acc.sccs
    , counter = acc.counter + 1
    }


{-| DFS.
-}
runFrames : (comparable -> List comparable) -> List (Frame comparable) -> Acc comparable -> Acc comparable
runFrames edges frames acc =
    case frames of
        [] ->
            acc

        frame :: outerFrames ->
            case frame.remaining of
                [] ->
                    -- Done exploring `frame.node`'s neighbours.
                    let
                        v : comparable
                        v =
                            frame.node

                        vIndex : Int
                        vIndex =
                            Dict.get v acc.index |> Maybe.withDefault -1

                        vLowlink : Int
                        vLowlink =
                            Dict.get v acc.lowlink |> Maybe.withDefault -1

                        accAfterPop : Acc comparable
                        accAfterPop =
                            if vLowlink == vIndex then
                                let
                                    ( component, remainingStack ) =
                                        splitOffComponent v acc.nodeStack
                                in
                                { index = acc.index
                                , lowlink = acc.lowlink
                                , onStack = List.foldl Set.remove acc.onStack component
                                , nodeStack = remainingStack
                                , sccs = component :: acc.sccs
                                , counter = acc.counter
                                }

                            else
                                acc
                    in
                    case outerFrames of
                        [] ->
                            runFrames edges outerFrames accAfterPop

                        parent :: _ ->
                            let
                                parentLowlink : Int
                                parentLowlink =
                                    Dict.get parent.node accAfterPop.lowlink |> Maybe.withDefault -1
                            in
                            runFrames edges
                                outerFrames
                                { index = accAfterPop.index
                                , lowlink = Dict.insert parent.node (min parentLowlink vLowlink) accAfterPop.lowlink
                                , onStack = accAfterPop.onStack
                                , nodeStack = accAfterPop.nodeStack
                                , sccs = accAfterPop.sccs
                                , counter = accAfterPop.counter
                                }

                w :: ws ->
                    let
                        framesWithNextNeighbour : List (Frame comparable)
                        framesWithNextNeighbour =
                            { node = frame.node
                            , remaining = ws
                            }
                                :: outerFrames
                    in
                    if not (Dict.member w acc.index) then
                        -- Tree edge: recurse into `w`.
                        runFrames edges
                            ({ node = w, remaining = edges w } :: framesWithNextNeighbour)
                            (initNode w acc)

                    else if Set.member w acc.onStack then
                        let
                            wIndex : Int
                            wIndex =
                                Dict.get w acc.index |> Maybe.withDefault -1

                            vLowlink : Int
                            vLowlink =
                                Dict.get frame.node acc.lowlink |> Maybe.withDefault -1
                        in
                        runFrames edges
                            framesWithNextNeighbour
                            { index = acc.index
                            , lowlink = Dict.insert frame.node (min vLowlink wIndex) acc.lowlink
                            , onStack = acc.onStack
                            , nodeStack = acc.nodeStack
                            , sccs = acc.sccs
                            , counter = acc.counter
                            }

                    else
                        -- `w` belongs to an already-completed component
                        runFrames edges framesWithNextNeighbour acc


splitOffComponent : comparable -> List comparable -> ( List comparable, List comparable )
splitOffComponent v stack =
    case stack of
        [] ->
            ( [], [] )

        x :: rest ->
            if x == v then
                ( [ x ], rest )

            else
                let
                    ( component, remaining ) =
                        splitOffComponent v rest
                in
                ( x :: component, remaining )
