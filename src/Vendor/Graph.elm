module Vendor.Graph exposing
    ( NodeId, Node, Edge, Adjacency, NodeContext, Graph
    , addNode
    , get
    , fromNodesAndEdges
    , AcyclicGraph, checkAcyclic
    , NeighborSelector, alongIncomingEdges, SimpleNodeVisitor
    , DfsNodeVisitor
    , BfsNodeVisitor, guidedBfs
    , topologicalSort
    )

{-| This module contains the primitives to build, update and traverse graphs.
If you find that this module is hard to use or the documentation
is insufficient, consider opening an issue for that (and possibly even a
pull request :)).

Internally, we use the `elm-intdict` package for efficient dynamic graph
representation.


# Data

@docs NodeId, Node, Edge, Adjacency, NodeContext, Graph


# Building

@docs addNode


# Query

@docs get


# List representations

@docs fromNodesAndEdges


# Transforms


# Characterization

@docs AcyclicGraph, checkAcyclic


# Traversals


## Neighbor selectors and node visitors

@docs NeighborSelector, alongIncomingEdges, SimpleNodeVisitor


## Depth-first

@docs DfsNodeVisitor


## Breadth-first

@docs BfsNodeVisitor, guidedBfs


# Topological Sort

@docs topologicalSort

-}

import Vendor.Fifo as Fifo
import Vendor.Graph.Hack
import Vendor.IntDict as IntDict exposing (IntDict)


{-| The type used for identifying nodes, an integer.
-}
type alias NodeId =
    Int


{-| The type representing a node: An identifier with
a label.
-}
type alias Node n =
    { id : NodeId
    , label : n
    }


{-| Represents a directd edge in the graph. In addition
to start and end node identifiers, a label value can
be attached to an edge.
-}
type alias Edge e =
    { from : NodeId
    , to : NodeId
    , label : e
    }


{-| Adjacency is represented as an ordered dictionary
rather than as an ordered list. This enables more dynamic
graphs with efficient edge removal and insertion on the run.
-}
type alias Adjacency e =
    IntDict e


{-| Represents a node with its incoming and outgoing edges
(predecessors and successors).
-}
type alias NodeContext n e =
    { node : Node n
    , incoming : Adjacency e
    , outgoing : Adjacency e
    }



-- We will only have the Patricia trie based DynGraph implementation for simplicity.
-- Also, there is no real practical reason to separate that or to allow other implementations
-- which would justify the complexity.


type alias GraphRep n e =
    IntDict (NodeContext n e)


{-| The central graph type. It is parameterized both over the node label type `n`
and the edge label type `e`.

One can build such a graph with the primitives under _Build_. Most of the time
`fromNodesAndEdges` works fairly well.

For simplicity, this library just uses a patricia trie based graph representation, which means
it is just an efficient version of `Dict NodeId (NodeContext n e)`. This allows efficient insertion and
removal of nodes of the graph after building.

-}
type Graph n e
    = Graph (GraphRep n e)


unGraph : Graph n e -> GraphRep n e
unGraph (Graph rep) =
    rep



{- BUILD -}


type EdgeUpdate e
    = Insert e
    | Remove e


type alias EdgeDiff e =
    { incoming : IntDict (EdgeUpdate e)
    , outgoing : IntDict (EdgeUpdate e)
    }


emptyDiff : EdgeDiff e
emptyDiff =
    { incoming = IntDict.empty
    , outgoing = IntDict.empty
    }


computeEdgeDiff : Maybe (NodeContext n e) -> Maybe (NodeContext n e) -> EdgeDiff e
computeEdgeDiff old new =
    let
        collectUpdates edgeUpdate updatedId label =
            let
                replaceUpdate old_ =
                    case ( old_, edgeUpdate label ) of
                        ( Just (Remove oldLbl), Insert newLbl ) ->
                            if oldLbl == newLbl then
                                Nothing

                            else
                                Just (Insert newLbl)

                        ( Just (Remove _), Remove _ ) ->
                            Vendor.Graph.Hack.crashHack "Graph.computeEdgeDiff: Collected two removals for the same edge. This is an error in the implementation of Graph and you should file a bug report!"

                        ( Just (Insert _), _ ) ->
                            Vendor.Graph.Hack.crashHack "Graph.computeEdgeDiff: Collected inserts before removals. This is an error in the implementation of Graph and you should file a bug report!"

                        ( Nothing, eu ) ->
                            Just eu
            in
            IntDict.update updatedId replaceUpdate

        collect edgeUpdate adj updates =
            IntDict.foldl (collectUpdates edgeUpdate) updates adj
    in
    case ( old, new ) of
        ( Nothing, Nothing ) ->
            emptyDiff

        ( Just rem, Nothing ) ->
            { outgoing = IntDict.empty |> collect Remove rem.incoming
            , incoming = IntDict.empty |> collect Remove rem.outgoing
            }

        ( Nothing, Just ins ) ->
            { outgoing = IntDict.empty |> collect Insert ins.incoming
            , incoming = IntDict.empty |> collect Insert ins.outgoing
            }

        ( Just rem, Just ins ) ->
            if rem == ins then
                emptyDiff

            else
                { outgoing = IntDict.empty |> collect Remove rem.incoming |> collect Insert ins.incoming
                , incoming = IntDict.empty |> collect Remove rem.outgoing |> collect Insert ins.outgoing
                }



-- applies an EdgeDiff to the graphRep, where nodeId is adjacent
-- to all touched edges. incoming and outgoing is wrt. to the node set (e.g.
-- flipped wrt. nodeId). This is the most critical function, as it can mess up
-- the internal invariants of the graph.


applyEdgeDiff : NodeId -> EdgeDiff e -> GraphRep n e -> GraphRep n e
applyEdgeDiff nodeId diff graphRep =
    let
        flippedFoldl f dict acc =
            IntDict.foldl f acc dict

        edgeUpdateToMaybe edgeUpdate =
            case edgeUpdate of
                Insert lbl ->
                    Just lbl

                Remove _ ->
                    Nothing

        updateAdjacency updateEdge updatedId edgeUpdate =
            let
                updateLbl =
                    updateEdge (always (edgeUpdateToMaybe edgeUpdate))
            in
            IntDict.update updatedId (Maybe.map updateLbl)

        -- ignores edges to nodes not in the graph
        updateIncomingEdge upd node =
            { node | incoming = IntDict.update nodeId upd node.incoming }

        updateOutgoingEdge upd node =
            { node | outgoing = IntDict.update nodeId upd node.outgoing }
    in
    graphRep
        |> flippedFoldl (updateAdjacency updateIncomingEdge) diff.incoming
        |> flippedFoldl (updateAdjacency updateOutgoingEdge) diff.outgoing


{-| Analogous to `Dict.update`, `update nodeId updater graph` will find
the node context of the node with id `nodeId` in `graph`. It will then call `updater`
with `Just` that node context if that node was found and `Nothing`
otherwise. `updater` can then return `Just` an updated node context
(modifying edges is also permitted!) or delete the node by returning
`Nothing`. The updated `graph` is returned.

This is the most powerful building function since all possible per-node
operations are possible (node removal, insertion and updating of context
properties).

The other operations can be implemented in terms of `update` like this:

    remove nodeId graph =
        update nodeId (always Nothing) graph

    insert nodeContext graph =
        update nodeContext.node.id (always (Just nodeContext)) graph

-}
update : NodeId -> (Maybe (NodeContext n e) -> Maybe (NodeContext n e)) -> Graph n e -> Graph n e
update nodeId updater =
    -- This basically wraps updater so that the edges are consistent.
    -- This is, it cannot use the lookup focus, because it needs to update other contexts, too.
    let
        wrappedUpdater rep =
            let
                old =
                    IntDict.get nodeId rep

                filterInvalidEdges ctx =
                    IntDict.filter (\id _ -> id == ctx.node.id || IntDict.member id rep)

                cleanUpEdges ctx =
                    { ctx
                        | incoming = filterInvalidEdges ctx ctx.incoming
                        , outgoing = filterInvalidEdges ctx ctx.outgoing
                    }

                new =
                    old
                        |> updater
                        |> Maybe.map cleanUpEdges

                diff =
                    computeEdgeDiff old new
            in
            rep
                |> applyEdgeDiff nodeId diff
                |> IntDict.update nodeId (always new)
    in
    unGraph >> wrappedUpdater >> Graph


{-| Analogous to `Dict.remove`, `remove nodeId graph` returns a version of `graph`
without a node with id `nodeId`. If there was no node with that id, then remove
is a no-op:

    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] [Edge 1 2 ()]
    graph == remove 42 graph
    graph |> remove 2 |> size == 1

-}
remove : NodeId -> Graph n e -> Graph n e
remove nodeId graph =
    update nodeId (always Nothing) graph



{- QUERY -}


{-| Analogous to `Dict.get`, `get nodeId graph` returns the `Just` the node
context with id `nodeId` in `graph` if there is one and `Nothing` otherwise.

    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] []
    get 42 graph == Nothing
    get 1 graph == Just <node context of node 1>

-}
get : NodeId -> Graph n e -> Maybe (NodeContext n e)
get nodeId =
    unGraph >> IntDict.get nodeId



{- LIST REPRESENTATIONS -}


{-| `nodeIds graph` returns a list of all nodes' ids in `graph`.

    nodeIds empty == []
    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] []
    nodeIds graph == [1, 2]

-}
nodeIds : Graph n e -> List NodeId
nodeIds =
    unGraph >> IntDict.keys


fromNodesAndEdges : IntDict (NodeContext n e) -> List (Edge e) -> Graph n e
fromNodesAndEdges nodeRep edges_ =
    let
        addEdge edge rep =
            let
                updateOutgoing ctx =
                    { node = ctx.node
                    , incoming = ctx.incoming
                    , outgoing = IntDict.insert edge.to edge.label ctx.outgoing
                    }

                updateIncoming ctx =
                    { node = ctx.node
                    , incoming = IntDict.insert edge.from edge.label ctx.incoming
                    , outgoing = ctx.outgoing
                    }
            in
            rep
                |> IntDict.update edge.from (Maybe.map updateOutgoing)
                |> IntDict.update edge.to (Maybe.map updateIncoming)

        addEdgeIfValid edge rep =
            if IntDict.member edge.from rep && IntDict.member edge.to rep then
                addEdge edge rep

            else
                rep
    in
    Graph (List.foldl addEdgeIfValid nodeRep edges_)


addNode : Node n -> IntDict (NodeContext n v) -> IntDict (NodeContext n v)
addNode n intDict =
    IntDict.insert n.id (NodeContext n IntDict.empty IntDict.empty) intDict



{- TRANSFORMS -}
{- CHARACTERIZATION -}


{-| `AcyclicGraph` wraps a `Graph` and witnesses the fact that
it is acyclic.

This can be passed on to functions that only work on acyclic graphs,
like `topologicalSort` and `heightLevels`.

-}
type AcyclicGraph n e
    = AcyclicGraph (Graph n e) (List NodeId)



{- This is a **really** ugly hack since Elm 0.19 doesn't allow `Debug.crash` any more.
   Hopefully this will never get executed, but if it does, it will make your browser
   hang (or hopefully give a stack overflow error).

   The only justification for this is that it *should* never get called, and there are
   no sensible default cases if we do get there.
-}


unsafeGet : String -> NodeId -> Graph n e -> NodeContext n e
unsafeGet msg id graph =
    case get id graph of
        Nothing ->
            Vendor.Graph.Hack.crashHack msg

        Just ctx ->
            ctx


checkForBackEdges : List NodeId -> Graph n e -> Result (Edge e) (AcyclicGraph n e)
checkForBackEdges ordering graph =
    let
        check id ( backSet, _ ) =
            let
                backSetWithId =
                    IntDict.insert id () backSet

                error =
                    "Graph.checkForBackEdges: `ordering` didn't contain `id`"

                ctx =
                    unsafeGet error id graph

                backEdges =
                    IntDict.intersect ctx.outgoing backSetWithId
            in
            case IntDict.findMin backEdges of
                Nothing ->
                    Ok ( backSetWithId, () )

                Just ( to, label ) ->
                    Err (Edge id to label)

        success _ =
            AcyclicGraph graph ordering
    in
    ordering
        |> List.foldl
            (\id res -> res |> Result.andThen (check id))
            (Ok ( IntDict.empty, () ))
        |> Result.map success


{-| `checkAcyclic graph` checks `graph` for cycles.

If there are any cycles, this will return `Err edge`,
where `edge` is an `Edge` that is part of a cycle.
If there aren't any cycles, this will return `Ok acyclic`, where
`acyclic` is an `AcyclicGraph` that witnesses this fact.

-}
checkAcyclic : Graph n e -> Result (Edge e) (AcyclicGraph n e)
checkAcyclic graph =
    let
        reversePostOrder =
            dfs (onFinish (\{ node } acc -> node.id :: acc)) [] graph
    in
    checkForBackEdges reversePostOrder graph



{- TRAVERSALS -}


{-| Selects the next neighbors for the currently visited node in the traversal.
-}
type alias NeighborSelector n e =
    NodeContext n e
    -> List NodeId


{-| A good default for selecting neighbors is to just go along outgoing edges:

    alongOutgoingEdges ctx =
        IntDict.keys ctx.outgoing

`dfs`/`bfs` use this as their selecting strategy.

-}
alongOutgoingEdges : NeighborSelector n e
alongOutgoingEdges ctx =
    IntDict.keys ctx.outgoing


{-| A less common way for selecting neighbors is to follow incoming edges:

    alongIncomingEdges ctx =
        IntDict.keys ctx.incoming

-}
alongIncomingEdges : NeighborSelector n e
alongIncomingEdges ctx =
    IntDict.keys ctx.incoming


{-| A generic node visitor just like that in the ordinary `fold` function.
There are combinators that make these usable for both depth-first traversal
(`onDiscovery`, `onFinish`) and breadth-first traversal (`ignorePath`).
-}
type alias SimpleNodeVisitor n e acc =
    NodeContext n e
    -> acc
    -> acc



{- DFS -}


{-| A node visitor specialized for depth-first traversal. Along with the node
context of the currently visited node, the current accumulated value is passed.
The visitor then has the chance to both modify the value at discovery of the
node through the first return value and also provide a finishing
transformation which is called with the value after all children were processed
and the node is about to be finished.

In the cases where you don't need access to the value both at dicovery and at
finish, look into `onDiscovery` and `onFinish`.

-}
type alias DfsNodeVisitor n e acc =
    NodeContext n e
    -> acc
    -> ( acc, acc -> acc )


{-| Transform a `SimpleNodeVisitor` into an equivalent `DfsNodeVisitor`, which
will be called upon node finish. This eases providing `DfsNodeVisitor`s in
the default case:

    dfsPostOrder : Graph n e -> List (NodeContext n e)
    dfsPostOrder graph =
        List.reverse (dfs (onFinish (::)) [] graph)

-}
onFinish : SimpleNodeVisitor n e acc -> DfsNodeVisitor n e acc
onFinish visitor ctx acc =
    ( acc, visitor ctx )


{-| The `dfs*` functions are not powerful enough? Go for this beast.

`guidedDfs selectNeighbors visitNode seeds acc graph` will perform a depth-first
traversal on `graph` starting with a stack of `seeds`. The children of each node
will be selected with `selectNeighbors` (see `NeighborSelector`), the visiting
of nodes is handled by `visitNode` (c.f. `DfsNodeVisitor`), folding `acc` over
the graph.

When there are not any more nodes to be visited, the function will return the
accumulated value together with the unvisited rest of `graph`.

    dfsPreOrder graph =
        -- NodeId 1 is just a wild guess here
        guidedDfs alongOutgoingEdges (onDiscovery (::)) [ 1 ] [] graph

-}
guidedDfs :
    NeighborSelector n e
    -> DfsNodeVisitor n e acc
    -> List NodeId
    -> acc
    -> Graph n e
    -> ( acc, Graph n e )
guidedDfs selectNeighbors visitNode startingSeeds startingAcc startingGraph =
    let
        go seeds acc graph =
            case seeds of
                [] ->
                    -- We are done with this connected component, so we return acc and the rest of the graph
                    ( acc, graph )

                next :: seeds1 ->
                    case get next graph of
                        -- This can actually happen since we don't filter for already visited nodes.
                        -- That would be an opportunity for time-memory-tradeoff.
                        -- E.g. Passing along a set of visited nodeIds.
                        Nothing ->
                            go seeds1 acc graph

                        Just ctx ->
                            let
                                ( accAfterDiscovery, finishNode ) =
                                    visitNode ctx acc

                                ( accBeforeFinish, graph1 ) =
                                    go (selectNeighbors ctx) accAfterDiscovery (remove next graph)

                                accAfterFinish =
                                    finishNode accBeforeFinish
                            in
                            go seeds1 accAfterFinish graph1
    in
    go startingSeeds startingAcc startingGraph


{-| An off-the-shelf depth-first traversal. It will visit all components of the
graph in no guaranteed order, discovering nodes `alongOutgoingEdges`.
See the docs of `DfsNodeVisitor` on how to supply such a beast. There are also
examples on how to use `dfs`.
-}
dfs : DfsNodeVisitor n e acc -> acc -> Graph n e -> acc
dfs visitNode acc graph =
    guidedDfs alongOutgoingEdges visitNode (nodeIds graph) acc graph |> Tuple.first



{- BFS -}


{-| A specialized node visitor for breadth-first traversal. Compared to a
`SimpleNodeVisitor`, the path of contexts from the root to the current
node is passed instead of just the current node's context. Additionally, the
distance from the root is passed as an `Int` (the root has distance 0 and it
holds always that `length path == distance - 1`).

If you don't need the additional information, you can turn a `SimpleNodeVisitor`
into a `BfsNodeVisitor` by calling `ignorePath`.

-}
type alias BfsNodeVisitor n e acc =
    List (NodeContext n e)
    -> Int
    -> acc
    -> acc


{-| The `bfs` function is not powerful enough? Go for this beast.

`guidedBfs selectNeighbors visitNode seeds acc graph` will perform a breadth-first
traversal on `graph` starting with a queue of `seeds`. The children of each node
will be selected with `selectNeighbors` (see `NeighborSelector`), the visiting
of nodes is handled by `visitNode` (c.f. `BfsNodeVisitor`), folding `acc` over
the graph.

When there are not any more nodes to be visited, the function will return the
accumulated value together with the unvisited rest of `graph`.

    bfsLevelOrder graph =
        -- NodeId 1 is just a wild guess here
        guidedBfs alongOutgoingEdges (ignorePath (::)) [ 1 ] [] graph

-}
guidedBfs :
    NeighborSelector n e
    -> BfsNodeVisitor n e acc
    -> List NodeId
    -> acc
    -> Graph n e
    -> ( acc, Graph n e )
guidedBfs selectNeighbors visitNode startingSeeds startingAcc startingGraph =
    let
        enqueueMany distance parentPath nodeIds_ queue =
            nodeIds_
                |> List.map (\id -> ( id, parentPath, distance ))
                |> List.foldl Fifo.insert queue

        go seeds acc graph =
            case Fifo.remove seeds of
                ( Nothing, _ ) ->
                    -- We are done with this connected component, so we return acc and the rest of the graph
                    ( acc, graph )

                ( Just ( next, parentPath, distance ), seeds1 ) ->
                    case get next graph of
                        -- This can actually happen since we don't filter for already visited nodes.
                        -- That would be an opportunity for time-memory-tradeoff.
                        -- E.g. Passing along a set of visited nodeIds_.
                        Nothing ->
                            go seeds1 acc graph

                        Just ctx ->
                            let
                                path =
                                    ctx :: parentPath

                                accAfterVisit =
                                    visitNode path distance acc

                                seeds2 =
                                    enqueueMany (distance + 1) path (selectNeighbors ctx) seeds1
                            in
                            go seeds2 accAfterVisit (remove next graph)
    in
    go (enqueueMany 0 [] startingSeeds Fifo.empty) startingAcc startingGraph


{-| Computes a
[topological ordering](https://en.wikipedia.org/wiki/Topological_sorting)
of the given `AcyclicGraph`.
-}
topologicalSort : AcyclicGraph n e -> List (NodeContext n e)
topologicalSort (AcyclicGraph graph ordering) =
    let
        error =
            "Graph.topologicalSort: Invalid `AcyclicGraph`, where the ordering contained nodes not present in the graph"
    in
    List.map (\id -> unsafeGet error id graph) ordering
