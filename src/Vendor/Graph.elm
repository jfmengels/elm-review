module Vendor.Graph exposing
    ( NodeId, Node, Edge, Adjacency, NodeContext, Graph
    , addNode
    , get
    , fromNodesAndEdges
    , AcyclicGraph, checkAcyclic
    , NeighborSelector, alongIncomingEdges
    , BfsNodeVisitor, guidedBfs
    , topologicalSort
    , empty, removeEdge
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

@docs NeighborSelector, alongIncomingEdges


## Depth-first


## Breadth-first

@docs BfsNodeVisitor, guidedBfs


# Topological Sort

@docs topologicalSort

-}

import Vendor.Fifo as Fifo
import Vendor.Graph.Hack
import Vendor.IntDict as IntDict exposing (IntDict)
import Vendor.IntSet as IntSet exposing (IntSet)


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
type alias Edge =
    { from : NodeId
    , to : NodeId
    }


{-| Adjacency is represented as an ordered dictionary
rather than as an ordered list. This enables more dynamic
graphs with efficient edge removal and insertion on the run.
-}
type alias Adjacency =
    IntSet


{-| Represents a node with its incoming and outgoing edges
(predecessors and successors).
-}
type alias NodeContext n =
    { node : Node n
    , incoming : Adjacency
    , outgoing : Adjacency
    }



-- We will only have the Patricia trie based DynGraph implementation for simplicity.
-- Also, there is no real practical reason to separate that or to allow other implementations
-- which would justify the complexity.


type alias GraphRep n =
    IntDict (NodeContext n)


{-| The central graph type. It is parameterized both over the node label type `n`
and the edge label type `e`.

One can build such a graph with the primitives under _Build_. Most of the time
`fromNodesAndEdges` works fairly well.

For simplicity, this library just uses a patricia trie based graph representation, which means
it is just an efficient version of `Dict NodeId (NodeContext n)`. This allows efficient insertion and
removal of nodes of the graph after building.

-}
type Graph n
    = Graph (GraphRep n)



{- BUILD -}
-- applies an EdgeDiff to the graphRep, where nodeId is adjacent
-- to all touched edges. incoming and outgoing is wrt. to the node set (e.g.
-- flipped wrt. nodeId). This is the most critical function, as it can mess up
-- the internal invariants of the graph.


{-| Analogous to `Dict.remove`, `remove nodeId graph` returns a version of `graph`
without a node with id `nodeId`. If there was no node with that id, then remove
is a no-op:

    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] [Edge 1 2 ()]
    graph == remove 42 graph
    graph |> remove 2 |> size == 1

-}
remove : NodeId -> Graph n -> Graph n
remove nodeId (Graph rep) =
    rep
        |> IntDict.remove nodeId
        |> Graph



{- QUERY -}


{-| Analogous to `Dict.get`, `get nodeId graph` returns the `Just` the node
context with id `nodeId` in `graph` if there is one and `Nothing` otherwise.

    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] []
    get 42 graph == Nothing
    get 1 graph == Just <node context of node 1>

-}
get : NodeId -> Graph n -> Maybe (NodeContext n)
get nodeId (Graph rep) =
    IntDict.get nodeId rep



{- LIST REPRESENTATIONS -}


{-| `nodeIds graph` returns a list of all nodes' ids in `graph`.

    nodeIds empty == []
    graph = fromNodesAndEdges [Node 1 "1", Node 2 "2"] []
    nodeIds graph == [1, 2]

-}
nodeIds : Graph n -> List NodeId
nodeIds (Graph rep) =
    IntDict.keys rep


empty : Graph n
empty =
    Graph IntDict.empty


fromNodesAndEdges : IntDict (NodeContext n) -> List Edge -> Graph n
fromNodesAndEdges nodes edges =
    Graph (List.foldl addEdge nodes edges)


addEdge : Edge -> IntDict (NodeContext n) -> IntDict (NodeContext n)
addEdge edge rep =
    let
        updateOutgoing : NodeContext n -> NodeContext n
        updateOutgoing ctx =
            { node = ctx.node
            , incoming = ctx.incoming
            , outgoing = IntSet.insert edge.to ctx.outgoing
            }

        updateIncoming : NodeContext n -> NodeContext n
        updateIncoming ctx =
            { node = ctx.node
            , incoming = IntSet.insert edge.from ctx.incoming
            , outgoing = ctx.outgoing
            }
    in
    rep
        |> IntDict.mapKey edge.from updateOutgoing
        |> IntDict.mapKey edge.to updateIncoming


removeEdge : Edge -> IntDict (NodeContext n) -> IntDict (NodeContext n)
removeEdge edge rep =
    let
        updateOutgoing : NodeContext n -> NodeContext n
        updateOutgoing ctx =
            { node = ctx.node
            , incoming = ctx.incoming
            , outgoing = IntSet.remove edge.to ctx.outgoing
            }

        updateIncoming : NodeContext n -> NodeContext n
        updateIncoming ctx =
            { node = ctx.node
            , incoming = IntSet.remove edge.from ctx.incoming
            , outgoing = ctx.outgoing
            }
    in
    rep
        |> IntDict.mapKey edge.from updateOutgoing
        |> IntDict.mapKey edge.to updateIncoming


addNode : Node n -> IntDict (NodeContext n) -> IntDict (NodeContext n)
addNode n intDict =
    IntDict.insert n.id (NodeContext n IntSet.empty IntSet.empty) intDict


{-| `AcyclicGraph` wraps a `Graph` and witnesses the fact that
it is acyclic.

This can be passed on to functions that only work on acyclic graphs,
like `topologicalSort` and `heightLevels`.

-}
type AcyclicGraph n
    = AcyclicGraph (Graph n) (List NodeId)



{- This is a **really** ugly hack since Elm 0.19 doesn't allow `Debug.crash` any more.
   Hopefully this will never get executed, but if it does, it will make your browser
   hang (or hopefully give a stack overflow error).

   The only justification for this is that it *should* never get called, and there are
   no sensible default cases if we do get there.
-}


unsafeGet : String -> NodeId -> Graph n -> NodeContext n
unsafeGet msg id graph =
    case get id graph of
        Nothing ->
            Vendor.Graph.Hack.crashHack msg

        Just ctx ->
            ctx


checkForBackEdges : Graph n -> List NodeId -> Result Edge (AcyclicGraph n)
checkForBackEdges graph ordering =
    checkOrdering graph ordering IntSet.empty
        |> Result.map (\() -> AcyclicGraph graph ordering)


checkOrdering : Graph n -> List Int -> IntSet -> Result Edge ()
checkOrdering graph ordering set =
    case ordering of
        [] ->
            Ok ()

        id :: rest ->
            case check graph id set of
                Ok newSet ->
                    checkOrdering graph rest newSet

                Err err ->
                    Err err


check : Graph n -> Int -> IntSet -> Result Edge IntSet
check graph id backSet =
    let
        backSetWithId : IntSet
        backSetWithId =
            IntSet.insert id backSet

        error : String
        error =
            "Graph.checkForBackEdges: `ordering` didn't contain `id`"

        ctx : NodeContext n
        ctx =
            unsafeGet error id graph
    in
    case IntSet.getSharedKey ctx.outgoing backSetWithId of
        Nothing ->
            Ok backSetWithId

        Just to ->
            Err { from = id, to = to }


{-| `checkAcyclic graph` checks `graph` for cycles.

If there are any cycles, this will return `Err edge`,
where `edge` is an `Edge` that is part of a cycle.
If there aren't any cycles, this will return `Ok acyclic`, where
`acyclic` is an `AcyclicGraph` that witnesses this fact.

-}
checkAcyclic : Graph n -> Result Edge (AcyclicGraph n)
checkAcyclic graph =
    let
        reversePostOrder : List NodeId
        reversePostOrder =
            dfs [] graph
    in
    checkForBackEdges graph reversePostOrder



{- TRAVERSALS -}


{-| Selects the next neighbors for the currently visited node in the traversal.
-}
type alias NeighborSelector n =
    NodeContext n
    -> List NodeId


{-| A good default for selecting neighbors is to just go along outgoing edges:

    alongOutgoingEdges ctx =
        IntSet.keys ctx.outgoing

`dfs`/`bfs` use this as their selecting strategy.

-}
alongOutgoingEdges : NeighborSelector n
alongOutgoingEdges ctx =
    IntSet.keys ctx.outgoing


{-| A less common way for selecting neighbors is to follow incoming edges:

    alongIncomingEdges ctx =
        IntSet.keys ctx.incoming

-}
alongIncomingEdges : NeighborSelector n
alongIncomingEdges ctx =
    IntSet.keys ctx.incoming



{- DFS -}


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
    NeighborSelector n
    -> List NodeId
    -> List NodeId
    -> Graph n
    -> List NodeId
guidedDfs selectNeighbors startingSeeds startingAcc startingGraph =
    let
        go : List NodeId -> List NodeId -> Graph n -> ( List NodeId, Graph n )
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
                                ( accBeforeFinish, graph1 ) =
                                    go (selectNeighbors ctx) acc (remove next graph)
                            in
                            go seeds1 (ctx.node.id :: accBeforeFinish) graph1
    in
    go startingSeeds startingAcc startingGraph
        |> Tuple.first


{-| An off-the-shelf depth-first traversal. It will visit all components of the
graph in no guaranteed order, discovering nodes `alongOutgoingEdges`.
See the docs of `DfsNodeVisitor` on how to supply such a beast. There are also
examples on how to use `dfs`.
-}
dfs : List NodeId -> Graph n -> List NodeId
dfs acc graph =
    guidedDfs alongOutgoingEdges (nodeIds graph) acc graph



{- BFS -}


{-| A specialized node visitor for breadth-first traversal. Compared to a
`SimpleNodeVisitor`, the path of contexts from the root to the current
node is passed instead of just the current node's context. Additionally, the
distance from the root is passed as an `Int` (the root has distance 0 and it
holds always that `length path == distance - 1`).

If you don't need the additional information, you can turn a `SimpleNodeVisitor`
into a `BfsNodeVisitor` by calling `ignorePath`.

-}
type alias BfsNodeVisitor n acc =
    List (NodeContext n)
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
    NeighborSelector n
    -> BfsNodeVisitor n acc
    -> List NodeId
    -> acc
    -> Graph n
    -> ( acc, Graph n )
guidedBfs selectNeighbors visitNode startingSeeds startingAcc startingGraph =
    let
        enqueueMany distance parentPath nodeIds_ queue =
            List.foldl (\id acc -> Fifo.insert ( id, parentPath, distance ) acc) queue nodeIds_

        go seeds acc graph =
            case Fifo.remove seeds of
                Nothing ->
                    -- We are done with this connected component, so we return acc and the rest of the graph
                    ( acc, graph )

                Just ( ( next, parentPath, distance ), seeds1 ) ->
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
topologicalSort : AcyclicGraph n -> List (NodeContext n)
topologicalSort (AcyclicGraph graph ordering) =
    let
        error =
            "Graph.topologicalSort: Invalid `AcyclicGraph`, where the ordering contained nodes not present in the graph"
    in
    List.map (\id -> unsafeGet error id graph) ordering
