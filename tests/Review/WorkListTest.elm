module Review.WorkListTest exposing (all)

import Expect
import Review.WorkList as WorkList
import Test exposing (Test, describe, test)
import Vendor.Graph as Graph exposing (Graph)
import Vendor.IntDict as IntDict


all : Test
all =
    describe "WorkList"
        [ test "should create a worklist sorted based on the order of a graph" <|
            \() ->
                let
                    graph : Graph String
                    graph =
                        Graph.fromNodesAndEdges
                            (IntDict.empty
                                |> Graph.addNode (Graph.Node 0 "A")
                                |> Graph.addNode (Graph.Node 2 "C")
                                |> Graph.addNode (Graph.Node 1 "B")
                            )
                            -- edges are in the opposite direction
                            [ { from = 0, to = 2 } -- C imports A
                            , { from = 1, to = 2 } -- C imports B
                            , { from = 0, to = 1 } -- B imports A
                            ]
                in
                case Graph.checkAcyclic graph of
                    Err err ->
                        Debug.todo ("Import cycle in graph: " ++ Debug.toString err)

                    Ok acyclicGraph ->
                        acyclicGraph
                            |> Graph.topologicalSort
                            |> List.map (\m -> m.node.label)
                            |> WorkList.fromSortedModules
                            |> .modules
                            |> Expect.equalLists [ "A", "B", "C" ]
        ]
