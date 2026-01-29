module Review.WorkListTest exposing (all)

import Expect
import Review.WorkList as WorkList exposing (WorkList)
import Test exposing (Test, describe, test)
import Vendor.Graph as Graph exposing (Graph)


all : Test
all =
    describe "WorkList"
        [ test "should create a worklist sorted based on the order of a graph" <|
            \() ->
                let
                    graph : Graph String
                    graph =
                        Graph.empty
                            |> Graph.addNode (Graph.Node 0 "A")
                            |> Graph.addNode (Graph.Node 2 "C")
                            |> Graph.addNode (Graph.Node 1 "B")
                            -- edges are in the opposite direction
                            -- C imports A
                            |> Graph.addEdge { from = 0, to = 2 }
                            -- C imports B
                            |> Graph.addEdge { from = 1, to = 2 }
                            -- B imports A
                            |> Graph.addEdge { from = 0, to = 1 }
                in
                case Graph.checkAcyclic graph of
                    Err err ->
                        Debug.todo ("Import cycle in graph: " ++ Debug.toString err)

                    Ok sortedModules ->
                        sortedModules
                            |> List.map (\m -> m.node.label)
                            |> WorkList.fromSortedModules
                            |> .modules
                            |> Expect.equalLists [ "A", "B", "C" ]
        , test "should visit the modules in order" <|
            \() ->
                let
                    workList : WorkList
                    workList =
                        [ "A", "B", "C" ]
                            |> WorkList.fromSortedModules
                            |> WorkList.visitedElmJson
                            |> WorkList.visitedReadme
                            |> WorkList.visitedExtraFiles
                            |> WorkList.visitedDependencies
                in
                Expect.all
                    [ \() ->
                        workList
                            |> WorkList.nextStep
                            |> Expect.equal (WorkList.Module "A")
                    , \() ->
                        workList
                            |> WorkList.visitedNextModule
                            |> WorkList.nextStep
                            |> Expect.equal (WorkList.Module "B")
                    , \() ->
                        workList
                            |> WorkList.visitedNextModule
                            |> WorkList.visitedNextModule
                            |> WorkList.nextStep
                            |> Expect.equal (WorkList.Module "C")
                    , \() ->
                        workList
                            |> WorkList.visitedNextModule
                            |> WorkList.visitedNextModule
                            |> WorkList.visitedNextModule
                            |> WorkList.nextStep
                            |> Expect.equal WorkList.FinalProjectEvaluation
                    ]
                    ()
        , test "should revisit all the dependent files of the touched files when recomputing graph" <|
            \() ->
                let
                    graph : Graph String
                    graph =
                        Graph.empty
                            |> Graph.addNode (Graph.Node 0 "A")
                            |> Graph.addNode (Graph.Node 2 "C")
                            |> Graph.addNode (Graph.Node 1 "B")
                            -- edges are in the opposite direction
                            -- C imports A
                            |> Graph.addEdge { from = 0, to = 2 }
                            -- C imports B
                            |> Graph.addEdge { from = 1, to = 2 }
                            -- B imports A
                            |> Graph.addEdge { from = 0, to = 1 }
                in
                case Graph.checkAcyclic graph of
                    Err err ->
                        Debug.todo ("Import cycle in graph: " ++ Debug.toString err)

                    Ok sortedModules ->
                        sortedModules
                            |> List.map (\m -> m.node.label)
                            |> WorkList.fromSortedModules
                            |> WorkList.visitedElmJson
                            |> WorkList.visitedReadme
                            |> WorkList.visitedExtraFiles
                            |> WorkList.visitedDependencies
                            |> WorkList.visitedNextModule
                            |> WorkList.visitedNextModule
                            |> WorkList.visitedNextModule
                            -- Has nothing to visit anymore
                            |> WorkList.touchedModule "B"
                            |> WorkList.recomputeModules graph sortedModules
                            |> .modules
                            |> Expect.equalLists [ "B", "C" ]
        , test "should revisit all the dependent files of the touched files when recomputing graph, even indirect ones" <|
            \() ->
                let
                    graph : Graph String
                    graph =
                        Graph.empty
                            |> Graph.addNode (Graph.Node 0 "A")
                            |> Graph.addNode (Graph.Node 3 "D")
                            |> Graph.addNode (Graph.Node 2 "C")
                            |> Graph.addNode (Graph.Node 1 "B")
                            -- edges are in the opposite direction
                            -- C imports A
                            |> Graph.addEdge { from = 0, to = 2 }
                            -- C imports B
                            |> Graph.addEdge { from = 1, to = 2 }
                            -- B imports A
                            |> Graph.addEdge { from = 0, to = 1 }
                            -- D imports C
                            |> Graph.addEdge { from = 2, to = 3 }
                in
                case Graph.checkAcyclic graph of
                    Err err ->
                        Debug.todo ("Import cycle in graph: " ++ Debug.toString err)

                    Ok sortedModules ->
                        sortedModules
                            |> List.map (\m -> m.node.label)
                            |> WorkList.fromSortedModules
                            |> WorkList.visitedElmJson
                            |> WorkList.visitedReadme
                            |> WorkList.visitedExtraFiles
                            |> WorkList.visitedDependencies
                            |> WorkList.visitedNextModule
                            |> WorkList.visitedNextModule
                            |> WorkList.visitedNextModule
                            |> WorkList.visitedNextModule
                            -- Has nothing to visit anymore
                            |> WorkList.touchedModule "B"
                            |> WorkList.recomputeModules graph sortedModules
                            |> .modules
                            |> Expect.equalLists [ "B", "C", "D" ]
        ]
