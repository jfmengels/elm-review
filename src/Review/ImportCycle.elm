module Review.ImportCycle exposing (error, findCycle)

import Ansi
import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import Vendor.Graph as Graph exposing (Graph)
import Vendor.IntDict as IntDict


error : List String -> { message : String, details : List String }
error cycle =
    { message = "Your module imports form a cycle"
    , details =
        [ printCycle cycle
        , "Learn more about why this is disallowed and how to break cycles here:<https://elm-lang.org/0.19.1/import-cycles>"
        ]
    }


findCycle : Dict String OpaqueProjectModule -> Graph String e -> Graph.Edge e -> List String
findCycle modules graph edge =
    let
        initialCycle : List (Graph.Node String)
        initialCycle =
            Graph.guidedBfs Graph.alongIncomingEdges (visitorDiscoverCycle edge.to) [ edge.from ] [] graph
                |> Tuple.first
    in
    findSmallerCycle graph initialCycle initialCycle
        |> List.map (filePathToModuleName modules)


filePathToModuleName : Dict String OpaqueProjectModule -> { a | label : String } -> String
filePathToModuleName modules { label } =
    case Dict.get label modules of
        Just mod ->
            String.join "." (ProjectModule.moduleName mod)

        Nothing ->
            label


findSmallerCycle : Graph n e -> List (Graph.Node n) -> List (Graph.Node n) -> List (Graph.Node n)
findSmallerCycle graph currentBest nodesToVisit =
    case nodesToVisit of
        [] ->
            currentBest

        startingNode :: restOfNodes ->
            let
                cycle : List (Graph.Node n)
                cycle =
                    Graph.guidedBfs Graph.alongIncomingEdges (visitorDiscoverCycle startingNode.id) [ startingNode.id ] [] graph
                        |> Tuple.first

                newBest : List (Graph.Node n)
                newBest =
                    if List.length cycle > 0 && List.length cycle < List.length currentBest then
                        cycle

                    else
                        currentBest
            in
            if List.length newBest == 1 then
                newBest

            else
                findSmallerCycle graph newBest restOfNodes


reachedTarget : Graph.NodeId -> List (Graph.NodeContext n e) -> Bool
reachedTarget targetNode path =
    case List.head path of
        Just node ->
            node.node.id == targetNode

        Nothing ->
            False


visitorDiscoverCycle : Graph.NodeId -> List (Graph.NodeContext n e) -> Int -> List (Graph.Node n) -> List (Graph.Node n)
visitorDiscoverCycle targetNode path distance acc =
    if List.isEmpty acc then
        -- We haven't found the cycle yet
        if distance == 0 then
            case List.head path of
                Just head ->
                    if IntDict.member head.node.id head.incoming then
                        [ head.node ]

                    else
                        acc

                Nothing ->
                    acc

        else if reachedTarget targetNode path then
            List.map .node path

        else
            []

    else
        -- We already found the cycle
        acc


printCycle : List String -> String
printCycle moduleNames =
    moduleNames
        |> List.map Ansi.yellow
        |> String.join "\n    │     ↓\n    │    "
        |> wrapInCycle


wrapInCycle : String -> String
wrapInCycle string =
    "    ┌─────┐\n    │    " ++ string ++ "\n    └─────┘"
