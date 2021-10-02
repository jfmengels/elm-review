module Docs.Utils.ExposedFromProject exposing (exposedModules)

import Elm.Module
import Elm.Project
import Set exposing (Set)


exposedModules : Elm.Project.Project -> Set String
exposedModules project =
    case project of
        Elm.Project.Package package ->
            case package.exposed of
                Elm.Project.ExposedList list ->
                    list
                        |> List.map Elm.Module.toString
                        |> Set.fromList

                Elm.Project.ExposedDict list ->
                    list
                        |> List.concatMap Tuple.second
                        |> List.map Elm.Module.toString
                        |> Set.fromList

        Elm.Project.Application _ ->
            Set.empty
