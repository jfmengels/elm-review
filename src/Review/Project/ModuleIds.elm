module Review.Project.ModuleIds exposing
    ( ModuleId
    , ModuleIds
    , add
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)


type alias ModuleId =
    Int


type ModuleIds
    = ModuleIds
        { nextId : ModuleId
        , moduleNameToId : Dict ModuleName ModuleId
        }


add : ModuleName -> ModuleIds -> ( ModuleId, ModuleIds )
add moduleName ((ModuleIds ids) as untouched) =
    case Dict.get moduleName ids.moduleNameToId of
        Just id ->
            ( id, untouched )

        Nothing ->
            ( ids.nextId
            , ModuleIds
                { nextId = ids.nextId + 1
                , moduleNameToId = Dict.insert moduleName ids.nextId ids.moduleNameToId
                }
            )
