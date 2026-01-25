module Review.Project.ModuleIds exposing
    ( ModuleId
    , ModuleIds
    , add
    , addAndGet
    , empty
    , get
    , moduleNameToId
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


empty : ModuleIds
empty =
    ModuleIds
        { nextId = 0
        , moduleNameToId = Dict.empty
        }


add : ModuleName -> ModuleIds -> ModuleIds
add moduleName ((ModuleIds ids) as untouched) =
    if Dict.member moduleName ids.moduleNameToId then
        untouched

    else
        ModuleIds
            { nextId = ids.nextId + 1
            , moduleNameToId = Dict.insert moduleName ids.nextId ids.moduleNameToId
            }


addAndGet : ModuleName -> ModuleIds -> ( ModuleId, ModuleIds )
addAndGet moduleName ((ModuleIds ids) as untouched) =
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


get : ModuleName -> ModuleIds -> Maybe ModuleId
get moduleName (ModuleIds ids) =
    Dict.get moduleName ids.moduleNameToId


moduleNameToId : ModuleIds -> Dict ModuleName ModuleId
moduleNameToId (ModuleIds ids) =
    ids.moduleNameToId
