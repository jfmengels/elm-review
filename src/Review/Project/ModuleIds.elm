module Review.Project.ModuleIds exposing
    ( ModuleId
    , ModuleIds
    , addAndGet
    , empty
    , get
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
