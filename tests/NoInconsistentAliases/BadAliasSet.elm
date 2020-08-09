module NoInconsistentAliases.BadAliasSet exposing (BadAliasSet, empty, fold, insert, use)

import Dict exposing (Dict)
import NoInconsistentAliases.BadAlias as BadAlias exposing (BadAlias)
import NoInconsistentAliases.ModuleUse exposing (ModuleUse)


type BadAliasSet
    = BadAliasSet (Dict String BadAlias)


empty : BadAliasSet
empty =
    BadAliasSet Dict.empty


insert : BadAlias -> BadAliasSet -> BadAliasSet
insert badAlias (BadAliasSet aliases) =
    BadAliasSet (BadAlias.mapName (\name -> Dict.insert name badAlias aliases) badAlias)


use : String -> ModuleUse -> BadAliasSet -> BadAliasSet
use name moduleUse (BadAliasSet aliases) =
    case Dict.get name aliases of
        Nothing ->
            BadAliasSet aliases

        Just badAlias ->
            let
                badAliasWithUse =
                    BadAlias.withModuleUse moduleUse badAlias
            in
            BadAliasSet (Dict.insert name badAliasWithUse aliases)


fold : (BadAlias -> a -> a) -> a -> BadAliasSet -> a
fold folder start (BadAliasSet aliases) =
    aliases |> Dict.values |> List.foldl folder start
