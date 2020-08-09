module NoInconsistentAliases.MissingAliasSet exposing (MissingAliasSet, empty, fold, insert, use)

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import NoInconsistentAliases.MissingAlias as MissingAlias exposing (MissingAlias)
import NoInconsistentAliases.ModuleUse exposing (ModuleUse)


type MissingAliasSet
    = MissingAliasSet (Dict ModuleName MissingAlias)


empty : MissingAliasSet
empty =
    MissingAliasSet Dict.empty


insert : MissingAlias -> MissingAliasSet -> MissingAliasSet
insert missingAlias (MissingAliasSet aliases) =
    MissingAliasSet (MissingAlias.mapModuleName (\name -> Dict.insert name missingAlias aliases) missingAlias)


use : ModuleName -> ModuleUse -> MissingAliasSet -> MissingAliasSet
use moduleName moduleUse (MissingAliasSet aliases) =
    case Dict.get moduleName aliases of
        Nothing ->
            MissingAliasSet aliases

        Just missingAlias ->
            let
                missingAliasWithUse =
                    MissingAlias.withModuleUse moduleUse missingAlias
            in
            MissingAliasSet (Dict.insert moduleName missingAliasWithUse aliases)


fold : (MissingAlias -> a -> a) -> a -> MissingAliasSet -> a
fold folder start (MissingAliasSet aliases) =
    aliases |> Dict.values |> List.foldl folder start
