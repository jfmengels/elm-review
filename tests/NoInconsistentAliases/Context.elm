module NoInconsistentAliases.Context exposing
    ( Module, initial
    , addModuleAlias, lookupModuleName
    , addMissingAlias, foldMissingAliases
    , addBadAlias, foldBadAliases
    , addModuleCall
    )

{-|

@docs Module, initial
@docs addModuleAlias, lookupModuleName
@docs addMissingAlias, foldMissingAliases
@docs addBadAlias, foldBadAliases
@docs addModuleCall

-}

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import NoInconsistentAliases.BadAlias exposing (BadAlias)
import NoInconsistentAliases.BadAliasSet as BadAliasSet exposing (BadAliasSet)
import NoInconsistentAliases.MissingAlias exposing (MissingAlias)
import NoInconsistentAliases.MissingAliasSet as MissingAliasSet exposing (MissingAliasSet)
import NoInconsistentAliases.ModuleUse as ModuleUse exposing (ModuleUse)


type Module
    = Module
        { aliases : Dict String ModuleName
        , badAliases : BadAliasSet
        , missingAliases : MissingAliasSet
        }


initial : Module
initial =
    Module
        { aliases = Dict.empty
        , badAliases = BadAliasSet.empty
        , missingAliases = MissingAliasSet.empty
        }


addModuleAlias : ModuleName -> String -> Module -> Module
addModuleAlias moduleName moduleAlias (Module context) =
    Module { context | aliases = Dict.insert moduleAlias moduleName context.aliases }


addBadAlias : BadAlias -> Module -> Module
addBadAlias badAlias (Module context) =
    Module { context | badAliases = BadAliasSet.insert badAlias context.badAliases }


addMissingAlias : MissingAlias -> Module -> Module
addMissingAlias missingAlias (Module context) =
    Module { context | missingAliases = MissingAliasSet.insert missingAlias context.missingAliases }


addModuleCall : ModuleName -> String -> Range -> Module -> Module
addModuleCall moduleName function range context =
    let
        moduleUse =
            ModuleUse.new function range
    in
    context
        |> useBadAliasCall moduleName moduleUse
        |> useMissingAliasCall moduleName moduleUse


useBadAliasCall : ModuleName -> ModuleUse -> Module -> Module
useBadAliasCall moduleName moduleUse (Module context) =
    case moduleName of
        [ moduleAlias ] ->
            Module
                { context
                    | badAliases = BadAliasSet.use moduleAlias moduleUse context.badAliases
                }

        _ ->
            Module context


useMissingAliasCall : ModuleName -> ModuleUse -> Module -> Module
useMissingAliasCall moduleName moduleUse (Module context) =
    Module
        { context
            | missingAliases = MissingAliasSet.use moduleName moduleUse context.missingAliases
        }


foldBadAliases : (BadAlias -> a -> a) -> a -> Module -> a
foldBadAliases folder start (Module { badAliases }) =
    BadAliasSet.fold folder start badAliases


lookupModuleName : Module -> String -> Maybe ModuleName
lookupModuleName (Module { aliases }) moduleAlias =
    Dict.get moduleAlias aliases


foldMissingAliases : (MissingAlias -> a -> a) -> a -> Module -> a
foldMissingAliases folder start (Module { missingAliases }) =
    MissingAliasSet.fold folder start missingAliases
