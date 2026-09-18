module Elm.TypeInference.ImplicitImports exposing
    ( elmCorePackage
    , unaliasModuleId
    , implicitValueHomeId
    , moduleExposingTypeId
    , isImplicitlyImportedModule
    )

{-| Elm compiles every module with these implicit imports:

    import Basics exposing (..)
    import List exposing (List, (::))
    import Maybe exposing (Maybe(..))
    import Result exposing (Result(..))
    import String exposing (String)
    import Char exposing (Char)
    import Tuple

    import Debug

    import Platform exposing ( Program )
    import Platform.Cmd as Cmd exposing ( Cmd )
    import Platform.Sub as Sub exposing ( Sub )

The data below is hardcoded because `elm/core`'s prelude never changes.
We don't list all of `Basics`' functions; `docs.json` supplies that, so any
otherwise-unknown unqualified value can only come from `Basics`.

@docs elmCorePackage
@docs unaliasModuleId
@docs implicitValueHomeId
@docs moduleExposingTypeId
@docs isImplicitlyImportedModule

-}

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.TypeInference.ModuleIds as ModuleIds exposing (ModuleId)
import Elm.TypeInference.Type exposing (VarName)


elmCorePackage : String
elmCorePackage =
    "elm/core"


{-| `Cmd` -> `Platform.Cmd`, `Sub` -> `Platform.Sub`.

Only these two implicit imports use an alias.
Module names are interned `Int`s: stable ids, no string building on the hot path.

-}
unaliasModuleId : String -> Maybe ModuleId
unaliasModuleId singleSegmentAlias =
    case singleSegmentAlias of
        "Cmd" ->
            Just ModuleIds.platformCmdId

        "Sub" ->
            Just ModuleIds.platformSubId

        _ ->
            Nothing


{-| Which implicit module could expose this unqualified value?

Only `Basics` (`exposing (..)`), `List` (`(::)`), `Maybe`
(`Just`, `Nothing`) and `Result` (`Ok`, `Err`) expose values; everything else
exposes `Only []` for values. `Basics` is the fallback candidate because we don't
enumerate its contents here -- `docs.json` settles whether it defines the name.
`(::)`, `Just`/`Nothing` and `Ok`/`Err` are known to come only from
`List`/`Maybe`/`Result` (`Basics` in `elm/core` 1.0.x defines none of them),
so they skip the `Basics` probe.

     identity --> Basics
     foobar   --> Basics
     "::"     --> List
     "Just"   --> Maybe

Module names are interned `Int`s: stable ids, no string building on the hot path.

-}
implicitValueHomeId : VarName -> ModuleId
implicitValueHomeId varName =
    case varName of
        "::" ->
            ModuleIds.listId

        "Just" ->
            ModuleIds.maybeId

        "Nothing" ->
            ModuleIds.maybeId

        "Ok" ->
            ModuleIds.resultId

        "Err" ->
            ModuleIds.resultId

        _ ->
            ModuleIds.basicsId


{-| Which implicit module exposes this type unqualified?
(`Tuple` and `Debug` don't expose anything.)

Module names are interned `Int`s: stable ids, no string building on the hot path.

-}
moduleExposingTypeId : String -> Maybe ModuleId
moduleExposingTypeId typeName =
    case typeName of
        "Int" ->
            Just ModuleIds.basicsId

        "Float" ->
            Just ModuleIds.basicsId

        "Bool" ->
            Just ModuleIds.basicsId

        "Never" ->
            Just ModuleIds.basicsId

        "Order" ->
            Just ModuleIds.basicsId

        "List" ->
            Just ModuleIds.listId

        "Maybe" ->
            Just ModuleIds.maybeId

        "Result" ->
            Just ModuleIds.resultId

        "String" ->
            Just ModuleIds.stringId

        "Char" ->
            Just ModuleIds.charId

        "Program" ->
            Just ModuleIds.platformId

        "Cmd" ->
            Just ModuleIds.platformCmdId

        "Sub" ->
            Just ModuleIds.platformSubId

        _ ->
            Nothing


{-| Is this module implicitly imported under its own name?
-}
isImplicitlyImportedModule : ModuleName -> Bool
isImplicitlyImportedModule qualifier =
    case qualifier of
        [ "Basics" ] ->
            True

        [ "List" ] ->
            True

        [ "Maybe" ] ->
            True

        [ "Result" ] ->
            True

        [ "String" ] ->
            True

        [ "Char" ] ->
            True

        [ "Tuple" ] ->
            True

        [ "Debug" ] ->
            True

        [ "Platform" ] ->
            True

        _ ->
            False
