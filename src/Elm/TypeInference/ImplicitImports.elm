module Elm.TypeInference.ImplicitImports exposing
    ( elmCorePackage
    , unaliasModule
    , implicitValueHome
    , moduleExposingType
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
@docs unaliasModule
@docs implicitValueHome
@docs moduleExposingType
@docs isImplicitlyImportedModule

-}

import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.TypeInference.Type exposing (VarName)


elmCorePackage : String
elmCorePackage =
    "elm/core"


{-| `Cmd` -> `Platform.Cmd`, `Sub` -> `Platform.Sub`

Only these two implicit imports use an alias.

-}
unaliasModule : String -> Maybe FullModuleName
unaliasModule singleSegmentAlias =
    case singleSegmentAlias of
        "Cmd" ->
            Just (FullModuleName.fromDotted "Platform.Cmd")

        "Sub" ->
            Just (FullModuleName.fromDotted "Platform.Sub")

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

-}
implicitValueHome : VarName -> FullModuleName
implicitValueHome varName =
    case varName of
        "::" ->
            FullModuleName.fromDotted "List"

        "Just" ->
            FullModuleName.fromDotted "Maybe"

        "Nothing" ->
            FullModuleName.fromDotted "Maybe"

        "Ok" ->
            FullModuleName.fromDotted "Result"

        "Err" ->
            FullModuleName.fromDotted "Result"

        _ ->
            FullModuleName.fromDotted "Basics"


{-| Which implicit module exposes this type unqualified?
(`Tuple` and `Debug` don't expose anything.)
-}
moduleExposingType : String -> Maybe FullModuleName
moduleExposingType typeName =
    case typeName of
        "Int" ->
            Just (FullModuleName.fromDotted "Basics")

        "Float" ->
            Just (FullModuleName.fromDotted "Basics")

        "Bool" ->
            Just (FullModuleName.fromDotted "Basics")

        "Never" ->
            Just (FullModuleName.fromDotted "Basics")

        "Order" ->
            Just (FullModuleName.fromDotted "Basics")

        "List" ->
            Just (FullModuleName.fromDotted "List")

        "Maybe" ->
            Just (FullModuleName.fromDotted "Maybe")

        "Result" ->
            Just (FullModuleName.fromDotted "Result")

        "String" ->
            Just (FullModuleName.fromDotted "String")

        "Char" ->
            Just (FullModuleName.fromDotted "Char")

        "Program" ->
            Just (FullModuleName.fromDotted "Platform")

        "Cmd" ->
            Just (FullModuleName.fromDotted "Platform.Cmd")

        "Sub" ->
            Just (FullModuleName.fromDotted "Platform.Sub")

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
