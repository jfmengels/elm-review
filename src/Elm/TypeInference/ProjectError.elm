module Elm.TypeInference.ProjectError exposing
    ( ProjectError(..), Location
    , toString
    )

{-| Errors reported while building a `Project` with
`Elm.TypeInference.init` or updating it with `Elm.TypeInference.addFile`.

Errors from type inference itself live in
[`Elm.TypeInference.InferError`](Elm-TypeInference-InferError).

@docs ProjectError, Location
@docs toString

-}

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.ModuleName.Extra
import Elm.Syntax.Node as Node
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Elm.TypeInference.Error.Internal exposing (list, record)
import Elm.TypeInference.Type exposing (PackageName, VarName)
import Elm.Writer


{-| Where in a dependency an error was found.
-}
type alias Location =
    { package : PackageName
    , moduleName : ModuleName
    , declarationName : VarName
    }


{-| Types of errors:

  - **`NeedPackageSources`:** Raised by `Elm.TypeInference.init` when
    extra dependency sources are needed to analyze whether a
    `docs.json`-mentioned internal type is a record or a custom type.
    Provide these files in `sourcesToResolveAmbiguity` in the next
    `Elm.TypeInference.init` call.

  - **`MissingModuleName`:** Raised when a `File` has an empty module name.

  - **`ImpossibleDocsType`:** for hand-crafted `docs.json` with nonsensical
    data, like a 4-tuple. Real `docs.json` files emitted by the Elm compiler
    should never produce these.

  - **`ImpossibleType`:** Similar but for hand-crafted `elm-syntax` Files
    given in `sourcesToResolveAmbiguity`.

  - **`AmbiguousModuleOwner`:** Raised when a type in a dependency's
    `docs.json` or sources refers to a module (`moduleName`) exposed by more
    than one package visible to that dependency.

-}
type ProjectError
    = NeedPackageSources (Dict PackageName (List String))
    | MissingModuleName
    | ImpossibleDocsType { location : Location, type_ : Elm.Type.Type }
    | ImpossibleType { location : Location, typeAnnotation : TypeAnnotation }
    | AmbiguousModuleOwner { location : Location, moduleName : String, possiblePackages : List PackageName }


{-| Render an error for diagnostic output.
-}
toString : ProjectError -> String
toString error =
    case error of
        NeedPackageSources needed ->
            "Need package sources "
                ++ record
                    (needed
                        |> Dict.toList
                        |> List.map (\( pkg, paths ) -> ( pkg, list paths ))
                    )

        MissingModuleName ->
            "Missing module name"

        ImpossibleDocsType r ->
            "Impossible docs type "
                ++ docsTypeToString r.type_
                ++ locationToString r.location

        ImpossibleType r ->
            "Impossible type "
                ++ Elm.Writer.write
                    (Elm.Writer.writeTypeAnnotation
                        (Node.Node Range.emptyRange r.typeAnnotation)
                    )
                ++ locationToString r.location

        AmbiguousModuleOwner r ->
            "Ambiguous module owner "
                ++ record
                    [ ( "moduleName", r.moduleName )
                    , ( "possiblePackages", list r.possiblePackages )
                    ]
                ++ locationToString r.location



-- HELPERS


locationToString : Location -> String
locationToString location =
    " (in "
        ++ Elm.Syntax.ModuleName.Extra.toString location.moduleName
        ++ "."
        ++ location.declarationName
        ++ " from "
        ++ location.package
        ++ ")"


docsTypeToString : Elm.Type.Type -> String
docsTypeToString type_ =
    let
        -- Wraps in parens if it wouldn't parse back unambiguously in arg position
        wrapped : Elm.Type.Type -> String
        wrapped t =
            case t of
                Elm.Type.Lambda _ _ ->
                    "(" ++ docsTypeToString t ++ ")"

                Elm.Type.Type _ (_ :: _) ->
                    "(" ++ docsTypeToString t ++ ")"

                _ ->
                    docsTypeToString t
    in
    case type_ of
        Elm.Type.Var name ->
            name

        Elm.Type.Lambda from to ->
            -- `->` is right-associative, so only the left side is ambiguous
            wrapped from ++ " -> " ++ docsTypeToString to

        Elm.Type.Tuple [] ->
            "()"

        Elm.Type.Tuple types ->
            "( " ++ String.join ", " (List.map docsTypeToString types) ++ " )"

        Elm.Type.Type name args ->
            (name :: List.map wrapped args)
                |> String.join " "

        Elm.Type.Record fields extensibleVar ->
            let
                prefix : String
                prefix =
                    case extensibleVar of
                        Nothing ->
                            ""

                        Just var ->
                            var ++ " | "
            in
            if String.isEmpty prefix && List.isEmpty fields then
                "{}"

            else
                let
                    fieldsStr : String
                    fieldsStr =
                        fields
                            |> List.map
                                (\( fieldName, fieldType ) ->
                                    fieldName ++ " : " ++ docsTypeToString fieldType
                                )
                            |> String.join ", "
                in
                "{ " ++ prefix ++ fieldsStr ++ " }"
