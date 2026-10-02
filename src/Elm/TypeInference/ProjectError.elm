module Elm.TypeInference.ProjectError exposing
    ( ProjectError, ProjectErrorDetails(..)
    , toString
    )

{-| Errors reported while building a `Project` with
`Elm.TypeInference.init` or updating it with `Elm.TypeInference.addFile`.

Errors from type inference itself live in
[`Elm.TypeInference.InferError`](Elm-TypeInference-InferError).

@docs ProjectError, ProjectErrorDetails
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
import Elm.TypeInference.Type exposing (VarName)
import Elm.Writer


{-| A project initialization error + location info.
-}
type alias ProjectError =
    { moduleName : ModuleName
    , declarationNames : List VarName
    , details : ProjectErrorDetails
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
    `docs.json` or sources refers to a module exposed by more than one
    package visible to that dependency.

-}
type ProjectErrorDetails
    = NeedPackageSources (Dict String (List String))
    | MissingModuleName
    | ImpossibleDocsType Elm.Type.Type
    | ImpossibleType TypeAnnotation
    | AmbiguousModuleOwner { moduleName : String, possiblePackages : List String }


{-| Render an error for diagnostic output.
-}
toString : ProjectError -> String
toString error =
    detailsToString error.details
        ++ " (in "
        ++ Elm.Syntax.ModuleName.Extra.toString error.moduleName
        ++ (if List.isEmpty error.declarationNames then
                ""

            else
                "." ++ String.join "/" error.declarationNames
           )
        ++ ")"


detailsToString : ProjectErrorDetails -> String
detailsToString details =
    case details of
        NeedPackageSources needed ->
            "Need package sources "
                ++ record
                    (needed
                        |> Dict.toList
                        |> List.map (\( pkg, paths ) -> ( pkg, list paths ))
                    )

        MissingModuleName ->
            "Missing module name"

        ImpossibleDocsType type_ ->
            "Impossible docs type " ++ docsTypeToString type_

        ImpossibleType typeAnnotation ->
            "Impossible type "
                ++ Elm.Writer.write
                    (Elm.Writer.writeTypeAnnotation
                        (Node.Node Range.emptyRange typeAnnotation)
                    )

        AmbiguousModuleOwner r ->
            "Ambiguous module owner "
                ++ record
                    [ ( "moduleName", r.moduleName )
                    , ( "possiblePackages", list r.possiblePackages )
                    ]



-- HELPERS


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
