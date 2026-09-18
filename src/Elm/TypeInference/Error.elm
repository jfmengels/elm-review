module Elm.TypeInference.Error exposing
    ( Error, ErrorDetails(..)
    , toString
    )

{-| Errors reported while resolving or inferring a module.

@docs Error, ErrorDetails
@docs toString

-}

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.ModuleName.Extra
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Elm.TypeInference.Type as Type exposing (Type, VarName)
import Elm.Writer


{-| A type inference error + location info.
-}
type alias Error =
    { moduleName : ModuleName
    , declarationNames : List VarName
    , details : ErrorDetails
    }


{-| Types of errors.
-}
type ErrorDetails
    = -- Syntax errors
      ImpossibleExpr (Node Expression)
    | ImpossiblePattern (Node Pattern)
    | ImpossibleType TypeAnnotation
    | ImpossibleDocsType Elm.Type.Type
    | MissingModuleName
      -- Var qualification errors
    | VarNotFound { usedIn : ModuleName, varName : VarName }
    | AmbiguousName { usedIn : ModuleName, varName : VarName, possibleModules : List ModuleName }
    | AmbiguousModuleOwner { moduleName : String, possiblePackages : List String }
      -- Type errors
    | TypeMismatch Type Type
    | InfiniteType Type Type
    | ConstraintMismatch Type Type
    | InternalInconsistency Type Type


{-| Render an error for diagnostic output.
-}
toString : Error -> String
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


detailsToString : ErrorDetails -> String
detailsToString details =
    case details of
        ImpossibleExpr exprNode ->
            String.join " "
                [ "ImpossibleExpr"
                , rangeToString (Node.range exprNode)
                , Elm.Writer.write (Elm.Writer.writeExpression exprNode)
                ]

        ImpossiblePattern patternNode ->
            String.join " "
                [ "ImpossiblePattern"
                , rangeToString (Node.range patternNode)
                , Elm.Writer.write (Elm.Writer.writePattern patternNode)
                ]

        ImpossibleType typeAnnotation ->
            "ImpossibleType "
                ++ Elm.Writer.write
                    (Elm.Writer.writeTypeAnnotation
                        (Node.Node Range.emptyRange typeAnnotation)
                    )

        ImpossibleDocsType type_ ->
            "ImpossibleDocsType " ++ docsTypeToString type_

        MissingModuleName ->
            "MissingModuleName"

        VarNotFound r ->
            "VarNotFound "
                ++ record
                    [ ( "usedIn", Elm.Syntax.ModuleName.Extra.toString r.usedIn )
                    , ( "varName", r.varName )
                    ]

        AmbiguousName r ->
            "AmbiguousName "
                ++ record
                    [ ( "usedIn", Elm.Syntax.ModuleName.Extra.toString r.usedIn )
                    , ( "varName", r.varName )
                    , ( "possibleModules", list (List.map Elm.Syntax.ModuleName.Extra.toString r.possibleModules) )
                    ]

        AmbiguousModuleOwner r ->
            "AmbiguousModuleOwner "
                ++ record
                    [ ( "moduleName", r.moduleName )
                    , ( "possiblePackages", list r.possiblePackages )
                    ]

        TypeMismatch t1 t2 ->
            String.join " "
                [ "TypeMismatch"
                , parenIfHasSpace (Type.toString t1)
                , parenIfHasSpace (Type.toString t2)
                ]

        InfiniteType varType type_ ->
            String.join " "
                [ "InfiniteType"
                , parenIfHasSpace (Type.toString varType)
                , parenIfHasSpace (Type.toString type_)
                ]

        ConstraintMismatch varType type_ ->
            String.join " "
                [ "ConstraintMismatch"
                , parenIfHasSpace (Type.toString varType)
                , parenIfHasSpace (Type.toString type_)
                ]

        InternalInconsistency t1 t2 ->
            String.join " "
                [ "InternalInconsistency"
                , parenIfHasSpace (Type.toString t1)
                , parenIfHasSpace (Type.toString t2)
                ]



-- HELPERS


{-| Adds (...) if the string has spaces.
Handy for types: eg. `TypeMismatch Int (List String)`
-}
parenIfHasSpace : String -> String
parenIfHasSpace str =
    if String.contains " " str then
        "(" ++ str ++ ")"

    else
        str


record : List ( String, String ) -> String
record fields =
    fields
        |> List.map (\( key, value ) -> key ++ " = " ++ value)
        |> String.join ", "
        |> (\str -> "{ " ++ str ++ " }")


list : List String -> String
list items =
    "[" ++ String.join ", " items ++ "]"


rangeToString : Range -> String
rangeToString { start } =
    String.fromInt start.row ++ ":" ++ String.fromInt start.column


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
