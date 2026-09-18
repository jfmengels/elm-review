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


{-| Types of errors:

  - **`ImpossibleExpr`, `ImpossiblePattern` and `ImpossibleType`:** for
    hand-crafted `elm-syntax` Files with nonsensical data, like a function call
    without arguments or a 4-tuple. You should never be able to reach these when
    using `elm-syntax`'s parser on real Elm files.

  - **`ImpossibleDocsType`:** Similar but for hand-crafted `docs.json`. Real
    `docs.json` files emitted by the Elm compiler should never produce these.

  - **`MissingModuleName`:** Raised when `Elm.TypeInference.project` is called
    with a Dict key `[]`.

  - **`ModuleNotFound`:** Raised when `Elm.TypeInference.inferModule` is called
    with module that's not part of the indexed `Project`.

  - **`VarNotFound`:**

        module Main exposing (foo)

        foo =
            bar

        --> VarNotFound
        --    { usedIn = [ "Main" ]
        --    , varName = "bar"
        --    }

  - **`AmbiguousName`:**

        module Main exposing (foo)

        import A exposing (thing)
        import B exposing (thing)

        foo =
            thing

        --> AmbiguousName
        --    { usedIn = [ "Main" ]
        --    , varName = "thing"
        --    , possibleModules = [ [ "A" ], [ "B" ] ]
        --    }

  - **`AmbiguousModuleOwner`:**

    Raised when two direct dependencies expose a module of the same name, eg.
    `mdgriffith/elm-ui` and `mdgriffith/style-elements` (both expose `Element`):

        module Main exposing (foo)

        import Element

        foo _ =
            Element.text "hi"

        --> AmbiguousModuleOwner
        --    { moduleName = "Element"
        --    , possiblePackages =
        --        [ "mdgriffith/elm-ui"
        --        , "mdgriffith/style-elements"
        --        ]
        --    }

  - **`TypeMismatch`:**

        module Main exposing (foo)

        foo : Int
        foo =
            "abc"

        --> TypeMismatch Type.Int Type.String

  - **`InfiniteType`:**

        module Main exposing (foo)

        foo x =
            x x

        --> InfiniteType
        --    (Type.TypeVar "a")
        --    (Type.Function
        --      { from = Type.TypeVar "a"
        --      , to = Type.TypeVar "b"
        --      }
        --    )

  - **`ConstraintMismatch`:**

        module Main exposing (foo)

        foo x =
            x + "a"

        --> ConstraintMismatch
        --    (Type.TypeVar "number")
        --    Type.String

-}
type ErrorDetails
    = -- Syntax errors
      ImpossibleExpr (Node Expression)
    | ImpossiblePattern (Node Pattern)
    | ImpossibleType TypeAnnotation
    | ImpossibleDocsType Elm.Type.Type
    | MissingModuleName
    | ModuleNotFound
      -- Var qualification errors
    | VarNotFound { usedIn : ModuleName, varName : VarName }
    | AmbiguousName { usedIn : ModuleName, varName : VarName, possibleModules : List ModuleName }
    | AmbiguousModuleOwner { moduleName : String, possiblePackages : List String }
      -- Type errors
    | TypeMismatch Type Type
    | InfiniteType Type Type
    | ConstraintMismatch Type Type


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
                [ "Impossible expression"
                , rangeToString (Node.range exprNode)
                , Elm.Writer.write (Elm.Writer.writeExpression exprNode)
                ]

        ImpossiblePattern patternNode ->
            String.join " "
                [ "Impossible pattern"
                , rangeToString (Node.range patternNode)
                , Elm.Writer.write (Elm.Writer.writePattern patternNode)
                ]

        ImpossibleType typeAnnotation ->
            "Impossible type "
                ++ Elm.Writer.write
                    (Elm.Writer.writeTypeAnnotation
                        (Node.Node Range.emptyRange typeAnnotation)
                    )

        ImpossibleDocsType type_ ->
            "Impossible docs type " ++ docsTypeToString type_

        MissingModuleName ->
            "Missing module name"

        ModuleNotFound ->
            "Module not found"

        VarNotFound r ->
            "Var not found "
                ++ record
                    [ ( "usedIn", Elm.Syntax.ModuleName.Extra.toString r.usedIn )
                    , ( "varName", r.varName )
                    ]

        AmbiguousName r ->
            "Ambiguous name "
                ++ record
                    [ ( "usedIn", Elm.Syntax.ModuleName.Extra.toString r.usedIn )
                    , ( "varName", r.varName )
                    , ( "possibleModules", list (List.map Elm.Syntax.ModuleName.Extra.toString r.possibleModules) )
                    ]

        AmbiguousModuleOwner r ->
            "Ambiguous module owner "
                ++ record
                    [ ( "moduleName", r.moduleName )
                    , ( "possiblePackages", list r.possiblePackages )
                    ]

        TypeMismatch t1 t2 ->
            String.join " "
                [ "Type mismatch"
                , parenIfHasSpace (Type.toString t1)
                , parenIfHasSpace (Type.toString t2)
                ]

        InfiniteType varType type_ ->
            String.join " "
                [ "Infinite type"
                , parenIfHasSpace (Type.toString varType)
                , parenIfHasSpace (Type.toString type_)
                ]

        ConstraintMismatch varType type_ ->
            String.join " "
                [ "Constraint mismatch"
                , parenIfHasSpace (Type.toString varType)
                , parenIfHasSpace (Type.toString type_)
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
