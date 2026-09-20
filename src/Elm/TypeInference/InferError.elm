module Elm.TypeInference.InferError exposing
    ( InferError, InferErrorDetails(..)
    , toString
    )

{-| Errors reported while resolving or inferring a module.

Errors from building a `Project` (`Elm.TypeInference.init`,
`Elm.TypeInference.addFile`) live in
[`Elm.TypeInference.ProjectError`](Elm-TypeInference-ProjectError).

@docs InferError, InferErrorDetails
@docs toString

-}

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.ModuleName.Extra
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.TypeInference.Error.Internal exposing (list, record)
import Elm.TypeInference.Type as Type exposing (Type, VarName)
import Elm.Writer


{-| A type inference error + location info.
-}
type alias InferError =
    { moduleName : ModuleName
    , declarationNames : List VarName
    , details : InferErrorDetails
    }


{-| Types of errors:

  - **`ImpossibleExpr`, `ImpossiblePattern` and `ImpossibleType`:** for
    hand-crafted `elm-syntax` Files with nonsensical data, like a function call
    without arguments or a 4-tuple. You should never be able to reach these when
    using `elm-syntax`'s parser on real Elm files.

  - **`ModuleNotFound`:** Raised when `Elm.TypeInference.getType` is called
    with module that's not part of the indexed `Project`.

  - **`RangeNotFound`:** Raised when `Elm.TypeInference.getType` is called
    with a `Range` not corresponding to an AST node from the provided `File`s.

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
type InferErrorDetails
    = -- Syntax errors
      ImpossibleExpr (Node Expression)
    | ImpossiblePattern (Node Pattern)
    | ImpossibleType TypeAnnotation
    | ModuleNotFound
    | RangeNotFound
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
toString : InferError -> String
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


detailsToString : InferErrorDetails -> String
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

        ModuleNotFound ->
            "Module not found"

        RangeNotFound ->
            "Range not found"

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


rangeToString : Range -> String
rangeToString { start } =
    String.fromInt start.row ++ ":" ++ String.fromInt start.column
