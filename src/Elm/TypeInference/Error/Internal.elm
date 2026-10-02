module Elm.TypeInference.Error.Internal exposing (FromTypeAnnotationError(..), ResolverAmbiguity, list, record)

{-| Shared error types for type-annotation resolution.

Leaf module on purpose: `Elm.TypeInference.Type.Internal` needs these for
`fromTypeAnnotation` and `fromTypeAnnotationError`. Defining them here keeps
the dependency graph acyclic (`Error.Internal` depends only on `elm-syntax`,
never on inference modules).
Type errors themselves carry only the public `Elm.TypeInference.Type`, so
`InferError` never needs `MonoType` from `Type.Internal`.

@docs FromTypeAnnotationError, ResolverAmbiguity
@docs list, record

-}

import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)


{-| Type/module collision
-}
type alias ResolverAmbiguity =
    { moduleName : String
    , possiblePackages : List String
    }


{-| What can go wrong when resolving a `TypeAnnotation` to a `MonoType`.
-}
type FromTypeAnnotationError
    = ImpossibleAnnotation TypeAnnotation
    | {- To trigger: have mdgriffith/elm-ui and mdgriffith/style-elements as direct deps (both expose Element.Element).
         Then this is enough:

             import Element

             thing : Element.Element msg -> Int
             thing _ = 1

      -}
      AmbiguousModuleName ResolverAmbiguity


{-| Renders `{ key = value, ... }` for diagnostic output.
-}
record : List ( String, String ) -> String
record fields =
    fields
        |> List.map (\( key, value ) -> key ++ " = " ++ value)
        |> String.join ", "
        |> (\str -> "{ " ++ str ++ " }")


{-| Renders `[a, b, ...]` for diagnostic output.
-}
list : List String -> String
list items =
    "[" ++ String.join ", " items ++ "]"
