module Elm.TypeInference.Error.Internal exposing (FromTypeAnnotationError(..), ResolverAmbiguity)

{-| Shared error types for type-annotation resolution.

Leaf module on purpose: `Elm.TypeInference.Type.Internal` needs these for
`fromTypeAnnotation` and `fromTypeAnnotationError`. Defining them here keeps
the dependency graph acyclic (`Error.Internal` depends only on `elm-syntax`,
never on inference modules).
Type errors themselves carry only the public `Elm.TypeInference.Type`, so
`Error` never needs `MonoType` from `Type.Internal`.

@docs FromTypeAnnotationError, ResolverAmbiguity

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
    | AmbiguousModuleName ResolverAmbiguity
