module Elm.TypeInference.TypeEquation exposing (TypeEquation, dropLabel)

import Elm.TypeInference.Type.Internal exposing (MonoType)


{-| Equations are always between mono types, never between schemes (foralls).
If schemes are involved, they get instantiated to mono types first.

The third element (String) is a debugging label.

-}
type alias TypeEquation =
    ( MonoType, MonoType, String )


dropLabel : TypeEquation -> ( MonoType, MonoType )
dropLabel ( t1, t2, _ ) =
    ( t1, t2 )
