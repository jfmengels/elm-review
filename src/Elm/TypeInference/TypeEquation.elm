module Elm.TypeInference.TypeEquation exposing
    ( Equations
    , TypeEquation
    , append
    , batch
    , cons
    , dropLabel
    , empty
    , single
    , toList
    )

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


{-| O(1)-append equation builder (rope-like)
-}
type Equations
    = Empty
    | One TypeEquation
    | Batch (List TypeEquation)
    | Append Equations Equations


empty : Equations
empty =
    Empty


single : TypeEquation -> Equations
single eq =
    One eq


batch : List TypeEquation -> Equations
batch eqs =
    case eqs of
        [] ->
            Empty

        [ eq ] ->
            One eq

        _ ->
            Batch eqs


cons : TypeEquation -> Equations -> Equations
cons eq rest =
    case rest of
        Empty ->
            One eq

        _ ->
            Append (One eq) rest


append : Equations -> Equations -> Equations
append left right =
    case ( left, right ) of
        ( Empty, r ) ->
            r

        ( l, Empty ) ->
            l

        _ ->
            Append left right


toList : Equations -> List TypeEquation
toList eqns =
    List.reverse (go [ eqns ] [])


go : List Equations -> List TypeEquation -> List TypeEquation
go stack acc =
    case stack of
        [] ->
            acc

        Empty :: rest ->
            go rest acc

        (One eq) :: rest ->
            go rest (eq :: acc)

        (Batch eqs) :: rest ->
            go rest (List.foldl (::) acc eqs)

        (Append left right) :: rest ->
            go (left :: right :: rest) acc
