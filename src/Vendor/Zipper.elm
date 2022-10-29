module Vendor.Zipper exposing
    ( Zipper, fromList
    , current
    , next
    , start
    , focusl, focusr
    )

{-|

@docs Zipper, fromList


## Query

Functions that query `Zipper` for additional data.

@docs current


## Bounded Movement

These function will return `Nothing` when moving out of bounds of `Zipper`.

@docs next


## Bounds

These helper function will move from either side of a `Zipper`

@docs start

-}

import Vendor.NonEmpty as NE exposing (NonEmpty)


{-| Zipper type.

This can be thought of as `NonEmpty` which holds keeps track
of unconsed data.

Unlike `NonEmpty` this type is opaque as it needs to ensure
internal invariants.

-}
type Zipper a
    = Zipper (List a) a (List a)


{-| Init `Zipper` from `NonEmpty` list type.

    fromNonEmpty ( 1, [ 2, 3 ] )
    |> current
    --> 1

    fromNonEmpty ( 1, [ 2, 3 ] )
    |> toList
    --> [ 1, 2, 3 ]

-}
fromNonEmpty : NonEmpty a -> Zipper a
fromNonEmpty ( h, t ) =
    Zipper [] h t


{-| Init `Zipper` from `List`.
This operation is not successful for `[]`

    fromList []
    --> Nothing

    fromList [1, 2, 3]
    --> Just (custom [] 1 [2,3])

-}
fromList : List a -> Maybe (Zipper a)
fromList =
    Maybe.map fromNonEmpty << NE.fromList



-- Query


{-| Get current focus

    custom [1,2] 3 [4,5]
    |> current
    --> 3

-}
current : Zipper a -> a
current (Zipper _ f _) =
    f



-- Movement


{-| Move focus to next value.

    custom [] 1 [2,3]
    |> next
    |> Maybe.map current
    |> Just 2

    custom [] 1 []
    |> next
    --> Nothing

-}
next : Zipper a -> Maybe (Zipper a)
next (Zipper p f n) =
    case n of
        [] ->
            Nothing

        h :: t ->
            Just <| Zipper (f :: p) h t


{-| Move focus to next value.

    custom [1, 2] 3 []
    |> prev
    |> Maybe.map current
    |> Just 2

    custom [] 1 []
    |> prev
    --> Nothing

-}
prev : Zipper a -> Maybe (Zipper a)
prev (Zipper p f n) =
    case p of
        [] ->
            Nothing

        h :: t ->
            Just <| Zipper t h <| f :: n



-- Ends


{-| Move focus to the very first value

    custom [ 1, 2, 3 ] 4 [ 5, 6, 7 ]
    |> start
    |> current
    --> 1

-}
start : Zipper a -> Zipper a
start =
    toEndHelper prev


toEndHelper : (a -> Maybe a) -> a -> a
toEndHelper f acc =
    case f acc of
        Just val ->
            toEndHelper f val

        Nothing ->
            acc



-- Cycling


{-| Move focus to the first next element that satisfy the predicate.

    fromConsList [1,2] (3, [4, 5])
    |> focusr ((==) 5)
    |> Maybe.map current
    --> Just 5

    fromConsList [1,2] (3, [4, 5])
    |> focusr ((==) 3)
    |> Maybe.map current
    --> Just 3

    fromConsList [1,2] (3, [4, 5])
    |> focusr ((==) 1)
    |> Maybe.map current
    --> Nothing

-}
focusr : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
focusr fc zipper =
    if fc <| current zipper then
        Just <| zipper

    else
        case next zipper of
            Just z ->
                focusr fc z

            Nothing ->
                Nothing


{-| Move focus to the first previous element that satisfy the predicate

    fromConsList [1,2] (3, [4, 5])
    |> focusl ((==) 1)
    |> Maybe.map current
    --> Just 1

    fromConsList [1,2] (3, [4, 5])
    |> focusl ((==) 3)
    |> Maybe.map current
    --> Just 3

    fromConsList [1,2] (3, [4, 5])
    |> focusl ((==) 4)
    |> Maybe.map current
    --> Nothing

-}
focusl : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
focusl fc zipper =
    if fc <| current zipper then
        Just <| zipper

    else
        case prev zipper of
            Just z ->
                focusl fc z

            Nothing ->
                Nothing
