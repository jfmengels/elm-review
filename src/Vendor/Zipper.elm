module Vendor.Zipper exposing
    ( Zipper, fromList
    , current
    , next, focusl, focusr, position
    , start
    )

{-|

@docs Zipper, fromList


## Query

Functions that query `Zipper` for additional data.

@docs current


## Bounded Movement

These functions will return `Nothing` when moving out of bounds of `Zipper`.

@docs next, focusl, focusr, position


## Bounds

These helper function will move from either side of a `Zipper`

@docs start

-}

import Array exposing (Array)


{-| Zipper type.

This can be thought of as `NonEmpty` which holds keeps track
of unconsed data.

Unlike `NonEmpty` this type is opaque as it needs to ensure
internal invariants.

-}
type Zipper a
    = Zipper (Array a) Int a


{-| Init `Zipper` from `List`.
This operation is not successful for `[]`

    fromList []
    --> Nothing

    fromList [1, 2, 3]
    --> Just (custom [] 1 [2,3])

-}
fromList : List a -> Maybe (Zipper a)
fromList list =
    case list of
        current_ :: _ ->
            Just (Zipper (Array.fromList list) 0 current_)

        [] ->
            Nothing



-- Query


{-| Get current focus

    custom [1,2] 3 [4,5]
    |> current
    --> 3

-}
current : Zipper a -> a
current (Zipper _ _ current_) =
    current_


position : Zipper a -> Int
position (Zipper _ pos _) =
    pos



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
next (Zipper array pos _) =
    setPosition (pos + 1) array


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
prev (Zipper array pos _) =
    setPosition (pos + 1) array


setPosition : Int -> Array a -> Maybe (Zipper a)
setPosition pos array =
    Array.get pos array
        |> Maybe.map
            (\next_ ->
                Zipper array pos next_
            )



-- Ends


{-| Move focus to the very first value

    custom [ 1, 2, 3 ] 4 [ 5, 6, 7 ]
    |> start
    |> current
    --> 1

-}
start : Zipper a -> Zipper a
start ((Zipper array _ _) as zipper) =
    setPosition 0 array
        |> Maybe.withDefault zipper



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
    if fc (current zipper) then
        Just zipper

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
    if fc (current zipper) then
        Just zipper

    else
        case prev zipper of
            Just z ->
                focusl fc z

            Nothing ->
                Nothing
