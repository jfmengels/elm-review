module Vendor.NonEmpty exposing
    ( NonEmpty
    , fromList
    )

{-|

@docs NonEmpty


# Create

@docs fromList

-}


{-| `NonEmpty` list is an alias for a pair of `a` and `List a`.

This makes it possible to construct value of non empty List
without relying on any specific implementation of this type.

-}
type alias NonEmpty a =
    ( a, List a )


{-| Converts List to `Maybe NonEmpty`.

    fromList [ 1, 2 ]
    --> Just ( 1, [ 2 ] )

    fromList []
    --> Nothing

-}
fromList : List a -> Maybe (NonEmpty a)
fromList xs =
    case xs of
        h :: t ->
            Just ( h, t )

        [] ->
            Nothing
