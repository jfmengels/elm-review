module Vendor.Fifo exposing
    ( Fifo, empty
    , insert, remove
    )

{-|


# Creating FIFOs

@docs Fifo, empty


# Inserting/Removing

@docs insert, remove


# To List

-}


{-| A FIFO containing items of type `a`.
-}
type Fifo a
    = Fifo (List a) (List a)


{-| Creates an empty Fifo.

    Fifo.empty
        -- == Fifo.fromList []

-}
empty : Fifo a
empty =
    Fifo [] []


{-| Inserts an item into a Fifo

    Fifo.empty
    |> Fifo.insert 7
    |> Fifo.insert 8
        -- == Fifo.fromList [7,8]

-}
insert : a -> Fifo a -> Fifo a
insert a (Fifo front back) =
    Fifo front (a :: back)


{-| Removes the next (oldest) item from a Fifo, returning the item (if any), and the updated Fifo.

    Fifo.fromList [3,7]
    |> Fifo.remove
        -- == (Just 3, Fifo.fromList [7])

-}
remove : Fifo a -> Maybe ( a, Fifo a )
remove fifo =
    case fifo of
        Fifo [] [] ->
            Nothing

        Fifo [] back ->
            remove <| Fifo (List.reverse back) []

        Fifo (next :: rest) back ->
            Just ( next, Fifo rest back )
