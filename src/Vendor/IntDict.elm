module Vendor.IntDict exposing
    ( IntDict
    , empty, insert, remove, mapKey
    , member, get
    , keys
    )

{-|


# IntDict

This module exposes the same API as [`Dict`](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict).


# Technicalities

Since JavaScript's number type is kind of messed up, Elm's `Int` is not particularly
well-behaved wrt. bitwise operations. Currently, JS supports 32 bit integers, so there is
probably enough room for key picks. **However, when sanitizing user input, it is mandatory
that a prior `isValidKey` or one of the safe versions in `IntDict.Safe` is used!** This is
to prevent the overflow behavior.

This library is inspired by Haskells [IntMap](http://hackage.haskell.org/package/containers-0.2.0.1/docs/Data-IntMap.html),
which in turn implements Okasaki and Gill's [Fast mergable integer maps](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf).

As noted in the [references](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf), here are some runtimes:

_O(min(n, W))_: `insert`, `mapKey`, `remove`, `get`, `member`

_O(n + m)_: `uniteWith`, `union`, `intersection`, `diff`, `merge`

where _n_ and _m_ are the sizes of the first and second dictionary respectively and _W_
is the number of bits in `Int` (so a constant with current value 32).

Dictionary equality with `(==)` is unreliable and should not be used.


# Data

@docs IntDict


# Build

@docs empty, insert, remove, mapKey


# Query

@docs member, get


# Combine


# Lists

@docs keys


# Transform


# String representation

-}

import Bitwise


type alias KeyPrefix =
    { prefixBits : Int -- higher key prefix excluding the branching bit
    , branchingBit : Int -- already in 2^i form -> always > 0 (except when the sign bit is set, then it's < 0)
    }



-- only so that we don't repeat ourselves


type alias InnerType v =
    { prefix : KeyPrefix
    , left : IntDict v
    , right : IntDict v
    }


{-| A dictionary mapping `Int`s to values of a type `v`. Analogous to
`Dict Int v`.
-}
type IntDict v
    = Empty () -- Invariant: Never child of an `Inner` node
    | Leaf { key : Int, value : v }
    | Inner (InnerType v)



-- into an integer. We can then check for overflow.
-- This seems easier than checking for 32 bits.
-- `or` 0 is similar to `mod` <32bits>
-- SMART CONSTRUCTORS
-- not exported
-- exported as the singleton alias


leaf : Int -> v -> IntDict v
leaf k v =
    Leaf
        { key = k
        , value = v
        }



-- SOME PRIMITIVES


{-| Consider a branchingBit of 2^4 = 16 = 0b00010000.
Then branchingBit-1 = 15 = 0b00001111,
Now apply bitwise NOT to get the mask 0b11110000.
Finally, we clear out the branchingBit itself with `Bitwise.xor`.
-}
higherBitMask : Int -> Int
higherBitMask branchingBit =
    branchingBit
        - 1
        |> Bitwise.complement
        |> Bitwise.xor branchingBit


prefixMatches : KeyPrefix -> Int -> Bool
prefixMatches p n =
    Bitwise.and n (higherBitMask p.branchingBit) == p.prefixBits


{-| Clear all bits other than the highest in n.
For implementation notes, see [this](<http://aggregate.org/MAGIC/#Most> Significant 1 Bit).
-}
highestBitSet : Int -> Int
highestBitSet n =
    let
        shiftOr i shift =
            Bitwise.or i (Bitwise.shiftRightZfBy shift i)

        n1 =
            shiftOr n 1

        n2 =
            shiftOr n1 2

        n3 =
            shiftOr n2 4

        n4 =
            shiftOr n3 8

        n5 =
            shiftOr n4 16

        -- n6 = shiftOr n5 32 -- 64 bit support?!
        -- n5 has the same msb set as diff. However, all
        -- bits below the msb are also 1! This means we can
        -- do the following to get the msb:
    in
    n5 |> Bitwise.shiftRightZfBy 1 |> Bitwise.complement |> Bitwise.and n5


{-| Compute the longest common prefix of two keys.
Returns 0 as branchingBit if equal.

Find the highest bit not set in


    diff =
        x `xor` y

    -- 0b011001 `xor` 0b011010 = 0b000011

-}
lcp : Int -> Int -> KeyPrefix
lcp x y =
    let
        branchingBit =
            highestBitSet (Bitwise.xor x y)

        mask =
            higherBitMask branchingBit

        prefixBits =
            Bitwise.and x mask

        -- should equal y & mask
    in
    { prefixBits = prefixBits
    , branchingBit = branchingBit
    }


signBit : Int
signBit =
    highestBitSet -1


isBranchingBitSet : KeyPrefix -> Int -> Bool
isBranchingBitSet p n =
    (n
        |> Bitwise.xor signBit
        -- This is a hack that fixes the ordering of keys.
        |> Bitwise.and p.branchingBit
    )
        /= 0



-- BUILD


{-| Create an empty dictionary.
-}
empty : IntDict v
empty =
    Empty ()


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : Int -> v -> IntDict v -> IntDict v
insert key value dict =
    let
        join : Int -> IntDict v
        join k2 =
            let
                prefix : KeyPrefix
                prefix =
                    lcp key k2
            in
            if isBranchingBitSet prefix k2 then
                Inner
                    { prefix = prefix
                    , left = leaf key value
                    , right = dict
                    }

            else
                Inner
                    { prefix = prefix
                    , left = dict
                    , right = leaf key value
                    }
    in
    case dict of
        Empty () ->
            leaf key value

        Leaf l ->
            if l.key == key then
                leaf key value

            else
                join l.key

        Inner i ->
            if prefixMatches i.prefix key then
                if isBranchingBitSet i.prefix key then
                    if i.left == empty then
                        insert key value i.right

                    else
                        Inner
                            { prefix = i.prefix
                            , left = i.left
                            , right = insert key value i.right
                            }

                else if i.right == empty then
                    insert key value i.left

                else
                    Inner
                        { prefix = i.prefix
                        , left = insert key value i.left
                        , right = i.right
                        }

            else
                join i.prefix.prefixBits


{-| Update the value of a dictionary for a specific key with a given function.
-}
mapKey : Int -> (v -> v) -> IntDict v -> IntDict v
mapKey key alter dict =
    case dict of
        Empty () ->
            empty

        Leaf l ->
            if l.key == key then
                leaf key (alter l.value)

            else
                dict

        -- This potentially inserts a new node
        Inner { prefix, left, right } ->
            if prefixMatches prefix key then
                if isBranchingBitSet prefix key then
                    Inner { prefix = prefix, left = left, right = mapKey key alter right }

                else
                    Inner { prefix = prefix, left = mapKey key alter left, right = right }

            else
                dict


{-| Update the value of a dictionary for a specific key with a given function.
-}
remove : Int -> IntDict v -> IntDict v
remove key dict =
    case dict of
        Empty () ->
            empty

        Leaf l ->
            if l.key == key then
                empty

            else
                dict

        Inner { prefix, left, right } ->
            if prefixMatches prefix key then
                if isBranchingBitSet prefix key then
                    let
                        r : IntDict v
                        r =
                            remove key right
                    in
                    if left == empty then
                        r

                    else if r == empty then
                        left

                    else
                        Inner
                            { prefix = prefix
                            , left = left
                            , right = r
                            }

                else
                    let
                        l : IntDict v
                        l =
                            remove key left
                    in
                    if l == empty then
                        right

                    else if right == empty then
                        l

                    else
                        Inner
                            { prefix = prefix
                            , left = l
                            , right = right
                            }

            else
                dict



-- QUERY


{-| Determine if a key is in a dictionary.
-}
member : Int -> IntDict v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.
-}
get : Int -> IntDict v -> Maybe v
get key dict =
    case dict of
        Empty () ->
            Nothing

        Leaf l ->
            if l.key == key then
                Just l.value

            else
                Nothing

        Inner i ->
            if not (prefixMatches i.prefix key) then
                Nothing

            else if
                -- continue in left or right branch
                isBranchingBitSet i.prefix key
            then
                -- depending on whether the branching
                get key i.right

            else
                -- bit is set in the key
                get key i.left



-- TRANSFORM


{-| Fold over the keys pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (Int -> a -> a) -> a -> IntDict v -> a
foldr f acc dict =
    case dict of
        Empty () ->
            acc

        Leaf l ->
            f l.key acc

        Inner i ->
            foldr f (foldr f acc i.right) i.left



-- COMBINE
{- Take bits from a or b, depending on the value of the bit in that position in mask.
   0 -> a, 1 -> b. Implemented as a & ~mask | b & mask
-}
{- While merging/uniting 2 inner nodes, we encounter the 4 possible base cases
   represented by BranchRelation. This function computes that relation.
-}
-- `Right` --> `r` is the left child.
-- We have no common keys
-- l and r contain different keys
-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.
-}
keys : IntDict v -> List Int
keys dict =
    foldr (\key keyList -> key :: keyList) [] dict
