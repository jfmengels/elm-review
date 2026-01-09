module Vendor.IntSet exposing
    ( IntSet
    , empty, insert
    , member, findMin
    , intersect
    , keys
    , foldl
    )

{-|


# IntSet

This module exposes the same API as [`Dict`](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict).


# Technicalities

Since JavaScript's number type is kind of messed up, Elm's `Int` is not particularly
well-behaved wrt. bitwise operations. Currently, JS supports 32 bit integers, so there is
probably enough room for key picks. **However, when sanitizing user input, it is mandatory
that a prior `isValidKey` or one of the safe versions in `IntSet.Safe` is used!** This is
to prevent the overflow behavior.

This library is inspired by Haskells [IntMap](http://hackage.haskell.org/package/containers-0.2.0.1/docs/Data-IntMap.html),
which in turn implements Okasaki and Gill's [Fast mergable integer maps](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf).

As noted in the [references](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf), here are some runtimes:

_O(min(n, W))_: `insert`, `update`, `remove`, `get`, `member`

_O(n + m)_: `uniteWith`, `union`, `intersection`, `diff`, `merge`

where _n_ and _m_ are the sizes of the first and second dictionary respectively and _W_
is the number of bits in `Int` (so a constant with current value 32).

Dictionary equality with `(==)` is unreliable and should not be used.


# Data

@docs IntSet


# Build

@docs empty, insert


# Query

@docs member, findMin


# Combine

@docs intersect


# Lists

@docs keys


# Transform

@docs foldl


# String representation

-}

import Bitwise


type alias KeyPrefix =
    { prefixBits : Int -- higher key prefix excluding the branching bit
    , branchingBit : Int -- already in 2^i form -> always > 0 (except when the sign bit is set, then it's < 0)
    }



-- only so that we don't repeat ourselves


type alias InnerType =
    { prefix : KeyPrefix
    , left : IntSet
    , right : IntSet
    }


{-| A dictionary mapping `Int`s to values of a type `v`. Analogous to
`Dict Int v`.
-}
type IntSet
    = Empty () -- Invariant: Never child of an `Inner` node
    | Leaf Int
    | Inner InnerType



-- into an integer. We can then check for overflow.
-- This seems easier than checking for 32 bits.
-- `or` 0 is similar to `mod` <32bits>
-- SMART CONSTRUCTORS
-- not exported


inner : KeyPrefix -> IntSet -> IntSet -> IntSet
inner p l r =
    if l == empty then
        r

    else if r == empty then
        l

    else
        Inner
            { prefix = p
            , left = l
            , right = r
            }



-- exported as the singleton alias


leaf : Int -> IntSet
leaf =
    Leaf



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


mostSignificantBranchingBit : Int -> Int -> Int
mostSignificantBranchingBit a b =
    if a == signBit || b == signBit then
        signBit

    else
        max a b


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
isBranchingBitSet p =
    Bitwise.xor signBit
        -- This is a hack that fixes the ordering of keys.
        >> Bitwise.and p.branchingBit
        >> (/=) 0



-- BUILD


{-| Create an empty dictionary.
-}
empty : IntSet
empty =
    Empty ()


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : Int -> IntSet -> IntSet
insert key dict =
    let
        -- The inner constructor will do the rest
        join k2 =
            -- precondition: k1 /= k2
            let
                prefix =
                    lcp key k2
            in
            if
                isBranchingBitSet prefix k2
                -- if so, r will be the right child
            then
                inner prefix (leaf key) dict

            else
                inner prefix dict (leaf key)
    in
    case dict of
        Empty () ->
            leaf key

        Leaf leafKey ->
            if leafKey == key then
                dict
                -- This updates or removes the leaf with the same key

            else
                join leafKey

        -- This potentially inserts a new node
        Inner i ->
            if prefixMatches i.prefix key then
                if isBranchingBitSet i.prefix key then
                    inner i.prefix i.left (insert key i.right)

                else
                    inner i.prefix (insert key i.left) i.right

            else
                -- we have to join a new leaf with the current diverging Inner node
                join i.prefix.prefixBits



-- QUERY


{-| Determine if a key is in a dictionary.
-}
member : Int -> IntSet -> Bool
member key dict =
    case dict of
        Empty () ->
            False

        Leaf leafKey ->
            leafKey == key

        Inner i ->
            if not (prefixMatches i.prefix key) then
                False

            else if
                -- continue in left or right branch
                isBranchingBitSet i.prefix key
            then
                -- depending on whether the branching
                member key i.right

            else
                -- bit is set in the key
                member key i.left


{-| Find the minimum key and value in the dictionary.
-}
findMin : IntSet -> Maybe Int
findMin dict =
    case dict of
        Empty () ->
            Nothing

        Leaf key ->
            Just key

        Inner i ->
            findMin i.left



-- TRANSFORM


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (Int -> a -> a) -> a -> IntSet -> a
foldl f acc dict =
    case dict of
        Empty () ->
            acc

        Leaf key ->
            f key acc

        Inner i ->
            foldl f (foldl f acc i.left) i.right


{-| Fold over the keys pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (Int -> a -> a) -> a -> IntSet -> a
foldr f acc dict =
    case dict of
        Empty () ->
            acc

        Leaf key ->
            f key acc

        Inner i ->
            foldr f (foldr f acc i.right) i.left



-- COMBINE


type Choice
    = Left
    | Right


type BranchRelation
    = SamePrefix
    | Parent Choice Choice -- which is the parent and which child the other is of the parent
    | Disjunct -- the longest common prefix and which child would be the left edge



{- Take bits from a or b, depending on the value of the bit in that position in mask.
   0 -> a, 1 -> b. Implemented as a & ~mask | b & mask
-}


combineBits : Int -> Int -> Int -> Int
combineBits a b mask =
    Bitwise.or
        (Bitwise.and a (Bitwise.complement mask))
        (Bitwise.and b mask)



{- While merging/uniting 2 inner nodes, we encounter the 4 possible base cases
   represented by BranchRelation. This function computes that relation.
-}


determineBranchRelation : InnerType -> InnerType -> BranchRelation
determineBranchRelation l r =
    let
        lp =
            l.prefix

        rp =
            r.prefix

        -- l.prefixBits and modifiedRightPrefix are guaranteed to be different
        childEdge branchPrefix c =
            if isBranchingBitSet branchPrefix c.prefix.prefixBits then
                Right

            else
                Left
    in
    if lp == rp then
        SamePrefix

    else
        let
            mask =
                -- this is the region where we want to force different bits
                highestBitSet (mostSignificantBranchingBit lp.branchingBit rp.branchingBit)

            modifiedRightPrefix =
                combineBits rp.prefixBits (Bitwise.complement lp.prefixBits) mask

            prefix =
                lcp lp.prefixBits modifiedRightPrefix
        in
        if prefix == lp then
            Parent Left (childEdge l.prefix r)

        else if prefix == rp then
            Parent Right (childEdge r.prefix l)

        else
            Disjunct



-- `Right` --> `r` is the left child.


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : IntSet -> IntSet -> IntSet
intersect l r =
    case ( l, r ) of
        ( Empty (), _ ) ->
            empty

        ( _, Empty () ) ->
            empty

        ( Leaf key, _ ) ->
            if member key r then
                l

            else
                empty

        ( _, Leaf key ) ->
            if member key l then
                leaf key

            else
                empty

        ( Inner il, Inner ir ) ->
            case determineBranchRelation il ir of
                SamePrefix ->
                    -- Intersect both left and right sub trees
                    inner il.prefix (intersect il.left ir.left) (intersect il.right ir.right)

                Parent Left Right ->
                    intersect il.right r

                Parent Right Right ->
                    intersect l ir.right

                Parent Left Left ->
                    intersect il.left r

                Parent Right Left ->
                    intersect l ir.left

                Disjunct ->
                    empty



-- We have no common keys
-- l and r contain different keys
-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.
-}
keys : IntSet -> List Int
keys dict =
    foldr (\key keyList -> key :: keyList) [] dict
