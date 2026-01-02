module Vendor.IntDict exposing
    ( IntDict
    , empty, insert, update
    , member, get, findMin
    , intersect
    , keys
    , foldl, filter
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

_O(min(n, W))_: `insert`, `update`, `remove`, `get`, `member`

_O(n + m)_: `uniteWith`, `union`, `intersection`, `diff`, `merge`

where _n_ and _m_ are the sizes of the first and second dictionary respectively and _W_
is the number of bits in `Int` (so a constant with current value 32).

Dictionary equality with `(==)` is unreliable and should not be used.


# Data

@docs IntDict


# Build

@docs empty, insert, update


# Query

@docs member, get, findMin


# Combine

@docs intersect


# Lists

@docs keys


# Transform

@docs foldl, filter


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
    , size : Int
    }


{-| A dictionary mapping `Int`s to values of a type `v`. Analogous to
`Dict Int v`.
-}
type IntDict v
    = Empty -- Invariant: Never child of an `Inner` node
    | Leaf { key : Int, value : v }
    | Inner (InnerType v)



-- into an integer. We can then check for overflow.
-- This seems easier than checking for 32 bits.
-- `or` 0 is similar to `mod` <32bits>
-- SMART CONSTRUCTORS
-- not exported


inner : KeyPrefix -> IntDict v -> IntDict v -> IntDict v
inner p l r =
    case ( l, r ) of
        ( Empty, _ ) ->
            r

        ( _, Empty ) ->
            l

        _ ->
            Inner
                { prefix = p
                , left = l
                , right = r
                , size = size l + size r
                }



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
empty : IntDict v
empty =
    Empty


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : Int -> v -> IntDict v -> IntDict v
insert key value dict =
    update key (always (Just value)) dict


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : Int -> (Maybe v -> Maybe v) -> IntDict v -> IntDict v
update key alter dict =
    let
        alteredNode mv =
            case alter mv of
                -- handle this centrally
                Just v ->
                    leaf key v

                Nothing ->
                    empty

        -- The inner constructor will do the rest
        join k1 l k2 r =
            -- precondition: k1 /= k2
            let
                prefix =
                    lcp k1 k2
            in
            if
                isBranchingBitSet prefix k2
                -- if so, r will be the right child
            then
                inner prefix l r

            else
                inner prefix r l
    in
    case dict of
        Empty ->
            alteredNode Nothing

        Leaf l ->
            if l.key == key then
                alteredNode (Just l.value)
                -- This updates or removes the leaf with the same key

            else
                join key (alteredNode Nothing) l.key dict

        -- This potentially inserts a new node
        Inner i ->
            if prefixMatches i.prefix key then
                if isBranchingBitSet i.prefix key then
                    inner i.prefix i.left (update key alter i.right)

                else
                    inner i.prefix (update key alter i.left) i.right

            else
                -- we have to join a new leaf with the current diverging Inner node
                join key (alteredNode Nothing) i.prefix.prefixBits dict



-- QUERY


{-| The number of items in the dictionary. `O(1)`.
-}
size : IntDict v -> Int
size dict =
    case dict of
        Empty ->
            0

        Leaf _ ->
            1

        Inner i ->
            i.size


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
        Empty ->
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


{-| Find the minimum key and value in the dictionary.
-}
findMin : IntDict v -> Maybe ( Int, v )
findMin dict =
    case dict of
        Empty ->
            Nothing

        Leaf l ->
            Just ( l.key, l.value )

        Inner i ->
            findMin i.left



-- TRANSFORM


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (Int -> v -> Bool) -> IntDict v -> IntDict v
filter predicate dict =
    let
        add k v d =
            if predicate k v then
                insert k v d

            else
                d
    in
    foldl add empty dict


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (Int -> v -> a -> a) -> a -> IntDict v -> a
foldl f acc dict =
    case dict of
        Empty ->
            acc

        Leaf l ->
            f l.key l.value acc

        Inner i ->
            foldl f (foldl f acc i.left) i.right


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (Int -> v -> a -> a) -> a -> IntDict v -> a
foldr f acc dict =
    case dict of
        Empty ->
            acc

        Leaf l ->
            f l.key l.value acc

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


determineBranchRelation : InnerType l -> InnerType r -> BranchRelation
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
intersect : IntDict a -> IntDict b -> IntDict a
intersect l r =
    case ( l, r ) of
        ( Empty, _ ) ->
            Empty

        ( _, Empty ) ->
            Empty

        ( Leaf ll, _ ) ->
            if member ll.key r then
                l

            else
                Empty

        ( _, Leaf lr ) ->
            case get lr.key l of
                Just v ->
                    leaf lr.key v

                Nothing ->
                    Empty

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
                    Empty



-- We have no common keys
-- l and r contain different keys
-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.
-}
keys : IntDict v -> List Int
keys dict =
    foldr (\key _ keyList -> key :: keyList) [] dict



-- STRING REPRESENTATION
