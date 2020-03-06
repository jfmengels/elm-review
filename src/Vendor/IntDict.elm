module Vendor.IntDict exposing
    ( IntDict, isValidKey
    , empty, singleton, insert, update, remove
    , isEmpty, size, member, get, before, after, findMin, findMax
    , uniteWith, union, intersect, diff, merge
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , toString
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

@docs IntDict, isValidKey


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, size, member, get, before, after, findMin, findMax


# Combine

@docs uniteWith, union, intersect, diff, merge


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# String representation

@docs toString

-}

import Bitwise
import List
import Maybe exposing (Maybe(..))


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


{-| Validates that a given integer is usable as a key.
This is necessary due to JavaScript's weird number type.
Basically this assures that we can use the functions
from `Bitwise` without risking integer overflow.

**This function is a necessity for sanitizing user input!** Alternatively,
use the safe functions from `IntDict.Safe` which perform the check for you.

As with the current version of JavaScript (2015), only 32 bit signed integers are supported.
If this ever changes, contact me! Certain parts of the implementation depend on this!

-}
isValidKey : Int -> Bool
isValidKey k =
    -- perform some dirty JS magic to turn the double
    Bitwise.or k 0 == k



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

        ( _, _ ) ->
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


{-| Create a dictionary with one key-value pair.
-}
singleton : Int -> v -> IntDict v
singleton key value =
    leaf key value


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : Int -> v -> IntDict v -> IntDict v
insert key value dict =
    update key (always (Just value)) dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : Int -> IntDict v -> IntDict v
remove key dict =
    update key (always Nothing) dict


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
        join ( k1, l ) ( k2, r ) =
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
                join ( key, alteredNode Nothing ) ( l.key, dict )

        -- This potentially inserts a new node
        Inner i ->
            if prefixMatches i.prefix key then
                if isBranchingBitSet i.prefix key then
                    inner i.prefix i.left (update key alter i.right)

                else
                    inner i.prefix (update key alter i.left) i.right

            else
                -- we have to join a new leaf with the current diverging Inner node
                join ( key, alteredNode Nothing ) ( i.prefix.prefixBits, dict )



-- QUERY


{-| Check if the dictionary contains no items.
-}
isEmpty : IntDict v -> Bool
isEmpty dict =
    case dict of
        Empty ->
            True

        _ ->
            False


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


{-| Find the key and value in the dictionary before the given key.
-}
before : Int -> IntDict v -> Maybe ( Int, v )
before key dict =
    let
        go def currentDict =
            case currentDict of
                Empty ->
                    findMax def

                Leaf l ->
                    if l.key >= key then
                        findMax def

                    else
                        Just ( l.key, l.value )

                Inner i ->
                    if not (prefixMatches i.prefix key) then
                        if i.prefix.prefixBits > key then
                            findMax def

                        else
                            -- right must always be non-empty
                            findMax i.right

                    else if isBranchingBitSet i.prefix key then
                        go i.left i.right

                    else
                        go def i.left
    in
    go Empty dict


{-| Find the key and value in the dictionary after the given key.
-}
after : Int -> IntDict v -> Maybe ( Int, v )
after key dict =
    let
        go def currentDict =
            case currentDict of
                Empty ->
                    findMin def

                Leaf l ->
                    if l.key <= key then
                        findMin def

                    else
                        Just ( l.key, l.value )

                Inner i ->
                    if not (prefixMatches i.prefix key) then
                        if i.prefix.prefixBits < key then
                            findMin def

                        else
                            -- left must always be non-empty
                            findMin i.left

                    else if isBranchingBitSet i.prefix key then
                        go def i.right

                    else
                        go i.right i.left
    in
    go Empty dict


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


{-| Find the maximum key and value in the dictionary.
-}
findMax : IntDict v -> Maybe ( Int, v )
findMax dict =
    case dict of
        Empty ->
            Nothing

        Leaf l ->
            Just ( l.key, l.value )

        Inner i ->
            findMax i.right



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


{-| Apply a function to all values in a dictionary.
-}
map : (Int -> a -> b) -> IntDict a -> IntDict b
map f dict =
    case dict of
        Empty ->
            empty

        Leaf l ->
            leaf l.key (f l.key l.value)

        Inner i ->
            inner i.prefix (map f i.left) (map f i.right)


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


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (Int -> v -> Bool) -> IntDict v -> ( IntDict v, IntDict v )
partition predicate dict =
    let
        add key value ( l, r ) =
            if predicate key value then
                ( insert key value l, r )

            else
                ( l, insert key value r )
    in
    foldl add ( empty, empty ) dict



-- COMBINE


type Choice
    = Left
    | Right


type BranchRelation
    = SamePrefix
    | Parent Choice Choice -- which is the parent and which child the other is of the parent
    | Disjunct KeyPrefix Choice -- the longest common prefix and which child would be the left edge



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

        mask =
            -- this is the region where we want to force different bits
            highestBitSet (mostSignificantBranchingBit lp.branchingBit rp.branchingBit)

        modifiedRightPrefix =
            combineBits rp.prefixBits (Bitwise.complement lp.prefixBits) mask

        prefix =
            lcp lp.prefixBits modifiedRightPrefix

        -- l.prefixBits and modifiedRightPrefix are guaranteed to be different
        childEdge branchPrefix c =
            if isBranchingBitSet branchPrefix c.prefix.prefixBits then
                Right

            else
                Left
    in
    if lp == rp then
        SamePrefix

    else if prefix == lp then
        Parent Left (childEdge l.prefix r)

    else if prefix == rp then
        Parent Right (childEdge r.prefix l)

    else
        Disjunct prefix (childEdge prefix l)


{-| `uniteWith merger l r` combines two dictionaries. If there is a collision, `merger`
is called with the conflicting key, the value from `l` and that from `r`.
-}
uniteWith : (Int -> v -> v -> v) -> IntDict v -> IntDict v -> IntDict v
uniteWith merger l r =
    let
        mergeWith key left right =
            case ( left, right ) of
                ( Just l2, Just r2 ) ->
                    Just (merger key l2 r2)

                ( Just _, _ ) ->
                    left

                ( _, Just _ ) ->
                    right

                ( Nothing, Nothing ) ->
                    -- This is a bug in the implementation, please file a bug report!
                    Nothing
    in
    case ( l, r ) of
        ( Empty, _ ) ->
            r

        ( _, Empty ) ->
            l

        ( Leaf l2, _ ) ->
            update l2.key (\r_ -> mergeWith l2.key (Just l2.value) r_) r

        ( _, Leaf r2 ) ->
            update r2.key (\l_ -> mergeWith r2.key l_ (Just r2.value)) l

        ( Inner il, Inner ir ) ->
            case determineBranchRelation il ir of
                SamePrefix ->
                    -- Merge both left and right sub trees
                    inner il.prefix (uniteWith merger il.left ir.left) (uniteWith merger il.right ir.right)

                Parent Left Right ->
                    -- Merge the right sub tree
                    inner il.prefix il.left (uniteWith merger il.right r)

                Parent Right Right ->
                    inner ir.prefix ir.left (uniteWith merger l ir.right)

                Parent Left Left ->
                    -- Merge the left sub tree
                    inner il.prefix (uniteWith merger il.left r) il.right

                Parent Right Left ->
                    inner ir.prefix (uniteWith merger l ir.left) ir.right

                Disjunct parentPrefix Left ->
                    -- Create a new inner node with l and r as sub trees
                    inner parentPrefix l r

                -- `Left` --> `l` is the left child.
                Disjunct parentPrefix Right ->
                    -- Create a new inner node with l and r as sub trees
                    inner parentPrefix r l



-- `Right` --> `r` is the left child.


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : IntDict v -> IntDict v -> IntDict v
union =
    uniteWith (\key old new -> old)


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

                Disjunct _ _ ->
                    Empty



-- We have no common keys


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : IntDict a -> IntDict b -> IntDict a
diff l r =
    case ( l, r ) of
        ( Empty, _ ) ->
            Empty

        ( _, Empty ) ->
            l

        ( Leaf ll, _ ) ->
            if member ll.key r then
                Empty

            else
                l

        ( _, Leaf lr ) ->
            remove lr.key l

        ( Inner il, Inner ir ) ->
            case determineBranchRelation il ir of
                SamePrefix ->
                    -- Diff both left and right sub trees
                    inner il.prefix (diff il.left ir.left) (diff il.right ir.right)

                Parent Left Left ->
                    inner il.prefix (diff il.left r) il.right

                Parent Left Right ->
                    inner il.prefix il.left (diff il.right r)

                Parent Right Left ->
                    diff l ir.left

                Parent Right Right ->
                    diff l ir.right

                Disjunct _ _ ->
                    l



-- l and r contain different keys


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

Note that `uniteWith`, `union`, `intersect` and `diff` could all be implemented
in terms of this function. The only reason that's not the case is to have more
sharing of substructure.

    uniteWith merger l r =
        merge insert merger insert l r empty

    union l r =
        merge insert (\k a _ d -> insert k a d) insert l r empty

    intersect l r =
        merge (\_ _ d -> d) (\k a _ d -> insert k a d) (\_ _ d -> d) l r empty

    diff l r =
        merge insert (\_ _ _ d -> d) (\_ _ d -> d) l r empty

-}
merge :
    (Int -> a -> result -> result)
    -> (Int -> a -> b -> result -> result)
    -> (Int -> b -> result -> result)
    -> IntDict a
    -> IntDict b
    -> result
    -> result
merge left both right l r acc =
    let
        m =
            merge left both right
    in
    case ( l, r ) of
        ( Empty, _ ) ->
            foldl right acc r

        ( _, Empty ) ->
            foldl left acc l

        ( Leaf l2, _ ) ->
            case get l2.key r of
                Nothing ->
                    m Empty r (left l2.key l2.value acc)

                Just v ->
                    m Empty (remove l2.key r) (both l2.key l2.value v acc)

        ( _, Leaf r2 ) ->
            case get r2.key l of
                Nothing ->
                    m l Empty (right r2.key r2.value acc)

                Just v ->
                    m (remove r2.key l) Empty (both r2.key v r2.value acc)

        ( Inner il, Inner ir ) ->
            case determineBranchRelation il ir of
                SamePrefix ->
                    acc |> m il.left ir.left |> m il.right ir.right

                Parent Left Left ->
                    acc |> m il.left r |> m il.right Empty

                Parent Left Right ->
                    acc |> m il.left Empty |> m il.right r

                Parent Right Left ->
                    acc |> m l ir.left |> m Empty ir.right

                Parent Right Right ->
                    acc |> m Empty ir.left |> m l ir.right

                Disjunct _ Left ->
                    -- left and right dict are disjunct; left child would be the left edge in the merged dict
                    acc |> m l Empty |> m Empty r

                Disjunct _ Right ->
                    -- left and right dict are disjunct; left child would be the left edge in the merged dict
                    acc |> m Empty r |> m l Empty



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.
-}
keys : IntDict v -> List Int
keys dict =
    foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.
-}
values : IntDict v -> List v
values dict =
    foldr (\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : IntDict v -> List ( Int, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
-}
fromList : List ( Int, v ) -> IntDict v
fromList pairs =
    List.foldl (\( a, b ) -> insert a b) empty pairs



-- STRING REPRESENTATION


{-| Generates a string representation similar to what `toString`
generates for `Dict`. You must provide a function to convert
your value type into a string.
-}
toString : IntDict v -> (v -> String) -> String
toString dict valueToStr =
    let
        pairToStr ( k, v ) =
            "(" ++ String.fromInt k ++ ", \"" ++ valueToStr v ++ "\")"
    in
    "IntDict.fromList " ++ String.join ", " (List.map pairToStr (toList dict))
