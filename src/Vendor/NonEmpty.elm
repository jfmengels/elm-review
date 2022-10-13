module Vendor.NonEmpty exposing
    ( NonEmpty
    , singleton, cons, fromList, fromCons, unfoldl, unfoldr
    , map, indexedMap, foldl, foldl1, foldr, foldr1, filter, filterMap, partition
    , length, reverse, member, all, any, maximum, minimum, sum, product, last, find, unique
    , append, concat, concatMap, intersperse, map2, andMap
    , sort, sortBy, sortWith
    , isSingleton, head, tail, take, dropHead, drop, uncons, toList
    , duplicate, extend
    , decodeList, decode, encodeList
    )

{-|

@docs NonEmpty


# Create

@docs singleton, cons, fromList, fromCons, unfoldl, unfoldr


# Transform

@docs map, indexedMap, foldl, foldl1, foldr, foldr1, filter, filterMap, partition


# Utilities

@docs length, reverse, member, all, any, maximum, minimum, sum, product, last, find, unique


# Combine

@docs append, concat, concatMap, intersperse, map2, andMap


# Sort

@docs sort, sortBy, sortWith


# Deconstruct

@docs isSingleton, head, tail, take, dropHead, drop, uncons, toList


# Expand

@docs duplicate, extend


# JSON

@docs decodeList, decode, encodeList

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


{-| `NonEmpty` list is an alias for a pair of `a` and `List a`.

This makes it possible to construct value of non empty List
without relying on any specific implementation of this type.

-}
type alias NonEmpty a =
    ( a, List a )


{-| Creates `NonEmpty` list with only one element.

    singleton 1
    --> ( 1, [] )

-}
singleton : a -> NonEmpty a
singleton h =
    ( h, [] )


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


{-| Cons element onto `List` to create `NonEmpty`.

    fromCons 0 [ 1, 2 ]
    --> (0, [1, 2])

This function is just an alias for `Tuple.pair`

-}
fromCons : a -> List a -> NonEmpty a
fromCons =
    Tuple.pair


{-| Create `NonEmpty` by unfolding other data from left.

This is more expert way of constructing `NonEmpty`.
It's useful in rare cases.

    stepPrev : Int -> (String, Maybe Int)
    stepPrev n =
        ( String.fromInt n
        , if n > 0 then
            Just (n - 1)
          else
             Nothing
         )

    unfoldl stepPrev 5
    --> ("0", ["1","2","3","4", "5"])

-}
unfoldl : (a -> ( b, Maybe a )) -> a -> NonEmpty b
unfoldl f a =
    unfoldrHelp f [] a


{-| Create `NonEmpty` by unfolding other data from right.

This is more expert way of constructing `NonEmpty`.
It's useful in rare cases.

    stepNext : Int -> (String, Maybe Int)
    stepNext n =
        ( String.fromInt n
        , if n < 5 then
            Just (n + 1)
          else
             Nothing
         )

    unfoldr stepNext 0
    --> ("0", ["1","2","3","4", "5"])

-}
unfoldr : (a -> ( b, Maybe a )) -> a -> NonEmpty b
unfoldr f a =
    reverse <| unfoldrHelp f [] a


unfoldrHelp : (a -> ( b, Maybe a )) -> List b -> a -> NonEmpty b
unfoldrHelp f acc a =
    case f a of
        ( h, Just next_ ) ->
            unfoldrHelp f (h :: acc) next_

        ( h, Nothing ) ->
            ( h, acc )


{-| Converts `NonEmpty` to `List`.

    toList ( 1, [ 2 ] )
    --> [1, 2]

-}
toList : NonEmpty a -> List a
toList ( h, t ) =
    h :: t


{-| Add element to the beginning of `NonEmpty` list.

    cons 2 ( 1, [] )
    --> (2, [ 1 ])

-}
cons : a -> NonEmpty a -> NonEmpty a
cons a ( h, t ) =
    ( a, h :: t )


{-| Remove first element form `NonEmpty` list.

    uncons ( 3, [ 2, 1 ] )
    --> ( 3, Just ( 2, [ 1 ] ) )

    uncons ( "hello!", [] )
    --> ( "hello!", Nothing )

-}
uncons : NonEmpty a -> ( a, Maybe (NonEmpty a) )
uncons ( h, t ) =
    ( h, fromList t )


{-| Returns first element of the `NonEmpty`.

    head ( 1, [ 2 ] )
    --> 1

-}
head : NonEmpty a -> a
head ( h, _ ) =
    h


{-| Returns tail of the `NonEmpty`.
The return type is List and may be empty.

    tail ( 1, [ 2, 3 ] )
    --> [2, 3]

If you're looking for function that produces
another `NonEmpty` see [`dropHead`](#dropHead)

-}
tail : NonEmpty a -> List a
tail ( _, t ) =
    t


{-| Returns last element of the `NonEmpty`.

    last ( 1, [ 2 ] )
    --> 2

    last ( 1, [] )
    --> 1

This function is _O(n)_

-}
last : NonEmpty a -> a
last ( h, t ) =
    Maybe.withDefault h <| lastHelper t


lastHelper : List a -> Maybe a
lastHelper xs =
    case xs of
        [] ->
            Nothing

        h :: [] ->
            Just h

        _ :: t ->
            lastHelper t


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) (2, [ 4, 6, 8 ])
    --> Just 6

-}
find : (a -> Bool) -> NonEmpty a -> Maybe a
find predicate ( x, xs ) =
    if predicate x then
        Just x

    else
        findHelper predicate xs


findHelper : (a -> Bool) -> List a -> Maybe a
findHelper predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                findHelper predicate rest


{-| Take the first n elements of `NonEmpty`.

    take 2 ( 1, [ 2, 3, 4 ] )
    --> ( 1, [ 2 ] )

    take 0 ( 1, [ 2, 3, 4 ] )
    --> ( 1, [] )

-}
take : Int -> NonEmpty a -> NonEmpty a
take n ( h, t ) =
    ( h, List.take (n - 1) t )


{-| Removes first element of the `NonEmpty` list.

    dropHead ( 1, [ 2 ] )
    --> Just (2, [])

    dropHead ( 1, [] )
    --> Nothing

-}
dropHead : NonEmpty a -> Maybe (NonEmpty a)
dropHead ( _, t ) =
    fromList t


{-| Drop the first n elements of `NonEmpty` list.

    drop 2 ( 1, [ 2, 3, 4 ] )
    --> Just ( 3, [4] )

-}
drop : Int -> NonEmpty a -> Maybe (NonEmpty a)
drop n ne =
    if n > 0 then
        case dropHead ne of
            Just new ->
                drop (n - 1) new

            Nothing ->
                Nothing

    else
        Just ne



--


{-| Map a function over `NonEmpty` list.

    map (\x -> x + 1) ( 1, [ 2, 3 ] )
    --> ( 2, [3, 4] )

    map String.fromInt ( 1, [ 2 ] )
    --> ( "1", [ "2" ] )

-}
map : (a -> b) -> NonEmpty a -> NonEmpty b
map f ( h, t ) =
    ( f h, List.map f t )


{-| Same as `map` but an index is passed with each element.

Index starts at 0.

    indexedMap (\i x -> String.fromInt i ++ " is " ++ x) ("a", ["b", "c"])
    --> ("0 is a",["1 is b","2 is c"])

-}
indexedMap : (Int -> a -> b) -> NonEmpty a -> NonEmpty b
indexedMap f ( h, t ) =
    ( f 0 h, List.indexedMap (\i -> f (i + 1)) t )


{-| Reduce `NonEmpty` from left.

    foldl (+) 0 (1, [2,3,4])
    --> 10

    foldl cons (0, []) (1, [2,3,4])
    --> (4, [3,2,1,0])

-}
foldl : (a -> b -> b) -> b -> NonEmpty a -> b
foldl f acc =
    List.foldl f acc << toList


{-| Collapse `NonEmpty a` into `a` value from left

    foldl1 (+) (1, [2,3,4])
    --> 10

    foldl1 (++) ("hello", [" ","world"])
    --> "world hello"

-}
foldl1 : (a -> a -> a) -> NonEmpty a -> a
foldl1 f ( h, t ) =
    List.foldl f h t


{-| Reduce `NonEmpty` from right.

    foldr (+) 0 (1, [2,3,4])
    --> 10

    foldr cons (5, []) (1, [2,3,4])
    --> (1, [2, 3, 4, 5])

-}
foldr : (a -> b -> b) -> b -> NonEmpty a -> b
foldr f acc =
    List.foldr f acc << toList


{-| Collapse `NonEmpty a` into `a` value from right.

    foldr1 (+) (1, [2,3,4])
    --> 10

    foldr1 (++) ("hello", [" ","world"])
    --> "hello world"

-}
foldr1 : (a -> a -> a) -> NonEmpty a -> a
foldr1 f =
    foldl1 f << reverse


{-| Keep elements that satisfy the test

    isEven : Int -> Bool
    isEven n = (n |> modBy 2) == 0

    filter isEven (1,[2,3,4,5])
    --> Just (2, [4] )

-}
filter : (a -> Bool) -> NonEmpty a -> Maybe (NonEmpty a)
filter f =
    fromList << List.filter f << toList


{-| Apply function to each element of `NonEmpty` and leave out values that result in `Nothing`.

    filterMap String.toInt ("1", ["baz", "3rd", "4"])
    --> Just (1, [4])

    filterMap String.toInt ("foo", ["baz", "3rd"])
    --> Nothing

-}
filterMap : (a -> Maybe b) -> NonEmpty a -> Maybe (NonEmpty b)
filterMap f =
    fromList << List.filterMap f << toList



--


{-| Calculate length of `NonEmpty` list.

    length ( 1, [ 2, 3 ] )
    --> 3

    length ( 1, [] )
    --> 1

-}
length : NonEmpty a -> Int
length ( _, t ) =
    List.length t + 1


{-| Reverse `NonEmpty` list.

    reverse (1, [2, 3, 4])
    --> (4, [3, 2, 1])

-}
reverse : NonEmpty a -> NonEmpty a
reverse ( h, t ) =
    case List.reverse <| h :: t of
        [] ->
            ( h, [] )

        nH :: nT ->
            ( nH, nT )


{-| Figure out whether a `NonEmpty` list contains a value.

    member 2 ( 1, [ 2 ] )
    --> True

    member 3 ( 1, [ 2 ] )
    --> False

-}
member : a -> NonEmpty a -> Bool
member a ( h, t ) =
    a == h || List.member a t


{-| Determine if all elements satisfy the test.

    all Char.isUpper ( 'A', [ 'B' ] )
    --> True

    all Char.isUpper ( 'a', [ 'B' ] )
    --> False

-}
all : (a -> Bool) -> NonEmpty a -> Bool
all f ( h, t ) =
    f h && List.all f t


{-| Determine if any elements satisfies the test.

    any Char.isUpper ( 'a', [ 'B' ] )
    --> True

    any Char.isUpper ( 'a', [ 'b' ] )
    --> False

-}
any : (a -> Bool) -> NonEmpty a -> Bool
any f ( h, t ) =
    f h || List.any f t


{-| Find the maximum element.

    maximum ( 3, [ 3, 5, 2 ] )
    --> 5

-}
maximum : NonEmpty comparable -> comparable
maximum (( h, _ ) as ne) =
    case List.maximum <| toList ne of
        Just x ->
            x

        Nothing ->
            h


{-| Find the minimum element.

    minimum ( 3, [ 3, 5, 2 ] )
    --> 2

-}
minimum : NonEmpty comparable -> comparable
minimum (( h, _ ) as ne) =
    case List.minimum <| toList ne of
        Just x ->
            x

        Nothing ->
            h


{-| Get the sum of the list elements.

    sum ( 2, [ 2, 2 ] )
    --> 6

-}
sum : NonEmpty number -> number
sum =
    List.sum << toList


{-| Get the product of the list elements.

    product ( 2, [ 2, 2 ] )
    --> 8

-}
product : NonEmpty number -> number
product =
    List.product << toList


{-| Put two lists together.

    append ( 1, [ 2, 3 ] ) ( 4, [ 5 ] )
    --> ( 1, [ 2, 3, 4, 5 ] )

-}
append : NonEmpty a -> NonEmpty a -> NonEmpty a
append ne1 ne2 =
    case toList ne2 of
        [] ->
            ne1

        _ ->
            foldr cons ne2 ne1


{-| Concatenate a bunch of lists into a single list.

    concat ((1, [2, 3]), [(4, [5, 6]), (7, [8]), (9, []), (10, [11])])
    --> (1,[2,3,4,5,6,7,8,9,10,11])

-}
concat : NonEmpty (NonEmpty a) -> NonEmpty a
concat ( h, t ) =
    let
        hx =
            head h

        tx =
            tail h ++ List.concat (List.map toList t)
    in
    ( hx, tx )


{-| Map a given function onto a list and flatten the resulting lists.

    concatMap singleton ( 1, [ 2 ] )
    -->  ( 1, [ 2 ] )

    concatMap (\x -> ( x + 1, [ x + 1 ] )) ( 1, [ 2 ] )
    --> ( 2, [ 2, 3, 3 ] )

-}
concatMap : (a -> NonEmpty b) -> NonEmpty a -> NonEmpty b
concatMap f =
    concat << map f


{-| Create `NonEmpty` containing sub `NoneEmpty` lists.

This is a more advanced function following [`Comonad`](https://hackage.haskell.org/package/comonad)

    duplicate ( 1, [ 2, 3 ] )
    --> ( ( 1, [ 2, 3 ] ), [ ( 2, [ 3 ] ), ( 3, [] ) ] )

    duplicate ( "alone", [] )
    --> ( ( "alone", [] ), [] )

-}
duplicate : NonEmpty a -> NonEmpty (NonEmpty a)
duplicate ne =
    ( ne
    , case fromList <| tail ne of
        Nothing ->
            []

        Just sec ->
            List.reverse <| duplicateHelper [] sec
    )


duplicateHelper : List (NonEmpty a) -> NonEmpty a -> List (NonEmpty a)
duplicateHelper acc (( _, t ) as ne) =
    case fromList t of
        Nothing ->
            ne :: acc

        Just newNE ->
            duplicateHelper (ne :: acc) newNE


{-| Map value to a new value based on tail of a list.

This is a more advanced function following [`Comonad`](https://hackage.haskell.org/package/comonad)

    -- for each element sum all elements till the end
    extend sum ( 1, [ 2, 3 ] )
    --> ( 6, [ 5, 3 ] )

    -- calculate length at each point of NonEmpty list
    extend length ("foo", [ "bar", "baz", "EOF"] )
    --> ( 4, [ 3, 2, 1 ])

-}
extend : (NonEmpty a -> b) -> NonEmpty a -> NonEmpty b
extend f =
    map f << duplicate


{-| Places the given value between all members of given list.

    intersperse "and" ( "1", [ "2", "3" ] )
    --> ("1", ["and", "2", "and", "3"])

    intersperse "and" ( "1", [ "2" ] )
    --> ("1", ["and", "2"])

    intersperse "and" ( "1", [] )
    --> ("1", [])

-}
intersperse : a -> NonEmpty a -> NonEmpty a
intersperse x ne =
    case ne of
        ( _, [] ) ->
            ne

        ( h, t ) ->
            ( h, x :: List.intersperse x t )


{-| Combine two lists with a given function.
In case where one of the two lists is longer the extra elements are ignored.

    map2 (+) ( 1, [ 2 ] ) ( 1, [ 1 ] )
    --> (2, [3])

    map2 (+) ( 1, [] ) ( 1, [ 1 ] )
    --> (2, [])

    map2 (+) ( 1, [ 1 ] ) ( 1, [] )
    --> (2, [])

    map2 Tuple.pair ( 1, [ 2, 3 ]) ("foo", [ "bar" ])
    --> ( ( 1, "foo"), [ ( 2, "bar" ) ] )

-}
map2 : (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
map2 f ( h1, t1 ) ( h2, t2 ) =
    ( f h1 h2, List.map2 f t1 t2 )


{-| Map over multiple `NonEmpty` lists.

    map (+) (1, [2])
    |> andMap (1, [1])
    --> (2, [3])

    type alias User =
        { name : String
        , age : Int
        , admin : Bool
        }

    ( User, [ User, User ] )
    |> andMap ( "Alice", [ "Bob", "Charlie"] )
    |> andMap ( 30, [ 50, 19 ] )
    |> andMap ( True, [ False, False ])
    --> ( User "Alice" 30 True
    --> , [ User "Bob" 50 False, User "Charlie" 19 False ]
    --> )

-}
andMap : NonEmpty a -> NonEmpty (a -> b) -> NonEmpty b
andMap =
    map2 (|>)


sortHelper : (List a -> List a) -> ( a, List a ) -> NonEmpty a
sortHelper f ne =
    case f <| toList ne of
        h :: t ->
            ( h, t )

        [] ->
            -- impossible state
            ne


{-| Sort values from lowest to highest.

    sort ( 3, [ 4, 1, 2 ] )
    --> (1, [2, 3, 4])

-}
sort : NonEmpty comparable -> NonEmpty comparable
sort =
    sortHelper List.sort


{-| Sort values by a derived property.

    sortBy String.length ( "333", [ "4444", "1", "22" ] )
    --> ("1", ["22", "333", "4444"])

-}
sortBy : (a -> comparable) -> NonEmpty a -> NonEmpty a
sortBy f =
    sortHelper (List.sortBy f)


{-| Sort values with a custom comparison function.
-}
sortWith : (a -> a -> Order) -> NonEmpty a -> NonEmpty a
sortWith f =
    sortHelper (List.sortWith f)


{-| Is the nonempty list exactly one element?

    isSingleton ( 1, [] )
    --> True

    isSingleton ( 1, [ 2 ] )
    --> False

-}
isSingleton : NonEmpty a -> Bool
isSingleton ( _, t ) =
    List.isEmpty t



-- JSON functions


decodeListHelper : List a -> Decoder (NonEmpty a)
decodeListHelper xs =
    case fromList xs of
        Just res ->
            Decode.succeed res

        Nothing ->
            Decode.fail "Expecting at least ONE ELEMENT array"


{-| Decode JSON array to `NonEmpty`.

    import Json.Decode as JD exposing (Decoder)
    import Json.Encode as JE

    strings : Decoder (NonEmpty String)
    strings =
        decodeList JD.string

    JD.decodeString strings "[\"foo\",\"bar\",\"baz\"]"
    --> Ok ( "foo", [ "bar", "baz" ])

    JD.decodeString strings "[]"
    --> Err <| JD.Failure "Expecting at least ONE ELEMENT array" <|
    -->     JE.list identity []

    JD.decodeString strings "{}"
    --> Err <| JD.Failure "Expecting a LIST" <|
    -->      JE.object []

-}
decodeList : Decoder a -> Decoder (NonEmpty a)
decodeList decoder =
    Decode.list decoder
        |> Decode.andThen decodeListHelper


{-| Helper for creating custom `Decoder`.

    import Json.Decode as JD exposing (Decoder)
    import Json.Encode as JE
    import Json.Decode.Extra as JDE
    import Json.Decode.Pipeline as JDP

    -- Decoding from custom object

    objectDecoder : Decoder (NonEmpty Int)
    objectDecoder =
        decode
         |> JDP.required "head" JD.int
         |> JDP.required "tail" (JD.list JD.int)


    JD.decodeString objectDecoder "{\"head\":1,\"tail\":[2,3]}"
    --> Ok (1, [ 2, 3 ])

    JD.decodeString objectDecoder "{\"head\":true}"
    --> Err <| JD.Failure "Expecting an OBJECT with a field named `tail`" <|
    -->     JE.object [ ("head", JE.bool True) ]

    -- Decoding from Array of Arrays

    nestedArrayDecoder : Decoder (NonEmpty Bool)
    nestedArrayDecoder =
        decode
        |> JDE.andMap (JD.index 0 JD.bool)
        |> JDE.andMap (JD.index 1 <| JD.list JD.bool)

    JD.decodeString nestedArrayDecoder "[true, [false, true]]"
    --> Ok (True, [False, True])

    JD.decodeString nestedArrayDecoder "[false]"
    --> Err <| JD.Failure "Expecting a LONGER array. Need index 1 but only see 1 entries" <|
    -->     JE.list JE.bool [False]

-}
decode : Decoder (a -> List a -> NonEmpty a)
decode =
    Decode.succeed fromCons


{-| Encode `NonEmpty` as JSON array.
-}
encodeList : (a -> Value) -> NonEmpty a -> Value
encodeList f =
    Encode.list f << toList


{-| Partition a non empty list on some test. The first list contains all values
that satisfy the test, and the second list contains all the values that do not.

    isEven : Int -> Bool
    isEven n = (n |> modBy 2) == 0

    partition isEven (0,[1,2,3,4,5])
    --> ([0,2,4], [1,3,5])

-}
partition : (a -> Bool) -> NonEmpty a -> ( List a, List a )
partition f =
    List.partition f << toList


{-| Remove duplicates of a `NonEmpty` list.

    unique (0,[1,0,1,1,0])
    --> (0,[1])

-}
unique : NonEmpty a -> NonEmpty a
unique xs =
    xs
        |> toList
        |> uniqueHelper
        |> fromList
        |> Maybe.withDefault xs


uniqueHelper : List a -> List a
uniqueHelper list =
    let
        uniqueHelp : (a -> b) -> List b -> List a -> List a -> List a
        uniqueHelp f existing remaining accumulator =
            case remaining of
                [] ->
                    List.reverse accumulator

                first :: rest ->
                    let
                        computedFirst =
                            f first
                    in
                    if List.member computedFirst existing then
                        uniqueHelp f existing rest accumulator

                    else
                        uniqueHelp f (computedFirst :: existing) rest (first :: accumulator)
    in
    uniqueHelp identity [] list []
