module Simplify exposing
    ( rule
    , Configuration, defaults, expectNaN, ignoreCaseOfForTypes
    )

{-| Reports when an expression can be simplified.

🔧 Running with `--fix` will automatically remove all the reported errors.

    config =
        [ Simplify.rule Simplify.defaults
        ]

@docs rule
@docs Configuration, defaults, expectNaN, ignoreCaseOfForTypes


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplify/example --rules Simplify
```


## Simplifications

Below is the list of all kinds of simplifications this rule applies.


### Booleans

    x || True
    --> True

    x || False
    --> x

    x && True
    --> x

    x && False
    --> False

    not True
    --> False

    not (not x)
    --> x

    -- for `<`, `>`, `<=`, `>=`, `==` and `/=`
    not (a < b)
    --> a >= b


### Comparisons

    x == True
    --> x

    x /= False
    --> x

    not x == not y
    --> x == y

    anything == anything
    --> True

    anything /= anything
    --> False

    { r | a = 1 } == { r | a = 2 }
    --> False

    x == []
    --> List.isEmpty x

    x /= []
    --> not (List.isEmpty x)

    x == Array.empty -- same for Dict or Set
    --> Array.isEmpty x

    1 < 2 -- same for >=, <=, < for any comparable operands
    --> True

    -- when `expectNaN` is not enabled
    -- whenever number intervals for both sides are known to always pass or fail a comparison
    List.length l >= min -1 n
    --> True

    n < n
    --> False

    -- when `expectNaN` is not enabled
    n > n
    --> False

    -- when `expectNaN` is not enabled
    n <= n
    --> True

    n >= n
    --> True


### If expressions

    if True then x else y
    --> x

    if False then x else y
    --> y

    if condition then x else x
    --> x

    if condition then True else False
    --> condition

    if condition then False else True
    --> not condition

    if condition then
        if not condition then
            1
        else
            2
    else
        3
    --> if condition then 2 else 3


### Case expressions

    case condition of
        True -> x
        False -> y
    --> if condition then x else y

    case condition of
        False -> y
        True -> x
    --> if not condition then x else y

    -- only when no variables are introduced in the pattern
    -- and no custom types defined in the project are referenced
    case value of
        Just _ -> x
        Nothing -> x
    --> x

    -- same with any variant, list or tuple containing either
    case Just value of
        Nothing -> a
        Just (Ok b) -> c
        Just (Err d) -> e
    --> case value of
    -->     Ok b -> c
    -->     Err d -> e

Destructuring using case expressions

    case value of
        ( x, y ) ->
            x + y

    -->
    let
        ( x, y ) =
            value
    in
    x + y


### Let expressions

    let
        a =
            1
    in
    let
        b =
            1
    in
    a + b

    -->
    let
        a =
            1

        b =
            1
    in
    a + b


### Record updates

    { a | b = a.b }
    --> a

    { a | b = a.b, c = 1 }
    --> { a | c = 1 }


### Field access

    { a = b }.a
    --> b

    { a | b = c }.b
    --> c

    { a | b = c }.d
    --> a.d

    (let a = b in c).d
    --> let a = b in c.d

    (Record first second).first
    --> first


### Basics functions

    identity x
    --> x

    f >> identity
    --> f

    always x y
    --> x

    f >> always x
    --> always x

    toFloat 1
    --> 1

    round 1.1
    --> 1

    ceiling 0.9
    --> 1

    floor 1.1
    --> 1

    truncate 1.1
    --> 1

    round (toFloat n) -- same for ceiling, floor and truncate
    --> n

    abs 3
    --> 3

    abs (abs n)
    --> abs n

    abs -n
    --> abs n

    min n n
    --> n

    min n -n
    --> -(abs n)

    min (min n0 n1) n0
    --> min n0 n1

    min 3 4
    --> 3

    max n n
    --> n

    max n -n
    --> abs n

    max (max n0 n1) n0
    --> max n0 n1

    max 3 4
    --> 4

    -- when `expectNaN` is not enabled
    compare n n
    --> EQ

    compare 3 4
    --> LT


### Lambdas

    (\_ -> x) data
    --> x

    (\x -> x) data
    --> data

    (\() y -> x) ()
    --> (\y -> x)

    (\_ y -> x) data
    --> (\y -> x)

    (\y _ -> x) data otherData
    --> (\y -> x)


### Operators

    (++) a b
    --> a ++ b

    a |> f >> g
    --> a |> f |> g

    (f >> g) a
    --> g <| f a

    (f >> g) <| a
    --> g <| f <| a

    a |> (g << f)
    --> a |> g |> f


### Numbers

    n + 0
    --> n

    n - 0
    --> n

    0 - n
    --> -n

    n * 1
    --> n

    0 // n
    --> 0

    n // 0
    --> 0

    n // 1
    --> n

    n / 1
    --> n

    0 / n
    --> 0

    -(-n)
    --> n

    negate (negate n)
    --> n

    n - n
    --> 0


### Tuples

    Tuple.pair a b
    --> ( a, b )

    Tuple.first ( a, b )
    --> a

    Tuple.first (Tuple.mapSecond changeFirst tuple)
    --> Tuple.first tuple

    Tuple.first (Tuple.mapFirst changeFirst tuple)
    --> changeFirst (Tuple.first tuple)

    Tuple.first (Tuple.mapBoth changeFirst changeSecond tuple)
    --> changeFirst (Tuple.first tuple)

    Tuple.second ( a, b )
    --> b

    Tuple.second (Tuple.mapFirst changeFirst tuple)
    --> Tuple.second tuple

    Tuple.second (Tuple.mapSecond changeSecond tuple)
    --> changeSecond (Tuple.second tuple)

    Tuple.second (Tuple.mapBoth changeFirst changeSecond tuple)
    --> changeSecond (Tuple.second tuple)


### Strings

    "a" ++ ""
    --> "a"

    -- only when the end quotes of the left side and
    -- the start quotes of the right side are on the same line
    "a" ++ "b"
    --> "ab"

    String.fromList []
    --> ""

    String.fromList [ a ]
    --> String.fromChar a

    String.fromList (String.toList str)
    --> str

    String.toList (String.fromList list)
    --> list

    String.isEmpty ""
    --> True

    String.isEmpty "a"
    --> False

    -- same for String.reverse
    String.isEmpty (String.map f str)
    --> String.isEmpty str

    String.isEmpty (String.fromList list)
    --> List.isEmpty list

    String.uncons ""
    --> Nothing

    String.concat []
    --> ""

    String.concat [ string ]
    --> string

    String.concat [ hello, "", world ]
    --> String.concat [ hello, world ]

    String.concat [ "a", String.concat [ b, c ], d ]
    --> String.concat [ "a", b, c, d ]

    String.concat (List.repeat n str)
    --> String.repeat n str

    String.concat (List.intersperse str strings)
    --> String.join str strings

    String.append "" str
    --> str

    String.append (String.fromList [ a, b ]) (String.fromList [ c, d ])
    --> String.fromList [ a, b, c, d ]

    String.join str []
    --> ""

    String.join "" list
    --> String.concat list

    String.length "abc"
    --> 3

    String.length str == 0
    --> String.isEmpty str

    String.length (String.reverse str)
    --> String.length str

    String.repeat n ""
    --> ""

    String.repeat 0 str
    --> ""

    String.repeat 1 str
    --> str

    String.replace x y ""
    --> ""

    String.replace x x z
    --> z

    String.replace "x" "y" "z"
    --> "z" -- only when resulting string is unchanged

    String.words ""
    --> [ "" ]

    String.lines ""
    --> [ "" ]

    String.toLower ""
    --> ""

    String.toLower (String.toLower str)
    --> String.toLower str

    String.toUpper ""
    --> ""

    String.toUpper (String.toUpper str)
    --> String.toUpper str

    String.reverse ""
    --> ""

    String.reverse (String.fromChar a)
    --> String.fromChar a

    String.reverse (String.reverse str)
    --> str

    String.trimLeft ""
    --> ""

    String.trimLeft (String.trimLeft str)
    --> String.trimLeft str

    String.trimLeft (String.trimRight str)
    --> String.trim str

    String.trimLeft (String.trim str)
    --> String.trim str

    String.trimRight ""
    --> ""

    String.trimRight (String.trimLeft str)
    --> String.trim str

    String.trimRight (String.trimRight str)
    --> String.trimRight str

    String.trimRight (String.trim str)
    --> String.trim str

    String.trim ""
    --> ""

    String.trim (String.trimLeft str)
    --> String.trim str

    String.trim (String.trimRight str)
    --> String.trim str

    String.trim (String.trim str)
    --> String.trim str

    String.slice n n str
    --> ""

    String.slice n 0 str
    --> ""

    String.slice a z ""
    --> ""

    String.left 0 str
    --> ""

    String.left -1 str
    --> ""

    String.left n ""
    --> ""

    String.left n (String.left n str)
    --> String.left n str

    String.right 0 str
    --> ""

    String.right -1 str
    --> ""

    String.right n ""
    --> ""

    String.right n (String.right n str)
    --> String.right n str

    -- The following simplifications for String.dropLeft also work for String.dropRight
    String.dropLeft n ""
    --> ""

    String.dropLeft 0 str
    --> str

    String.dropLeft -1 str
    --> str

    String.dropLeft 10 "Hello"
    --> ""

    String.slice start end ""
    --> ""

    String.slice 2 1 str
    --> ""

    String.slice -1 -2 str
    --> ""

    String.filter f (String.filter f str)
    --> String.filter f str

    String.filter (always True) str
    --> str

    String.filter (always False) str
    --> ""

    String.filter f (String.reverse str)
    --> String.reverse (String.filter f str)

    String.map f ""
    --> ""

    String.map identity str
    --> str

    String.map f (String.repeat n (String.fromChar c))
    --> String.repeat n (String.fromChar (f c))

    String.any f ""
    --> False

    String.any (always False) str
    -> False

    String.any (always True) str
    --> not (String.isEmpty str)

    String.all f ""
    --> True

    String.all (always True) str
    --> True

    String.all (always False) str
    --> String.isEmpty str

    -- The following simplifications for String.foldl also work for String.foldr
    String.foldl f initial ""
    --> initial

    String.foldl (\_ soFar -> soFar) initial string
    --> initial

    String.fromInt 123
    --> "123"

    String.fromFloat 1.23
    --> "1.23"


### Maybes

    Maybe.map identity x
    --> x

    Maybe.map f Nothing
    --> Nothing

    Maybe.map f (Just x)
    --> Just (f x)

    -- the following simplifications for map2 work for all Maybe.mapN
    Maybe.map2 f firstMaybe Nothing
    --> Nothing

    Maybe.map2 f (Just a) (Just b)
    --> Just (f a b)

    Maybe.andThen f Nothing
    --> Nothing

    Maybe.andThen (always Nothing) x
    --> Nothing

    Maybe.andThen (\a -> Just b) x
    --> Maybe.map (\a -> b) x

    Maybe.andThen (\a -> if condition a then Just b else Just c) x
    --> Maybe.map (\a -> if condition a then b else c) x

    Maybe.andThen f (Just x)
    --> f x

    Maybe.withDefault x Nothing
    --> x

    Maybe.withDefault x (Just y)
    --> y

    Maybe.withDefault Nothing (Maybe.map f maybe)
    --> Maybe.andThen f maybe


### Results

    Result.map identity x
    --> x

    Result.map f (Err x)
    --> Err x

    Result.map f (Ok x)
    --> Ok (f x)

    -- the following simplifications for map3 work for all Result.mapN
    Result.map3 f (Ok a) (Ok b) (Ok c)
    --> Ok (f a b c)

    Result.map3 f (Ok a) (Err x) thirdResult
    --> Err x

    Result.map3 f firstResult (Err x) thirdResult
    --> Result.map2 f firstResult (Err x)

    Result.mapError identity x
    --> x

    Result.mapError f (Ok x)
    --> Ok x

    Result.mapError f (Err x)
    --> Err (f x)

    Result.andThen f (Err x)
    --> Err x

    Result.andThen f (Ok x)
    --> f x

    Result.andThen (\a -> Ok b) x
    --> Result.map (\a -> b) x

    Result.withDefault x (Err y)
    --> x

    Result.withDefault x (Ok y)
    --> y

    Result.fromMaybe x (Just a)
    --> Ok a

    Result.fromMaybe x Nothing
    --> Err x

    Result.toMaybe (Ok x)
    --> Just x

    Result.toMaybe (Err e)
    --> Nothing

    Result.toMaybe (Result.mapError f result)
    --> Result.toMaybe result


### Lists

    a :: []
    --> [ a ]

    a :: [ b ]
    --> [ a, b ]

    [ a ] ++ list
    --> a :: list

    [] ++ list
    --> list

    [ a, b ] ++ [ c ]
    --> [ a, b, c ]

    List.append [] ys
    --> ys

    List.append [ a, b ] [ c ]
    --> [ a, b, c ]

    List.head []
    --> Nothing

    List.head [ a, b, c ]
    --> Just a

    List.head (a :: bToZ)
    --> Just a

    List.head (List.intersperse sep list)
    --> List.head list

    List.head (List.map f list)
    --> Maybe.map f (List.head list)

    List.head (List.repeat n a)
    --> if n >= 1 then Just a else Nothing

    List.tail []
    --> Nothing

    List.tail (a :: bToZ)
    --> Just bToZ

    List.member a []
    --> False

    List.member a [ a, b, c ]
    --> True

    List.member a ([a, b] ++ list)
    --> True

    List.member a [ b ]
    --> a == b

    List.member -999 [ 0, 1 ]
    --> False

    List.member needle (List.repeat n b)
    --> n >= 1 && needle == b

    List.map f [] -- same for most List functions like List.filter, List.filterMap, ...
    --> []

    List.map identity list
    --> list

    List.map f [ a ]
    --> [ f a ]

    List.map f (List.repeat n a)
    --> List.repeat n (f a)

    List.map Tuple.first (Array.toIndexedList array)
    --> List.range 0 (Array.length array - 1)

    List.filter f (List.filter f list)
    --> List.filter f list

    List.filter (always True) list
    --> list

    List.filter (always False) list
    --> []

    -- same for List.sort, List.sortBy, List.sortWith
    List.filter f (List.reverse list)
    --> List.reverse (List.filter f list)

    List.filterMap Just list
    --> list

    List.filterMap (\a -> if condition a then Just b else Just c) list
    --> List.map (\a -> if condition a then b else c) list

    List.filterMap (always Nothing) list
    --> []

    List.filterMap identity (List.map f list)
    --> List.filterMap f list

    List.filterMap identity [ Just x, Just y ]
    --> [ x, y ]

    List.filterMap identity [ a, Nothing, b ]
    --> List.filterMap identity [ a, b ]

    List.concat [ [ a, b ], [ c ] ]
    --> [ a, b, c ]

    List.concat [ a, [ 1 ], [ 2 ] ]
    --> List.concat [ a, [ 1, 2 ] ]

    List.concat [ a, [], b ]
    --> List.concat [ a, b ]

    List.concat [ a, List.concat [ b, c ], d ]
    --> List.concat [ a, b, c, d ]

    List.concatMap identity list
    --> List.concat list

    List.concatMap (\a -> [ b ]) list
    --> List.map (\a -> b) list

    List.concatMap f [ x ]
    --> f x

    List.concatMap (always []) list
    --> []

    List.concat (List.map f list)
    --> List.concatMap f list

    List.indexedMap (\_ value -> f value) list
    --> List.map (\value -> f value) list

    List.intersperse a []
    --> []

    List.intersperse s [ a ]
    --> [ a ]

    List.isEmpty []
    --> True

    List.isEmpty [ a ]
    --> False

    List.isEmpty (x :: xs)
    --> False

    -- same for List.sort, List.sortBy, List.sortWith, List.map, List.indexedMap
    List.isEmpty (List.reverse list)
    --> List.isEmpty list

    List.isEmpty (List.filter f list)
    --> not (List.any f list)

    List.isEmpty (List.filter (not << f) list)
    --> List.all f list

    List.isEmpty (String.toList string)
    --> String.isEmpty string

    List.sum []
    --> 0

    List.sum [ a ]
    --> a

    List.sum [ a, 0, b ]
    --> List.sum [ a, b ]

    -- when `expectNaN` is enabled
    List.sum [ a, 0 / 0, b ]
    --> 0 / 0

    List.product []
    --> 1

    List.product [ a ]
    --> a

    List.product [ a, 1, b ]
    --> List.product [ a, b ]

    -- when `expectNaN` is not enabled
    List.product [ a, 0, b ]
    --> 0

    -- when `expectNaN` is enabled
    List.product [ a, 0 / 0, b ]
    --> 0 / 0

    List.minimum []
    --> Nothing

    List.minimum [ a ]
    --> Just a

    List.minimum (List.range 2 3)
    --> Just 2

    -- when expectNaN is not enabled, same for List.sort, List.sortBy, List.sortWith
    List.minimum (List.reverse list)
    --> List.minimum list

    List.maximum []
    --> Nothing

    List.maximum [ a ]
    --> Just a

    List.maximum (List.range 2 3)
    --> Just 3

    -- when expectNaN is not enabled, same for List.sort, List.sortBy, List.sortWith
    List.maximum (List.reverse list)
    --> List.maximum list

    List.foldr (++) "" list
    --> String.concat list

    List.foldr (++) [] list
    --> List.concat list

    List.foldr (::) [] list
    --> list

    List.foldl (::) [] list
    --> List.reverse list

    -- The following simplifications for List.foldl also work for List.foldr
    List.foldl f x []
    --> x

    List.foldl (\_ soFar -> soFar) x list
    --> x

    List.foldl (+) 0 list
    --> List.sum list

    List.foldl (+) initial list
    --> initial + List.sum list

    List.foldl (*) 1 list
    --> List.product list

    List.foldl (*) 0 list
    --> 0

    List.foldl (*) initial list
    --> initial * List.product list

    List.foldl (&&) True list
    --> List.all identity list

    List.foldl (&&) False list
    --> False

    List.foldl (||) False list
    --> List.any identity list

    List.foldl (||) True list
    --> True

    List.foldl f initial (List.reverse list)
    --> List.foldr f initial list

    List.foldl (\( k, v ) -> f k v) init (Dict.toList dict)
    --> Dict.foldl (\k v -> f k v) init dict

    Array.foldl f x (Array.fromList list)
    --> List.foldl f x array

    -- when `expectNaN` is not enabled
    Set.member x (Set.fromList list)
    --> List.member x list

    List.all f []
    --> True

    List.all (always True) list
    --> True

    List.all (always False) list
    --> List.isEmpty list

    List.all identity [ a, False, b ]
    --> False

    List.all not [ a, True, b ]
    --> False

    List.all identity [ a, True, b ]
    --> List.all identity [ a, b ]

    List.all not [ a, False, b ]
    --> List.all not [ a, b ]

    List.all f (List.repeat n a)
    --> n <= 0 || f a

    List.all identity (List.map f list)
    --> List.all f list

    not (List.all (not << f) list)
    --> List.any f list

    List.any f []
    --> False

    List.any (always False) list
    --> False

    List.any (always True) list
    --> not (List.isEmpty list)

    List.any identity [ a, True, b ]
    --> True

    List.any not [ a, False, b ]
    --> True

    List.any identity [ a, False, b ]
    --> List.any identity [ a, b ]

    List.any not [ a, True, b ]
    --> List.any not [ a, b ]

    List.any ((==) x) list
    --> List.member x list

    List.any f (List.repeat n a)
    --> n >= 1 && f a

    List.any identity (List.map f list)
    --> List.any f list

    not (List.any (not << f) list)
    --> List.all f list

    List.range 6 3
    --> []

    List.range n n
    --> [ n ]

    List.length [ a, b, c ]
    --> 3

    -- same for List.sort, List.sortBy, List.sortWith, List.map, List.indexedMap
    List.length (List.reverse list)
    --> List.length list

    List.repeat 0 x
    --> []

    List.repeat 1 x
    --> List.singleton x

    List.partition f []
    --> ( [], [] )

    List.partition (always True) list
    --> ( list, [] )

    List.partition (always False) list
    --> ( [], list )

    Tuple.first (List.partition f list)
    --> List.filter f list

    List.take 0 list
    --> []

    List.take n (List.take n list)
    --> List.take n list

    List.take n (List.map f list)
    --> List.map f (List.take n list)

    List.take n (List.indexedMap f list)
    --> List.indexedMap f (List.take n list)

    List.drop 0 list
    --> list

    List.drop 3 [ a, b ]
    --> []

    List.drop 2 [ a, b, c ]
    --> [ c ]

    List.drop n (List.map f list)
    --> List.map f (List.drop n list)

    List.reverse []
    --> []

    List.reverse [ a ]
    --> [ a ]

    List.reverse (List.repeat n a)
    --> List.repeat n a

    List.reverse (List.reverse list)
    --> list

    List.sort (List.sort list)
    --> List.sort list

    List.sort (List.repeat n a)
    --> List.repeat n a

    List.sort (Set.toList set)
    --> Set.toList set

    List.sort (Dict.toList dict)
    --> Dict.toList dict

    List.sortBy (always a) list
    --> list

    List.sortBy identity list
    --> List.sort list

    List.sortBy f (List.repeat n a)
    --> List.repeat n a

    List.sortBy f (List.sortBy f list)
    --> List.sortBy f list

    List.sortWith (\_ _ -> LT) list
    --> List.reverse list

    List.sortWith (\_ _ -> EQ) list
    --> list

    List.sortWith (\_ _ -> GT) list
    --> list

    List.sortWith f (List.repeat n a)
    --> List.repeat n a

    -- The following simplifications for List.sort also work for List.sortBy f and List.sortWith f
    List.sort []
    --> []

    List.sort [ a ]
    --> [ a ]

    -- same for up to List.map5 when any list is empty
    List.map2 f xs []
    --> []

    List.map2 f [] ys
    --> []

    List.unzip []
    --> ( [], [] )

    List.length l == 0
    --> List.isEmpty l

    List.length l /= 0
    --> not (List.isEmpty l)


### Arrays

    Array.fromList []
    --> Array.empty

    Array.fromList (Array.toList array)
    --> array

    Array.fromList (List.repeat n a)
    --> Array.repeat n a

    Array.fromList (List.range 0 n)
    --> Array.initialize (n + 1) identity

    Array.fromList (List.map f (List.range 0 n))
    --> Array.initialize (n + 1) f

    Array.toList (Array.fromList list)
    --> list

    Array.toList Array.empty
    --> []

    Array.toList (Array.repeat n a)
    --> List.repeat n a

    Array.map f Array.empty -- same for Array.filter, Array.indexedMap, ...
    --> Array.empty

    Array.map identity array
    --> array

    Array.map f (Array.repeat n a)
    --> Array.repeat n (f a)

    Array.map f (Array.initialize n identity)
    --> Array.initialize n f

    Array.indexedMap (\_ value -> f value) array
    --> Array.map (\value -> f value) array

    Array.isEmpty Array.empty
    --> True

    -- same for Array.indexedMap
    Array.isEmpty (Array.map f array)
    --> Array.isEmpty array

    Array.repeat 0 x
    --> Array.empty

    Array.initialize 0 f
    --> Array.empty

    Array.initialize n (always a)
    --> Array.repeat n a

    Array.length Array.empty
    --> 0

    Array.length (Array.fromList [ a, b, c ])
    --> 3

    Array.length (Array.repeat 3 x)
    --> 3

    Array.length (Array.initialize 3 f)
    --> 3

    Array.length (Array.repeat n x)
    --> max 0 n

    Array.length (Array.initialize n f)
    --> max 0 n

    -- same for Array.indexedMap
    Array.length (Array.map f array)
    --> Array.length array

    Array.filter f (Array.filter f array)
    --> Array.filter f array

    Array.filter (\_ -> True) array
    --> array

    Array.filter (\_ -> False) array
    --> Array.empty

    Array.append Array.empty array
    --> array

    Array.append (Array.fromList [ a, b ]) (Array.fromList [ c, d ])
    --> Array.fromList [ a, b, c, d ]

    Array.slice n n array
    --> Array.empty

    Array.slice n 0 array
    --> Array.empty

    Array.slice a z Array.empty
    --> Array.empty

    Array.slice 2 1 array
    --> Array.empty

    Array.slice -1 -2 array
    --> Array.empty

    Array.slice start end (Array.map f array)
    --> Array.map f (Array.slice start end array)

    Array.slice 0 end (Array.indexedMap f array)
    --> Array.indexedMap f (Array.slice 0 end array)

    Array.get n Array.empty
    --> Nothing

    Array.get 1 (Array.fromList [ a, b, c ])
    --> Just b

    Array.get 100 (Array.fromList [ a, b, c ])
    --> Nothing

    Array.get -1 array
    --> Nothing

    Array.get 2 (Array.repeat 10 x)
    --> Just x

    Array.get 100 (Array.repeat 10 x)
    --> Nothing

    Array.get 2 (Array.initialize 10 f)
    --> Just (f 2)

    Array.get 100 (Array.initialize 10 f)
    --> Nothing

    Array.get i (Array.map f array)
    --> Maybe.map f (Array.get i array)

    Array.set n x Array.empty
    --> Array.empty

    Array.set i v1 (Array.set i v0 array)
    --> Array.set i v1 array

    Array.set -1 x array
    --> array

    Array.set 1 x (Array.fromList [ a, b, c ])
    --> Array.fromList [ a, x, c ]

    Array.set 100 x (Array.fromList [ a, b, c ])
    --> Array.fromList [ a, b, c ]

    -- The following simplifications for Array.foldl also work for Array.foldr
    Array.foldl f initial Array.empty
    --> initial

    Array.foldl (\_ soFar -> soFar) initial array
    --> initial

    List.foldl f x (Array.toList array)
    --> Array.foldl f x array

    Array.toIndexedList Array.empty
    --> []

    List.map Tuple.second (Array.toIndexedList array)
    --> Array.toList array

    Array.length (Array.fromList list)
    --> List.length list

    -- The following simplification also works for Array.toIndexedList
    List.length (Array.toList array)
    --> Array.length array

    -- The following simplification also works for Array.toIndexedList
    List.isEmpty (Array.toList array)
    --> Array.isEmpty array

    Array.length a == 0
    --> Array.isEmpty a

    Array.length a /= 0
    --> not (Array.isEmpty a)


### Sets

    Set.fromList []
    --> Set.empty

    Set.fromList [ a ]
    --> Set.singleton a

    Set.fromList (Set.toList set)
    --> set

    Set.fromList [ a, a ]
    --> Set.fromList [ a ]

    -- when expectNaN is not enabled, same for List.sort, List.sortBy, List.sortWith
    Set.fromList (List.reverse list)
    --> Set.fromList list

    -- when expectNaN is not enabled
    Set.fromList (List.repeat n a)
    --> if n >= 1 then Set.singleton a else Set.empty

    Set.map f Set.empty -- same for Set.filter, Set.remove...
    --> Set.empty

    Set.map identity set
    --> set

    Set.isEmpty Set.empty
    --> True

    Set.isEmpty (Set.fromList ([a] ++ list))
    --> False

    Set.isEmpty (Set.map f set)
    --> Set.isEmpty set

    Set.member x Set.empty
    --> False

    Set.member x (Set.singleton y)
    --> x == y

    Set.member x (Set.fromList [ y, x ])
    --> True

    Set.member -999 (Set.fromList [ 0, 1 ])
    --> False

    Set.toList Set.empty
    --> []

    Set.size Set.empty
    --> 0

    Set.intersect Set.empty set
    --> Set.empty

    Set.intersect set set
    --> set

    Set.intersect (Set.intersect set0 set1) set0
    --> Set.intersect set0 set1

    Set.diff Set.empty set
    --> Set.empty

    Set.diff set Set.empty
    --> set

    Set.union set Set.empty
    --> set

    Set.union set set
    --> set

    Set.union (Set.union set0 set1) set0
    --> Set.union set0 set1

    Set.union (Set.singleton a) set
    --> Set.insert a set

    Set.union set (Set.singleton a)
    --> Set.insert a set

    Set.union (Set.fromList [ a, b ]) (Set.fromList [ c, d ])
    --> Set.fromList [ a, b, c, d ]

    Set.insert x Set.empty
    --> Set.singleton x

    Set.insert k (Set.insert k set)
    --> Set.insert k set

    Set.remove k (Set.remove k set)
    --> Set.remove k set

    -- same for foldr
    List.foldl f x (Set.toList set)
    --> Set.foldl f x set

    List.member x (Set.toList set)
    --> Set.member x set

    Set.filter f (Set.filter f set)
    --> Set.filter f set

    Set.filter (\_ -> True) set
    --> set

    Set.filter (\_ -> False) set
    --> Set.empty

    -- when `expectNaN` is not enabled
    Set.filter (\k -> k /= specificKey) set
    --> Set.remove specificKey set

    Set.partition f Set.empty
    --> ( Set.empty, Set.empty )

    Set.partition (always True) set
    --> ( set, Set.empty )

    Tuple.first (Set.partition f set)
    --> Set.filter f set

    Set.foldr (::) [] set
    --> Set.toList set

    -- The following simplifications for Set.foldl also work for Set.foldr
    Set.foldl f initial Set.empty
    --> initial

    Set.foldl (\_ soFar -> soFar) initial set
    --> initial

    -- when `expectNaN` is not enabled
    Set.foldl Set.insert Set.empty set
    --> set

    List.length (Set.toList set)
    --> Set.size set

    List.isEmpty (Set.toList set)
    --> Set.isEmpty set

    Set.size set == 0
    --> Set.isEmpty set

    Set.size set /= 0
    --> not (Set.isEmpty set)


### Dict

    Dict.fromList []
    --> Dict.empty

    Dict.fromList (Dict.toList dict)
    --> dict

    Dict.fromList [ a, a ]
    --> Dict.fromList [ a ]

    Dict.fromList [ ( key, a ), ( key, b ) ]
    --> Dict.fromList [ ( key, b ) ]

    -- when expectNaN is not enabled
    Dict.fromList (List.repeat n a)
    --> if n >= 1 then Dict.fromList [ a ] else Dict.empty

    Dict.isEmpty Dict.empty
    --> True

    Dict.isEmpty (Dict.map f dict)
    --> Dict.isEmpty dict

    Dict.toList Dict.empty
    --> []

    Dict.size Dict.empty
    --> 0

    Dict.size (Dict.map f dict)
    --> Dict.size dict

    Dict.member x Dict.empty
    --> False

    Dict.member x (Dict.fromList [ ( y, v0 ), ( x, v1 ) ])
    --> True

    Dict.member -999 (Dict.fromList [ ( 0, v0 ), ( 1, v1 ) ])
    --> False

    -- when `expectNaN` is not enabled
    Dict.member x (Dict.fromList list)
    --> List.any (Tuple.first >> (==) x) list

    Dict.insert k v1 (Dict.insert k v0 dict)
    --> Dict.insert k v1 dict

    Dict.remove k Dict.empty
    --> Dict.empty

    Dict.remove k (Dict.remove k dict)
    --> Dict.remove k dict

    Dict.remove k (Dict.map f dict)
    --> Dict.map f (Dict.remove k dict)

    Dict.update k identity dict
    --> dict

    Dict.update k (\_ -> Nothing) dict
    --> Dict.remove k dict

    Dict.update k (\_ -> Just v) dict
    --> Dict.insert k v dict

    Dict.filter f Dict.empty
    --> Dict.empty

    Dict.filter f (Dict.filter f dict)
    --> Dict.filter f dict

    Dict.filter (\_ _ -> True) dict
    --> dict

    Dict.filter (\_ _ -> False) dict
    --> Dict.empty

    Dict.filter (\k _ -> f k) (Dict.map g dict)
    --> Dict.map g (Dict.filter (\k _ -> f k) dict)

    -- when `expectNaN` is not enabled
    Dict.filter (\k _ -> k /= specificKey) dict
    --> Dict.remove specificKey dict

    Dict.map f Dict.empty
    --> Dict.empty

    Dict.map (\_ value -> value) dict
    --> dict

    Dict.intersect Dict.empty dict
    --> Dict.empty

    Dict.intersect dict dict
    --> dict

    Dict.intersect (Dict.intersect dict0 dict1) dict0
    --> Dict.intersect dict0 dict1

    Dict.diff Dict.empty dict
    --> Dict.empty

    Dict.diff dict Dict.empty
    --> dict

    Dict.diff dict (Dict.map f remove)
    --> Dict.diff dict remove

    Dict.diff (Dict.map f dict) remove
    --> Dict.map f (Dict.diff dict remove)

    Dict.union dict Dict.empty
    --> dict

    Dict.union dict dict
    --> dict

    Dict.union (Dict.union dict0 dict1) dict0
    --> Dict.union dict0 dict1

    Dict.union (Dict.singleton k v) dict
    --> Dict.insert k v dict

    Dict.union (Dict.fromList [ a, b ]) (Dict.fromList [ c, d ])
    --> Dict.fromList [ c, d, a, b ]

    Dict.partition f Dict.empty
    --> ( Dict.empty, Dict.empty )

    Dict.partition (\_ _ -> True) dict
    --> ( dict, Dict.empty )

    Dict.partition (\_ _ -> False) dict
    --> ( Dict.empty, dict )

    Tuple.first (Dict.partition f dict)
    --> Dict.filter f dict

    List.map Tuple.first (Dict.toList dict)
    --> Dict.keys dict

    List.map Tuple.second (Dict.toList dict)
    --> Dict.values dict

    Dict.foldr (\k _ ks -> k :: ks) [] dict
    --> Dict.keys dict

    Dict.foldr (\_ v vs -> v :: vs) [] dict
    --> Dict.values dict

    Dict.foldr (\k v kvs -> ( k, v ) :: kvs) [] dict
    --> Dict.toList dict

    -- The following foldl simplifications also work for foldr
    Dict.foldl f initial Dict.empty
    --> initial

    Dict.foldl (\_ soFar -> soFar) initial dict
    --> initial

    -- when `expectNaN` is not enabled
    Dict.foldl Dict.insert Dict.empty dict
    --> dict

    List.foldl (\v s -> f v s) init (Dict.values dict)
    --> Dict.foldl (\_ v s -> f v s) init dict

    List.foldl (\k s -> f k s) init (Dict.keys dict)
    --> Dict.foldl (\k _ s -> f k s) init dict

    -- The following simplification also works for Dict.keys, Dict.values
    List.length (Dict.toList dict)
    --> Dict.size dict

    -- The following simplification also works for Dict.keys, Dict.values
    List.isEmpty (Dict.toList dict)
    --> Dict.isEmpty dict

    Dict.size dict == 0
    --> Dict.isEmpty dict

    Dict.size dict /= 0
    --> not (Dict.isEmpty dict)


### Cmd / Sub

All of these also apply for `Sub`.

    Cmd.batch []
    --> Cmd.none

    Cmd.batch [ a ]
    --> a

    Cmd.batch [ a, Cmd.none, b ]
    --> Cmd.batch [ a, b ]

    Cmd.batch [ a, Cmd.batch [ b, c ], d ]
    --> Cmd.batch [ a, b, c, d ]

    Cmd.map identity cmd
    --> cmd

    Cmd.map f Cmd.none
    --> Cmd.none


### Task

    Task.map identity task
    --> task

    Task.map f (Task.fail x)
    --> Task.fail x

    Task.map f (Task.succeed a)
    --> Task.succeed (f a)

    -- the following simplifications for map3 work for all Task.mapN
    Task.map3 f (Task.succeed a) (Task.succeed b) (Task.succeed c)
    --> Task.succeed (f a b c)

    Task.map3 f (Task.succeed a) (Task.fail x) thirdTask
    --> Task.fail x

    Task.map3 f firstTask (Task.fail x) thirdTask
    --> Task.map2 f firstTask (Task.fail x)

    Task.andThen f (Task.fail x)
    --> Task.fail x

    Task.andThen f (Task.succeed a)
    --> f a

    Task.andThen Task.succeed task
    --> task

    Task.andThen (\a -> Task.succeed b) task
    --> Task.map (\a -> b) task

    Task.mapError identity task
    --> task

    Task.mapError f (Task.succeed a)
    --> Task.succeed a

    Task.mapError f (Task.fail x)
    --> Task.fail (f x)

    Task.onError f (Task.succeed a)
    --> Task.succeed a

    Task.onError f (Task.fail x)
    --> f x

    Task.onError Task.fail task
    --> task

    Task.onError (\x -> Task.fail y) task
    --> Task.mapError (\x -> y) x

    Task.sequence [ Task.succeed a, Task.succeed b ]
    --> Task.succeed [ a, b ]

    Task.sequence [ Task.succeed a, Task.fail x ]
    --> Task.fail x

    Task.sequence [ a, Task.fail x, b ]
    --> Task.sequence [ a, Task.fail x ]

    Task.sequence [ task ]
    --> Task.map List.singleton task

    Task.attempt identity (Task.map f task)
    --> Task.attempt f task

    Task.perform identity (Task.map f task)
    --> Task.perform f task

    Cmd.map f (Task.perform identity task)
    --> Task.perform f task

    Cmd.map f (Task.attempt identity task)
    --> Task.attempt f task


### Html.Attributes

    Html.Attributes.classList [ x, y, ( z, False ) ]
    --> Html.Attributes.classList [ x, y ]

    Html.Attributes.classList [ ( onlyOneThing, True ) ]
    --> Html.Attributes.class onlyOneThing


### Json.Encode

    Json.Encode.list f (Array.toList array)
    --> Json.Encode.array f array

    Json.Encode.array identity (Array.map f array)
    --> Json.Encode.array f array

    Json.Encode.array f (Array.fromList list)
    --> Json.Encode.list f list

    Json.Encode.list identity (List.map f list)
    --> Json.Encode.list f list

    Json.Encode.list f (Set.toList set)
    --> Json.Encode.set f set

    Json.Encode.set identity (Set.map f set)
    --> Json.Encode.set f set


### Json.Decode

    Json.Decode.map identity decoder
    --> decoder

    Json.Decode.map f (Json.Decode.fail x)
    --> Json.Decode.fail x

    Json.Decode.map f (Json.Decode.succeed a)
    --> Json.Decode.succeed (f a)

    -- the following simplifications for map3 work for all Json.Decode.mapN
    Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.succeed b) (Json.Decode.succeed c)
    --> Json.Decode.succeed (f a b c)

    Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.fail x) thirdDecoder
    --> Json.Decode.fail x

    Json.Decode.map3 f firstDecoder (Json.Decode.fail x) thirdDecoder
    --> Json.Decode.map2 f firstDecoder (Json.Decode.fail x)

    Json.Decode.andThen f (Json.Decode.fail x)
    --> Json.Decode.fail x

    Json.Decode.andThen f (Json.Decode.succeed a)
    --> f a

    Json.Decode.andThen Json.Decode.succeed decoder
    --> decoder

    Json.Decode.andThen (\a -> Json.Decode.succeed b) decoder
    --> Json.Decode.map (\a -> b) decoder

    Json.Decode.oneOf [ a ]
    --> a


### Parser

    Parser.oneOf [ a ]
    --> a


### Random

    Random.uniform a []
    --> Random.constant a

    Random.weighted ( weight, a ) []
    --> Random.constant a

    Random.weighted tuple []
    --> Random.constant (Tuple.first tuple)

    Random.list 0 generator
    --> Random.constant []

    Random.list 1 generator
    --> Random.map List.singleton generator

    Random.list n (Random.constant el)
    --> Random.constant (List.repeat n el)

    Random.map identity generator
    --> generator

    Random.map (always a) generator
    --> Random.constant a

    Random.map f (Random.constant x)
    --> Random.constant (f x)

    Random.andThen f (Random.constant x)
    --> f x

    Random.andThen Random.constant generator
    --> generator

    Random.andThen (\a -> Random.constant b) generator
    --> Random.map (\a -> b) generator

    Random.andThen (always thenGenerator) generator
    --> thenGenerator


### Test

    Test.concat [ test ]
    --> test

    Test.concat [ test0, Test.concat [ test1, test2 ], test3 ]
    --> Test.concat [ test0, test1, test2, test3 ]

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Elm.Type
import Fn.Array
import Fn.Basics
import Fn.Dict
import Fn.Html.Attributes
import Fn.Json.Decode
import Fn.Json.Encode
import Fn.List
import Fn.Maybe
import Fn.Parser
import Fn.Parser.Advanced
import Fn.Platform.Cmd
import Fn.Platform.Sub
import Fn.Random
import Fn.Result
import Fn.Set
import Fn.String
import Fn.Task
import Fn.Test
import Fn.Tuple
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Simplify.AstHelpers as AstHelpers exposing (emptyStringAsString, qualifiedToString)
import Simplify.CallStyle as CallStyle exposing (FunctionCallStyle)
import Simplify.CoreHelpers exposing (consIf, countUnique, findMap, findMapAndAllBefore, findMapNeighboring, indexedFindMap, isJust, isNothing, listAll2, listFilledFromList, listFilledHead, listFilledInit, listFilledLast, listFilledLength, listFilledMap, listFilledTail, listFilledToList, listFind, listIndexedFilterMap, listLast, maybeWithDefaultLazy, onNothing, traverse, traverseConcat, uniqueByThenMap)
import Simplify.HashExpression as HashExpression
import Simplify.Infer as Infer
import Simplify.Match exposing (Match(..))
import Simplify.Normalize as Normalize
import Simplify.RangeDict as RangeDict exposing (RangeDict)


{-| Rule to simplify Elm code.
-}
rule : Configuration -> Rule
rule (Configuration config) =
    Rule.newProjectRuleSchema "Simplify" initialContext
        |> Rule.withDirectDependenciesProjectVisitor
            (\deps -> dependenciesVisitor (Set.fromList config.ignoreConstructors) deps)
        |> Rule.withModuleVisitor (\moduleSchema -> moduleVisitor config moduleSchema)
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


moduleVisitor : { config | expectNaN : Bool } -> Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor config schema =
    schema
        |> Rule.withCommentsVisitor (\comments context -> ( [], commentsVisitor comments context ))
        |> Rule.withDeclarationListVisitor (\decls context -> ( [], declarationListVisitor decls context ))
        |> Rule.withDeclarationEnterVisitor (\node context -> ( [], declarationVisitor node context ))
        |> Rule.withExpressionEnterVisitor (\expressionNode context -> expressionVisitor expressionNode config context)
        |> Rule.withExpressionExitVisitor (\node context -> ( [], expressionExitVisitor node context ))



-- CONFIGURATION


{-| Configuration for this rule. Create a new one with [`defaults`](#defaults) and use [`ignoreCaseOfForTypes`](#ignoreCaseOfForTypes) and [`expectNaN`](#expectNaN) to alter it.
-}
type Configuration
    = Configuration
        { ignoreConstructors : List String
        , expectNaN : Bool
        }


{-| Default configuration for this rule.

The rule aims tries to improve the code through simplifications that don't impact the behavior. An exception to this are
when the presence of `NaN` values

Use [`expectNaN`](#expectNaN) if you want to opt out of changes that can impact the behaviour of your code if you expect to work with `NaN` values.

Use [`ignoreCaseOfForTypes`](#ignoreCaseOfForTypes) if you want to prevent simplifying case expressions that work on custom types defined in dependencies.

    config =
        [ Simplify.rule Simplify.defaults
        ]

    -- or
    config =
        [ Simplify.defaults
            |> Simplify.expectNaN
            |> Simplify.ignoreCaseOfForTypes [ "Module.Name.Type" ]
            |> Simplify.rule
        ]

-}
defaults : Configuration
defaults =
    Configuration
        { ignoreConstructors = []
        , expectNaN = False
        }


{-| Ignore some reports about types from dependencies used in case expressions.

This rule simplifies the following construct:

    module Module.Name exposing (..)

    case value of
        Just _ -> x
        Nothing -> x
    --> x

(Since `v2.0.19`) it will not try to simplify the case expression when some of the patterns references custom types constructors
defined in the project. It will only do so for custom types that are defined in dependencies (including `elm/core`).

If you do happen to want to disable this simplification for a type `Module.Name.Type`, you can configure the rule like this:

    config =
        [ Simplify.defaults
            |> Simplify.ignoreCaseOfForTypes [ "Module.Name.Type" ]
            |> Simplify.rule
        ]

I personally don't recommend to use this function too much, because this could be a sign of premature abstraction, and because
I think that often [You Aren't Gonna Need this code](https://jfmengels.net/safe-dead-code-removal/#yagni-you-arent-gonna-need-it).

Please let me know by opening an issue if you do use this function, I am very curious to know;

-}
ignoreCaseOfForTypes : List String -> Configuration -> Configuration
ignoreCaseOfForTypes ignoreConstructors (Configuration config) =
    Configuration { ignoreConstructors = ignoreConstructors ++ config.ignoreConstructors, expectNaN = config.expectNaN }


{-| Usually, `elm-review-simplify` will only suggest simplifications that are safe to apply without risk of changing the original behavior.
However, when encountering [`NaN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN)
values, some simplifications can actually impact behavior.

For instance, the following expression will evaluate to `True`:

    x == x
    --> True

However, if `x` is `NaN` or a value containing `NaN` then the expression will evaluate to `False`:

    -- given x = NaN
    x == x
    --> False

    -- given x = { a = ( NaN, 0 ) }
    x == x
    --> False

Given the potential presence of `NaN`, some simplifications become unsafe to apply:

  - `x == x` to `True`
  - `List.member x [ x ]` to `True`
  - `n * 0` to `0`

This special value is hard to recreate in Elm code both intentionally and unintentionally,
and it's therefore unlikely to be found in your application,
which is why the rule applies these simplifications by defaults.

If you somehow expect to create and encounter `NaN` values in your codebase, then you can use this function to disable these simplifications altogether.

    config =
        [ Simplify.defaults
            |> Simplify.expectNaN
            |> Simplify.rule
        ]

-}
expectNaN : Configuration -> Configuration
expectNaN (Configuration config) =
    Configuration { ignoreConstructors = config.ignoreConstructors, expectNaN = True }



-- CONTEXT


type alias ProjectContext =
    { customTypesToReportInCases : Set ( ModuleName, ConstructorName )
    , exposedVariants : Dict ModuleName (Set String)
    , exposedRecordTypeAliases : Dict ModuleName (Dict String (List String))
    , exposedCustomTypes :
        Dict
            ModuleName
            (Dict
                String
                { variantNames : Set String
                , allParametersAreUsedInVariants : Bool
                }
            )
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleName : ModuleName
    , exposed : ExposingContext
    , commentRanges : List Range
    , importRecordTypeAliases : Dict ModuleName (Dict String (List String))
    , moduleRecordTypeAliases : Dict String (List String)
    , importCustomTypes :
        Dict
            ModuleName
            (Dict
                String
                { variantNames : Set String
                , allParametersAreUsedInVariants : Bool
                }
            )
    , moduleCustomTypes :
        Dict
            String
            { variantNames : Set String
            , allParametersAreUsedInVariants : Bool
            }
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , branchLocalBindings : RangeDict (Set String)
    , rangesToIgnore : List Range
    , rightSidesOfPlusPlus : RangeDict ()
    , customTypesToReportInCases : Set ( ModuleName, ConstructorName )
    , localIgnoredCustomTypes : List Constructor
    , constructorsToIgnore : Set ( ModuleName, String )
    , inferredConstantsDict : RangeDict Infer.Inferred
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , extractSourceCode : Range -> String
    , exposedVariants : Set String
    , importLookup : ImportLookup
    }


type alias ImportLookup =
    Dict
        ModuleName
        { alias : Maybe ModuleName
        , exposed : Exposed -- includes names of found variants
        }


type alias QualifyResources a =
    { a
        | importLookup : ImportLookup
        , moduleBindings : Set String
        , localBindings : RangeDict (Set String)
    }


defaultQualifyResources : QualifyResources {}
defaultQualifyResources =
    { importLookup = implicitImports
    , localBindings = RangeDict.empty
    , moduleBindings = Set.empty
    }


type ExposingContext
    = ExposingAllContext
    | ExposingSomeContext { typesExposingVariants : Set String, potentialTypeAliases : Set String }


type Exposed
    = ExposedAll
    | ExposedSome (Set String)


isExposedFrom : Exposed -> String -> Bool
isExposedFrom exposed name =
    case exposed of
        ExposedAll ->
            True

        ExposedSome some ->
            Set.member name some


type alias ConstructorName =
    String


type alias Constructor =
    { moduleName : ModuleName
    , name : String
    , constructors : List String
    }


initialContext : ProjectContext
initialContext =
    { customTypesToReportInCases = Set.empty
    , exposedVariants = Dict.empty
    , exposedRecordTypeAliases = Dict.empty
    , exposedCustomTypes = Dict.empty
    }


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            { customTypesToReportInCases = Set.empty
            , exposedVariants =
                Dict.singleton moduleContext.moduleName
                    moduleContext.exposedVariants
            , exposedRecordTypeAliases =
                Dict.singleton moduleContext.moduleName
                    (case moduleContext.exposed of
                        ExposingAllContext ->
                            moduleContext.moduleRecordTypeAliases

                        ExposingSomeContext exposingSomeContext ->
                            Set.foldl
                                (\exposedPotentialTypeAlias soFar ->
                                    case Dict.get exposedPotentialTypeAlias moduleContext.moduleRecordTypeAliases of
                                        Nothing ->
                                            soFar

                                        Just recordTypeAlias ->
                                            Dict.insert exposedPotentialTypeAlias recordTypeAlias soFar
                                )
                                Dict.empty
                                exposingSomeContext.potentialTypeAliases
                    )
            , exposedCustomTypes =
                Dict.singleton moduleContext.moduleName
                    (case moduleContext.exposed of
                        ExposingAllContext ->
                            moduleContext.moduleCustomTypes

                        ExposingSomeContext exposingSomeContext ->
                            Set.foldl
                                (\exposedPotentialTypeAlias soFar ->
                                    case Dict.get exposedPotentialTypeAlias moduleContext.moduleCustomTypes of
                                        Nothing ->
                                            soFar

                                        Just recordTypeAlias ->
                                            Dict.insert exposedPotentialTypeAlias recordTypeAlias soFar
                                )
                                Dict.empty
                                exposingSomeContext.typesExposingVariants
                    )
            }
        )


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable metadata extractSourceCode fullAst projectContext ->
            let
                imports : ImportLookup
                imports =
                    List.foldl
                        (\import_ importLookup ->
                            insertImport (importContext import_) importLookup
                        )
                        implicitImports
                        fullAst.imports
            in
            { lookupTable = lookupTable
            , moduleName = Rule.moduleNameFromMetadata metadata
            , exposed =
                moduleExposingContext (Elm.Syntax.Module.exposingList (Node.value fullAst.moduleDefinition))
            , importLookup =
                createImportLookup
                    { imports = imports
                    , importExposedVariants = projectContext.exposedVariants
                    }
            , commentRanges = []
            , importRecordTypeAliases = projectContext.exposedRecordTypeAliases
            , moduleRecordTypeAliases = Dict.empty
            , importCustomTypes = projectContext.exposedCustomTypes
            , moduleCustomTypes = Dict.empty
            , moduleBindings = Set.empty
            , localBindings = RangeDict.empty
            , branchLocalBindings = RangeDict.empty
            , rangesToIgnore = []
            , rightSidesOfPlusPlus = RangeDict.empty
            , localIgnoredCustomTypes = []
            , customTypesToReportInCases = projectContext.customTypesToReportInCases
            , constructorsToIgnore = Set.empty
            , inferredConstantsDict = RangeDict.empty
            , inferredConstants = ( Infer.empty, [] )
            , extractSourceCode = extractSourceCode
            , exposedVariants = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withMetadata
        |> Rule.withSourceCodeExtractor
        |> Rule.withFullAst


importContext : Node Import -> { moduleName : ModuleName, exposed : Exposed, alias : Maybe ModuleName }
importContext importNode =
    let
        import_ : Import
        import_ =
            Node.value importNode
    in
    { moduleName = import_.moduleName |> Node.value
    , alias =
        import_.moduleAlias |> Maybe.map Node.value
    , exposed =
        case import_.exposingList of
            Nothing ->
                ExposedSome Set.empty

            Just (Node _ existingExposing) ->
                case existingExposing of
                    Exposing.All _ ->
                        ExposedAll

                    Exposing.Explicit exposes ->
                        ExposedSome
                            (List.foldl
                                (\(Node _ expose) soFar ->
                                    Set.insert (AstHelpers.nameOfExpose expose) soFar
                                )
                                Set.empty
                                exposes
                            )
    }


createImportLookup :
    { imports : Dict ModuleName { alias : Maybe ModuleName, exposed : Exposed }
    , importExposedVariants : Dict ModuleName (Set String)
    }
    -> ImportLookup
createImportLookup context =
    context.imports
        |> Dict.map
            (\moduleName import_ ->
                case import_.exposed of
                    ExposedAll ->
                        import_

                    ExposedSome some ->
                        case Dict.get moduleName context.importExposedVariants of
                            Nothing ->
                                import_

                            Just importExposedVariants ->
                                { alias = import_.alias
                                , exposed =
                                    ExposedSome
                                        (Set.union some importExposedVariants)
                                }
            )


moduleExposingContext : Exposing.Exposing -> ExposingContext
moduleExposingContext exposingSyntax =
    case exposingSyntax of
        Exposing.All _ ->
            ExposingAllContext

        Exposing.Explicit some ->
            ExposingSomeContext
                (List.foldl
                    (\(Node _ expose) soFar ->
                        case expose of
                            Exposing.InfixExpose _ ->
                                soFar

                            Exposing.FunctionExpose _ ->
                                soFar

                            Exposing.TypeOrAliasExpose name ->
                                { typesExposingVariants = soFar.typesExposingVariants
                                , potentialTypeAliases = Set.insert name soFar.potentialTypeAliases
                                }

                            Exposing.TypeExpose variantType ->
                                case variantType.open of
                                    Nothing ->
                                        soFar

                                    Just _ ->
                                        { potentialTypeAliases = soFar.potentialTypeAliases
                                        , typesExposingVariants = Set.insert variantType.name soFar.typesExposingVariants
                                        }
                    )
                    exposingSomeContextEmpty
                    some
                )


exposingSomeContextEmpty : { typesExposingVariants : Set String, potentialTypeAliases : Set String }
exposingSomeContextEmpty =
    { typesExposingVariants = Set.empty
    , potentialTypeAliases = Set.empty
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { customTypesToReportInCases = Set.empty
    , exposedVariants = Dict.union newContext.exposedVariants previousContext.exposedVariants
    , exposedRecordTypeAliases = Dict.union newContext.exposedRecordTypeAliases previousContext.exposedRecordTypeAliases
    , exposedCustomTypes = Dict.union newContext.exposedCustomTypes previousContext.exposedCustomTypes
    }



-- DEPENDENCIES VISITOR


dependenciesVisitor : Set String -> Dict String Dependency -> ProjectContext -> ( List (Error scope), ProjectContext )
dependenciesVisitor typeNamesAsStrings dependencies context =
    ( if Set.isEmpty typeNamesAsStrings then
        []

      else
        let
            unions : Set String
            unions =
                dependencies
                    |> Dict.foldl
                        (\_ dependency soFarAcrossDependencies ->
                            dependency
                                |> Dependency.modules
                                |> List.foldl
                                    (\moduleDocs withDependencySoFar ->
                                        List.foldl
                                            (\union withModuleUnionsSoFar ->
                                                Set.insert (moduleDocs.name ++ "." ++ union.name)
                                                    withModuleUnionsSoFar
                                            )
                                            withDependencySoFar
                                            moduleDocs.unions
                                    )
                                    soFarAcrossDependencies
                        )
                        Set.empty

            unknownTypesToIgnore : Set String
            unknownTypesToIgnore =
                Set.diff typeNamesAsStrings unions
        in
        if Set.isEmpty unknownTypesToIgnore then
            []

        else
            [ errorForUnknownIgnoredConstructor (Set.toList unknownTypesToIgnore) ]
    , dependencies
        |> Dict.foldl
            (\_ dependency soFarAcrossDependencies ->
                dependency
                    |> Dependency.modules
                    |> List.foldl
                        (\moduleDocs withDependencySoFar ->
                            updateContextWithModuleInterface typeNamesAsStrings
                                moduleDocs
                                withDependencySoFar
                        )
                        soFarAcrossDependencies
            )
            context
    )


updateContextWithModuleInterface : Set String -> Elm.Docs.Module -> ProjectContext -> ProjectContext
updateContextWithModuleInterface typeNamesAsStrings moduleDocs withDependencySoFar =
    let
        moduleName : ModuleName
        moduleName =
            AstHelpers.moduleNameFromString moduleDocs.name
    in
    { exposedVariants =
        Dict.insert
            (AstHelpers.moduleNameFromString moduleDocs.name)
            (moduleDocs.unions
                |> List.foldl
                    (\union moduleVariantNamesSoFar ->
                        union.tags
                            |> List.foldl
                                (\( variantName, _ ) withVariantsSoFar ->
                                    Set.insert variantName withVariantsSoFar
                                )
                                moduleVariantNamesSoFar
                    )
                    Set.empty
            )
            withDependencySoFar.exposedVariants
    , customTypesToReportInCases =
        moduleDocs.unions
            |> List.foldl
                (\union withModuleUnionsSoFar ->
                    if Set.member (moduleDocs.name ++ "." ++ union.name) typeNamesAsStrings then
                        withModuleUnionsSoFar

                    else
                        union.tags
                            |> List.foldl
                                (\( tagName, _ ) withUnionVariantsSoFar ->
                                    Set.insert ( moduleName, tagName ) withUnionVariantsSoFar
                                )
                                withModuleUnionsSoFar
                )
                withDependencySoFar.customTypesToReportInCases
    , exposedRecordTypeAliases =
        Dict.insert (AstHelpers.moduleNameFromString moduleDocs.name)
            (moduleDocs.aliases
                |> List.foldl
                    (\typeAliasDocs moduleAliasesSoFar ->
                        case typeAliasDocs.tipe of
                            Elm.Type.Record fields Nothing ->
                                Dict.insert typeAliasDocs.name
                                    (List.map (\( name, _ ) -> name) fields)
                                    moduleAliasesSoFar

                            _ ->
                                moduleAliasesSoFar
                    )
                    Dict.empty
            )
            withDependencySoFar.exposedRecordTypeAliases
    , exposedCustomTypes =
        Dict.insert
            (moduleDocs.name |> AstHelpers.moduleNameFromString)
            (moduleDocs.unions
                |> List.foldl
                    (\choiceTypeDocs moduleChoiceTypesSoFar ->
                        Dict.insert choiceTypeDocs.name
                            (interfaceChoiceTypeToInfo choiceTypeDocs)
                            moduleChoiceTypesSoFar
                    )
                    Dict.empty
            )
            withDependencySoFar.exposedCustomTypes
    }


interfaceChoiceTypeToInfo :
    Elm.Docs.Union
    ->
        { variantNames : Set String
        , allParametersAreUsedInVariants : Bool
        }
interfaceChoiceTypeToInfo interfacesChoiceType =
    { variantNames =
        interfacesChoiceType.tags
            |> List.foldl
                (\( name, _ ) variantNamesSoFar ->
                    Set.insert name variantNamesSoFar
                )
                Set.empty
    , allParametersAreUsedInVariants =
        interfacesChoiceType.args
            |> List.all
                (\parameter ->
                    interfacesChoiceType.tags
                        |> List.any
                            (\( _, variantValues ) ->
                                variantValues
                                    |> List.any
                                        (\variantValue ->
                                            variantValue |> interfaceTypeUsesVariable parameter
                                        )
                            )
                )
    }


interfaceTypeUsesVariable : String -> Elm.Type.Type -> Bool
interfaceTypeUsesVariable variableNeedle interfaceType =
    case interfaceType of
        Elm.Type.Var variable ->
            variable == variableNeedle

        Elm.Type.Lambda input output ->
            if interfaceTypeUsesVariable variableNeedle input then
                True

            else
                interfaceTypeUsesVariable variableNeedle output

        Elm.Type.Tuple parts ->
            List.any
                (\part -> interfaceTypeUsesVariable variableNeedle part)
                parts

        Elm.Type.Type _ arguments ->
            List.any
                (\argument -> interfaceTypeUsesVariable variableNeedle argument)
                arguments

        Elm.Type.Record fields maybeExtendedRecordVariable ->
            let
                extendedRecordVariableIsNeedle : Bool
                extendedRecordVariableIsNeedle =
                    case maybeExtendedRecordVariable of
                        Nothing ->
                            False

                        Just extendedRecordVariable ->
                            extendedRecordVariable == variableNeedle
            in
            if extendedRecordVariableIsNeedle then
                True

            else
                List.any
                    (\( _, fieldValue ) ->
                        interfaceTypeUsesVariable variableNeedle fieldValue
                    )
                    fields


errorForUnknownIgnoredConstructor : List String -> Error scope
errorForUnknownIgnoredConstructor list =
    Rule.globalError
        { message = "Could not find type names: " ++ (String.join ", " <| List.map wrapInBackticks list)
        , details =
            [ "I expected to find these custom types in the dependencies, but I could not find them."
            , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
            , "If you find that these types have been moved or renamed, please update your configuration."
            , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
            , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
            ]
        }



-- COMMENTS VISITOR


commentsVisitor : List (Node String) -> ModuleContext -> ModuleContext
commentsVisitor comments context =
    { context | commentRanges = List.map Node.range comments }



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor declarationList context =
    { context
        | moduleBindings = AstHelpers.declarationListBindings declarationList
        , moduleRecordTypeAliases =
            List.foldl
                (\(Node _ declaration) soFar ->
                    case declaration of
                        Declaration.AliasDeclaration typeAliasDeclaration ->
                            case typeAliasDeclaration.typeAnnotation of
                                Node _ (TypeAnnotation.Record fields) ->
                                    Dict.insert (Node.value typeAliasDeclaration.name)
                                        (List.map (\(Node _ ( Node _ field, _ )) -> field) fields)
                                        soFar

                                _ ->
                                    soFar

                        _ ->
                            soFar
                )
                Dict.empty
                declarationList
        , moduleCustomTypes =
            List.foldl
                (\(Node _ declaration) soFar ->
                    case declaration of
                        Declaration.CustomTypeDeclaration variantType ->
                            Dict.insert (Node.value variantType.name)
                                { variantNames =
                                    variantType.constructors
                                        |> List.foldl
                                            (\(Node _ variant) variantNamesSoFar ->
                                                Set.insert (Node.value variant.name) variantNamesSoFar
                                            )
                                            Set.empty
                                , allParametersAreUsedInVariants =
                                    variantType.generics
                                        |> List.all
                                            (\(Node _ parameter) ->
                                                variantType.constructors
                                                    |> List.any
                                                        (\(Node _ variant) ->
                                                            variant.arguments
                                                                |> List.any
                                                                    (\variantValue ->
                                                                        variantValue |> AstHelpers.typeUsesVariable parameter
                                                                    )
                                                        )
                                            )
                                }
                                soFar

                        _ ->
                            soFar
                )
                Dict.empty
                declarationList
    }



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ModuleContext
declarationVisitor declarationNode context =
    case Node.value declarationNode of
        Declaration.CustomTypeDeclaration variantType ->
            let
                variantsAreExposed : Bool
                variantsAreExposed =
                    case context.exposed of
                        ExposingAllContext ->
                            True

                        ExposingSomeContext exposingSome ->
                            Set.member (Node.value variantType.name) exposingSome.typesExposingVariants
            in
            if variantsAreExposed then
                let
                    exposedVariants : Set String
                    exposedVariants =
                        List.foldl
                            (\(Node _ variant) acc -> Set.insert (Node.value variant.name) acc)
                            context.exposedVariants
                            variantType.constructors
                in
                { context | exposedVariants = exposedVariants }

            else
                context

        Declaration.FunctionDeclaration functionDeclaration ->
            { context
                | rangesToIgnore = []
                , rightSidesOfPlusPlus = RangeDict.empty
                , inferredConstantsDict = RangeDict.empty
                , localBindings =
                    RangeDict.singleton
                        (Node.range functionDeclaration.declaration)
                        (AstHelpers.patternListBindings (Node.value functionDeclaration.declaration).arguments)
            }

        _ ->
            context



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> { config | expectNaN : Bool } -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node config context =
    let
        expressionRange : Range
        expressionRange =
            Node.range node

        maybeIfIgnoredRemainingRanges : Maybe (List Range)
        maybeIfIgnoredRemainingRanges =
            case context.rangesToIgnore of
                [] ->
                    Nothing

                rangeToIgnore :: remainingRangesToIgnore ->
                    if rangeToIgnore == expressionRange then
                        Just remainingRangesToIgnore

                    else
                        Nothing
    in
    case maybeIfIgnoredRemainingRanges of
        Just remainingRangesToIgnore ->
            ( []
            , { rangesToIgnore = remainingRangesToIgnore

              --
              , rightSidesOfPlusPlus = context.rightSidesOfPlusPlus
              , inferredConstantsDict = context.inferredConstantsDict
              , localBindings = context.localBindings
              , branchLocalBindings = context.branchLocalBindings
              , inferredConstants = context.inferredConstants
              , lookupTable = context.lookupTable
              , moduleName = context.moduleName
              , exposed = context.exposed
              , commentRanges = context.commentRanges
              , importRecordTypeAliases = context.importRecordTypeAliases
              , moduleRecordTypeAliases = context.moduleRecordTypeAliases
              , importCustomTypes = context.importCustomTypes
              , moduleCustomTypes = context.moduleCustomTypes
              , moduleBindings = context.moduleBindings
              , customTypesToReportInCases = context.customTypesToReportInCases
              , localIgnoredCustomTypes = context.localIgnoredCustomTypes
              , constructorsToIgnore = context.constructorsToIgnore
              , extractSourceCode = context.extractSourceCode
              , exposedVariants = context.exposedVariants
              , importLookup = context.importLookup
              }
            )

        Nothing ->
            let
                newInferredConstants : ( Infer.Inferred, List Infer.Inferred )
                newInferredConstants =
                    case RangeDict.get expressionRange context.inferredConstantsDict of
                        Nothing ->
                            context.inferredConstants

                        Just inferredConstants ->
                            let
                                ( previous, previousStack ) =
                                    context.inferredConstants
                            in
                            ( inferredConstants, previous :: previousStack )

                expression : Expression
                expression =
                    Node.value node

                withNewBranchLocalBindings : RangeDict (Set String)
                withNewBranchLocalBindings =
                    expressionBranchLocalBindingsInto context.branchLocalBindings expression

                contextWithInferredConstantsAndLocalBindings : ModuleContext
                contextWithInferredConstantsAndLocalBindings =
                    case RangeDict.get expressionRange context.branchLocalBindings of
                        Nothing ->
                            { localBindings =
                                RangeDict.insert expressionRange
                                    (expressionSurfaceBindingsInto Set.empty expression)
                                    context.localBindings
                            , branchLocalBindings = withNewBranchLocalBindings
                            , inferredConstants = newInferredConstants

                            --
                            , lookupTable = context.lookupTable
                            , moduleName = context.moduleName
                            , exposed = context.exposed
                            , commentRanges = context.commentRanges
                            , importRecordTypeAliases = context.importRecordTypeAliases
                            , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                            , importCustomTypes = context.importCustomTypes
                            , moduleCustomTypes = context.moduleCustomTypes
                            , moduleBindings = context.moduleBindings
                            , rangesToIgnore = context.rangesToIgnore
                            , rightSidesOfPlusPlus = context.rightSidesOfPlusPlus
                            , customTypesToReportInCases = context.customTypesToReportInCases
                            , localIgnoredCustomTypes = context.localIgnoredCustomTypes
                            , constructorsToIgnore = context.constructorsToIgnore
                            , inferredConstantsDict = context.inferredConstantsDict
                            , extractSourceCode = context.extractSourceCode
                            , exposedVariants = context.exposedVariants
                            , importLookup = context.importLookup
                            }

                        Just currentBranchLocalBindings ->
                            { localBindings =
                                RangeDict.insert expressionRange
                                    (expressionSurfaceBindingsInto currentBranchLocalBindings expression)
                                    context.localBindings
                            , branchLocalBindings =
                                RangeDict.remove expressionRange withNewBranchLocalBindings
                            , inferredConstants = newInferredConstants

                            --
                            , lookupTable = context.lookupTable
                            , moduleName = context.moduleName
                            , exposed = context.exposed
                            , commentRanges = context.commentRanges
                            , importRecordTypeAliases = context.importRecordTypeAliases
                            , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                            , importCustomTypes = context.importCustomTypes
                            , moduleCustomTypes = context.moduleCustomTypes
                            , moduleBindings = context.moduleBindings
                            , rangesToIgnore = context.rangesToIgnore
                            , rightSidesOfPlusPlus = context.rightSidesOfPlusPlus
                            , customTypesToReportInCases = context.customTypesToReportInCases
                            , localIgnoredCustomTypes = context.localIgnoredCustomTypes
                            , constructorsToIgnore = context.constructorsToIgnore
                            , inferredConstantsDict = context.inferredConstantsDict
                            , extractSourceCode = context.extractSourceCode
                            , exposedVariants = context.exposedVariants
                            , importLookup = context.importLookup
                            }

                expressionChecked : { error : Maybe (Error {}), subRangeToIgnore : Maybe Range, rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
                expressionChecked =
                    expressionVisitorHelp node config contextWithInferredConstantsAndLocalBindings
            in
            ( case expressionChecked.error of
                Nothing ->
                    []

                Just error ->
                    [ error ]
            , { rangesToIgnore =
                    case expressionChecked.subRangeToIgnore of
                        Nothing ->
                            context.rangesToIgnore

                        Just additionalRangeToIgnore ->
                            additionalRangeToIgnore :: context.rangesToIgnore
              , rightSidesOfPlusPlus = RangeDict.union expressionChecked.rightSidesOfPlusPlus context.rightSidesOfPlusPlus
              , inferredConstantsDict =
                    List.foldl (\( range, constants ) acc -> RangeDict.insert range constants acc)
                        context.inferredConstantsDict
                        expressionChecked.inferredConstants

              --
              , localBindings = contextWithInferredConstantsAndLocalBindings.localBindings
              , branchLocalBindings = contextWithInferredConstantsAndLocalBindings.branchLocalBindings
              , inferredConstants = contextWithInferredConstantsAndLocalBindings.inferredConstants
              , lookupTable = contextWithInferredConstantsAndLocalBindings.lookupTable
              , moduleName = contextWithInferredConstantsAndLocalBindings.moduleName
              , exposed = contextWithInferredConstantsAndLocalBindings.exposed
              , commentRanges = contextWithInferredConstantsAndLocalBindings.commentRanges
              , importRecordTypeAliases = contextWithInferredConstantsAndLocalBindings.importRecordTypeAliases
              , moduleRecordTypeAliases = contextWithInferredConstantsAndLocalBindings.moduleRecordTypeAliases
              , importCustomTypes = contextWithInferredConstantsAndLocalBindings.importCustomTypes
              , moduleCustomTypes = contextWithInferredConstantsAndLocalBindings.moduleCustomTypes
              , moduleBindings = contextWithInferredConstantsAndLocalBindings.moduleBindings
              , customTypesToReportInCases = contextWithInferredConstantsAndLocalBindings.customTypesToReportInCases
              , localIgnoredCustomTypes = contextWithInferredConstantsAndLocalBindings.localIgnoredCustomTypes
              , constructorsToIgnore = contextWithInferredConstantsAndLocalBindings.constructorsToIgnore
              , extractSourceCode = contextWithInferredConstantsAndLocalBindings.extractSourceCode
              , exposedVariants = contextWithInferredConstantsAndLocalBindings.exposedVariants
              , importLookup = contextWithInferredConstantsAndLocalBindings.importLookup
              }
            )


{-| From the `elm/core` readme:

>
> ### Default Imports

> The modules in this package are so common, that some of them are imported by default in all Elm files. So it is as if every Elm file starts with these imports:
>
>     import Basics exposing (..)
>     import List exposing (List, (::))
>     import Maybe exposing (Maybe(..))
>     import Result exposing (Result(..))
>     import String exposing (String)
>     import Char exposing (Char)
>     import Tuple
>     import Debug
>     import Platform exposing (Program)
>     import Platform.Cmd as Cmd exposing (Cmd)
>     import Platform.Sub as Sub exposing (Sub)

-}
implicitImports : ImportLookup
implicitImports =
    [ ( [ "Basics" ], { alias = Nothing, exposed = ExposedAll } )
    , ( [ "List" ], { alias = Nothing, exposed = ExposedSome (Set.fromList [ "List", "(::)" ]) } )
    , ( [ "Maybe" ], { alias = Nothing, exposed = ExposedSome (Set.fromList [ "Maybe", "Just", "Nothing" ]) } )
    , ( [ "Result" ], { alias = Nothing, exposed = ExposedSome (Set.fromList [ "Result", "Ok", "Err" ]) } )
    , ( [ "String" ], { alias = Nothing, exposed = ExposedSome (Set.singleton "String") } )
    , ( [ "Char" ], { alias = Nothing, exposed = ExposedSome (Set.singleton "Char") } )
    , ( [ "Tuple" ], { alias = Nothing, exposed = ExposedSome Set.empty } )
    , ( [ "Debug" ], { alias = Nothing, exposed = ExposedSome Set.empty } )
    , ( [ "Platform" ], { alias = Nothing, exposed = ExposedSome (Set.singleton "Program") } )
    , ( [ "Platform", "Cmd" ], { alias = Just [ "Cmd" ], exposed = ExposedSome (Set.singleton "Cmd") } )
    , ( [ "Platform", "Sub" ], { alias = Just [ "Sub" ], exposed = ExposedSome (Set.singleton "Sub") } )
    ]
        |> Dict.fromList


{-| Merge a given new import with an existing import lookup.
This is strongly preferred over Dict.insert since the implicit default imports can be overridden
-}
insertImport : { moduleName : ModuleName, alias : Maybe ModuleName, exposed : Exposed } -> ImportLookup -> ImportLookup
insertImport importInfoToAdd importLookup =
    Dict.insert importInfoToAdd.moduleName
        (case Dict.get importInfoToAdd.moduleName importLookup of
            Nothing ->
                { alias = importInfoToAdd.alias, exposed = importInfoToAdd.exposed }

            Just import_ ->
                { alias = import_.alias |> onNothing (\() -> importInfoToAdd.alias)
                , exposed = exposedMerge import_.exposed importInfoToAdd.exposed
                }
        )
        importLookup


exposedMerge : Exposed -> Exposed -> Exposed
exposedMerge exposedA exposedB =
    case exposedA of
        ExposedAll ->
            ExposedAll

        ExposedSome aSet ->
            case exposedB of
                ExposedAll ->
                    ExposedAll

                ExposedSome bSet ->
                    ExposedSome (Set.union aSet bSet)


qualify : ( ModuleName, String ) -> QualifyResources a -> ( ModuleName, String )
qualify ( moduleName, name ) qualifyResources =
    let
        qualification : ModuleName
        qualification =
            case qualifyResources.importLookup |> Dict.get moduleName of
                Nothing ->
                    moduleName

                Just import_ ->
                    let
                        moduleImportedName : ModuleName
                        moduleImportedName =
                            import_.alias |> Maybe.withDefault moduleName
                    in
                    if not (isExposedFrom import_.exposed name) then
                        moduleImportedName

                    else
                        let
                            isShadowed : Bool
                            isShadowed =
                                isBindingInScope qualifyResources name
                        in
                        if isShadowed then
                            moduleImportedName

                        else
                            []
    in
    ( qualification, name )


isBindingInScope :
    { a
        | moduleBindings : Set String
        , localBindings : RangeDict (Set String)
    }
    -> String
    -> Bool
isBindingInScope resources name =
    Set.member name resources.moduleBindings
        || RangeDict.any (\bindings -> Set.member name bindings) resources.localBindings


{-| Whenever you add ranges on expression enter, the same ranges should be removed on expression exit.
Having one function finding unique ranges and a function for extracting bindings there ensures said consistency.

An alternative approach would be to use some kind of tree structure
with parent and sub ranges and bindings as leaves (maybe a "trie", tho I've not seen one as an elm package).

Removing all bindings for an expression's range on leave would then be trivial

-}
expressionSurfaceBindingsInto : Set String -> Expression -> Set String
expressionSurfaceBindingsInto soFar expression =
    case expression of
        Expression.LambdaExpression lambda ->
            Set.union (AstHelpers.patternListBindings lambda.args) soFar

        Expression.LetExpression letBlock ->
            List.foldl insertLetDeclarationBindings soFar letBlock.declarations

        _ ->
            soFar


insertLetDeclarationBindings : Node Expression.LetDeclaration -> Set String -> Set String
insertLetDeclarationBindings (Node _ letDeclaration) soFar =
    case letDeclaration of
        Expression.LetFunction fun ->
            Set.insert (fun.declaration |> Node.value |> .name |> Node.value) soFar

        Expression.LetDestructuring (Node _ pattern) _ ->
            Set.union (AstHelpers.patternBindings pattern) soFar


expressionBranchLocalBindingsInto : RangeDict (Set String) -> Expression -> RangeDict (Set String)
expressionBranchLocalBindingsInto existingBindings expression =
    case expression of
        Expression.CaseExpression caseBlock ->
            List.foldl
                (\( Node _ pattern, Node resultRange _ ) soFar ->
                    soFar
                        |> RangeDict.insert resultRange
                            (AstHelpers.patternBindings pattern)
                )
                existingBindings
                caseBlock.cases

        Expression.LetExpression letBlock ->
            List.foldl
                (\(Node _ letDeclaration) acc ->
                    case letDeclaration of
                        Expression.LetFunction letFunctionOrValueDeclaration ->
                            RangeDict.insert
                                (Node.range (Node.value letFunctionOrValueDeclaration.declaration).expression)
                                (AstHelpers.patternListBindings
                                    (Node.value letFunctionOrValueDeclaration.declaration).arguments
                                )
                                acc

                        _ ->
                            acc
                )
                existingBindings
                letBlock.declarations

        _ ->
            existingBindings


expressionExitVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionExitVisitor (Node expressionRange _) context =
    { localBindings = RangeDict.remove expressionRange context.localBindings
    , inferredConstants =
        if RangeDict.member expressionRange context.inferredConstantsDict then
            case Tuple.second context.inferredConstants of
                inferredConstantsTopOfStack :: inferredConstantsRestOfStack ->
                    ( inferredConstantsTopOfStack, inferredConstantsRestOfStack )

                [] ->
                    context.inferredConstants

        else
            context.inferredConstants

    --
    , lookupTable = context.lookupTable
    , moduleName = context.moduleName
    , exposed = context.exposed
    , commentRanges = context.commentRanges
    , importRecordTypeAliases = context.importRecordTypeAliases
    , moduleRecordTypeAliases = context.moduleRecordTypeAliases
    , importCustomTypes = context.importCustomTypes
    , moduleCustomTypes = context.moduleCustomTypes
    , moduleBindings = context.moduleBindings
    , branchLocalBindings = context.branchLocalBindings
    , rangesToIgnore = context.rangesToIgnore
    , rightSidesOfPlusPlus = context.rightSidesOfPlusPlus
    , customTypesToReportInCases = context.customTypesToReportInCases
    , localIgnoredCustomTypes = context.localIgnoredCustomTypes
    , constructorsToIgnore = context.constructorsToIgnore
    , inferredConstantsDict = context.inferredConstantsDict
    , extractSourceCode = context.extractSourceCode
    , exposedVariants = context.exposedVariants
    , importLookup = context.importLookup
    }


maybeErrorAndRangesToIgnore : Maybe (Error {}) -> Range -> { error : Maybe (Error {}), subRangeToIgnore : Maybe Range, rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
maybeErrorAndRangesToIgnore maybeError rangeToIgnore =
    { error = maybeError
    , subRangeToIgnore = Just rangeToIgnore
    , rightSidesOfPlusPlus = RangeDict.empty
    , inferredConstants = []
    }


onlyMaybeError : Maybe (Error {}) -> { error : Maybe (Error {}), subRangeToIgnore : Maybe Range, rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
onlyMaybeError maybeError =
    { error = maybeError
    , subRangeToIgnore = Nothing
    , rightSidesOfPlusPlus = RangeDict.empty
    , inferredConstants = []
    }


expressionVisitResultNoError : { error : Maybe (Error {}), subRangeToIgnore : Maybe Range, rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
expressionVisitResultNoError =
    { error = Nothing
    , subRangeToIgnore = Nothing
    , rightSidesOfPlusPlus = RangeDict.empty
    , inferredConstants = []
    }


expressionVisitorHelp : Node Expression -> { config | expectNaN : Bool } -> ModuleContext -> { error : Maybe (Error {}), subRangeToIgnore : Maybe Range, rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
expressionVisitorHelp (Node expressionRange expression) config context =
    case expression of
        -----------------
        -- APPLICATION --
        -----------------
        Expression.Application (applied :: firstArg :: argsAfterFirst) ->
            onlyMaybeError
                (case applied of
                    Node fnRange (Expression.FunctionOrValue _ fnName) ->
                        case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                            Just moduleName ->
                                let
                                    reference : ( ModuleName, String )
                                    reference =
                                        ( moduleName, fnName )
                                in
                                case Dict.get reference functionCallChecks of
                                    Just ( argCount, checkFn ) ->
                                        checkFn
                                            (toCallCheckInfo config
                                                context
                                                { parentRange = expressionRange
                                                , fnRange = fnRange
                                                , fn = reference
                                                , argCount = argCount
                                                , firstArg = firstArg
                                                , argsAfterFirst = argsAfterFirst
                                                , callStyle = CallStyle.Application
                                                }
                                            )

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing

                    Node lambdaWithParens (Expression.ParenthesizedExpression ((Node _ (Expression.LambdaExpression lambda)) as lambdaNode)) ->
                        appliedLambdaError
                            { lambda = lambda
                            , lambdaNode = lambdaNode
                            , lambdaWithParens = lambdaWithParens
                            , firstArgument = firstArg
                            , argsAfterFirst = argsAfterFirst
                            , lookupTable = context.lookupTable
                            , importCustomTypes = context.importCustomTypes
                            , moduleCustomTypes = context.moduleCustomTypes
                            }

                    Node parens (Expression.ParenthesizedExpression ((Node _ (Expression.OperatorApplication "<<" _ _ _)) as operationNode)) ->
                        callingCompositionChecks
                            { commentRanges = context.commentRanges
                            , extractSourceCode = context.extractSourceCode
                            , direction = CallStyle.RightToLeft
                            }
                            (\() ->
                                if List.isEmpty argsAfterFirst then
                                    Fix.insertAt expressionRange.start "("
                                        :: Fix.insertAt expressionRange.end ")"
                                        :: removeBoundariesFix applied

                                else
                                    [ Fix.removeRange
                                        { start = { row = parens.end.row, column = parens.end.column - 1 }
                                        , end = parens.end
                                        }
                                    , Fix.insertAt (Node.range firstArg).end ")"
                                    ]
                            )
                            operationNode

                    Node parens (Expression.ParenthesizedExpression ((Node _ (Expression.OperatorApplication ">>" _ _ _)) as operationNode)) ->
                        reversedCompositionChecks
                            { commentRanges = context.commentRanges
                            , extractSourceCode = context.extractSourceCode
                            , direction = CallStyle.RightToLeft
                            }
                            (\() ->
                                if List.isEmpty argsAfterFirst then
                                    Fix.insertAt expressionRange.start "("
                                        :: Fix.insertAt expressionRange.end ")"
                                        :: removeBoundariesFix applied

                                else
                                    [ Fix.removeRange
                                        { start = { row = parens.end.row, column = parens.end.column - 1 }
                                        , end = parens.end
                                        }
                                    , Fix.insertAt (Node.range firstArg).end ")"
                                    ]
                            )
                            operationNode

                    Node operatorRange (Expression.PrefixOperator operator) ->
                        (if operator == "==" then
                            comparisonWithEmptyCheckInPrefixOperator
                                context.lookupTable
                                operatorRange
                                firstArg

                         else
                            Nothing
                        )
                            |> onNothing
                                (\() ->
                                    case argsAfterFirst of
                                        [ right ] ->
                                            Just
                                                (fullyAppliedPrefixOperatorError
                                                    { operator = operator
                                                    , operatorRange = operatorRange
                                                    , left = firstArg
                                                    , right = right
                                                    }
                                                )

                                        _ ->
                                            Nothing
                                )

                    otherApplied ->
                        case AstHelpers.getRecordAccessFunction otherApplied of
                            Just fieldName ->
                                accessingRecordChecks
                                    { parentRange = Range.combine [ Node.range applied, Node.range firstArg ]
                                    , record = firstArg
                                    , fieldRange = Node.range otherApplied
                                    , fieldName = fieldName
                                    , importRecordTypeAliases = context.importRecordTypeAliases
                                    , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                                    , importCustomTypes = context.importCustomTypes
                                    , moduleCustomTypes = context.moduleCustomTypes
                                    , lookupTable = context.lookupTable
                                    }
                                    |> Maybe.map (\e -> Rule.errorWithFix e.info (Node.range otherApplied) e.fix)

                            Nothing ->
                                Nothing
                )

        ----------
        -- (<|) --
        ----------
        Expression.OperatorApplication "<|" _ pipedInto lastArg ->
            case pipedInto of
                Node fnRange (Expression.FunctionOrValue _ fnName) ->
                    onlyMaybeError
                        (case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                            Just moduleName ->
                                let
                                    reference : ( ModuleName, String )
                                    reference =
                                        ( moduleName, fnName )
                                in
                                case Dict.get reference functionCallChecks of
                                    Just ( argCount, checkFn ) ->
                                        checkFn
                                            (toCallCheckInfo config
                                                context
                                                { parentRange = expressionRange
                                                , fnRange = fnRange
                                                , fn = reference
                                                , argCount = argCount
                                                , firstArg = lastArg
                                                , argsAfterFirst = []
                                                , callStyle = CallStyle.pipeRightToLeft
                                                }
                                            )

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing
                        )

                Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: argsBetweenFirstAndLast)) ->
                    case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                        Just moduleName ->
                            let
                                reference : ( ModuleName, String )
                                reference =
                                    ( moduleName, fnName )
                            in
                            case Dict.get reference functionCallChecks of
                                Just ( argCount, checkFn ) ->
                                    maybeErrorAndRangesToIgnore
                                        (checkFn
                                            (toCallCheckInfo config
                                                context
                                                { parentRange = expressionRange
                                                , fnRange = fnRange
                                                , argCount = argCount
                                                , fn = reference
                                                , firstArg = firstArg
                                                , argsAfterFirst = argsBetweenFirstAndLast ++ [ lastArg ]
                                                , callStyle = CallStyle.pipeRightToLeft
                                                }
                                            )
                                        )
                                        applicationRange

                                Nothing ->
                                    expressionVisitResultNoError

                        Nothing ->
                            expressionVisitResultNoError

                pipedIntoOther ->
                    onlyMaybeError
                        (pipelineChecks
                            { commentRanges = context.commentRanges
                            , extractSourceCode = context.extractSourceCode
                            , direction = CallStyle.RightToLeft
                            , nodeRange = expressionRange
                            , pipedInto = pipedIntoOther
                            , arg = lastArg
                            , importRecordTypeAliases = context.importRecordTypeAliases
                            , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                            , importCustomTypes = context.importCustomTypes
                            , moduleCustomTypes = context.moduleCustomTypes
                            , lookupTable = context.lookupTable
                            }
                        )

        ----------
        -- (|>) --
        ----------
        Expression.OperatorApplication "|>" _ lastArg pipedInto ->
            case pipedInto of
                Node fnRange (Expression.FunctionOrValue _ fnName) ->
                    case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                        Just moduleName ->
                            let
                                reference : ( ModuleName, String )
                                reference =
                                    ( moduleName, fnName )
                            in
                            case Dict.get reference functionCallChecks of
                                Just ( argCount, checks ) ->
                                    onlyMaybeError
                                        (checks
                                            (toCallCheckInfo config
                                                context
                                                { parentRange = expressionRange
                                                , fnRange = fnRange
                                                , fn = reference
                                                , argCount = argCount
                                                , firstArg = lastArg
                                                , argsAfterFirst = []
                                                , callStyle = CallStyle.pipeLeftToRight
                                                }
                                            )
                                        )

                                Nothing ->
                                    expressionVisitResultNoError

                        Nothing ->
                            expressionVisitResultNoError

                Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: argsBetweenFirstAndLast)) ->
                    case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                        Just moduleName ->
                            let
                                reference : ( ModuleName, String )
                                reference =
                                    ( moduleName, fnName )
                            in
                            case Dict.get reference functionCallChecks of
                                Just ( argCount, checks ) ->
                                    maybeErrorAndRangesToIgnore
                                        (checks
                                            (toCallCheckInfo config
                                                context
                                                { parentRange = expressionRange
                                                , fnRange = fnRange
                                                , fn = reference
                                                , argCount = argCount
                                                , firstArg = firstArg
                                                , argsAfterFirst = argsBetweenFirstAndLast ++ [ lastArg ]
                                                , callStyle = CallStyle.pipeLeftToRight
                                                }
                                            )
                                        )
                                        applicationRange

                                Nothing ->
                                    expressionVisitResultNoError

                        Nothing ->
                            expressionVisitResultNoError

                pipedIntoOther ->
                    onlyMaybeError
                        (pipelineChecks
                            { commentRanges = context.commentRanges
                            , extractSourceCode = context.extractSourceCode
                            , direction = CallStyle.LeftToRight
                            , nodeRange = expressionRange
                            , pipedInto = pipedIntoOther
                            , arg = lastArg
                            , importRecordTypeAliases = context.importRecordTypeAliases
                            , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                            , importCustomTypes = context.importCustomTypes
                            , moduleCustomTypes = context.moduleCustomTypes
                            , lookupTable = context.lookupTable
                            }
                        )

        ----------
        -- (>>) --
        ----------
        Expression.OperatorApplication ">>" _ earlier composedLater ->
            onlyMaybeError
                (compositionChecks
                    (toCompositionCheckInfo config context { earlier = earlier, later = composedLater })
                )

        ----------
        -- (<<) --
        ----------
        Expression.OperatorApplication "<<" _ composedLater earlier ->
            onlyMaybeError
                (compositionChecks
                    (toCompositionCheckInfo config context { earlier = earlier, later = composedLater })
                )

        ---------------------
        -- OTHER OPERATION --
        ---------------------
        Expression.OperatorApplication operator _ left right ->
            { error =
                let
                    leftRange : Range
                    leftRange =
                        Node.range left

                    rightRange : Range
                    rightRange =
                        Node.range right
                in
                operatorApplicationChecks operator
                    { lookupTable = context.lookupTable
                    , extractSourceCode = context.extractSourceCode
                    , expectNaN = config.expectNaN
                    , importLookup = context.importLookup
                    , moduleCustomTypes = context.moduleCustomTypes
                    , importCustomTypes = context.importCustomTypes
                    , moduleBindings = context.moduleBindings
                    , localBindings = context.localBindings
                    , inferredConstants = context.inferredConstants
                    , parentRange = expressionRange
                    , operator = operator
                    , operatorRange =
                        findOperatorRange
                            { operator = operator
                            , commentRanges = context.commentRanges
                            , extractSourceCode = context.extractSourceCode
                            , leftRange = leftRange
                            , rightRange = rightRange
                            }
                    , left = left
                    , leftRange = leftRange
                    , right = right
                    , rightRange = rightRange
                    , isOnTheRightSideOfPlusPlus = RangeDict.member expressionRange context.rightSidesOfPlusPlus
                    }
            , subRangeToIgnore = Nothing
            , rightSidesOfPlusPlus =
                case operator of
                    "++" ->
                        RangeDict.singleton (Node.range (AstHelpers.removeParens right)) ()

                    _ ->
                        RangeDict.empty
            , inferredConstants = []
            }

        --------------
        -- NEGATION --
        --------------
        Expression.Negation negatedExpression ->
            onlyMaybeError
                (negationChecks { parentRange = expressionRange, negatedExpression = negatedExpression })

        -------------------
        -- RECORD ACCESS --
        -------------------
        Expression.RecordAccess record (Node fieldRange fieldName) ->
            onlyMaybeError
                (accessingRecordChecks
                    { parentRange = expressionRange
                    , record = record
                    , fieldRange = fieldRange
                    , fieldName = fieldName
                    , importRecordTypeAliases = context.importRecordTypeAliases
                    , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                    , importCustomTypes = context.importCustomTypes
                    , moduleCustomTypes = context.moduleCustomTypes
                    , lookupTable = context.lookupTable
                    }
                    |> Maybe.map (\e -> Rule.errorWithFix e.info { start = (Node.range record).end, end = fieldRange.end } e.fix)
                )

        --------
        -- IF --
        --------
        Expression.IfBlock condition trueBranch falseBranch ->
            let
                ifCheckInfo : IfCheckInfo
                ifCheckInfo =
                    { nodeRange = expressionRange
                    , condition = condition
                    , trueBranch = trueBranch
                    , falseBranch = falseBranch
                    , lookupTable = context.lookupTable
                    , inferredConstants = context.inferredConstants
                    , importLookup = context.importLookup
                    , moduleCustomTypes = context.moduleCustomTypes
                    , importCustomTypes = context.importCustomTypes
                    , moduleBindings = context.moduleBindings
                    , localBindings = context.localBindings
                    }
            in
            case ifChecks ifCheckInfo of
                Just ifErrors ->
                    onlyMaybeError (Just ifErrors)

                Nothing ->
                    { error = Nothing
                    , subRangeToIgnore = Nothing
                    , rightSidesOfPlusPlus = RangeDict.empty
                    , inferredConstants =
                        Infer.inferForIfCondition
                            (Normalize.normalizeExpression context condition)
                            { trueBranchRange = Node.range trueBranch
                            , falseBranchRange = Node.range falseBranch
                            }
                            (Tuple.first context.inferredConstants)
                    }

        -------------
        -- CASE OF --
        -------------
        Expression.CaseExpression caseBlock ->
            onlyMaybeError
                (caseOfChecks
                    { lookupTable = context.lookupTable
                    , importLookup = context.importLookup
                    , moduleCustomTypes = context.moduleCustomTypes
                    , importCustomTypes = context.importCustomTypes
                    , extractSourceCode = context.extractSourceCode
                    , customTypesToReportInCases = context.customTypesToReportInCases
                    , inferredConstants = context.inferredConstants
                    , moduleBindings = context.moduleBindings
                    , localBindings = context.localBindings
                    , parentRange = expressionRange
                    , caseOf = caseBlock
                    }
                )

        ------------
        -- LET IN --
        ------------
        Expression.LetExpression caseBlock ->
            onlyMaybeError (letInChecks caseBlock)

        -------------------
        -- RECORD UPDATE --
        -------------------
        Expression.RecordUpdateExpression variable fields ->
            onlyMaybeError (recordUpdateChecks expressionRange variable fields)

        --------------------
        -- NOT SIMPLIFIED --
        --------------------
        Expression.UnitExpr ->
            expressionVisitResultNoError

        Expression.CharLiteral _ ->
            expressionVisitResultNoError

        Expression.Integer _ ->
            expressionVisitResultNoError

        Expression.Hex _ ->
            expressionVisitResultNoError

        Expression.Floatable _ ->
            expressionVisitResultNoError

        Expression.Literal _ ->
            expressionVisitResultNoError

        Expression.GLSLExpression _ ->
            expressionVisitResultNoError

        Expression.PrefixOperator _ ->
            expressionVisitResultNoError

        Expression.RecordAccessFunction _ ->
            expressionVisitResultNoError

        Expression.FunctionOrValue _ _ ->
            expressionVisitResultNoError

        Expression.ParenthesizedExpression _ ->
            expressionVisitResultNoError

        Expression.TupledExpression _ ->
            expressionVisitResultNoError

        Expression.ListExpr _ ->
            expressionVisitResultNoError

        Expression.RecordExpr _ ->
            expressionVisitResultNoError

        Expression.LambdaExpression _ ->
            expressionVisitResultNoError

        ----------------------
        -- IMPOSSIBLE CASES --
        ----------------------
        Expression.Operator _ ->
            expressionVisitResultNoError

        Expression.Application [] ->
            expressionVisitResultNoError

        Expression.Application (_ :: []) ->
            expressionVisitResultNoError


toCallCheckInfo :
    { config | expectNaN : Bool }
    -> ModuleContext
    ->
        { parentRange : Range
        , fnRange : Range
        , fn : ( ModuleName, String )
        , argCount : Int
        , firstArg : Node Expression
        , argsAfterFirst : List (Node Expression)
        , callStyle : FunctionCallStyle
        }
    -> CallCheckInfo
toCallCheckInfo config context checkInfo =
    case List.drop (checkInfo.argCount - 1) (checkInfo.firstArg :: checkInfo.argsAfterFirst) of
        lastExpectedArg :: _ :: _ ->
            -- Too many arguments!
            -- We'll update the range to drop the extra ones and force the call style to application
            { parentRange =
                case checkInfo.callStyle of
                    CallStyle.Application ->
                        { start = checkInfo.fnRange.start, end = (Node.range lastExpectedArg).end }

                    CallStyle.Pipe CallStyle.LeftToRight ->
                        { start = checkInfo.fnRange.start, end = (Node.range lastExpectedArg).end }

                    CallStyle.Pipe CallStyle.RightToLeft ->
                        { start = (Node.range checkInfo.firstArg).start, end = (Node.range checkInfo.firstArg).end }
            , argsAfterFirst = List.take (checkInfo.argCount - 1) checkInfo.argsAfterFirst
            , callStyle = CallStyle.Application
            , fnRange = checkInfo.fnRange
            , fn = checkInfo.fn
            , argCount = checkInfo.argCount
            , firstArg = checkInfo.firstArg
            , lookupTable = context.lookupTable
            , expectNaN = config.expectNaN
            , extractSourceCode = context.extractSourceCode
            , importLookup = context.importLookup
            , moduleCustomTypes = context.moduleCustomTypes
            , importCustomTypes = context.importCustomTypes
            , moduleBindings = context.moduleBindings
            , localBindings = context.localBindings
            , commentRanges = context.commentRanges
            , inferredConstants = context.inferredConstants
            }

        -- [] | [ _ ] ->
        _ ->
            { parentRange = checkInfo.parentRange
            , fnRange = checkInfo.fnRange
            , fn = checkInfo.fn
            , argCount = checkInfo.argCount
            , firstArg = checkInfo.firstArg
            , argsAfterFirst = checkInfo.argsAfterFirst
            , callStyle = checkInfo.callStyle
            , lookupTable = context.lookupTable
            , expectNaN = config.expectNaN
            , extractSourceCode = context.extractSourceCode
            , importLookup = context.importLookup
            , moduleCustomTypes = context.moduleCustomTypes
            , importCustomTypes = context.importCustomTypes
            , moduleBindings = context.moduleBindings
            , localBindings = context.localBindings
            , commentRanges = context.commentRanges
            , inferredConstants = context.inferredConstants
            }


toCompositionCheckInfo :
    { config | expectNaN : Bool }
    -> ModuleContext
    ->
        { earlier : Node Expression
        , later : Node Expression
        }
    -> CompositionCheckInfo
toCompositionCheckInfo config context compositionSpecific =
    let
        innerComposition :
            { earlier :
                { node : Node Expression, removeRange : Range }
            , later :
                { node : Node Expression, removeRange : Range }
            , isEmbeddedInComposition : Bool
            }
        innerComposition =
            getInnerComposition compositionSpecific
    in
    { expectNaN = config.expectNaN
    , lookupTable = context.lookupTable
    , importLookup = context.importLookup
    , importRecordTypeAliases = context.importRecordTypeAliases
    , moduleRecordTypeAliases = context.moduleRecordTypeAliases
    , moduleCustomTypes = context.moduleCustomTypes
    , importCustomTypes = context.importCustomTypes
    , moduleBindings = context.moduleBindings
    , localBindings = context.localBindings
    , extractSourceCode = context.extractSourceCode
    , inferredConstants = context.inferredConstants
    , earlier = innerComposition.earlier
    , later = innerComposition.later
    , isEmbeddedInComposition = innerComposition.isEmbeddedInComposition
    }


type alias OperatorApplicationCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , extractSourceCode : Range -> String
    , expectNaN : Bool
    , importLookup : ImportLookup
    , importCustomTypes :
        Dict
            ModuleName
            (Dict
                String
                { variantNames : Set String
                , allParametersAreUsedInVariants : Bool
                }
            )
    , moduleCustomTypes :
        Dict
            String
            { variantNames : Set String
            , allParametersAreUsedInVariants : Bool
            }
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , parentRange : Range
    , operator : String
    , operatorRange : Range
    , left : Node Expression
    , leftRange : Range
    , right : Node Expression
    , rightRange : Range
    , isOnTheRightSideOfPlusPlus : Bool
    }


operatorApplicationChecks : String -> OperatorApplicationCheckInfo -> Maybe (Error {})
operatorApplicationChecks operator checkInfo =
    case operator of
        "+" ->
            plusChecks checkInfo

        "-" ->
            minusChecks checkInfo

        "*" ->
            multiplyChecks checkInfo

        "/" ->
            divisionChecks checkInfo

        "//" ->
            intDivideChecks checkInfo

        "++" ->
            plusplusChecks checkInfo

        "::" ->
            consChecks checkInfo

        "||" ->
            orChecks checkInfo

        "&&" ->
            andChecks checkInfo

        "==" ->
            equalityChecks True checkInfo

        "/=" ->
            equalityChecks False checkInfo

        "<" ->
            lessThanChecks checkInfo

        ">" ->
            greaterThanChecks checkInfo

        "<=" ->
            lessThanOrEqualToChecks checkInfo

        ">=" ->
            greaterThanOrEqualToChecks checkInfo

        _ ->
            Nothing


type alias CallCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , expectNaN : Bool
    , importLookup : ImportLookup
    , importCustomTypes :
        Dict
            ModuleName
            (Dict
                String
                { variantNames : Set String
                , allParametersAreUsedInVariants : Bool
                }
            )
    , moduleCustomTypes :
        Dict
            String
            { variantNames : Set String
            , allParametersAreUsedInVariants : Bool
            }
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , extractSourceCode : Range -> String
    , commentRanges : List Range
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , parentRange : Range
    , fnRange : Range
    , fn : ( ModuleName, String )
    , argCount : Int
    , callStyle : FunctionCallStyle
    , firstArg : Node Expression
    , argsAfterFirst : List (Node Expression)
    }


secondArg : CallCheckInfo -> Maybe (Node Expression)
secondArg checkInfo =
    List.head checkInfo.argsAfterFirst


thirdArg : CallCheckInfo -> Maybe (Node Expression)
thirdArg checkInfo =
    case checkInfo.argsAfterFirst of
        _ :: thirdArgument :: _ ->
            Just thirdArgument

        _ ->
            Nothing


type alias CompositionIntoCheckInfo =
    { expectNaN : Bool
    , lookupTable : ModuleNameLookupTable
    , importLookup : ImportLookup
    , importCustomTypes :
        Dict
            ModuleName
            (Dict
                String
                { variantNames : Set String
                , allParametersAreUsedInVariants : Bool
                }
            )
    , moduleCustomTypes :
        Dict
            String
            { variantNames : Set String
            , allParametersAreUsedInVariants : Bool
            }
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , extractSourceCode : Range -> String
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , later :
        { range : Range
        , fn : ( ModuleName, String )
        , fnRange : Range
        , args : List (Node Expression)
        , -- how many arguments a fully applied call would have
          argCount : Int
        , removeRange : Range
        }
    , earlier :
        { range : Range
        , fn : ( ModuleName, String )
        , fnRange : Range
        , args : List (Node Expression)
        , removeRange : Range
        }
    , isEmbeddedInComposition : Bool
    }


type alias ErrorInfoAndFix =
    { info : { message : String, details : List String }
    , fix : List Fix
    }


compositionCheckInfoDirection :
    { composition | earlier : { earlier | range : Range }, later : { later | range : Range } }
    -> CallStyle.LeftOrRightDirection
compositionCheckInfoDirection checkInfo =
    case Range.compareLocations checkInfo.earlier.range.start checkInfo.later.range.start of
        LT ->
            CallStyle.LeftToRight

        _ ->
            CallStyle.RightToLeft


{-| Checking both the function call of and composition into a specific fn.

Construct the record directly or use `intoFnCheckOnlyCall`/`intoFnCheckOnlyComposition`.
Provide multiple checks using `intoFnChecksFirstThatConstructsError`.

-}
type alias IntoFnCheck =
    { call : CallCheckInfo -> Maybe (Error {})
    , composition : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
    }


{-| There is no equivalent composition check.
-}
intoFnCheckOnlyCall : (CallCheckInfo -> Maybe (Error {})) -> IntoFnCheck
intoFnCheckOnlyCall callFnCheck =
    { call = callFnCheck, composition = \_ -> Nothing }


{-| There is no equivalent call check.
-}
intoFnCheckOnlyComposition : (CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix) -> IntoFnCheck
intoFnCheckOnlyComposition compositionIntoFnCheck =
    { composition = compositionIntoFnCheck, call = \_ -> Nothing }


{-| Try the given `IntoFnCheck`s in order and report the first found error.
-}
intoFnChecksFirstThatConstructsError : List IntoFnCheck -> IntoFnCheck
intoFnChecksFirstThatConstructsError intoFnCheckList =
    { call =
        \checkInfo ->
            findMap (\fnCheck -> fnCheck.call checkInfo) intoFnCheckList
    , composition =
        \checkInfo ->
            findMap (\fnCheck -> fnCheck.composition checkInfo) intoFnCheckList
    }


intoFnCheckOnlyWhenExpectNaNIsNotEnabled : IntoFnCheck -> IntoFnCheck
intoFnCheckOnlyWhenExpectNaNIsNotEnabled intoFnCheck =
    { call =
        \checkInfo ->
            if checkInfo.expectNaN then
                Nothing

            else
                intoFnCheck.call checkInfo
    , composition =
        \checkInfo ->
            if checkInfo.expectNaN then
                Nothing

            else
                intoFnCheck.composition checkInfo
    }


intoFnChecks : List ( ( ModuleName, String ), ( Int, IntoFnCheck ) )
intoFnChecks =
    -- The number of arguments is used to determine how many arguments to pass to the check function.
    -- This corresponds to the number of arguments that the function to check is expected to have.
    -- Any additional arguments will be ignored in order to avoid removing too many arguments
    -- when replacing the entire argument, which is quite common.
    [ ( Fn.Basics.identity, ( 1, basicsIdentityChecks ) )
    , ( Fn.Basics.always, ( 2, basicsAlwaysChecks ) )
    , ( Fn.Basics.not, ( 1, basicsNotChecks ) )
    , ( Fn.Basics.negate, ( 1, basicsNegateChecks ) )
    , ( Fn.Basics.toFloat, ( 1, basicsToFloatChecks ) )
    , ( Fn.Basics.round, ( 1, floatToIntConversionChecks Basics.round ) )
    , ( Fn.Basics.ceiling, ( 1, floatToIntConversionChecks Basics.ceiling ) )
    , ( Fn.Basics.floor, ( 1, floatToIntConversionChecks Basics.floor ) )
    , ( Fn.Basics.truncate, ( 1, floatToIntConversionChecks Basics.truncate ) )
    , ( Fn.Basics.abs, ( 1, basicsAbsChecks ) )
    , ( Fn.Basics.min, ( 2, basicsMinChecks ) )
    , ( Fn.Basics.max, ( 2, basicsMaxChecks ) )
    , ( Fn.Basics.compare, ( 2, basicsCompareChecks ) )
    , ( Fn.Tuple.first, ( 1, tupleFirstChecks ) )
    , ( Fn.Tuple.second, ( 1, tupleSecondChecks ) )
    , ( Fn.Tuple.pair, ( 2, tuplePairChecks ) )
    , ( Fn.Maybe.map, ( 2, maybeMapChecks ) )
    , ( Fn.Maybe.map2, ( 3, maybeMapNChecks ) )
    , ( Fn.Maybe.map3, ( 4, maybeMapNChecks ) )
    , ( Fn.Maybe.map4, ( 5, maybeMapNChecks ) )
    , ( Fn.Maybe.map5, ( 6, maybeMapNChecks ) )
    , ( Fn.Maybe.andThen, ( 2, maybeAndThenChecks ) )
    , ( Fn.Maybe.withDefault, ( 2, maybeWithDefaultChecks ) )
    , ( Fn.Result.map, ( 2, resultMapChecks ) )
    , ( Fn.Result.map2, ( 3, resultMapNChecks ) )
    , ( Fn.Result.map3, ( 4, resultMapNChecks ) )
    , ( Fn.Result.map4, ( 5, resultMapNChecks ) )
    , ( Fn.Result.map5, ( 6, resultMapNChecks ) )
    , ( Fn.Result.mapError, ( 2, resultMapErrorChecks ) )
    , ( Fn.Result.andThen, ( 2, resultAndThenChecks ) )
    , ( Fn.Result.withDefault, ( 2, resultWithDefaultChecks ) )
    , ( Fn.Result.toMaybe, ( 1, resultToMaybeChecks ) )
    , ( Fn.Result.fromMaybe, ( 2, resultFromMaybeWithEmptyValueOnNothingCheck ) )
    , ( Fn.List.append, ( 2, listAppendChecks ) )
    , ( Fn.List.head, ( 1, listHeadChecks ) )
    , ( Fn.List.tail, ( 1, listTailChecks ) )
    , ( Fn.List.member, ( 2, listMemberChecks ) )
    , ( Fn.List.map, ( 2, listMapChecks ) )
    , ( Fn.List.filter, ( 2, listFilterChecks ) )
    , ( Fn.List.filterMap, ( 2, listFilterMapChecks ) )
    , ( Fn.List.concat, ( 1, listConcatChecks ) )
    , ( Fn.List.concatMap, ( 2, listConcatMapChecks ) )
    , ( Fn.List.indexedMap, ( 2, listIndexedMapChecks ) )
    , ( Fn.List.intersperse, ( 2, listIntersperseChecks ) )
    , ( Fn.List.sum, ( 1, listSumChecks ) )
    , ( Fn.List.product, ( 1, listProductChecks ) )
    , ( Fn.List.minimum, ( 1, listMinimumChecks ) )
    , ( Fn.List.maximum, ( 1, listMaximumChecks ) )
    , ( Fn.List.foldl, ( 3, listFoldlChecks ) )
    , ( Fn.List.foldr, ( 3, listFoldrChecks ) )
    , ( Fn.List.all, ( 2, listAllChecks ) )
    , ( Fn.List.any, ( 2, listAnyChecks ) )
    , ( Fn.List.range, ( 2, listRangeChecks ) )
    , ( Fn.List.length, ( 1, listLengthChecks ) )
    , ( Fn.List.repeat, ( 2, listRepeatChecks ) )
    , ( Fn.List.isEmpty, ( 1, listIsEmptyChecks ) )
    , ( Fn.List.partition, ( 2, listPartitionChecks ) )
    , ( Fn.List.reverse, ( 1, listReverseChecks ) )
    , ( Fn.List.sort, ( 1, listSortChecks ) )
    , ( Fn.List.sortBy, ( 2, listSortByChecks ) )
    , ( Fn.List.sortWith, ( 2, listSortWithChecks ) )
    , ( Fn.List.take, ( 2, listTakeChecks ) )
    , ( Fn.List.drop, ( 2, listDropChecks ) )
    , ( Fn.List.map2, ( 3, listMapNChecks ) )
    , ( Fn.List.map3, ( 4, listMapNChecks ) )
    , ( Fn.List.map4, ( 5, listMapNChecks ) )
    , ( Fn.List.map5, ( 6, listMapNChecks ) )
    , ( Fn.List.unzip, ( 1, listUnzipChecks ) )
    , ( Fn.Array.toList, ( 1, arrayToListChecks ) )
    , ( Fn.Array.toIndexedList, ( 1, arrayToIndexedListChecks ) )
    , ( Fn.Array.fromList, ( 1, arrayFromListChecks ) )
    , ( Fn.Array.map, ( 2, arrayMapChecks ) )
    , ( Fn.Array.indexedMap, ( 2, arrayIndexedMapChecks ) )
    , ( Fn.Array.filter, ( 2, arrayFilterChecks ) )
    , ( Fn.Array.isEmpty, ( 1, arrayIsEmptyChecks ) )
    , ( Fn.Array.length, ( 1, arrayLengthChecks ) )
    , ( Fn.Array.repeat, ( 2, arrayRepeatChecks ) )
    , ( Fn.Array.initialize, ( 2, arrayInitializeChecks ) )
    , ( Fn.Array.append, ( 2, arrayAppendChecks ) )
    , ( Fn.Array.get, ( 2, arrayGetChecks ) )
    , ( Fn.Array.set, ( 3, arraySetChecks ) )
    , ( Fn.Array.slice, ( 3, arraySliceChecks ) )
    , ( Fn.Array.foldl, ( 3, arrayFoldlChecks ) )
    , ( Fn.Array.foldr, ( 3, arrayFoldrChecks ) )
    , ( Fn.Set.map, ( 2, setMapChecks ) )
    , ( Fn.Set.filter, ( 2, setFilterChecks ) )
    , ( Fn.Set.remove, ( 2, setRemoveChecks ) )
    , ( Fn.Set.isEmpty, ( 1, setIsEmptyChecks ) )
    , ( Fn.Set.size, ( 1, setSizeChecks ) )
    , ( Fn.Set.member, ( 2, setMemberChecks ) )
    , ( Fn.Set.fromList, ( 1, setFromListChecks ) )
    , ( Fn.Set.toList, ( 1, setToListChecks ) )
    , ( Fn.Set.partition, ( 2, setPartitionChecks ) )
    , ( Fn.Set.intersect, ( 2, setIntersectChecks ) )
    , ( Fn.Set.diff, ( 2, setDiffChecks ) )
    , ( Fn.Set.union, ( 2, setUnionChecks ) )
    , ( Fn.Set.insert, ( 2, setInsertChecks ) )
    , ( Fn.Set.foldl, ( 3, setFoldlChecks ) )
    , ( Fn.Set.foldr, ( 3, setFoldrChecks ) )
    , ( Fn.Dict.isEmpty, ( 1, dictIsEmptyChecks ) )
    , ( Fn.Dict.fromList, ( 1, dictFromListChecks ) )
    , ( Fn.Dict.toList, ( 1, dictToListChecks ) )
    , ( Fn.Dict.size, ( 1, dictSizeChecks ) )
    , ( Fn.Dict.member, ( 2, dictMemberChecks ) )
    , ( Fn.Dict.insert, ( 3, dictInsertChecks ) )
    , ( Fn.Dict.remove, ( 2, dictRemoveChecks ) )
    , ( Fn.Dict.update, ( 3, dictUpdateChecks ) )
    , ( Fn.Dict.filter, ( 2, dictFilterChecks ) )
    , ( Fn.Dict.partition, ( 2, dictPartitionChecks ) )
    , ( Fn.Dict.map, ( 2, dictMapChecks ) )
    , ( Fn.Dict.intersect, ( 2, dictIntersectChecks ) )
    , ( Fn.Dict.diff, ( 2, dictDiffChecks ) )
    , ( Fn.Dict.union, ( 2, dictUnionChecks ) )
    , ( Fn.Dict.foldl, ( 3, dictFoldlChecks ) )
    , ( Fn.Dict.foldr, ( 3, dictFoldrChecks ) )
    , ( Fn.String.toList, ( 1, stringToListChecks ) )
    , ( Fn.String.fromList, ( 1, stringFromListChecks ) )
    , ( Fn.String.fromInt, ( 1, stringFromIntChecks ) )
    , ( Fn.String.fromFloat, ( 1, stringFromFloatChecks ) )
    , ( Fn.String.isEmpty, ( 1, stringIsEmptyChecks ) )
    , ( Fn.String.concat, ( 1, stringConcatChecks ) )
    , ( Fn.String.join, ( 2, stringJoinChecks ) )
    , ( Fn.String.length, ( 1, stringLengthChecks ) )
    , ( Fn.String.repeat, ( 2, stringRepeatChecks ) )
    , ( Fn.String.replace, ( 3, stringReplaceChecks ) )
    , ( Fn.String.words, ( 1, stringWordsChecks ) )
    , ( Fn.String.lines, ( 1, stringLinesChecks ) )
    , ( Fn.String.toLower, ( 1, stringToLowerChecks ) )
    , ( Fn.String.toUpper, ( 1, stringToUpperChecks ) )
    , ( Fn.String.reverse, ( 1, stringReverseChecks ) )
    , ( Fn.String.trimLeft, ( 1, stringTrimLeftChecks ) )
    , ( Fn.String.trimRight, ( 1, stringTrimRightChecks ) )
    , ( Fn.String.trim, ( 1, stringTrimChecks ) )
    , ( Fn.String.slice, ( 3, stringSliceChecks ) )
    , ( Fn.String.left, ( 2, stringLeftChecks ) )
    , ( Fn.String.right, ( 2, stringRightChecks ) )
    , ( Fn.String.dropLeft, ( 2, stringDropLeftChecks ) )
    , ( Fn.String.dropRight, ( 2, stringDropRightChecks ) )
    , ( Fn.String.filter, ( 2, stringFilterChecks ) )
    , ( Fn.String.map, ( 2, stringMapChecks ) )
    , ( Fn.String.append, ( 2, stringAppendChecks ) )
    , ( Fn.String.uncons, ( 1, stringUnconsChecks ) )
    , ( Fn.String.any, ( 2, stringAnyChecks ) )
    , ( Fn.String.all, ( 2, stringAllChecks ) )
    , ( Fn.String.foldl, ( 3, stringFoldlChecks ) )
    , ( Fn.String.foldr, ( 3, stringFoldrChecks ) )
    , ( Fn.Platform.Cmd.batch, ( 1, platformCmdBatchChecks ) )
    , ( Fn.Platform.Cmd.map, ( 2, platformCmdMapChecks ) )
    , ( Fn.Platform.Sub.batch, ( 1, platformSubBatchChecks ) )
    , ( Fn.Platform.Sub.map, ( 2, platformSubChecks ) )
    , ( Fn.Task.map, ( 2, taskMapChecks ) )
    , ( Fn.Task.map2, ( 3, taskMapNChecks ) )
    , ( Fn.Task.map3, ( 4, taskMapNChecks ) )
    , ( Fn.Task.map4, ( 5, taskMapNChecks ) )
    , ( Fn.Task.map5, ( 6, taskMapNChecks ) )
    , ( Fn.Task.andThen, ( 2, taskAndThenChecks ) )
    , ( Fn.Task.mapError, ( 2, taskMapErrorChecks ) )
    , ( Fn.Task.onError, ( 2, taskOnErrorChecks ) )
    , ( Fn.Task.sequence, ( 1, taskSequenceChecks ) )
    , ( Fn.Task.perform, ( 2, taskPerformChecks ) )
    , ( Fn.Task.attempt, ( 2, taskAttemptChecks ) )
    , ( Fn.Json.Encode.list, ( 2, jsonEncodeListChecks ) )
    , ( Fn.Json.Encode.array, ( 2, jsonEncodeArrayChecks ) )
    , ( Fn.Json.Encode.set, ( 2, jsonEncodeSetChecks ) )
    , ( Fn.Json.Decode.oneOf, ( 1, jsonDecodeOneOfChecks ) )
    , ( Fn.Json.Decode.map, ( 2, jsonDecodeMapChecks ) )
    , ( Fn.Json.Decode.map2, ( 3, jsonDecodeMapNChecks ) )
    , ( Fn.Json.Decode.map3, ( 4, jsonDecodeMapNChecks ) )
    , ( Fn.Json.Decode.map4, ( 5, jsonDecodeMapNChecks ) )
    , ( Fn.Json.Decode.map5, ( 6, jsonDecodeMapNChecks ) )
    , ( Fn.Json.Decode.map6, ( 7, jsonDecodeMapNChecks ) )
    , ( Fn.Json.Decode.map7, ( 8, jsonDecodeMapNChecks ) )
    , ( Fn.Json.Decode.map8, ( 9, jsonDecodeMapNChecks ) )
    , ( Fn.Json.Decode.andThen, ( 2, jsonDecodeAndThenChecks ) )
    , ( Fn.Html.Attributes.classList, ( 1, htmlAttributesClassListChecks ) )
    , ( Fn.Parser.oneOf, ( 1, parserOneOfChecks ) )
    , ( Fn.Parser.Advanced.oneOf, ( 1, parserAdvancedOneOfChecks ) )
    , ( Fn.Random.uniform, ( 2, randomUniformChecks ) )
    , ( Fn.Random.weighted, ( 2, randomWeightedChecks ) )
    , ( Fn.Random.list, ( 2, randomListChecks ) )
    , ( Fn.Random.map, ( 2, randomMapChecks ) )
    , ( Fn.Random.andThen, ( 2, randomAndThenChecks ) )
    , ( Fn.Test.concat, ( 1, testConcatChecks ) )
    ]


functionCallChecks : Dict ( ModuleName, String ) ( Int, CallCheckInfo -> Maybe (Error {}) )
functionCallChecks =
    List.foldl
        (\( reference, ( argCount, checks ) ) soFar ->
            Dict.insert reference ( argCount, checks.call ) soFar
        )
        Dict.empty
        intoFnChecks


type alias CompositionCheckInfo =
    { expectNaN : Bool
    , lookupTable : ModuleNameLookupTable
    , importLookup : ImportLookup
    , importRecordTypeAliases : Dict ModuleName (Dict String (List String))
    , moduleRecordTypeAliases : Dict String (List String)
    , importCustomTypes :
        Dict
            ModuleName
            (Dict
                String
                { variantNames : Set String
                , allParametersAreUsedInVariants : Bool
                }
            )
    , moduleCustomTypes :
        Dict
            String
            { variantNames : Set String
            , allParametersAreUsedInVariants : Bool
            }
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , extractSourceCode : Range -> String
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , earlier :
        { node : Node Expression
        , removeRange : Range
        }
    , later :
        { node : Node Expression
        , removeRange : Range
        }
    , isEmbeddedInComposition : Bool
    }


compositionChecks : CompositionCheckInfo -> Maybe (Error {})
compositionChecks checkInfo =
    pipingCompositionIntoLambdaChecks checkInfo
        |> onNothing (\() -> accessingRecordCompositionChecks checkInfo)
        |> onNothing (\() -> basicsIdentityCompositionChecks checkInfo)
        |> onNothing
            (\() ->
                case AstHelpers.getValueOrFnOrFnCall checkInfo checkInfo.earlier.node of
                    Just earlierFnOrCall ->
                        case ModuleNameLookupTable.moduleNameAt checkInfo.lookupTable earlierFnOrCall.fnRange of
                            Just earlierFnModuleName ->
                                valueOrFnOrFnCallThenAnyCompositionChecks
                                    { range = earlierFnOrCall.nodeRange
                                    , fn = ( earlierFnModuleName, earlierFnOrCall.fnName )
                                    , fnRange = earlierFnOrCall.fnRange
                                    , args = earlierFnOrCall.args
                                    , removeRange = checkInfo.earlier.removeRange
                                    }
                                    checkInfo

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            )


valueOrFnOrFnCallThenAnyCompositionChecks :
    { range : Range
    , fn : ( ModuleName, String )
    , fnRange : Range
    , args : List (Node Expression)
    , removeRange : Range
    }
    -> CompositionCheckInfo
    -> Maybe (Error {})
valueOrFnOrFnCallThenAnyCompositionChecks earlier checkInfo =
    case AstHelpers.getValueOrFnOrFnCall checkInfo checkInfo.later.node of
        Just laterFnOrCall ->
            case ModuleNameLookupTable.moduleNameAt checkInfo.lookupTable laterFnOrCall.fnRange of
                Just laterFnModuleName ->
                    case Dict.get ( laterFnModuleName, laterFnOrCall.fnName ) compositionIntoChecks of
                        Just ( laterArgCount, compositionIntoChecksForSpecificLater ) ->
                            compositionIntoChecksForSpecificLater
                                { expectNaN = checkInfo.expectNaN
                                , lookupTable = checkInfo.lookupTable
                                , importLookup = checkInfo.importLookup
                                , importCustomTypes = checkInfo.importCustomTypes
                                , moduleCustomTypes = checkInfo.moduleCustomTypes
                                , moduleBindings = checkInfo.moduleBindings
                                , localBindings = checkInfo.localBindings
                                , extractSourceCode = checkInfo.extractSourceCode
                                , inferredConstants = checkInfo.inferredConstants
                                , later =
                                    { range = laterFnOrCall.nodeRange
                                    , fn = ( laterFnModuleName, laterFnOrCall.fnName )
                                    , fnRange = laterFnOrCall.fnRange
                                    , args = laterFnOrCall.args
                                    , argCount = laterArgCount
                                    , removeRange = checkInfo.later.removeRange
                                    }
                                , earlier = earlier
                                , isEmbeddedInComposition = checkInfo.isEmbeddedInComposition
                                }
                                |> Maybe.map (\e -> Rule.errorWithFix e.info laterFnOrCall.fnRange e.fix)

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            case AstHelpers.getSingleArgCall checkInfo.later.node of
                Nothing ->
                    Nothing

                Just laterSingleArgCall ->
                    case Normalize.getUnappliedBinaryOperation checkInfo laterSingleArgCall.called of
                        Nothing ->
                            Nothing

                        Just operator ->
                            valueOrFnOrFnCallThenOperatorCallCompositionChecks
                                { lookupTable = checkInfo.lookupTable
                                , importLookup = checkInfo.importLookup
                                , inferredConstants = checkInfo.inferredConstants
                                , importCustomTypes = checkInfo.importCustomTypes
                                , moduleCustomTypes = checkInfo.moduleCustomTypes
                                , moduleBindings = checkInfo.moduleBindings
                                , localBindings = checkInfo.localBindings
                                , earlier = earlier
                                , later =
                                    { range = Node.range checkInfo.later.node
                                    , operator = operator
                                    , arg = laterSingleArgCall.arg
                                    , removeRange = checkInfo.later.removeRange
                                    }
                                }
                                |> Maybe.map
                                    (\e ->
                                        Rule.errorWithFix e.info (Node.range laterSingleArgCall.called) e.fix
                                    )


type alias ValueOrFnOrFnCallThenOperatorCallCheckInfo =
    QualifyResources
        (Normalize.Resources
            { earlier :
                { range : Range
                , fn : ( ModuleName, String )
                , fnRange : Range
                , args : List (Node Expression)
                , removeRange : Range
                }
            , later :
                { range : Range
                , operator : String
                , arg : Node Expression
                , removeRange : Range
                }
            }
        )


valueOrFnOrFnCallThenOperatorCallCompositionChecks : ValueOrFnOrFnCallThenOperatorCallCheckInfo -> Maybe ErrorInfoAndFix
valueOrFnOrFnCallThenOperatorCallCompositionChecks checkInfo =
    case checkInfo.later.operator of
        "==" ->
            elementCountFnThenCompareSpecificIntOperationChecks
                { isEmpty = True, specificInt = 0, intRightInDescription = True }
                checkInfo

        "/=" ->
            elementCountFnThenCompareSpecificIntOperationChecks
                { isEmpty = False, specificInt = 0, intRightInDescription = True }
                checkInfo

        ">" ->
            elementCountFnThenCompareSpecificIntOperationChecks
                { isEmpty = True, specificInt = 1, intRightInDescription = False }
                checkInfo

        ">=" ->
            elementCountFnThenCompareSpecificIntOperationChecks
                { isEmpty = True, specificInt = 0, intRightInDescription = False }
                checkInfo

        "<" ->
            elementCountFnThenCompareSpecificIntOperationChecks
                { isEmpty = False, specificInt = 0, intRightInDescription = False }
                checkInfo

        "<=" ->
            elementCountFnThenCompareSpecificIntOperationChecks
                { isEmpty = False, specificInt = 1, intRightInDescription = False }
                checkInfo

        _ ->
            Nothing


elementCountFnThenCompareSpecificIntOperationChecks :
    { isEmpty : Bool, specificInt : Int, intRightInDescription : Bool }
    -> ValueOrFnOrFnCallThenOperatorCallCheckInfo
    -> Maybe ErrorInfoAndFix
elementCountFnThenCompareSpecificIntOperationChecks config checkInfo =
    case checkInfo.earlier.args of
        _ :: _ ->
            Nothing

        [] ->
            if Normalize.getInt checkInfo checkInfo.later.arg == Just config.specificInt then
                case
                    compareElementCountChecks
                        { isEmpty = config.isEmpty
                        , operation =
                            \fn ->
                                if config.intRightInDescription then
                                    fn ++ " " ++ checkInfo.later.operator ++ " " ++ String.fromInt config.specificInt

                                else
                                    String.fromInt config.specificInt ++ " " ++ checkInfo.later.operator ++ " " ++ fn
                        , replaceByInstruction =
                            \descriptions ->
                                "replace this composition by " ++ descriptions.replacementOperation
                        , fn = checkInfo.earlier.fn
                        }
                of
                    Nothing ->
                        Nothing

                    Just error ->
                        Just
                            { info = { message = error.message, details = error.details }
                            , fix =
                                [ Fix.replaceRangeBy checkInfo.earlier.range
                                    (qualifiedToString (qualify error.isEmptyFn checkInfo))
                                , if config.isEmpty then
                                    Fix.removeRange checkInfo.later.removeRange

                                  else
                                    Fix.replaceRangeBy checkInfo.later.range
                                        (qualifiedToString (qualify Fn.Basics.not checkInfo))
                                ]
                            }

            else
                Nothing


compositionIntoChecks : Dict ( ModuleName, String ) ( Int, CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix )
compositionIntoChecks =
    List.foldl
        (\( reference, ( argCount, checks ) ) soFar ->
            Dict.insert reference ( argCount, checks.composition ) soFar
        )
        Dict.empty
        intoFnChecks


findOperatorRange :
    { extractSourceCode : Range -> String
    , commentRanges : List Range
    , operator : String
    , leftRange : Range
    , rightRange : Range
    }
    -> Range
findOperatorRange context =
    let
        betweenOperands : String
        betweenOperands =
            context.extractSourceCode
                { start = context.leftRange.end, end = context.rightRange.start }

        operatorStartLocationFound : Maybe Location
        operatorStartLocationFound =
            case String.indexes context.operator betweenOperands of
                [ operatorOffset ] ->
                    Just
                        (offsetInStringToLocation
                            { offset = operatorOffset
                            , startLocation = context.leftRange.end
                            , source = betweenOperands
                            }
                        )

                possiblyOperatorOffsets ->
                    possiblyOperatorOffsets
                        |> findMap
                            (\operatorOffset ->
                                let
                                    operatorStartLocation : Location
                                    operatorStartLocation =
                                        offsetInStringToLocation
                                            { offset = operatorOffset
                                            , startLocation = context.leftRange.end
                                            , source = betweenOperands
                                            }

                                    isPartOfComment : Bool
                                    isPartOfComment =
                                        List.any
                                            (\commentRange ->
                                                rangeContainsLocation operatorStartLocation commentRange
                                            )
                                            context.commentRanges
                                in
                                if isPartOfComment then
                                    Nothing

                                else
                                    Just operatorStartLocation
                            )
    in
    case operatorStartLocationFound of
        Just operatorStartLocation ->
            { start = operatorStartLocation
            , end =
                { row = operatorStartLocation.row
                , column = operatorStartLocation.column + String.length context.operator
                }
            }

        -- there's a bug somewhere
        Nothing ->
            Range.emptyRange


offsetInStringToLocation : { offset : Int, source : String, startLocation : Location } -> Location
offsetInStringToLocation config =
    case config.source |> String.left config.offset |> String.lines |> List.reverse of
        [] ->
            config.startLocation

        [ onlyLine ] ->
            { row = config.startLocation.row
            , column = config.startLocation.column + String.length onlyLine
            }

        lineWithOffsetLocation :: _ :: linesBeforeBeforeWithOffsetLocation ->
            { row = config.startLocation.row + 1 + List.length linesBeforeBeforeWithOffsetLocation
            , column = 1 + String.length lineWithOffsetLocation
            }


plusChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
plusChecks checkInfo =
    addingZeroCheck checkInfo
        |> onNothing (\() -> addingOppositesCheck checkInfo)


addingZeroCheck : OperatorApplicationCheckInfo -> Maybe (Error {})
addingZeroCheck checkInfo =
    checkOperationFromBothSides checkInfo
        (\side ->
            if AstHelpers.getUncomputedNumberValue side.node == Just 0 then
                Just
                    (Rule.errorWithFix
                        { message = "Unnecessary adding 0"
                        , details = [ "You can replace this operation by the " ++ side.otherDescription ++ " number you added 0 to." ]
                        }
                        (Range.combine [ checkInfo.operatorRange, Node.range side.node ])
                        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.otherNode })
                    )

            else
                Nothing
        )


addingOppositesCheck : OperatorApplicationCheckInfo -> Maybe (Error {})
addingOppositesCheck checkInfo =
    if
        checkInfo.expectNaN
            && (AstHelpers.couldBeValueContainingNaN checkInfo.left
                    || AstHelpers.couldBeValueContainingNaN checkInfo.right
               )
    then
        Nothing

    else
        case Normalize.compare checkInfo checkInfo.left (Node.empty (Expression.Negation checkInfo.right)) of
            Normalize.ConfirmedEquality ->
                Just
                    (Rule.errorWithFix
                        { message = "Adding opposite numbers will result in 0"
                        , details = [ "Adding two numbers with an equal absolute value and an opposite sign will cancel each other out. You can replace this operation by 0." ]
                        }
                        checkInfo.parentRange
                        [ Fix.replaceRangeBy checkInfo.parentRange "0" ]
                    )

            Normalize.ConfirmedInequality ->
                Nothing

            Normalize.Unconfirmed ->
                Nothing


minusChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
minusChecks checkInfo =
    if AstHelpers.getUncomputedNumberValue checkInfo.right == Just 0 then
        Just
            (Rule.errorWithFix
                { message = "Unnecessary subtracting 0"
                , details = [ "You can replace this operation by the left number you subtracted 0 from." ]
                }
                checkInfo.operatorRange
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.left })
            )

    else if AstHelpers.getUncomputedNumberValue checkInfo.left == Just 0 then
        Just
            (Rule.errorWithFix
                { message = "Subtracting from 0 is the same as negating"
                , details = [ "You can replace this operation by the negated right number you subtracted from 0, like `-n`." ]
                }
                checkInfo.operatorRange
                (Fix.insertAt checkInfo.parentRange.start "-"
                    :: replaceBySubExpressionFix checkInfo.parentRange checkInfo.right
                )
            )

    else
        checkIfMinusResultsInZero checkInfo


checkIfMinusResultsInZero : OperatorApplicationCheckInfo -> Maybe (Error {})
checkIfMinusResultsInZero checkInfo =
    if
        checkInfo.expectNaN
            && (AstHelpers.couldBeValueContainingNaN checkInfo.left
                    || AstHelpers.couldBeValueContainingNaN checkInfo.right
               )
    then
        Nothing

    else
        case Normalize.compare checkInfo checkInfo.left checkInfo.right of
            Normalize.ConfirmedEquality ->
                Just
                    (Rule.errorWithFix
                        { message = "Subtracting equal numbers will result in 0"
                        , details = [ "You can replace this operation by 0." ]
                        }
                        checkInfo.parentRange
                        [ Fix.replaceRangeBy checkInfo.parentRange "0" ]
                    )

            Normalize.ConfirmedInequality ->
                Nothing

            Normalize.Unconfirmed ->
                Nothing


multiplyChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
multiplyChecks checkInfo =
    checkOperationFromBothSides checkInfo
        (\side ->
            unnecessaryOperationWithEmptySideChecks numberForMultiplyProperties side checkInfo
        )
        |> onNothing
            (\() ->
                checkOperationFromBothSides checkInfo
                    (\side ->
                        if numberNotExpectingNaNForMultiplyProperties.absorbing.is (extractNormalizeResources checkInfo) side.node then
                            Just
                                (Rule.errorWithFix
                                    { message = "Multiplication by 0 should be replaced"
                                    , details =
                                        [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                        , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                        , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                        , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                        ]
                                    }
                                    (Range.combine [ checkInfo.operatorRange, Node.range side.node ])
                                    (if
                                        checkInfo.expectNaN
                                            && AstHelpers.couldBeValueContainingNaN side.otherNode
                                     then
                                        []

                                     else
                                        [ Fix.replaceRangeBy checkInfo.parentRange "0" ]
                                    )
                                )

                        else
                            Nothing
                    )
            )


checkOperationFromBothSides :
    OperatorApplicationCheckInfo
    ->
        ({ node : Node Expression, otherNode : Node Expression, otherDescription : String }
         -> Maybe checked
        )
    -> Maybe checked
checkOperationFromBothSides checkInfo checkFromSide =
    checkFromSide
        { node = checkInfo.left
        , otherNode = checkInfo.right
        , otherDescription = "right"
        }
        |> onNothing
            (\() ->
                checkFromSide
                    { node = checkInfo.right
                    , otherNode = checkInfo.left
                    , otherDescription = "left"
                    }
            )


divisionChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
divisionChecks checkInfo =
    let
        maybeDivisorNumber : Maybe Float
        maybeDivisorNumber =
            AstHelpers.getUncomputedNumberValue checkInfo.right
    in
    if maybeDivisorNumber == Just 1 then
        Just
            (Rule.errorWithFix
                { message = "Unnecessary dividing by 1"
                , details = [ "You can replace this operation by the left number you divided by 1." ]
                }
                checkInfo.operatorRange
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.left })
            )

    else if AstHelpers.getUncomputedNumberValue checkInfo.left == Just 0 then
        if maybeDivisorNumber == Just 0 then
            if checkInfo.expectNaN then
                Nothing

            else
                Just
                    (Rule.error
                        { message = "0 / 0 is NaN but the configuration option expectNaN is not enabled"
                        , details =
                            [ "Dividing 0 by 0 is the simplest way to obtain a NaN value in elm. NaN is a special Float value that signifies a failure of a mathematical operation and tends to spread through code."
                            , "By default, Simplify assumes that your code does not expect NaN values so it can enable a few more checks. If creating NaN here was not your intention, replace this division by a more fitting number like 0."
                            , "If you do want to use NaN here, please add expectNaN to your Simplify configuration to let it know NaN is a possible value in your code."
                            , "expectNaN: https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/Simplify#expectNaN"
                            ]
                        }
                        checkInfo.operatorRange
                    )

        else if
            checkInfo.expectNaN
                && AstHelpers.couldBeValueContainingNaN checkInfo.right
        then
            Nothing

        else
            Just
                (Rule.errorWithFix
                    { message = "Dividing 0 will result in 0"
                    , details =
                        [ "Dividing 0 by anything, even infinite numbers, gives 0 which means you can replace the whole division operation by 0."
                        , "Most likely, dividing 0 was unintentional and you had a different number in mind."
                        ]
                    }
                    checkInfo.operatorRange
                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.leftRange })
                )

    else
        Nothing


intDivideChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
intDivideChecks checkInfo =
    (case AstHelpers.getUncomputedNumberValue checkInfo.right of
        Just rightNumber ->
            if rightNumber == 1 then
                Just
                    (Rule.errorWithFix
                        { message = "Unnecessary dividing by 1"
                        , details = [ "You can replace this operation by the left integer you divided by 1." ]
                        }
                        checkInfo.operatorRange
                        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.leftRange })
                    )

            else if rightNumber == 0 then
                Just
                    (Rule.errorWithFix
                        { message = "Dividing by 0 will result in 0"
                        , details =
                            [ "Dividing anything by 0 using (//) gives 0 which means you can replace the whole division operation by 0."
                            , "Most likely, dividing by 0 was unintentional and you had a different number in mind."
                            ]
                        }
                        checkInfo.operatorRange
                        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.rightRange })
                    )

            else
                Nothing

        Nothing ->
            Nothing
    )
        |> onNothing
            (\() ->
                if AstHelpers.getUncomputedNumberValue checkInfo.left == Just 0 then
                    Just
                        (Rule.errorWithFix
                            { message = "Dividing 0 will result in 0"
                            , details =
                                [ "Dividing 0 by anything using (//), even 0, gives 0 which means you can replace the whole division operation by 0."
                                , "Most likely, dividing 0 was unintentional and you had a different number in mind."
                                ]
                            }
                            checkInfo.operatorRange
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.leftRange })
                        )

                else
                    Nothing
            )


plusplusChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
plusplusChecks checkInfo =
    appendStringCheck checkInfo
        |> onNothing
            (\() ->
                checkOperationFromBothSides checkInfo
                    (\side -> appendEmptyCheck side listCollection checkInfo)
            )
        |> onNothing
            (\() ->
                collectionUnionWithLiteralsChecks
                    { leftElementsStayOnTheLeft = True }
                    { first = checkInfo.left
                    , second = checkInfo.right
                    , operationRange = checkInfo.operatorRange
                    , operation = "++"
                    }
                    listCollection
                    checkInfo
            )
        |> onNothing
            (\() ->
                collectionUnionWithLiteralsChecks
                    { leftElementsStayOnTheLeft = True }
                    { first = checkInfo.left
                    , second = checkInfo.right
                    , operationRange = checkInfo.operatorRange
                    , operation = "++"
                    }
                    stringCollection
                    checkInfo
            )
        |> onNothing
            (\() ->
                case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.left of
                    Just leftListSingletonElement ->
                        if checkInfo.isOnTheRightSideOfPlusPlus then
                            Nothing

                        else
                            Just
                                (Rule.errorWithFix
                                    { message = "Appending a singleton list to the beginning is the same as using (::) with the value inside"
                                    , details = [ "You can replace this (++) operation by using (::) with the value inside the left singleton list on the right list." ]
                                    }
                                    checkInfo.operatorRange
                                    (Fix.replaceRangeBy checkInfo.operatorRange "::"
                                        :: replaceBySubExpressionFix checkInfo.leftRange leftListSingletonElement
                                    )
                                )

                    Nothing ->
                        Nothing
            )


findNextStringLiteral : Node Expression -> Node Expression -> Maybe { left : Range, right : Node String }
findNextStringLiteral (Node range node) previousNode =
    case node of
        Expression.Literal str ->
            Just { left = Node.range previousNode, right = Node range str }

        Expression.OperatorApplication "++" _ ((Node leftRange left) as leftNode) rightNode ->
            case left of
                Expression.Literal str ->
                    Just { left = Node.range previousNode, right = Node leftRange str }

                _ ->
                    findNextStringLiteral rightNode leftNode

        _ ->
            Nothing


appendStringCheck : OperatorApplicationCheckInfo -> Maybe (Error {})
appendStringCheck checkInfo =
    case Node.value checkInfo.left of
        Expression.Literal leftContents ->
            appendEmptyStringCheck checkInfo leftContents
                |> onNothing (\() -> appendStringLiteralsCheck checkInfo (Node.range checkInfo.left))

        _ ->
            Nothing


appendStringLiteralsCheck : OperatorApplicationCheckInfo -> Range -> Maybe (Error {})
appendStringLiteralsCheck checkInfo leftRange =
    let
        otherNode : Maybe (Node Expression)
        otherNode =
            case Node.value checkInfo.right of
                Expression.Literal _ ->
                    Just checkInfo.right

                Expression.OperatorApplication "++" _ ((Node _ (Expression.Literal _)) as left) _ ->
                    Just left

                _ ->
                    Nothing
    in
    case otherNode of
        Just (Node rightRange _) ->
            if leftRange.end.row == rightRange.start.row then
                let
                    leftUsesTripleQuotes : Bool
                    leftUsesTripleQuotes =
                        usesTripleQuotes checkInfo.extractSourceCode leftRange

                    rightUsesTripleQuotes : Bool
                    rightUsesTripleQuotes =
                        usesTripleQuotes checkInfo.extractSourceCode rightRange
                in
                Just
                    (Rule.errorWithFix
                        { message = checkInfo.operator ++ " on " ++ stringCollection.represents ++ " literals can be turned into a single " ++ stringCollection.represents ++ " literal"
                        , details = [ "Try moving all the elements into a single " ++ stringCollection.represents ++ " literal." ]
                        }
                        checkInfo.operatorRange
                        ([ Fix.removeRange
                            { start = { row = leftRange.end.row, column = leftRange.end.column - sizeOfQuotes leftUsesTripleQuotes }
                            , end = { row = rightRange.start.row, column = rightRange.start.column + sizeOfQuotes rightUsesTripleQuotes }
                            }
                         ]
                            |> consIf (leftUsesTripleQuotes && not rightUsesTripleQuotes) (\() -> Fix.insertAt rightRange.end "\"\"")
                            |> consIf (not leftUsesTripleQuotes && rightUsesTripleQuotes) (\() -> Fix.insertAt leftRange.start "\"\"")
                        )
                    )

            else
                Nothing

        Nothing ->
            Nothing


usesTripleQuotes : (Range -> String) -> Range -> Bool
usesTripleQuotes extractSourceCode range =
    String.startsWith "\"\"\"" (extractSourceCode range)


sizeOfQuotes : Bool -> number
sizeOfQuotes isTripleQuotes =
    if isTripleQuotes then
        3

    else
        1


appendEmptyStringCheck : OperatorApplicationCheckInfo -> String -> Maybe (Error {})
appendEmptyStringCheck checkInfo leftContents =
    case findNextStringLiteral checkInfo.right checkInfo.left of
        Just { left, right } ->
            if String.isEmpty leftContents then
                Just
                    (Rule.errorWithFix
                        (appendEmptyErrorInfo "right" stringCollection)
                        checkInfo.operatorRange
                        [ Fix.removeRange
                            { start = (Node.range checkInfo.left).start
                            , end = (Node.range checkInfo.right).start
                            }
                        ]
                    )

            else if String.isEmpty (Node.value right) then
                Just
                    (Rule.errorWithFix
                        (appendEmptyErrorInfo "left" stringCollection)
                        checkInfo.operatorRange
                        [ Fix.removeRange
                            { start = left.end
                            , end = (Node.range right).end
                            }
                        ]
                    )

            else
                Nothing

        Nothing ->
            Nothing


appendEmptyCheck :
    { side | node : Node Expression, otherNode : Node Expression, otherDescription : String }
    -> TypeProperties (EmptiableProperties empty otherProperties)
    -> OperatorApplicationCheckInfo
    -> Maybe (Error {})
appendEmptyCheck side collection checkInfo =
    if isInTypeSubset collection.empty checkInfo side.node then
        Just
            (Rule.errorWithFix
                (appendEmptyErrorInfo side.otherDescription
                    collection
                )
                checkInfo.operatorRange
                (keepOnlyFix
                    { keep = Node.range side.otherNode
                    , parentRange = checkInfo.parentRange
                    }
                )
            )

    else
        Nothing


appendEmptyErrorInfo :
    String
    -> TypeProperties (EmptiableProperties empty otherProperties)
    -> { message : String, details : List String }
appendEmptyErrorInfo description collection =
    { message = "Unnecessary appending " ++ typeSubsetDescriptionIndefinite collection.empty
    , details = [ "You can replace this operation by the " ++ description ++ " " ++ collection.represents ++ "." ]
    }


consChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
consChecks checkInfo =
    (case Node.value checkInfo.right of
        Expression.ListExpr tailElements ->
            let
                fix : List Fix
                fix =
                    case tailElements of
                        [] ->
                            [ Fix.insertAt checkInfo.leftRange.start "[ "
                            , Fix.replaceRangeBy
                                { start = checkInfo.leftRange.end
                                , end = checkInfo.rightRange.end
                                }
                                " ]"
                            ]

                        _ :: _ ->
                            [ Fix.insertAt checkInfo.leftRange.start "[ "
                            , Fix.replaceRangeBy checkInfo.operatorRange ","
                            , Fix.removeRange (leftBoundaryRange checkInfo.rightRange)
                            ]
            in
            Just
                (Rule.errorWithFix
                    { message = "Element added to the beginning of the list could be included in the list"
                    , details = [ "Try moving the element inside the list it is being added to." ]
                    }
                    checkInfo.operatorRange
                    fix
                )

        _ ->
            Nothing
    )
        |> onNothing
            (\() ->
                case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.right of
                    Just tailSingletonElement ->
                        Just
                            (Rule.errorWithFix
                                { message = "Element added to the beginning of the list could be included in the list"
                                , details = [ "You can replace this operation by a list that contains both the added element and the value inside the singleton list." ]
                                }
                                checkInfo.operatorRange
                                [ Fix.insertAt checkInfo.leftRange.start "[ "
                                , Fix.replaceRangeBy
                                    { start = checkInfo.leftRange.end
                                    , end = (Node.range tailSingletonElement).start
                                    }
                                    ", "
                                , Fix.replaceRangeBy
                                    { start = (Node.range tailSingletonElement).end
                                    , end = checkInfo.parentRange.end
                                    }
                                    " ]"
                                ]
                            )

                    Nothing ->
                        Nothing
            )



-- EQUALITY


equalityChecks : Bool -> OperatorApplicationCheckInfo -> Maybe (Error {})
equalityChecks isEqual checkInfo =
    elementCountEqualityTo0OperationChecks isEqual checkInfo
        |> onNothing
            (\() ->
                checkOperationFromBothSides checkInfo
                    (\side ->
                        if Normalize.getBool checkInfo side.node == Just isEqual then
                            Just
                                (Rule.errorWithFix
                                    { message = "Unnecessary comparison with boolean"
                                    , details = [ "The result of the expression will be the same with or without the comparison." ]
                                    }
                                    (Range.combine [ checkInfo.operatorRange, Node.range side.node ])
                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.otherNode })
                                )

                        else
                            Nothing
                    )
            )
        |> onNothing
            (\() ->
                case AstHelpers.getSpecificUnreducedFnCall Fn.Basics.not checkInfo.lookupTable checkInfo.left of
                    Just leftNotCall ->
                        case AstHelpers.getSpecificUnreducedFnCall Fn.Basics.not checkInfo.lookupTable checkInfo.right of
                            Just rightNotCall ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "Unnecessary `not` on both sides of (" ++ checkInfo.operator ++ ")"
                                        , details = [ "You can replace the bool on each side by the value given to `not`." ]
                                        }
                                        checkInfo.operatorRange
                                        (replaceBySubExpressionFix leftNotCall.nodeRange leftNotCall.firstArg
                                            ++ replaceBySubExpressionFix rightNotCall.nodeRange rightNotCall.firstArg
                                        )
                                    )

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            )
        |> onNothing
            (\() ->
                let
                    leftNormal : Expression
                    leftNormal =
                        Normalize.normalizeExpression checkInfo checkInfo.left

                    rightNormal : Expression
                    rightNormal =
                        Normalize.normalizeExpression checkInfo checkInfo.right
                in
                case Normalize.compareExistingNormals leftNormal rightNormal of
                    Normalize.ConfirmedEquality ->
                        if
                            checkInfo.expectNaN
                                && (AstHelpers.couldBeValueContainingNaN checkInfo.left
                                        || AstHelpers.couldBeValueContainingNaN checkInfo.right
                                   )
                        then
                            Nothing

                        else
                            Just (comparisonError isEqual checkInfo)

                    Normalize.ConfirmedInequality ->
                        Just (comparisonError (not isEqual) checkInfo)

                    Normalize.Unconfirmed ->
                        compareNumberIntervalsResultsInOperationCheck
                            { leftNormal = leftNormal
                            , rightNormal = rightNormal
                            , boundsOrderToResult =
                                \boundsOrder ->
                                    case boundsMaybeEqual boundsOrder of
                                        CanBeEqualInOnlyOneNumber ->
                                            Nothing

                                        CannotBeEqual ->
                                            Just
                                                (if isEqual then
                                                    Fn.Basics.falseVariant

                                                 else
                                                    Fn.Basics.trueVariant
                                                )
                            }
                            checkInfo
            )
        |> onNothing
            (\() -> comparisonWithEmptyChecks isEqual checkInfo)


numberBoundsToDescription : { min : Float, max : Float } -> String
numberBoundsToDescription numberBounds =
    if numberBounds.min == numberBounds.max then
        "exactly " ++ String.fromFloat numberBounds.min

    else if Basics.isInfinite numberBounds.min then
        if Basics.isInfinite numberBounds.max then
            "any number"

        else
            "at most " ++ String.fromFloat numberBounds.max

    else if Basics.isInfinite numberBounds.max then
        "at least " ++ String.fromFloat numberBounds.min

    else
        "between "
            ++ String.fromFloat numberBounds.min
            ++ " and "
            ++ String.fromFloat numberBounds.max
            ++ " inclusive"


compareNumberIntervalsResultsInOperationCheck :
    { leftNormal : Expression
    , rightNormal : Expression
    , boundsOrderToResult : BoundsOrder -> Maybe ( ModuleName, String )
    }
    -> OperatorApplicationCheckInfo
    -> Maybe (Error {})
compareNumberIntervalsResultsInOperationCheck config checkInfo =
    if
        checkInfo.expectNaN
            && (AstHelpers.couldBeValueContainingNaN checkInfo.left
                    || AstHelpers.couldBeValueContainingNaN checkInfo.right
               )
    then
        Nothing

    else
        let
            leftNumberBounds : { min : Float, max : Float }
            leftNumberBounds =
                normalGetNumberBounds config.leftNormal

            rightNumberBounds : { min : Float, max : Float }
            rightNumberBounds =
                normalGetNumberBounds config.rightNormal
        in
        case numberBoundsCompare leftNumberBounds rightNumberBounds of
            Nothing ->
                Nothing

            Just boundsOrder ->
                case config.boundsOrderToResult boundsOrder of
                    Nothing ->
                        Nothing

                    Just result ->
                        Just
                            (Rule.errorWithFix
                                { message =
                                    "("
                                        ++ checkInfo.operator
                                        ++ ") comparison will result in "
                                        ++ qualifiedToString (qualify result defaultQualifyResources)
                                , details =
                                    [ "Based on the values and/or the context, we can determine that the interval of the left number is always "
                                        ++ boundsOrderToDescription boundsOrder
                                        ++ " the interval of the right number. As a result, this operation can be replaced by "
                                        ++ qualifiedToString (qualify result defaultQualifyResources)
                                        ++ "."
                                    , "The left number was determined to be "
                                        ++ numberBoundsToDescription leftNumberBounds
                                        ++ " and the right number was determined to be "
                                        ++ numberBoundsToDescription rightNumberBounds
                                        ++ "."
                                    ]
                                }
                                checkInfo.operatorRange
                                [ Fix.replaceRangeBy checkInfo.parentRange
                                    (qualifiedToString (qualify result checkInfo))
                                ]
                            )


elementCountEqualityTo0OperationChecks : Bool -> OperatorApplicationCheckInfo -> Maybe (Error {})
elementCountEqualityTo0OperationChecks isEqual checkInfo =
    checkOperationFromBothSides checkInfo
        (\side ->
            compareElementCountToSpecificIntOperationOneSidedChecks
                { isEmpty = isEqual
                , specificInt = 0
                , operation = \fn -> fn ++ " " ++ checkInfo.operator ++ " 0"
                , potentialElementCount = side.node
                , potentialInt = side.otherNode
                }
                checkInfo
        )


compareElementCountToSpecificIntOperationOneSidedChecks :
    { isEmpty : Bool
    , specificInt : Int
    , operation : String -> String
    , potentialElementCount : Node Expression
    , potentialInt : Node Expression
    }
    -> OperatorApplicationCheckInfo
    -> Maybe (Error {})
compareElementCountToSpecificIntOperationOneSidedChecks config checkInfo =
    if Normalize.getInt checkInfo config.potentialInt == Just config.specificInt then
        case AstHelpers.getUnreducedValueOrFnOrFnCall config.potentialElementCount of
            Nothing ->
                Nothing

            Just call ->
                case ModuleNameLookupTable.moduleNameAt checkInfo.lookupTable call.fnRange of
                    Nothing ->
                        Nothing

                    Just callFn ->
                        case
                            compareElementCountChecks
                                { isEmpty = config.isEmpty
                                , operation = config.operation
                                , replaceByInstruction =
                                    \descriptions ->
                                        "replace this operation by "
                                            ++ descriptions.replacementOperation
                                            ++ " on the "
                                            ++ descriptions.collection
                                            ++ " given to the "
                                            ++ descriptions.elementCountFn
                                            ++ " call"
                                , fn = ( callFn, call.fnName )
                                }
                        of
                            Nothing ->
                                Nothing

                            Just error ->
                                let
                                    isEmptyFnQualifiedString : String
                                    isEmptyFnQualifiedString =
                                        qualifiedToString (qualify error.isEmptyFn checkInfo)
                                in
                                Just
                                    (Rule.errorWithFix { message = error.message, details = error.details }
                                        call.fnRange
                                        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range config.potentialElementCount }
                                            ++ (if config.isEmpty then
                                                    [ Fix.replaceRangeBy call.fnRange isEmptyFnQualifiedString
                                                    ]

                                                else
                                                    let
                                                        notFn : String
                                                        notFn =
                                                            qualifiedToString (qualify Fn.Basics.not checkInfo)
                                                    in
                                                    case call.callStyle of
                                                        CallStyle.Application ->
                                                            [ Fix.replaceRangeBy call.fnRange (notFn ++ " (" ++ isEmptyFnQualifiedString)
                                                            , Fix.insertAt checkInfo.parentRange.end ")"
                                                            ]

                                                        CallStyle.Pipe CallStyle.LeftToRight ->
                                                            [ Fix.replaceRangeBy call.fnRange (isEmptyFnQualifiedString ++ " |> " ++ notFn)
                                                            ]

                                                        CallStyle.Pipe CallStyle.RightToLeft ->
                                                            [ Fix.replaceRangeBy call.fnRange (notFn ++ " <| " ++ isEmptyFnQualifiedString)
                                                            ]
                                               )
                                        )
                                    )

    else
        Nothing


compareElementCountChecks :
    { isEmpty : Bool
    , operation : String -> String
    , replaceByInstruction :
        { collection : String, elementCountFn : String, replacementOperation : String }
        -> String
    , fn : ( ModuleName, String )
    }
    ->
        Maybe
            { message : String
            , details : List String
            , isEmptyFn : ( ModuleName, String )
            }
compareElementCountChecks checkInfo =
    collectionCompareElementCountCheck listCollection checkInfo
        |> onNothing
            (\() -> collectionCompareElementCountCheck stringCollection checkInfo)
        |> onNothing
            (\() -> collectionCompareElementCountCheck dictCollection checkInfo)
        |> onNothing
            (\() -> collectionCompareElementCountCheck setCollection checkInfo)
        |> onNothing
            (\() -> collectionCompareElementCountCheck arrayCollection checkInfo)


collectionCompareElementCountCheck :
    TypeProperties (WithElementCountFn { a | isEmptyFn : ( ModuleName, String ) })
    ->
        { isEmpty : Bool
        , operation : String -> String
        , replaceByInstruction :
            { collection : String, elementCountFn : String, replacementOperation : String }
            -> String
        , fn : ( ModuleName, String )
        }
    ->
        Maybe
            { message : String
            , details : List String
            , isEmptyFn : ( ModuleName, String )
            }
collectionCompareElementCountCheck collection checkInfo =
    if checkInfo.fn == collection.elementCount.fn then
        let
            isEmptyFnDescription : String
            isEmptyFnDescription =
                qualifiedToString (qualify collection.isEmptyFn defaultQualifyResources)

            replacementDescription : String
            replacementDescription =
                if checkInfo.isEmpty then
                    isEmptyFnDescription

                else
                    "not on " ++ isEmptyFnDescription
        in
        Just
            { isEmptyFn = collection.isEmptyFn
            , message = checkInfo.operation (qualifiedToString collection.elementCount.fn) ++ " can be replaced by " ++ replacementDescription
            , details =
                [ (if collection.elementCount.isConstantTime then
                    ""

                   else
                    "Whereas "
                        ++ qualifiedToString collection.elementCount.fn
                        ++ " takes as long to run as the number of elements in the "
                        ++ collection.represents
                        ++ ", "
                        ++ isEmptyFnDescription
                        ++ " runs in constant time. "
                  )
                    ++ "You can "
                    ++ checkInfo.replaceByInstruction
                        { replacementOperation = replacementDescription
                        , collection = collection.represents
                        , elementCountFn = qualifiedToString collection.elementCount.fn
                        }
                    ++ "."
                ]
            }

    else
        Nothing


comparisonWithEmptyChecks : Bool -> OperatorApplicationCheckInfo -> Maybe (Error {})
comparisonWithEmptyChecks isEqual checkInfo =
    let
        surroundWith : ( ModuleName, String ) -> Node Expression -> ( String, String )
        surroundWith isEmptyFn (Node _ expr) =
            let
                fnName : String
                fnName =
                    qualifiedToString (qualify isEmptyFn checkInfo)
            in
            if isEqual then
                if needsParens expr then
                    ( fnName ++ " (", ")" )

                else
                    ( fnName ++ " ", "" )

            else if needsParens expr then
                ( qualifiedToString (qualify Fn.Basics.not checkInfo) ++ " (" ++ fnName ++ " (", "))" )

            else
                ( qualifiedToString (qualify Fn.Basics.not checkInfo) ++ " (" ++ fnName ++ " ", ")" )
    in
    case getEmptyCollection checkInfo.lookupTable checkInfo.right of
        Just emptyCollection ->
            let
                ( left, right ) =
                    surroundWith emptyCollection.isEmptyFn checkInfo.left
            in
            Just
                (Rule.errorWithFix (toComparisonWithEmptyErrorInfo emptyCollection)
                    (Range.combine [ checkInfo.operatorRange, checkInfo.rightRange ])
                    [ Fix.insertAt checkInfo.leftRange.start left
                    , Fix.replaceRangeBy
                        { start = checkInfo.leftRange.end
                        , end = checkInfo.rightRange.end
                        }
                        right
                    ]
                )

        Nothing ->
            case getEmptyCollection checkInfo.lookupTable checkInfo.left of
                Just emptyCollection ->
                    let
                        ( left, right ) =
                            surroundWith emptyCollection.isEmptyFn checkInfo.right
                    in
                    Just
                        (Rule.errorWithFix (toComparisonWithEmptyErrorInfo emptyCollection)
                            (Range.combine [ checkInfo.leftRange, checkInfo.operatorRange ])
                            [ Fix.replaceRangeBy
                                { start = checkInfo.leftRange.start
                                , end = checkInfo.rightRange.start
                                }
                                left
                            , Fix.insertAt checkInfo.rightRange.end right
                            ]
                        )

                Nothing ->
                    Nothing


toComparisonWithEmptyErrorInfo :
    { represents : String, isEmptyFn : ( ModuleName, String ) }
    -> { message : String, details : List String }
toComparisonWithEmptyErrorInfo emptyCollection =
    let
        modIsEmpty : String
        modIsEmpty =
            qualifiedToString (qualify emptyCollection.isEmptyFn defaultQualifyResources)
    in
    { message = "Comparison with an empty " ++ emptyCollection.represents ++ " can be replaced by a call to " ++ modIsEmpty
    , details = [ "You can replace this comparison to an empty " ++ emptyCollection.represents ++ " with a call to " ++ modIsEmpty ++ ", which is more efficient." ]
    }


comparisonWithEmptyCheckInPrefixOperator : ModuleNameLookupTable -> Range -> Node Expression -> Maybe (Error {})
comparisonWithEmptyCheckInPrefixOperator lookupTable operatorRange arg =
    case getEmptyCollection lookupTable arg of
        Just emptyCollection ->
            let
                modIsEmpty : String
                modIsEmpty =
                    qualifiedToString (qualify emptyCollection.isEmptyFn defaultQualifyResources)
            in
            Just
                (Rule.errorWithFix
                    { message = "Comparison with an empty " ++ emptyCollection.represents ++ " can be replaced by a call to " ++ modIsEmpty
                    , details = [ "You can replace this comparison to an empty " ++ emptyCollection.represents ++ " with a call to " ++ modIsEmpty ++ ", which is more efficient." ]
                    }
                    (Range.combine [ operatorRange, Node.range arg ])
                    [ Fix.replaceRangeBy
                        { start = operatorRange.start
                        , end = (Node.range arg).end
                        }
                        modIsEmpty
                    ]
                )

        Nothing ->
            Nothing


{-| If the expression is [], Array.empty, Set.empty or Dict.empty, return
the associated collection name and isEmpty reference.
-}
getEmptyCollection : ModuleNameLookupTable -> Node Expression -> Maybe { represents : String, isEmptyFn : ( ModuleName, String ) }
getEmptyCollection lookupTable node =
    case AstHelpers.removeParens node of
        Node _ (Expression.ListExpr []) ->
            Just { represents = "list", isEmptyFn = Fn.List.isEmpty }

        Node range (Expression.FunctionOrValue _ "empty") ->
            case ModuleNameLookupTable.moduleNameAt lookupTable range of
                Just moduleOrigin ->
                    case moduleOrigin of
                        [ "Array" ] ->
                            Just { represents = "array", isEmptyFn = Fn.Array.isEmpty }

                        [ "Set" ] ->
                            Just { represents = "set", isEmptyFn = Fn.Set.isEmpty }

                        [ "Dict" ] ->
                            Just { represents = "dict", isEmptyFn = Fn.Dict.isEmpty }

                        _ ->
                            Nothing

                Nothing ->
                    Nothing

        _ ->
            Nothing



-- COMPARISONS


lessThanChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
lessThanChecks checkInfo =
    compareElementCountToSpecificIntOperationOneSidedChecks
        { isEmpty = True
        , specificInt = 1
        , operation = \fn -> fn ++ " < 1"
        , potentialElementCount = checkInfo.left
        , potentialInt = checkInfo.right
        }
        checkInfo
        |> onNothing
            (\() ->
                compareElementCountToSpecificIntOperationOneSidedChecks
                    { isEmpty = False
                    , specificInt = 0
                    , operation = \fn -> "0 < " ++ fn
                    , potentialElementCount = checkInfo.right
                    , potentialInt = checkInfo.left
                    }
                    checkInfo
            )
        |> onNothing
            (\() ->
                let
                    leftNormal : Expression
                    leftNormal =
                        Normalize.normalizeExpression checkInfo checkInfo.left

                    rightNormal : Expression
                    rightNormal =
                        Normalize.normalizeExpression checkInfo checkInfo.right
                in
                case Normalize.compareExistingNormals leftNormal rightNormal of
                    Normalize.ConfirmedEquality ->
                        -- also: NaN < NaN --> False
                        Just
                            (Rule.errorWithFix
                                { message =
                                    "(" ++ checkInfo.operator ++ ") with two equal operands results in False"
                                , details = [ "You can replace this call by False." ]
                                }
                                checkInfo.operatorRange
                                [ Fix.replaceRangeBy checkInfo.parentRange
                                    (qualifiedToString (qualify Fn.Basics.falseVariant checkInfo))
                                ]
                            )

                    _ ->
                        comparisonOperatorCheck
                            { leftNormal = leftNormal
                            , rightNormal = rightNormal
                            , orderSatisfiesOperator = \order -> order == LT
                            , boundsOrderToResult =
                                \boundsOrder ->
                                    case boundsOrder of
                                        BoundsLess CanBeEqualInOnlyOneNumber ->
                                            Nothing

                                        BoundsLess CannotBeEqual ->
                                            Just Fn.Basics.trueVariant

                                        BoundsGreater _ ->
                                            Just Fn.Basics.falseVariant
                            }
                            checkInfo
            )


lessThanOrEqualToChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
lessThanOrEqualToChecks checkInfo =
    compareElementCountToSpecificIntOperationOneSidedChecks
        { isEmpty = True
        , specificInt = 0
        , operation = \fn -> fn ++ " <= 0"
        , potentialElementCount = checkInfo.left
        , potentialInt = checkInfo.right
        }
        checkInfo
        |> onNothing
            (\() ->
                compareElementCountToSpecificIntOperationOneSidedChecks
                    { isEmpty = False
                    , specificInt = 1
                    , operation = \fn -> "1 <= " ++ fn
                    , potentialElementCount = checkInfo.right
                    , potentialInt = checkInfo.left
                    }
                    checkInfo
            )
        |> onNothing
            (\() ->
                let
                    leftNormal : Expression
                    leftNormal =
                        Normalize.normalizeExpression checkInfo checkInfo.left

                    rightNormal : Expression
                    rightNormal =
                        Normalize.normalizeExpression checkInfo checkInfo.right
                in
                case Normalize.compareExistingNormals leftNormal rightNormal of
                    Normalize.ConfirmedEquality ->
                        -- because: NaN <= NaN --> False
                        if
                            checkInfo.expectNaN
                                && (AstHelpers.couldBeValueContainingNaN checkInfo.left
                                        || AstHelpers.couldBeValueContainingNaN checkInfo.right
                                   )
                        then
                            Nothing

                        else
                            Just
                                (Rule.errorWithFix
                                    { message =
                                        "(" ++ checkInfo.operator ++ ") with two equal operands results in True"
                                    , details = [ "You can replace this call by True." ]
                                    }
                                    checkInfo.operatorRange
                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                        (qualifiedToString (qualify Fn.Basics.trueVariant checkInfo))
                                    ]
                                )

                    _ ->
                        comparisonOperatorCheck
                            { leftNormal = leftNormal
                            , rightNormal = rightNormal
                            , orderSatisfiesOperator =
                                \order ->
                                    case order of
                                        LT ->
                                            True

                                        EQ ->
                                            True

                                        GT ->
                                            False
                            , boundsOrderToResult =
                                \boundsOrder ->
                                    case boundsOrder of
                                        BoundsLess _ ->
                                            Just Fn.Basics.trueVariant

                                        BoundsGreater CannotBeEqual ->
                                            Just Fn.Basics.falseVariant

                                        BoundsGreater CanBeEqualInOnlyOneNumber ->
                                            Nothing
                            }
                            checkInfo
            )


greaterThanChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
greaterThanChecks checkInfo =
    compareElementCountToSpecificIntOperationOneSidedChecks
        { isEmpty = True
        , specificInt = 1
        , operation = \fn -> "1 > " ++ fn
        , potentialElementCount = checkInfo.right
        , potentialInt = checkInfo.left
        }
        checkInfo
        |> onNothing
            (\() ->
                compareElementCountToSpecificIntOperationOneSidedChecks
                    { isEmpty = False
                    , specificInt = 0
                    , operation = \fn -> fn ++ " > 0"
                    , potentialElementCount = checkInfo.left
                    , potentialInt = checkInfo.right
                    }
                    checkInfo
            )
        |> onNothing
            (\() ->
                let
                    leftNormal : Expression
                    leftNormal =
                        Normalize.normalizeExpression checkInfo checkInfo.left

                    rightNormal : Expression
                    rightNormal =
                        Normalize.normalizeExpression checkInfo checkInfo.right
                in
                case Normalize.compareExistingNormals leftNormal rightNormal of
                    Normalize.ConfirmedEquality ->
                        -- because: NaN > NaN --> True (unlike <)
                        if
                            checkInfo.expectNaN
                                && (AstHelpers.couldBeValueContainingNaN checkInfo.left
                                        || AstHelpers.couldBeValueContainingNaN checkInfo.right
                                   )
                        then
                            Nothing

                        else
                            Just
                                (Rule.errorWithFix
                                    { message =
                                        "(" ++ checkInfo.operator ++ ") with two equal operands results in False"
                                    , details = [ "You can replace this call by False." ]
                                    }
                                    checkInfo.operatorRange
                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                        (qualifiedToString (qualify Fn.Basics.falseVariant checkInfo))
                                    ]
                                )

                    _ ->
                        comparisonOperatorCheck
                            { leftNormal = leftNormal
                            , rightNormal = rightNormal
                            , orderSatisfiesOperator = \order -> order == GT
                            , boundsOrderToResult =
                                \boundsOrder ->
                                    case boundsOrder of
                                        BoundsGreater CanBeEqualInOnlyOneNumber ->
                                            Nothing

                                        BoundsGreater CannotBeEqual ->
                                            Just Fn.Basics.trueVariant

                                        BoundsLess _ ->
                                            Just Fn.Basics.falseVariant
                            }
                            checkInfo
            )


greaterThanOrEqualToChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
greaterThanOrEqualToChecks checkInfo =
    compareElementCountToSpecificIntOperationOneSidedChecks
        { isEmpty = True
        , specificInt = 0
        , operation = \fn -> "0 >= " ++ fn
        , potentialElementCount = checkInfo.right
        , potentialInt = checkInfo.left
        }
        checkInfo
        |> onNothing
            (\() ->
                compareElementCountToSpecificIntOperationOneSidedChecks
                    { isEmpty = False
                    , specificInt = 1
                    , operation = \fn -> fn ++ " >= 1"
                    , potentialElementCount = checkInfo.left
                    , potentialInt = checkInfo.right
                    }
                    checkInfo
            )
        |> onNothing
            (\() ->
                let
                    leftNormal : Expression
                    leftNormal =
                        Normalize.normalizeExpression checkInfo checkInfo.left

                    rightNormal : Expression
                    rightNormal =
                        Normalize.normalizeExpression checkInfo checkInfo.right
                in
                case Normalize.compareExistingNormals leftNormal rightNormal of
                    Normalize.ConfirmedEquality ->
                        -- also: NaN >= NaN --> True
                        Just
                            (Rule.errorWithFix
                                { message =
                                    "(" ++ checkInfo.operator ++ ") with two equal operands results in True"
                                , details = [ "You can replace this call by True." ]
                                }
                                checkInfo.operatorRange
                                [ Fix.replaceRangeBy checkInfo.parentRange
                                    (qualifiedToString (qualify Fn.Basics.trueVariant checkInfo))
                                ]
                            )

                    _ ->
                        comparisonOperatorCheck
                            { leftNormal = leftNormal
                            , rightNormal = rightNormal
                            , orderSatisfiesOperator =
                                \order ->
                                    case order of
                                        LT ->
                                            False

                                        EQ ->
                                            True

                                        GT ->
                                            True
                            , boundsOrderToResult =
                                \boundsOrder ->
                                    case boundsOrder of
                                        BoundsGreater _ ->
                                            Just Fn.Basics.trueVariant

                                        BoundsLess CanBeEqualInOnlyOneNumber ->
                                            Nothing

                                        BoundsLess CannotBeEqual ->
                                            Just Fn.Basics.falseVariant
                            }
                            checkInfo
            )


comparisonOperatorCheck :
    { leftNormal : Expression
    , rightNormal : Expression
    , orderSatisfiesOperator : Order -> Bool
    , boundsOrderToResult : BoundsOrder -> Maybe ( ModuleName, String )
    }
    -> OperatorApplicationCheckInfo
    -> Maybe (Error {})
comparisonOperatorCheck config checkInfo =
    case evaluateCompare checkInfo checkInfo.left checkInfo.right of
        Determined order ->
            let
                result : Bool
                result =
                    config.orderSatisfiesOperator order

                resultReference : ( ModuleName, String )
                resultReference =
                    if result then
                        Fn.Basics.trueVariant

                    else
                        Fn.Basics.falseVariant
            in
            Just
                (Rule.errorWithFix
                    { message =
                        "("
                            ++ checkInfo.operator
                            ++ ") with a left value "
                            ++ orderToCompareOperationDescription order
                            ++ " the right results in "
                            ++ qualifiedToString (qualify resultReference defaultQualifyResources)
                    , details =
                        [ "You can replace this call by "
                            ++ qualifiedToString (qualify resultReference defaultQualifyResources)
                            ++ "."
                        ]
                    }
                    checkInfo.operatorRange
                    [ Fix.replaceRangeBy checkInfo.parentRange
                        (qualifiedToString (qualify resultReference checkInfo))
                    ]
                )

        Undetermined ->
            compareNumberIntervalsResultsInOperationCheck
                { leftNormal = config.leftNormal
                , rightNormal = config.rightNormal
                , boundsOrderToResult = config.boundsOrderToResult
                }
                checkInfo


orderToCompareOperationDescription : Order -> String
orderToCompareOperationDescription order =
    case order of
        LT ->
            "less than"

        EQ ->
            "equal to"

        GT ->
            "greater than"


comparisonError : Bool -> QualifyResources { a | parentRange : Range, operator : String } -> Error {}
comparisonError bool checkInfo =
    let
        boolAsString : String
        boolAsString =
            AstHelpers.boolToString bool
    in
    Rule.errorWithFix
        { message = "(" ++ checkInfo.operator ++ ") comparison will result in " ++ boolAsString
        , details =
            [ "Based on the values and/or the context, we can determine the result. You can replace this operation by " ++ boolAsString ++ "."
            ]
        }
        checkInfo.parentRange
        [ Fix.replaceRangeBy checkInfo.parentRange
            (qualifiedToString (qualify ( [ "Basics" ], boolAsString ) checkInfo))
        ]



-- BASICS FUNCTIONS


basicsIdentityChecks : IntoFnCheck
basicsIdentityChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            Just
                (Rule.errorWithFix
                    { message = "`identity` should be removed"
                    , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
                    }
                    checkInfo.fnRange
                    (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle checkInfo.firstArg)
                )
        )


basicsIdentityCompositionErrorInfo : AstHelpers.IdentityKind -> { message : String, details : List String }
basicsIdentityCompositionErrorInfo identityKind =
    case identityKind of
        AstHelpers.IdentityFunction ->
            { message = "`identity` should be removed"
            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
            }

        AstHelpers.IdentityLambda ->
            { message = "The identity function should be removed"
            , details = [ "Composing a function with the identity function is the same as simplify referencing the function." ]
            }


basicsIdentityCompositionChecks : CompositionCheckInfo -> Maybe (Error {})
basicsIdentityCompositionChecks checkInfo =
    case AstHelpers.isIdentityWithKind checkInfo checkInfo.later.node of
        Just identityKind ->
            Just
                (Rule.errorWithFix
                    (basicsIdentityCompositionErrorInfo identityKind)
                    (Node.range checkInfo.later.node)
                    [ Fix.removeRange checkInfo.later.removeRange ]
                )

        Nothing ->
            case AstHelpers.isIdentityWithKind checkInfo checkInfo.earlier.node of
                Just identityKind ->
                    Just
                        (Rule.errorWithFix
                            (basicsIdentityCompositionErrorInfo identityKind)
                            (Node.range checkInfo.earlier.node)
                            [ Fix.removeRange checkInfo.earlier.removeRange ]
                        )

                Nothing ->
                    Nothing


basicsAlwaysChecks : IntoFnCheck
basicsAlwaysChecks =
    { call =
        \checkInfo ->
            case checkInfo.argsAfterFirst of
                [ _ ] ->
                    Just
                        (Rule.errorWithFix
                            { message = "Expression can be replaced by the first argument given to `always`"
                            , details = [ "The second argument will be ignored because of the `always` call." ]
                            }
                            checkInfo.fnRange
                            (replaceCallBySubExpressionFix
                                checkInfo.parentRange
                                checkInfo.callStyle
                                checkInfo.firstArg
                            )
                        )

                _ ->
                    Nothing
    , composition =
        \checkInfo ->
            case checkInfo.later.args of
                _ :: [] ->
                    Just
                        { info =
                            { message = "Function composed with always will be ignored"
                            , details = [ "`always` will swallow the function composed into it." ]
                            }
                        , fix =
                            [ Fix.removeRange checkInfo.earlier.removeRange ]
                        }

                _ ->
                    Nothing
    }


basicsNegateChecks : IntoFnCheck
basicsNegateChecks =
    toggleFnChecks


basicsNotChecks : IntoFnCheck
basicsNotChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall notOnKnownBoolCheck
        , toggleFnChecks
        , intoFnCheckOnlyCall isNotOnBooleanOperatorCheck
        , basicsNotOSpecificFnTakingFunctionIntoNotCanBeCombinedWithFunctionWithoutNotCheck
            { earlierFn = Fn.List.any
            , combinedFn = Fn.List.all
            }
        , basicsNotOSpecificFnTakingFunctionIntoNotCanBeCombinedWithFunctionWithoutNotCheck
            { earlierFn = Fn.List.all
            , combinedFn = Fn.List.any
            }
        ]


{-| The "operation taking function into Basics.not, then Basics.not" check:

    earlierFn (g << Basics.not) >> Basics.not
    --> combinedFn g

    earlierFn Basics.not >> Basics.not
    --> combinedFn identity

-}
basicsNotOSpecificFnTakingFunctionIntoNotCanBeCombinedWithFunctionWithoutNotCheck :
    { earlierFn : ( ModuleName, String )
    , combinedFn : ( ModuleName, String )
    }
    -> IntoFnCheck
basicsNotOSpecificFnTakingFunctionIntoNotCanBeCombinedWithFunctionWithoutNotCheck config =
    { composition =
        \checkInfo ->
            if checkInfo.earlier.fn == config.earlierFn then
                case checkInfo.earlier.args of
                    [ elementIsBadCheck ] ->
                        case getFunctionIntoNot checkInfo elementIsBadCheck of
                            Nothing ->
                                Nothing

                            Just functionIntoNot ->
                                Just
                                    { info =
                                        { message =
                                            qualifiedToString checkInfo.later.fn
                                                ++ " on "
                                                ++ qualifiedToString config.earlierFn
                                                ++ (if functionIntoNot.isStandaloneNot then
                                                        " "

                                                    else
                                                        " with a function into "
                                                   )
                                                ++ qualifiedToString Fn.Basics.not
                                                ++ " can be combined into "
                                                ++ qualifiedToString config.combinedFn
                                        , details =
                                            [ "You can replace this composition by "
                                                ++ qualifiedToString config.combinedFn
                                                ++ (if functionIntoNot.isStandaloneNot then
                                                        " " ++ qualifiedToString (qualify Fn.Basics.identity defaultQualifyResources)

                                                    else
                                                        " with the function given to "
                                                            ++ qualifiedToString config.earlierFn
                                                            ++ " before the "
                                                            ++ qualifiedToString Fn.Basics.not
                                                   )
                                                ++ "."
                                            ]
                                        }
                                    , fix =
                                        Fix.removeRange checkInfo.later.removeRange
                                            :: Fix.replaceRangeBy checkInfo.earlier.fnRange
                                                (qualifiedToString (qualify config.combinedFn checkInfo))
                                            :: functionIntoNot.removeFix
                                    }

                    _ ->
                        Nothing

            else
                Nothing
    , call =
        \checkInfo ->
            case AstHelpers.getSpecificUnreducedFnCall config.earlierFn checkInfo.lookupTable checkInfo.firstArg of
                Nothing ->
                    Nothing

                Just filterCall ->
                    case getFunctionIntoNot checkInfo filterCall.firstArg of
                        Nothing ->
                            Nothing

                        Just functionIntoNot ->
                            Just
                                (Rule.errorWithFix
                                    { message =
                                        qualifiedToString checkInfo.fn
                                            ++ " on "
                                            ++ qualifiedToString config.earlierFn
                                            ++ (if functionIntoNot.isStandaloneNot then
                                                    " "

                                                else
                                                    " with a function into "
                                               )
                                            ++ qualifiedToString Fn.Basics.not
                                            ++ " can be combined into "
                                            ++ qualifiedToString config.combinedFn
                                    , details =
                                        [ "You can replace this call by "
                                            ++ qualifiedToString config.combinedFn
                                            ++ (if functionIntoNot.isStandaloneNot then
                                                    " " ++ qualifiedToString (qualify Fn.Basics.identity defaultQualifyResources)

                                                else
                                                    " with the function given to "
                                                        ++ qualifiedToString config.earlierFn
                                                        ++ " before the "
                                                        ++ qualifiedToString Fn.Basics.not
                                               )
                                            ++ "."
                                        ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy filterCall.fnRange
                                        (qualifiedToString (qualify config.combinedFn checkInfo))
                                        :: replaceCallBySubExpressionFix checkInfo.parentRange
                                            checkInfo.callStyle
                                            checkInfo.firstArg
                                        ++ functionIntoNot.removeFix
                                    )
                                )
    }


notOnKnownBoolCheck : CallCheckInfo -> Maybe (Error {})
notOnKnownBoolCheck checkInfo =
    case Normalize.getBool checkInfo checkInfo.firstArg of
        Just bool ->
            let
                notBoolAsString : String
                notBoolAsString =
                    AstHelpers.boolToString (not bool)
            in
            Just
                (Rule.errorWithFix
                    { message = wrapInBackticks "not" ++ " on a bool known to be " ++ AstHelpers.boolToString bool ++ " can be replaced by " ++ notBoolAsString
                    , details = [ "You can replace this call by " ++ notBoolAsString ++ "." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange
                        (qualifiedToString (qualify ( [ "Basics" ], notBoolAsString ) checkInfo))
                    ]
                )

        Nothing ->
            Nothing


isNotOnBooleanOperatorCheck : CallCheckInfo -> Maybe (Error {})
isNotOnBooleanOperatorCheck checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.ParenthesizedExpression (Node _ (Expression.OperatorApplication operator _ (Node leftRange _) (Node rightRange _))) ->
            case isNegatableOperator operator of
                Just replacement ->
                    let
                        operatorRange : Range
                        operatorRange =
                            findOperatorRange
                                { operator = operator
                                , commentRanges = checkInfo.commentRanges
                                , extractSourceCode = checkInfo.extractSourceCode
                                , leftRange = leftRange
                                , rightRange = rightRange
                                }
                    in
                    Just
                        (Rule.errorWithFix
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `" ++ replacement ++ "` instead." ]
                            }
                            checkInfo.fnRange
                            [ Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                            , Fix.replaceRangeBy operatorRange replacement
                            ]
                        )

                Nothing ->
                    Nothing

        _ ->
            Nothing


isNegatableOperator : String -> Maybe String
isNegatableOperator op =
    case op of
        "<" ->
            Just ">="

        ">" ->
            Just "<="

        "<=" ->
            Just ">"

        ">=" ->
            Just "<"

        "==" ->
            Just "/="

        "/=" ->
            Just "=="

        _ ->
            Nothing


basicsToFloatChecks : IntoFnCheck
basicsToFloatChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            case AstHelpers.getUncomputedInt checkInfo.firstArg of
                Just _ ->
                    Just
                        (Rule.errorWithFix
                            { message = "Unnecessary " ++ qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " on a literal number"
                            , details =
                                [ "A literal integer is considered a number which means it can be used as both an Int and a Float and there is no need to explicitly convert it to a Float."
                                , "You can replace this function call by the literal number."
                                ]
                            }
                            checkInfo.fnRange
                            (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle checkInfo.firstArg)
                        )

                Nothing ->
                    Nothing
        )


floatToIntConversionChecks : (Float -> Int) -> IntoFnCheck
floatToIntConversionChecks operation =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo -> evaluateConversionToIntOnNumberCheck operation checkInfo)
        , onSpecificFnCallReturnsItsLastArgCheck Fn.Basics.toFloat
        ]


basicsAbsChecks : IntoFnCheck
basicsAbsChecks =
    intoFnChecksFirstThatConstructsError
        [ operationDoesNotChangeResultOfOperationCheck
        , absOnNegatedChecks
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case AstHelpers.getUncomputedNumberValue checkInfo.firstArg of
                    Nothing ->
                        Nothing

                    Just numberInput ->
                        let
                            result : Float
                            result =
                                Basics.abs numberInput
                        in
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " on a number literal can be evaluated"
                                , details =
                                    [ "You can replace this call by the resulting absolute value." ]
                                }
                                checkInfo.fnRange
                                (if result == numberInput then
                                    -- preserve style like 0x1 or 2e10
                                    replaceBySubExpressionFix checkInfo.parentRange checkInfo.firstArg

                                 else
                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                        (String.fromFloat result)
                                    ]
                                )
                            )
            )
        ]


absOnNegatedChecks : IntoFnCheck
absOnNegatedChecks =
    { composition =
        \checkInfo ->
            if checkInfo.earlier.fn == Fn.Basics.negate then
                Just
                    { info =
                        { message =
                            "Unnecessary "
                                ++ qualifiedToString Fn.Basics.negate
                                ++ " before "
                                ++ qualifiedToString checkInfo.later.fn
                        , details =
                            [ "You can replace this composition by "
                                ++ qualifiedToString (qualify Fn.Basics.negate defaultQualifyResources)
                                ++ "."
                            ]
                        }
                    , fix = [ Fix.removeRange checkInfo.earlier.removeRange ]
                    }

            else
                Nothing
    , call =
        \checkInfo ->
            case sameInAllBranches (\branch -> getNegated checkInfo.lookupTable branch) checkInfo.firstArg of
                Just branchesNegated ->
                    Just
                        (Rule.errorWithFix
                            { message =
                                qualifiedToString checkInfo.fn
                                    ++ " on a negated value makes the negation unnecessary"
                            , details =
                                [ "You can remove the negation of the value given to the "
                                    ++ qualifiedToString (qualify checkInfo.fn defaultQualifyResources)
                                    ++ " call."
                                ]
                            }
                            checkInfo.fnRange
                            (List.concatMap
                                (\branchNegated ->
                                    replaceBySubExpressionFix branchNegated.range branchNegated.inNegation
                                )
                                branchesNegated
                            )
                        )

                Nothing ->
                    Nothing
    }


getNegated : ModuleNameLookupTable -> Node Expression -> Maybe { range : Range, inNegation : Node Expression }
getNegated lookupTable expressionNode =
    case AstHelpers.removeParens expressionNode of
        Node _ (Expression.Negation inNegation) ->
            Just { range = Node.range expressionNode, inNegation = inNegation }

        _ ->
            Maybe.map (\negateCall -> { range = negateCall.nodeRange, inNegation = negateCall.firstArg })
                (AstHelpers.getSpecificUnreducedFnCall Fn.Basics.negate lookupTable expressionNode)


basicsMinChecks : IntoFnCheck
basicsMinChecks =
    intoFnChecksFirstThatConstructsError
        [ callWithTwoEqualArgumentsReturnsEitherArgumentCheck
            { represents = "value"
            , representsPlural = "values"
            }
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Nothing ->
                        Nothing

                    Just rightArg ->
                        case evaluateCompare checkInfo checkInfo.firstArg rightArg of
                            Determined LT ->
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " with a first value that is less than the second value results in the first value"
                                        , details = [ "You can replace this call by the its first argument." ]
                                        }
                                        checkInfo.fnRange
                                        (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle checkInfo.firstArg)
                                    )

                            Determined GT ->
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " with a first value that is greater than the second value results in the second value"
                                        , details = [ "You can replace this call by the its second argument." ]
                                        }
                                        checkInfo.fnRange
                                        (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle rightArg)
                                    )

                            _ ->
                                case Normalize.compare checkInfo checkInfo.firstArg (Node.empty (Expression.Negation rightArg)) of
                                    Normalize.ConfirmedEquality ->
                                        Just
                                            (Rule.errorWithFix
                                                { message = qualifiedToString checkInfo.fn ++ " with a first value that is equal to negative the second value results in the its negative absolute value"
                                                , details = [ "You can replace this call by the negated Basics.abs on either its first or second argument." ]
                                                }
                                                checkInfo.fnRange
                                                (Fix.replaceRangeBy checkInfo.fnRange
                                                    (qualifiedToString (qualify Fn.Basics.abs checkInfo))
                                                    :: keepOnlyAndSurroundWithFix
                                                        { parentRange = checkInfo.parentRange
                                                        , keep = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                                        , left = "-("
                                                        , right = ")"
                                                        }
                                                )
                                            )

                                    _ ->
                                        Nothing
            )
        ]


basicsMaxChecks : IntoFnCheck
basicsMaxChecks =
    intoFnChecksFirstThatConstructsError
        [ callWithTwoEqualArgumentsReturnsEitherArgumentCheck
            { represents = "value"
            , representsPlural = "values"
            }
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Nothing ->
                        Nothing

                    Just rightArg ->
                        case evaluateCompare checkInfo checkInfo.firstArg rightArg of
                            Determined GT ->
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " with a first value that is greater than the second value results in the first value"
                                        , details = [ "You can replace this call by the its first argument." ]
                                        }
                                        checkInfo.fnRange
                                        (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle checkInfo.firstArg)
                                    )

                            Determined LT ->
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " with a first value that is less than the second value results in the second value"
                                        , details = [ "You can replace this call by the its second argument." ]
                                        }
                                        checkInfo.fnRange
                                        (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle rightArg)
                                    )

                            _ ->
                                case Normalize.compare checkInfo checkInfo.firstArg (Node.empty (Expression.Negation rightArg)) of
                                    Normalize.ConfirmedEquality ->
                                        Just
                                            (Rule.errorWithFix
                                                { message = qualifiedToString checkInfo.fn ++ " with a first value that is equal to negative the second value results in the its absolute value"
                                                , details = [ "You can replace this call by Basics.abs on either its first or second argument." ]
                                                }
                                                checkInfo.fnRange
                                                (Fix.replaceRangeBy checkInfo.fnRange
                                                    (qualifiedToString (qualify Fn.Basics.abs checkInfo))
                                                    :: keepOnlyAndParenthesizeFix
                                                        { parentRange = checkInfo.parentRange
                                                        , keep = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                                        }
                                                )
                                            )

                                    _ ->
                                        Nothing
            )
        ]


basicsCompareChecks : IntoFnCheck
basicsCompareChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            case secondArg checkInfo of
                Nothing ->
                    Nothing

                Just rightArg ->
                    case Normalize.compare checkInfo checkInfo.firstArg rightArg of
                        Normalize.ConfirmedEquality ->
                            -- because: compare NaN NaN --> GT
                            if
                                checkInfo.expectNaN
                                    && (AstHelpers.couldBeValueContainingNaN checkInfo.firstArg
                                            || AstHelpers.couldBeValueContainingNaN rightArg
                                       )
                            then
                                Nothing

                            else
                                Just
                                    (Rule.errorWithFix
                                        { message =
                                            qualifiedToString checkInfo.fn ++ " with two equal arguments results in EQ"
                                        , details = [ "You can replace this call by EQ." ]
                                        }
                                        checkInfo.fnRange
                                        [ Fix.replaceRangeBy checkInfo.parentRange
                                            (qualifiedToString (qualify Fn.Basics.eQVariant checkInfo))
                                        ]
                                    )

                        _ ->
                            case evaluateCompare checkInfo checkInfo.firstArg rightArg of
                                Undetermined ->
                                    Nothing

                                Determined result ->
                                    let
                                        resultReference : ( ModuleName, String )
                                        resultReference =
                                            orderToReference result

                                        resultAsString : String
                                        resultAsString =
                                            qualifiedToString (qualify resultReference checkInfo)
                                    in
                                    Just
                                        (Rule.errorWithFix
                                            { message =
                                                qualifiedToString checkInfo.fn
                                                    ++ " with a left value "
                                                    ++ orderToCompareOperationDescription result
                                                    ++ " the right results in "
                                                    ++ qualifiedToString (qualify resultReference defaultQualifyResources)
                                            , details = [ "You can replace this call by " ++ resultAsString ++ "." ]
                                            }
                                            checkInfo.fnRange
                                            [ Fix.replaceRangeBy checkInfo.parentRange resultAsString
                                            ]
                                        )
        )


orderToReference : Basics.Order -> ( ModuleName, String )
orderToReference order =
    case order of
        LT ->
            Fn.Basics.lTVariant

        EQ ->
            Fn.Basics.eQVariant

        GT ->
            Fn.Basics.gTVariant


evaluateCompare : Normalize.Resources a -> Node Expression -> Node Expression -> Match Order
evaluateCompare resources left right =
    case expressionToComparable resources left of
        Just leftComparable ->
            case expressionToComparable resources right of
                Nothing ->
                    Undetermined

                Just rightComparable ->
                    compareComparableExpressions leftComparable rightComparable

        Nothing ->
            Undetermined


{-| If you just want to check for equality, use == or better
if possible skip the "to comparable" and use Normalize.areTheSame
-}
compareComparableExpressions : ComparableExpression -> ComparableExpression -> Match Order
compareComparableExpressions left right =
    case left of
        ComparableNumber leftNumber ->
            case right of
                ComparableNumber rightNumber ->
                    Determined (compare leftNumber rightNumber)

                _ ->
                    Undetermined

        ComparableChar leftChar ->
            case right of
                ComparableChar rightChar ->
                    Determined (compare leftChar rightChar)

                _ ->
                    Undetermined

        ComparableString leftString ->
            case right of
                ComparableString rightString ->
                    Determined (compare leftString rightString)

                _ ->
                    Undetermined

        ComparableTuple leftFirst leftSecond ->
            case right of
                ComparableTuple rightFirst rightSecond ->
                    case compareComparableExpressions leftFirst rightFirst of
                        Determined EQ ->
                            compareComparableExpressions leftSecond rightSecond

                        firstOrderMatchNotEqual ->
                            firstOrderMatchNotEqual

                _ ->
                    Undetermined

        ComparableTriple leftFirst leftSecond leftThird ->
            case right of
                ComparableTriple rightFirst rightSecond rightThird ->
                    case compareComparableExpressions leftFirst rightFirst of
                        Determined EQ ->
                            case compareComparableExpressions leftSecond rightSecond of
                                Determined EQ ->
                                    compareComparableExpressions leftThird rightThird

                                firstOrderMatchNotEqual ->
                                    firstOrderMatchNotEqual

                        firstOrderMatchNotEqual ->
                            firstOrderMatchNotEqual

                _ ->
                    Undetermined

        ComparableList leftList ->
            case right of
                ComparableList rightList ->
                    compareComparableExpressionLists leftList rightList

                _ ->
                    Undetermined


compareComparableExpressionLists : List ComparableExpression -> List ComparableExpression -> Match Order
compareComparableExpressionLists leftList rightList =
    case leftList of
        [] ->
            Determined
                (case rightList of
                    [] ->
                        EQ

                    _ :: _ ->
                        LT
                )

        leftHead :: leftTail ->
            case rightList of
                [] ->
                    Determined GT

                rightHead :: rightTail ->
                    case compareComparableExpressions leftHead rightHead of
                        Determined EQ ->
                            compareComparableExpressionLists leftTail rightTail

                        headOrderMatchNotEqual ->
                            headOrderMatchNotEqual


evaluateConversionToIntOnNumberCheck : (Float -> Int) -> CallCheckInfo -> Maybe (Error {})
evaluateConversionToIntOnNumberCheck operation checkInfo =
    case AstHelpers.getUncomputedInt checkInfo.firstArg of
        Just _ ->
            Just
                (Rule.errorWithFix
                    { message = "Unnecessary integer conversion on a literal integer"
                    , details =
                        [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                        , "You can replace this function call by the literal integer."
                        ]
                    }
                    checkInfo.fnRange
                    (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle checkInfo.firstArg)
                )

        Nothing ->
            -- this is a superset of the first rule.
            -- The first check only exists to preserve int style like
            --     toInt 0x1 --> 0x1
            case AstHelpers.getUncomputedNumberValue checkInfo.firstArg of
                Just number ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " on a number literal can be evaluated"
                            , details =
                                [ "You can replace this call by the resulting Int value."
                                ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                (String.fromInt (operation number))
                            ]
                        )

                Nothing ->
                    Nothing


orChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
orChecks checkInfo =
    checkOperationFromBothSides checkInfo
        (\side -> unnecessaryOperationWithEmptySideChecks boolForOrProperties side checkInfo)
        |> onNothing
            (\() ->
                checkOperationFromBothSides checkInfo
                    (\side -> operationWithAbsorbingSideChecks boolForOrProperties side checkInfo)
            )
        |> onNothing
            (\() -> findSimilarConditionsError checkInfo)


andChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
andChecks checkInfo =
    checkOperationFromBothSides checkInfo
        (\side -> unnecessaryOperationWithEmptySideChecks boolForAndProperties side checkInfo)
        |> onNothing
            (\() ->
                checkOperationFromBothSides checkInfo
                    (\side -> operationWithAbsorbingSideChecks boolForAndProperties side checkInfo)
            )
        |> onNothing
            (\() -> findSimilarConditionsError checkInfo)


unnecessaryOperationWithEmptySideChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> { side | node : Node Expression, otherNode : Node Expression, otherDescription : String } -> OperatorApplicationCheckInfo -> Maybe (Error {})
unnecessaryOperationWithEmptySideChecks forOperationProperties side checkInfo =
    if isInTypeSubset forOperationProperties.empty checkInfo side.node then
        Just
            (Rule.errorWithFix
                { message = "Unnecessary " ++ checkInfo.operator ++ " " ++ typeSubsetDescriptionIndefinite forOperationProperties.empty
                , details = [ "You can replace this operation by the " ++ side.otherDescription ++ " " ++ forOperationProperties.represents ++ "." ]
                }
                (Range.combine [ checkInfo.operatorRange, Node.range side.node ])
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.otherNode })
            )

    else
        Nothing


operationWithAbsorbingSideChecks : TypeProperties (AbsorbableProperties otherProperties) -> { side | node : Node Expression, otherNode : Node Expression, otherDescription : String } -> OperatorApplicationCheckInfo -> Maybe (Error {})
operationWithAbsorbingSideChecks forOperationProperties side checkInfo =
    if forOperationProperties.absorbing.is (extractNormalizeResources checkInfo) side.node then
        Just
            (Rule.errorWithFix
                { message = "(" ++ checkInfo.operator ++ ") with any side being " ++ forOperationProperties.absorbing.description ++ " will result in " ++ forOperationProperties.absorbing.description
                , details =
                    [ "You can replace this operation by " ++ forOperationProperties.absorbing.asString defaultQualifyResources ++ "."
                    , "Maybe you have hardcoded a value or mistyped a condition?"
                    ]
                }
                checkInfo.parentRange
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.node })
            )

    else
        Nothing


type RedundantConditionResolution
    = RemoveFrom Location
    | ReplaceByNoop Bool


findSimilarConditionsError : OperatorApplicationCheckInfo -> Maybe (Error {})
findSimilarConditionsError operatorCheckInfo =
    let
        conditionsOnTheRight : List ( RedundantConditionResolution, Node Expression )
        conditionsOnTheRight =
            listConditions
                operatorCheckInfo.operator
                (RemoveFrom operatorCheckInfo.leftRange.end)
                operatorCheckInfo.right
    in
    operatorCheckInfo.left
        |> listConditions operatorCheckInfo.operator (RemoveFrom operatorCheckInfo.leftRange.end)
        |> findMap
            (\( _, condition ) ->
                findMap
                    (areSimilarConditionsError
                        operatorCheckInfo
                        operatorCheckInfo.operator
                        condition
                    )
                    conditionsOnTheRight
            )


areSimilarConditionsError :
    QualifyResources (Normalize.Resources a)
    -> String
    -> Node Expression
    -> ( RedundantConditionResolution, Node Expression )
    -> Maybe (Error {})
areSimilarConditionsError resources operator nodeToCompareTo ( redundantConditionResolution, nodeToLookAt ) =
    case Normalize.compare resources nodeToCompareTo nodeToLookAt of
        Normalize.ConfirmedEquality ->
            Just (errorForRedundantCondition operator redundantConditionResolution nodeToLookAt resources)

        Normalize.ConfirmedInequality ->
            Nothing

        Normalize.Unconfirmed ->
            Nothing


errorForRedundantCondition : String -> RedundantConditionResolution -> Node a -> QualifyResources b -> Error {}
errorForRedundantCondition operator redundantConditionResolution node qualifyResources =
    let
        ( range, fix ) =
            rangeAndFixForRedundantCondition redundantConditionResolution node qualifyResources
    in
    Rule.errorWithFix
        { message = "Condition is redundant"
        , details =
            [ "This condition is the same as another one found on the left side of the (" ++ operator ++ ") operator, therefore one of them can be removed."
            ]
        }
        range
        fix


rangeAndFixForRedundantCondition : RedundantConditionResolution -> Node a -> QualifyResources b -> ( Range, List Fix )
rangeAndFixForRedundantCondition redundantConditionResolution (Node nodeRange _) qualifyResources =
    case redundantConditionResolution of
        RemoveFrom locationOfPrevElement ->
            let
                range : Range
                range =
                    { start = locationOfPrevElement
                    , end = nodeRange.end
                    }
            in
            ( range
            , [ Fix.removeRange range ]
            )

        ReplaceByNoop noopValue ->
            ( nodeRange
            , [ Fix.replaceRangeBy nodeRange
                    (qualifiedToString (qualify ( [ "Basics" ], AstHelpers.boolToString noopValue ) qualifyResources))
              ]
            )


listConditions : String -> RedundantConditionResolution -> Node Expression -> List ( RedundantConditionResolution, Node Expression )
listConditions operatorToLookFor redundantConditionResolution expressionNode =
    case Node.value expressionNode of
        Expression.ParenthesizedExpression expr ->
            let
                noopValue : Bool
                noopValue =
                    operatorToLookFor == "&&"
            in
            listConditions operatorToLookFor (ReplaceByNoop noopValue) expr

        Expression.OperatorApplication operator _ left right ->
            if operator == operatorToLookFor then
                listConditions operatorToLookFor redundantConditionResolution left
                    ++ listConditions operatorToLookFor (RemoveFrom (Node.range left).end) right

            else
                [ ( redundantConditionResolution, expressionNode ) ]

        _ ->
            [ ( redundantConditionResolution, expressionNode ) ]



-- TUPLE FUNCTIONS


tuplePairChecks : IntoFnCheck
tuplePairChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            case checkInfo.argsAfterFirst of
                tuplePairCallSecondArg :: _ ->
                    let
                        firstRange : Range
                        firstRange =
                            Node.range checkInfo.firstArg

                        secondRange : Range
                        secondRange =
                            Node.range tuplePairCallSecondArg
                    in
                    case Range.compareLocations firstRange.end secondRange.start of
                        LT ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Fully constructed " ++ qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " can be replaced by tuple literal"
                                    , details = [ "You can replace this call by a tuple literal ( _, _ ). Consistently using ( _, _ ) to create a tuple is more idiomatic in elm." ]
                                    }
                                    checkInfo.fnRange
                                    (if checkInfo.parentRange.start.row /= checkInfo.parentRange.end.row then
                                        [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = firstRange.start }
                                            ("(\n" ++ String.repeat (firstRange.start.column - 1) " ")
                                        , Fix.replaceRangeBy { start = firstRange.end, end = secondRange.start }
                                            ("\n"
                                                ++ String.repeat (checkInfo.parentRange.start.column - 1) " "
                                                ++ ",\n"
                                                ++ String.repeat (secondRange.start.column - 1) " "
                                            )
                                        , Fix.replaceRangeBy { start = secondRange.end, end = checkInfo.parentRange.end }
                                            ("\n"
                                                ++ String.repeat (checkInfo.parentRange.start.column - 1) " "
                                                ++ ")"
                                            )
                                        ]

                                     else
                                        [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = firstRange.start } "( "
                                        , Fix.replaceRangeBy { start = firstRange.end, end = secondRange.start } ", "
                                        , Fix.replaceRangeBy { start = secondRange.end, end = checkInfo.parentRange.end } " )"
                                        ]
                                    )
                                )

                        EQ ->
                            Nothing

                        GT ->
                            Nothing

                [] ->
                    Nothing
        )


tupleFirstChecks : IntoFnCheck
tupleFirstChecks =
    intoFnChecksFirstThatConstructsError
        [ tuplePartChecks
            { part = TupleFirst
            , description = "first"
            , mapUnrelatedFn = Fn.Tuple.mapSecond
            , mapFn = Fn.Tuple.mapFirst
            , mapBothArgIndex = 0
            }
        , intoFnCheckOnlyComposition
            (\checkInfo ->
                case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
                    ( ( [ "Tuple" ], "pair" ), first :: [] ) ->
                        Just
                            { info =
                                { message = qualifiedToString (qualify checkInfo.earlier.fn defaultQualifyResources) ++ " with a first part, then " ++ qualifiedToString (qualify checkInfo.later.fn defaultQualifyResources) ++ " will always result in that first part"
                                , details = [ "You can replace this call by always with the first argument given to " ++ qualifiedToString (qualify checkInfo.earlier.fn defaultQualifyResources) ++ "." ]
                                }
                            , fix =
                                Fix.insertAt checkInfo.earlier.range.start
                                    (qualifiedToString (qualify Fn.Basics.always checkInfo) ++ " ")
                                    :: Fix.removeRange checkInfo.later.removeRange
                                    :: replaceBySubExpressionFix checkInfo.earlier.range first
                            }

                    _ ->
                        Nothing
            )
        , onSpecificFnCallCanBeCombinedCheck
            { args = [], earlierFn = Fn.List.partition, combinedFn = Fn.List.filter }
        , onSpecificFnCallCanBeCombinedCheck
            { args = [], earlierFn = Fn.Set.partition, combinedFn = Fn.Set.filter }
        , onSpecificFnCallCanBeCombinedCheck
            { args = [], earlierFn = Fn.Dict.partition, combinedFn = Fn.Dict.filter }
        ]


tupleSecondChecks : IntoFnCheck
tupleSecondChecks =
    intoFnChecksFirstThatConstructsError
        [ tuplePartChecks
            { part = TupleSecond
            , description = "second"
            , mapFn = Fn.Tuple.mapSecond
            , mapUnrelatedFn = Fn.Tuple.mapFirst
            , mapBothArgIndex = 1
            }
        , intoFnCheckOnlyComposition
            (\checkInfo ->
                case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
                    ( ( [ "Tuple" ], "pair" ), _ :: [] ) ->
                        Just
                            (compositionAlwaysReturnsIncomingError
                                (qualifiedToString (qualify checkInfo.earlier.fn defaultQualifyResources) ++ " with a first part, then " ++ qualifiedToString (qualify checkInfo.later.fn defaultQualifyResources) ++ " will always result in the incoming second part")
                                checkInfo
                            )

                    _ ->
                        Nothing
            )
        ]


type TuplePart
    = TupleFirst
    | TupleSecond


tuplePartChecks :
    { part : TuplePart
    , description : String
    , mapFn : ( ModuleName, String )
    , mapUnrelatedFn : ( ModuleName, String )
    , mapBothArgIndex : Int
    }
    -> IntoFnCheck
tuplePartChecks partConfig =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                Maybe.map
                    (\tuple ->
                        Rule.errorWithFix
                            { message = qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " on a known tuple will result in the tuple's " ++ partConfig.description ++ " part"
                            , details = [ "You can replace this call by the tuple's " ++ partConfig.description ++ " part." ]
                            }
                            checkInfo.fnRange
                            (replaceCallBySubExpressionFix
                                checkInfo.parentRange
                                checkInfo.callStyle
                                (case partConfig.part of
                                    TupleFirst ->
                                        tuple.first

                                    TupleSecond ->
                                        tuple.second
                                )
                            )
                    )
                    (AstHelpers.getTuple2 checkInfo.lookupTable checkInfo.firstArg)
            )
        , unnecessarySpecificFnBeforeCheck
            { fn = partConfig.mapUnrelatedFn
            , fnArgCount = 2
            , fnLastArgRepresents = "tuple"
            , whyUnnecessary = "Changing a tuple part which ultimately isn't accessed is unnecessary"
            }
        , tuplePartOnMapPartCheck partConfig.mapFn
        , tuplePartMapOnMapBothCheck
            { argIndex = partConfig.mapBothArgIndex }
        ]


tuplePartOnMapPartCheck : ( ModuleName, String ) -> IntoFnCheck
tuplePartOnMapPartCheck mapPartFn =
    { call =
        \checkInfo ->
            case AstHelpers.getSpecificUnreducedFnCall mapPartFn checkInfo.lookupTable checkInfo.firstArg of
                Just mapPartCall ->
                    case mapPartCall.argsAfterFirst of
                        [ tupleArg ] ->
                            let
                                tupleArgNeedsParens : Bool
                                tupleArgNeedsParens =
                                    needsParens (Node.value tupleArg)
                            in
                            Just
                                (Rule.errorWithFix
                                    { message =
                                        qualifiedToString checkInfo.fn
                                            ++ " on "
                                            ++ qualifiedToString mapPartFn
                                            ++ " can be replaced by directly calling the given function on the accessed tuple part"
                                    , details =
                                        [ "You can take the function argument of the the "
                                            ++ qualifiedToString mapPartFn
                                            ++ " call and call it with the accessed tuple part."
                                        ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.insertAt (Node.range tupleArg).start
                                        ("("
                                            ++ qualifiedToString (qualify checkInfo.fn checkInfo)
                                            ++ " "
                                            ++ (if tupleArgNeedsParens then
                                                    "("

                                                else
                                                    ""
                                               )
                                        )
                                        :: Fix.insertAt (Node.range tupleArg).end
                                            (")"
                                                ++ (if tupleArgNeedsParens then
                                                        ")"

                                                    else
                                                        ""
                                                   )
                                            )
                                        :: replaceBySubExpressionFix
                                            (Range.combine
                                                [ mapPartCall.fnRange
                                                , Node.range mapPartCall.firstArg
                                                ]
                                            )
                                            mapPartCall.firstArg
                                        ++ replaceCallBySubExpressionFix
                                            checkInfo.parentRange
                                            checkInfo.callStyle
                                            checkInfo.firstArg
                                    )
                                )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if checkInfo.earlier.fn == mapPartFn then
                case checkInfo.earlier.args of
                    [ partChangeFunctionArg ] ->
                        Just
                            { info =
                                { message =
                                    qualifiedToString checkInfo.later.fn
                                        ++ " on "
                                        ++ qualifiedToString mapPartFn
                                        ++ " can be replaced by directly calling the given function on the accessed tuple part"
                                , details =
                                    [ "You can take the function argument of the the "
                                        ++ qualifiedToString mapPartFn
                                        ++ " call and compose it after the tuple part access."
                                    ]
                                }
                            , fix =
                                Fix.removeRange checkInfo.later.removeRange
                                    :: composeWithEarlierFix
                                        { earlier = qualifiedToString (qualify checkInfo.later.fn checkInfo)
                                        , direction = compositionCheckInfoDirection checkInfo
                                        , range = checkInfo.earlier.range
                                        }
                                    ++ replaceBySubExpressionFix
                                        checkInfo.earlier.range
                                        partChangeFunctionArg
                            }

                    _ ->
                        Nothing

            else
                Nothing
    }


{-| For example with `{ earlier = "g" }`

    f
    --> (f << g)

    f
    --> (g >> f)

-}
composeWithEarlierFix :
    { direction : CallStyle.LeftOrRightDirection
    , range : Range
    , earlier : String
    }
    -> List Fix
composeWithEarlierFix config =
    case config.direction of
        CallStyle.LeftToRight ->
            [ Fix.insertAt config.range.start
                ("(" ++ config.earlier ++ " >> ")
            , Fix.insertAt config.range.end ")"
            ]

        CallStyle.RightToLeft ->
            [ Fix.insertAt config.range.start "("
            , Fix.insertAt config.range.end
                (" << " ++ config.earlier ++ ")")
            ]


fnCompositionString :
    { direction : CallStyle.LeftOrRightDirection
    , earlier : ( ModuleName, String )
    , later : ( ModuleName, String )
    }
    -> QualifyResources a
    -> String
fnCompositionString config resources =
    compositionString
        { earlier = qualifiedToString (qualify config.earlier resources)
        , later = qualifiedToString (qualify config.later resources)
        , direction = config.direction
        }


compositionString :
    { direction : CallStyle.LeftOrRightDirection, earlier : String, later : String }
    -> String
compositionString config =
    case config.direction of
        CallStyle.LeftToRight ->
            config.earlier ++ " >> " ++ config.later

        CallStyle.RightToLeft ->
            config.later ++ " << " ++ config.earlier


tuplePartMapOnMapBothCheck : { argIndex : Int } -> IntoFnCheck
tuplePartMapOnMapBothCheck config =
    { call =
        \checkInfo ->
            case AstHelpers.getSpecificUnreducedFnCall Fn.Tuple.mapBoth checkInfo.lookupTable checkInfo.firstArg of
                Just mapBothCall ->
                    case mapBothCall.argsAfterFirst of
                        [ _, tupleArg ] ->
                            case List.drop config.argIndex (mapBothCall.firstArg :: mapBothCall.argsAfterFirst) of
                                [] ->
                                    Nothing

                                partChangeFunctionArg :: _ ->
                                    Just
                                        (Rule.errorWithFix
                                            { message =
                                                qualifiedToString checkInfo.fn
                                                    ++ " on "
                                                    ++ qualifiedToString Fn.Tuple.mapBoth
                                                    ++ " can be replaced by directly calling the given function on the accessed tuple part"
                                            , details =
                                                [ "You can take the "
                                                    ++ indexthToString config.argIndex
                                                    ++ " function argument of the the "
                                                    ++ qualifiedToString Fn.Tuple.mapBoth
                                                    ++ " call and call it with the accessed tuple part."
                                                ]
                                            }
                                            checkInfo.fnRange
                                            (Fix.insertAt (Node.range tupleArg).start
                                                ("("
                                                    ++ qualifiedToString (qualify checkInfo.fn checkInfo)
                                                    ++ " "
                                                )
                                                :: Fix.insertAt (Node.range tupleArg).end
                                                    ")"
                                                :: callReplaceOperationBySubExpressionsFix
                                                    checkInfo.parentRange
                                                    { originalCallStyle = mapBothCall.callStyle
                                                    , subFunctionArg = partChangeFunctionArg
                                                    , lastArg = tupleArg
                                                    , parenthesize =
                                                        callReplacementNeedsParens checkInfo.callStyle (Node.value checkInfo.firstArg)
                                                    }
                                            )
                                        )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if
                (checkInfo.earlier.fn == Fn.Tuple.mapBoth)
                    && onlyLastArgIsCurried { args = checkInfo.earlier.args, argCount = 3 }
            then
                case List.drop config.argIndex checkInfo.earlier.args of
                    [] ->
                        Nothing

                    partChangeFunctionArg :: _ ->
                        Just
                            { info =
                                { message =
                                    qualifiedToString checkInfo.later.fn
                                        ++ " on "
                                        ++ qualifiedToString Fn.Tuple.mapBoth
                                        ++ " can be replaced by directly calling the given function on the accessed tuple part"
                                , details =
                                    [ "You can take the "
                                        ++ indexthToString config.argIndex
                                        ++ " function argument of the the "
                                        ++ qualifiedToString Fn.Tuple.mapBoth
                                        ++ " call and compose it after the tuple part access."
                                    ]
                                }
                            , fix =
                                -- Tuple.first << Tuple.mapFirst f
                                --> f << Tuple.first
                                --
                                -- Tuple.mapFirst f >> Tuple.first
                                --> Tuple.first >> f
                                Fix.removeRange checkInfo.later.removeRange
                                    :: (if Range.compareLocations checkInfo.earlier.range.start checkInfo.later.range.start == LT then
                                            [ Fix.insertAt checkInfo.earlier.range.start
                                                ("("
                                                    ++ qualifiedToString (qualify checkInfo.later.fn checkInfo)
                                                    ++ " >> "
                                                )
                                            , Fix.insertAt checkInfo.earlier.range.end ")"
                                            ]

                                        else
                                            [ Fix.insertAt checkInfo.earlier.range.start
                                                "("
                                            , Fix.insertAt checkInfo.earlier.range.end
                                                (" << "
                                                    ++ qualifiedToString (qualify checkInfo.later.fn checkInfo)
                                                    ++ ")"
                                                )
                                            ]
                                       )
                                    ++ replaceBySubExpressionFix
                                        checkInfo.earlier.range
                                        partChangeFunctionArg
                            }

            else
                Nothing
    }



-- STRING FUNCTIONS


stringFromListChecks : IntoFnCheck
stringFromListChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (callOnEmptyReturnsCheck { resultAsString = stringCollection.empty.specific.asString } listCollection)
        , wrapperFromListSingletonChecks stringCollection
        , onSpecificFnCallReturnsItsLastArgCheck Fn.String.toList
        ]


stringFromIntChecks : IntoFnCheck
stringFromIntChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            case Node.value checkInfo.firstArg of
                Expression.Integer i ->
                    let
                        str : String
                        str =
                            "\"" ++ String.fromInt i ++ "\""
                    in
                    Rule.errorWithFix
                        { message = "String.fromInt on " ++ String.fromInt i ++ " will result in " ++ str
                        , details = [ "You can replace this call by " ++ str ++ "." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy checkInfo.parentRange str ]
                        |> Just

                _ ->
                    Nothing
        )


stringFromFloatChecks : IntoFnCheck
stringFromFloatChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            case Node.value checkInfo.firstArg of
                Expression.Floatable f ->
                    let
                        str : String
                        str =
                            "\"" ++ String.fromFloat f ++ "\""
                    in
                    Rule.errorWithFix
                        { message = "String.fromFloat on " ++ String.fromFloat f ++ " will result in " ++ str
                        , details = [ "You can replace this call by " ++ str ++ "." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy checkInfo.parentRange str ]
                        |> Just

                _ ->
                    Nothing
        )


stringIsEmptyChecks : IntoFnCheck
stringIsEmptyChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionIsEmptyChecks stringCollection)
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.String.fromList
            , combinedFn = Fn.List.isEmpty
            }
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.String.reverse
            , fnArgCount = 1
            , fnLastArgRepresents = "string"
            , whyUnnecessary = "Reordering the chars in a string does not affect its length"
            }
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.String.map
            , fnArgCount = 2
            , fnLastArgRepresents = "string"
            , whyUnnecessary = "Changing each char in a string to another char can never make a non-empty string empty or an empty string non-empty"
            }
        ]


stringLengthChecks : IntoFnCheck
stringLengthChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionSizeChecks stringCollection)
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.String.reverse
            , fnArgCount = 1
            , fnLastArgRepresents = "string"
            , whyUnnecessary = "Reordering the chars in a string does not affect its length"
            }
        ]


stringSliceChecks : IntoFnCheck
stringSliceChecks =
    collectionSliceChecks stringCollection


stringReverseChecks : IntoFnCheck
stringReverseChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableReverseChecks stringCollection
        , unnecessaryOnWrappedCheck stringCollection
        ]


stringTrimLeftChecks : IntoFnCheck
stringTrimLeftChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck stringCollection
        , operationDoesNotChangeResultOfOperationCheck
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.String.trimRight
            , combinedFn = Fn.String.trim
            }
        , unnecessaryOnSpecificFnCallCheck Fn.String.trim
        ]


stringTrimRightChecks : IntoFnCheck
stringTrimRightChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck stringCollection
        , operationDoesNotChangeResultOfOperationCheck
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.String.trimLeft
            , combinedFn = Fn.String.trim
            }
        , unnecessaryOnSpecificFnCallCheck Fn.String.trim
        ]


stringTrimChecks : IntoFnCheck
stringTrimChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck stringCollection
        , operationDoesNotChangeResultOfOperationCheck
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.String.trimLeft
            , fnArgCount = 1
            , fnLastArgRepresents = "string"
            , whyUnnecessary =
                "Trimming from the start is already covered by the final "
                    ++ qualifiedToString Fn.String.trim
            }
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.String.trimRight
            , fnArgCount = 1
            , fnLastArgRepresents = "string"
            , whyUnnecessary =
                "Trimming from the end is already covered by the final "
                    ++ qualifiedToString Fn.String.trim
            }
        ]


stringToLowerChecks : IntoFnCheck
stringToLowerChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck stringCollection
        , operationDoesNotChangeResultOfOperationCheck
        ]


stringToUpperChecks : IntoFnCheck
stringToUpperChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck stringCollection
        , operationDoesNotChangeResultOfOperationCheck
        ]


stringLeftChecks : IntoFnCheck
stringLeftChecks =
    collectionTakeChecks stringCollection


stringRightChecks : IntoFnCheck
stringRightChecks =
    collectionTakeChecks stringCollection


stringDropLeftChecks : IntoFnCheck
stringDropLeftChecks =
    collectionDropChecks stringCollection


stringDropRightChecks : IntoFnCheck
stringDropRightChecks =
    collectionDropChecks stringCollection


stringFilterChecks : IntoFnCheck
stringFilterChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableKeepWhenChecks stringCollection
        , earlierOperationCanBeMovedAfterAsForPerformanceChecks
            { laterOperationArgsDescription = Just "function"
            , earlierFn = Fn.String.reverse
            , earlierFnArgCount = 1
            , earlierFnOperationArgsDescription = Nothing
            , asLaterFn = Fn.String.reverse
            }
        ]


stringMapChecks : IntoFnCheck
stringMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks stringCollection
        , mapOnWrappedChecks stringCollection
        , intoFnCheckOnlyCall stringMapOnRepeatFromCharCallCheck
        ]


stringMapOnRepeatFromCharCallCheck : CallCheckInfo -> Maybe (Error {})
stringMapOnRepeatFromCharCallCheck checkInfo =
    case checkInfo.argsAfterFirst of
        [ mappedArg ] ->
            case AstHelpers.getSpecificUnreducedFnCall Fn.String.repeat checkInfo.lookupTable mappedArg of
                Nothing ->
                    Nothing

                Just repeatFnCall ->
                    case repeatFnCall.argsAfterFirst of
                        [ stringToRepeatArg ] ->
                            case AstHelpers.getSpecificUnreducedFnCall Fn.String.fromChar checkInfo.lookupTable stringToRepeatArg of
                                Nothing ->
                                    Nothing

                                Just toRepeatStringFromCharCall ->
                                    Just
                                        (Rule.errorWithFix
                                            { message =
                                                qualifiedToString checkInfo.fn
                                                    ++ " on "
                                                    ++ qualifiedToString Fn.String.repeat
                                                    ++ " on "
                                                    ++ qualifiedToString Fn.String.fromChar
                                                    ++ " is the same as "
                                                    ++ qualifiedToString Fn.String.repeat
                                                    ++ " on "
                                                    ++ qualifiedToString Fn.String.fromChar
                                                    ++ " with the mapped char"
                                            , details =
                                                [ "You can replace this call by the "
                                                    ++ qualifiedToString Fn.String.repeat
                                                    ++ " on "
                                                    ++ qualifiedToString Fn.String.fromChar
                                                    ++ " operation but with the function given to the "
                                                    ++ qualifiedToString checkInfo.fn
                                                    ++ " operation applied to the original char."
                                                ]
                                            }
                                            checkInfo.fnRange
                                            (wrapInApplicationFix
                                                { arg = toRepeatStringFromCharCall.firstArg
                                                , function =
                                                    parenthesizeIf
                                                        (needsParens (Node.value checkInfo.firstArg))
                                                        (checkInfo.extractSourceCode
                                                            (Node.range checkInfo.firstArg)
                                                        )
                                                }
                                                ++ replaceCallBySubExpressionFix checkInfo.parentRange
                                                    checkInfo.callStyle
                                                    mappedArg
                                            )
                                        )

                        _ ->
                            Nothing

        _ ->
            Nothing


stringReplaceChecks : IntoFnCheck
stringReplaceChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck stringCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Just replacementArg ->
                        Maybe.andThen
                            (\stringArg ->
                                case ( checkInfo.firstArg, stringArg ) of
                                    ( Node _ (Expression.Literal toReplace), Node _ (Expression.Literal third) ) ->
                                        if not (String.contains "\u{000D}" toReplace) && not (String.contains toReplace third) then
                                            Just
                                                (Rule.errorWithFix
                                                    { message = "String.replace with a pattern not present in the given string will result in the given string"
                                                    , details = [ "You can replace this call by the given string itself." ]
                                                    }
                                                    checkInfo.fnRange
                                                    (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle stringArg)
                                                )

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
                            (thirdArg checkInfo)
                            |> onNothing
                                (\() ->
                                    case Normalize.compare checkInfo checkInfo.firstArg replacementArg of
                                        Normalize.ConfirmedEquality ->
                                            Just
                                                (alwaysReturnsLastArgError
                                                    (qualifiedToString checkInfo.fn ++ " where the pattern to replace and the replacement are equal")
                                                    stringCollection
                                                    checkInfo
                                                )

                                        _ ->
                                            Nothing
                                )

                    Nothing ->
                        Nothing
            )
        ]


stringAppendChecks : IntoFnCheck
stringAppendChecks =
    intoFnCheckOnlyCall (collectionUnionChecks { leftElementsStayOnTheLeft = True } stringCollection)


stringConcatChecks : IntoFnCheck
stringConcatChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableFlatFromListChecks stringCollection
        , onSpecificFnCallCanBeCombinedCheck { args = [], earlierFn = Fn.List.repeat, combinedFn = Fn.String.repeat }
        , onSpecificFnCallCanBeCombinedCheck { args = [], earlierFn = Fn.List.intersperse, combinedFn = Fn.String.join }
        ]


stringJoinChecks : IntoFnCheck
stringJoinChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            callOnEmptyReturnsCheck { resultAsString = stringCollection.empty.specific.asString } listCollection checkInfo
                |> onNothing
                    (\() ->
                        flatIntersperseWithEmptySeparatorIsEquivalentToFnCheck stringCollection Fn.String.concat checkInfo
                    )
        )


stringRepeatChecks : IntoFnCheck
stringRepeatChecks =
    intoFnCheckOnlyCall (emptiableFlatRepeatChecks stringCollection)


stringWordsChecks : IntoFnCheck
stringWordsChecks =
    intoFnCheckOnlyCall
        (callOnEmptyReturnsCheck { resultAsString = \_ -> "[ \"\" ]" } stringCollection)


stringLinesChecks : IntoFnCheck
stringLinesChecks =
    intoFnCheckOnlyCall
        (callOnEmptyReturnsCheck { resultAsString = \_ -> "[ \"\" ]" } stringCollection)


stringToListChecks : IntoFnCheck
stringToListChecks =
    onSpecificFnCallReturnsItsLastArgCheck Fn.String.fromList


stringUnconsChecks : IntoFnCheck
stringUnconsChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            callOnEmptyReturnsCheck
                { resultAsString = \res -> qualifiedToString (qualify Fn.Maybe.nothingVariant res) }
                stringCollection
                checkInfo
        )


stringAnyChecks : IntoFnCheck
stringAnyChecks =
    intoFnCheckOnlyCall
        (\checkInfo -> emptiableAnyChecks stringCollection checkInfo)


stringAllChecks : IntoFnCheck
stringAllChecks =
    intoFnCheckOnlyCall
        (\checkInfo -> emptiableAllChecks stringCollection checkInfo)


stringFoldlChecks : IntoFnCheck
stringFoldlChecks =
    intoFnCheckOnlyCall (emptiableFoldChecks stringCollection)


stringFoldrChecks : IntoFnCheck
stringFoldrChecks =
    intoFnCheckOnlyCall (emptiableFoldChecks stringCollection)



-- MAYBE FUNCTIONS


maybeMapChecks : IntoFnCheck
maybeMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks maybeWithJustAsWrap
        , mapOnWrappedChecks maybeWithJustAsWrap
        ]


maybeMapNChecks : IntoFnCheck
maybeMapNChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            wrapperMapNChecks maybeWithJustAsWrap checkInfo
                |> onNothing
                    (\() -> emptiableMapNChecks maybeWithJustAsWrap checkInfo)
        )


maybeAndThenChecks : IntoFnCheck
maybeAndThenChecks =
    intoFnChecksFirstThatConstructsError
        [ wrapperFlatMapChecks maybeWithJustAsWrap
        , emptiableFlatMapChecks maybeWithJustAsWrap
        ]


maybeWithDefaultChecks : IntoFnCheck
maybeWithDefaultChecks =
    intoFnChecksFirstThatConstructsError
        [ withDefaultChecks maybeWithJustAsWrap
        , onSpecificFnCallCanBeCombinedCheck
            { earlierFn = Fn.Maybe.map
            , args = [ maybeNothingProperties ]
            , combinedFn = Fn.Maybe.andThen
            }
        ]



-- RESULT FUNCTIONS


resultMapChecks : IntoFnCheck
resultMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks resultWithOkAsWrap
        , mapOnWrappedChecks resultWithOkAsWrap
        ]


resultMapNChecks : IntoFnCheck
resultMapNChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            wrapperMapNChecks resultWithOkAsWrap checkInfo
                |> onNothing
                    (\() -> mapNOrFirstEmptyConstructionChecks resultWithOkAsWrap checkInfo)
        )


mapWrapErrorInfo :
    ( ModuleName, String )
    -> WrapperProperties otherProperties
    -> { message : String, details : List String }
mapWrapErrorInfo mapFn wrapper =
    let
        wrapFnInErrorInfo : String
        wrapFnInErrorInfo =
            qualifiedToString (qualify wrapper.wrap.fn defaultQualifyResources)
    in
    { message = qualifiedToString mapFn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will result in " ++ wrapFnInErrorInfo ++ " with the function applied to the value inside"
    , details = [ "You can replace this call by " ++ wrapFnInErrorInfo ++ " with the function directly applied to the value inside " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description ++ " itself." ]
    }


resultMapErrorChecks : IntoFnCheck
resultMapErrorChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks resultWithErrAsWrap
        , mapOnWrappedChecks resultWithErrAsWrap
        ]


resultAndThenChecks : IntoFnCheck
resultAndThenChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck resultWithOkAsWrap
        , wrapperFlatMapChecks resultWithOkAsWrap
        ]


resultWithDefaultChecks : IntoFnCheck
resultWithDefaultChecks =
    withDefaultChecks resultWithOkAsWrap


resultToMaybeChecks : IntoFnCheck
resultToMaybeChecks =
    intoFnChecksFirstThatConstructsError
        [ unwrapToMaybeChecks resultWithOkAsWrap
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.Result.mapError
            , fnArgCount = 2
            , fnLastArgRepresents = "result"
            , whyUnnecessary = qualifiedToString Fn.Result.toMaybe ++ " converts any error value to Nothing, so changing that value is unnecessary"
            }
        , intoFnCheckOnlyComposition
            (\checkInfo ->
                case checkInfo.earlier.fn of
                    ( [ "Result" ], "Err" ) ->
                        Just
                            { info =
                                { message = qualifiedToString Fn.Result.toMaybe ++ " on an error will result in Nothing"
                                , details = [ "You can replace this call by always Nothing." ]
                                }
                            , fix =
                                compositionReplaceByFix
                                    (qualifiedToString (qualify Fn.Basics.always checkInfo)
                                        ++ " "
                                        ++ qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo)
                                    )
                                    checkInfo
                            }

                    _ ->
                        Nothing
            )
        ]


resultFromMaybeWithEmptyValueOnNothingCheck : IntoFnCheck
resultFromMaybeWithEmptyValueOnNothingCheck =
    fromMaybeWithEmptyValueOnNothingCheck resultWithOkAsWrap



-- LIST FUNCTIONS


listAppendChecks : IntoFnCheck
listAppendChecks =
    intoFnCheckOnlyCall
        (collectionUnionChecks { leftElementsStayOnTheLeft = True } listCollection)


listConcatChecks : IntoFnCheck
listConcatChecks =
    intoFnChecksFirstThatConstructsError
        [ onSpecificFnCallCanBeCombinedCheck
            { args = [], earlierFn = Fn.List.map, combinedFn = Fn.List.concatMap }
        , emptiableFlatFromListChecks listCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case fromListGetLiteral listCollection checkInfo.lookupTable checkInfo.firstArg of
                    Just listLiteral ->
                        let
                            (Node firstArgRange _) =
                                checkInfo.firstArg
                        in
                        if
                            (firstArgRange.start.row /= firstArgRange.end.row)
                                && not (List.all (\(Node itemRange _) -> itemRange.start.row /= itemRange.end.row) listLiteral.elements)
                        then
                            Nothing

                        else if List.all AstHelpers.isListLiteral listLiteral.elements then
                            Just
                                (Rule.errorWithFix
                                    { message = "Expression could be simplified to be a single List"
                                    , details = [ "Try moving all the elements into a single list." ]
                                    }
                                    checkInfo.fnRange
                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = firstArgRange }
                                        ++ List.concatMap removeBoundariesFix listLiteral.elements
                                    )
                                )

                        else
                            mergeConsecutiveFromListLiteralsCheck listCollection listLiteral.elements checkInfo

                    Nothing ->
                        Nothing
            )
        ]


listConcatMapChecks : IntoFnCheck
listConcatMapChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (operationWithIdentityIsEquivalentToFnCheck Fn.List.concat)
        , emptiableFlatMapChecks listCollection
        , wrapperFlatMapChecks listCollection
        ]


listIndexedMapChecks : IntoFnCheck
listIndexedMapChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck listCollection
        , intoFnCheckOnlyCall (operationWithExtraArgChecks { operationWithoutExtraArg = Fn.List.map })
        ]


listIntersperseChecks : IntoFnCheck
listIntersperseChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck listCollection
        , unnecessaryOnWrappedCheck listCollection
        ]


listHeadChecks : IntoFnCheck
listHeadChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.specific.asString } listCollection checkInfo
                    |> onNothing
                        (\() ->
                            Maybe.map
                                (\listArgHead ->
                                    Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " on a list with a first element will result in Just that element"
                                        , details = [ "You can replace this call by Just the first list element." ]
                                        }
                                        checkInfo.fnRange
                                        (Fix.replaceRangeBy checkInfo.fnRange
                                            (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo))
                                            :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) listArgHead
                                        )
                                )
                                (getListHead checkInfo.lookupTable checkInfo.firstArg)
                        )
                    |> onNothing
                        (\() ->
                            AstHelpers.getSpecificUnreducedFnCall Fn.List.reverse checkInfo.lookupTable checkInfo.firstArg
                                |> Maybe.andThen
                                    (\reverseCall ->
                                        AstHelpers.getSpecificUnreducedFnCall Fn.List.sort checkInfo.lookupTable reverseCall.firstArg
                                            |> Maybe.map
                                                (\sortCall ->
                                                    Rule.errorWithFix
                                                        { message = "List.sort, then List.reverse, then List.head can be combined into List.maximum"
                                                        , details = [ "You can replace this call by List.maximum with the same list given to List.sort which is meant for this exact purpose." ]
                                                        }
                                                        checkInfo.fnRange
                                                        (Fix.replaceRangeBy checkInfo.fnRange
                                                            (qualifiedToString (qualify Fn.List.maximum checkInfo))
                                                            :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) sortCall.firstArg
                                                        )
                                                )
                                    )
                        )
            )
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.List.sort
            , combinedFn = Fn.List.minimum
            }
        , earlierOperationCanBeMovedAfterAsForPerformanceChecks
            { laterOperationArgsDescription = Nothing
            , earlierFn = Fn.List.map
            , earlierFnArgCount = 2
            , earlierFnOperationArgsDescription = Just "function"
            , asLaterFn = Fn.Maybe.map
            }
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.List.intersperse
            , fnArgCount = 2
            , fnLastArgRepresents = "list"
            , whyUnnecessary = "Interspersed elements will only appear from the second element onward, and an empty list will remain empty, so the head will be unchanged"
            }
        , onRepeatReturnsWrapElementIfCountIsPositiveOtherwiseAlwaysFn
            { repeatFn = Fn.List.repeat
            , wrapFnOnRepeatCountPositive = Fn.Maybe.justVariant
            , fnOnRepeatCountNotPositive = Fn.Maybe.nothingVariant
            }
        ]


{-| The reorder checks for optimization:

    later a (earlier b subject)
    --> asLater b (later a subject)

    later a << earlier b
    --> asLater b >> later a

so for example with `{ earlierFn = Fn.List.take, asLaterFn = Fn.List.take }`:

    List.take x (List.map f list)
    --> List.map f (List.take n list)

or with `{ earlierFn = Fn.List.take, asLaterFn = Fn.Maybe.map }`:

    List.head (List.map f list)
    --> Maybe.map f (List.head list)

-}
earlierOperationCanBeMovedAfterAsForPerformanceChecks :
    { laterOperationArgsDescription : Maybe String
    , earlierFn : ( ModuleName, String )
    , earlierFnArgCount : Int
    , earlierFnOperationArgsDescription : Maybe String
    , asLaterFn : ( ModuleName, String )
    }
    -> IntoFnCheck
earlierOperationCanBeMovedAfterAsForPerformanceChecks config =
    { composition =
        \checkInfo ->
            if
                (checkInfo.earlier.fn == config.earlierFn)
                    && onlyLastArgIsCurried checkInfo.later
            then
                Just
                    (compositionEarlierOperationCanBeMovedAfterAsForPerformanceError
                        { generalLaterOperationArgsDescription = config.laterOperationArgsDescription
                        , specificLaterOperationArgsDescription = Nothing
                        , earlierFnOperationArgsDescription = config.earlierFnOperationArgsDescription
                        , asLaterFn = config.asLaterFn
                        }
                        checkInfo
                    )

            else
                Nothing
    , call =
        \checkInfo ->
            case fullyAppliedLastArg checkInfo of
                Nothing ->
                    Nothing

                Just laterLastArg ->
                    case AstHelpers.getSpecificUnreducedFnCall config.earlierFn checkInfo.lookupTable laterLastArg of
                        Nothing ->
                            Nothing

                        Just earlierFnCall ->
                            case fullyAppliedLastArg { firstArg = earlierFnCall.firstArg, argsAfterFirst = earlierFnCall.argsAfterFirst, argCount = config.earlierFnArgCount } of
                                Nothing ->
                                    Nothing

                                Just earlierFnCallLastArg ->
                                    Just
                                        (callEarlierOperationCanBeMovedAfterAsForPerformanceError
                                            { laterOperationArgsDescription = config.laterOperationArgsDescription
                                            , earlierFn = config.earlierFn
                                            , specificLaterOperationDescription = Nothing
                                            , earlierFnOperationArgsDescription = config.earlierFnOperationArgsDescription
                                            , asLaterFn = config.asLaterFn
                                            , laterLastArg = laterLastArg
                                            , earlierFnRange = earlierFnCall.fnRange
                                            , earlierFnCallStyle = earlierFnCall.callStyle
                                            , earlierFnCallLastArg = earlierFnCallLastArg
                                            }
                                            checkInfo
                                        )
    }


{-| Like `earlierOperationCanBeMovedAfterAsForPerformanceChecks`
with an extra condition for the first argument of the later operation
and no change in operation fn while swapping the operations.
-}
operationWithSpecificFirstArgOnSpecificFnCanBeOptimizedBySwappingOperationsChecks :
    { specificEarlierFn : ( ModuleName, String )
    , earlierFnOperationArgsDescription : String
    , isSpecificLaterFirstArg : Normalize.Resources {} -> Node Expression -> Bool
    , generalLaterOperationArgsDescription : String
    , specificLaterFirstArgDescription : String
    }
    -> IntoFnCheck
operationWithSpecificFirstArgOnSpecificFnCanBeOptimizedBySwappingOperationsChecks config =
    { composition =
        \checkInfo ->
            if checkInfo.earlier.fn == config.specificEarlierFn then
                case checkInfo.later.args of
                    [] ->
                        Nothing

                    laterFirstArg :: _ ->
                        if config.isSpecificLaterFirstArg (extractNormalizeResources checkInfo) laterFirstArg then
                            Just
                                (compositionEarlierOperationCanBeMovedAfterAsForPerformanceError
                                    { generalLaterOperationArgsDescription = Just config.generalLaterOperationArgsDescription
                                    , specificLaterOperationArgsDescription =
                                        Just config.specificLaterFirstArgDescription
                                    , earlierFnOperationArgsDescription =
                                        Just config.earlierFnOperationArgsDescription
                                    , asLaterFn = config.specificEarlierFn
                                    }
                                    checkInfo
                                )

                        else
                            Nothing

            else
                Nothing
    , call =
        \checkInfo ->
            if config.isSpecificLaterFirstArg (extractNormalizeResources checkInfo) checkInfo.firstArg then
                case fullyAppliedLastArg checkInfo of
                    Nothing ->
                        Nothing

                    Just laterLastArg ->
                        case AstHelpers.getSpecificUnreducedFnCall config.specificEarlierFn checkInfo.lookupTable laterLastArg of
                            Nothing ->
                                Nothing

                            Just earlierFnCall ->
                                case fullyAppliedLastArg { firstArg = earlierFnCall.firstArg, argsAfterFirst = earlierFnCall.argsAfterFirst, argCount = 2 } of
                                    Nothing ->
                                        Nothing

                                    Just earlierFnCallLastArg ->
                                        Just
                                            (callEarlierOperationCanBeMovedAfterAsForPerformanceError
                                                { laterOperationArgsDescription = Just config.generalLaterOperationArgsDescription
                                                , earlierFn = config.specificEarlierFn
                                                , specificLaterOperationDescription =
                                                    Just config.specificLaterFirstArgDescription
                                                , earlierFnOperationArgsDescription =
                                                    Just config.earlierFnOperationArgsDescription
                                                , asLaterFn = config.specificEarlierFn
                                                , laterLastArg = laterLastArg
                                                , earlierFnRange = earlierFnCall.fnRange
                                                , earlierFnCallStyle = earlierFnCall.callStyle
                                                , earlierFnCallLastArg = earlierFnCallLastArg
                                                }
                                                checkInfo
                                            )

            else
                Nothing
    }


compositionEarlierOperationCanBeMovedAfterAsForPerformanceError :
    { generalLaterOperationArgsDescription : Maybe String
    , specificLaterOperationArgsDescription : Maybe String
    , earlierFnOperationArgsDescription : Maybe String
    , asLaterFn : ( ModuleName, String )
    }
    -> CompositionIntoCheckInfo
    -> ErrorInfoAndFix
compositionEarlierOperationCanBeMovedAfterAsForPerformanceError config checkInfo =
    { info =
        { message =
            (case config.specificLaterOperationArgsDescription of
                Just specificLaterOperationArgsDescription ->
                    qualifiedToString checkInfo.later.fn ++ " " ++ specificLaterOperationArgsDescription

                Nothing ->
                    qualifiedToString checkInfo.later.fn
            )
                ++ " on "
                ++ qualifiedToString checkInfo.earlier.fn
                ++ " can be optimized to "
                ++ qualifiedToString config.asLaterFn
                ++ " on "
                ++ qualifiedToString checkInfo.later.fn
        , details =
            [ "You can replace this composition by "
                ++ qualifiedToString checkInfo.later.fn
                ++ (case config.generalLaterOperationArgsDescription of
                        Nothing ->
                            ""

                        Just laterOperationArgsDescription ->
                            " with the "
                                ++ laterOperationArgsDescription
                                ++ " given to the original "
                                ++ qualifiedToString checkInfo.later.fn
                   )
                ++ ", then "
                ++ qualifiedToString config.asLaterFn
                ++ (case config.earlierFnOperationArgsDescription of
                        Nothing ->
                            ""

                        Just earlierFnOperationArgsDescription ->
                            " with the "
                                ++ earlierFnOperationArgsDescription
                                ++ " given to the original "
                                ++ qualifiedToString checkInfo.earlier.fn
                   )
                ++ "."
            ]
        }
    , fix =
        Fix.removeRange checkInfo.later.removeRange
            :: Fix.replaceRangeBy checkInfo.earlier.fnRange
                (qualifiedToString (qualify config.asLaterFn checkInfo))
            :: composeWithEarlierFix
                { earlier = checkInfo.extractSourceCode checkInfo.later.range
                , direction = compositionCheckInfoDirection checkInfo
                , range = checkInfo.earlier.range
                }
    }


callEarlierOperationCanBeMovedAfterAsForPerformanceError :
    { laterOperationArgsDescription : Maybe String
    , earlierFn : ( ModuleName, String )
    , specificLaterOperationDescription : Maybe String
    , earlierFnOperationArgsDescription : Maybe String
    , asLaterFn : ( ModuleName, String )
    , laterLastArg : Node Expression
    , earlierFnRange : Range
    , earlierFnCallStyle : FunctionCallStyle
    , earlierFnCallLastArg : Node Expression
    }
    -> CallCheckInfo
    -> Error {}
callEarlierOperationCanBeMovedAfterAsForPerformanceError config checkInfo =
    Rule.errorWithFix
        { message =
            (case config.specificLaterOperationDescription of
                Just specificLaterOperationDescription ->
                    qualifiedToString checkInfo.fn ++ " " ++ specificLaterOperationDescription

                Nothing ->
                    qualifiedToString checkInfo.fn
            )
                ++ " on "
                ++ qualifiedToString config.earlierFn
                ++ " can be optimized to "
                ++ qualifiedToString config.asLaterFn
                ++ " on "
                ++ qualifiedToString checkInfo.fn
        , details =
            [ "You can replace this call by "
                ++ qualifiedToString config.asLaterFn
                ++ (case config.earlierFnOperationArgsDescription of
                        Nothing ->
                            ""

                        Just earlierFnOperationArgsDescription ->
                            " with the "
                                ++ earlierFnOperationArgsDescription
                                ++ " given to the original "
                                ++ qualifiedToString config.earlierFn
                   )
                ++ ", on "
                ++ qualifiedToString checkInfo.fn
                ++ (case config.laterOperationArgsDescription of
                        Nothing ->
                            ""

                        Just laterOperationArgsDescription ->
                            " with the "
                                ++ laterOperationArgsDescription
                                ++ " given to the original "
                                ++ qualifiedToString checkInfo.fn
                   )
                ++ "."
            ]
        }
        checkInfo.fnRange
        (extractAndInsertCallAroundReplacementLastArgFix
            { extractSourceCode = checkInfo.extractSourceCode
            , originalCallRange = checkInfo.parentRange
            , originalCallStyle = checkInfo.callStyle
            , originalLastArgRange = Node.range config.laterLastArg
            , replacementLastArgRange = Node.range config.earlierFnCallLastArg
            , parenthesizeReplacementLastArg =
                lastArgOfCallNeedsParens checkInfo.callStyle
                    (Node.value config.earlierFnCallLastArg)
            , parenthesize =
                lastArgCallOfCallNeedsParens
                    { parent = config.earlierFnCallStyle, lastArg = checkInfo.callStyle }
            }
            ++ replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle config.laterLastArg
            |> consIf (config.earlierFn /= config.asLaterFn)
                (\() ->
                    Fix.replaceRangeBy config.earlierFnRange
                        (qualifiedToString (qualify config.asLaterFn checkInfo))
                )
        )


extractAndInsertCallAroundReplacementLastArgFix :
    { extractSourceCode : Range -> String
    , parenthesize : Bool
    , originalCallRange : Range
    , originalCallStyle : FunctionCallStyle
    , originalLastArgRange : Range
    , replacementLastArgRange : Range
    , parenthesizeReplacementLastArg : Bool
    }
    -> List Fix
extractAndInsertCallAroundReplacementLastArgFix config =
    case config.originalCallStyle of
        CallStyle.Pipe CallStyle.LeftToRight ->
            [ Fix.insertAt config.replacementLastArgRange.start
                ((if config.parenthesize then
                    "("

                  else
                    ""
                 )
                    ++ (if config.parenthesizeReplacementLastArg then
                            "("

                        else
                            ""
                       )
                )
            , Fix.insertAt config.replacementLastArgRange.end
                ((if config.parenthesizeReplacementLastArg then
                    ")"

                  else
                    ""
                 )
                    ++ config.extractSourceCode
                        { start = config.originalLastArgRange.end
                        , end = config.originalCallRange.end
                        }
                    ++ (if config.parenthesize then
                            ")"

                        else
                            ""
                       )
                )
            ]

        _ ->
            [ Fix.insertAt config.replacementLastArgRange.start
                ((if config.parenthesize then
                    "("

                  else
                    ""
                 )
                    ++ config.extractSourceCode
                        { start = config.originalCallRange.start
                        , end = config.originalLastArgRange.start
                        }
                    ++ (if config.parenthesizeReplacementLastArg then
                            "("

                        else
                            ""
                       )
                )
            , Fix.insertAt config.replacementLastArgRange.end
                ((if config.parenthesizeReplacementLastArg then
                    ")"

                  else
                    ""
                 )
                    ++ (if config.parenthesize then
                            ")"

                        else
                            ""
                       )
                )
            ]


getListHead : ModuleNameLookupTable -> Node Expression -> Maybe (Node Expression)
getListHead lookupTable expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.ListExpr (head :: _) ->
            Just head

        Expression.OperatorApplication "::" _ head _ ->
            Just head

        _ ->
            AstHelpers.getListSingleton lookupTable expressionNode


listTailExistsError : List Fix -> CallCheckInfo -> Error {}
listTailExistsError replaceListArgByTailFix checkInfo =
    Rule.errorWithFix
        { message = qualifiedToString checkInfo.fn ++ " on a list with some elements will result in Just the elements after the first"
        , details = [ "You can replace this call by Just the list elements after the first." ]
        }
        checkInfo.fnRange
        (Fix.replaceRangeBy checkInfo.fnRange
            (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo))
            :: replaceListArgByTailFix
        )


listTailChecks : IntoFnCheck
listTailChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.specific.asString } listCollection checkInfo
                |> onNothing
                    (\() ->
                        case Node.value (AstHelpers.removeParens checkInfo.firstArg) of
                            Expression.ListExpr ((Node headRange _) :: (Node tailFirstRange _) :: _) ->
                                Just
                                    (listTailExistsError
                                        [ Fix.removeRange { start = headRange.start, end = tailFirstRange.start }
                                        ]
                                        checkInfo
                                    )

                            Expression.OperatorApplication "::" _ _ tail ->
                                Just
                                    (listTailExistsError
                                        (replaceBySubExpressionFix (Node.range checkInfo.firstArg) tail)
                                        checkInfo
                                    )

                            _ ->
                                Nothing
                    )
                |> onNothing
                    (\() ->
                        case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
                            Just _ ->
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " on a singleton list will result in Just []"
                                        , details = [ "You can replace this call by Just []." ]
                                        }
                                        checkInfo.fnRange
                                        [ Fix.replaceRangeBy (Node.range checkInfo.firstArg) "[]"
                                        , Fix.replaceRangeBy checkInfo.fnRange
                                            (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo))
                                        ]
                                    )

                            Nothing ->
                                Nothing
                    )
        )


listMapChecks : IntoFnCheck
listMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks listCollection
        , listMapOnSingletonCheck
        , onSpecificFnCallCanBeCombinedCheck
            { args = [ tupleSecondAccessFunctionProperties ]
            , earlierFn = Fn.Array.toIndexedList
            , combinedFn = Fn.Array.toList
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = [ tupleFirstAccessFunctionProperties ]
            , earlierFn = Fn.Dict.toList
            , combinedFn = Fn.Dict.keys
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = [ tupleSecondAccessFunctionProperties ]
            , earlierFn = Fn.Dict.toList
            , combinedFn = Fn.Dict.values
            }
        , mapOnRepeatAppliesTheFunctionToTheRepeatedElementCheck Fn.List.repeat
        , listMapTupleFirstOnArrayToIndexedListCheck
        ]


listMapTupleFirstOnArrayToIndexedListCheck : IntoFnCheck
listMapTupleFirstOnArrayToIndexedListCheck =
    { call =
        \checkInfo ->
            if AstHelpers.isTupleFirstAccess checkInfo checkInfo.firstArg then
                case checkInfo.argsAfterFirst of
                    [ unmappedListArg ] ->
                        case AstHelpers.getSpecificUnreducedFnCall Fn.Array.toIndexedList checkInfo.lookupTable unmappedListArg of
                            Nothing ->
                                Nothing

                            Just arrayToIndexedListCall ->
                                let
                                    unmappedListArgNeedsParens : Bool
                                    unmappedListArgNeedsParens =
                                        needsParens (Node.value unmappedListArg)
                                in
                                Just
                                    (Rule.errorWithFix
                                        { message =
                                            qualifiedToString Fn.List.map
                                                ++ " with a function accessing the first tuple part on "
                                                ++ qualifiedToString Fn.Array.toIndexedList
                                                ++ " is the same as "
                                                ++ qualifiedToString Fn.List.range
                                                ++ " from 0 to its length - 1"
                                        , details =
                                            [ "You can replace this call by "
                                                ++ qualifiedToString Fn.List.range
                                                ++ " starting with 0 and ending with "
                                                ++ qualifiedToString Fn.Array.length
                                                ++ " of the array given to the "
                                                ++ qualifiedToString Fn.Array.toIndexedList
                                                ++ " call - 1."
                                            ]
                                        }
                                        checkInfo.fnRange
                                        [ Fix.replaceRangeBy checkInfo.fnRange
                                            (qualifiedToString (qualify Fn.List.range checkInfo))
                                        , Fix.replaceRangeBy (Node.range checkInfo.firstArg)
                                            "0"
                                        , Fix.insertAt (Node.range unmappedListArg).start
                                            ("("
                                                ++ (if unmappedListArgNeedsParens then
                                                        "("

                                                    else
                                                        ""
                                                   )
                                            )
                                        , Fix.insertAt (Node.range unmappedListArg).end
                                            ((if unmappedListArgNeedsParens then
                                                ")"

                                              else
                                                ""
                                             )
                                                ++ " - 1)"
                                            )
                                        , Fix.replaceRangeBy arrayToIndexedListCall.fnRange
                                            (qualifiedToString (qualify Fn.Array.length checkInfo))
                                        ]
                                    )

                    _ ->
                        Nothing

            else
                Nothing
    , composition =
        \checkInfo ->
            case checkInfo.later.args of
                [ mapFunctionArg ] ->
                    if AstHelpers.isTupleFirstAccess checkInfo mapFunctionArg then
                        if checkInfo.earlier.fn == Fn.Array.toIndexedList then
                            Just
                                { info =
                                    { message =
                                        qualifiedToString Fn.List.map
                                            ++ " with a function accessing the first tuple part on "
                                            ++ qualifiedToString Fn.Array.toIndexedList
                                            ++ " is the same as "
                                            ++ qualifiedToString Fn.List.range
                                            ++ " from 0 to its length - 1"
                                    , details =
                                        [ "You can replace this composition by "
                                            ++ qualifiedToString Fn.Array.length
                                            ++ ", then a function subtracting 1, then "
                                            ++ qualifiedToString Fn.List.range
                                            ++ " 0."
                                        ]
                                    }
                                , fix =
                                    [ Fix.replaceRangeBy checkInfo.later.fnRange
                                        (qualifiedToString (qualify Fn.List.range checkInfo))
                                    , Fix.replaceRangeBy (Node.range mapFunctionArg)
                                        "0"
                                    , Fix.replaceRangeBy checkInfo.earlier.range
                                        ("("
                                            ++ compositionString
                                                { earlier = qualifiedToString (qualify Fn.Array.length checkInfo)
                                                , later = "(+) -1"
                                                , direction = compositionCheckInfoDirection checkInfo
                                                }
                                            ++ ")"
                                        )
                                    ]
                                }

                        else
                            Nothing

                    else
                        Nothing

                _ ->
                    Nothing
    }


listMapNChecks : IntoFnCheck
listMapNChecks =
    intoFnCheckOnlyCall (emptiableMapNChecks listCollection)


listMapOnSingletonCheck : IntoFnCheck
listMapOnSingletonCheck =
    { call =
        \checkInfo ->
            -- we do not re-use mapOnWrappedChecks because that would fix e.g.
            -- map f (if c then List.singleton a else [ b ]) --> map f (List.singleton (if c then a else b))
            -- while we instead fix it to the more compact form
            -- map f [ if c then a else b ]
            case secondArg checkInfo of
                Just listArg ->
                    (case AstHelpers.getListSingleton checkInfo.lookupTable listArg of
                        Just wrappedElement ->
                            let
                                mappedValueRange : Range
                                mappedValueRange =
                                    Node.range wrappedElement

                                mappingArgRange : Range
                                mappingArgRange =
                                    Node.range checkInfo.firstArg
                            in
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " on a singleton list will result in a singleton list with the function applied to the value inside"
                                    , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                                    }
                                    checkInfo.fnRange
                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range listArg }
                                        ++ parenthesizeIfNeededFix wrappedElement
                                        ++ (case checkInfo.callStyle of
                                                CallStyle.Pipe CallStyle.LeftToRight ->
                                                    [ Fix.insertAt mappedValueRange.start "("
                                                    , Fix.insertAt mappedValueRange.end
                                                        (" |> " ++ checkInfo.extractSourceCode mappingArgRange ++ ")")
                                                    ]

                                                CallStyle.Pipe CallStyle.RightToLeft ->
                                                    [ Fix.insertAt mappedValueRange.start ("(" ++ checkInfo.extractSourceCode mappingArgRange ++ " <| ")
                                                    , Fix.insertAt mappedValueRange.end ")"
                                                    ]

                                                CallStyle.Application ->
                                                    [ Fix.insertAt mappedValueRange.start ("(" ++ checkInfo.extractSourceCode mappingArgRange ++ " ")
                                                    , Fix.insertAt mappedValueRange.end ")"
                                                    ]
                                           )
                                    )
                                )

                        Nothing ->
                            Nothing
                    )
                        |> onNothing
                            (\() ->
                                case sameInAllBranches (getValueWithNodeRange (listCollection.wrap.getValue checkInfo.lookupTable)) listArg of
                                    Just wraps ->
                                        let
                                            mappingArgRange : Range
                                            mappingArgRange =
                                                Node.range checkInfo.firstArg
                                        in
                                        Just
                                            (Rule.errorWithFix
                                                { message = qualifiedToString checkInfo.fn ++ " on a singleton list will result in a singleton list with the function applied to the value inside"
                                                , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                                                }
                                                checkInfo.fnRange
                                                (Fix.insertAt checkInfo.parentRange.start "[ "
                                                    :: Fix.insertAt checkInfo.parentRange.end " ]"
                                                    :: keepOnlyFix
                                                        { parentRange = Range.combine [ checkInfo.fnRange, mappingArgRange ]
                                                        , keep = mappingArgRange
                                                        }
                                                    ++ List.concatMap
                                                        (\wrap -> replaceBySubExpressionFix wrap.nodeRange wrap.value)
                                                        wraps
                                                )
                                            )

                                    Nothing ->
                                        Nothing
                            )

                Nothing ->
                    Nothing
    , composition = (mapOnWrappedChecks listCollection).composition
    }


listMemberChecks : IntoFnCheck
listMemberChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                callOnEmptyReturnsCheck
                    { resultAsString = \res -> qualifiedToString (qualify Fn.Basics.falseVariant res) }
                    listCollection
                    checkInfo
                    |> onNothing (\() -> knownMemberChecks listCollection checkInfo)
                    |> onNothing (\() -> wrapperMemberChecks listCollection checkInfo)
                    |> onNothing (\() -> listMemberOnRepeatCallCheck checkInfo)
            )
        , containsElementOnConversionFnCallCanBeCombinedCheck
            { combinedOperationRepresents = "check for a set member"
            , convertFn = Fn.Set.toList
            , convertedRepresentsIndefinite = "a list"
            , combinedFn = Fn.Set.member
            }
        ]


listMemberOnRepeatCallCheck : CallCheckInfo -> Maybe (Error {})
listMemberOnRepeatCallCheck checkInfo =
    case fullyAppliedLastArg checkInfo of
        Nothing ->
            Nothing

        Just listArg ->
            case AstHelpers.getSpecificUnreducedFnCall Fn.List.repeat checkInfo.lookupTable listArg of
                Nothing ->
                    Nothing

                Just listRepeatCall ->
                    case listRepeatCall.argsAfterFirst of
                        [ elementToRepeatArg ] ->
                            Just
                                (Rule.errorWithFix
                                    { message =
                                        qualifiedToString checkInfo.fn
                                            ++ " on "
                                            ++ qualifiedToString Fn.List.repeat
                                            ++ " is the same as checking whether the repeat count is positive and the element to repeat equals the checked member"
                                    , details =
                                        [ "You can replace this call by (the count argument given to "
                                            ++ qualifiedToString Fn.List.repeat
                                            ++ ") >= 1 && (the checked member argument given to "
                                            ++ qualifiedToString checkInfo.fn
                                            ++ ") == (the element to repeat argument given to "
                                            ++ qualifiedToString Fn.List.repeat
                                            ++ ")."
                                        ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.insertAt checkInfo.parentRange.start
                                        ("(("
                                            ++ parenthesizeIf
                                                (needsParens (Node.value listRepeatCall.firstArg))
                                                (checkInfo.extractSourceCode
                                                    (Node.range listRepeatCall.firstArg)
                                                )
                                            ++ " >= 1) && ("
                                        )
                                        :: Fix.insertAt checkInfo.parentRange.end
                                            "))"
                                        :: replaceByEqualsOperationOn2SubExpressions
                                            checkInfo.parentRange
                                            checkInfo.firstArg
                                            elementToRepeatArg
                                    )
                                )

                        _ ->
                            Nothing


replaceByEqualsOperationOn2SubExpressions : Range -> Node Expression -> Node Expression -> List Fix
replaceByEqualsOperationOn2SubExpressions parentRange aSub bSub =
    case Range.compareLocations (Node.range aSub).start (Node.range bSub).start of
        LT ->
            replaceByEqualsOperationOn2SubExpressionsInOrder parentRange aSub bSub

        _ ->
            replaceByEqualsOperationOn2SubExpressionsInOrder parentRange bSub aSub


replaceByEqualsOperationOn2SubExpressionsInOrder : Range -> Node Expression -> Node Expression -> List Fix
replaceByEqualsOperationOn2SubExpressionsInOrder parentRange (Node leftSubRange leftSub) (Node rightSubRange rightSub) =
    let
        leftSubNeedsParens : Bool
        leftSubNeedsParens =
            needsParens leftSub

        rightSubNeedsParens : Bool
        rightSubNeedsParens =
            needsParens rightSub
    in
    [ Fix.replaceRangeBy { start = parentRange.start, end = leftSubRange.start }
        (if leftSubNeedsParens then
            "("

         else
            ""
        )
    , Fix.replaceRangeBy { start = leftSubRange.end, end = rightSubRange.start }
        ((if leftSubNeedsParens then
            ")"

          else
            ""
         )
            ++ " == "
            ++ (if rightSubNeedsParens then
                    "("

                else
                    ""
               )
        )
    , Fix.replaceRangeBy { start = rightSubRange.end, end = parentRange.end }
        (if rightSubNeedsParens then
            ")"

         else
            ""
        )
    ]


containsElementOnConversionFnCallCanBeCombinedCheck :
    { combinedOperationRepresents : String
    , convertFn : ( ModuleName, String )
    , convertedRepresentsIndefinite : String
    , combinedFn : ( ModuleName, String )
    }
    -> IntoFnCheck
containsElementOnConversionFnCallCanBeCombinedCheck config =
    { call =
        \checkInfo ->
            if checkInfo.expectNaN && AstHelpers.couldBeValueContainingNaN checkInfo.firstArg then
                Nothing

            else
                case fullyAppliedLastArg checkInfo of
                    Just convertedArg ->
                        case AstHelpers.getSpecificUnreducedFnCall config.convertFn checkInfo.lookupTable convertedArg of
                            Just conversionCall ->
                                if checkInfo.expectNaN && AstHelpers.couldBeValueContainingNaN conversionCall.firstArg then
                                    Nothing

                                else
                                    Just
                                        (Rule.errorWithFix
                                            { message =
                                                "To "
                                                    ++ config.combinedOperationRepresents
                                                    ++ ", you don't need to convert to "
                                                    ++ config.convertedRepresentsIndefinite
                                            , details = [ "Using " ++ qualifiedToString config.combinedFn ++ " directly is meant for this exact purpose and will also be faster." ]
                                            }
                                            checkInfo.fnRange
                                            (Fix.replaceRangeBy checkInfo.fnRange
                                                (qualifiedToString (qualify config.combinedFn checkInfo))
                                                :: replaceBySubExpressionFix conversionCall.nodeRange conversionCall.firstArg
                                            )
                                        )

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
    , composition =
        \checkInfo ->
            if
                Basics.not checkInfo.expectNaN
                    && (checkInfo.earlier.fn == config.convertFn)
                    && onlyLastArgIsCurried checkInfo.later
            then
                Just
                    { info =
                        { message =
                            "To "
                                ++ config.combinedOperationRepresents
                                ++ ", you don't need to convert to "
                                ++ config.convertedRepresentsIndefinite
                        , details = [ "Using " ++ qualifiedToString config.combinedFn ++ " directly is meant for this exact purpose and will also be faster." ]
                        }
                    , fix =
                        [ Fix.replaceRangeBy checkInfo.later.fnRange
                            (qualifiedToString (qualify config.combinedFn checkInfo))
                        , Fix.removeRange checkInfo.earlier.removeRange
                        ]
                    }

            else
                Nothing
    }


listSumChecks : IntoFnCheck
listSumChecks =
    intoFnChecksFirstThatConstructsError
        [ onWrappedReturnsItsValueCheck listCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                callOnEmptyReturnsCheck { resultAsString = \_ -> "0" } listCollection checkInfo
                    |> onNothing
                        (\() ->
                            callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn)
                                ( listCollection, numberForAddProperties )
                                checkInfo
                        )
                    |> onNothing
                        (\() ->
                            if checkInfo.expectNaN then
                                callOnCollectionWithAbsorbingElementChecks (qualifiedToString checkInfo.fn)
                                    ( listCollection, numberForAddProperties )
                                    checkInfo

                            else
                                Nothing
                        )
            )
        ]


listProductChecks : IntoFnCheck
listProductChecks =
    intoFnChecksFirstThatConstructsError
        [ onWrappedReturnsItsValueCheck listCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                callOnEmptyReturnsCheck { resultAsString = \_ -> "1" } listCollection checkInfo
                    |> onNothing
                        (\() ->
                            callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn)
                                ( listCollection, numberForMultiplyProperties )
                                checkInfo
                        )
                    |> onNothing
                        (\() ->
                            if
                                checkInfo.expectNaN
                                    && List.any AstHelpers.couldBeValueContainingNaN
                                        (checkInfo.firstArg :: checkInfo.argsAfterFirst)
                            then
                                callOnCollectionWithAbsorbingElementChecks (qualifiedToString checkInfo.fn)
                                    ( listCollection, numberForMultiplyProperties )
                                    checkInfo

                            else
                                callOnCollectionWithAbsorbingElementChecks (qualifiedToString checkInfo.fn)
                                    ( listCollection, numberNotExpectingNaNForMultiplyProperties )
                                    checkInfo
                        )
            )
        ]


listMinimumChecks : IntoFnCheck
listMinimumChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.specific.asString } listCollection checkInfo
                    |> onNothing
                        (\() ->
                            case getNonEmptyListRangeCall checkInfo checkInfo.firstArg of
                                Nothing ->
                                    Nothing

                                Just listRangeCall ->
                                    Just
                                        (Rule.errorWithFix
                                            { message =
                                                qualifiedToString checkInfo.fn
                                                    ++ " on a non-empty "
                                                    ++ qualifiedToString Fn.List.range
                                                    ++ " results in Just its start number"
                                            , details =
                                                [ "You can replace this call by the first argument given to the "
                                                    ++ qualifiedToString Fn.List.range
                                                    ++ " call, wrapped in Just."
                                                ]
                                            }
                                            checkInfo.fnRange
                                            (Fix.replaceRangeBy checkInfo.fnRange
                                                (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo))
                                                :: replaceBySubExpressionFix
                                                    (Node.range checkInfo.firstArg)
                                                    listRangeCall.start
                                            )
                                        )
                        )
            )
        , onWrappedReturnsJustItsValueCheck listCollection
        , intoFnCheckOnlyWhenExpectNaNIsNotEnabled
            (listReorderOperationsBeforeAreUnnecessaryChecks "overall minimum")
        ]


listMaximumChecks : IntoFnCheck
listMaximumChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.specific.asString } listCollection checkInfo
                    |> onNothing
                        (\() ->
                            case getNonEmptyListRangeCall checkInfo checkInfo.firstArg of
                                Nothing ->
                                    Nothing

                                Just listRangeCall ->
                                    Just
                                        (Rule.errorWithFix
                                            { message =
                                                qualifiedToString checkInfo.fn
                                                    ++ " on a non-empty "
                                                    ++ qualifiedToString Fn.List.range
                                                    ++ " results in Just its end number"
                                            , details =
                                                [ "You can replace this call by the second argument given to the "
                                                    ++ qualifiedToString Fn.List.range
                                                    ++ " call, wrapped in Just."
                                                ]
                                            }
                                            checkInfo.fnRange
                                            (Fix.replaceRangeBy checkInfo.fnRange
                                                (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo))
                                                :: replaceBySubExpressionFix
                                                    (Node.range checkInfo.firstArg)
                                                    listRangeCall.end
                                            )
                                        )
                        )
            )
        , onWrappedReturnsJustItsValueCheck listCollection
        , intoFnCheckOnlyWhenExpectNaNIsNotEnabled
            (listReorderOperationsBeforeAreUnnecessaryChecks "overall maximum")
        ]


getNonEmptyListRangeCall :
    Normalize.Resources a
    -> Node Expression
    -> Maybe { start : Node Expression, end : Node Expression }
getNonEmptyListRangeCall checkInfo expressionNode =
    case AstHelpers.getSpecificUnreducedFnCall Fn.List.range checkInfo.lookupTable expressionNode of
        Nothing ->
            Nothing

        Just listRangeCall ->
            case listRangeCall.argsAfterFirst of
                [ rangeEndArg ] ->
                    case Normalize.getInt checkInfo listRangeCall.firstArg of
                        Nothing ->
                            Nothing

                        Just rangeStartValue ->
                            case Normalize.getInt checkInfo rangeEndArg of
                                Nothing ->
                                    Nothing

                                Just rangeEndValue ->
                                    if rangeStartValue <= rangeEndValue then
                                        Just { start = listRangeCall.firstArg, end = rangeEndArg }

                                    else
                                        Nothing

                _ ->
                    Nothing


listFoldlChecks : IntoFnCheck
listFoldlChecks =
    intoFnChecksFirstThatConstructsError
        [ listFoldChecks "foldl" "foldr"
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Nothing ->
                        Nothing

                    Just initialArg ->
                        if
                            isEmptyList initialArg
                                && Normalize.isSpecificUnappliedBinaryOperation "::" checkInfo checkInfo.firstArg
                        then
                            Just
                                (operationWithSpecificArgsIsEquivalentToFnError
                                    { specificArgsDescription = "(::) []"
                                    , replacementFn = Fn.List.reverse
                                    }
                                    checkInfo
                                )

                        else
                            Nothing
            )
        ]


listFoldrChecks : IntoFnCheck
listFoldrChecks =
    intoFnChecksFirstThatConstructsError
        [ listFoldChecks "foldr" "foldl"
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Nothing ->
                        Nothing

                    Just initialArg ->
                        if
                            isEmptyString initialArg
                                && isStringAppendFunction checkInfo checkInfo.firstArg
                        then
                            Just
                                (operationWithSpecificArgsIsEquivalentToFnError
                                    { specificArgsDescription = "(++) \"\""
                                    , replacementFn = Fn.String.concat
                                    }
                                    checkInfo
                                )

                        else if
                            isEmptyList initialArg
                                && isListAppendFunction checkInfo checkInfo.firstArg
                        then
                            Just
                                (operationWithSpecificArgsIsEquivalentToFnError
                                    { specificArgsDescription = "(++) []"
                                    , replacementFn = Fn.List.concat
                                    }
                                    checkInfo
                                )

                        else if
                            isEmptyList initialArg
                                && Normalize.isSpecificUnappliedBinaryOperation "::" checkInfo checkInfo.firstArg
                        then
                            Just
                                (alwaysReturnsLastArgError
                                    (qualifiedToString checkInfo.fn ++ " (::) []")
                                    listCollection
                                    checkInfo
                                )

                        else
                            Nothing
            )
        ]


isStringAppendFunction :
    Infer.Resources (AstHelpers.ReduceLambdaResources a)
    -> Node Expression
    -> Bool
isStringAppendFunction resources expressionNode =
    Normalize.isSpecificUnappliedBinaryOperation "++" resources expressionNode
        || AstHelpers.isSpecificValueOrFn Fn.String.append resources expressionNode


isListAppendFunction :
    Infer.Resources (AstHelpers.ReduceLambdaResources a)
    -> Node Expression
    -> Bool
isListAppendFunction resources expressionNode =
    Normalize.isSpecificUnappliedBinaryOperation "++" resources expressionNode
        || AstHelpers.isSpecificValueOrFn Fn.List.append resources expressionNode


listFoldChecks : String -> String -> IntoFnCheck
listFoldChecks foldFnName reverseFoldFnName =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (emptiableFoldChecks listCollection)
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Nothing ->
                        Nothing

                    Just initialArg ->
                        let
                            numberBinaryOperationChecks : { identity : Int, two : String, list : String } -> Maybe (Error {})
                            numberBinaryOperationChecks operation =
                                let
                                    fixWith : List Fix -> Error {}
                                    fixWith fixes =
                                        let
                                            replacementOperationAsString : String
                                            replacementOperationAsString =
                                                qualifiedToString ( listModuleName, operation.list )
                                        in
                                        Rule.errorWithFix
                                            { message = qualifiedToString checkInfo.fn ++ " (" ++ operation.two ++ ") " ++ String.fromInt operation.identity ++ " is the same as " ++ replacementOperationAsString
                                            , details = [ "You can replace this call by " ++ replacementOperationAsString ++ " which is meant for this exact purpose." ]
                                            }
                                            checkInfo.fnRange
                                            fixes
                                in
                                if AstHelpers.getUncomputedNumberValue initialArg == Just (Basics.toFloat operation.identity) then
                                    Just
                                        (fixWith
                                            [ Fix.replaceRangeBy
                                                { start = checkInfo.fnRange.start
                                                , end = (Node.range initialArg).end
                                                }
                                                (qualifiedToString (qualify ( listModuleName, operation.list ) checkInfo))
                                            ]
                                        )

                                else
                                    case thirdArg checkInfo of
                                        Nothing ->
                                            Nothing

                                        Just _ ->
                                            case checkInfo.callStyle of
                                                CallStyle.Pipe CallStyle.LeftToRight ->
                                                    -- list |> fold op initial --> ((list |> List.op) op initial)
                                                    Just
                                                        (fixWith
                                                            [ Fix.insertAt (Node.range initialArg).end ")"
                                                            , Fix.insertAt (Node.range initialArg).start (operation.two ++ " ")
                                                            , Fix.replaceRangeBy
                                                                { start = checkInfo.fnRange.start
                                                                , end = (Node.range checkInfo.firstArg).end
                                                                }
                                                                (qualifiedToString (qualify ( listModuleName, operation.list ) checkInfo) ++ ")")
                                                            , Fix.insertAt checkInfo.parentRange.start "(("
                                                            ]
                                                        )

                                                -- CallStyle.Pipe CallStyle.RightToLeft | CallStyle.Application ->
                                                _ ->
                                                    -- fold op initial list --> (initial op (List.op list))
                                                    Just
                                                        (fixWith
                                                            [ Fix.insertAt checkInfo.parentRange.end ")"
                                                            , Fix.insertAt (Node.range initialArg).end
                                                                (" "
                                                                    ++ operation.two
                                                                    ++ " ("
                                                                    ++ qualifiedToString (qualify ( listModuleName, operation.list ) checkInfo)
                                                                )
                                                            , Fix.removeRange
                                                                { start = checkInfo.fnRange.start
                                                                , end = (Node.range initialArg).start
                                                                }
                                                            ]
                                                        )

                            boolBinaryOperationChecks : { two : String, list : String, determining : Bool } -> Bool -> Error {}
                            boolBinaryOperationChecks operation initialIsDetermining =
                                if initialIsDetermining == operation.determining then
                                    let
                                        determiningAsString : String
                                        determiningAsString =
                                            AstHelpers.boolToString operation.determining
                                    in
                                    alwaysResultsInUnparenthesizedConstantError
                                        (qualifiedToString checkInfo.fn ++ " with (" ++ operation.two ++ ") and the initial accumulator " ++ determiningAsString)
                                        { replacement = \res -> qualifiedToString (qualify ( [ "Basics" ], determiningAsString ) res) }
                                        checkInfo

                                else
                                    -- initialIsTrue /= operation.determining
                                    let
                                        replacementOperationAsString : String
                                        replacementOperationAsString =
                                            qualifiedToString ( listModuleName, operation.list ) ++ " identity"
                                    in
                                    Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " (" ++ operation.two ++ ") " ++ AstHelpers.boolToString (not operation.determining) ++ " is the same as " ++ replacementOperationAsString
                                        , details = [ "You can replace this call by " ++ replacementOperationAsString ++ " which is meant for this exact purpose." ]
                                        }
                                        checkInfo.fnRange
                                        [ Fix.replaceRangeBy
                                            { start = checkInfo.fnRange.start, end = (Node.range initialArg).end }
                                            (qualifiedToString (qualify ( listModuleName, operation.list ) checkInfo)
                                                ++ " "
                                                ++ qualifiedToString (qualify Fn.Basics.identity checkInfo)
                                            )
                                        ]
                        in
                        if Normalize.isSpecificUnappliedBinaryOperation "*" checkInfo checkInfo.firstArg then
                            numberBinaryOperationChecks { two = "*", list = "product", identity = 1 }

                        else if Normalize.isSpecificUnappliedBinaryOperation "+" checkInfo checkInfo.firstArg then
                            numberBinaryOperationChecks { two = "+", list = "sum", identity = 0 }

                        else
                            case Normalize.getBool checkInfo initialArg of
                                Nothing ->
                                    Nothing

                                Just initialBool ->
                                    if Normalize.isSpecificUnappliedBinaryOperation "&&" checkInfo checkInfo.firstArg then
                                        Just (boolBinaryOperationChecks { two = "&&", list = "all", determining = False } initialBool)

                                    else if Normalize.isSpecificUnappliedBinaryOperation "||" checkInfo checkInfo.firstArg then
                                        Just (boolBinaryOperationChecks { two = "||", list = "any", determining = True } initialBool)

                                    else
                                        Nothing
            )
        , onConversionFnCallCanBeCombinedCheck
            { combinedOperationRepresents = "fold a list"
            , convertFn = Fn.List.reverse
            , actionRepresents = "reverse it"
            , combinedFn = ( [ "List" ], reverseFoldFnName )
            }
        , onConversionFnCallCanBeCombinedCheck
            { combinedOperationRepresents = "fold a set"
            , convertFn = Fn.Set.toList
            , actionRepresents = "convert to a list"
            , combinedFn = ( [ "Set" ], foldFnName )
            }
        , onConversionFnCallCanBeCombinedCheck
            { combinedOperationRepresents = "fold an array"
            , convertFn = Fn.Array.toList
            , actionRepresents = "convert to a list"
            , combinedFn = ( [ "Array" ], foldFnName )
            }
        , listFoldOnDictEntryPartListChecks
            { combinedFn = ( [ "Dict" ], foldFnName )
            }
        ]


listFoldOnDictEntryPartListChecks : { combinedFn : ( ModuleName, String ) } -> IntoFnCheck
listFoldOnDictEntryPartListChecks config =
    { composition =
        \checkInfo ->
            case checkInfo.later.args of
                [ reduceArg, _ ] ->
                    case checkInfo.earlier.fn of
                        ( [ "Dict" ], "values" ) ->
                            Just
                                { info =
                                    { message = "To fold over dict values, you don't need to convert to a list"
                                    , details =
                                        [ "You can replace this composition by "
                                            ++ qualifiedToString config.combinedFn
                                            ++ " and ignore the first incoming value in the reduce function."
                                        ]
                                    }
                                , fix =
                                    Fix.replaceRangeBy checkInfo.later.fnRange
                                        (qualifiedToString (qualify config.combinedFn checkInfo))
                                        :: Fix.removeRange checkInfo.earlier.removeRange
                                        :: ignoreFirstIncomingFix checkInfo reduceArg
                                }

                        ( [ "Dict" ], "keys" ) ->
                            Just
                                { info =
                                    { message = "To fold over dict keys, you don't need to convert to a list"
                                    , details =
                                        [ "You can replace this composition by "
                                            ++ qualifiedToString config.combinedFn
                                            ++ " and ignore the second incoming value in the reduce function."
                                        ]
                                    }
                                , fix =
                                    Fix.replaceRangeBy checkInfo.later.fnRange
                                        (qualifiedToString (qualify config.combinedFn checkInfo))
                                        :: Fix.removeRange checkInfo.earlier.removeRange
                                        :: ignoreSecondIncomingFix checkInfo reduceArg
                                }

                        ( [ "Dict" ], "toList" ) ->
                            case getLambdaDestructuringFirstIncomingTuple reduceArg of
                                Nothing ->
                                    Nothing

                                Just tuplePattern ->
                                    Just
                                        { info =
                                            { message = "To fold over dict entries, you don't need to convert to a list"
                                            , details =
                                                [ "You can replace this composition by "
                                                    ++ qualifiedToString config.combinedFn
                                                    ++ " and split the incoming entry tuple pattern into separate key and value patterns in the reduce function."
                                                ]
                                            }
                                        , fix =
                                            Fix.replaceRangeBy checkInfo.later.fnRange
                                                (qualifiedToString (qualify config.combinedFn checkInfo))
                                                :: Fix.removeRange checkInfo.earlier.removeRange
                                                :: spreadPatternTuplePartsFix tuplePattern
                                        }

                        _ ->
                            Nothing

                _ ->
                    Nothing
    , call =
        \checkInfo ->
            case checkInfo.argsAfterFirst of
                [ _, foldedOverArg ] ->
                    (case AstHelpers.getSpecificUnreducedFnCall Fn.Dict.values checkInfo.lookupTable foldedOverArg of
                        Nothing ->
                            Nothing

                        Just dictValuesCall ->
                            Just
                                (Rule.errorWithFix
                                    { message = "To fold over dict values, you don't need to convert to a list"
                                    , details =
                                        [ "You can replace these calls by "
                                            ++ qualifiedToString config.combinedFn
                                            ++ " and ignore the first incoming value in the reduce function."
                                        ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify config.combinedFn checkInfo))
                                        :: ignoreFirstIncomingFix checkInfo checkInfo.firstArg
                                        ++ replaceBySubExpressionFix
                                            (Node.range foldedOverArg)
                                            dictValuesCall.firstArg
                                    )
                                )
                    )
                        |> onNothing
                            (\() ->
                                case AstHelpers.getSpecificUnreducedFnCall Fn.Dict.keys checkInfo.lookupTable foldedOverArg of
                                    Nothing ->
                                        Nothing

                                    Just dictKeysCall ->
                                        Just
                                            (Rule.errorWithFix
                                                { message = "To fold over dict keys, you don't need to convert to a list"
                                                , details =
                                                    [ "You can replace these calls by "
                                                        ++ qualifiedToString config.combinedFn
                                                        ++ " and ignore the second incoming value in the reduce function."
                                                    ]
                                                }
                                                checkInfo.fnRange
                                                (Fix.replaceRangeBy checkInfo.fnRange
                                                    (qualifiedToString (qualify config.combinedFn checkInfo))
                                                    :: ignoreSecondIncomingFix checkInfo checkInfo.firstArg
                                                    ++ replaceBySubExpressionFix
                                                        (Node.range foldedOverArg)
                                                        dictKeysCall.firstArg
                                                )
                                            )
                            )
                        |> onNothing
                            (\() ->
                                case AstHelpers.getSpecificUnreducedFnCall Fn.Dict.toList checkInfo.lookupTable foldedOverArg of
                                    Nothing ->
                                        Nothing

                                    Just dictToListCall ->
                                        case getLambdaDestructuringFirstIncomingTuple checkInfo.firstArg of
                                            Nothing ->
                                                Nothing

                                            Just tuplePattern ->
                                                Just
                                                    (Rule.errorWithFix
                                                        { message = "To fold over dict entries, you don't need to convert to a list"
                                                        , details =
                                                            [ "You can replace these calls by "
                                                                ++ qualifiedToString config.combinedFn
                                                                ++ " and split the incoming entry tuple pattern into separate key and value patterns in the reduce function."
                                                            ]
                                                        }
                                                        checkInfo.fnRange
                                                        (Fix.replaceRangeBy checkInfo.fnRange
                                                            (qualifiedToString (qualify config.combinedFn checkInfo))
                                                            :: spreadPatternTuplePartsFix tuplePattern
                                                            ++ replaceBySubExpressionFix
                                                                (Node.range foldedOverArg)
                                                                dictToListCall.firstArg
                                                        )
                                                    )
                            )

                _ ->
                    Nothing
    }


spreadPatternTuplePartsFix : Node ( Node Pattern, Node Pattern ) -> List Fix
spreadPatternTuplePartsFix (Node entryPatternRange ( lambdaKeyPattern, lambdaValuePattern )) =
    Fix.replaceRangeBy
        { start = (Node.range lambdaKeyPattern).end
        , end = (Node.range lambdaValuePattern).start
        }
        " "
        :: replaceBySubPatternFix
            { start = entryPatternRange.start
            , end = (Node.range lambdaKeyPattern).end
            }
            lambdaKeyPattern
        ++ replaceBySubPatternFix
            { start = (Node.range lambdaValuePattern).start
            , end = entryPatternRange.end
            }
            lambdaValuePattern


getLambdaDestructuringFirstIncomingTuple : Node Expression -> Maybe (Node ( Node Pattern, Node Pattern ))
getLambdaDestructuringFirstIncomingTuple expressionNode =
    case AstHelpers.removeParens expressionNode of
        Node _ (Expression.LambdaExpression lambda) ->
            case lambda.args of
                [] ->
                    Nothing

                firstLambdaPattern :: _ ->
                    case AstHelpers.removeParensFromPattern firstLambdaPattern of
                        Node tuplePatternRange (Pattern.TuplePattern [ first, second ]) ->
                            Just (Node tuplePatternRange ( first, second ))

                        _ ->
                            Nothing

        _ ->
            Nothing


ignoreFirstIncomingFix : QualifyResources a -> Node Expression -> List Fix
ignoreFirstIncomingFix resources functionExpressionNode =
    case sameInAllBranches getLambda functionExpressionNode of
        Nothing ->
            -- not \_ -> to preserve eager evaluation of the function expression
            let
                functionExpressionNeedsParens : Bool
                functionExpressionNeedsParens =
                    needsParens (Node.value functionExpressionNode)
            in
            [ Fix.insertAt (Node.range functionExpressionNode).start
                ("("
                    ++ qualifiedToString (qualify Fn.Basics.always resources)
                    ++ " "
                    ++ (if functionExpressionNeedsParens then
                            "("

                        else
                            ""
                       )
                )
            , Fix.insertAt (Node.range functionExpressionNode).end
                ((if functionExpressionNeedsParens then
                    ")"

                  else
                    ""
                 )
                    ++ ")"
                )
            ]

        Just lambdas ->
            lambdas
                |> List.map
                    (\lambda ->
                        Fix.insertAt (Node.range lambda.firstParameter).start "_ "
                    )


ignoreSecondIncomingFix : QualifyResources a -> Node Expression -> List Fix
ignoreSecondIncomingFix resources functionExpressionNode =
    case sameInAllBranches getLambda functionExpressionNode of
        Nothing ->
            -- don't fix to a lambda to preserve eager evaluation of the function expression
            let
                functionExpressionNeedsParens : Bool
                functionExpressionNeedsParens =
                    needsParens (Node.value functionExpressionNode)
            in
            [ Fix.insertAt (Node.range functionExpressionNode).start
                -- always << f  is the equivalent point-free version of
                -- \f a _ -> f a
                -- Think:
                --  always << reduce
                --> (\f _ -> f) << reduce
                --> \a -> (\f _ -> f) <| reduce a
                --> \a -> \_ -> reduce a
                --> \a _ acc -> reduce a acc
                ("("
                    ++ qualifiedToString (qualify Fn.Basics.always resources)
                    ++ " << "
                    ++ (if functionExpressionNeedsParens then
                            "("

                        else
                            ""
                       )
                )
            , Fix.insertAt (Node.range functionExpressionNode).end
                ((if functionExpressionNeedsParens then
                    ")"

                  else
                    ""
                 )
                    ++ ")"
                )
            ]

        Just lambdas ->
            lambdas
                |> List.map
                    (\lambda ->
                        Fix.insertAt (Node.range lambda.firstParameter).end
                            " _"
                    )


getLambda : Node Expression -> Maybe { firstParameter : Node Pattern }
getLambda expressionNode =
    case AstHelpers.removeParens expressionNode of
        Node _ (Expression.LambdaExpression lambda) ->
            case lambda.args of
                [] ->
                    Nothing

                firstParameter :: _ ->
                    Just { firstParameter = firstParameter }

        _ ->
            Nothing


onConversionFnCallCanBeCombinedCheck :
    { combinedOperationRepresents : String
    , convertFn : ( ModuleName, String )
    , actionRepresents : String
    , combinedFn : ( ModuleName, String )
    }
    -> IntoFnCheck
onConversionFnCallCanBeCombinedCheck config =
    { call =
        \checkInfo ->
            case fullyAppliedLastArg checkInfo of
                Just convertedArg ->
                    case AstHelpers.getSpecificUnreducedFnCall config.convertFn checkInfo.lookupTable convertedArg of
                        Just conversionCall ->
                            Just
                                (Rule.errorWithFix
                                    { message =
                                        "To "
                                            ++ config.combinedOperationRepresents
                                            ++ ", you don't need to "
                                            ++ config.actionRepresents
                                    , details = [ "Using " ++ qualifiedToString config.combinedFn ++ " directly is meant for this exact purpose and will also be faster." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify config.combinedFn checkInfo))
                                        :: replaceBySubExpressionFix conversionCall.nodeRange conversionCall.firstArg
                                    )
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if
                (checkInfo.earlier.fn == config.convertFn)
                    && onlyLastArgIsCurried checkInfo.later
            then
                Just
                    { info =
                        { message =
                            "To "
                                ++ config.combinedOperationRepresents
                                ++ ", you don't need to "
                                ++ config.actionRepresents
                        , details = [ "Using " ++ qualifiedToString config.combinedFn ++ " directly is meant for this exact purpose and will also be faster." ]
                        }
                    , fix =
                        [ Fix.replaceRangeBy checkInfo.later.fnRange
                            (qualifiedToString (qualify config.combinedFn checkInfo))
                        , Fix.removeRange checkInfo.earlier.removeRange
                        ]
                    }

            else
                Nothing
    }


listIsEmptyChecks : IntoFnCheck
listIsEmptyChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionIsEmptyChecks listCollection)
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Set.toList
            , combinedFn = Fn.Set.isEmpty
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Array.toList
            , combinedFn = Fn.Array.isEmpty
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Array.toIndexedList
            , combinedFn = Fn.Array.isEmpty
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.toList
            , combinedFn = Fn.Dict.isEmpty
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.values
            , combinedFn = Fn.Dict.isEmpty
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.keys
            , combinedFn = Fn.Dict.isEmpty
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.String.toList
            , combinedFn = Fn.String.isEmpty
            }
        , listOperationsKeepingLengthBeforeAreUnnecessaryChecks
        , listIsEmptyOnListFilterChecks
        ]


listIsEmptyOnListFilterChecks : IntoFnCheck
listIsEmptyOnListFilterChecks =
    { composition =
        \checkInfo ->
            if checkInfo.earlier.fn == Fn.List.filter then
                case checkInfo.earlier.args of
                    [ elementIsBadCheck ] ->
                        Just
                            (case getFunctionIntoNot checkInfo elementIsBadCheck of
                                Just functionIntoNot ->
                                    { info =
                                        { message =
                                            qualifiedToString checkInfo.later.fn
                                                ++ " on "
                                                ++ qualifiedToString Fn.List.filter
                                                ++ (if functionIntoNot.isStandaloneNot then
                                                        " "

                                                    else
                                                        " with a function into "
                                                   )
                                                ++ qualifiedToString Fn.Basics.not
                                                ++ " can be combined into "
                                                ++ qualifiedToString Fn.List.all
                                        , details =
                                            [ "You can replace this composition by "
                                                ++ qualifiedToString Fn.List.all
                                                ++ (if functionIntoNot.isStandaloneNot then
                                                        " " ++ qualifiedToString (qualify Fn.Basics.identity defaultQualifyResources)

                                                    else
                                                        " with the function given to "
                                                            ++ qualifiedToString Fn.List.filter
                                                            ++ " before the "
                                                            ++ qualifiedToString Fn.Basics.not
                                                   )
                                                ++ "."
                                            ]
                                        }
                                    , fix =
                                        Fix.removeRange checkInfo.later.removeRange
                                            :: Fix.replaceRangeBy checkInfo.earlier.fnRange
                                                (qualifiedToString (qualify Fn.List.all checkInfo))
                                            :: functionIntoNot.removeFix
                                    }

                                Nothing ->
                                    { info =
                                        { message =
                                            qualifiedToString checkInfo.later.fn
                                                ++ " on "
                                                ++ qualifiedToString Fn.List.filter
                                                ++ " is the same as "
                                                ++ qualifiedToString Fn.Basics.not
                                                ++ " on "
                                                ++ qualifiedToString Fn.List.any
                                        , details =
                                            [ "You can replace this composition by "
                                                ++ qualifiedToString Fn.List.any
                                                ++ " with the function given to "
                                                ++ qualifiedToString Fn.List.filter
                                                ++ ", then "
                                                ++ qualifiedToString Fn.Basics.not
                                                ++ "."
                                            ]
                                        }
                                    , fix =
                                        [ Fix.replaceRangeBy checkInfo.later.fnRange
                                            (qualifiedToString (qualify Fn.Basics.not checkInfo))
                                        , Fix.replaceRangeBy checkInfo.earlier.fnRange
                                            (qualifiedToString (qualify Fn.List.any checkInfo))
                                        ]
                                    }
                            )

                    _ ->
                        Nothing

            else
                Nothing
    , call =
        \checkInfo ->
            case AstHelpers.getSpecificUnreducedFnCall Fn.List.filter checkInfo.lookupTable checkInfo.firstArg of
                Nothing ->
                    Nothing

                Just filterCall ->
                    Just
                        (case getFunctionIntoNot checkInfo filterCall.firstArg of
                            Just functionIntoNot ->
                                Rule.errorWithFix
                                    { message =
                                        qualifiedToString checkInfo.fn
                                            ++ " on "
                                            ++ qualifiedToString Fn.List.filter
                                            ++ (if functionIntoNot.isStandaloneNot then
                                                    " "

                                                else
                                                    " with a function into "
                                               )
                                            ++ qualifiedToString Fn.Basics.not
                                            ++ " can be combined into "
                                            ++ qualifiedToString Fn.List.all
                                    , details =
                                        [ "You can replace this call by "
                                            ++ qualifiedToString Fn.List.all
                                            ++ (if functionIntoNot.isStandaloneNot then
                                                    " " ++ qualifiedToString (qualify Fn.Basics.identity defaultQualifyResources)

                                                else
                                                    " with the function given to "
                                                        ++ qualifiedToString Fn.List.filter
                                                        ++ " before the "
                                                        ++ qualifiedToString Fn.Basics.not
                                               )
                                            ++ "."
                                        ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy filterCall.fnRange
                                        (qualifiedToString (qualify Fn.List.all checkInfo))
                                        :: replaceCallBySubExpressionFix checkInfo.parentRange
                                            checkInfo.callStyle
                                            checkInfo.firstArg
                                        ++ functionIntoNot.removeFix
                                    )

                            Nothing ->
                                Rule.errorWithFix
                                    { message =
                                        qualifiedToString checkInfo.fn
                                            ++ " on "
                                            ++ qualifiedToString Fn.List.filter
                                            ++ " is the same as "
                                            ++ qualifiedToString Fn.Basics.not
                                            ++ " on "
                                            ++ qualifiedToString Fn.List.any
                                    , details =
                                        [ "You can replace this call by "
                                            ++ qualifiedToString Fn.Basics.not
                                            ++ " on "
                                            ++ qualifiedToString Fn.List.any
                                            ++ " with the function given to "
                                            ++ qualifiedToString Fn.List.filter
                                            ++ "."
                                        ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify Fn.Basics.not checkInfo))
                                    , Fix.replaceRangeBy filterCall.fnRange
                                        (qualifiedToString (qualify Fn.List.any checkInfo))
                                    ]
                        )
    }


{-| Remove the not in a function that ends in not, for example:

    f << not
    --> f

    \x -> if c then not (f x) else not (g y)
    --> \x -> if c then f x else g y

    not
    --> identity

-}
getFunctionIntoNot : AstHelpers.ReduceLambdaResources (QualifyResources a) -> Node Expression -> Maybe { removeFix : List Fix, isStandaloneNot : Bool }
getFunctionIntoNot resources expressionNode =
    if AstHelpers.isSpecificValueOrFn Fn.Basics.not resources expressionNode then
        Just
            { isStandaloneNot = True
            , removeFix =
                [ Fix.replaceRangeBy (Node.range expressionNode)
                    (qualifiedToString (qualify Fn.Basics.identity resources))
                ]
            }

    else
        sameInAllBranches
            (\fullFunctionBranch ->
                case AstHelpers.removeParens fullFunctionBranch of
                    Node _ (Expression.LambdaExpression lambda) ->
                        case lambda.args of
                            [ _ ] ->
                                sameInAllBranches
                                    (\lambdaResultBranch ->
                                        AstHelpers.getSpecificUnreducedFnCall Fn.Basics.not resources.lookupTable lambdaResultBranch
                                            |> Maybe.map
                                                (\notFnCall ->
                                                    replaceBySubExpressionFix notFnCall.nodeRange
                                                        notFnCall.firstArg
                                                )
                                    )
                                    lambda.expression
                                    |> Maybe.map List.concat

                            _ ->
                                Nothing

                    unparenthesizedExpressionNode ->
                        case getFullComposition unparenthesizedExpressionNode of
                            Nothing ->
                                Nothing

                            Just composition ->
                                if AstHelpers.isSpecificValueOrFn Fn.Basics.not resources composition.composedLater then
                                    Just
                                        (replaceBySubExpressionFix (Node.range fullFunctionBranch)
                                            composition.earlier
                                        )

                                else
                                    Nothing
            )
            expressionNode
            |> Maybe.map
                (\removeFixes ->
                    { isStandaloneNot = False, removeFix = List.concat removeFixes }
                )


listLengthChecks : IntoFnCheck
listLengthChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionSizeChecks listCollection)
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.toList
            , combinedFn = Fn.Dict.size
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.values
            , combinedFn = Fn.Dict.size
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.keys
            , combinedFn = Fn.Dict.size
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Set.toList
            , combinedFn = Fn.Set.size
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Array.toList
            , combinedFn = Fn.Array.length
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Array.toIndexedList
            , combinedFn = Fn.Array.length
            }
        , listOperationsKeepingLengthBeforeAreUnnecessaryChecks
        ]


listOperationsKeepingLengthBeforeAreUnnecessaryChecks : IntoFnCheck
listOperationsKeepingLengthBeforeAreUnnecessaryChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessarySpecificFnBeforeCheck
            { fn = Fn.List.map
            , fnArgCount = 2
            , fnLastArgRepresents = "list"
            , whyUnnecessary = "Changing each element in a list does not affect its length"
            }
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.List.indexedMap
            , fnArgCount = 2
            , fnLastArgRepresents = "list"
            , whyUnnecessary = "Changing each element in a list does not affect its length"
            }
        , listReorderOperationsBeforeAreUnnecessaryChecks "length"
        ]


listAllChecks : IntoFnCheck
listAllChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                emptiableAllChecks listCollection checkInfo
                    |> onNothing (\() -> collectionAllChecks listCollection checkInfo)
                    |> onNothing (\() -> listAllOnRepeatCallCheck checkInfo)
            )
        , mapToOperationWithIdentityCanBeCombinedToOperationChecks listCollection
        ]


listAllOnRepeatCallCheck : CallCheckInfo -> Maybe (Error {})
listAllOnRepeatCallCheck checkInfo =
    case fullyAppliedLastArg checkInfo of
        Nothing ->
            Nothing

        Just listArg ->
            case AstHelpers.getSpecificUnreducedFnCall Fn.List.repeat checkInfo.lookupTable listArg of
                Nothing ->
                    Nothing

                Just listRepeatCall ->
                    case listRepeatCall.argsAfterFirst of
                        [ elementToRepeatArg ] ->
                            Just
                                (Rule.errorWithFix
                                    { message =
                                        qualifiedToString checkInfo.fn
                                            ++ " on "
                                            ++ qualifiedToString Fn.List.repeat
                                            ++ " is the same as checking whether the repeat count is 0 or negative or the function passes for the element to repeat"
                                    , details =
                                        [ "You can replace this call by (the count argument given to "
                                            ++ qualifiedToString Fn.List.repeat
                                            ++ ") <= 0 || (the function argument given to "
                                            ++ qualifiedToString checkInfo.fn
                                            ++ ") (the element to repeat argument given to "
                                            ++ qualifiedToString Fn.List.repeat
                                            ++ ")."
                                        ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.insertAt checkInfo.parentRange.start
                                        ("(("
                                            ++ parenthesizeIf
                                                (needsParens (Node.value listRepeatCall.firstArg))
                                                (checkInfo.extractSourceCode
                                                    (Node.range listRepeatCall.firstArg)
                                                )
                                            ++ " <= 0) || ("
                                        )
                                        :: Fix.insertAt checkInfo.parentRange.end
                                            "))"
                                        :: replaceBySubExpressionFix
                                            (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                            checkInfo.firstArg
                                        ++ replaceBySubExpressionFix
                                            (Node.range listArg)
                                            elementToRepeatArg
                                    )
                                )

                        _ ->
                            Nothing


listAnyChecks : IntoFnCheck
listAnyChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                emptiableAnyChecks listCollection checkInfo
                    |> onNothing (\() -> collectionAnyChecks listCollection checkInfo)
                    |> onNothing
                        (\() -> operationWithEqualsConstantIsEquivalentToFnWithThatConstantCheck Fn.List.member checkInfo)
                    |> onNothing (\() -> listAnyOnRepeatCallCheck checkInfo)
            )
        , mapToOperationWithIdentityCanBeCombinedToOperationChecks listCollection
        ]


listAnyOnRepeatCallCheck : CallCheckInfo -> Maybe (Error {})
listAnyOnRepeatCallCheck checkInfo =
    case fullyAppliedLastArg checkInfo of
        Nothing ->
            Nothing

        Just listArg ->
            case AstHelpers.getSpecificUnreducedFnCall Fn.List.repeat checkInfo.lookupTable listArg of
                Nothing ->
                    Nothing

                Just listRepeatCall ->
                    case listRepeatCall.argsAfterFirst of
                        [ elementToRepeatArg ] ->
                            Just
                                (Rule.errorWithFix
                                    { message =
                                        qualifiedToString checkInfo.fn
                                            ++ " on "
                                            ++ qualifiedToString Fn.List.repeat
                                            ++ " is the same as checking whether the repeat count is positive and the function passes for the element to repeat"
                                    , details =
                                        [ "You can replace this call by (the count argument given to "
                                            ++ qualifiedToString Fn.List.repeat
                                            ++ ") >= 1 && (the function argument given to "
                                            ++ qualifiedToString checkInfo.fn
                                            ++ ") (the element to repeat argument given to "
                                            ++ qualifiedToString Fn.List.repeat
                                            ++ ")."
                                        ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.insertAt checkInfo.parentRange.start
                                        ("(("
                                            ++ parenthesizeIf
                                                (needsParens (Node.value listRepeatCall.firstArg))
                                                (checkInfo.extractSourceCode
                                                    (Node.range listRepeatCall.firstArg)
                                                )
                                            ++ " >= 1) && ("
                                        )
                                        :: Fix.insertAt checkInfo.parentRange.end
                                            "))"
                                        :: replaceBySubExpressionFix
                                            (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                            checkInfo.firstArg
                                        ++ replaceBySubExpressionFix
                                            (Node.range listArg)
                                            elementToRepeatArg
                                    )
                                )

                        _ ->
                            Nothing


listFilterChecks : IntoFnCheck
listFilterChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableKeepWhenChecks listCollection
        , earlierOperationCanBeMovedAfterAsForPerformanceChecks
            { laterOperationArgsDescription = Just "function"
            , earlierFn = Fn.List.reverse
            , earlierFnArgCount = 1
            , earlierFnOperationArgsDescription = Nothing
            , asLaterFn = Fn.List.reverse
            }
        , earlierOperationCanBeMovedAfterAsForPerformanceChecks
            { laterOperationArgsDescription = Just "function"
            , earlierFn = Fn.List.sort
            , earlierFnArgCount = 1
            , earlierFnOperationArgsDescription = Nothing
            , asLaterFn = Fn.List.sort
            }
        , earlierOperationCanBeMovedAfterAsForPerformanceChecks
            { laterOperationArgsDescription = Just "function"
            , earlierFn = Fn.List.sortBy
            , earlierFnArgCount = 2
            , earlierFnOperationArgsDescription = Just "function"
            , asLaterFn = Fn.List.sortBy
            }
        , earlierOperationCanBeMovedAfterAsForPerformanceChecks
            { laterOperationArgsDescription = Just "function"
            , earlierFn = Fn.List.sortWith
            , earlierFnArgCount = 2
            , earlierFnOperationArgsDescription = Just "compare function"
            , asLaterFn = Fn.List.sortWith
            }
        ]


listPartitionChecks : IntoFnCheck
listPartitionChecks =
    intoFnCheckOnlyCall (collectionPartitionChecks listCollection)


listFilterMapChecks : IntoFnCheck
listFilterMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableWrapperFilterMapChecks listCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                if AstHelpers.isIdentity checkInfo checkInfo.firstArg then
                    callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn ++ " with an identity function")
                        ( listCollection, maybeWithJustAsWrap )
                        checkInfo
                        |> onNothing
                            (\() ->
                                case secondArg checkInfo of
                                    Just listArg ->
                                        case AstHelpers.getListLiteral listArg of
                                            Just list ->
                                                case
                                                    traverse
                                                        (\branch ->
                                                            AstHelpers.getSpecificUnreducedFnCall Fn.Maybe.justVariant
                                                                checkInfo.lookupTable
                                                                branch
                                                        )
                                                        list
                                                of
                                                    Just justCalls ->
                                                        Just
                                                            (Rule.errorWithFix
                                                                { message = "Unnecessary use of " ++ qualifiedToString checkInfo.fn ++ " identity"
                                                                , details = [ "All of the elements in the list are `Just`s, which can be simplified by removing all of the `Just`s." ]
                                                                }
                                                                checkInfo.fnRange
                                                                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range listArg }
                                                                    ++ List.concatMap
                                                                        (\just -> keepOnlyFix { parentRange = just.nodeRange, keep = Node.range just.firstArg })
                                                                        justCalls
                                                                )
                                                            )

                                                    Nothing ->
                                                        Nothing

                                            Nothing ->
                                                Nothing

                                    Nothing ->
                                        Nothing
                            )

                else
                    Nothing
            )
        ]


listRangeChecks : IntoFnCheck
listRangeChecks =
    intoFnCheckOnlyCall (emptiableRangeChecks listCollection)


listRepeatChecks : IntoFnCheck
listRepeatChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            emptiableRepeatChecks listCollection checkInfo
                |> onNothing (\() -> wrapperRepeatChecks listCollection checkInfo)
        )


listReverseChecks : IntoFnCheck
listReverseChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableReverseChecks listCollection
        , unnecessaryOnWrappedCheck listCollection
        , unnecessaryOnSpecificFnCallCheck Fn.List.repeat
        ]


listSortChecks : IntoFnCheck
listSortChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck listCollection
        , unnecessaryOnWrappedCheck listCollection
        , operationDoesNotChangeResultOfOperationCheck
        , unnecessaryOnSpecificFnCallCheck Fn.List.repeat
        , unnecessaryOnSpecificFnCallCheck Fn.Set.toList
        , unnecessaryOnSpecificFnCallCheck Fn.Dict.toList
        ]


listSortByChecks : IntoFnCheck
listSortByChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck listCollection
        , unnecessaryOnWrappedCheck listCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case AstHelpers.getAlwaysResult checkInfo checkInfo.firstArg of
                    Just _ ->
                        Just
                            (alwaysReturnsLastArgError
                                (qualifiedToString checkInfo.fn ++ " with a function that always returns the same constant")
                                listCollection
                                checkInfo
                            )

                    Nothing ->
                        Nothing
            )
        , intoFnCheckOnlyCall (operationWithIdentityIsEquivalentToFnCheck Fn.List.sort)
        , operationDoesNotChangeResultOfOperationCheck
        , unnecessaryOnSpecificFnCallCheck Fn.List.repeat
        ]


listSortWithChecks : IntoFnCheck
listSortWithChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck listCollection
        , unnecessaryOnWrappedCheck listCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                let
                    alwaysAlwaysOrder : Maybe Order
                    alwaysAlwaysOrder =
                        AstHelpers.getAlwaysResult checkInfo checkInfo.firstArg
                            |> Maybe.andThen (\res -> AstHelpers.getAlwaysResult checkInfo res)
                            |> Maybe.andThen (\res -> AstHelpers.getOrder checkInfo.lookupTable res)
                in
                case alwaysAlwaysOrder of
                    Just order ->
                        Just
                            (case order of
                                LT ->
                                    operationWithFirstArgIsEquivalentToFnError
                                        { firstArgDescription = "a comparison that always returns LT"
                                        , replacementFn = Fn.List.reverse
                                        }
                                        checkInfo

                                EQ ->
                                    alwaysReturnsLastArgError
                                        (qualifiedToString checkInfo.fn ++ " with a comparison that always returns EQ")
                                        listCollection
                                        checkInfo

                                GT ->
                                    alwaysReturnsLastArgError
                                        (qualifiedToString checkInfo.fn ++ " with a comparison that always returns GT")
                                        listCollection
                                        checkInfo
                            )

                    Nothing ->
                        Nothing
            )
        , unnecessaryOnSpecificFnCallCheck Fn.List.repeat
        ]


listTakeChecks : IntoFnCheck
listTakeChecks =
    intoFnChecksFirstThatConstructsError
        [ collectionTakeChecks listCollection
        , earlierOperationCanBeMovedAfterAsForPerformanceChecks
            { laterOperationArgsDescription = Just "length"
            , earlierFn = Fn.List.map
            , earlierFnArgCount = 2
            , earlierFnOperationArgsDescription = Just "function"
            , asLaterFn = Fn.List.map
            }
        , earlierOperationCanBeMovedAfterAsForPerformanceChecks
            { laterOperationArgsDescription = Just "length"
            , earlierFn = Fn.List.indexedMap
            , earlierFnArgCount = 2
            , earlierFnOperationArgsDescription = Just "function"
            , asLaterFn = Fn.List.indexedMap
            }
        ]


listDropChecks : IntoFnCheck
listDropChecks =
    intoFnChecksFirstThatConstructsError
        [ collectionDropChecks listCollection
        , earlierOperationCanBeMovedAfterAsForPerformanceChecks
            { laterOperationArgsDescription = Just "count"
            , earlierFn = Fn.List.map
            , earlierFnArgCount = 2
            , earlierFnOperationArgsDescription = Just "function"
            , asLaterFn = Fn.List.map
            }
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case Normalize.getInt checkInfo checkInfo.firstArg of
                    Nothing ->
                        Nothing

                    Just count ->
                        dropOnLargerConstructionFromListLiteralWillRemoveTheseElementsCheck { dropCount = count }
                            listCollection
                            checkInfo
            )
        ]


{-| The remove n elements from a side checks

    drop n empty --> empty

    drop 0 collection --> collection

    drop -1 collection --> collection

    drop 10 collectionWith2Elements --> collection

-}
collectionDropChecks : TypeProperties (EmptiableProperties ConstantProperties (CollectionProperties properties)) -> IntoFnCheck
collectionDropChecks collection =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck collection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                Normalize.getInt checkInfo checkInfo.firstArg
                    |> Maybe.andThen
                        (\count ->
                            callWithNonPositiveIntCheckErrorSituation
                                { int = count, intDescription = "count", fn = checkInfo.fn }
                                |> Maybe.map
                                    (\situation -> alwaysReturnsLastArgError situation collection checkInfo)
                                |> onNothing
                                    (\() -> dropOnSmallerCollectionCheck { dropCount = count } collection checkInfo)
                        )
            )
        ]


listUnzipChecks : IntoFnCheck
listUnzipChecks =
    intoFnCheckOnlyCall
        (callOnEmptyReturnsCheck { resultAsString = \_ -> "( [], [] )" } listCollection)



-- ARRAY FUNCTIONS


arrayToListChecks : IntoFnCheck
arrayToListChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (callOnEmptyReturnsCheck { resultAsString = listCollection.empty.specific.asString } arrayCollection)
        , onSpecificFnCallReturnsItsLastArgCheck Fn.Array.fromList
        , onSpecificFnCallCanBeCombinedCheck
            { args = [], earlierFn = Fn.Array.repeat, combinedFn = Fn.List.repeat }
        ]


arrayToIndexedListChecks : IntoFnCheck
arrayToIndexedListChecks =
    intoFnCheckOnlyCall
        (callOnEmptyReturnsCheck { resultAsString = listCollection.empty.specific.asString } arrayCollection)


arrayFromListChecks : IntoFnCheck
arrayFromListChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                emptiableFromListChecks arrayCollection checkInfo
                    |> onNothing
                        (\() ->
                            case AstHelpers.getSpecificUnreducedFnCall Fn.List.range checkInfo.lookupTable checkInfo.firstArg of
                                Nothing ->
                                    Nothing

                                Just listRangeCall ->
                                    case listRangeCall.argsAfterFirst of
                                        [ Node listRangeEndArgRange _ ] ->
                                            case Normalize.getInt checkInfo listRangeCall.firstArg of
                                                Just 0 ->
                                                    Just
                                                        (Rule.errorWithFix
                                                            { message =
                                                                qualifiedToString checkInfo.fn
                                                                    ++ " on "
                                                                    ++ qualifiedToString Fn.List.range
                                                                    ++ " starting at 0 is the same as "
                                                                    ++ qualifiedToString Fn.Array.initialize
                                                            , details =
                                                                [ "You can replace this call by "
                                                                    ++ qualifiedToString Fn.Array.initialize
                                                                    ++ " with the range end argument + 1 and identity."
                                                                ]
                                                            }
                                                            checkInfo.fnRange
                                                            (Fix.insertAt listRangeEndArgRange.start "(("
                                                                :: Fix.insertAt listRangeEndArgRange.end ") + 1)"
                                                                :: Fix.replaceRangeBy listRangeCall.fnRange
                                                                    (qualifiedToString (qualify Fn.Array.initialize checkInfo))
                                                                :: Fix.insertAt checkInfo.parentRange.end
                                                                    (" " ++ qualifiedToString (qualify Fn.Basics.identity checkInfo))
                                                                :: replaceBySubExpressionFix checkInfo.parentRange
                                                                    checkInfo.firstArg
                                                                ++ keepOnlyFix
                                                                    { parentRange =
                                                                        Range.combine [ listRangeCall.fnRange, Node.range listRangeCall.firstArg ]
                                                                    , keep = listRangeCall.fnRange
                                                                    }
                                                            )
                                                        )

                                                _ ->
                                                    Nothing

                                        _ ->
                                            Nothing
                        )
                    |> onNothing
                        (\() ->
                            case AstHelpers.getSpecificUnreducedFnCall Fn.List.map checkInfo.lookupTable checkInfo.firstArg of
                                Nothing ->
                                    Nothing

                                Just listMapCall ->
                                    case listMapCall.argsAfterFirst of
                                        [ unmappedListArg ] ->
                                            case AstHelpers.getSpecificUnreducedFnCall Fn.List.range checkInfo.lookupTable unmappedListArg of
                                                Nothing ->
                                                    Nothing

                                                Just listRangeCall ->
                                                    case listRangeCall.argsAfterFirst of
                                                        [ Node listRangeEndArgRange _ ] ->
                                                            case Normalize.getInt checkInfo listRangeCall.firstArg of
                                                                Just 0 ->
                                                                    Just
                                                                        (Rule.errorWithFix
                                                                            { message =
                                                                                qualifiedToString checkInfo.fn
                                                                                    ++ " on "
                                                                                    ++ qualifiedToString Fn.List.map
                                                                                    ++ " on "
                                                                                    ++ qualifiedToString Fn.List.range
                                                                                    ++ " starting at 0 is the same as "
                                                                                    ++ qualifiedToString Fn.Array.initialize
                                                                            , details =
                                                                                [ "You can replace this call by "
                                                                                    ++ qualifiedToString Fn.Array.initialize
                                                                                    ++ " with the range end argument + 1 and the function argument given to the "
                                                                                    ++ qualifiedToString Fn.List.map
                                                                                    ++ " call."
                                                                                ]
                                                                            }
                                                                            checkInfo.fnRange
                                                                            (Fix.insertAt listRangeEndArgRange.start "(("
                                                                                :: Fix.insertAt listRangeEndArgRange.end ") + 1)"
                                                                                :: Fix.replaceRangeBy listRangeCall.fnRange
                                                                                    (qualifiedToString (qualify Fn.Array.initialize checkInfo))
                                                                                :: Fix.insertAt checkInfo.parentRange.end
                                                                                    (" "
                                                                                        ++ parenthesizeIf (needsParens (Node.value listMapCall.firstArg))
                                                                                            (checkInfo.extractSourceCode
                                                                                                (Node.range listMapCall.firstArg)
                                                                                            )
                                                                                    )
                                                                                :: replaceBySubExpressionFix checkInfo.parentRange
                                                                                    unmappedListArg
                                                                                ++ keepOnlyFix
                                                                                    { parentRange =
                                                                                        Range.combine [ listRangeCall.fnRange, Node.range listRangeCall.firstArg ]
                                                                                    , keep = listRangeCall.fnRange
                                                                                    }
                                                                            )
                                                                        )

                                                                _ ->
                                                                    Nothing

                                                        _ ->
                                                            Nothing

                                        _ ->
                                            Nothing
                        )
            )
        , onSpecificFnCallReturnsItsLastArgCheck Fn.Array.toList
        , onSpecificFnCallCanBeCombinedCheck
            { args = [], earlierFn = Fn.List.repeat, combinedFn = Fn.Array.repeat }
        ]


arrayRepeatChecks : IntoFnCheck
arrayRepeatChecks =
    intoFnCheckOnlyCall (emptiableRepeatChecks arrayCollection)


arrayInitializeChecks : IntoFnCheck
arrayInitializeChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo -> emptiableRepeatChecks arrayCollection checkInfo)
        , arrayInitializeIsTheSameAsRepeatCheck
        ]


arrayInitializeIsTheSameAsRepeatCheck : IntoFnCheck
arrayInitializeIsTheSameAsRepeatCheck =
    { call =
        \checkInfo ->
            case checkInfo.argsAfterFirst of
                [ indexToElementArg ] ->
                    case AstHelpers.getAlwaysResult checkInfo indexToElementArg of
                        Nothing ->
                            Nothing

                        Just repeatedElement ->
                            Just
                                (Rule.errorWithFix
                                    { message =
                                        qualifiedToString checkInfo.fn
                                            ++ " with a function that always results in the same element is the same as "
                                            ++ qualifiedToString Fn.Array.repeat
                                    , details =
                                        [ "You can replace this call by "
                                            ++ qualifiedToString Fn.Array.repeat
                                            ++ " with the length and index to element function result given to the original "
                                            ++ qualifiedToString checkInfo.fn
                                            ++ " call."
                                        ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify Fn.Array.repeat checkInfo))
                                        :: replaceBySubExpressionFix (Node.range indexToElementArg)
                                            repeatedElement
                                    )
                                )

                _ ->
                    Nothing
    , composition =
        \checkInfo ->
            if checkInfo.earlier.fn == Fn.Basics.always then
                Just
                    { info =
                        { message =
                            qualifiedToString checkInfo.later.fn
                                ++ " with a function that always results in the same element is the same as "
                                ++ qualifiedToString Fn.Array.repeat
                        , details =
                            [ "You can replace this composition by "
                                ++ qualifiedToString Fn.Array.repeat
                                ++ " with the length given to the original "
                                ++ qualifiedToString checkInfo.later.fn
                                ++ " operation."
                            ]
                        }
                    , fix =
                        [ Fix.replaceRangeBy checkInfo.later.fnRange
                            (qualifiedToString (qualify Fn.Array.repeat checkInfo))
                        , Fix.removeRange checkInfo.earlier.removeRange
                        ]
                    }

            else
                Nothing
    }


arrayMapChecks : IntoFnCheck
arrayMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks arrayCollection
        , mapOnRepeatAppliesTheFunctionToTheRepeatedElementCheck Fn.Array.repeat
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case fullyAppliedLastArg checkInfo of
                    Nothing ->
                        Nothing

                    Just arrayArg ->
                        case AstHelpers.getSpecificUnreducedFnCall Fn.Array.initialize checkInfo.lookupTable arrayArg of
                            Nothing ->
                                Nothing

                            Just specificFnCall ->
                                case specificFnCall.argsAfterFirst of
                                    [ indexToElementArg ] ->
                                        if AstHelpers.isIdentity checkInfo indexToElementArg then
                                            Just
                                                (Rule.errorWithFix
                                                    { message =
                                                        qualifiedToString checkInfo.fn
                                                            ++ " on "
                                                            ++ qualifiedToString Fn.Array.initialize
                                                            ++ " with an identity function can be combined"
                                                    , details =
                                                        [ "You can replace this call by "
                                                            ++ qualifiedToString Fn.Array.initialize
                                                            ++ " with the length given to the original "
                                                            ++ qualifiedToString Fn.Array.initialize
                                                            ++ " and the function given to "
                                                            ++ qualifiedToString (qualify checkInfo.fn checkInfo)
                                                            ++ "."
                                                        ]
                                                    }
                                                    checkInfo.fnRange
                                                    (Fix.replaceRangeBy (Node.range indexToElementArg)
                                                        (parenthesizeIf (needsParens (Node.value checkInfo.firstArg))
                                                            (checkInfo.extractSourceCode
                                                                (Node.range checkInfo.firstArg)
                                                            )
                                                        )
                                                        :: replaceCallBySubExpressionFix checkInfo.parentRange
                                                            checkInfo.callStyle
                                                            arrayArg
                                                    )
                                                )

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
            )
        ]


arrayIndexedMapChecks : IntoFnCheck
arrayIndexedMapChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck arrayCollection
        , intoFnCheckOnlyCall (operationWithExtraArgChecks { operationWithoutExtraArg = Fn.Array.map })
        ]


arrayIsEmptyChecks : IntoFnCheck
arrayIsEmptyChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionIsEmptyChecks arrayCollection)
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Array.fromList
            , combinedFn = Fn.List.isEmpty
            }
        , arrayOperationsKeepingLengthBeforeAreUnnecessaryChecks
        ]


arrayLengthChecks : IntoFnCheck
arrayLengthChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                collectionSizeChecks arrayCollection checkInfo
                    |> onNothing (\() -> arrayLengthOnArrayRepeatOrInitializeChecks checkInfo)
            )
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Array.fromList
            , combinedFn = Fn.List.length
            }
        , arrayOperationsKeepingLengthBeforeAreUnnecessaryChecks
        ]


arrayOperationsKeepingLengthBeforeAreUnnecessaryChecks : IntoFnCheck
arrayOperationsKeepingLengthBeforeAreUnnecessaryChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessarySpecificFnBeforeCheck
            { fn = Fn.Array.map
            , fnArgCount = 2
            , fnLastArgRepresents = "array"
            , whyUnnecessary = "Changing each element in an array does not affect its length"
            }
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.Array.indexedMap
            , fnArgCount = 2
            , fnLastArgRepresents = "array"
            , whyUnnecessary = "Changing each element in an array does not affect its length"
            }
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.Array.set
            , fnArgCount = 3
            , fnLastArgRepresents = "array"
            , whyUnnecessary = "Changing one element in an array does not affect its length"
            }
        ]


arrayGetChecks : IntoFnCheck
arrayGetChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (\checkInfo -> getChecks arrayCollection checkInfo)
        , earlierOperationCanBeMovedAfterAsForPerformanceChecks
            { laterOperationArgsDescription = Just "index"
            , earlierFn = Fn.Array.map
            , earlierFnArgCount = 2
            , earlierFnOperationArgsDescription = Just "function"
            , asLaterFn = Fn.Maybe.map
            }
        ]


arrayLengthOnArrayRepeatOrInitializeChecks : CallCheckInfo -> Maybe (Error {})
arrayLengthOnArrayRepeatOrInitializeChecks checkInfo =
    let
        maybeCall : Maybe ( String, { nodeRange : Range, fnRange : Range, firstArg : Node Expression, argsAfterFirst : List (Node Expression), callStyle : FunctionCallStyle } )
        maybeCall =
            AstHelpers.getSpecificUnreducedFnCall Fn.Array.repeat
                checkInfo.lookupTable
                checkInfo.firstArg
                |> Maybe.map (\call -> ( "repeat", call ))
                |> onNothing
                    (\() ->
                        AstHelpers.getSpecificUnreducedFnCall Fn.Array.initialize
                            checkInfo.lookupTable
                            checkInfo.firstArg
                            |> Maybe.map (\call -> ( "initialize", call ))
                    )
    in
    case maybeCall of
        Just ( fnName, call ) ->
            let
                maxFn : String
                maxFn =
                    qualifiedToString (qualify Fn.Basics.max defaultQualifyResources)
            in
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString (qualify checkInfo.fn checkInfo) ++ " on an array created by " ++ qualifiedToString (qualify ( [ "Array" ], fnName ) defaultQualifyResources) ++ " with a given length will result in that length"
                    , details = [ "You can replace this call by " ++ maxFn ++ " 0 with the given length. " ++ maxFn ++ " 0 makes sure that negative given lengths return 0." ]
                    }
                    checkInfo.fnRange
                    (Fix.insertAt checkInfo.parentRange.start (qualifiedToString (qualify Fn.Basics.max checkInfo) ++ " 0 ")
                        :: keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range call.firstArg }
                    )
                )

        Nothing ->
            Nothing


arraySetChecks : IntoFnCheck
arraySetChecks =
    collectionSetChecks arrayCollection


arraySliceChecks : IntoFnCheck
arraySliceChecks =
    intoFnChecksFirstThatConstructsError
        [ collectionSliceChecks arrayCollection
        , earlierOperationCanBeMovedAfterAsForPerformanceChecks
            { laterOperationArgsDescription = Just "indices"
            , earlierFn = Fn.Array.map
            , earlierFnArgCount = 2
            , earlierFnOperationArgsDescription = Just "function"
            , asLaterFn = Fn.Array.map
            }
        , operationWithSpecificFirstArgOnSpecificFnCanBeOptimizedBySwappingOperationsChecks
            { specificEarlierFn = Fn.Array.indexedMap
            , earlierFnOperationArgsDescription = "function"
            , generalLaterOperationArgsDescription = "indices"
            , isSpecificLaterFirstArg =
                \checkInfo laterFirstArg ->
                    Normalize.getInt checkInfo laterFirstArg == Just 0
            , specificLaterFirstArgDescription = "from index 0"
            }
        ]


arrayFilterChecks : IntoFnCheck
arrayFilterChecks =
    emptiableKeepWhenChecks arrayCollection


arrayAppendChecks : IntoFnCheck
arrayAppendChecks =
    intoFnCheckOnlyCall (collectionUnionChecks { leftElementsStayOnTheLeft = True } arrayCollection)


arrayFoldlChecks : IntoFnCheck
arrayFoldlChecks =
    arrayFoldChecks "foldl"


arrayFoldrChecks : IntoFnCheck
arrayFoldrChecks =
    arrayFoldChecks "foldr"


arrayFoldChecks : String -> IntoFnCheck
arrayFoldChecks foldFnName =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (\checkInfo -> emptiableFoldChecks arrayCollection checkInfo)
        , onConversionFnCallCanBeCombinedCheck
            { combinedOperationRepresents = "fold a list"
            , convertFn = Fn.Array.fromList
            , actionRepresents = "convert to an array"
            , combinedFn = ( [ "List" ], foldFnName )
            }
        ]



-- SET FUNCTIONS


setFromListChecks : IntoFnCheck
setFromListChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (emptiableFromListChecks setCollection)
        , wrapperFromListSingletonChecks setCollection
        , onSpecificFnCallReturnsItsLastArgCheck Fn.Set.toList
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case Node.value checkInfo.firstArg of
                    Expression.ListExpr elements ->
                        case List.map (\element -> Normalize.normalizeExpressionButKeepRange checkInfo element) elements of
                            [] ->
                                Nothing

                            first :: rest ->
                                allValuesDifferent
                                    checkInfo.expectNaN
                                    { message = "Set.fromList on a list with a duplicate key will only keep one of them"
                                    , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove one of the duplicate keys." ]
                                    }
                                    first
                                    rest

                    _ ->
                        Nothing
            )
        , intoFnCheckOnlyWhenExpectNaNIsNotEnabled
            (listReorderOperationsBeforeAreUnnecessaryChecks "final representation as a set")
        , intoFnCheckOnlyWhenExpectNaNIsNotEnabled
            (onRepeatReturnsWrapElementIfCountIsPositiveOtherwiseAlwaysFn
                { repeatFn = Fn.List.repeat
                , wrapFnOnRepeatCountPositive = Fn.Set.singleton
                , fnOnRepeatCountNotPositive = Fn.Set.empty
                }
            )
        ]


allValuesDifferent : Bool -> { message : String, details : List String } -> Node Expression -> List (Node Expression) -> Maybe (Error {})
allValuesDifferent expectingNaN errorInfo firstKeyToCheck otherKeysToCheck =
    findWithAccAndLookahead
        (\((Node keyRange _) as current) next dict ->
            if expectingNaN && AstHelpers.couldBeValueContainingNaN current then
                NotFound dict

            else
                let
                    key : String
                    key =
                        HashExpression.hash current
                in
                case Dict.get key dict of
                    Just ( found, extended ) ->
                        Found
                            (Rule.errorWithFix
                                errorInfo
                                found
                                [ Fix.removeRange extended ]
                            )

                    Nothing ->
                        case next of
                            Nothing ->
                                -- This is the last element, we won't find a duplicate
                                NotFound dict

                            Just (Node nextRange _) ->
                                NotFound
                                    (Dict.insert key
                                        ( keyRange
                                        , { start = keyRange.start
                                          , end = nextRange.start
                                          }
                                        )
                                        dict
                                    )
        )
        Dict.empty
        (firstKeyToCheck :: otherKeysToCheck)


type FindResult acc r
    = NotFound acc
    | Found r


findWithAccAndLookahead : (a -> Maybe a -> acc -> FindResult acc r) -> acc -> List a -> Maybe r
findWithAccAndLookahead f acc list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case f head (List.head tail) acc of
                Found r ->
                    Just r

                NotFound newAcc ->
                    findWithAccAndLookahead f newAcc tail


setIsEmptyChecks : IntoFnCheck
setIsEmptyChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionIsEmptyChecks setCollection)
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Set.fromList
            , combinedFn = Fn.List.isEmpty
            }
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.Set.map
            , fnArgCount = 2
            , fnLastArgRepresents = "set"
            , whyUnnecessary = "Mapping an empty set will result in an empty set and otherwise creating a new set with any element inserted will not be empty, even if all new elements are the same"
            }
        ]


setSizeChecks : IntoFnCheck
setSizeChecks =
    intoFnCheckOnlyCall (collectionSizeChecks setCollection)


setMemberChecks : IntoFnCheck
setMemberChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                callOnEmptyReturnsCheck
                    { resultAsString = \res -> qualifiedToString (qualify Fn.Basics.falseVariant res) }
                    setCollection
                    checkInfo
                    |> onNothing (\() -> knownMemberChecks setCollection checkInfo)
                    |> onNothing (\() -> wrapperMemberChecks setCollection checkInfo)
            )
        , containsElementOnConversionFnCallCanBeCombinedCheck
            { combinedOperationRepresents = "check for a list member"
            , convertFn = Fn.Set.fromList
            , convertedRepresentsIndefinite = "a set"
            , combinedFn = Fn.List.member
            }
        ]


setInsertChecks : IntoFnCheck
setInsertChecks =
    collectionInsertChecks setCollection


setRemoveChecks : IntoFnCheck
setRemoveChecks =
    collectionRemoveElementChecks setCollection


setFilterChecks : IntoFnCheck
setFilterChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableKeepWhenChecks setCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                if checkInfo.expectNaN then
                    Nothing

                else
                    case getSpecificOperationWithConstantFunction "/=" checkInfo checkInfo.firstArg of
                        Nothing ->
                            Nothing

                        Just inequalityTo ->
                            Just
                                (Rule.errorWithFix
                                    { message =
                                        qualifiedToString checkInfo.fn
                                            ++ " checking for inequality with a specific value is the same as "
                                            ++ qualifiedToString Fn.Set.remove
                                    , details =
                                        [ "You can replace this call by "
                                            ++ qualifiedToString Fn.Set.remove
                                            ++ " with the specific value you compared against which is meant for this exact purpose and will also be faster."
                                        ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify Fn.Set.remove checkInfo))
                                        :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) inequalityTo.constant
                                    )
                                )
            )
        ]


setPartitionChecks : IntoFnCheck
setPartitionChecks =
    intoFnCheckOnlyCall (collectionPartitionChecks setCollection)


setIntersectChecks : IntoFnCheck
setIntersectChecks =
    collectionIntersectChecks setCollection


setDiffChecks : IntoFnCheck
setDiffChecks =
    intoFnCheckOnlyCall (collectionDiffChecks setCollection)


setUnionChecks : IntoFnCheck
setUnionChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionUnionChecks { leftElementsStayOnTheLeft = True } setCollection)
        , unionWithFirstArgWrappedCanBeCombinedInto
            { combinedFn = Fn.Set.insert
            , wrapFn = Fn.Set.singleton
            , wrapFnArgCount = 1
            , articleWrapperName = "a singleton set"
            , wrapFnArgumentsDescription = "element"
            }
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Nothing ->
                        Nothing

                    Just secondSetArg ->
                        AstHelpers.getSpecificUnreducedFnCall Fn.Set.singleton
                            checkInfo.lookupTable
                            secondSetArg
                            |> Maybe.map
                                (\setSingletonCall ->
                                    let
                                        insertedElementRange : Range
                                        insertedElementRange =
                                            Node.range setSingletonCall.firstArg

                                        replacement : String
                                        replacement =
                                            qualifiedToString (qualify Fn.Set.insert checkInfo)
                                                ++ (if insertedElementRange.start.row == insertedElementRange.end.row then
                                                        " "

                                                    else
                                                        "\n"
                                                            ++ String.repeat
                                                                (insertedElementRange.start.column - 2)
                                                                " "
                                                   )
                                                ++ (if needsParens (Node.value setSingletonCall.firstArg) then
                                                        "(" ++ checkInfo.extractSourceCode insertedElementRange ++ ")"

                                                    else
                                                        checkInfo.extractSourceCode insertedElementRange
                                                   )
                                    in
                                    Rule.errorWithFix
                                        { message =
                                            qualifiedToString checkInfo.fn
                                                ++ " with a singleton set can be combined into "
                                                ++ qualifiedToString Fn.Set.insert
                                        , details =
                                            [ "You can replace this call by "
                                                ++ qualifiedToString Fn.Set.insert
                                                ++ " with the same element given to "
                                                ++ qualifiedToString Fn.Set.singleton
                                                ++ " which is meant for this exact purpose."
                                            ]
                                        }
                                        checkInfo.fnRange
                                        (Fix.replaceRangeBy checkInfo.fnRange replacement
                                            :: keepOnlyFix
                                                { parentRange = checkInfo.parentRange
                                                , keep =
                                                    Range.combine
                                                        [ checkInfo.fnRange
                                                        , Node.range checkInfo.firstArg
                                                        ]
                                                }
                                        )
                                )
            )
        , callWithTwoEqualArgumentsReturnsEitherArgumentCheck setCollection
        ]


setMapChecks : IntoFnCheck
setMapChecks =
    emptiableMapChecks setCollection


setToListChecks : IntoFnCheck
setToListChecks =
    intoFnCheckOnlyCall (emptiableToListChecks setCollection)


setFoldlChecks : IntoFnCheck
setFoldlChecks =
    intoFnCheckOnlyCall setFoldChecks


setFoldrChecks : IntoFnCheck
setFoldrChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            setFoldChecks checkInfo
                |> onNothing
                    (\() ->
                        case secondArg checkInfo of
                            Nothing ->
                                Nothing

                            Just initialArg ->
                                if
                                    isEmptyList initialArg
                                        && Normalize.isSpecificUnappliedBinaryOperation "::" checkInfo checkInfo.firstArg
                                then
                                    Just
                                        (operationWithSpecificArgsIsEquivalentToFnError
                                            { specificArgsDescription = "(::) []"
                                            , replacementFn = Fn.Set.toList
                                            }
                                            checkInfo
                                        )

                                else
                                    Nothing
                    )
        )


setFoldChecks : CallCheckInfo -> Maybe (Error {})
setFoldChecks checkInfo =
    emptiableFoldChecks setCollection checkInfo
        |> onNothing
            (\() ->
                if checkInfo.expectNaN then
                    Nothing

                else
                    case secondArg checkInfo of
                        Nothing ->
                            Nothing

                        Just initialArg ->
                            if
                                AstHelpers.isSpecificValueReference checkInfo.lookupTable Fn.Set.empty initialArg
                                    && AstHelpers.isSpecificValueOrFn Fn.Set.insert checkInfo checkInfo.firstArg
                            then
                                Just
                                    (alwaysReturnsLastArgError
                                        (qualifiedToString checkInfo.fn ++ " Set.insert Set.empty")
                                        setCollection
                                        checkInfo
                                    )

                            else
                                Nothing
            )



-- DICT FUNCTIONS


dictFromListChecks : IntoFnCheck
dictFromListChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (emptiableFromListChecks dictCollection)
        , onSpecificFnCallReturnsItsLastArgCheck Fn.Dict.toList
        , intoFnCheckOnlyCall
            (\checkInfo ->
                (case Node.value checkInfo.firstArg of
                    Expression.ListExpr elements ->
                        case elements of
                            [] ->
                                Nothing

                            [ _ ] ->
                                Nothing

                            first :: rest ->
                                let
                                    toEntry : Node Expression -> { entryRange : Range, first : Maybe (Node Expression) }
                                    toEntry entry =
                                        { entryRange = Node.range entry
                                        , first =
                                            AstHelpers.getTuple2 checkInfo.lookupTable entry
                                                |> Maybe.map (\tuple -> Normalize.normalizeExpressionButKeepRange checkInfo tuple.first)
                                        }
                                in
                                allKeysDifferent checkInfo.expectNaN (toEntry first) (List.map toEntry rest)
                                    |> onNothing
                                        (\() ->
                                            allValuesDifferent
                                                checkInfo.expectNaN
                                                { message = "Dict.fromList on a list with a duplicate entry will only keep one of them"
                                                , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove earlier entries with duplicate keys." ]
                                                }
                                                (Normalize.normalizeExpressionButKeepRange checkInfo first)
                                                (List.map (\entry -> Normalize.normalizeExpressionButKeepRange checkInfo entry) rest)
                                        )

                    _ ->
                        Nothing
                )
                    |> onNothing (\() -> dictFromListOnRepeatCheck checkInfo)
            )
        ]


dictFromListOnRepeatCheck : CallCheckInfo -> Maybe (Error {})
dictFromListOnRepeatCheck checkInfo =
    if checkInfo.expectNaN then
        Nothing

    else
        case AstHelpers.getSpecificUnreducedFnCall Fn.List.repeat checkInfo.lookupTable checkInfo.firstArg of
            Nothing ->
                Nothing

            Just repeatFnCall ->
                case repeatFnCall.argsAfterFirst of
                    [ elementToRepeatArg ] ->
                        let
                            branchIndentation : String
                            branchIndentation =
                                String.repeat ((Node.range elementToRepeatArg).start.column - 1) " "
                        in
                        Just
                            (Rule.errorWithFix
                                { message =
                                    qualifiedToString checkInfo.fn
                                        ++ " on "
                                        ++ qualifiedToString Fn.List.repeat
                                        ++ " will result in a dict singleton with the repeated element if the count is positive and "
                                        ++ qualifiedToString Fn.Dict.empty
                                        ++ " otherwise"
                                , details =
                                    [ "You can replace this call by if (the count argument given to "
                                        ++ qualifiedToString Fn.List.repeat
                                        ++ ") >= 1 then "
                                        ++ qualifiedToString Fn.Dict.fromList
                                        ++ " [ (the element to repeat argument given to "
                                        ++ qualifiedToString Fn.List.repeat
                                        ++ ") ] else "
                                        ++ qualifiedToString Fn.Dict.empty
                                        ++ "."
                                    ]
                                }
                                checkInfo.fnRange
                                (Fix.insertAt checkInfo.parentRange.start
                                    ("(if "
                                        ++ parenthesizeIf
                                            (needsParens (Node.value repeatFnCall.firstArg))
                                            (checkInfo.extractSourceCode
                                                (Node.range repeatFnCall.firstArg)
                                            )
                                        ++ " >= 1 then\n"
                                        ++ branchIndentation
                                    )
                                    :: Fix.insertAt checkInfo.parentRange.end
                                        ("\n\n"
                                            ++ String.repeat (checkInfo.parentRange.start.column - 1) " "
                                            ++ "else\n"
                                            ++ branchIndentation
                                            ++ qualifiedToString (qualify Fn.Dict.empty checkInfo)
                                            ++ ")"
                                        )
                                    :: keepOnlyAndSurroundWithFix
                                        { parentRange = Node.range checkInfo.firstArg
                                        , keep = Node.range elementToRepeatArg
                                        , left = "[ "
                                        , right = " ]"
                                        }
                                )
                            )

                    _ ->
                        Nothing


allKeysDifferent : Bool -> { entryRange : Range, first : Maybe (Node Expression) } -> List { entryRange : Range, first : Maybe (Node Expression) } -> Maybe (Error {})
allKeysDifferent expectingNaN entry otherEntriesToCheck =
    case otherEntriesToCheck of
        nextEntry :: restOfEntries ->
            case entry.first of
                Just firstKey ->
                    if
                        (not expectingNaN || not (AstHelpers.couldBeValueContainingNaN firstKey))
                            && isAnyTheSameAsBy firstKey otherEntriesToCheck
                    then
                        Just
                            (Rule.errorWithFix
                                { message =
                                    "Dict.fromList on entries with a duplicate key will only keep the last entry"
                                , details =
                                    [ "Maybe one of the keys was supposed to be a different value? If not, you can remove earlier entries with duplicate keys." ]
                                }
                                (Node.range firstKey)
                                [ Fix.removeRange
                                    { start = entry.entryRange.start
                                    , end = nextEntry.entryRange.start
                                    }
                                ]
                            )

                    else
                        allKeysDifferent expectingNaN nextEntry restOfEntries

                Nothing ->
                    allKeysDifferent expectingNaN nextEntry restOfEntries

        [] ->
            -- entry is the last element
            -- so it can't be equal to any other key
            Nothing


isAnyTheSameAsBy : Node Expression -> List { a | first : Maybe (Node Expression) } -> Bool
isAnyTheSameAsBy first rest =
    List.any
        (\node ->
            case node.first of
                Just element ->
                    Node.value element == Node.value first

                Nothing ->
                    False
        )
        rest


dictIsEmptyChecks : IntoFnCheck
dictIsEmptyChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionIsEmptyChecks dictCollection)
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.fromList
            , combinedFn = Fn.List.isEmpty
            }
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.Dict.map
            , fnArgCount = 2
            , fnLastArgRepresents = "dict"
            , whyUnnecessary = "Changing each value in a dict does not affect its size"
            }
        ]


dictSizeChecks : IntoFnCheck
dictSizeChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionSizeChecks dictCollection)
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.Dict.map
            , fnArgCount = 2
            , fnLastArgRepresents = "dict"
            , whyUnnecessary = "Changing each value in a dict does not affect its size"
            }
        ]


dictMemberChecks : IntoFnCheck
dictMemberChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                callOnEmptyReturnsCheck
                    { resultAsString = \res -> qualifiedToString (qualify Fn.Basics.falseVariant res) }
                    dictCollection
                    checkInfo
                    |> onNothing
                        (\() ->
                            knownMemberChecks
                                { represents = "dict"
                                , representsPlural = "dicts"
                                , elements =
                                    { countDescription = "size"
                                    , elementDescription = "key"
                                    , determineCount = determineCollectionSize
                                    , get = dictGetKeys
                                    }
                                }
                                checkInfo
                        )
            )
        , dictMemberOnFromListToListAnyChecks
        ]


dictMemberOnFromListToListAnyChecks : IntoFnCheck
dictMemberOnFromListToListAnyChecks =
    { call =
        \checkInfo ->
            if checkInfo.expectNaN && AstHelpers.couldBeValueContainingNaN checkInfo.firstArg then
                Nothing

            else
                case fullyAppliedLastArg checkInfo of
                    Just convertedArg ->
                        case AstHelpers.getSpecificUnreducedFnCall Fn.Dict.fromList checkInfo.lookupTable convertedArg of
                            Just conversionCall ->
                                if checkInfo.expectNaN && AstHelpers.couldBeValueContainingNaN conversionCall.firstArg then
                                    Nothing

                                else
                                    let
                                        needleArgRange : Range
                                        needleArgRange =
                                            Node.range checkInfo.firstArg
                                    in
                                    Just
                                        (Rule.errorWithFix
                                            { message =
                                                qualifiedToString checkInfo.fn
                                                    ++ " on "
                                                    ++ qualifiedToString Fn.Dict.fromList
                                                    ++ " can be replaced by List.any with a function comparing the key"
                                            , details =
                                                [ "You can replace these calls by "
                                                    ++ qualifiedToString Fn.List.any
                                                    ++ " comparing the key which is both simpler and faster. The automatic fix suggests Tuple.first >> (==) ... to only evaluate the member to check for once. However, if it is a variable, a simpler alternative might be using a lambda like \\( k, _ ) -> k == ..."
                                                ]
                                            }
                                            checkInfo.fnRange
                                            (Fix.replaceRangeBy checkInfo.fnRange
                                                (qualifiedToString (qualify Fn.List.any checkInfo))
                                                :: Fix.insertAt needleArgRange.start
                                                    ("("
                                                        ++ qualifiedToString (qualify Fn.Tuple.first checkInfo)
                                                        ++ " >> (==)"
                                                        ++ (if needleArgRange.start.row == needleArgRange.end.row then
                                                                " "

                                                            else
                                                                "\n" ++ String.repeat (needleArgRange.start.column - 1) " "
                                                           )
                                                    )
                                                :: Fix.insertAt needleArgRange.end ")"
                                                :: replaceBySubExpressionFix conversionCall.nodeRange conversionCall.firstArg
                                            )
                                        )

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
    , composition =
        \checkInfo ->
            if
                Basics.not checkInfo.expectNaN
                    && (checkInfo.earlier.fn == Fn.Dict.fromList)
            then
                case checkInfo.later.args of
                    [ needleArg ] ->
                        let
                            needleArgRange : Range
                            needleArgRange =
                                Node.range needleArg
                        in
                        Just
                            { info =
                                { message =
                                    qualifiedToString checkInfo.later.fn
                                        ++ " on "
                                        ++ qualifiedToString Fn.Dict.fromList
                                        ++ " can be replaced by List.any with a function comparing the key"
                                , details =
                                    [ "You can replace this composition by "
                                        ++ qualifiedToString Fn.List.any
                                        ++ " comparing the key which is both simpler and faster. The automatic fix suggests Tuple.first >> (==) ... to only evaluate the member to check for once. However, if it is a variable, a simpler alternative might be using a lambda like \\( k, _ ) -> k == ..."
                                    ]
                                }
                            , fix =
                                [ Fix.removeRange checkInfo.earlier.removeRange
                                , Fix.replaceRangeBy checkInfo.later.fnRange
                                    (qualifiedToString (qualify Fn.List.any checkInfo))
                                , Fix.insertAt needleArgRange.start
                                    ("("
                                        ++ qualifiedToString (qualify Fn.Tuple.first checkInfo)
                                        ++ " >> (==) ("
                                        ++ (if needleArgRange.start.row == needleArgRange.end.row then
                                                ""

                                            else
                                                "\n" ++ String.repeat (needleArgRange.start.column - 1) " "
                                           )
                                    )
                                , Fix.insertAt needleArgRange.end "))"
                                ]
                            }

                    _ ->
                        Nothing

            else
                Nothing
    }


dictInsertChecks : IntoFnCheck
dictInsertChecks =
    operationOverridesPreviousOperationWithEqualFirstArgCheck { firstArgDescription = "key" }


dictRemoveChecks : IntoFnCheck
dictRemoveChecks =
    intoFnChecksFirstThatConstructsError
        [ collectionRemoveElementChecks dictCollection
        , earlierOperationCanBeMovedAfterAsForPerformanceChecks
            { laterOperationArgsDescription = Just "key"
            , earlierFn = Fn.Dict.map
            , earlierFnArgCount = 2
            , earlierFnOperationArgsDescription = Just "function"
            , asLaterFn = Fn.Dict.map
            }
        ]


dictUpdateChecks : IntoFnCheck
dictUpdateChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            case secondArg checkInfo of
                Nothing ->
                    Nothing

                Just updateFunctionArgument ->
                    if AstHelpers.isIdentity checkInfo updateFunctionArgument then
                        Just
                            (alwaysReturnsLastArgError
                                (qualifiedToString checkInfo.fn ++ " with an identity update function")
                                dictCollection
                                checkInfo
                            )

                    else
                        case AstHelpers.getAlwaysResult checkInfo updateFunctionArgument of
                            Nothing ->
                                Nothing

                            Just updateFunctionArgumentAlwaysResult ->
                                if
                                    AstHelpers.isSpecificValueReference checkInfo.lookupTable
                                        Fn.Maybe.nothingVariant
                                        updateFunctionArgumentAlwaysResult
                                then
                                    let
                                        replacementFn : ( ModuleName, String )
                                        replacementFn =
                                            Fn.Dict.remove
                                    in
                                    Just
                                        (Rule.errorWithFix
                                            { message =
                                                qualifiedToString checkInfo.fn
                                                    ++ " with an update function that is always Nothing is the same as "
                                                    ++ qualifiedToString replacementFn
                                            , details =
                                                [ "You can replace this call by "
                                                    ++ qualifiedToString replacementFn
                                                    ++ " with the same given key."
                                                ]
                                            }
                                            checkInfo.fnRange
                                            (Fix.replaceRangeBy
                                                checkInfo.fnRange
                                                (qualifiedToString (qualify replacementFn checkInfo))
                                                :: keepOnlyFix
                                                    { parentRange =
                                                        Range.combine
                                                            [ checkInfo.fnRange
                                                            , Node.range checkInfo.firstArg
                                                            , Node.range updateFunctionArgument
                                                            ]
                                                    , keep =
                                                        Range.combine
                                                            [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                                    }
                                            )
                                        )

                                else
                                    case
                                        AstHelpers.getSpecificUnreducedFnCall Fn.Maybe.justVariant
                                            checkInfo.lookupTable
                                            updateFunctionArgumentAlwaysResult
                                    of
                                        Nothing ->
                                            Nothing

                                        Just updateFunctionArgumentAlwaysResultJustCall ->
                                            let
                                                replacementFn : ( ModuleName, String )
                                                replacementFn =
                                                    Fn.Dict.insert
                                            in
                                            Just
                                                (Rule.errorWithFix
                                                    { message =
                                                        qualifiedToString checkInfo.fn
                                                            ++ " with an update function that is always Just a value is the same as "
                                                            ++ qualifiedToString replacementFn
                                                            ++ " with that value"
                                                    , details =
                                                        [ "You can replace this call by "
                                                            ++ qualifiedToString replacementFn
                                                            ++ " with the same given key and the value inside the Just variant."
                                                        ]
                                                    }
                                                    checkInfo.fnRange
                                                    (Fix.replaceRangeBy
                                                        checkInfo.fnRange
                                                        (qualifiedToString (qualify replacementFn checkInfo))
                                                        :: replaceBySubExpressionFix
                                                            (Node.range updateFunctionArgument)
                                                            updateFunctionArgumentAlwaysResultJustCall.firstArg
                                                    )
                                                )
        )


dictFilterChecks : IntoFnCheck
dictFilterChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableKeepWhenWithExtraArgChecks dictCollection
        , operationWithSpecificFirstArgOnSpecificFnCanBeOptimizedBySwappingOperationsChecks
            { specificEarlierFn = Fn.Dict.map
            , earlierFnOperationArgsDescription = "function"
            , generalLaterOperationArgsDescription = "test function"
            , isSpecificLaterFirstArg =
                \checkInfo laterFirstArg ->
                    isJust (getFunctionIgnoringSecondIncoming checkInfo laterFirstArg)
            , specificLaterFirstArgDescription = "by key"
            }
        , intoFnCheckOnlyCall
            (\checkInfo ->
                if checkInfo.expectNaN then
                    Nothing

                else
                    case getFunctionIgnoringSecondIncoming checkInfo checkInfo.firstArg of
                        Nothing ->
                            Nothing

                        Just filterFunctionArgIgnoringSecond ->
                            case getSpecificOperationWithConstantFunction "/=" checkInfo filterFunctionArgIgnoringSecond of
                                Nothing ->
                                    Nothing

                                Just inequalityTo ->
                                    Just
                                        (Rule.errorWithFix
                                            { message =
                                                qualifiedToString checkInfo.fn
                                                    ++ " checking each key for inequality with a specific value is the same as "
                                                    ++ qualifiedToString Fn.Dict.remove
                                            , details =
                                                [ "You can replace this call by "
                                                    ++ qualifiedToString Fn.Dict.remove
                                                    ++ " with the specific value you compared against which is meant for this exact purpose and will also be faster."
                                                ]
                                            }
                                            checkInfo.fnRange
                                            (Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify Fn.Dict.remove checkInfo))
                                                :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) inequalityTo.constant
                                            )
                                        )
            )
        ]


dictPartitionChecks : IntoFnCheck
dictPartitionChecks =
    intoFnCheckOnlyCall (emptiablePartitionWithExtraArgChecks dictCollection)


dictMapChecks : IntoFnCheck
dictMapChecks =
    emptiableMapWithExtraArgChecks dictCollection


dictIntersectChecks : IntoFnCheck
dictIntersectChecks =
    collectionIntersectChecks dictCollection


dictDiffChecks : IntoFnCheck
dictDiffChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessarySpecificFnBeforeCheck
            { fn = Fn.Dict.map
            , fnArgCount = 2
            , fnLastArgRepresents = "dict"
            , whyUnnecessary = qualifiedToString Fn.Dict.diff ++ " removes all keys present in the second dict, therefore mapping only its values will have no effect"
            }
        , intoFnCheckOnlyCall
            (\checkInfo ->
                collectionDiffChecks dictCollection checkInfo
                    |> onNothing
                        (\() ->
                            case checkInfo.argsAfterFirst of
                                [ _ ] ->
                                    case
                                        AstHelpers.getSpecificUnreducedFnCall Fn.Dict.map
                                            checkInfo.lookupTable
                                            -- to let the resulting .nodeRange be only the call itself
                                            (AstHelpers.removeParens checkInfo.firstArg)
                                    of
                                        Nothing ->
                                            Nothing

                                        Just dictMapFnCall ->
                                            case dictMapFnCall.argsAfterFirst of
                                                [ unmappedDictArg ] ->
                                                    Just
                                                        (Rule.errorWithFix
                                                            { message =
                                                                qualifiedToString checkInfo.fn
                                                                    ++ " with a base dict resulting from a "
                                                                    ++ qualifiedToString Fn.Dict.map
                                                                    ++ " can be optimized to "
                                                                    ++ qualifiedToString Fn.Dict.map
                                                                    ++ " on "
                                                                    ++ qualifiedToString checkInfo.fn
                                                            , details =
                                                                [ "You can replace this call by "
                                                                    ++ qualifiedToString Fn.Dict.map
                                                                    ++ " with the function given to the original call, on "
                                                                    ++ qualifiedToString checkInfo.fn
                                                                    ++ " with the unmapped dict and the dict containing the keys to remove given to the original call."
                                                                ]
                                                            }
                                                            checkInfo.fnRange
                                                            (extractAndInsertCallAroundReplacementLastArgFix
                                                                { parenthesize = True
                                                                , extractSourceCode = checkInfo.extractSourceCode
                                                                , replacementLastArgRange = checkInfo.parentRange
                                                                , parenthesizeReplacementLastArg = True
                                                                , originalCallRange = dictMapFnCall.nodeRange
                                                                , originalCallStyle = dictMapFnCall.callStyle
                                                                , originalLastArgRange = Node.range unmappedDictArg
                                                                }
                                                                ++ replaceBySubExpressionFix (Node.range checkInfo.firstArg)
                                                                    unmappedDictArg
                                                            )
                                                        )

                                                _ ->
                                                    Nothing

                                _ ->
                                    Nothing
                        )
            )
        ]


dictUnionChecks : IntoFnCheck
dictUnionChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                collectionUnionChecks { leftElementsStayOnTheLeft = False } dictCollection checkInfo
            )
        , callWithTwoEqualArgumentsReturnsEitherArgumentCheck dictCollection
        , unionWithFirstArgWrappedCanBeCombinedInto
            { combinedFn = Fn.Dict.insert
            , wrapFn = Fn.Dict.singleton
            , wrapFnArgCount = 2
            , articleWrapperName = "a singleton dict"
            , wrapFnArgumentsDescription = "key and value"
            }
        ]


dictToListChecks : IntoFnCheck
dictToListChecks =
    intoFnCheckOnlyCall (emptiableToListChecks dictCollection)


dictFoldlChecks : IntoFnCheck
dictFoldlChecks =
    intoFnCheckOnlyCall dictFoldChecks


dictFoldrChecks : IntoFnCheck
dictFoldrChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            dictFoldChecks checkInfo
                |> onNothing
                    (\() ->
                        case secondArg checkInfo of
                            Nothing ->
                                Nothing

                            Just initialArg ->
                                if isEmptyList initialArg then
                                    if
                                        case AstHelpers.getAlwaysResult checkInfo checkInfo.firstArg of
                                            Nothing ->
                                                False

                                            Just reduceIgnoringKey ->
                                                Normalize.isSpecificUnappliedBinaryOperation "::" checkInfo reduceIgnoringKey
                                    then
                                        Just
                                            (operationWithSpecificArgsIsEquivalentToFnError
                                                { specificArgsDescription = "(\\_ v vs -> v :: vs) []"
                                                , replacementFn = Fn.Dict.values
                                                }
                                                checkInfo
                                            )

                                    else if
                                        case getFunctionIgnoringSecondIncoming checkInfo checkInfo.firstArg of
                                            Nothing ->
                                                False

                                            Just reduceIgnoringValue ->
                                                Normalize.isSpecificUnappliedBinaryOperation "::" checkInfo reduceIgnoringValue
                                    then
                                        Just
                                            (operationWithSpecificArgsIsEquivalentToFnError
                                                { specificArgsDescription = "(\\k _ ks -> k :: ks) []"
                                                , replacementFn = Fn.Dict.keys
                                                }
                                                checkInfo
                                            )

                                    else if
                                        case getFunctionWithFirstAndSecondIncomingAsTuple checkInfo checkInfo.firstArg of
                                            Nothing ->
                                                False

                                            Just reduceFunctionWithKeyValueIncomingAsTuple ->
                                                Normalize.isSpecificUnappliedBinaryOperation "::"
                                                    checkInfo
                                                    reduceFunctionWithKeyValueIncomingAsTuple
                                    then
                                        Just
                                            (operationWithSpecificArgsIsEquivalentToFnError
                                                { specificArgsDescription = "(\\k v kvs -> ( k, v ) :: kvs) []"
                                                , replacementFn = Fn.Dict.toList
                                                }
                                                checkInfo
                                            )

                                    else
                                        Nothing

                                else
                                    Nothing
                    )
        )


getFunctionWithFirstAndSecondIncomingAsTuple :
    AstHelpers.ReduceLambdaResources a
    -> Node Expression
    -> Maybe (Node Expression)
getFunctionWithFirstAndSecondIncomingAsTuple resources expressionNode =
    case AstHelpers.getCollapsedLambda expressionNode of
        Nothing ->
            Nothing

        Just lambda ->
            case lambda.patterns of
                keyPattern :: valuePattern :: patternsAfterKeyValue ->
                    Just
                        (Node.empty
                            (Expression.LambdaExpression
                                { args =
                                    Node.empty (Pattern.TuplePattern [ keyPattern, valuePattern ])
                                        :: patternsAfterKeyValue
                                , expression = lambda.expression
                                }
                            )
                        )

                [ keyPatternOnly ] ->
                    case getFullComposition lambda.expression of
                        Nothing ->
                            Nothing

                        Just fullComposition ->
                            if
                                AstHelpers.isSpecificValueOrFn Fn.Tuple.pair
                                    resources
                                    (Node.empty
                                        (Expression.LambdaExpression
                                            { args = [ keyPatternOnly ]
                                            , expression = fullComposition.earlier
                                            }
                                        )
                                    )
                            then
                                Just fullComposition.composedLater

                            else
                                Nothing

                _ ->
                    Nothing


getFunctionIgnoringSecondIncoming :
    AstHelpers.ReduceLambdaResources a
    -> Node Expression
    -> Maybe (Node Expression)
getFunctionIgnoringSecondIncoming resources expressionNode =
    case expressionNode of
        Node _ (Expression.ParenthesizedExpression inParens) ->
            getFunctionIgnoringSecondIncoming resources inParens

        Node _ (Expression.LambdaExpression lambda) ->
            case lambda.args of
                [] ->
                    getFunctionIgnoringSecondIncoming resources
                        lambda.expression

                keyPattern :: reduceLambdaPatternsAfterKey ->
                    AstHelpers.getAlwaysResult resources
                        (case reduceLambdaPatternsAfterKey of
                            [] ->
                                lambda.expression

                            _ :: _ ->
                                Node.empty
                                    (Expression.LambdaExpression
                                        { args = reduceLambdaPatternsAfterKey
                                        , expression = lambda.expression
                                        }
                                    )
                        )
                        |> Maybe.map
                            (\alwaysResult ->
                                Node.empty
                                    (Expression.LambdaExpression
                                        { args = [ keyPattern ]
                                        , expression = alwaysResult
                                        }
                                    )
                            )

        _ ->
            case getFullComposition expressionNode of
                Nothing ->
                    Nothing

                Just fullComposition ->
                    if AstHelpers.isSpecificValueOrFn Fn.Basics.always resources fullComposition.composedLater then
                        Just fullComposition.earlier

                    else
                        Nothing


dictFoldChecks : CallCheckInfo -> Maybe (Error {})
dictFoldChecks checkInfo =
    emptiableFoldWithExtraArgChecks dictCollection checkInfo
        |> onNothing
            (\() ->
                if checkInfo.expectNaN then
                    Nothing

                else
                    case secondArg checkInfo of
                        Nothing ->
                            Nothing

                        Just initialArg ->
                            if
                                AstHelpers.isSpecificValueReference checkInfo.lookupTable Fn.Dict.empty initialArg
                                    && AstHelpers.isSpecificValueOrFn Fn.Dict.insert checkInfo checkInfo.firstArg
                            then
                                Just
                                    (alwaysReturnsLastArgError
                                        (qualifiedToString checkInfo.fn ++ " Dict.insert Dict.empty")
                                        dictCollection
                                        checkInfo
                                    )

                            else
                                Nothing
            )



-- PLATFORM.CMD FUNCTIONS


platformCmdBatchChecks : IntoFnCheck
platformCmdBatchChecks =
    emptiableFlatFromListChecks cmdCollection


platformCmdMapChecks : IntoFnCheck
platformCmdMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks cmdCollection
        , mapAfterConvertMapIdentityCanBeCombinedCheck { convertMapFn = Fn.Task.perform }
        , mapAfterConvertMapIdentityCanBeCombinedCheck { convertMapFn = Fn.Task.attempt }
        ]


mapAfterConvertMapIdentityCanBeCombinedCheck :
    { config | convertMapFn : ( ModuleName, String ) }
    -> IntoFnCheck
mapAfterConvertMapIdentityCanBeCombinedCheck config =
    { composition =
        \checkInfo ->
            let
                earlierIsConvertMapIdentity : Bool
                earlierIsConvertMapIdentity =
                    (checkInfo.earlier.fn == config.convertMapFn)
                        && (case checkInfo.earlier.args of
                                [ changeFunctionArg ] ->
                                    AstHelpers.isIdentity checkInfo changeFunctionArg

                                _ ->
                                    False
                           )
            in
            if earlierIsConvertMapIdentity then
                Just
                    { info =
                        { message =
                            qualifiedToString checkInfo.later.fn
                                ++ " on "
                                ++ qualifiedToString config.convertMapFn
                                ++ " with an identity function can be combined"
                        , details =
                            [ "You can replace this composition by "
                                ++ qualifiedToString config.convertMapFn
                                ++ " with the function given to "
                                ++ qualifiedToString (qualify checkInfo.later.fn checkInfo)
                                ++ "."
                            ]
                        }
                    , fix =
                        [ Fix.removeRange checkInfo.earlier.removeRange
                        , Fix.replaceRangeBy
                            checkInfo.later.fnRange
                            (qualifiedToString (qualify config.convertMapFn checkInfo))
                        ]
                    }

            else
                Nothing
    , call =
        \checkInfo ->
            case fullyAppliedLastArg checkInfo of
                Nothing ->
                    Nothing

                Just cmdArg ->
                    case AstHelpers.getSpecificUnreducedFnCall config.convertMapFn checkInfo.lookupTable cmdArg of
                        Nothing ->
                            Nothing

                        Just convertMapFnCall ->
                            if AstHelpers.isIdentity checkInfo convertMapFnCall.firstArg then
                                case convertMapFnCall.argsAfterFirst of
                                    [ toConvertArg ] ->
                                        Just
                                            (Rule.errorWithFix
                                                { message =
                                                    qualifiedToString checkInfo.fn
                                                        ++ " on "
                                                        ++ qualifiedToString config.convertMapFn
                                                        ++ " with an identity function can be combined"
                                                , details =
                                                    [ "You can replace these operations by "
                                                        ++ qualifiedToString config.convertMapFn
                                                        ++ " with the function given to "
                                                        ++ qualifiedToString (qualify checkInfo.fn checkInfo)
                                                        ++ "."
                                                    ]
                                                }
                                                checkInfo.fnRange
                                                (Fix.replaceRangeBy
                                                    checkInfo.fnRange
                                                    (qualifiedToString (qualify config.convertMapFn checkInfo))
                                                    :: replaceBySubExpressionFix
                                                        (Node.range cmdArg)
                                                        toConvertArg
                                                )
                                            )

                                    _ ->
                                        Nothing

                            else
                                Nothing
    }



-- PLATFORM.SUB FUNCTIONS


platformSubBatchChecks : IntoFnCheck
platformSubBatchChecks =
    emptiableFlatFromListChecks subCollection


platformSubChecks : IntoFnCheck
platformSubChecks =
    emptiableMapChecks subCollection



-- TASK FUNCTIONS


taskMapChecks : IntoFnCheck
taskMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks taskWithSucceedAsWrap
        , mapOnWrappedChecks taskWithSucceedAsWrap
        ]


taskMapNChecks : IntoFnCheck
taskMapNChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            wrapperMapNChecks taskWithSucceedAsWrap checkInfo
                |> onNothing (\() -> mapNOrFirstEmptyConstructionChecks taskWithSucceedAsWrap checkInfo)
        )


taskAndThenChecks : IntoFnCheck
taskAndThenChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck taskWithSucceedAsWrap
        , wrapperFlatMapChecks taskWithSucceedAsWrap
        ]


taskMapErrorChecks : IntoFnCheck
taskMapErrorChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks taskWithFailAsWrap
        , mapOnWrappedChecks taskWithFailAsWrap
        ]


taskOnErrorChecks : IntoFnCheck
taskOnErrorChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck taskWithFailAsWrap
        , wrapperFlatMapChecks taskWithFailAsWrap
        ]


taskSequenceChecks : IntoFnCheck
taskSequenceChecks =
    intoFnChecksFirstThatConstructsError
        [ listOfWrapperSequenceChecks taskWithSucceedAsWrap
        , intoFnCheckOnlyCall
            (sequenceOrFirstEmptyChecks ( listCollection, taskWithSucceedAsWrap ))
        ]


taskPerformChecks : IntoFnCheck
taskPerformChecks =
    mapToOperationWithIdentityCanBeCombinedToOperationChecks taskWithSucceedAsWrap


taskAttemptChecks : IntoFnCheck
taskAttemptChecks =
    mapToOperationWithIdentityCanBeCombinedToOperationChecks taskWithSucceedAsWrap



-- HTML.ATTRIBUTES FUNCTIONS


getTupleWithSpecificSecondBoolExpressionNode : Bool -> ModuleNameLookupTable -> Node Expression -> Maybe (Node Expression)
getTupleWithSpecificSecondBoolExpressionNode specificBool lookupTable expressionNode =
    AstHelpers.getTuple2Literal expressionNode
        |> Maybe.andThen
            (\tuple ->
                if
                    AstHelpers.isSpecificValueReference lookupTable
                        (if specificBool then
                            Fn.Basics.trueVariant

                         else
                            Fn.Basics.falseVariant
                        )
                        tuple.second
                then
                    Just expressionNode

                else
                    Nothing
            )


htmlAttributesClassListFalseElementErrorInfo : CallCheckInfo -> { message : String, details : List String }
htmlAttributesClassListFalseElementErrorInfo checkInfo =
    { message = "In a " ++ qualifiedToString checkInfo.fn ++ ", a tuple paired with False can be removed"
    , details = [ "You can remove the tuple list element where the second part is False." ]
    }


htmlAttributesClassListChecks : IntoFnCheck
htmlAttributesClassListChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            (case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
                Just singletonElement ->
                    case AstHelpers.getTuple2Literal singletonElement of
                        Just tuple ->
                            case AstHelpers.getBool checkInfo.lookupTable tuple.second of
                                Just bool ->
                                    if bool then
                                        let
                                            replacementFn : ( ModuleName, String )
                                            replacementFn =
                                                Fn.Html.Attributes.class
                                        in
                                        Just
                                            (Rule.errorWithFix
                                                { message = qualifiedToString checkInfo.fn ++ " with a single tuple paired with True can be replaced with " ++ qualifiedToString replacementFn
                                                , details = [ "You can replace this call by " ++ qualifiedToString replacementFn ++ " with the String from the single tuple list element." ]
                                                }
                                                checkInfo.fnRange
                                                (Fix.replaceRangeBy checkInfo.fnRange
                                                    (qualifiedToString (qualify replacementFn checkInfo))
                                                    :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) tuple.first
                                                )
                                            )

                                    else
                                        Just
                                            (Rule.errorWithFix (htmlAttributesClassListFalseElementErrorInfo checkInfo)
                                                checkInfo.fnRange
                                                [ Fix.replaceRangeBy (Node.range checkInfo.firstArg) "[]" ]
                                            )

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
            )
                |> onNothing
                    (\() ->
                        case AstHelpers.getListLiteral checkInfo.firstArg of
                            Just (tuple0 :: tuple1 :: tuple2Up) ->
                                case findMapNeighboring (\el -> getTupleWithSpecificSecondBoolExpressionNode False checkInfo.lookupTable el) (tuple0 :: tuple1 :: tuple2Up) of
                                    Just classPart ->
                                        Just
                                            (Rule.errorWithFix (htmlAttributesClassListFalseElementErrorInfo checkInfo)
                                                checkInfo.fnRange
                                                (listLiteralRemoveElementFix classPart)
                                            )

                                    Nothing ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> onNothing
                    (\() ->
                        case AstHelpers.getCollapsedCons checkInfo.firstArg of
                            Just classParts ->
                                case findMapNeighboring (\el -> getTupleWithSpecificSecondBoolExpressionNode False checkInfo.lookupTable el) classParts.consed of
                                    Just classPart ->
                                        Just
                                            (Rule.errorWithFix (htmlAttributesClassListFalseElementErrorInfo checkInfo)
                                                checkInfo.fnRange
                                                (collapsedConsRemoveElementFix
                                                    { toRemove = classPart
                                                    , tailRange = Node.range classParts.tail
                                                    }
                                                )
                                            )

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing
                    )
        )



-- JSON.ENCODE FUNCTIONS


jsonEncodeListChecks : IntoFnCheck
jsonEncodeListChecks =
    intoFnChecksFirstThatConstructsError
        [ onConversionFnCallCanBeCombinedCheck
            { combinedOperationRepresents = "encode an array"
            , convertFn = Fn.Array.toList
            , actionRepresents = "convert to a list"
            , combinedFn = Fn.Json.Encode.array
            }
        , onConversionFnCallCanBeCombinedCheck
            { combinedOperationRepresents = "encode a set"
            , convertFn = Fn.Set.toList
            , actionRepresents = "convert to a list"
            , combinedFn = Fn.Json.Encode.set
            }
        , mapToOperationWithIdentityCanBeCombinedToOperationChecks listCollection
        ]


jsonEncodeArrayChecks : IntoFnCheck
jsonEncodeArrayChecks =
    intoFnChecksFirstThatConstructsError
        [ onConversionFnCallCanBeCombinedCheck
            { combinedOperationRepresents = "encode a list"
            , convertFn = Fn.Array.fromList
            , actionRepresents = "convert to an array"
            , combinedFn = Fn.Json.Encode.list
            }
        , mapToOperationWithIdentityCanBeCombinedToOperationChecks arrayCollection
        ]


jsonEncodeSetChecks : IntoFnCheck
jsonEncodeSetChecks =
    mapToOperationWithIdentityCanBeCombinedToOperationChecks setCollection



-- JSON.DECODE FUNCTIONS


jsonDecodeMapChecks : IntoFnCheck
jsonDecodeMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks jsonDecoderWithSucceedAsWrap
        , mapOnWrappedChecks jsonDecoderWithSucceedAsWrap
        ]


jsonDecodeMapNChecks : IntoFnCheck
jsonDecodeMapNChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            wrapperMapNChecks jsonDecoderWithSucceedAsWrap checkInfo
                |> onNothing (\() -> mapNOrFirstEmptyConstructionChecks jsonDecoderWithSucceedAsWrap checkInfo)
        )


jsonDecodeAndThenChecks : IntoFnCheck
jsonDecodeAndThenChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck jsonDecoderWithSucceedAsWrap
        , wrapperFlatMapChecks jsonDecoderWithSucceedAsWrap
        ]


jsonDecodeOneOfChecks : IntoFnCheck
jsonDecodeOneOfChecks =
    oneOfChecks



-- RANDOM FUNCTIONS


randomUniformChecks : IntoFnCheck
randomUniformChecks =
    intoFnCheckOnlyCall (oneOfConstantsWithOneAndRestListChecks randomGeneratorWrapper)


randomWeightedChecks : IntoFnCheck
randomWeightedChecks =
    intoFnCheckOnlyCall (oneOfWeightedConstantsWithOneAndRestChecks randomGeneratorWrapper)


randomListChecks : IntoFnCheck
randomListChecks =
    intoFnCheckOnlyCall (sequenceRepeatChecks randomGeneratorWrapper)


randomMapChecks : IntoFnCheck
randomMapChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (mapIdentityChecks randomGeneratorWrapper)
        , mapOnWrappedChecks randomGeneratorWrapper
        , nonEmptiableWrapperMapAlwaysChecks randomGeneratorWrapper
        ]


randomAndThenChecks : IntoFnCheck
randomAndThenChecks =
    intoFnChecksFirstThatConstructsError
        [ wrapperFlatMapChecks randomGeneratorWrapper
        , intoFnCheckOnlyCall (nonEmptiableWrapperFlatMapAlwaysChecks randomGeneratorWrapper)
        ]



-- PARSER FUNCTIONS


parserOneOfChecks : IntoFnCheck
parserOneOfChecks =
    oneOfChecks



-- PARSER.ADVANCED FUNCTIONS


parserAdvancedOneOfChecks : IntoFnCheck
parserAdvancedOneOfChecks =
    oneOfChecks



-- TEST FUNCTIONS


testConcatChecks : IntoFnCheck
testConcatChecks =
    -- does not use emptiableFlatFromListChecks because test has no simple Test.none
    intoFnChecksFirstThatConstructsError
        [ onWrappedReturnsItsValueCheck listCollection
        , intoFnCheckOnlyCall flatFromListsSpreadFlatFromListElementsCheck
        ]



-- TYPE PROPERTIES


type alias TypeProperties properties =
    { properties
        | represents : String
        , representsPlural : String
    }


{-| Properties of a type that either holds some data or is "empty" with the given properties.
-}
type alias EmptiableProperties emptySpecificProperties otherProperties =
    { otherProperties
        | empty : TypeSubsetProperties emptySpecificProperties
    }


{-| Properties of a structure type that will always have data inside, for example a non-empty list, a `Test`, a `Benchmark` or a tree (but not a forest).

This can be really valuable, for example when you want to know whether the function of a map or flatMap will always be called.

The way this type is defined,
it is impossible to have one type that has both `EmptiableProperties` and `NonEmptiableProperties`

-}
type alias NonEmptiableProperties otherProperties =
    { otherProperties | empty : { invalid : () } }


{-| Properties of a type that has a construction function that takes one value.

Example "wrap" construction functions: `Just`, `Err`, `List.singleton` and `[ a ]`
Note that for example `Cmd.batch [ a ]` is not a "wrap" because it keeps the type of the inner value `a`

-}
type alias WrapperProperties otherProperties =
    { otherProperties
        | wrap : ConstructWithOneValueProperties
    }


{-| Properties of a type that can be constructed from a list, like String with String.fromList.
See `ConstructionFromList`
-}
type alias ConstructibleFromListProperties otherProperties =
    { otherProperties
        | fromList : ConstructionFromList
    }


{-| How the type can be constructed from a list.

  - `ConstructionAsList`: the type is an alias to a list, like
      - `type alias Forest a = List (Tree a)`
      - `type alias TreePath = List Index`
      - `type alias Options = List Option`
  - `ConstructionFromListCall`: the type can be constructed using a function that takes one list as the only argument, like
      - String.fromList : List Char -> String\`

-}
type ConstructionFromList
    = ConstructionAsList
    | ConstructionFromListCall ( ModuleName, String )


{-| Properties of a type with with multiple elements.
-}
type alias CollectionProperties otherProperties =
    { otherProperties
        | elements :
            { countDescription : String
            , elementDescription : String
            , determineCount : Normalize.Resources {} -> Node Expression -> CollectionSize
            , get :
                Normalize.Resources {}
                -> Node Expression
                ->
                    Maybe
                        { known : List (Node Expression)
                        , -- whether every contained element known.
                          -- E.g. in x :: xs we just know the first, not all elements
                          allKnown : Bool
                        }
            }
    }


type alias WithElementCountFn otherProperties =
    { otherProperties
        | elementCount : { fn : ( ModuleName, String ), isConstantTime : Bool }
    }


{-| Properties of a type that under specific operations has a constant element that "annihilates"
all others, which means any application with one such element results in that element:

    a && False
    --> False

    False && a
    --> False

    List.all identity [ a, False, b ]
    --> False

    a || True
    --> True

    List.any identity [ a, True, b ]
    --> True

    Set.intersection Set.empty set
    --> Set.empty

    Set.intersection set Set.empty
    --> Set.empty

Even NaN falls into this category with +, \*, min, max:

    10 * Basics.max 10 (0 / 0) + 10
    --> 0 / 0 (NaN)

    10 - Basics.min 10 (0 / 0) * 10
    --> 0 / 0

And some properties only hold when `expectNaN` is not enabled, e.g.

    10 * 0
    --> 0

    List.product [ a, 0, b ]
    --> 0

    Basics.max a (1 / 0)
    --> 1 / 0 (Infinity)

    List.maximum [ a, 1 / 0, b ]
    --> Just (1 / 0)

More info: <https://en.wikipedia.org/wiki/Absorbing_element>

-}
type alias AbsorbableProperties otherProperties =
    { otherProperties
        | absorbing : ConstantProperties
    }


{-| Properties of a type that has a function that can change each current value given a function.
Examples are `Result.mapError`, `List.map` or `Random.map`.

Types with a `map` function typically have associated `WrapperProperties`.
For now, this has to be explicitly specified!

-}
type alias MappableProperties otherProperties =
    { otherProperties
        | mapFn : ( ModuleName, String )
    }


{-| Common properties of a specific set of values for a type.

Examples:

  - a task that is known to fail
  - a non-empty list with exactly one element
  - an empty string

The first 2 are examples of a subset with `ConstructWithOneValueProperties`,
the last one is an example of a subset with `ConstantProperties`

-}
type alias TypeSubsetProperties specificProperties =
    { specific : specificProperties
    , kind : specificProperties -> TypeSubsetKindProperties
    }


{-| Properties of a set of values.

  - Only one value is possible, like Cmd.none or [] → Constant
  - Multiple values are possible, like `Ok anyValue` or `[ onlyElementAnyValue ]`? → `A`/`An` depending on the indefinite article in front of the description

-}
type TypeSubsetKindProperties
    = ConstructWithOneValue ConstructWithOneValueProperties
    | Constant ConstantProperties


type alias ConstantProperties =
    { description : String
    , asString : QualifyResources {} -> String
    , is : Normalize.Resources {} -> Node Expression -> Bool
    }


type alias ConstructWithOneValueProperties =
    { description : ConstructWithOneValueDescription
    , fn : ( ModuleName, String )
    , getValue : ModuleNameLookupTable -> Node Expression -> Maybe (Node Expression)
    }


type ConstructWithOneValueDescription
    = A String
    | An String


isInTypeSubset : TypeSubsetProperties specificProperties -> Normalize.Resources res -> Node Expression -> Bool
isInTypeSubset typeSubsetProperties resources expressionNode =
    case typeSubsetProperties.kind typeSubsetProperties.specific of
        Constant constantProperties ->
            constantProperties.is (extractNormalizeResources resources) expressionNode

        ConstructWithOneValue constructWithOneValue ->
            isJust (constructWithOneValue.getValue resources.lookupTable expressionNode)


typeSubsetDescriptionIndefinite : TypeSubsetProperties specificProperties -> String
typeSubsetDescriptionIndefinite typeSubsetProperties =
    case typeSubsetProperties.kind typeSubsetProperties.specific of
        Constant constantProperties ->
            constantProperties.description

        ConstructWithOneValue constructWithOneValue ->
            constructWithOneValueDescriptionIndefinite constructWithOneValue.description


typeSubsetDescriptionDefinite : String -> TypeSubsetProperties specificProperties -> String
typeSubsetDescriptionDefinite definiteArticle typeSubsetProperties =
    case typeSubsetProperties.kind typeSubsetProperties.specific of
        Constant constantProperties ->
            constantProperties.description

        ConstructWithOneValue constructWithOneValue ->
            constructWithOneValueDescriptionDefinite definiteArticle constructWithOneValue.description


typeSubsetDescriptionWithoutArticle : TypeSubsetProperties specificProperties -> String
typeSubsetDescriptionWithoutArticle typeSubsetProperties =
    case typeSubsetProperties.kind typeSubsetProperties.specific of
        Constant constantProperties ->
            constantProperties.description

        ConstructWithOneValue constructWithOneValue ->
            constructWithOneValueDescriptionWithoutArticle constructWithOneValue.description


{-| Create `ConstantProperties` for a value with a given fully qualified name.
-}
constantFnProperties : ( ModuleName, String ) -> ConstantProperties
constantFnProperties fullyQualified =
    { description = qualifiedToString (qualify fullyQualified defaultQualifyResources)
    , is =
        \res expr ->
            AstHelpers.isSpecificValueReference res.lookupTable fullyQualified expr
    , asString =
        \res -> qualifiedToString (qualify fullyQualified res)
    }


{-| Create `ConstructWithOneValueProperties` for a function call with a given fully qualified name with a given `ConstructWithOneValueDescription`.
-}
fnCallConstructWithOneValueProperties :
    ConstructWithOneValueDescription
    -> ( ModuleName, String )
    -> ConstructWithOneValueProperties
fnCallConstructWithOneValueProperties description fullyQualified =
    { description = description
    , fn = fullyQualified
    , getValue =
        \lookupTable expr ->
            Maybe.map .firstArg
                (AstHelpers.getSpecificUnreducedFnCall fullyQualified lookupTable expr)
    }


getEmptyExpressionNode :
    Normalize.Resources a
    -> EmptiableProperties empty otherProperties
    -> Node Expression
    -> Maybe (Node Expression)
getEmptyExpressionNode resources emptiable expressionNode =
    if isInTypeSubset emptiable.empty resources expressionNode then
        Just expressionNode

    else
        Nothing


getAbsorbingExpressionNode : AbsorbableProperties otherProperties -> Normalize.Resources res -> Node Expression -> Maybe (Node Expression)
getAbsorbingExpressionNode absorbable inferResources expressionNode =
    if absorbable.absorbing.is (extractNormalizeResources inferResources) expressionNode then
        Just expressionNode

    else
        Nothing


getValueWithNodeRange :
    (Node Expression -> Maybe (Node Expression))
    -> Node Expression
    -> Maybe { value : Node Expression, nodeRange : Range }
getValueWithNodeRange getValue expressionNode =
    Maybe.map (\value -> { value = value, nodeRange = Node.range expressionNode })
        (getValue expressionNode)


fromListGetLiteral : ConstructibleFromListProperties otherProperties -> ModuleNameLookupTable -> Node Expression -> Maybe { constructionNodeRange : Range, literalRange : Range, elements : List (Node Expression) }
fromListGetLiteral constructibleFromList lookupTable expressionNode =
    case constructibleFromList.fromList of
        ConstructionAsList ->
            case AstHelpers.removeParens expressionNode of
                Node listLiteralRange (Expression.ListExpr listElements) ->
                    Just { constructionNodeRange = Node.range expressionNode, literalRange = listLiteralRange, elements = listElements }

                _ ->
                    Nothing

        ConstructionFromListCall fromListFn ->
            case AstHelpers.getSpecificUnreducedFnCall fromListFn lookupTable expressionNode of
                Just fromListCall ->
                    case AstHelpers.removeParens fromListCall.firstArg of
                        Node listLiteralRange (Expression.ListExpr listElements) ->
                            Just { constructionNodeRange = Node.range expressionNode, literalRange = listLiteralRange, elements = listElements }

                        _ ->
                            Nothing

                Nothing ->
                    Nothing


constructWithOneValueDescriptionIndefinite : ConstructWithOneValueDescription -> String
constructWithOneValueDescriptionIndefinite incomingArgDescription =
    case incomingArgDescription of
        A description ->
            "a " ++ description

        An description ->
            "an " ++ description


constructWithOneValueDescriptionDefinite : String -> ConstructWithOneValueDescription -> String
constructWithOneValueDescriptionDefinite startWithDefiniteArticle referenceArgDescription =
    case referenceArgDescription of
        A description ->
            startWithDefiniteArticle ++ " " ++ description

        An description ->
            startWithDefiniteArticle ++ " " ++ description


constructWithOneValueDescriptionWithoutArticle : ConstructWithOneValueDescription -> String
constructWithOneValueDescriptionWithoutArticle referenceArgDescription =
    case referenceArgDescription of
        A description ->
            description

        An description ->
            description


extractQualifyResources : QualifyResources a -> QualifyResources {}
extractQualifyResources resources =
    { importLookup = resources.importLookup
    , moduleBindings = resources.moduleBindings
    , localBindings = resources.localBindings
    }


extractNormalizeResources :
    Normalize.Resources a
    -> Normalize.Resources {}
extractNormalizeResources resources =
    { lookupTable = resources.lookupTable
    , inferredConstants = resources.inferredConstants
    , moduleCustomTypes = resources.moduleCustomTypes
    , importCustomTypes = resources.importCustomTypes
    }


extractReduceLambdaResources :
    AstHelpers.ReduceLambdaResources a
    -> AstHelpers.ReduceLambdaResources {}
extractReduceLambdaResources resources =
    { lookupTable = resources.lookupTable
    , importCustomTypes = resources.importCustomTypes
    , moduleCustomTypes = resources.moduleCustomTypes
    }


emptyAsString : QualifyResources a -> EmptiableProperties ConstantProperties otherProperties -> String
emptyAsString qualifyResources emptiable =
    emptiable.empty.specific.asString (extractQualifyResources qualifyResources)


boolForAndProperties : TypeProperties (EmptiableProperties ConstantProperties (AbsorbableProperties {}))
boolForAndProperties =
    { represents = "bool"
    , representsPlural = "bools"
    , empty = { specific = boolTrueConstant, kind = Constant }
    , absorbing = boolFalseConstant
    }


boolForOrProperties : TypeProperties (EmptiableProperties ConstantProperties (AbsorbableProperties {}))
boolForOrProperties =
    { represents = "bool"
    , representsPlural = "bools"
    , empty = { specific = boolFalseConstant, kind = Constant }
    , absorbing = boolTrueConstant
    }


boolTrueConstant : ConstantProperties
boolTrueConstant =
    { description = qualifiedToString (qualify Fn.Basics.trueVariant defaultQualifyResources)
    , is = \res expr -> Normalize.getBool res expr == justTrue
    , asString = \res -> qualifiedToString (qualify Fn.Basics.trueVariant res)
    }


justTrue : Maybe Bool
justTrue =
    Just True


boolFalseConstant : ConstantProperties
boolFalseConstant =
    { description = qualifiedToString (qualify Fn.Basics.falseVariant defaultQualifyResources)
    , is = \res expr -> Normalize.getBool res expr == justFalse
    , asString = \res -> qualifiedToString (qualify Fn.Basics.falseVariant res)
    }


justFalse : Maybe Bool
justFalse =
    Just False


numberForAddProperties : TypeProperties (EmptiableProperties ConstantProperties (AbsorbableProperties {}))
numberForAddProperties =
    { represents = "number"
    , representsPlural = "numbers"
    , empty = { specific = number0ConstantSpecific, kind = Constant }
    , absorbing = numberNaNConstantSpecific
    }


{-| Be aware that in elm, 0 is not absorbing for `(*)` (see `AbsorbableProperties`) because NaN can "overwrite" it.

    0 * (0 / 0)
    --> 0 / 0 (NaN)

In fact, NaN is an absorbing element for `(*)`.
If `expectingNaN` is not enabled, use `numberNotExpectingNaNForMultiplyProperties`.

-}
numberForMultiplyProperties : TypeProperties (EmptiableProperties ConstantProperties (AbsorbableProperties {}))
numberForMultiplyProperties =
    { represents = "number"
    , representsPlural = "numbers"
    , empty = { specific = number1ConstantSpecific, kind = Constant }
    , absorbing = numberNaNConstantSpecific
    }


{-| If `expectingNaN` is enabled, 0 is not absorbing for `(*)` (see `AbsorbableProperties`) because NaN can "overwrite" it.

    0 * (0 / 0)
    --> 0 / 0 (NaN)

If that's the case, use `numberForMultiplyProperties`.

Not having `expectingNaN` enabled however, 0 _is_ absorbing, so we can now simplify e.g.

    List.product [ a, 0, b ]
    --> 0

(see `callOnListWithAbsorbingElement`)

-}
numberNotExpectingNaNForMultiplyProperties : TypeProperties (EmptiableProperties ConstantProperties (AbsorbableProperties {}))
numberNotExpectingNaNForMultiplyProperties =
    { represents = "number"
    , representsPlural = "numbers"
    , empty = { specific = number1ConstantSpecific, kind = Constant }
    , absorbing = number0ConstantSpecific
    }


number0ConstantSpecific : ConstantProperties
number0ConstantSpecific =
    { description = "0"
    , is = \res expr -> Normalize.getNumber res expr == Just 0
    , asString = \_ -> "0"
    }


number1ConstantSpecific : ConstantProperties
number1ConstantSpecific =
    { description = "1"
    , is = \res expr -> Normalize.getNumber res expr == Just 1
    , asString = \_ -> "1"
    }


numberNaNConstantSpecific : ConstantProperties
numberNaNConstantSpecific =
    { description = "NaN"
    , is =
        \res expr ->
            case AstHelpers.removeParens expr of
                Node _ (Expression.OperatorApplication "/" _ dividend divisor) ->
                    (Normalize.getNumber res dividend == Just 0)
                        && (Normalize.getNumber res divisor == Just 0)

                _ ->
                    False
    , asString = \_ -> "(0 / 0)"
    }


randomGeneratorWrapper : TypeProperties (NonEmptiableProperties (WrapperProperties (MappableProperties {})))
randomGeneratorWrapper =
    { represents = "random generator"
    , representsPlural = "random generators"
    , wrap = randomGeneratorConstantConstruct
    , empty = { invalid = () }
    , mapFn = Fn.Random.map
    }


randomGeneratorConstantConstruct : ConstructWithOneValueProperties
randomGeneratorConstantConstruct =
    fnCallConstructWithOneValueProperties (A "constant generator") Fn.Random.constant


maybeWithJustAsWrap : TypeProperties (EmptiableProperties ConstantProperties (WrapperProperties (MappableProperties {})))
maybeWithJustAsWrap =
    { represents = "maybe"
    , representsPlural = "maybes"
    , empty = { specific = constantFnProperties Fn.Maybe.nothingVariant, kind = Constant }
    , wrap = maybeJustConstruct
    , mapFn = Fn.Maybe.map
    }


maybeJustConstruct : ConstructWithOneValueProperties
maybeJustConstruct =
    fnCallConstructWithOneValueProperties (A "just maybe") Fn.Maybe.justVariant


resultWithOkAsWrap : TypeProperties (WrapperProperties (EmptiableProperties ConstructWithOneValueProperties (MappableProperties {})))
resultWithOkAsWrap =
    { represents = "result"
    , representsPlural = "results"
    , wrap = resultOkayConstruct
    , empty = { specific = resultErrorConstruct, kind = ConstructWithOneValue }
    , mapFn = Fn.Result.map
    }


resultOkayConstruct : ConstructWithOneValueProperties
resultOkayConstruct =
    fnCallConstructWithOneValueProperties (An "okay result") Fn.Result.okVariant


resultErrorConstruct : ConstructWithOneValueProperties
resultErrorConstruct =
    fnCallConstructWithOneValueProperties (An "error") Fn.Result.errVariant


resultWithErrAsWrap : TypeProperties (WrapperProperties (EmptiableProperties ConstructWithOneValueProperties (MappableProperties {})))
resultWithErrAsWrap =
    { represents = "result"
    , representsPlural = "results"
    , wrap = resultErrorConstruct
    , empty = { specific = resultOkayConstruct, kind = ConstructWithOneValue }
    , mapFn = Fn.Result.mapError
    }


taskWithSucceedAsWrap : TypeProperties (WrapperProperties (EmptiableProperties ConstructWithOneValueProperties (MappableProperties {})))
taskWithSucceedAsWrap =
    { represents = "task"
    , representsPlural = "tasks"
    , wrap = taskSucceedingConstruct
    , empty = { specific = taskFailingConstruct, kind = ConstructWithOneValue }
    , mapFn = Fn.Task.map
    }


taskSucceedingConstruct : ConstructWithOneValueProperties
taskSucceedingConstruct =
    fnCallConstructWithOneValueProperties (A "succeeding task") Fn.Task.succeed


taskFailingConstruct : ConstructWithOneValueProperties
taskFailingConstruct =
    fnCallConstructWithOneValueProperties (A "failing task") Fn.Task.fail


taskWithFailAsWrap : TypeProperties (WrapperProperties (EmptiableProperties ConstructWithOneValueProperties (MappableProperties {})))
taskWithFailAsWrap =
    { represents = "task"
    , representsPlural = "tasks"
    , wrap = taskFailingConstruct
    , empty = { specific = taskSucceedingConstruct, kind = ConstructWithOneValue }
    , mapFn = Fn.Task.mapError
    }


jsonDecoderWithSucceedAsWrap : TypeProperties (WrapperProperties (EmptiableProperties ConstructWithOneValueProperties (MappableProperties {})))
jsonDecoderWithSucceedAsWrap =
    { represents = "json decoder"
    , representsPlural = "json decoders"
    , wrap = jsonDecoderSucceedingConstruct
    , empty = { specific = jsonDecoderFailingConstruct, kind = ConstructWithOneValue }
    , mapFn = Fn.Json.Decode.map
    }


jsonDecoderSucceedingConstruct : ConstructWithOneValueProperties
jsonDecoderSucceedingConstruct =
    fnCallConstructWithOneValueProperties (A "succeeding decoder") Fn.Json.Decode.succeed


jsonDecoderFailingConstruct : ConstructWithOneValueProperties
jsonDecoderFailingConstruct =
    fnCallConstructWithOneValueProperties (A "failing decoder") Fn.Json.Decode.fail


listCollection : TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties (WrapperProperties (ConstructibleFromListProperties (MappableProperties (WithElementCountFn { isEmptyFn : ( ModuleName, String ) }))))))
listCollection =
    { represents = "list"
    , representsPlural = "lists"
    , empty = { specific = listEmptyConstantSpecific, kind = Constant }
    , elements =
        { get = listGetElements
        , elementDescription = "element"
        , countDescription = "length"
        , determineCount = determineCollectionSize
        }
    , wrap = listSingletonConstruct
    , mapFn = Fn.List.map
    , isEmptyFn = Fn.List.isEmpty
    , elementCount = { fn = Fn.List.length, isConstantTime = False }
    , fromList = ConstructionAsList
    }


listEmptyConstantSpecific : ConstantProperties
listEmptyConstantSpecific =
    { description = "[]"
    , is = \_ expressionNode -> isEmptyList expressionNode
    , asString = \_ -> "[]"
    }


isEmptyList : Node Expression -> Bool
isEmptyList (Node _ expression) =
    case expression of
        Expression.ParenthesizedExpression inParens ->
            isEmptyList inParens

        Expression.ListExpr [] ->
            True

        _ ->
            False


listSingletonConstruct : ConstructWithOneValueProperties
listSingletonConstruct =
    { description = A "singleton list"
    , fn = Fn.List.singleton
    , getValue = AstHelpers.getListSingleton
    }


listGetElements :
    Infer.Resources a
    -> Node Expression
    -> Maybe { known : List (Node Expression), allKnown : Bool }
listGetElements resources expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.ListExpr elements ->
            Just { known = elements, allKnown = True }

        Expression.OperatorApplication "::" _ head tail ->
            case listGetElements resources tail of
                Just tailElements ->
                    Just { known = head :: tailElements.known, allKnown = tailElements.allKnown }

                Nothing ->
                    Just { known = [ head ], allKnown = False }

        Expression.OperatorApplication "++" _ leftList rightList ->
            case listGetElements resources leftList of
                Nothing ->
                    Nothing

                Just leftElements ->
                    case listGetElements resources rightList of
                        Just rightElements ->
                            Just
                                { allKnown = leftElements.allKnown && rightElements.allKnown
                                , known = leftElements.known ++ rightElements.known
                                }

                        Nothing ->
                            Just { known = leftElements.known, allKnown = False }

        _ ->
            expressionNode
                |> AstHelpers.getSpecificUnreducedFnCall Fn.List.singleton resources.lookupTable
                |> Maybe.map (\singletonCall -> { known = [ singletonCall.firstArg ], allKnown = True })


normalListGetElements : Expression -> Maybe { known : List (Node Expression), allKnown : Bool }
normalListGetElements expression =
    case expression of
        Expression.ListExpr elements ->
            Just { known = elements, allKnown = True }

        Expression.OperatorApplication "::" _ head (Node _ tail) ->
            case normalListGetElements tail of
                Just tailElements ->
                    Just { known = head :: tailElements.known, allKnown = tailElements.allKnown }

                Nothing ->
                    Just { known = [ head ], allKnown = False }

        Expression.OperatorApplication "++" _ (Node _ leftList) (Node _ rightList) ->
            case normalListGetElements leftList of
                Nothing ->
                    Nothing

                Just leftElements ->
                    case normalListGetElements rightList of
                        Just rightElements ->
                            Just
                                { allKnown = leftElements.allKnown && rightElements.allKnown
                                , known = leftElements.known ++ rightElements.known
                                }

                        Nothing ->
                            Just { known = leftElements.known, allKnown = False }

        _ ->
            Nothing


stringCollection : TypeProperties (CollectionProperties (WrapperProperties (EmptiableProperties ConstantProperties (ConstructibleFromListProperties (WithElementCountFn { isEmptyFn : ( ModuleName, String ) })))))
stringCollection =
    { represents = "string"
    , representsPlural = "strings"
    , empty = { specific = stringEmptyConstantSpecific, kind = Constant }
    , elements =
        { countDescription = "length"
        , elementDescription = "character"
        , determineCount = determineCollectionSize
        , get = stringGetElements
        }
    , wrap = singleCharConstruct
    , fromList = ConstructionFromListCall Fn.String.fromList
    , isEmptyFn = Fn.String.isEmpty
    , elementCount = { fn = Fn.String.length, isConstantTime = True }
    }


stringEmptyConstantSpecific : ConstantProperties
stringEmptyConstantSpecific =
    { description = emptyStringAsString
    , asString = \_ -> emptyStringAsString
    , is = \_ expressionNode -> isEmptyString expressionNode
    }


isEmptyString : Node Expression -> Bool
isEmptyString (Node _ expression) =
    case expression of
        Expression.ParenthesizedExpression inParens ->
            isEmptyString inParens

        Expression.Literal "" ->
            True

        _ ->
            False


singleCharConstruct : ConstructWithOneValueProperties
singleCharConstruct =
    fnCallConstructWithOneValueProperties (A "single-char string") Fn.String.fromChar


stringGetElements : Infer.Resources res -> Node Expression -> Maybe { known : List (Node Expression), allKnown : Bool }
stringGetElements resources expressionNode =
    expressionNode
        |> AstHelpers.getSpecificUnreducedFnCall Fn.String.fromChar resources.lookupTable
        |> Maybe.map (\fromCharCall -> { known = [ fromCharCall.firstArg ], allKnown = True })
        |> onNothing
            (\() ->
                expressionNode
                    |> AstHelpers.getSpecificUnreducedFnCall Fn.String.fromList resources.lookupTable
                    |> Maybe.andThen (\fromListCall -> listGetElements resources fromListCall.firstArg)
            )


arrayCollection : TypeProperties (CollectionProperties (ConstructibleFromListProperties (EmptiableProperties ConstantProperties (MappableProperties (WithElementCountFn { isEmptyFn : ( ModuleName, String ) })))))
arrayCollection =
    { represents = "array"
    , representsPlural = "arrays"
    , empty = { specific = constantFnProperties Fn.Array.empty, kind = Constant }
    , elements =
        { countDescription = "length"
        , elementDescription = "element"
        , determineCount = determineCollectionSize
        , get = arrayGetElements
        }
    , fromList = ConstructionFromListCall Fn.Array.fromList
    , mapFn = Fn.Array.map
    , isEmptyFn = Fn.Array.isEmpty
    , elementCount = { fn = Fn.Array.length, isConstantTime = True }
    }


arrayGetElements : Infer.Resources a -> Node Expression -> Maybe { known : List (Node Expression), allKnown : Bool }
arrayGetElements resources expressionNode =
    if AstHelpers.isSpecificValueReference resources.lookupTable Fn.Array.empty expressionNode then
        Just { known = [], allKnown = True }

    else
        case AstHelpers.getSpecificUnreducedFnCall Fn.Array.fromList resources.lookupTable expressionNode of
            Just fromListCall ->
                listGetElements resources fromListCall.firstArg

            Nothing ->
                Nothing


setCollection : TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties (WrapperProperties (ConstructibleFromListProperties (MappableProperties (WithElementCountFn { isEmptyFn : ( ModuleName, String ) }))))))
setCollection =
    { represents = "set"
    , representsPlural = "sets"
    , empty = { specific = constantFnProperties Fn.Set.empty, kind = Constant }
    , elements =
        { countDescription = "size"
        , elementDescription = "element"
        , determineCount = determineCollectionSize
        , get = setGetElements
        }
    , wrap = setSingletonConstruct
    , fromList = ConstructionFromListCall Fn.Set.fromList
    , mapFn = Fn.Set.map
    , isEmptyFn = Fn.Set.isEmpty
    , elementCount = { fn = Fn.Set.size, isConstantTime = False }
    }


setSingletonConstruct : ConstructWithOneValueProperties
setSingletonConstruct =
    fnCallConstructWithOneValueProperties (A "singleton set") Fn.Set.singleton


setGetElements : Normalize.Resources a -> Node Expression -> Maybe { known : List (Node Expression), allKnown : Bool }
setGetElements resources expressionNode =
    (if AstHelpers.isSpecificValueReference resources.lookupTable Fn.Set.empty expressionNode then
        Just { known = [], allKnown = True }

     else
        Nothing
    )
        |> onNothing
            (\() ->
                case AstHelpers.getSpecificUnreducedFnCall Fn.Set.singleton resources.lookupTable expressionNode of
                    Just singletonCall ->
                        Just { known = [ singletonCall.firstArg ], allKnown = True }

                    Nothing ->
                        Nothing
            )
        |> onNothing
            (\() ->
                case AstHelpers.getSpecificUnreducedFnCall Fn.Set.fromList resources.lookupTable expressionNode of
                    Just fromListCall ->
                        case listGetElements resources fromListCall.firstArg of
                            Just listElements ->
                                case traverse (\element -> getComparableWithExpressionNode resources element) listElements.known of
                                    Just comparableElements ->
                                        Just
                                            { known = uniqueByThenMap .comparable .expressionNode comparableElements
                                            , allKnown = listElements.allKnown
                                            }

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            )


getComparableWithExpressionNode :
    Normalize.Resources a
    -> Node Expression
    -> Maybe { comparable : ComparableExpression, expressionNode : Node Expression }
getComparableWithExpressionNode resources expressionNode =
    expressionToComparable resources expressionNode
        |> Maybe.map (\comparable -> { comparable = comparable, expressionNode = expressionNode })


dictCollection : TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties (ConstructibleFromListProperties (WithElementCountFn { isEmptyFn : ( ModuleName, String ) }))))
dictCollection =
    { represents = "dict"
    , representsPlural = "dicts"
    , empty = { specific = constantFnProperties Fn.Dict.empty, kind = Constant }
    , elements =
        { countDescription = "size"
        , elementDescription = "value"
        , determineCount = determineCollectionSize
        , get = dictGetValues
        }
    , fromList = ConstructionFromListCall Fn.Dict.fromList
    , isEmptyFn = Fn.Dict.isEmpty
    , elementCount = { fn = Fn.Dict.size, isConstantTime = False }
    }


dictGetValues : Normalize.Resources res -> Node Expression -> Maybe { known : List (Node Expression), allKnown : Bool }
dictGetValues resources expressionNode =
    (if AstHelpers.isSpecificValueReference resources.lookupTable Fn.Dict.empty expressionNode then
        Just { known = [], allKnown = True }

     else
        Nothing
    )
        |> onNothing
            (\() ->
                expressionNode
                    |> AstHelpers.getSpecificUnreducedFnCall Fn.Dict.singleton resources.lookupTable
                    |> Maybe.andThen
                        (\singletonCall ->
                            case singletonCall.argsAfterFirst of
                                singletonValue :: [] ->
                                    Just { known = [ singletonValue ], allKnown = True }

                                _ ->
                                    Nothing
                        )
            )
        |> onNothing
            (\() ->
                expressionNode
                    |> AstHelpers.getSpecificUnreducedFnCall Fn.Dict.fromList resources.lookupTable
                    |> Maybe.andThen
                        (\fromListCall ->
                            case listGetElements resources fromListCall.firstArg of
                                Just listElements ->
                                    if listElements.allKnown then
                                        case traverse (getTupleWithComparableFirst resources) listElements.known of
                                            Just tuplesWithComparableKey ->
                                                Just
                                                    { known = uniqueByThenMap .comparableFirst .second tuplesWithComparableKey
                                                    , allKnown = True
                                                    }

                                            Nothing ->
                                                Nothing

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing
                        )
            )


getTupleWithComparableFirst :
    Normalize.Resources a
    -> Node Expression
    ->
        Maybe
            { comparableFirst : ComparableExpression
            , first : Node Expression
            , second : Node Expression
            }
getTupleWithComparableFirst resources expressionNode =
    case AstHelpers.getTuple2 resources.lookupTable expressionNode of
        Just tuple ->
            case expressionToComparable resources tuple.first of
                Just comparableFirst ->
                    Just
                        { comparableFirst = comparableFirst
                        , first = tuple.first
                        , second = tuple.second
                        }

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| Returns the `ComparableExpression` for the first part
-}
normalGetTupleComparableFirst : Expression -> Maybe ComparableExpression
normalGetTupleComparableFirst expression =
    case expression of
        Expression.TupledExpression [ first, _ ] ->
            expressionNormalToComparable (Node.value first)

        _ ->
            Nothing


dictGetKeys : Normalize.Resources res -> Node Expression -> Maybe { known : List (Node Expression), allKnown : Bool }
dictGetKeys resources expressionNode =
    (if AstHelpers.isSpecificValueReference resources.lookupTable Fn.Dict.empty expressionNode then
        Just { known = [], allKnown = True }

     else
        Nothing
    )
        |> onNothing
            (\() ->
                expressionNode
                    |> AstHelpers.getSpecificUnreducedFnCall Fn.Dict.singleton resources.lookupTable
                    |> Maybe.map
                        (\singletonCall ->
                            { known = [ singletonCall.firstArg ], allKnown = True }
                        )
            )
        |> onNothing
            (\() ->
                expressionNode
                    |> AstHelpers.getSpecificUnreducedFnCall Fn.Dict.fromList resources.lookupTable
                    |> Maybe.andThen
                        (\fromListCall ->
                            case listGetElements resources fromListCall.firstArg of
                                Just listElements ->
                                    if listElements.allKnown then
                                        case traverse (\element -> getTupleWithComparableFirst resources element) listElements.known of
                                            Just tuplesWithComparableKey ->
                                                Just
                                                    { known =
                                                        uniqueByThenMap .comparableFirst .first tuplesWithComparableKey
                                                    , allKnown = True
                                                    }

                                            Nothing ->
                                                Nothing

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing
                        )
            )


cmdCollection : TypeProperties (EmptiableProperties ConstantProperties {})
cmdCollection =
    { represents = "command"
    , representsPlural = "commands"
    , empty = { specific = constantFnProperties Fn.Platform.Cmd.none, kind = Constant }
    }


subCollection : TypeProperties (EmptiableProperties ConstantProperties {})
subCollection =
    { represents = "subscription"
    , representsPlural = "subscriptions"
    , empty = { specific = constantFnProperties Fn.Platform.Sub.none, kind = Constant }
    }



-- CHECKS FOR GENERIC TYPES
{-
   Abstract names used for operations in the generic checks below

   #### "flat"

   Turns a wrapper of a type into _that exact type_.

   Examples:

       String.concat : List String -> String
       Test.concat : List Test -> Test
       Range.combine : List Range -> Range
       Bytes.Encode.sequence : List Encoder -> Encoder
       Result.Extra.merge : Result a a -> a
       Platform.Cmd.batch : List (Cmd a) -> Cmd a
       List.concat : List (List a) -> List a
       Maybe.Extra.join : Maybe (Maybe a) -> Maybe a
       List.NonEmpty.concat : Nonempty (Nonempty a) -> Nonempty a
       Parser.Sequence.concat : List (Parser (List a)) -> Parser (List a) -- by lambda-phi

   Note the difference to "sequence":

   #### "sequence"

   Turns a wrapper of a type with an argument `a` into _that type with argument `wrapper a`_.

   Examples:

       Random.Extra.sequence : List (Generator a) -> Generator (List a)
       Json.Extra.sequence : List (Decoder a) -> Decoder (List a)
       Task.sequence : List (Task x a) -> Task x (List a)
       Maybe.Extra.combineArray : Array (Maybe a) -> Maybe (Array a)
       -- ↓ don't exist
       Result.Maybe.toMaybe : Result x (Maybe a) -> Maybe (Result x e)
       Maybe.Function.applyIfJust : Maybe (a -> b) -> (a -> Maybe b)
       Result.Function.applyIfOk : Result x (a -> b) -> (a -> Result x b)
       List.Function.feedEach : List (a -> b) -> (a -> List b)

   #### someName ++ otherName

   You'll find names that are a combination of two operation names like "sequenceRepeat" or "sequenceMap".
   How to interpret them?

   "sequenceMap ..args" for example is equivalent to `sequence (map ..args)` which means:
   First map the type, then apply a sequence operation to the result.

   This intuition works for all these.
   So "sequenceRepeat ..args" is equivalent to `sequence (repeat ..args)` which means:
   First create the collection using repeat, then apply a sequence operation to the result.

   #### handle ++ "when"

   For each value, only "handle" it when the given function results in True.

   Examples:

       Array.Extra.removeWhen : (a -> Bool) -> Array a -> Array a
       List.Extra.setIf : (a -> Bool) -> a -> List a -> List a
       List.Extra.updateIf : (a -> Bool) -> (a -> a) -> List a -> List a
       Set.filter : (comparable -> Bool) -> Set comparable -> Set comparable
       Maybe.Extra.filter : (a -> Bool) -> Maybe a -> Maybe a
-}


oneOfChecks : IntoFnCheck
oneOfChecks =
    onWrappedReturnsItsValueCheck listCollection


{-| Checks for a "oneOfConstants" operation:

    oneOfConstants firstPossibility [] --> wrap firstPossibility

so for example

    Random.uniform firstPossibility []
    --> Random.constant firstPossibility

Note that this is different (mostly in terms of error info) from both e.g.

    List.NonEmpty.fromCons firstState [] --> List.NonEmpty.singleton firstState

    Set.insert first Set.empty --> Set.singleton first

(For the second, you can use `collectionInsertChecks`.)

Note also that "oneOfConstants" is different from "oneOf":

    Codec.oneOf : Codec a -> List (Codec a) -> Codec a
    -- vs
    Random.uniform : a -> List a -> Random.Generator a

where the simplification would be

    Code.oneOf codec [] --> codec

-}
oneOfConstantsWithOneAndRestListChecks : WrapperProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
oneOfConstantsWithOneAndRestListChecks wrapper checkInfo =
    case secondArg checkInfo of
        Just otherOptionsArg ->
            if listCollection.empty.specific.is (extractNormalizeResources checkInfo) otherOptionsArg then
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " with one possible value will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with that value"
                        , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ " with the first given value." ]
                        }
                        checkInfo.fnRange
                        (Fix.replaceRangeBy checkInfo.fnRange
                            (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                            :: keepOnlyFix
                                { parentRange = checkInfo.parentRange
                                , keep = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                }
                        )
                    )

            else
                Nothing

        Nothing ->
            Nothing


{-| The "oneOfWeightedConstants" checks

    oneOfWeightedConstants ( w, a ) [] --> wrap a

    oneOfWeightedConstants weighted [] --> wrap (Tuple.first weighted)

Examples of such functions:

    Random.weighted : ( Float, a ) -> List ( Float, a ) -> Generator a
    -- ↓ doesn't exist
    SucceedingFuzzer.frequencyValues : ( Float, a ) -> List ( Float, a ) -> SucceedingFuzzer a

-}
oneOfWeightedConstantsWithOneAndRestChecks : WrapperProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
oneOfWeightedConstantsWithOneAndRestChecks wrapper checkInfo =
    case secondArg checkInfo of
        Just otherOptionsArg ->
            if listCollection.empty.specific.is (extractNormalizeResources checkInfo) otherOptionsArg then
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " with one possible value will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with that value"
                        , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ " with the first given value." ]
                        }
                        checkInfo.fnRange
                        (case AstHelpers.getTuple2 checkInfo.lookupTable checkInfo.firstArg of
                            Just tuple ->
                                Fix.replaceRangeBy checkInfo.fnRange
                                    (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                    :: keepOnlyFix
                                        { parentRange = checkInfo.parentRange
                                        , keep = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                        }
                                    ++ replaceBySubExpressionFix (Node.range checkInfo.firstArg) tuple.second

                            Nothing ->
                                let
                                    tupleArgRange : Range
                                    tupleArgRange =
                                        Node.range checkInfo.firstArg
                                in
                                Fix.replaceRangeBy checkInfo.fnRange
                                    (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                    :: Fix.insertAt tupleArgRange.start
                                        ("(" ++ qualifiedToString Fn.Tuple.first ++ " ")
                                    :: Fix.insertAt tupleArgRange.end ")"
                                    :: keepOnlyFix
                                        { parentRange = checkInfo.parentRange
                                        , keep = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                        }
                        )
                    )

            else
                Nothing

        Nothing ->
            Nothing


{-| The check

    f (repeat n a)
    --> if n >= 1 then wrapOnPositive a else onNotPositive

    f << repeat n
    --> if n >= 1 then wrapOnPositive else always onNotPositive

-}
onRepeatReturnsWrapElementIfCountIsPositiveOtherwiseAlwaysFn :
    { repeatFn : ( ModuleName, String )
    , wrapFnOnRepeatCountPositive : ( ModuleName, String )
    , fnOnRepeatCountNotPositive : ( ModuleName, String )
    }
    -> IntoFnCheck
onRepeatReturnsWrapElementIfCountIsPositiveOtherwiseAlwaysFn config =
    { composition =
        \checkInfo ->
            if (checkInfo.earlier.fn == config.repeatFn) && onlyLastArgIsCurried checkInfo.later then
                case checkInfo.earlier.args of
                    [ repeatCountArg ] ->
                        let
                            ifIndentation : String
                            ifIndentation =
                                String.repeat (checkInfo.later.range.start.column - 1) " "
                        in
                        Just
                            { info =
                                { message =
                                    qualifiedToString checkInfo.later.fn
                                        ++ " on "
                                        ++ qualifiedToString config.repeatFn
                                        ++ " will result in "
                                        ++ qualifiedToString (qualify config.wrapFnOnRepeatCountPositive defaultQualifyResources)
                                        ++ " the repeated element if the count is positive and "
                                        ++ qualifiedToString (qualify config.fnOnRepeatCountNotPositive defaultQualifyResources)
                                        ++ " otherwise"
                                , details =
                                    [ "You can replace this composition by if (the count argument given to "
                                        ++ qualifiedToString config.repeatFn
                                        ++ ") >= 1 then "
                                        ++ qualifiedToString (qualify config.wrapFnOnRepeatCountPositive defaultQualifyResources)
                                        ++ " else always "
                                        ++ qualifiedToString (qualify config.fnOnRepeatCountNotPositive defaultQualifyResources)
                                        ++ "."
                                    ]
                                }
                            , fix =
                                [ Fix.removeRange checkInfo.earlier.removeRange
                                , Fix.replaceRangeBy checkInfo.later.range
                                    ("(if "
                                        ++ parenthesizeIf
                                            (needsParens (Node.value repeatCountArg))
                                            (checkInfo.extractSourceCode
                                                (Node.range repeatCountArg)
                                            )
                                        ++ " >= 1 then\n"
                                        ++ ifIndentation
                                        ++ "    "
                                        ++ qualifiedToString (qualify config.wrapFnOnRepeatCountPositive checkInfo)
                                        ++ "\n\n"
                                        ++ ifIndentation
                                        ++ "else\n"
                                        ++ ifIndentation
                                        ++ "    "
                                        ++ qualifiedToString (qualify Fn.Basics.always checkInfo)
                                        ++ " "
                                        ++ qualifiedToString (qualify config.fnOnRepeatCountNotPositive checkInfo)
                                        ++ ")"
                                    )
                                ]
                            }

                    _ ->
                        Nothing

            else
                Nothing
    , call =
        \checkInfo ->
            case fullyAppliedLastArg checkInfo of
                Nothing ->
                    Nothing

                Just lastArg ->
                    case AstHelpers.getSpecificUnreducedFnCall config.repeatFn checkInfo.lookupTable lastArg of
                        Nothing ->
                            Nothing

                        Just repeatFnCall ->
                            case repeatFnCall.argsAfterFirst of
                                [ elementToRepeatArg ] ->
                                    let
                                        branchIndentation : String
                                        branchIndentation =
                                            String.repeat ((Node.range elementToRepeatArg).start.column - 1) " "
                                    in
                                    Just
                                        (Rule.errorWithFix
                                            { message =
                                                qualifiedToString checkInfo.fn
                                                    ++ " on "
                                                    ++ qualifiedToString config.repeatFn
                                                    ++ " will result in "
                                                    ++ qualifiedToString (qualify config.wrapFnOnRepeatCountPositive defaultQualifyResources)
                                                    ++ " the repeated element if the count is positive and "
                                                    ++ qualifiedToString (qualify config.fnOnRepeatCountNotPositive defaultQualifyResources)
                                                    ++ " otherwise"
                                            , details =
                                                [ "You can replace this call by if (the count argument given to "
                                                    ++ qualifiedToString config.repeatFn
                                                    ++ ") >= 1 then "
                                                    ++ qualifiedToString (qualify config.wrapFnOnRepeatCountPositive defaultQualifyResources)
                                                    ++ " (the element to repeat argument given to "
                                                    ++ qualifiedToString config.repeatFn
                                                    ++ ") else "
                                                    ++ qualifiedToString (qualify config.fnOnRepeatCountNotPositive defaultQualifyResources)
                                                    ++ "."
                                                ]
                                            }
                                            checkInfo.fnRange
                                            (Fix.insertAt checkInfo.parentRange.start
                                                ("(if "
                                                    ++ parenthesizeIf
                                                        (needsParens (Node.value repeatFnCall.firstArg))
                                                        (checkInfo.extractSourceCode
                                                            (Node.range repeatFnCall.firstArg)
                                                        )
                                                    ++ " >= 1 then\n"
                                                    ++ branchIndentation
                                                    ++ qualifiedToString (qualify config.wrapFnOnRepeatCountPositive checkInfo)
                                                    ++ " "
                                                )
                                                :: Fix.insertAt checkInfo.parentRange.end
                                                    ("\n\n"
                                                        ++ String.repeat (checkInfo.parentRange.start.column - 1) " "
                                                        ++ "else\n"
                                                        ++ branchIndentation
                                                        ++ qualifiedToString (qualify config.fnOnRepeatCountNotPositive checkInfo)
                                                        ++ ")"
                                                    )
                                                :: replaceBySubExpressionFix checkInfo.parentRange
                                                    elementToRepeatArg
                                            )
                                        )

                                _ ->
                                    Nothing
    }


{-| The check

    map f (repeat n a)
    --> repeat n (f a)

    map f << repeat n
    --> repeat n << f

-}
mapOnRepeatAppliesTheFunctionToTheRepeatedElementCheck : ( ModuleName, String ) -> IntoFnCheck
mapOnRepeatAppliesTheFunctionToTheRepeatedElementCheck repeatFn =
    { composition =
        \checkInfo ->
            if checkInfo.earlier.fn == repeatFn then
                case checkInfo.later.args of
                    [ elementChangeFunctionArg ] ->
                        Just
                            { info =
                                { message =
                                    qualifiedToString checkInfo.later.fn
                                        ++ " on "
                                        ++ qualifiedToString repeatFn
                                        ++ " is the same as "
                                        ++ qualifiedToString repeatFn
                                        ++ " with the mapped element"
                                , details =
                                    [ "You can replace this composition by composing the function argument given to the "
                                        ++ qualifiedToString checkInfo.later.fn
                                        ++ " operation before the "
                                        ++ qualifiedToString repeatFn
                                        ++ " operation."
                                    ]
                                }
                            , fix =
                                Fix.removeRange checkInfo.later.removeRange
                                    :: composeWithEarlierFix
                                        { range = checkInfo.earlier.range
                                        , direction = compositionCheckInfoDirection checkInfo
                                        , earlier =
                                            parenthesizeIf
                                                (needsParens (Node.value elementChangeFunctionArg))
                                                (checkInfo.extractSourceCode
                                                    (Node.range elementChangeFunctionArg)
                                                )
                                        }
                            }

                    _ ->
                        Nothing

            else
                Nothing
    , call =
        \checkInfo ->
            case checkInfo.argsAfterFirst of
                [ mappedArg ] ->
                    case AstHelpers.getSpecificUnreducedFnCall repeatFn checkInfo.lookupTable mappedArg of
                        Nothing ->
                            Nothing

                        Just repeatFnCall ->
                            case repeatFnCall.argsAfterFirst of
                                [ elementToRepeatArg ] ->
                                    Just
                                        (Rule.errorWithFix
                                            { message =
                                                qualifiedToString checkInfo.fn
                                                    ++ " on "
                                                    ++ qualifiedToString repeatFn
                                                    ++ " is the same as "
                                                    ++ qualifiedToString repeatFn
                                                    ++ " with the mapped element"
                                            , details =
                                                [ "You can replace this call by the "
                                                    ++ qualifiedToString repeatFn
                                                    ++ " operation but with the function given to the "
                                                    ++ qualifiedToString checkInfo.fn
                                                    ++ " operation applied to the original element to repeat."
                                                ]
                                            }
                                            checkInfo.fnRange
                                            (wrapInApplicationFix
                                                { arg = elementToRepeatArg
                                                , function =
                                                    parenthesizeIf
                                                        (needsParens (Node.value checkInfo.firstArg))
                                                        (checkInfo.extractSourceCode
                                                            (Node.range checkInfo.firstArg)
                                                        )
                                                }
                                                ++ replaceCallBySubExpressionFix checkInfo.parentRange
                                                    checkInfo.callStyle
                                                    mappedArg
                                            )
                                        )

                                _ ->
                                    Nothing

                _ ->
                    Nothing
    }


{-| The map checks

    map f empty --> empty

    map identity emptiable --> emptiable

If your mapping function also takes extra information like the key or index as an argument, use `emptiableMapWithExtraArgChecks`.

-}
emptiableMapChecks :
    TypeProperties (EmptiableProperties empty otherProperties)
    -> IntoFnCheck
emptiableMapChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (mapIdentityChecks emptiable)
        , unnecessaryOnEmptyCheck emptiable
        ]


mapIdentityChecks :
    TypeProperties properties
    -> CallCheckInfo
    -> Maybe (Error {})
mapIdentityChecks mappable checkInfo =
    if AstHelpers.isIdentity checkInfo checkInfo.firstArg then
        Just
            (alwaysReturnsLastArgError
                (qualifiedToString checkInfo.fn ++ " with an identity function")
                mappable
                checkInfo
            )

    else
        Nothing


{-| The map checks

    map f empty --> empty

    map (\_ v -> v) emptiable --> emptiable

If your mapping function only takes one value as an argument, use `emptiableMapChecks`.

-}
emptiableMapWithExtraArgChecks : TypeProperties (EmptiableProperties empty otherProperties) -> IntoFnCheck
emptiableMapWithExtraArgChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck emptiable
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case AstHelpers.getAlwaysResult checkInfo checkInfo.firstArg of
                    Just alwaysResult ->
                        if AstHelpers.isIdentity checkInfo alwaysResult then
                            Just
                                (alwaysReturnsLastArgError
                                    (qualifiedToString checkInfo.fn ++ " with a function that maps to the unchanged value")
                                    emptiable
                                    checkInfo
                                )

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )
        ]


{-| The map check

    map f (wrap a) --> wrap (f a)

    map f << wrap --> wrap << f

So for example

    Random.map f (Random.constant a)
    --> Random.constant (f a)

    Random.map f << Random.constant
    --> Random.constant << f

-}
mapOnWrappedChecks : WrapperProperties otherProperties -> IntoFnCheck
mapOnWrappedChecks wrapper =
    { call =
        \checkInfo ->
            case secondArg checkInfo of
                Just wrapperArg ->
                    case wrapper.wrap.getValue checkInfo.lookupTable wrapperArg of
                        Just wrappedValue ->
                            let
                                mappedValueRange : Range
                                mappedValueRange =
                                    Node.range wrappedValue

                                mappingArgRange : Range
                                mappingArgRange =
                                    Node.range checkInfo.firstArg
                            in
                            Just
                                (Rule.errorWithFix
                                    (mapWrapErrorInfo checkInfo.fn wrapper)
                                    checkInfo.fnRange
                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range wrapperArg }
                                        ++ parenthesizeIfNeededFix wrappedValue
                                        ++ (case checkInfo.callStyle of
                                                CallStyle.Pipe CallStyle.LeftToRight ->
                                                    [ Fix.insertAt mappedValueRange.start "("
                                                    , Fix.insertAt mappedValueRange.end
                                                        (" |> " ++ checkInfo.extractSourceCode mappingArgRange ++ ")")
                                                    ]

                                                CallStyle.Pipe CallStyle.RightToLeft ->
                                                    [ Fix.insertAt mappedValueRange.start ("(" ++ checkInfo.extractSourceCode mappingArgRange ++ " <| ")
                                                    , Fix.insertAt mappedValueRange.end ")"
                                                    ]

                                                CallStyle.Application ->
                                                    [ Fix.insertAt mappedValueRange.start ("(" ++ checkInfo.extractSourceCode mappingArgRange ++ " ")
                                                    , Fix.insertAt mappedValueRange.end ")"
                                                    ]
                                           )
                                    )
                                )

                        Nothing ->
                            case sameInAllBranches (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) wrapperArg of
                                Just wraps ->
                                    let
                                        mappingArgRange : Range
                                        mappingArgRange =
                                            Node.range checkInfo.firstArg

                                        removeWrapCalls : List Fix
                                        removeWrapCalls =
                                            List.concatMap
                                                (\wrap ->
                                                    keepOnlyFix
                                                        { parentRange = wrap.nodeRange
                                                        , keep = Node.range wrap.value
                                                        }
                                                )
                                                wraps
                                    in
                                    Just
                                        (Rule.errorWithFix
                                            (mapWrapErrorInfo checkInfo.fn wrapper)
                                            checkInfo.fnRange
                                            (case checkInfo.callStyle of
                                                CallStyle.Pipe CallStyle.LeftToRight ->
                                                    Fix.removeRange { start = checkInfo.fnRange.start, end = mappingArgRange.start }
                                                        :: Fix.insertAt mappingArgRange.end
                                                            (" |> " ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                                        :: removeWrapCalls

                                                CallStyle.Pipe CallStyle.RightToLeft ->
                                                    Fix.replaceRangeBy
                                                        { start = checkInfo.parentRange.start, end = mappingArgRange.start }
                                                        (qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " <| ")
                                                        :: removeWrapCalls

                                                CallStyle.Application ->
                                                    Fix.replaceRangeBy
                                                        { start = checkInfo.parentRange.start, end = mappingArgRange.start }
                                                        (qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " (")
                                                        :: Fix.insertAt checkInfo.parentRange.end ")"
                                                        :: removeWrapCalls
                                            )
                                        )

                                Nothing ->
                                    Nothing

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if checkInfo.earlier.fn == wrapper.wrap.fn then
                case checkInfo.later.args of
                    (Node mapperFunctionRange _) :: _ ->
                        Just
                            { info = mapWrapErrorInfo checkInfo.later.fn wrapper
                            , fix =
                                [ Fix.replaceRangeBy checkInfo.later.range
                                    (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                , Fix.replaceRangeBy checkInfo.earlier.range
                                    (checkInfo.extractSourceCode mapperFunctionRange)
                                ]
                            }

                    _ ->
                        Nothing

            else
                Nothing
    }


{-| The map check

    map (always a) wrapper --> wrap a

    map << always --> always << wrap

So for example

    Random.map (always a) generator
    --> Random.constant a

    Random.map << always
    --: a -> Generator a -> Generator a
    --> always << Random.constant

-}
nonEmptiableWrapperMapAlwaysChecks : NonEmptiableProperties (WrapperProperties otherProperties) -> IntoFnCheck
nonEmptiableWrapperMapAlwaysChecks wrapper =
    { call =
        \checkInfo ->
            case AstHelpers.getAlwaysResult checkInfo checkInfo.firstArg of
                Just (Node alwaysMapResultRange alwaysMapResult) ->
                    let
                        ( leftParenIfRequired, rightParenIfRequired ) =
                            if needsParens alwaysMapResult then
                                ( "(", ")" )

                            else
                                ( "", "" )
                    in
                    Just
                        (case secondArg checkInfo of
                            Nothing ->
                                Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " with a function that always maps to the same value will always result in " ++ qualifiedToString wrapper.wrap.fn ++ " with that value"
                                    , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ " with the value produced by the mapper function." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy
                                        { start = checkInfo.parentRange.start, end = alwaysMapResultRange.start }
                                        (qualifiedToString (qualify Fn.Basics.always checkInfo)
                                            ++ " ("
                                            ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo)
                                            ++ " "
                                            ++ leftParenIfRequired
                                        )
                                    , Fix.replaceRangeBy
                                        { start = alwaysMapResultRange.end, end = checkInfo.parentRange.end }
                                        (rightParenIfRequired ++ ")")
                                    ]

                            Just _ ->
                                Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " with a function that always maps to the same value will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with that value"
                                    , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ " with the value produced by the mapper function." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy
                                        { start = checkInfo.parentRange.start, end = alwaysMapResultRange.start }
                                        (qualifiedToString (qualify wrapper.wrap.fn checkInfo)
                                            ++ " "
                                            ++ leftParenIfRequired
                                        )
                                    , Fix.replaceRangeBy
                                        { start = alwaysMapResultRange.end, end = checkInfo.parentRange.end }
                                        rightParenIfRequired
                                    ]
                        )

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            case ( ( checkInfo.earlier.fn, checkInfo.earlier.args ), checkInfo.later.args ) of
                ( ( ( [ "Basics" ], "always" ), [] ), [] ) ->
                    let
                        equivalent : String
                        equivalent =
                            qualifiedToString wrapper.wrap.fn ++ ", then `always`"
                    in
                    Just
                        { info =
                            { message = qualifiedToString checkInfo.later.fn ++ " with a function that always maps to the same value is equivalent to " ++ equivalent
                            , details = [ "You can replace this call by " ++ equivalent ++ "." ]
                            }
                        , fix =
                            [ Fix.replaceRangeBy checkInfo.earlier.fnRange
                                (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                            , Fix.replaceRangeBy checkInfo.later.fnRange
                                (qualifiedToString (qualify Fn.Basics.always checkInfo))
                            ]
                        }

                _ ->
                    Nothing
    }


{-| The map checks

    flatMap (always emptyConstant) emptyConstant --> emptyConstant

    flatMap f empty --> empty

So for example

    List.concatMap (always []) list --> []

    List.concatMap f [] --> []

-}
emptiableFlatMapChecks : EmptiableProperties ConstantProperties otherProperties -> IntoFnCheck
emptiableFlatMapChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck emptiable
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case toConstructedResult checkInfo.lookupTable checkInfo.firstArg of
                    Just constructed ->
                        if trueInAllBranches (isInTypeSubset emptiable.empty checkInfo) constructed then
                            Just
                                (alwaysResultsInUnparenthesizedConstantError
                                    (qualifiedToString checkInfo.fn ++ " with a function that will always return " ++ emptiable.empty.specific.description)
                                    { replacement = emptiable.empty.specific.asString }
                                    checkInfo
                                )

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )
        ]


{-| `flatMap f` on a wrapped value is equivalent to `f`

    flatMap f (wrap a) --> f a

    flatMap f << wrap --> f

    flatMap wrap wrapper --> wrapper

    flatMap (\a -> wrap b) wrapper --> map (\a -> b) wrapper

    flatMap (wrap << f) wrapper --> map f wrapper

So for example

    List.concatMap f [ a ] --> f a

    List.concatMap f << List.singleton --> f

-}
wrapperFlatMapChecks :
    TypeProperties (WrapperProperties (MappableProperties otherProperties))
    -> IntoFnCheck
wrapperFlatMapChecks wrapper =
    intoFnChecksFirstThatConstructsError
        [ { call =
                \checkInfo ->
                    case secondArg checkInfo of
                        Just maybeArg ->
                            case sameInAllBranches (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) maybeArg of
                                Just wrapCalls ->
                                    Just
                                        (Rule.errorWithFix
                                            { message = qualifiedToString checkInfo.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " is the same as applying the function to the value from " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description
                                            , details = [ "You can replace this call by the function directly applied to the value inside " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description ++ "." ]
                                            }
                                            checkInfo.fnRange
                                            (Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                                                :: List.concatMap (\justCall -> replaceBySubExpressionFix justCall.nodeRange justCall.value) wrapCalls
                                            )
                                        )

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing
          , composition =
                \checkInfo ->
                    if wrapper.wrap.fn == checkInfo.earlier.fn then
                        case checkInfo.later.args of
                            [ Node functionRange _ ] ->
                                Just
                                    { info =
                                        { message = qualifiedToString checkInfo.later.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " is the same as applying the function to the value from " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description
                                        , details = [ "You can replace this composition by the function given to " ++ qualifiedToString checkInfo.later.fn ++ "." ]
                                        }
                                    , fix =
                                        Fix.removeRange checkInfo.earlier.removeRange
                                            :: keepOnlyFix { parentRange = checkInfo.later.range, keep = functionRange }
                                    }

                            _ ->
                                Nothing

                    else
                        Nothing
          }
        , intoFnCheckOnlyCall
            (\checkInfo ->
                if AstHelpers.isSpecificValueOrFn wrapper.wrap.fn checkInfo checkInfo.firstArg then
                    Just
                        (alwaysReturnsLastArgError
                            (qualifiedToString checkInfo.fn ++ " with a function equivalent to " ++ qualifiedToString (qualify wrapper.wrap.fn defaultQualifyResources))
                            wrapper
                            checkInfo
                        )

                else
                    Nothing
            )
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case constructsOrComposesInto wrapper.wrap checkInfo checkInfo.firstArg of
                    Just withoutWrap ->
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " with a function that always returns " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " is the same as " ++ qualifiedToString wrapper.mapFn ++ " with the function returning the value inside"
                                , details = [ "You can replace this call by " ++ qualifiedToString wrapper.mapFn ++ " with the function returning the value inside " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description ++ "." ]
                                }
                                checkInfo.fnRange
                                (Fix.replaceRangeBy checkInfo.fnRange
                                    (qualifiedToString (qualify wrapper.mapFn checkInfo))
                                    :: withoutWrap
                                )
                            )

                    Nothing ->
                        Nothing
            )
        ]


withDefaultChecks :
    WrapperProperties (EmptiableProperties empty otherProperties)
    -> IntoFnCheck
withDefaultChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ onWrappedReturnsItsValueCheck emptiable
        , intoFnCheckOnlyCall (emptiableWithDefaultChecks emptiable)
        ]


{-| The flatMap check

    flatMap (always nextWrapper) wrapper --> nextWrapper

So for example

    Random.andThen (always nextGenerator) generator
    --> nextGenerator

-}
nonEmptiableWrapperFlatMapAlwaysChecks :
    TypeProperties (NonEmptiableProperties (WrapperProperties otherProperties))
    -> CallCheckInfo
    -> Maybe (Error {})
nonEmptiableWrapperFlatMapAlwaysChecks wrapper checkInfo =
    case AstHelpers.getAlwaysResult checkInfo checkInfo.firstArg of
        Just alwaysResult ->
            Just
                (let
                    replacementAndFix : { replacementDescription : String, fix : List Fix }
                    replacementAndFix =
                        case secondArg checkInfo of
                            Nothing ->
                                { replacementDescription = "always with the " ++ wrapper.represents ++ " produced by the function"
                                , fix = replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle checkInfo.firstArg
                                }

                            Just _ ->
                                { replacementDescription = "the " ++ wrapper.represents ++ " produced by the function"
                                , fix = replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle alwaysResult
                                }
                 in
                 Rule.errorWithFix
                    { message = qualifiedToString checkInfo.fn ++ " with a function that always returns to the same " ++ wrapper.represents ++ " will result in that " ++ wrapper.represents
                    , details = [ "You can replace this call by " ++ replacementAndFix.replacementDescription ++ "." ]
                    }
                    checkInfo.fnRange
                    replacementAndFix.fix
                )

        Nothing ->
            Nothing


{-| The "withDefault" checks

    withDefault default empty --> default

so for example

    Result.withDefault "okay" (Err 1)
    --> "okay"

-}
emptiableWithDefaultChecks :
    EmptiableProperties empty otherProperties
    -> CallCheckInfo
    -> Maybe (Error {})
emptiableWithDefaultChecks emptiable checkInfo =
    case secondArg checkInfo of
        Just emptiableArg ->
            if trueInAllBranches (\branch -> isInTypeSubset emptiable.empty checkInfo branch) emptiableArg then
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " on " ++ typeSubsetDescriptionIndefinite emptiable.empty ++ " will result in the default value"
                        , details = [ "You can replace this call by the default value." ]
                        }
                        checkInfo.fnRange
                        (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle checkInfo.firstArg)
                    )

            else
                Nothing

        Nothing ->
            Nothing


unwrapToMaybeChecks :
    WrapperProperties (EmptiableProperties empty otherProperties)
    -> IntoFnCheck
unwrapToMaybeChecks emptiableWrapper =
    intoFnChecksFirstThatConstructsError
        [ onWrappedReturnsJustItsValueCheck emptiableWrapper
        , intoFnCheckOnlyCall
            (callOnEmptyReturnsCheck
                { resultAsString = \res -> qualifiedToString (qualify Fn.Maybe.nothingVariant res) }
                emptiableWrapper
            )
        ]


fromMaybeWithEmptyValueOnNothingCheck : WrapperProperties (EmptiableProperties ConstructWithOneValueProperties otherProperties) -> IntoFnCheck
fromMaybeWithEmptyValueOnNothingCheck wrapper =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Just maybeArg ->
                        if trueInAllBranches (\branch -> AstHelpers.isSpecificValueOrFn Fn.Maybe.nothingVariant checkInfo branch) maybeArg then
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " on Nothing will result in " ++ qualifiedToString (qualify wrapper.empty.specific.fn checkInfo) ++ " with the given first value"
                                    , details = [ "You can replace this call by " ++ qualifiedToString (qualify wrapper.empty.specific.fn checkInfo) ++ " with the given first value." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify wrapper.empty.specific.fn checkInfo))
                                        :: keepOnlyFix
                                            { parentRange = checkInfo.parentRange
                                            , keep = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                            }
                                    )
                                )

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )
        , { call =
                \checkInfo ->
                    case secondArg checkInfo of
                        Just maybeArg ->
                            case
                                sameInAllBranches
                                    (\branch ->
                                        AstHelpers.getSpecificUnreducedFnCall Fn.Maybe.justVariant
                                            checkInfo.lookupTable
                                            branch
                                    )
                                    maybeArg
                            of
                                Just justCalls ->
                                    Just
                                        (Rule.errorWithFix
                                            { message = qualifiedToString checkInfo.fn ++ " on a just maybe will result in " ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " with the value inside"
                                            , details = [ "You can replace this call by " ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " with the value inside the given just maybe." ]
                                            }
                                            checkInfo.fnRange
                                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range maybeArg }
                                                ++ List.map
                                                    (\justCall ->
                                                        Fix.replaceRangeBy justCall.fnRange
                                                            (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                                    )
                                                    justCalls
                                            )
                                        )

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing
          , composition =
                \checkInfo ->
                    case ( checkInfo.earlier.fn, checkInfo.later.args ) of
                        ( ( [ "Maybe" ], "Just" ), _ :: [] ) ->
                            Just
                                { info =
                                    { message = qualifiedToString checkInfo.later.fn ++ " on a just maybe will result in " ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " with the value inside"
                                    , details = [ "You can replace this call by " ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ "." ]
                                    }
                                , fix =
                                    [ Fix.removeRange checkInfo.later.removeRange
                                    , Fix.replaceRangeBy checkInfo.earlier.range (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                    ]
                                }

                        _ ->
                            Nothing
          }
        ]


{-| The "flatFromList" checks

    flatFromList []
    --> empty

    flatFromList [ emptiable ]
    --> emptiable

    flatFromList [ aEmptiable, empty, bEmptiable ]
    --> flatFromList [ aEmptiable, bEmptiable ]

    flatFromList [ a, flatFromList [ b, c ], d ]
    --> flatFromList [ a, b, c, d ]

So for example with `emptiableFlatFromListChecks stringCollection`

    String.concat []
    --> ""

    String.concat [ string ]
    --> string

    String.concat [ "hello", "", "world" ]
    --> String.concat [ "hello", "world" ]

    String.concat [ "a", String.concat [ b, c ], d ]
    --> String.concat [ "a", b, c, d ]

-}
emptiableFlatFromListChecks : EmptiableProperties ConstantProperties otherProperties -> IntoFnCheck
emptiableFlatFromListChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ onWrappedReturnsItsValueCheck listCollection
        , intoFnCheckOnlyCall (callOnEmptyReturnsCheck { resultAsString = emptiable.empty.specific.asString } listCollection)
        , intoFnCheckOnlyCall
            (\checkInfo ->
                callOnFromListWithIrrelevantEmptyElement (qualifiedToString (qualify checkInfo.fn defaultQualifyResources))
                    ( listCollection, emptiable )
                    checkInfo
            )
        , intoFnCheckOnlyCall flatFromListsSpreadFlatFromListElementsCheck
        ]


{-| The "flatFromList" check

    flatFromList [ a, flatFromList [ b, c ], d ]
    --> flatFromList [ a, b, c, d ]

So for example

    String.concat [ "a", String.concat [ b, c ], d ]
    --> String.concat [ "a", b, c, d ]

-}
flatFromListsSpreadFlatFromListElementsCheck : CallCheckInfo -> Maybe (Error {})
flatFromListsSpreadFlatFromListElementsCheck checkInfo =
    case AstHelpers.getListLiteral checkInfo.firstArg of
        Just listLiteralElements ->
            let
                getFlatFromListOnLiteralCheck : Node Expression -> Maybe { callNodeRange : Range, literalRange : Range }
                getFlatFromListOnLiteralCheck expressionNode =
                    AstHelpers.getSpecificUnreducedFnCall checkInfo.fn
                        checkInfo.lookupTable
                        expressionNode
                        |> Maybe.andThen
                            (\flatFromListCall ->
                                case flatFromListCall.firstArg of
                                    Node literalRange (Expression.ListExpr _) ->
                                        Just { callNodeRange = Node.range expressionNode, literalRange = literalRange }

                                    _ ->
                                        Nothing
                            )
            in
            case List.filterMap getFlatFromListOnLiteralCheck listLiteralElements of
                [] ->
                    Nothing

                (_ :: _) as nestedFromListLiterals ->
                    let
                        flatFromListOnLiteralDescription : String
                        flatFromListOnLiteralDescription =
                            qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " call"
                    in
                    Just
                        (Rule.errorWithFix
                            { message = "Nested " ++ flatFromListOnLiteralDescription ++ "s can be spread"
                            , details = [ "You can move the elements from the inner " ++ flatFromListOnLiteralDescription ++ "s to inside this outer " ++ flatFromListOnLiteralDescription ++ "." ]
                            }
                            checkInfo.fnRange
                            (List.concatMap
                                (\nestedFromListLiteral ->
                                    keepOnlyFix { parentRange = nestedFromListLiteral.callNodeRange, keep = rangeWithoutBoundaries nestedFromListLiteral.literalRange }
                                )
                                nestedFromListLiterals
                            )
                        )

        Nothing ->
            Nothing


callOnFromListWithIrrelevantEmptyElement :
    String
    ->
        ( TypeProperties (ConstructibleFromListProperties otherProperties)
        , EmptiableProperties empty elementOtherProperties
        )
    -> CallCheckInfo
    -> Maybe (Error {})
callOnFromListWithIrrelevantEmptyElement situation ( constructibleFromList, emptiableElement ) checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just collectionArg ->
            case fromListGetLiteral constructibleFromList checkInfo.lookupTable collectionArg of
                Just listLiteral ->
                    case findMapNeighboring (getEmptyExpressionNode checkInfo emptiableElement) listLiteral.elements of
                        Just emptyLiteralAndNeighbors ->
                            Just
                                (Rule.errorWithFix
                                    { message = situation ++ " on a " ++ constructibleFromList.represents ++ " containing an irrelevant " ++ typeSubsetDescriptionWithoutArticle emptiableElement.empty
                                    , details = [ "Including " ++ typeSubsetDescriptionDefinite "the" emptiableElement.empty ++ " in the " ++ constructibleFromList.represents ++ " does not change the result of this call. You can remove the " ++ typeSubsetDescriptionWithoutArticle emptiableElement.empty ++ " element." ]
                                    }
                                    (Node.range emptyLiteralAndNeighbors.found)
                                    (listLiteralRemoveElementFix emptyLiteralAndNeighbors)
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| The check to unify neighboring constructions from a literal list in a literal list

    fromList construction
        with
        [ a
        , fromList construction with [ b, c ]
        , fromList construction with [ d, e ]
        , fromList construction with [ f, g ]
        , h
        ]

    -->
    fromList construction
        with
        [ a
        , fromList construction with [ b, c, d, e, f, g ]
        , h
        ]

-}
mergeConsecutiveFromListLiteralsCheck :
    ConstructibleFromListProperties otherProperties
    -> List (Node Expression)
    -> CallCheckInfo
    -> Maybe (Error {})
mergeConsecutiveFromListLiteralsCheck constructibleFromList listElements checkInfo =
    case mergeConsecutiveFromListLiteralsFixWithBeforeEndingIn Nothing [] constructibleFromList checkInfo listElements of
        [] ->
            Nothing

        (_ :: _) as fixes ->
            Just
                (Rule.errorWithFix
                    { message = "Consecutive " ++ constructionFromListOnLiteralDescription constructibleFromList.fromList ++ "s can be merged"
                    , details = [ "Try moving all the elements from consecutive " ++ constructionFromListOnLiteralDescription constructibleFromList.fromList ++ "s so that they form a single list." ]
                    }
                    checkInfo.fnRange
                    fixes
                )


mergeConsecutiveFromListLiteralsFixWithBeforeEndingIn : Maybe { literalInsertLocation : Location, elementEndLocation : Location } -> List Fix -> ConstructibleFromListProperties otherProperties -> { resources | lookupTable : ModuleNameLookupTable, extractSourceCode : Range -> String } -> List (Node Expression) -> List Fix
mergeConsecutiveFromListLiteralsFixWithBeforeEndingIn maybeBefore fixEnding constructibleFromList resources listElements =
    case listElements of
        [] ->
            fixEnding

        first :: afterFirst ->
            case fromListGetLiteral constructibleFromList resources.lookupTable first of
                Nothing ->
                    mergeConsecutiveFromListLiteralsFixWithBeforeEndingIn Nothing fixEnding constructibleFromList resources afterFirst

                Just firstFromListLiteral ->
                    case maybeBefore of
                        Just before ->
                            mergeConsecutiveFromListLiteralsFixWithBeforeEndingIn
                                (Just
                                    { literalInsertLocation = before.literalInsertLocation
                                    , elementEndLocation = (Node.range first).end
                                    }
                                )
                                (Fix.removeRange
                                    { start = before.elementEndLocation, end = (Node.range first).end }
                                    :: Fix.insertAt before.literalInsertLocation
                                        (", " ++ resources.extractSourceCode (rangeWithoutBoundaries firstFromListLiteral.literalRange))
                                    :: fixEnding
                                )
                                constructibleFromList
                                resources
                                afterFirst

                        Nothing ->
                            mergeConsecutiveFromListLiteralsFixWithBeforeEndingIn
                                (Just
                                    { literalInsertLocation = endWithoutBoundary firstFromListLiteral.literalRange
                                    , elementEndLocation = (Node.range first).end
                                    }
                                )
                                fixEnding
                                constructibleFromList
                                resources
                                afterFirst


{-| The range checks

    range 5 4 --> empty

So for example

    List.range 5 4 --> []

-}
emptiableRangeChecks : EmptiableProperties ConstantProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
emptiableRangeChecks emptiable checkInfo =
    case secondArg checkInfo of
        Just rangeEndArg ->
            case Normalize.compare checkInfo checkInfo.firstArg rangeEndArg of
                Normalize.ConfirmedEquality ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " with equal start and end will result in a singleton with that value"
                            , details = [ "You can replace this call by its start or equivalent end argument and wrap it in a new list." ]
                            }
                            checkInfo.fnRange
                            (keepOnlyAndSurroundWithFix
                                { parentRange = checkInfo.parentRange
                                , keep =
                                    -- choosing to replace the start argument is arbitrary.
                                    -- we could instead also choose based on some heuristic
                                    Node.range checkInfo.firstArg
                                , left = "[ "
                                , right = " ]"
                                }
                            )
                        )

                _ ->
                    case Normalize.getInt checkInfo checkInfo.firstArg of
                        Just rangeStartValue ->
                            case Normalize.getInt checkInfo rangeEndArg of
                                Just rangeEndValue ->
                                    if rangeStartValue > rangeEndValue then
                                        Just
                                            (resultsInConstantError
                                                (qualifiedToString checkInfo.fn ++ " with a start index greater than the end index")
                                                emptiable.empty.specific.asString
                                                checkInfo
                                            )

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing

        Nothing ->
            Nothing


{-| Turn `yourFn identity` into `replacementFn`. If `replacementFn` should be `identity`, use `alwaysReturnsLastArgError` instead

Can be used to for example

  - turn `traverse identity` into `sequence`
  - turn `List.filterMap identity` into `Maybe.Extra.values`
  - turn `List.Extra.minimumBy identity` into `List.minimum`

-}
operationWithIdentityIsEquivalentToFnCheck : ( ModuleName, String ) -> CallCheckInfo -> Maybe (Error {})
operationWithIdentityIsEquivalentToFnCheck replacementFn checkInfo =
    if AstHelpers.isIdentity checkInfo checkInfo.firstArg then
        Just
            (operationWithFirstArgIsEquivalentToFnError
                { replacementFn = replacementFn
                , firstArgDescription = "an identity function"
                }
                checkInfo
            )

    else
        Nothing


{-| Check for a function that given an empty separator is equivalent to a given replacement operation.

    flatIntersperse empty something
    --> replacementOperation something

So for example

    List.Extra.intercalate : List a -> List (List a) -> List a
    List.Extra.intercalate [] list --> List.concat list

    DList.intersperse : DList a -> List (DList a) -> DList a
    DList.intersperse DList.empty dList --> DList.concat dList

Note that this really only applies to "flat-intersperse-like" functions, not for example

    concatMap identity type --> concat type
    -- where identity would be the "empty function"

for that specific example, there is `operationWithIdentityIsEquivalentToFnCheck`

-}
flatIntersperseWithEmptySeparatorIsEquivalentToFnCheck : EmptiableProperties empty otherProperties -> ( ModuleName, String ) -> CallCheckInfo -> Maybe (Error {})
flatIntersperseWithEmptySeparatorIsEquivalentToFnCheck elementProperties replacementFn checkInfo =
    if isInTypeSubset elementProperties.empty checkInfo checkInfo.firstArg then
        Just
            (operationWithFirstArgIsEquivalentToFnError
                { replacementFn = replacementFn
                , firstArgDescription = "separator " ++ typeSubsetDescriptionWithoutArticle elementProperties.empty
                }
                checkInfo
            )

    else
        Nothing


operationWithFirstArgIsEquivalentToFnError : { firstArgDescription : String, replacementFn : ( ModuleName, String ) } -> CallCheckInfo -> Error {}
operationWithFirstArgIsEquivalentToFnError config checkInfo =
    Rule.errorWithFix
        { message = qualifiedToString checkInfo.fn ++ " with " ++ config.firstArgDescription ++ " is the same as " ++ qualifiedToString config.replacementFn
        , details = [ "You can replace this call by " ++ qualifiedToString config.replacementFn ++ "." ]
        }
        checkInfo.fnRange
        [ Fix.replaceRangeBy
            (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
            (qualifiedToString (qualify config.replacementFn checkInfo))
        ]


operationWithSpecificArgsIsEquivalentToFnError :
    { specificArgsDescription : String, replacementFn : ( ModuleName, String ) }
    -> CallCheckInfo
    -> Error {}
operationWithSpecificArgsIsEquivalentToFnError config checkInfo =
    Rule.errorWithFix
        { message =
            qualifiedToString checkInfo.fn
                ++ " "
                ++ config.specificArgsDescription
                ++ " is the same as "
                ++ qualifiedToString config.replacementFn
        , details =
            [ "You can replace this call by "
                ++ qualifiedToString config.replacementFn
                ++ " which is meant for this exact purpose."
            ]
        }
        checkInfo.fnRange
        (case fullyAppliedLastArg checkInfo of
            Just lastArg ->
                fixToCall
                    { fn = config.replacementFn
                    , style = checkInfo.callStyle
                    , argRange = checkInfo.parentRange
                    }
                    checkInfo
                    :: replaceBySubExpressionFix checkInfo.parentRange
                        lastArg

            Nothing ->
                [ Fix.replaceRangeBy checkInfo.parentRange
                    (qualifiedToString (qualify config.replacementFn checkInfo))
                ]
        )


wrapInApplicationFix : { arg : Node Expression, function : String } -> List Fix
wrapInApplicationFix config =
    let
        elementToRepeatArgNeedsParens : Bool
        elementToRepeatArgNeedsParens =
            needsParens (Node.value config.arg)
    in
    [ Fix.insertAt (Node.range config.arg).start
        ("("
            ++ config.function
            ++ (if
                    rangeSpansMultipleLines (Node.range config.arg)
                        || String.contains "\n" config.function
                then
                    "\n" ++ String.repeat ((Node.range config.arg).start.column - 1) " "

                else
                    " "
               )
            ++ (if elementToRepeatArgNeedsParens then
                    "("

                else
                    ""
               )
        )
    , Fix.insertAt (Node.range config.arg).end
        ((if elementToRepeatArgNeedsParens then
            ")"

          else
            ""
         )
            ++ ")"
        )
    ]


fixToCall :
    { fn : ( ModuleName, String ), style : FunctionCallStyle, argRange : Range }
    -> QualifyResources a
    -> Fix
fixToCall config resources =
    let
        fnAsString : String
        fnAsString =
            qualifiedToString (qualify config.fn resources)
    in
    case config.style of
        CallStyle.Application ->
            Fix.insertAt config.argRange.start (fnAsString ++ " ")

        CallStyle.Pipe CallStyle.RightToLeft ->
            Fix.insertAt config.argRange.start (fnAsString ++ " <| ")

        CallStyle.Pipe CallStyle.LeftToRight ->
            Fix.insertAt config.argRange.end (" |> " ++ fnAsString)


callOnExistingCallFix :
    { fn : ( ModuleName, String ), style : FunctionCallStyle, range : Range }
    -> QualifyResources a
    -> List Fix
callOnExistingCallFix config resources =
    let
        fnAsString : String
        fnAsString =
            qualifiedToString (qualify config.fn resources)
    in
    case config.style of
        CallStyle.Application ->
            [ Fix.insertAt config.range.start (fnAsString ++ " (")
            , Fix.insertAt config.range.end ")"
            ]

        CallStyle.Pipe CallStyle.RightToLeft ->
            [ Fix.insertAt config.range.start (fnAsString ++ " <| ") ]

        CallStyle.Pipe CallStyle.LeftToRight ->
            [ Fix.insertAt config.range.end (" |> " ++ fnAsString) ]


{-| Map where the usual map function has an extra argument with special information.

For example `indexedMap` also supplied an index. Not using the index would be identical to `map`.

Another example would be [`List.Extra.indexedFoldl`](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra#indexedFoldl) which also supplies the current index.
Not using the path would be identical to `List.foldl`.

-}
operationWithExtraArgChecks : { operationWithoutExtraArg : ( ModuleName, String ) } -> CallCheckInfo -> Maybe (Error {})
operationWithExtraArgChecks config checkInfo =
    case getReplaceAlwaysByItsResultFix checkInfo.lookupTable checkInfo.firstArg of
        Just replaceAlwaysByFunctionResult ->
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString checkInfo.fn ++ " with a function that ignores the first argument is the same as " ++ qualifiedToString config.operationWithoutExtraArg
                    , details = [ "You can replace this call by " ++ qualifiedToString config.operationWithoutExtraArg ++ "." ]
                    }
                    checkInfo.fnRange
                    (Fix.replaceRangeBy checkInfo.fnRange
                        (qualifiedToString (qualify config.operationWithoutExtraArg checkInfo))
                        :: replaceAlwaysByFunctionResult
                    )
                )

        Nothing ->
            Nothing


getReplaceAlwaysByItsResultFix : ModuleNameLookupTable -> Node Expression -> Maybe (List Fix)
getReplaceAlwaysByItsResultFix lookupTable expressionNode =
    case AstHelpers.removeParens expressionNode of
        Node _ (Expression.LambdaExpression lambda) ->
            case lambda.args of
                firstArg :: argsAfterFirst ->
                    case AstHelpers.removeParensFromPattern firstArg of
                        Node _ Pattern.AllPattern ->
                            case argsAfterFirst of
                                [] ->
                                    Just (keepOnlyFix { parentRange = Node.range expressionNode, keep = Node.range lambda.expression })

                                (Node secondRange _) :: _ ->
                                    Just
                                        [ Fix.removeRange { start = (Node.range firstArg).start, end = secondRange.start } ]

                        _ ->
                            Nothing

                [] ->
                    Nothing

        _ ->
            case AstHelpers.getSpecificUnreducedFnCall Fn.Basics.always lookupTable expressionNode of
                Just alwaysCall ->
                    Just
                        (replaceBySubExpressionFix alwaysCall.nodeRange alwaysCall.firstArg)

                Nothing ->
                    Nothing


wrapperMemberChecks : TypeProperties (WrapperProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
wrapperMemberChecks wrapper checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just wrapperArg ->
            case wrapper.wrap.getValue checkInfo.lookupTable wrapperArg of
                Just wrapValue ->
                    let
                        needleArgRange : Range
                        needleArgRange =
                            Node.range checkInfo.firstArg
                    in
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description ++ " are equal." ]
                            }
                            checkInfo.fnRange
                            (Fix.replaceRangeBy
                                (rangeBetweenExclusive needleArgRange (Node.range wrapValue))
                                " == "
                                :: keepOnlyFix
                                    { parentRange = checkInfo.parentRange
                                    , keep = Range.combine [ needleArgRange, Node.range wrapValue ]
                                    }
                                ++ parenthesizeIfNeededFix wrapValue
                            )
                        )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


knownMemberChecks : TypeProperties (CollectionProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
knownMemberChecks collection checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just collectionArg ->
            case collection.elements.get (extractNormalizeResources checkInfo) collectionArg of
                Just collectionElements ->
                    let
                        needleArg : Node Expression
                        needleArg =
                            checkInfo.firstArg
                    in
                    if
                        checkInfo.expectNaN
                            && (not collectionElements.allKnown
                                    || (AstHelpers.couldBeValueContainingNaN needleArg
                                            || List.any AstHelpers.couldBeValueContainingNaN
                                                collectionElements.known
                                       )
                               )
                    then
                        Nothing

                    else
                        let
                            needleArgNormalized : Expression
                            needleArgNormalized =
                                Normalize.normalizeExpression checkInfo needleArg

                            elementEqualitiesToNeedle : List Normalize.Comparison
                            elementEqualitiesToNeedle =
                                collectionElements.known
                                    |> List.map
                                        (\element ->
                                            Normalize.compareExistingNormals
                                                (Normalize.normalizeExpression checkInfo element)
                                                needleArgNormalized
                                        )
                        in
                        if List.member Normalize.ConfirmedEquality elementEqualitiesToNeedle then
                            Just
                                (resultsInConstantError
                                    (qualifiedToString checkInfo.fn
                                        ++ " on a "
                                        ++ collection.represents
                                        ++ " which contains the given "
                                        ++ collection.elements.elementDescription
                                    )
                                    (\res -> qualifiedToString (qualify Fn.Basics.trueVariant res))
                                    checkInfo
                                )

                        else if
                            collectionElements.allKnown
                                && List.all
                                    (\elementEqualityToNeedle ->
                                        elementEqualityToNeedle == Normalize.ConfirmedInequality
                                    )
                                    elementEqualitiesToNeedle
                        then
                            Just
                                (resultsInConstantError
                                    (qualifiedToString checkInfo.fn
                                        ++ " on a "
                                        ++ collection.represents
                                        ++ " which does not contain the given "
                                        ++ collection.elements.elementDescription
                                    )
                                    (\res -> qualifiedToString (qualify Fn.Basics.falseVariant res))
                                    checkInfo
                                )

                        else
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| Replace a call on a collection containing an absorbing element to the absorbing element.
See `AbsorbableProperties` for details.

    operation ..args.. (collection containing absorbing)
    --> absorbing

So for example with `( listCollection, boolForAndProperties )`

    List.product [ a, 0, b ]
    --> 0

with `( listCollection, numberNotExpectingNaNForMultiplyProperties )` and a check for identity

    List.all identity [ a, False, b ]
    --> False

-}
callOnCollectionWithAbsorbingElementChecks :
    String
    -> ( TypeProperties (CollectionProperties otherProperties), AbsorbableProperties elementOtherProperties )
    -> CallCheckInfo
    -> Maybe (Error {})
callOnCollectionWithAbsorbingElementChecks situation ( collection, elementAbsorbable ) checkInfo =
    case Maybe.andThen (\lastArg -> collection.elements.get (extractNormalizeResources checkInfo) lastArg) (fullyAppliedLastArg checkInfo) of
        Just elements ->
            case findMap (getAbsorbingExpressionNode elementAbsorbable checkInfo) elements.known of
                Just absorbingElement ->
                    Just
                        (Rule.errorWithFix
                            { message = situation ++ " on a " ++ collection.represents ++ " with " ++ elementAbsorbable.absorbing.description ++ " will result in " ++ elementAbsorbable.absorbing.description
                            , details =
                                [ "You can replace this call by " ++ elementAbsorbable.absorbing.asString defaultQualifyResources ++ "." ]
                            }
                            checkInfo.fnRange
                            (replaceBySubExpressionFix checkInfo.parentRange absorbingElement)
                        )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| The all elements pass a given test checks

    all f empty --> True

    all (always True) --> always True

    all (always True) emptiable --> True

    all (always False) emptiable --> isEmpty emptiable

    all (always False) --> isEmpty

-}
emptiableAllChecks :
    TypeProperties (EmptiableProperties empty { otherProperties | isEmptyFn : ( ModuleName, String ) })
    -> CallCheckInfo
    -> Maybe (Error {})
emptiableAllChecks emptiable checkInfo =
    callOnEmptyReturnsCheck
        { resultAsString = \res -> qualifiedToString (qualify Fn.Basics.trueVariant res) }
        emptiable
        checkInfo
        |> onNothing
            (\() ->
                case AstHelpers.getAlwaysResult checkInfo checkInfo.firstArg of
                    Just alwaysResult ->
                        case Normalize.getBool checkInfo alwaysResult of
                            Just True ->
                                Just
                                    (alwaysResultsInUnparenthesizedConstantError
                                        (qualifiedToString checkInfo.fn ++ " with a function that will always return True")
                                        { replacement = \res -> qualifiedToString (qualify Fn.Basics.trueVariant res) }
                                        checkInfo
                                    )

                            Just False ->
                                Just
                                    (Rule.errorWithFix
                                        { message =
                                            qualifiedToString checkInfo.fn
                                                ++ " with a function that will always return False is the same as "
                                                ++ qualifiedToString emptiable.isEmptyFn
                                        , details =
                                            [ "You can replace this call by "
                                                ++ qualifiedToString emptiable.isEmptyFn
                                                ++ (case checkInfo.argsAfterFirst of
                                                        [] ->
                                                            ""

                                                        _ :: _ ->
                                                            " on the "
                                                                ++ emptiable.represents
                                                                ++ " given to the "
                                                                ++ qualifiedToString checkInfo.fn
                                                                ++ " call"
                                                   )
                                                ++ "."
                                            ]
                                        }
                                        checkInfo.fnRange
                                        [ Fix.replaceRangeBy (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                            (qualifiedToString (qualify emptiable.isEmptyFn checkInfo))
                                        ]
                                    )

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            )


collectionAllChecks : TypeProperties (CollectionProperties (ConstructibleFromListProperties otherProperties)) -> CallCheckInfo -> Maybe (Error {})
collectionAllChecks collection checkInfo =
    (if AstHelpers.isIdentity checkInfo checkInfo.firstArg then
        callOnCollectionWithAbsorbingElementChecks (qualifiedToString checkInfo.fn ++ " with an identity function")
            ( collection, boolForAndProperties )
            checkInfo
            |> onNothing
                (\() ->
                    callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn ++ " with an identity function")
                        ( collection, boolForAndProperties )
                        checkInfo
                )

     else
        Nothing
    )
        |> onNothing
            (\() ->
                if AstHelpers.isSpecificValueOrFn Fn.Basics.not checkInfo checkInfo.firstArg then
                    case Maybe.andThen (\collectionArg -> collection.elements.get (extractNormalizeResources checkInfo) collectionArg) (fullyAppliedLastArg checkInfo) of
                        Just elements ->
                            if List.any (\element -> boolTrueConstant.is (extractNormalizeResources checkInfo) element) elements.known then
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " with `not` on a " ++ collection.represents ++ " with True will result in False"
                                        , details =
                                            [ "You can replace this call by False." ]
                                        }
                                        checkInfo.fnRange
                                        [ Fix.replaceRangeBy checkInfo.parentRange
                                            (qualifiedToString (qualify Fn.Basics.falseVariant checkInfo))
                                        ]
                                    )

                            else
                                callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn ++ " with `not`")
                                    ( collection, { empty = { specific = boolFalseConstant, kind = Constant } } )
                                    checkInfo

                        Nothing ->
                            Nothing

                else
                    Nothing
            )


{-| The any element passes a given test checks

    any f empty --> False
    any (always False) --> always False
    any (always False) emptiable -> False
    any (always True) --> not << isEmpty
    any (always True) emptiable --> not (isEmpty emptiable)

-}
emptiableAnyChecks :
    TypeProperties (EmptiableProperties empty { otherProperties | isEmptyFn : ( ModuleName, String ) })
    -> CallCheckInfo
    -> Maybe (Error {})
emptiableAnyChecks emptiable checkInfo =
    callOnEmptyReturnsCheck
        { resultAsString = \res -> qualifiedToString (qualify Fn.Basics.falseVariant res) }
        emptiable
        checkInfo
        |> onNothing
            (\() ->
                case AstHelpers.getAlwaysResult checkInfo checkInfo.firstArg of
                    Just alwaysResult ->
                        case Normalize.getBool checkInfo alwaysResult of
                            Just False ->
                                Just
                                    (alwaysResultsInUnparenthesizedConstantError
                                        (qualifiedToString checkInfo.fn ++ " with a function that will always return False")
                                        { replacement = \res -> qualifiedToString (qualify Fn.Basics.falseVariant res) }
                                        checkInfo
                                    )

                            Just True ->
                                Just
                                    (Rule.errorWithFix
                                        { message =
                                            qualifiedToString checkInfo.fn
                                                ++ " with a function that will always return True is the same as "
                                                ++ qualifiedToString Fn.Basics.not
                                                ++ " on "
                                                ++ qualifiedToString emptiable.isEmptyFn
                                        , details =
                                            [ "You can replace this call by "
                                                ++ (case checkInfo.argsAfterFirst of
                                                        [] ->
                                                            qualifiedToString emptiable.isEmptyFn
                                                                ++ ", then "
                                                                ++ qualifiedToString Fn.Basics.not

                                                        _ :: _ ->
                                                            qualifiedToString Fn.Basics.not
                                                                ++ " on "
                                                                ++ qualifiedToString emptiable.isEmptyFn
                                                                ++ " on the "
                                                                ++ emptiable.represents
                                                                ++ " given to the "
                                                                ++ qualifiedToString checkInfo.fn
                                                                ++ " call"
                                                   )
                                                ++ "."
                                            ]
                                        }
                                        checkInfo.fnRange
                                        (case checkInfo.argsAfterFirst of
                                            [] ->
                                                [ Fix.replaceRangeBy checkInfo.parentRange
                                                    ("("
                                                        ++ fnCompositionString
                                                            { earlier = emptiable.isEmptyFn
                                                            , later = Fn.Basics.not
                                                            , direction =
                                                                case checkInfo.callStyle of
                                                                    CallStyle.Pipe direction ->
                                                                        direction

                                                                    CallStyle.Application ->
                                                                        CallStyle.RightToLeft
                                                            }
                                                            checkInfo
                                                        ++ ")"
                                                    )
                                                ]

                                            _ :: _ ->
                                                Fix.replaceRangeBy (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                                    (qualifiedToString (qualify emptiable.isEmptyFn checkInfo))
                                                    :: callOnExistingCallFix
                                                        { fn = Fn.Basics.not
                                                        , range = checkInfo.parentRange
                                                        , style = checkInfo.callStyle
                                                        }
                                                        checkInfo
                                        )
                                    )

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            )


collectionAnyChecks : TypeProperties (CollectionProperties (ConstructibleFromListProperties otherProperties)) -> CallCheckInfo -> Maybe (Error {})
collectionAnyChecks collection checkInfo =
    (if AstHelpers.isIdentity checkInfo checkInfo.firstArg then
        callOnCollectionWithAbsorbingElementChecks (qualifiedToString checkInfo.fn ++ " with an identity function")
            ( collection, boolForOrProperties )
            checkInfo
            |> onNothing
                (\() ->
                    callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn ++ " with an identity function")
                        ( collection, boolForOrProperties )
                        checkInfo
                )

     else
        Nothing
    )
        |> onNothing
            (\() ->
                if AstHelpers.isSpecificValueOrFn Fn.Basics.not checkInfo checkInfo.firstArg then
                    case Maybe.andThen (\lastArg -> collection.elements.get (extractNormalizeResources checkInfo) lastArg) (fullyAppliedLastArg checkInfo) of
                        Just elements ->
                            if List.any (\element -> boolFalseConstant.is (extractNormalizeResources checkInfo) element) elements.known then
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " with `not` on a " ++ collection.represents ++ " with False will result in True"
                                        , details =
                                            [ "You can replace this call by True." ]
                                        }
                                        checkInfo.fnRange
                                        [ Fix.replaceRangeBy checkInfo.parentRange
                                            (qualifiedToString (qualify Fn.Basics.trueVariant checkInfo))
                                        ]
                                    )

                            else
                                callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn ++ " with `not`")
                                    ( collection, { empty = { specific = boolTrueConstant, kind = Constant } } )
                                    checkInfo

                        Nothing ->
                            Nothing

                else
                    Nothing
            )


{-| The check

    fn (\el -> el == constant) something
    --> replacementFn constant something

So for example

    List.any (\el -> el == constant) list
    --> List.member constant list

    List.Extra.removeIfIndex (\i -> i == constant) list
    --> List.Extra.removeAt constant list

    -- ↓ doesn't exist
    Set.Extra.filterNot (\k -> k == constant) set
    --> Set.remove constant

-}
operationWithEqualsConstantIsEquivalentToFnWithThatConstantCheck : ( ModuleName, String ) -> CallCheckInfo -> Maybe (Error {})
operationWithEqualsConstantIsEquivalentToFnWithThatConstantCheck replacementFn checkInfo =
    case getSpecificOperationWithConstantFunction "==" checkInfo checkInfo.firstArg of
        Nothing ->
            Nothing

        Just equatedTo ->
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString checkInfo.fn ++ " with a check for equality with a specific value can be replaced by " ++ qualifiedToString replacementFn ++ " with that value"
                    , details = [ "You can replace this call by " ++ qualifiedToString replacementFn ++ " with the specific value to find which is meant for this exact purpose." ]
                    }
                    checkInfo.fnRange
                    (Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify replacementFn checkInfo))
                        :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) equatedTo.constant
                    )
                )


getSpecificOperationWithConstantFunction : String -> Normalize.Resources a -> Node Expression -> Maybe { constant : Node Expression }
getSpecificOperationWithConstantFunction specificOperator resources expressionNode =
    case Node.value expressionNode of
        Expression.ParenthesizedExpression inParens ->
            getSpecificOperationWithConstantFunction specificOperator resources inParens

        Expression.LambdaExpression lambda ->
            case lambda.args of
                [ Node _ (Pattern.VarPattern var) ] ->
                    case Node.value (AstHelpers.removeParens lambda.expression) of
                        Expression.OperatorApplication operator _ ((Node _ left) as leftNode) ((Node _ right) as rightNode) ->
                            if operator == specificOperator then
                                let
                                    nodeToFind : Expression
                                    nodeToFind =
                                        Expression.FunctionOrValue [] var
                                in
                                if (left == nodeToFind) && not (expressionContainsVariable var rightNode) then
                                    Just { constant = rightNode }

                                else if (right == nodeToFind) && not (expressionContainsVariable var leftNode) then
                                    Just { constant = leftNode }

                                else
                                    Nothing

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            case AstHelpers.getSingleArgCall expressionNode of
                Nothing ->
                    Nothing

                Just call ->
                    if call.called |> Normalize.isSpecificUnappliedBinaryOperation specificOperator resources then
                        Just { constant = call.arg }

                    else
                        Nothing


expressionContainsVariable : String -> Node Expression -> Bool
expressionContainsVariable variableToFind (Node _ expressionNode) =
    case expressionNode of
        Expression.FunctionOrValue [] variable ->
            variable == variableToFind

        Expression.LambdaExpression lambda ->
            expressionContainsVariable variableToFind lambda.expression

        Expression.ParenthesizedExpression expressionInParens ->
            expressionContainsVariable variableToFind expressionInParens

        Expression.Negation expressionInNegation ->
            expressionContainsVariable variableToFind expressionInNegation

        Expression.LetExpression letBlock ->
            expressionContainsVariable variableToFind letBlock.expression
                || List.any
                    (\(Node _ letDeclaration) ->
                        expressionContainsVariable variableToFind
                            (case letDeclaration of
                                Expression.LetFunction letFunction ->
                                    letFunction.declaration |> Node.value |> .expression

                                Expression.LetDestructuring _ expression_ ->
                                    expression_
                            )
                    )
                    letBlock.declarations

        Expression.ListExpr elements ->
            List.any (\element -> expressionContainsVariable variableToFind element)
                elements

        Expression.TupledExpression parts ->
            List.any (\part -> expressionContainsVariable variableToFind part) parts

        Expression.RecordExpr fields ->
            List.any
                (\(Node _ ( _, value )) ->
                    expressionContainsVariable variableToFind value
                )
                fields

        Expression.RecordUpdateExpression (Node _ recordVariable) setters ->
            (recordVariable == variableToFind)
                || List.any
                    (\(Node _ ( _, newValue )) ->
                        expressionContainsVariable variableToFind newValue
                    )
                    setters

        Expression.RecordAccess recordToAccess _ ->
            expressionContainsVariable variableToFind recordToAccess

        Expression.Application applicationElements ->
            List.any (\part -> expressionContainsVariable variableToFind part) applicationElements

        Expression.CaseExpression caseBlock ->
            expressionContainsVariable variableToFind caseBlock.expression
                || List.any
                    (\( _, caseExpression ) ->
                        expressionContainsVariable variableToFind caseExpression
                    )
                    caseBlock.cases

        Expression.OperatorApplication _ _ e1 e2 ->
            -- || split into if for TCO
            if expressionContainsVariable variableToFind e1 then
                True

            else
                expressionContainsVariable variableToFind e2

        Expression.IfBlock condition then_ else_ ->
            -- || split into if for TCO
            if
                expressionContainsVariable variableToFind condition
                    || expressionContainsVariable variableToFind then_
            then
                True

            else
                expressionContainsVariable variableToFind else_

        Expression.UnitExpr ->
            False

        Expression.Integer _ ->
            False

        Expression.Hex _ ->
            False

        Expression.Floatable _ ->
            False

        Expression.Literal _ ->
            False

        Expression.CharLiteral _ ->
            False

        Expression.GLSLExpression _ ->
            False

        Expression.RecordAccessFunction _ ->
            False

        Expression.FunctionOrValue _ _ ->
            False

        Expression.Operator _ ->
            False

        Expression.PrefixOperator _ ->
            False


{-| The sequence checks `sequenceOnCollectionWithKnownEmptyElementCheck` and `sequenceOnFromListWithEmptyIgnoresLaterElementsCheck`
-}
sequenceOrFirstEmptyChecks :
    ( TypeProperties (CollectionProperties (ConstructibleFromListProperties collectionOtherProperties))
    , EmptiableProperties empty (WrapperProperties elementOtherProperties)
    )
    -> CallCheckInfo
    -> Maybe (Error {})
sequenceOrFirstEmptyChecks ( collection, elementEmptiable ) checkInfo =
    sequenceOnCollectionWithKnownEmptyElementCheck ( collection, elementEmptiable ) checkInfo
        |> onNothing
            (\() ->
                sequenceOnFromListWithEmptyIgnoresLaterElementsCheck ( collection, elementEmptiable ) checkInfo
            )


{-| The sequence check

    sequence (construction fromList [ a, empty, b ])
    --> sequence (construction fromList [ a, empty ])

So for example

    Task.sequence [ aTask, Task.fail x, bTask ]
    --> Task.sequence [ aTask, Task.fail x ]

-}
sequenceOnFromListWithEmptyIgnoresLaterElementsCheck :
    ( TypeProperties (ConstructibleFromListProperties constructibleFromListOtherProperties), EmptiableProperties empty elementOtherProperties )
    -> CallCheckInfo
    -> Maybe (Error {})
sequenceOnFromListWithEmptyIgnoresLaterElementsCheck ( constructibleFromList, elementEmptiable ) checkInfo =
    case fromListGetLiteral constructibleFromList checkInfo.lookupTable checkInfo.firstArg of
        Just listLiteral ->
            case findMapNeighboring (\el -> getEmptyExpressionNode checkInfo elementEmptiable el) listLiteral.elements of
                Just emptyAndNeighbors ->
                    case emptyAndNeighbors.after of
                        Just _ ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " on a " ++ constructibleFromList.represents ++ " containing " ++ typeSubsetDescriptionIndefinite elementEmptiable.empty ++ " early will ignore later elements"
                                    , details = [ "You can remove all " ++ constructibleFromList.represents ++ " elements after " ++ typeSubsetDescriptionDefinite "the first" elementEmptiable.empty ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.removeRange
                                        { start = (Node.range emptyAndNeighbors.found).end
                                        , end = endWithoutBoundary listLiteral.literalRange
                                        }
                                    ]
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| The sequence check

    sequence (collection containing some wrapped elements, then empty, then other elements)
    --> empty

So for example

    Task.sequence [ Task.succeed a, Task.fail x, task ]
    --> Task.fail x

-}
sequenceOnCollectionWithKnownEmptyElementCheck :
    ( TypeProperties (CollectionProperties collectionOtherProperties), EmptiableProperties empty (WrapperProperties elementOtherProperties) )
    -> CallCheckInfo
    -> Maybe (Error {})
sequenceOnCollectionWithKnownEmptyElementCheck ( collection, elementEmptiable ) checkInfo =
    case collection.elements.get (extractNormalizeResources checkInfo) checkInfo.firstArg of
        Just elements ->
            case List.filter (\el -> isNothing (elementEmptiable.wrap.getValue checkInfo.lookupTable el)) elements.known of
                firstNonWrappedElement :: _ ->
                    if isInTypeSubset elementEmptiable.empty (extractNormalizeResources checkInfo) firstNonWrappedElement then
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " on a " ++ collection.represents ++ " containing " ++ typeSubsetDescriptionIndefinite elementEmptiable.empty ++ " will result in " ++ typeSubsetDescriptionDefinite "the first" elementEmptiable.empty
                                , details = [ "You can replace this call by " ++ typeSubsetDescriptionDefinite "the first" elementEmptiable.empty ++ " in the " ++ collection.represents ++ "." ]
                                }
                                checkInfo.fnRange
                                (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle firstNonWrappedElement)
                            )

                    else
                        Nothing

                [] ->
                    Nothing

        Nothing ->
            Nothing


listOfWrapperSequenceChecks : WrapperProperties (MappableProperties otherProperties) -> IntoFnCheck
listOfWrapperSequenceChecks wrapper =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (callOnEmptyReturnsCheck
                { resultAsString =
                    \res -> qualifiedToString (qualify wrapper.wrap.fn res) ++ " []"
                }
                listCollection
            )
        , onWrappedIsEquivalentToMapWrapOnValueCheck ( listCollection, wrapper )
        , intoFnCheckOnlyCall (sequenceOnCollectionWithAllElementsWrapped ( listCollection, wrapper ))
        ]


{-| The sequence check

    sequence (collection with each element being wrapped)
    --> wrap (collection with each value)

so for example

    Task.sequence [ Task.succeed a, Task.succeed b ]
    --> Task.succeed [ a, b ]

-}
sequenceOnCollectionWithAllElementsWrapped :
    ( TypeProperties (CollectionProperties otherCollectionProperties), WrapperProperties elementOtherProperties )
    -> CallCheckInfo
    -> Maybe (Error {})
sequenceOnCollectionWithAllElementsWrapped ( collection, elementWrapper ) checkInfo =
    case collection.elements.get (extractNormalizeResources checkInfo) checkInfo.firstArg of
        Just elements ->
            if elements.allKnown then
                case traverse (getValueWithNodeRange (elementWrapper.wrap.getValue checkInfo.lookupTable)) elements.known of
                    Just wrappeds ->
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " on a " ++ collection.represents ++ " where each element is " ++ constructWithOneValueDescriptionIndefinite elementWrapper.wrap.description ++ " will result in " ++ qualifiedToString elementWrapper.wrap.fn ++ " on the values inside"
                                , details = [ "You can replace this call by " ++ qualifiedToString elementWrapper.wrap.fn ++ " on a list where each element is replaced by its value inside " ++ constructWithOneValueDescriptionDefinite "the" elementWrapper.wrap.description ++ "." ]
                                }
                                checkInfo.fnRange
                                (Fix.replaceRangeBy
                                    checkInfo.fnRange
                                    (qualifiedToString (qualify elementWrapper.wrap.fn checkInfo))
                                    :: List.concatMap
                                        (\wrapped -> keepOnlyFix { parentRange = wrapped.nodeRange, keep = Node.range wrapped.value })
                                        wrappeds
                                )
                            )

                    Nothing ->
                        Nothing

            else
                Nothing

        Nothing ->
            Nothing


{-| The "sequenceRepeat" operation checks

    sequenceRepeat 0 wrapper --> wrap []

    sequenceRepeat 1 wrapper --> map List.singleton wrapper

    sequenceRepeat n (wrap a) --> wrap (List.repeat n a)

Examples of such functions:

    Random.list : Int -> Generator a -> Generator (List a)
    Parser.repeat : Int -> Parser a -> Parser (List a) -- by dasch

-}
sequenceRepeatChecks : WrapperProperties (MappableProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
sequenceRepeatChecks wrapper checkInfo =
    (case Normalize.getInt checkInfo checkInfo.firstArg of
        Just lengthInt ->
            case lengthInt of
                1 ->
                    let
                        replacement : QualifyResources res -> String
                        replacement res =
                            qualifiedToString (qualify wrapper.mapFn res)
                                ++ " "
                                ++ qualifiedToString (qualify Fn.List.singleton res)
                    in
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " 1 will result in " ++ replacement defaultQualifyResources
                            , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ "." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy
                                (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                (replacement checkInfo)
                            ]
                        )

                _ ->
                    callWithNonPositiveIntCheckErrorSituation { fn = checkInfo.fn, int = lengthInt, intDescription = "length" }
                        |> Maybe.map
                            (\situation ->
                                alwaysResultsInConstantError situation
                                    { replacement =
                                        \res -> qualifiedToString (qualify Fn.Random.constant res) ++ " []"
                                    , replacementNeedsParens = True
                                    }
                                    checkInfo
                            )

        Nothing ->
            Nothing
    )
        |> onNothing
            (\() ->
                case secondArg checkInfo of
                    Just elementArg ->
                        case AstHelpers.getSpecificUnreducedFnCall wrapper.wrap.fn checkInfo.lookupTable elementArg of
                            Just wrapCall ->
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " with " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with " ++ qualifiedToString Fn.List.repeat ++ " with the value in " ++ constructWithOneValueDescriptionDefinite "that" wrapper.wrap.description
                                        , details = [ "You can replace the call by " ++ qualifiedToString wrapper.wrap.fn ++ " with " ++ qualifiedToString Fn.List.repeat ++ " with the same length and the value inside " ++ constructWithOneValueDescriptionDefinite "the given" wrapper.wrap.description ++ "." ]
                                        }
                                        checkInfo.fnRange
                                        (Fix.replaceRangeBy checkInfo.fnRange
                                            (qualifiedToString (qualify Fn.List.repeat checkInfo))
                                            :: Fix.insertAt checkInfo.parentRange.start
                                                (qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " (")
                                            :: Fix.insertAt checkInfo.parentRange.end ")"
                                            :: replaceBySubExpressionFix wrapCall.nodeRange wrapCall.firstArg
                                        )
                                    )

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            )


{-| Checks for a repeat function that flattens the resulting list into the same type

    flatRepeat -1 emptiable --> empty

    flatRepeat n empty --> empty

    flatRepeat 1 emptiable --> emptiable

Examples of such functions:

    String.Graphemes.repeat : Int -> String -> String
    Animation.repeat : Int -> Animation.Step msg -> Animation.Step msg
    applyTimes : Int -> (a -> a) -> a -> a

-}
emptiableFlatRepeatChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
emptiableFlatRepeatChecks emptiable checkInfo =
    (case secondArg checkInfo of
        Just emptiableArg ->
            if emptiable.empty.specific.is (extractNormalizeResources checkInfo) emptiableArg then
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " with " ++ emptiable.empty.specific.description ++ " will result in " ++ emptiable.empty.specific.description
                        , details = [ "You can replace this call by " ++ emptiable.empty.specific.description ++ "." ]
                        }
                        checkInfo.fnRange
                        (keepOnlyFix
                            { parentRange = checkInfo.parentRange
                            , keep = Node.range emptiableArg
                            }
                        )
                    )

            else
                Nothing

        Nothing ->
            Nothing
    )
        |> onNothing
            (\() ->
                case Normalize.getInt checkInfo checkInfo.firstArg of
                    Just intValue ->
                        case intValue of
                            1 ->
                                Just
                                    (alwaysReturnsLastArgError (qualifiedToString checkInfo.fn ++ " 1")
                                        { represents = emptiable.represents ++ " to repeat" }
                                        checkInfo
                                    )

                            _ ->
                                callWithNonPositiveIntCanBeReplacedByCheck
                                    { int = intValue
                                    , intDescription = "length"
                                    , replacement = emptiable.empty.specific.asString
                                    }
                                    checkInfo

                    _ ->
                        Nothing
            )


{-| The Folding/reducing checks

    fold f initial empty
    --> initial

    fold (\_ soFar -> soFar) initial emptiable
    --> initial

which applies to for example

    Graph.fold : (NodeContext n e -> b -> b) -> b -> Graph n e -> b

but also functions like

    Either.foldl foldOnLeft initial (Either.Right r) --> initial

    Effects.apply f initial Effects.none

Any other argument order is not supported:

    Maybe.Extra.unwrap initial f Nothing
    -- not simplified

    Result.Extra.unwrap initial f (Err x)
    -- not simplified

    RemoteData.unwrap initial f (Err x)
    -- not simplified

If your fold function takes two arguments, use `emptiableFoldWithExtraArgChecks`

-}
emptiableFoldChecks :
    TypeProperties (EmptiableProperties empty otherProperties)
    -> CallCheckInfo
    -> Maybe (Error {})
emptiableFoldChecks emptiable checkInfo =
    foldToUnchangedAccumulatorCheck emptiable checkInfo
        |> onNothing (\() -> foldOnEmptyChecks emptiable checkInfo)


foldOnEmptyChecks : EmptiableProperties empty otherProperties -> CallCheckInfo -> Maybe (Error {})
foldOnEmptyChecks emptiable checkInfo =
    case checkInfo.argsAfterFirst of
        initialArg :: emptiableArg :: [] ->
            if isInTypeSubset emptiable.empty checkInfo emptiableArg then
                Just
                    (returnsArgError
                        (qualifiedToString checkInfo.fn ++ " on " ++ typeSubsetDescriptionIndefinite emptiable.empty)
                        { argRepresents = "initial accumulator"
                        , arg = initialArg
                        }
                        checkInfo
                    )

            else
                Nothing

        _ ->
            Nothing


foldToUnchangedAccumulatorCheck : TypeProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
foldToUnchangedAccumulatorCheck typeProperties checkInfo =
    case AstHelpers.getAlwaysResult checkInfo checkInfo.firstArg of
        Just reduceAlwaysResult ->
            if AstHelpers.isIdentity checkInfo reduceAlwaysResult then
                let
                    replacement : { description : String, fix : List Fix }
                    replacement =
                        case checkInfo.argsAfterFirst of
                            -- fold (\_ -> identity)
                            [] ->
                                { description = "`always` because the incoming accumulator will be returned, no matter which " ++ typeProperties.represents ++ " is supplied next"
                                , fix =
                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                        (qualifiedToString (qualify Fn.Basics.always checkInfo))
                                    ]
                                }

                            -- fold (\_ -> identity) initial
                            _ :: [] ->
                                { description = "`always` with the given initial accumulator"
                                , fix =
                                    [ Fix.replaceRangeBy
                                        (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                        (qualifiedToString (qualify Fn.Basics.always checkInfo))
                                    ]
                                }

                            -- fully applied
                            initialArg :: _ :: _ ->
                                { description = "the given initial accumulator"
                                , fix =
                                    replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle initialArg
                                }
                in
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " with a function that always returns the unchanged accumulator will result in the initial accumulator"
                        , details = [ "You can replace this call by " ++ replacement.description ++ "." ]
                        }
                        checkInfo.fnRange
                        replacement.fix
                    )

            else
                Nothing

        Nothing ->
            Nothing


{-| Folding/reducing checks with a reduce function that not only takes the current element but more information as an extra argument

    fold f initial empty --> initial

    fold (\_ _ soFar -> soFar) emptiable --> initial

which applies to for example

    Dict.foldl : (k -> v -> b -> b) -> b -> Dict.Dict k v -> b
    Graph.Tree.levelOrder : (l -> Forest l -> b -> b) -> b -> Tree l -> b

If your fold function does not have an extra arg, use `emptiableFoldChecks`.

-}
emptiableFoldWithExtraArgChecks :
    TypeProperties (EmptiableProperties empty otherProperties)
    -> CallCheckInfo
    -> Maybe (Error {})
emptiableFoldWithExtraArgChecks emptiable checkInfo =
    foldToUnchangedAccumulatorWithExtraArgCheck emptiable checkInfo
        |> onNothing (\() -> foldOnEmptyChecks emptiable checkInfo)


foldToUnchangedAccumulatorWithExtraArgCheck : TypeProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
foldToUnchangedAccumulatorWithExtraArgCheck typeProperties checkInfo =
    let
        maybeReduceFunctionResult : Maybe (Node Expression)
        maybeReduceFunctionResult =
            checkInfo.firstArg
                |> AstHelpers.getAlwaysResult checkInfo
                |> Maybe.andThen (\alwaysResult -> AstHelpers.getAlwaysResult checkInfo alwaysResult)
    in
    case maybeReduceFunctionResult of
        Just reduceAlwaysResult ->
            if AstHelpers.isIdentity checkInfo reduceAlwaysResult then
                let
                    replacement : { description : String, fix : List Fix }
                    replacement =
                        case checkInfo.argsAfterFirst of
                            -- fold (\_ -> identity)
                            [] ->
                                { description = "`always` because the incoming accumulator will be returned, no matter which " ++ typeProperties.represents ++ " is supplied next"
                                , fix =
                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                        (qualifiedToString (qualify Fn.Basics.always checkInfo))
                                    ]
                                }

                            -- fold (\_ -> identity) initial
                            _ :: [] ->
                                { description = "`always` with the given initial accumulator"
                                , fix =
                                    [ Fix.replaceRangeBy
                                        (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                        (qualifiedToString (qualify Fn.Basics.always checkInfo))
                                    ]
                                }

                            -- fully applied
                            initialArg :: _ :: _ ->
                                { description = "the given initial accumulator"
                                , fix =
                                    replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle initialArg
                                }
                in
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " with a function that always returns the unchanged accumulator will result in the initial accumulator"
                        , details = [ "You can replace this call by " ++ replacement.description ++ "." ]
                        }
                        checkInfo.fnRange
                        replacement.fix
                    )

            else
                Nothing

        Nothing ->
            Nothing


emptiableRepeatChecks : CollectionProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
emptiableRepeatChecks collection checkInfo =
    case Normalize.getInt checkInfo checkInfo.firstArg of
        Just intValue ->
            callWithNonPositiveIntCanBeReplacedByCheck
                { int = intValue
                , intDescription = collection.elements.countDescription
                , replacement = collection.empty.specific.asString
                }
                checkInfo

        Nothing ->
            Nothing


wrapperRepeatChecks : CollectionProperties (WrapperProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
wrapperRepeatChecks wrapper checkInfo =
    case Normalize.getInt checkInfo checkInfo.firstArg of
        Just 1 ->
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString checkInfo.fn ++ " with " ++ wrapper.elements.countDescription ++ " 1 will result in " ++ qualifiedToString wrapper.wrap.fn
                    , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ "." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy
                        (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                        (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                    ]
                )

        Just _ ->
            Nothing

        Nothing ->
            Nothing


getChecks : TypeProperties (CollectionProperties (EmptiableProperties empty otherProperties)) -> CallCheckInfo -> Maybe (Error {})
getChecks collection checkInfo =
    callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.specific.asString } collection checkInfo
        |> onNothing
            (\() ->
                Normalize.getInt checkInfo checkInfo.firstArg
                    |> Maybe.andThen (\index -> indexAccessChecks collection checkInfo index)
            )


indexAccessChecks : TypeProperties (CollectionProperties otherProperties) -> CallCheckInfo -> Int -> Maybe (Error {})
indexAccessChecks collection checkInfo n =
    if n < 0 then
        Just
            (alwaysResultsInUnparenthesizedConstantError (qualifiedToString checkInfo.fn ++ " with negative index")
                { replacement = maybeWithJustAsWrap.empty.specific.asString }
                checkInfo
            )

    else
        case secondArg checkInfo of
            Just arg ->
                (case collection.elements.get (extractNormalizeResources checkInfo) arg of
                    Just literalElements ->
                        case List.drop n literalElements.known |> List.head of
                            Just element ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "The element returned by " ++ qualifiedToString checkInfo.fn ++ " is known"
                                        , details = [ "You can replace this call by Just the targeted element." ]
                                        }
                                        checkInfo.fnRange
                                        (Fix.replaceRangeBy (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                            (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo))
                                            :: replaceBySubExpressionFix (Node.range arg) element
                                        )
                                    )

                            Nothing ->
                                if literalElements.allKnown then
                                    Just
                                        (Rule.errorWithFix
                                            { message = qualifiedToString checkInfo.fn ++ " with an index out of bounds of the given " ++ collection.represents ++ " will always return " ++ qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo)
                                            , details = [ "You can replace this call by Nothing." ]
                                            }
                                            checkInfo.fnRange
                                            [ Fix.replaceRangeBy checkInfo.parentRange (qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo)) ]
                                        )

                                else
                                    Nothing

                    Nothing ->
                        Nothing
                )
                    |> onNothing
                        (\() ->
                            AstHelpers.getSpecificUnreducedFnCall Fn.Array.repeat checkInfo.lookupTable arg
                                |> Maybe.andThen
                                    (\repeatCall ->
                                        List.head repeatCall.argsAfterFirst
                                            |> Maybe.andThen
                                                (\repeatSecondArg ->
                                                    case Normalize.getInt checkInfo repeatCall.firstArg of
                                                        Just repeatArgInt ->
                                                            if n < repeatArgInt then
                                                                Just
                                                                    (Rule.errorWithFix
                                                                        { message = "The element returned by " ++ qualifiedToString checkInfo.fn ++ " is known"
                                                                        , details = [ "You can replace this call by Just the repeated element." ]
                                                                        }
                                                                        checkInfo.fnRange
                                                                        [ Fix.replaceRangeBy checkInfo.parentRange
                                                                            (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo) ++ " " ++ checkInfo.extractSourceCode (Node.range repeatSecondArg))
                                                                        ]
                                                                    )

                                                            else
                                                                Just
                                                                    (Rule.errorWithFix
                                                                        { message = qualifiedToString checkInfo.fn ++ " with an index out of bounds of the given " ++ collection.represents ++ " will always return " ++ qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo)
                                                                        , details = [ "You can replace this call by Nothing." ]
                                                                        }
                                                                        checkInfo.fnRange
                                                                        [ Fix.replaceRangeBy checkInfo.parentRange
                                                                            (qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo))
                                                                        ]
                                                                    )

                                                        Nothing ->
                                                            Nothing
                                                )
                                    )
                        )
                    |> onNothing
                        (\() ->
                            AstHelpers.getSpecificUnreducedFnCall Fn.Array.initialize checkInfo.lookupTable arg
                                |> Maybe.andThen
                                    (\initializeCall ->
                                        List.head initializeCall.argsAfterFirst
                                            |> Maybe.andThen
                                                (\repeatSecondArg ->
                                                    case Normalize.getInt checkInfo initializeCall.firstArg of
                                                        Just initializeArgInt ->
                                                            if n < initializeArgInt then
                                                                Just
                                                                    (Rule.errorWithFix
                                                                        { message = "The element returned by " ++ qualifiedToString checkInfo.fn ++ " is known"
                                                                        , details = [ "You can replace this call by Just the function directly applied to the index." ]
                                                                        }
                                                                        checkInfo.fnRange
                                                                        [ Fix.replaceRangeBy checkInfo.parentRange
                                                                            (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo) ++ " (" ++ checkInfo.extractSourceCode (Node.range repeatSecondArg) ++ " " ++ String.fromInt n ++ ")")
                                                                        ]
                                                                    )

                                                            else
                                                                Just
                                                                    (Rule.errorWithFix
                                                                        { message = qualifiedToString checkInfo.fn ++ " with an index out of bounds of the given " ++ collection.represents ++ " will always return " ++ qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo)
                                                                        , details = [ "You can replace this call by Nothing." ]
                                                                        }
                                                                        checkInfo.fnRange
                                                                        [ Fix.replaceRangeBy checkInfo.parentRange
                                                                            (qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo))
                                                                        ]
                                                                    )

                                                        Nothing ->
                                                            Nothing
                                                )
                                    )
                        )

            Nothing ->
                Nothing


collectionSetChecks : TypeProperties (CollectionProperties (EmptiableProperties empty otherProperties)) -> IntoFnCheck
collectionSetChecks collection =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck collection
        , operationOverridesPreviousOperationWithEqualFirstArgCheck { firstArgDescription = "index" }
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case Normalize.getInt checkInfo checkInfo.firstArg of
                    Just n ->
                        if n < 0 then
                            Just
                                (alwaysReturnsLastArgError
                                    (qualifiedToString checkInfo.fn ++ " with negative index")
                                    collection
                                    checkInfo
                                )

                        else
                            case secondArg checkInfo of
                                Just replacementArg ->
                                    setOnKnownElementChecks collection checkInfo n (Node.range replacementArg)

                                Nothing ->
                                    Nothing

                    Nothing ->
                        Nothing
            )
        ]


{-| Checks that when the first arguments of consecutive calls of the same function are equal,
the effect of the earlier call gets "overwritten". For example

    array |> Array.set 1 0 |> Array.set 1 100
    --> array |> Array.set 1 100

Use `operationDoesNotChangeResultOfOperationCheck`
when _all arguments_ need to be the same, even if there is just one argument

-}
operationOverridesPreviousOperationWithEqualFirstArgCheck : { firstArgDescription : String } -> IntoFnCheck
operationOverridesPreviousOperationWithEqualFirstArgCheck config =
    { call =
        \checkInfo ->
            case
                Maybe.andThen (AstHelpers.getSpecificUnreducedFnCall checkInfo.fn checkInfo.lookupTable)
                    (fullyAppliedLastArg checkInfo)
            of
                Nothing ->
                    Nothing

                Just lastArgOperationCall ->
                    case
                        fullyAppliedLastArg
                            { firstArg = lastArgOperationCall.firstArg
                            , argsAfterFirst = lastArgOperationCall.argsAfterFirst
                            , argCount = checkInfo.argCount
                            }
                    of
                        Nothing ->
                            Nothing

                        Just lastArgOperationCallLastArg ->
                            case Normalize.compare checkInfo checkInfo.firstArg lastArgOperationCall.firstArg of
                                Normalize.ConfirmedEquality ->
                                    Just
                                        (Rule.errorWithFix
                                            (operationOverridesPreviousOperationWithEqualFirstArgErrorInfo config checkInfo.fn)
                                            checkInfo.fnRange
                                            (replaceBySubExpressionFix lastArgOperationCall.nodeRange
                                                lastArgOperationCallLastArg
                                            )
                                        )

                                _ ->
                                    Nothing
    , composition =
        \checkInfo ->
            if
                onlyLastArgIsCurried checkInfo.later
                    && (checkInfo.earlier.fn == checkInfo.later.fn)
            then
                case checkInfo.later.args of
                    laterFirstArg :: _ ->
                        case checkInfo.earlier.args of
                            earlierFirstArg :: _ ->
                                case Normalize.compare checkInfo laterFirstArg earlierFirstArg of
                                    Normalize.ConfirmedEquality ->
                                        Just
                                            { info = operationOverridesPreviousOperationWithEqualFirstArgErrorInfo config checkInfo.later.fn
                                            , fix = [ Fix.removeRange checkInfo.earlier.removeRange ]
                                            }

                                    _ ->
                                        Nothing

                            [] ->
                                Nothing

                    [] ->
                        Nothing

            else
                Nothing
    }


operationOverridesPreviousOperationWithEqualFirstArgErrorInfo : { firstArgDescription : String } -> ( ModuleName, String ) -> { message : String, details : List String }
operationOverridesPreviousOperationWithEqualFirstArgErrorInfo config fn =
    { message =
        qualifiedToString fn
            ++ " on "
            ++ qualifiedToString fn
            ++ " with the same "
            ++ config.firstArgDescription
            ++ " makes the earlier operation unnecessary"
    , details = [ "You can remove the earlier operation." ]
    }


setOnKnownElementChecks :
    TypeProperties (CollectionProperties otherProperties)
    -> CallCheckInfo
    -> Int
    -> Range
    -> Maybe (Error {})
setOnKnownElementChecks collection checkInfo n replacementArgRange =
    case thirdArg checkInfo of
        Just collectionArg ->
            case collection.elements.get (extractNormalizeResources checkInfo) collectionArg of
                Just literalElements ->
                    case List.drop n literalElements.known |> List.head of
                        Just element ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " will replace a known element"
                                    , details = [ "You can directly replace the element at the given index in the " ++ collection.represents ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy (Node.range element) (checkInfo.extractSourceCode replacementArgRange)
                                        :: keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range collectionArg }
                                    )
                                )

                        Nothing ->
                            if literalElements.allKnown then
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " with an index out of bounds of the given " ++ collection.represents ++ " will always return the same given " ++ collection.represents
                                        , details = [ "You can replace this call by the given " ++ collection.represents ++ "." ]
                                        }
                                        checkInfo.fnRange
                                        (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle collectionArg)
                                    )

                            else
                                Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


emptiableReverseChecks : EmptiableProperties empty otherProperties -> IntoFnCheck
emptiableReverseChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck emptiable
        , toggleFnChecks
        ]


{-| The drop check

    drop n (collection with size <= n) --> empty

So for example

    List.drop 3 [ a, b ] --> []

-}
dropOnSmallerCollectionCheck : { dropCount : Int } -> TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties otherProperties)) -> CallCheckInfo -> Maybe (Error {})
dropOnSmallerCollectionCheck config collection checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just listArg ->
            case (collection.elements.determineCount (extractNormalizeResources checkInfo) listArg).max of
                Nothing ->
                    Nothing

                Just elementCountMax ->
                    if config.dropCount >= elementCountMax then
                        Just
                            (alwaysResultsInUnparenthesizedConstantError
                                (qualifiedToString checkInfo.fn ++ " with a count greater than or equal to the given " ++ collection.represents ++ "'s length")
                                { replacement = collection.empty.specific.asString }
                                checkInfo
                            )

                    else
                        Nothing

        Nothing ->
            Nothing


{-| The drop check

    drop n (fromList on list with size > n)
    --> (fromList on list with the first n elements removed)

So for example

    Array.drop 2 (Array.fromList [ a, b, c ])
    --> Array.fromList [ c ]

-}
dropOnLargerConstructionFromListLiteralWillRemoveTheseElementsCheck : { dropCount : Int } -> TypeProperties (ConstructibleFromListProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
dropOnLargerConstructionFromListLiteralWillRemoveTheseElementsCheck config constructibleFromList checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just lastArg ->
            case fromListGetLiteral constructibleFromList checkInfo.lookupTable lastArg of
                Just fromListLiteral ->
                    case List.drop config.dropCount fromListLiteral.elements of
                        (Node elementAfterDroppedRange _) :: _ ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " with a count less than the given " ++ constructibleFromList.represents ++ "'s length will remove these elements"
                                    , details = [ "You can remove the first " ++ String.fromInt config.dropCount ++ " elements from the " ++ constructionFromListOnLiteralDescription constructibleFromList.fromList ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.removeRange
                                        { start = startWithoutBoundary fromListLiteral.literalRange
                                        , end = elementAfterDroppedRange.start
                                        }
                                        :: keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range lastArg }
                                    )
                                )

                        [] ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


emptiableMapNChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
emptiableMapNChecks emptiable checkInfo =
    if List.any (\emptiableArg -> isInTypeSubset emptiable.empty checkInfo emptiableArg) checkInfo.argsAfterFirst then
        Just
            (alwaysResultsInUnparenthesizedConstantError
                (qualifiedToString checkInfo.fn ++ " with any " ++ emptiable.represents ++ " being " ++ emptiable.empty.specific.description)
                { replacement = emptiable.empty.specific.asString }
                checkInfo
            )

    else
        Nothing


{-| When all arguments of a fully applied `mapN` are wrapped,
apply the given function to the values inside and wrap the whole thing again:

    map2 f (wrap first) (wrap second)
    --> wrap (f first second)

For example given `resultWithOkAsWrap`:

    Result.map2 f (Ok first) (Ok second)
    --> Ok (f first second)

This is pretty similar to `listOfWrapperSequenceChecks` where we look at arguments instead of list elements.

-}
wrapperMapNChecks : TypeProperties (WrapperProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
wrapperMapNChecks wrapper checkInfo =
    if List.length checkInfo.argsAfterFirst == (checkInfo.argCount - 1) then
        -- fully applied
        case traverse (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) checkInfo.argsAfterFirst of
            Just wraps ->
                let
                    wrapFnDescription : String
                    wrapFnDescription =
                        qualifiedToString (qualify wrapper.wrap.fn defaultQualifyResources)
                in
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " where each " ++ wrapper.represents ++ " is " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will result in " ++ wrapFnDescription ++ " on the values inside"
                        , details = [ "You can replace this call by " ++ wrapFnDescription ++ " with the function applied to the values inside each " ++ constructWithOneValueDescriptionWithoutArticle wrapper.wrap.description ++ "." ]
                        }
                        checkInfo.fnRange
                        ((case checkInfo.callStyle of
                            CallStyle.Pipe CallStyle.LeftToRight ->
                                [ Fix.insertAt checkInfo.parentRange.end
                                    (" |> " ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                ]

                            CallStyle.Pipe CallStyle.RightToLeft ->
                                [ Fix.insertAt checkInfo.parentRange.start
                                    (qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " <| ")
                                ]

                            CallStyle.Application ->
                                [ Fix.insertAt checkInfo.parentRange.end ")"
                                , Fix.insertAt checkInfo.parentRange.start (qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " (")
                                ]
                         )
                            ++ keepOnlyFix
                                { parentRange = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                , keep = Node.range checkInfo.firstArg
                                }
                            ++ List.concatMap (\wrap -> replaceBySubExpressionFix wrap.nodeRange wrap.value) wraps
                        )
                    )

            Nothing ->
                Nothing

    else
        Nothing


{-| If we find an empty argument given to the `mapN`, we either

  - replace the whole call by the first empty argument if all earlier arguments are wrapped

        map3 f (wrap first) empty thirdWrapper
        --> empty

        map2 f empty secondWrapper
        --> empty

    For example given `resultWithOkAsWrap`:

        Result.map3 f (Ok x) (Err y) thirdResult
        --> Err y

  - ignore arguments after the known empty argument because they will never have an effect on the result

        map3 f emptyOrWrappedWeDoNotKnow empty thirdWrapper
        --> map2 f emptyOrWrappedWeDoNotKnow empty

    For example given `resultWithOkAsWrap`:

        Result.map3 f errorOrOkWeDoNotKnow (Err x) thirdResult
        --> Result.map2 f errorOrOkWeDoNotKnow (Err x)

This is pretty similar to `listSequenceOrFirstEmptyChecks` where we look at arguments instead of list elements.

-}
mapNOrFirstEmptyConstructionChecks :
    WrapperProperties (EmptiableProperties empty otherProperties)
    -> CallCheckInfo
    -> Maybe (Error {})
mapNOrFirstEmptyConstructionChecks emptiable checkInfo =
    case findMapAndAllBefore (getEmptyExpressionNode checkInfo emptiable) checkInfo.argsAfterFirst of
        -- no empty arg found
        Nothing ->
            Nothing

        Just emptyAndBefore ->
            if List.all (\el -> isJust (emptiable.wrap.getValue checkInfo.lookupTable el)) emptyAndBefore.before then
                -- all args before are known to not be empty
                let
                    replacement : { description : String, fix : List Fix }
                    replacement =
                        case checkInfo.argCount - (1 + List.length checkInfo.argsAfterFirst) of
                            -- fully applied
                            0 ->
                                { description = typeSubsetDescriptionDefinite "the first" emptiable.empty
                                , fix = replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle emptyAndBefore.found
                                }

                            -- one arg curried
                            1 ->
                                { description =
                                    "always with " ++ typeSubsetDescriptionDefinite "the first" emptiable.empty
                                , fix =
                                    Fix.insertAt checkInfo.parentRange.start (qualifiedToString (qualify Fn.Basics.always checkInfo) ++ " ")
                                        :: replaceBySubExpressionFix checkInfo.parentRange emptyAndBefore.found
                                }

                            -- multiple args curried
                            atLeast2 ->
                                let
                                    lambdaStart : String
                                    lambdaStart =
                                        "\\" ++ String.repeat atLeast2 "_ " ++ "-> "
                                in
                                { description =
                                    lambdaStart ++ "with " ++ typeSubsetDescriptionDefinite "the first" emptiable.empty
                                , fix =
                                    Fix.insertAt checkInfo.parentRange.start ("(" ++ lambdaStart)
                                        :: Fix.insertAt checkInfo.parentRange.end ")"
                                        :: keepOnlyFix
                                            { parentRange = checkInfo.parentRange
                                            , keep = Node.range (AstHelpers.removeParens emptyAndBefore.found)
                                            }
                                }
                in
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " where we know " ++ typeSubsetDescriptionDefinite "the first" emptiable.empty ++ " will result in " ++ typeSubsetDescriptionDefinite "that" emptiable.empty
                        , details = [ "You can replace this call by " ++ replacement.description ++ "." ]
                        }
                        checkInfo.fnRange
                        replacement.fix
                    )

            else
                -- some args before could be empty
                let
                    keptArgCount : Int
                    keptArgCount =
                        List.length emptyAndBefore.before + 1
                in
                if keptArgCount == (checkInfo.argCount - 1) then
                    -- last arg is empty
                    Nothing

                else
                    -- there are args (curried or present) after the known empty arg
                    let
                        replacementMap : ( ModuleName, String )
                        replacementMap =
                            ( AstHelpers.qualifiedModuleName checkInfo.fn, "map" ++ String.fromInt keptArgCount )

                        keptRange : Range
                        keptRange =
                            Range.combine
                                (checkInfo.fnRange
                                    :: Node.range emptyAndBefore.found
                                    :: List.map Node.range emptyAndBefore.before
                                )

                        replacement : { description : String, fix : List Fix }
                        replacement =
                            case checkInfo.argCount - (1 + List.length checkInfo.argsAfterFirst) of
                                -- fully applied
                                0 ->
                                    { fix =
                                        [ Fix.removeRange
                                            { start = keptRange.end, end = checkInfo.parentRange.end }
                                        , Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify replacementMap checkInfo))
                                        , Fix.removeRange
                                            { start = checkInfo.parentRange.start, end = keptRange.start }
                                        ]
                                    , description =
                                        qualifiedToString replacementMap ++ " with the same arguments until " ++ typeSubsetDescriptionDefinite "the first" emptiable.empty
                                    }

                                -- one arg curried
                                1 ->
                                    { fix =
                                        [ Fix.replaceRangeBy
                                            { start = keptRange.end, end = checkInfo.parentRange.end }
                                            ")"
                                        , Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify replacementMap checkInfo))
                                        , Fix.replaceRangeBy
                                            { start = checkInfo.parentRange.start
                                            , end = keptRange.start
                                            }
                                            (qualifiedToString (qualify Fn.Basics.always checkInfo) ++ " (")
                                        ]
                                    , description =
                                        "always with " ++ qualifiedToString replacementMap ++ " with the same arguments until " ++ typeSubsetDescriptionDefinite "the first" emptiable.empty
                                    }

                                -- multiple args curried
                                atLeast2 ->
                                    let
                                        lambdaStart : String
                                        lambdaStart =
                                            "\\" ++ String.repeat atLeast2 "_ " ++ "-> "
                                    in
                                    { fix =
                                        [ Fix.replaceRangeBy
                                            { start = keptRange.end, end = checkInfo.parentRange.end }
                                            ")"
                                        , Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify replacementMap checkInfo))
                                        , Fix.replaceRangeBy
                                            { start = checkInfo.parentRange.start, end = keptRange.start }
                                            ("(" ++ lambdaStart)
                                        ]
                                    , description =
                                        lambdaStart ++ "with " ++ qualifiedToString replacementMap ++ " with the same arguments until " ++ typeSubsetDescriptionDefinite "the first" emptiable.empty
                                    }
                    in
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " with " ++ typeSubsetDescriptionIndefinite emptiable.empty ++ " early will ignore later arguments"
                            , details = [ "You can replace this call by " ++ replacement.description ++ "." ]
                            }
                            checkInfo.fnRange
                            replacement.fix
                        )


emptiableWrapperFilterMapChecks : TypeProperties (WrapperProperties (EmptiableProperties ConstantProperties (MappableProperties otherProperties))) -> IntoFnCheck
emptiableWrapperFilterMapChecks emptiableWrapper =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                case
                    checkInfo.firstArg
                        |> toConstructedResult checkInfo.lookupTable
                        |> Maybe.andThen
                            (\construct ->
                                sameInAllBranches
                                    (\branch ->
                                        AstHelpers.getSpecificUnreducedFnCall Fn.Maybe.justVariant checkInfo.lookupTable branch
                                    )
                                    construct
                            )
                of
                    Just justCalls ->
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " with a function that will always return Just is the same as " ++ qualifiedToString emptiableWrapper.mapFn
                                , details = [ "You can remove the `Just`s and replace the call by " ++ qualifiedToString emptiableWrapper.mapFn ++ "." ]
                                }
                                checkInfo.fnRange
                                (Fix.replaceRangeBy checkInfo.fnRange
                                    (qualifiedToString (qualify emptiableWrapper.mapFn checkInfo))
                                    :: List.concatMap (\call -> replaceBySubExpressionFix call.nodeRange call.firstArg) justCalls
                                )
                            )

                    Nothing ->
                        Nothing
            )
        , intoFnCheckOnlyCall
            (\checkInfo ->
                if AstHelpers.isSpecificValueOrFn Fn.Maybe.justVariant checkInfo checkInfo.firstArg then
                    Just
                        (alwaysReturnsLastArgError
                            (qualifiedToString checkInfo.fn ++ " with a function that will always return Just")
                            emptiableWrapper
                            checkInfo
                        )

                else
                    Nothing
            )
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case toConstructedResult checkInfo.lookupTable checkInfo.firstArg of
                    Just constructed ->
                        if
                            trueInAllBranches
                                (\branch ->
                                    AstHelpers.isSpecificValueReference checkInfo.lookupTable Fn.Maybe.nothingVariant branch
                                )
                                constructed
                        then
                            Just
                                (alwaysResultsInUnparenthesizedConstantError
                                    (qualifiedToString checkInfo.fn ++ " with a function that will always return Nothing")
                                    { replacement = emptiableWrapper.empty.specific.asString }
                                    checkInfo
                                )

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )
        , mapToOperationWithIdentityCanBeCombinedToOperationChecks emptiableWrapper
        , unnecessaryOnEmptyCheck emptiableWrapper
        ]


maybeNothingProperties :
    { description : String
    , is : { isRes | lookupTable : ModuleNameLookupTable } -> Node Expression -> Bool
    }
maybeNothingProperties =
    { description = "Nothing"
    , is =
        \res expr ->
            AstHelpers.isSpecificValueReference res.lookupTable Fn.Maybe.nothingVariant expr
    }


identityFunctionProperties :
    { description : String
    , is : AstHelpers.ReduceLambdaResources isRes -> Node Expression -> Bool
    , asString : QualifyResources asStringRes -> String
    , fn : ( ModuleName, String )
    }
identityFunctionProperties =
    { description = "an identity function"
    , is = \res expr -> AstHelpers.isIdentity res expr
    , asString = \res -> qualifiedToString (qualify Fn.Basics.identity res)
    , fn = Fn.Basics.identity
    }


tupleFirstAccessFunctionProperties :
    { description : String
    , is : AstHelpers.ReduceLambdaResources isRes -> Node Expression -> Bool
    , asString : QualifyResources asStringRes -> String
    , fn : ( ModuleName, String )
    }
tupleFirstAccessFunctionProperties =
    { description = "Tuple.first"
    , fn = Fn.Tuple.first
    , is = \res expr -> AstHelpers.isTupleFirstAccess res expr
    , asString = \res -> qualifiedToString (qualify Fn.Tuple.first res)
    }


tupleSecondAccessFunctionProperties :
    { description : String
    , is : AstHelpers.ReduceLambdaResources isRes -> Node Expression -> Bool
    , asString : QualifyResources asStringRes -> String
    , fn : ( ModuleName, String )
    }
tupleSecondAccessFunctionProperties =
    { description = "Tuple.second"
    , fn = Fn.Tuple.second
    , is = \res expr -> AstHelpers.isTupleSecondAccess res expr
    , asString = \res -> qualifiedToString (qualify Fn.Tuple.second res)
    }


mapToOperationWithIdentityCanBeCombinedToOperationChecks : MappableProperties otherProperties -> IntoFnCheck
mapToOperationWithIdentityCanBeCombinedToOperationChecks mappable =
    let
        checkIntoFn : ( ModuleName, String ) -> IntoFnCheck
        checkIntoFn combinedFn =
            onSpecificFnCallCanBeCombinedCheck
                { args = [ identityFunctionProperties ]
                , earlierFn = mappable.mapFn
                , combinedFn = combinedFn
                }
    in
    { call = \checkInfo -> (checkIntoFn checkInfo.fn).call checkInfo
    , composition = \checkInfo -> (checkIntoFn checkInfo.later.fn).composition checkInfo
    }


{-| Simplify this operation after a given call to `earlierFn` into a given `combinedFn`.

Examples:

  - `List.concat (List.map f list) --> List.concatMap f list` (same for sequence+map to traverse etc)
  - `List.concat << List.map f --> List.concatMap f`
  - `Parser/Decoder/Random/...sequence (List.repeat n x) --> Parser/Decoder/Random/...repeat n x`
  - `String.concat (List.repeat n x) --> String.repeat n x`
  - `Animation.loop (List.repeat n x) --> Animation.repeat n x` using [`mdgriffith/elm-style-animation`](https://package.elm-lang.org/packages/mdgriffith/elm-style-animation/4.0.0/)
  - `FormattedText.concat (List.intersperse s list) --> FormattedText.join s list` using [`NoRedInk/elm-formatted-text-19`](https://package.elm-lang.org/packages/NoRedInk/elm-formatted-text-19/1.0.0/)
  - `List.filterMap identity (List.map f list) --> List.filterMap f list`
  - `List.filterMap identity << List.map f --> List.filterMap f`
  - `traverse identity (map f a) --> traverse f a`

-}
onSpecificFnCallCanBeCombinedCheck :
    { args :
        List
            { argProperties
                | description : String
                , is : AstHelpers.ReduceLambdaResources {} -> Node Expression -> Bool
            }
    , earlierFn : ( ModuleName, String )
    , combinedFn : ( ModuleName, String )
    }
    -> IntoFnCheck
onSpecificFnCallCanBeCombinedCheck config =
    let
        laterOperationDescription : ( ModuleName, String ) -> String
        laterOperationDescription laterFn =
            case config.args of
                [] ->
                    qualifiedToString (qualify laterFn defaultQualifyResources)

                firstArg :: argsAfterFirst ->
                    qualifiedToString (qualify laterFn defaultQualifyResources)
                        ++ " with "
                        ++ firstArg.description
                        ++ String.concat (List.map (\arg -> " and " ++ arg.description) argsAfterFirst)

        laterInitArgsAreValidForOperation : List (Node Expression) -> AstHelpers.ReduceLambdaResources res -> Bool
        laterInitArgsAreValidForOperation laterInitArgs resources =
            let
                extractedResources : AstHelpers.ReduceLambdaResources {}
                extractedResources =
                    extractReduceLambdaResources resources
            in
            listAll2
                (\arg argConfig -> argConfig.is extractedResources arg)
                laterInitArgs
                config.args
    in
    { call =
        \checkInfo ->
            case
                Maybe.andThen (\lastArg -> AstHelpers.getSpecificUnreducedFnCall config.earlierFn checkInfo.lookupTable lastArg)
                    (fullyAppliedLastArg checkInfo)
            of
                Just fromFnCall ->
                    if laterInitArgsAreValidForOperation (listFilledInit ( checkInfo.firstArg, checkInfo.argsAfterFirst )) checkInfo then
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString config.earlierFn ++ ", then " ++ laterOperationDescription checkInfo.fn ++ " can be combined into " ++ qualifiedToString config.combinedFn
                                , details =
                                    [ "You can replace this call by "
                                        ++ qualifiedToString config.combinedFn
                                        ++ " with the same "
                                        ++ (case fromFnCall.argsAfterFirst of
                                                [] ->
                                                    "argument"

                                                _ :: _ ->
                                                    "arguments"
                                           )
                                        ++ " given to "
                                        ++ qualifiedToString config.earlierFn
                                        ++ " which is meant for this exact purpose."
                                    ]
                                }
                                checkInfo.fnRange
                                (Fix.replaceRangeBy
                                    fromFnCall.fnRange
                                    (qualifiedToString (qualify config.combinedFn checkInfo))
                                    :: keepOnlyFix { parentRange = checkInfo.parentRange, keep = fromFnCall.nodeRange }
                                )
                            )

                    else
                        Nothing

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if
                onlyLastArgIsCurried checkInfo.later
                    && (checkInfo.earlier.fn == config.earlierFn)
                    && laterInitArgsAreValidForOperation checkInfo.later.args checkInfo
            then
                Just
                    { info =
                        { message = qualifiedToString config.earlierFn ++ ", then " ++ laterOperationDescription checkInfo.later.fn ++ " can be combined into " ++ qualifiedToString config.combinedFn
                        , details =
                            [ "You can replace this composition by "
                                ++ qualifiedToString config.combinedFn
                                ++ (case checkInfo.earlier.args of
                                        [] ->
                                            ""

                                        [ _ ] ->
                                            " with the same argument given to " ++ qualifiedToString config.earlierFn

                                        _ :: _ :: _ ->
                                            " with the same arguments given to " ++ qualifiedToString config.earlierFn
                                   )
                                ++ " which is meant for this exact purpose."
                            ]
                        }
                    , fix =
                        [ Fix.replaceRangeBy
                            checkInfo.earlier.fnRange
                            (qualifiedToString (qualify config.combinedFn checkInfo))
                        , Fix.removeRange checkInfo.later.removeRange
                        ]
                    }

            else
                Nothing
    }


{-| The check

    operation (wrap a) --> map wrap a

    operation << wrap --> map wrap

So for example

    Task.sequence [ task ]
    --: Task x (List a)
    --> Task.map List.singleton task

    Task.sequence << List.singleton
    --: Task x a -> Task x (List a)
    --> Task.map List.singleton

Note that some functions called "sequence" have equal element and result types, like

    Bytes.Encode.sequence  [ encoder ]
    --: Bytes.Encode.Encoder

    Bytes.Encode.sequence << List.singleton
    --: Bytes.Encode.Encoder -> Bytes.Encode.Encoder

which means you can simplify them to `encoder`/`identity` using `unnecessaryOnWrappedCheck`.

-}
onWrappedIsEquivalentToMapWrapOnValueCheck :
    ( WrapperProperties wrapperOtherProperties, WrapperProperties (MappableProperties elementOtherProperties) )
    -> IntoFnCheck
onWrappedIsEquivalentToMapWrapOnValueCheck ( wrapper, valueMappable ) =
    { call =
        \checkInfo ->
            case fullyAppliedLastArg checkInfo of
                Just lastArg ->
                    case wrapper.wrap.getValue checkInfo.lookupTable lastArg of
                        Just wrappedValue ->
                            let
                                replacement : QualifyResources a -> String
                                replacement qualifyResources =
                                    qualifiedToString (qualify valueMappable.mapFn qualifyResources)
                                        ++ " "
                                        ++ qualifiedToString (qualify wrapper.wrap.fn qualifyResources)
                            in
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " is the same as " ++ replacement defaultQualifyResources ++ " on the value inside"
                                    , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ " on the value inside the singleton list." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy checkInfo.fnRange
                                        (replacement checkInfo)
                                        :: replaceBySubExpressionFix (Node.range lastArg) wrappedValue
                                    )
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == wrapper.wrap.fn) then
                let
                    replacement : QualifyResources a -> String
                    replacement qualifyResources =
                        qualifiedToString (qualify valueMappable.mapFn qualifyResources)
                            ++ " "
                            ++ qualifiedToString (qualify wrapper.wrap.fn qualifyResources)
                in
                Just
                    { info =
                        { message = qualifiedToString checkInfo.later.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " is the same as " ++ replacement defaultQualifyResources ++ " on the value inside"
                        , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ "." ]
                        }
                    , fix = compositionReplaceByFix (replacement checkInfo) checkInfo
                    }

            else
                Nothing
    }


pipelineChecks :
    { commentRanges : List Range
    , extractSourceCode : Range -> String
    , nodeRange : Range
    , pipedInto : Node Expression
    , arg : Node Expression
    , direction : CallStyle.LeftOrRightDirection
    , importRecordTypeAliases : Dict ModuleName (Dict String (List String))
    , moduleRecordTypeAliases : Dict String (List String)
    , importCustomTypes :
        Dict
            ModuleName
            (Dict
                String
                { variantNames : Set String
                , allParametersAreUsedInVariants : Bool
                }
            )
    , moduleCustomTypes :
        Dict
            String
            { variantNames : Set String
            , allParametersAreUsedInVariants : Bool
            }
    , lookupTable : ModuleNameLookupTable
    }
    -> Maybe (Error {})
pipelineChecks checkInfo =
    callingCompositionChecks checkInfo (always []) checkInfo.pipedInto
        |> onNothing
            (\() ->
                reversedCompositionChecks checkInfo (\() -> []) checkInfo.pipedInto
            )
        |> onNothing
            (\() ->
                fullyAppliedLambdaInPipelineChecks
                    { nodeRange = checkInfo.nodeRange
                    , function = checkInfo.pipedInto
                    , firstArgument = checkInfo.arg
                    , lookupTable = checkInfo.lookupTable
                    , importCustomTypes = checkInfo.importCustomTypes
                    , moduleCustomTypes = checkInfo.moduleCustomTypes
                    }
            )
        |> onNothing
            (\() ->
                case AstHelpers.getRecordAccessFunction checkInfo.pipedInto of
                    Just fieldName ->
                        accessingRecordChecks
                            { parentRange = checkInfo.nodeRange
                            , record = checkInfo.arg
                            , fieldRange = Node.range checkInfo.pipedInto
                            , fieldName = fieldName
                            , importRecordTypeAliases = checkInfo.importRecordTypeAliases
                            , moduleRecordTypeAliases = checkInfo.moduleRecordTypeAliases
                            , importCustomTypes = checkInfo.importCustomTypes
                            , moduleCustomTypes = checkInfo.moduleCustomTypes
                            , lookupTable = checkInfo.lookupTable
                            }
                            |> Maybe.map (\e -> Rule.errorWithFix e.info (Node.range checkInfo.pipedInto) e.fix)

                    Nothing ->
                        Nothing
            )


fullyAppliedLambdaInPipelineChecks : AstHelpers.ReduceLambdaResources { nodeRange : Range, firstArgument : Node Expression, function : Node Expression } -> Maybe (Error {})
fullyAppliedLambdaInPipelineChecks checkInfo =
    case Node.value checkInfo.function of
        Expression.ParenthesizedExpression ((Node _ (Expression.LambdaExpression lambda)) as lambdaNode) ->
            appliedLambdaError
                { lambda = lambda
                , lambdaNode = lambdaNode
                , lambdaWithParens = Node.range checkInfo.function
                , firstArgument = checkInfo.firstArgument
                , argsAfterFirst = []
                , lookupTable = checkInfo.lookupTable
                , importCustomTypes = checkInfo.importCustomTypes
                , moduleCustomTypes = checkInfo.moduleCustomTypes
                }

        Expression.Application ((Node _ (Expression.ParenthesizedExpression ((Node _ (Expression.LambdaExpression lambda)) as lambdaNode))) :: firstArgument :: restOfApplicationArgs) ->
            appliedLambdaError
                { lambda = lambda
                , lambdaNode = lambdaNode
                , lambdaWithParens = Node.range checkInfo.function
                , firstArgument = firstArgument
                , argsAfterFirst = restOfApplicationArgs ++ [ checkInfo.firstArgument ]
                , lookupTable = checkInfo.lookupTable
                , importCustomTypes = checkInfo.importCustomTypes
                , moduleCustomTypes = checkInfo.moduleCustomTypes
                }

        _ ->
            Nothing


callingCompositionChecks :
    { context
        | commentRanges : List Range
        , extractSourceCode : Range -> String
        , direction : CallStyle.LeftOrRightDirection
    }
    -> (() -> List Fix)
    -> Node Expression
    -> Maybe (Error {})
callingCompositionChecks context fixesFromParent expressionNode =
    let
        targetAndReplacement : { opToFind : String, replacement : String }
        targetAndReplacement =
            case context.direction of
                CallStyle.RightToLeft ->
                    { opToFind = "<<", replacement = "<|" }

                CallStyle.LeftToRight ->
                    { opToFind = ">>", replacement = "|>" }
    in
    case pipingIntoCompositionChecksHelp context targetAndReplacement expressionNode of
        Nothing ->
            Nothing

        Just error ->
            Just
                (Rule.errorWithFix
                    { message = "Use " ++ targetAndReplacement.replacement ++ " instead of " ++ targetAndReplacement.opToFind
                    , details =
                        [ "Because of the precedence of operators, using " ++ targetAndReplacement.opToFind ++ " at this location is the same as using " ++ targetAndReplacement.replacement ++ "."
                        , "To make it more idiomatic in Elm and generally easier to read, please use " ++ targetAndReplacement.replacement ++ " instead. You may need to remove some parentheses to do this."
                        , "Here is an example:"
                            ++ (case context.direction of
                                    CallStyle.RightToLeft ->
                                        leftPipeExample

                                    CallStyle.LeftToRight ->
                                        rightPipeExample
                               )
                        ]
                    }
                    error.opToReplaceRange
                    (fixesFromParent () ++ error.fixes)
                )


pipingIntoCompositionChecksHelp :
    { context
        | commentRanges : List Range
        , extractSourceCode : Range -> String
        , direction : CallStyle.LeftOrRightDirection
    }
    -> { opToFind : String, replacement : String }
    -> Node Expression
    -> Maybe { opToReplaceRange : Range, fixes : List Fix, firstStepIsComposition : Bool }
pipingIntoCompositionChecksHelp context targetAndReplacement subExpression =
    case Node.value subExpression of
        Expression.ParenthesizedExpression inParens ->
            case pipingIntoCompositionChecksHelp context targetAndReplacement inParens of
                Nothing ->
                    Nothing

                Just error ->
                    if error.firstStepIsComposition then
                        -- parens can safely be removed
                        Just
                            { opToReplaceRange = error.opToReplaceRange
                            , firstStepIsComposition = error.firstStepIsComposition
                            , fixes =
                                removeBoundariesFix subExpression ++ error.fixes
                            }

                    else
                        -- inside parenthesis is checked separately because
                        -- the parens here can't safely be removed
                        Nothing

        Expression.OperatorApplication symbol _ left right ->
            let
                continuedSearch : Maybe { opToReplaceRange : Range, fixes : List Fix, firstStepIsComposition : Bool }
                continuedSearch =
                    case context.direction of
                        CallStyle.RightToLeft ->
                            pipingIntoCompositionChecksHelp context targetAndReplacement left

                        CallStyle.LeftToRight ->
                            pipingIntoCompositionChecksHelp context targetAndReplacement right
            in
            if symbol == targetAndReplacement.replacement then
                Maybe.map (\errors -> { errors | firstStepIsComposition = False })
                    continuedSearch

            else if symbol == targetAndReplacement.opToFind then
                let
                    opToFindRange : Range
                    opToFindRange =
                        findOperatorRange
                            { operator = targetAndReplacement.opToFind
                            , commentRanges = context.commentRanges
                            , extractSourceCode = context.extractSourceCode
                            , leftRange = Node.range left
                            , rightRange = Node.range right
                            }
                in
                Just
                    { opToReplaceRange = opToFindRange
                    , fixes =
                        Fix.replaceRangeBy opToFindRange targetAndReplacement.replacement
                            :: (case continuedSearch of
                                    Nothing ->
                                        []

                                    Just additionalErrorsFound ->
                                        additionalErrorsFound.fixes
                               )
                    , firstStepIsComposition = True
                    }

            else
                Nothing

        _ ->
            Nothing


reversedCompositionChecks :
    { checkInfo
        | commentRanges : List Range
        , extractSourceCode : Range -> String
        , direction : CallStyle.LeftOrRightDirection
    }
    -> (() -> List Fix)
    -> Node Expression
    -> Maybe (Error {})
reversedCompositionChecks checkInfo fixesFromParent node =
    let
        { targetOp, replacement } =
            case checkInfo.direction of
                CallStyle.RightToLeft ->
                    { targetOp = ">>", replacement = "<|" }

                CallStyle.LeftToRight ->
                    { targetOp = "<<", replacement = "|>" }

        { operators, expressions, parens } =
            findCompositionElements
                { commentRanges = checkInfo.commentRanges
                , extractSourceCode = checkInfo.extractSourceCode
                , direction = checkInfo.direction
                , targetOp = targetOp
                , replacement = replacement
                }
                node
                { operators = []
                , expressions = []
                , parens = []
                }
    in
    case List.head operators of
        Nothing ->
            Nothing

        Just firstOperator ->
            Just
                (Rule.errorWithFix
                    { message = "Use " ++ replacement ++ " instead of " ++ targetOp
                    , details =
                        [ "Mixing chains of functions with " ++ targetOp ++ " in a direct function call with arguments positioned at the other end is confusing."
                        , "To make it more idiomatic in Elm and generally easier to read, please use " ++ replacement ++ " instead. You may need to remove some parentheses to do this."
                        , "Here is an example:\n"
                            ++ (case checkInfo.direction of
                                    CallStyle.RightToLeft ->
                                        "Before: data |> (fn1 >> fn2)\nAfter:  data |> fn1 |> fn2"

                                    CallStyle.LeftToRight ->
                                        "Before: (fn2 << fn1) data\nAfter:   fn2 <| fn1  data"
                               )
                        ]
                    }
                    firstOperator
                    (List.concat
                        [ List.concatMap (\range -> removeRangeBoundariesFix range) parens
                        , List.map2 (\selfRange oppositeRange -> Fix.replaceRangeBy selfRange (checkInfo.extractSourceCode oppositeRange))
                            (List.reverse expressions)
                            expressions
                        , List.map (\range -> Fix.replaceRangeBy range replacement) operators
                        , fixesFromParent ()
                        ]
                    )
                )


findCompositionElements :
    { commentRanges : List Range
    , extractSourceCode : Range -> String
    , direction : CallStyle.LeftOrRightDirection
    , targetOp : String
    , replacement : String
    }
    -> Node Expression
    -> { operators : List Range, expressions : List Range, parens : List Range }
    -> { operators : List Range, expressions : List Range, parens : List Range }
findCompositionElements context baseNode acc =
    let
        ( Node nodeRange expr, parens ) =
            AstHelpers.removeParensWithRanges baseNode acc.parens
    in
    case expr of
        Expression.OperatorApplication symbol _ ((Node leftRange _) as left) ((Node rightRange _) as right) ->
            if symbol == context.targetOp then
                let
                    operatorRange : Range
                    operatorRange =
                        findOperatorRange
                            { operator = context.targetOp
                            , commentRanges = context.commentRanges
                            , extractSourceCode = context.extractSourceCode
                            , leftRange = leftRange
                            , rightRange = rightRange
                            }

                    visit : { first : Node Expression, last : Node Expression }
                    visit =
                        case context.direction of
                            CallStyle.RightToLeft ->
                                { first = left, last = right }

                            CallStyle.LeftToRight ->
                                { first = right, last = left }

                    composition : { operators : List Range, expressions : List Range, parens : List Range }
                    composition =
                        findCompositionElements
                            context
                            visit.first
                            { operators = acc.operators
                            , expressions = acc.expressions
                            , parens = parens
                            }
                in
                findCompositionElements
                    context
                    visit.last
                    { operators = operatorRange :: composition.operators
                    , expressions = composition.expressions
                    , parens = composition.parens
                    }

            else
                { operators = acc.operators
                , expressions = nodeRange :: acc.expressions
                , parens = parens
                }

        _ ->
            { operators = acc.operators
            , expressions = nodeRange :: acc.expressions
            , parens = parens
            }


rightPipeExample : String
rightPipeExample =
    """
Before: data |> fn1 |> (fn2 >> fn3)
After:  data |> fn1 |>  fn2 |> fn3"""


leftPipeExample : String
leftPipeExample =
    """
Before: (fn3 << fn2) <| fn1 <| data
After:   fn3 <| fn2  <| fn1 <| data"""


{-| `unnecessarySpecificFnBeforeCheck` for reverse and sort fns
-}
listReorderOperationsBeforeAreUnnecessaryChecks : String -> IntoFnCheck
listReorderOperationsBeforeAreUnnecessaryChecks propertyUnaffectedByReorderingDescription =
    let
        whyUnnecessary : String
        whyUnnecessary =
            "Reordering a list does not affect its " ++ propertyUnaffectedByReorderingDescription
    in
    intoFnChecksFirstThatConstructsError
        [ unnecessarySpecificFnBeforeCheck
            { fn = Fn.List.reverse
            , fnArgCount = 1
            , fnLastArgRepresents = "list"
            , whyUnnecessary = whyUnnecessary
            }
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.List.sort
            , fnArgCount = 1
            , fnLastArgRepresents = "list"
            , whyUnnecessary = whyUnnecessary
            }
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.List.sortBy
            , fnArgCount = 2
            , fnLastArgRepresents = "list"
            , whyUnnecessary = whyUnnecessary
            }
        , unnecessarySpecificFnBeforeCheck
            { fn = Fn.List.sortWith
            , fnArgCount = 2
            , fnLastArgRepresents = "list"
            , whyUnnecessary = whyUnnecessary
            }
        ]



-- GENERIC CHECKS


unnecessarySpecificFnBeforeCheck :
    { fn : ( ModuleName, String )
    , fnArgCount : Int
    , fnLastArgRepresents : String
    , whyUnnecessary : String
    }
    -> IntoFnCheck
unnecessarySpecificFnBeforeCheck config =
    { call =
        \checkInfo ->
            case fullyAppliedLastArg checkInfo of
                Nothing ->
                    Nothing

                Just lastArg ->
                    case AstHelpers.getSpecificUnreducedFnCall config.fn checkInfo.lookupTable lastArg of
                        Nothing ->
                            Nothing

                        Just mapUnrelatedCall ->
                            case fullyAppliedLastArg { argCount = config.fnArgCount, firstArg = mapUnrelatedCall.firstArg, argsAfterFirst = mapUnrelatedCall.argsAfterFirst } of
                                Nothing ->
                                    Nothing

                                Just unmappedTuple ->
                                    Just
                                        (Rule.errorWithFix
                                            { message = "Unnecessary " ++ qualifiedToString config.fn ++ " before " ++ qualifiedToString checkInfo.fn
                                            , details = [ config.whyUnnecessary ++ ". You can replace the " ++ qualifiedToString config.fn ++ " call by the unchanged " ++ config.fnLastArgRepresents ++ "." ]
                                            }
                                            checkInfo.fnRange
                                            (replaceBySubExpressionFix mapUnrelatedCall.nodeRange
                                                unmappedTuple
                                            )
                                        )
    , composition =
        \checkInfo ->
            if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == config.fn) then
                Just
                    { info =
                        { message = "Unnecessary " ++ qualifiedToString config.fn ++ " before " ++ qualifiedToString checkInfo.later.fn
                        , details = [ config.whyUnnecessary ++ ". You can remove the " ++ qualifiedToString config.fn ++ " call." ]
                        }
                    , fix = [ Fix.removeRange checkInfo.earlier.removeRange ]
                    }

            else
                Nothing
    }


{-| Condense applying the same function with equal arguments (except the last one) twice in sequence into one.
This applies to functions that are equivalent to identity when operating on the result another such function.

Examples of such functions:

  - one argument: `Simplify.expectNaN`, `Review.Rule.providesFixesForModuleRule`, `List.sort`, `List.Extra.unique`, [`AVL.Set.clear`](https://package.elm-lang.org/packages/owanturist/elm-avl-dict/2.1.0/AVL-Set#clear)
  - two arguments: `List.filter f`, `List.Extra.filterNot f`, `List.Extra.takeWhile/dropWhile(Right) f`, `List.sortBy f`, `List.Extra.uniqueBy f`
  - three arguments: `Array.set i new`, `Array.Extra.resizelRepeat l pad`, `List.Extra.setAt i new`

Note that `update` or `setWhere` operations for example _can_ have an effect even after the same operation has already been applied.

For operations that toggle between 2 states, like `reverse` or `List.Extra.swapAt i j`, use `toggleCallChecks`

-}
operationDoesNotChangeResultOfOperationCheck : IntoFnCheck
operationDoesNotChangeResultOfOperationCheck =
    { call =
        \checkInfo ->
            case fullyAppliedLastArg checkInfo of
                Nothing ->
                    Nothing

                Just lastArg ->
                    case AstHelpers.getSpecificUnreducedFnCall checkInfo.fn checkInfo.lookupTable lastArg of
                        Nothing ->
                            Nothing

                        Just lastArgCall ->
                            let
                                areAllArgsEqual : Bool
                                areAllArgsEqual =
                                    listAll2
                                        (\arg lastArgCallArg ->
                                            Normalize.compare checkInfo arg lastArgCallArg == Normalize.ConfirmedEquality
                                        )
                                        (listFilledInit ( checkInfo.firstArg, checkInfo.argsAfterFirst ))
                                        (listFilledInit ( lastArgCall.firstArg, lastArgCall.argsAfterFirst ))
                            in
                            if areAllArgsEqual then
                                Just
                                    (Rule.errorWithFix
                                        { message =
                                            case checkInfo.argCount of
                                                1 ->
                                                    "Unnecessary " ++ qualifiedToString checkInfo.fn ++ " after " ++ qualifiedToString checkInfo.fn

                                                _ ->
                                                    "Unnecessary " ++ qualifiedToString checkInfo.fn ++ " after equivalent " ++ qualifiedToString checkInfo.fn
                                        , details = [ "You can remove this additional operation." ]
                                        }
                                        checkInfo.fnRange
                                        (replaceCallBySubExpressionFix
                                            checkInfo.parentRange
                                            checkInfo.callStyle
                                            lastArg
                                        )
                                    )

                            else
                                Nothing
    , composition =
        \checkInfo ->
            let
                areAllArgsEqual : () -> Bool
                areAllArgsEqual () =
                    listAll2
                        (\arg earlierArg ->
                            Normalize.compare checkInfo arg earlierArg == Normalize.ConfirmedEquality
                        )
                        checkInfo.later.args
                        checkInfo.earlier.args
            in
            if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == checkInfo.later.fn) && areAllArgsEqual () then
                Just
                    { info =
                        { message =
                            case checkInfo.later.argCount of
                                1 ->
                                    "Unnecessary " ++ qualifiedToString checkInfo.later.fn ++ " after " ++ qualifiedToString checkInfo.earlier.fn

                                _ ->
                                    "Unnecessary " ++ qualifiedToString checkInfo.later.fn ++ " after equivalent " ++ qualifiedToString checkInfo.earlier.fn
                        , details = [ "You can remove this additional operation." ]
                        }
                    , fix = [ Fix.removeRange checkInfo.later.removeRange ]
                    }

            else
                Nothing
    }


{-| When a function's first 2 arguments are equal it will return either argument.

    f a a
    --> a

Examples:

    Basics.max n n
    --> n

    Set.union (Set.union set0 set1) set0
    --> Set.union set0 set1

    Dict.intersect dict0 << Dict.intersect (Dict.intersect dict0 dict1)
    --> Dict.intersect (Dict.intersect dict0 dict1)

-}
callWithTwoEqualArgumentsReturnsEitherArgumentCheck : TypeProperties argumentProperties -> IntoFnCheck
callWithTwoEqualArgumentsReturnsEitherArgumentCheck argumentProperties =
    { call =
        \checkInfo ->
            case secondArg checkInfo of
                Nothing ->
                    Nothing

                Just rightArg ->
                    let
                        maybeLeftInnerArgs : Maybe (List { parentRange : Range, arg : Node Expression, otherArg : Node Expression })
                        maybeLeftInnerArgs =
                            collectArgsNestedInSpecificFnCallsWith2Args checkInfo checkInfo.firstArg

                        maybeRightInnerArgs : Maybe (List { parentRange : Range, arg : Node Expression, otherArg : Node Expression })
                        maybeRightInnerArgs =
                            collectArgsNestedInSpecificFnCallsWith2Args checkInfo rightArg

                        argsInLeft : List { parentRange : Range, arg : Node Expression, otherArg : Node Expression }
                        argsInLeft =
                            maybeLeftInnerArgs
                                |> maybeWithDefaultLazy
                                    (\() ->
                                        [ { parentRange = checkInfo.parentRange
                                          , arg = checkInfo.firstArg
                                          , otherArg = rightArg
                                          }
                                        ]
                                    )

                        argsInRight : List { parentRange : Range, arg : Node Expression, otherArg : Node Expression }
                        argsInRight =
                            maybeRightInnerArgs
                                |> maybeWithDefaultLazy
                                    (\() ->
                                        [ { parentRange = checkInfo.parentRange
                                          , arg = rightArg
                                          , otherArg = checkInfo.firstArg
                                          }
                                        ]
                                    )
                    in
                    case
                        listFind
                            (\argInLeft ->
                                let
                                    argInLeftNormal : Expression
                                    argInLeftNormal =
                                        Normalize.normalizeExpression checkInfo argInLeft.arg
                                in
                                List.any
                                    (\argInRight ->
                                        Normalize.compareExistingNormals
                                            argInLeftNormal
                                            (Normalize.normalizeExpression checkInfo argInRight.arg)
                                            == Normalize.ConfirmedEquality
                                    )
                                    argsInRight
                            )
                            argsInLeft
                    of
                        Just argInLeftEqualToArgInRight ->
                            let
                                isInSingleCall : Bool
                                isInSingleCall =
                                    isNothing maybeLeftInnerArgs && isNothing maybeRightInnerArgs
                            in
                            Just
                                (Rule.errorWithFix
                                    (if isInSingleCall then
                                        { message =
                                            qualifiedToString checkInfo.fn
                                                ++ " with two equal "
                                                ++ argumentProperties.representsPlural
                                                ++ " can be replaced by one of them"
                                        , details =
                                            [ "You can replace this call by one of its arguments." ]
                                        }

                                     else
                                        { message =
                                            "nested "
                                                ++ qualifiedToString checkInfo.fn
                                                ++ " contains unnecessary equal "
                                                ++ argumentProperties.representsPlural
                                                ++ " across both arguments"
                                        , details =
                                            [ "You can replace the call that has an equal argument by its other argument." ]
                                        }
                                    )
                                    checkInfo.fnRange
                                    (replaceBySubExpressionFix argInLeftEqualToArgInRight.parentRange
                                        -- choosing to replace the left argument is arbitrary.
                                        -- we could instead also choose based on some heuristic
                                        argInLeftEqualToArgInRight.otherArg
                                    )
                                )

                        Nothing ->
                            Nothing
    , composition =
        \checkInfo ->
            if checkInfo.earlier.fn == checkInfo.later.fn then
                case checkInfo.earlier.args of
                    [ earlierArg ] ->
                        case checkInfo.later.args of
                            [ laterArg ] ->
                                let
                                    maybeEarlierInnerArgs : Maybe (List { parentRange : Range, arg : Node Expression, otherArg : Node Expression })
                                    maybeEarlierInnerArgs =
                                        collectArgsNestedInSpecificFnCallsWith2Args
                                            { lookupTable = checkInfo.lookupTable
                                            , fn = checkInfo.later.fn
                                            }
                                            earlierArg

                                    maybeLaterInnerArgs : Maybe (List { parentRange : Range, arg : Node Expression, otherArg : Node Expression })
                                    maybeLaterInnerArgs =
                                        collectArgsNestedInSpecificFnCallsWith2Args
                                            { lookupTable = checkInfo.lookupTable
                                            , fn = checkInfo.later.fn
                                            }
                                            laterArg
                                in
                                case maybeLaterInnerArgs of
                                    Just laterInnerArgs ->
                                        case maybeEarlierInnerArgs of
                                            Just earlierInnerArgs ->
                                                case
                                                    listFind
                                                        (\laterInnerArg ->
                                                            let
                                                                laterInnerArgNormal : Expression
                                                                laterInnerArgNormal =
                                                                    Normalize.normalizeExpression checkInfo laterInnerArg.arg
                                                            in
                                                            List.any
                                                                (\earlierInnerArg ->
                                                                    Normalize.compareExistingNormals
                                                                        laterInnerArgNormal
                                                                        (Normalize.normalizeExpression checkInfo earlierInnerArg.arg)
                                                                        == Normalize.ConfirmedEquality
                                                                )
                                                                earlierInnerArgs
                                                        )
                                                        laterInnerArgs
                                                of
                                                    Nothing ->
                                                        Nothing

                                                    Just innerArgInEarlierEqualToInnerArgInLater ->
                                                        Just
                                                            { info =
                                                                { message =
                                                                    "nested "
                                                                        ++ qualifiedToString checkInfo.later.fn
                                                                        ++ " contains unnecessary equal "
                                                                        ++ argumentProperties.representsPlural
                                                                        ++ " across the arguments of the composed functions"
                                                                , details =
                                                                    [ "You can replace the inner call that has an equal argument by its other argument." ]
                                                                }
                                                            , fix =
                                                                replaceBySubExpressionFix innerArgInEarlierEqualToInnerArgInLater.parentRange
                                                                    -- choosing to replace the left argument is arbitrary.
                                                                    -- we could instead also choose based on some heuristic
                                                                    innerArgInEarlierEqualToInnerArgInLater.otherArg
                                                            }

                                            Nothing ->
                                                let
                                                    earlierArgNormal : Expression
                                                    earlierArgNormal =
                                                        Normalize.normalizeExpression checkInfo earlierArg
                                                in
                                                if
                                                    List.any
                                                        (\laterInnerArg ->
                                                            Normalize.compareExistingNormals
                                                                (Normalize.normalizeExpression checkInfo laterInnerArg.arg)
                                                                earlierArgNormal
                                                                == Normalize.ConfirmedEquality
                                                        )
                                                        laterInnerArgs
                                                then
                                                    Just
                                                        { info =
                                                            { message =
                                                                "nested "
                                                                    ++ qualifiedToString checkInfo.later.fn
                                                                    ++ " contains unnecessary equal "
                                                                    ++ argumentProperties.representsPlural
                                                                    ++ " across the arguments of the composed functions"
                                                            , details =
                                                                [ "You can remove the operation that has an equal argument." ]
                                                            }
                                                        , fix =
                                                            [ Fix.removeRange checkInfo.earlier.removeRange ]
                                                        }

                                                else
                                                    Nothing

                                    Nothing ->
                                        let
                                            laterArgNormal : Expression
                                            laterArgNormal =
                                                Normalize.normalizeExpression checkInfo laterArg
                                        in
                                        case maybeEarlierInnerArgs of
                                            Nothing ->
                                                case Normalize.compareExistingNormals (Normalize.normalizeExpression checkInfo earlierArg) laterArgNormal of
                                                    Normalize.ConfirmedEquality ->
                                                        Just
                                                            { info =
                                                                { message =
                                                                    "Unnecessary "
                                                                        ++ qualifiedToString checkInfo.later.fn
                                                                        ++ " on "
                                                                        ++ qualifiedToString checkInfo.later.fn
                                                                        ++ " with an equal "
                                                                        ++ argumentProperties.represents
                                                                , details =
                                                                    [ "You can replace this composition by either its left or right function as both are equivalent." ]
                                                                }
                                                            , fix =
                                                                [ Fix.removeRange checkInfo.later.removeRange ]
                                                            }

                                                    _ ->
                                                        Nothing

                                            Just earlierInnerArgs ->
                                                if
                                                    List.any
                                                        (\earlierInnerArg ->
                                                            Normalize.compareExistingNormals
                                                                (Normalize.normalizeExpression checkInfo earlierInnerArg.arg)
                                                                laterArgNormal
                                                                == Normalize.ConfirmedEquality
                                                        )
                                                        earlierInnerArgs
                                                then
                                                    Just
                                                        { info =
                                                            { message =
                                                                "nested "
                                                                    ++ qualifiedToString checkInfo.later.fn
                                                                    ++ " contains unnecessary equal "
                                                                    ++ argumentProperties.representsPlural
                                                                    ++ " across the arguments of the composed functions"
                                                            , details =
                                                                [ "You can remove the operation that has an equal argument." ]
                                                            }
                                                        , fix =
                                                            [ Fix.removeRange checkInfo.later.removeRange ]
                                                        }

                                                else
                                                    Nothing

                            _ ->
                                Nothing

                    _ ->
                        Nothing

            else
                Nothing
    }


collectArgsNestedInSpecificFnCallsWith2Args :
    { config
        | fn : ( ModuleName, String )
        , lookupTable : ModuleNameLookupTable
    }
    -> Node Expression
    -> Maybe (List { parentRange : Range, arg : Node Expression, otherArg : Node Expression })
collectArgsNestedInSpecificFnCallsWith2Args config expressionNode =
    case AstHelpers.getSpecificUnreducedFnCall config.fn config.lookupTable expressionNode of
        Nothing ->
            Nothing

        Just specificFnCall ->
            case specificFnCall.argsAfterFirst of
                [ specificFnCallSecondArg ] ->
                    Just
                        ((collectArgsNestedInSpecificFnCallsWith2Args config specificFnCall.firstArg
                            |> maybeWithDefaultLazy
                                (\() ->
                                    [ { parentRange = specificFnCall.nodeRange
                                      , arg = specificFnCall.firstArg
                                      , otherArg = specificFnCallSecondArg
                                      }
                                    ]
                                )
                         )
                            ++ (collectArgsNestedInSpecificFnCallsWith2Args config specificFnCallSecondArg
                                    |> maybeWithDefaultLazy
                                        (\() ->
                                            [ { parentRange = specificFnCall.nodeRange
                                              , arg = specificFnCallSecondArg
                                              , otherArg = specificFnCall.firstArg
                                              }
                                            ]
                                        )
                               )
                        )

                _ ->
                    Nothing


toggleFnChecks : IntoFnCheck
toggleFnChecks =
    { call = \checkInfo -> (onSpecificFnCallReturnsItsLastArgCheck checkInfo.fn).call checkInfo
    , composition = \checkInfo -> (onSpecificFnCallReturnsItsLastArgCheck checkInfo.later.fn).composition checkInfo
    }


{-| Chaining two operations that are inverses of each other and therefore cancel each other out.
For example

    Array.fromList (Array.toList array)
    --> array

    Array.fromList << Array.toList
    --> identity

    Array.toList (Array.fromList list)
    --> list

    Array.toList << Array.fromList
    --> identity

These usually exist in pairs, like above so make sure to add this check for both functions.
But there are exceptions!

    Set.fromList (Set.toList set) --> set

    Set.fromList << Set.toList --> identity

This will always work because `Set.toList` will never produce a list with duplicate elements. However!

    Set.toList (Set.fromList list) --> list

    Set.toList << Set.fromList --> identity

would be an incorrect fix. See for example

    Set.toList (Set.fromList [ 0, 0 ])
    --> not [ 0, 0 ] but actually [ 0 ]

-}
onSpecificFnCallReturnsItsLastArgCheck : ( ModuleName, String ) -> IntoFnCheck
onSpecificFnCallReturnsItsLastArgCheck inverseFn =
    { call =
        \checkInfo ->
            case AstHelpers.getSpecificUnreducedFnCall inverseFn checkInfo.lookupTable checkInfo.firstArg of
                Just call ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString inverseFn ++ ", then " ++ qualifiedToString checkInfo.fn ++ " cancels each other out"
                            , details = [ "You can replace this call by the argument given to " ++ qualifiedToString inverseFn ++ "." ]
                            }
                            checkInfo.fnRange
                            (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle call.firstArg)
                        )

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if checkInfo.earlier.fn == inverseFn then
                Just
                    (compositionAlwaysReturnsIncomingError
                        (qualifiedToString checkInfo.earlier.fn ++ ", then " ++ qualifiedToString checkInfo.later.fn ++ " cancels each other out")
                        checkInfo
                    )

            else
                Nothing
    }


unnecessaryOnSpecificFnCallCheck : ( ModuleName, String ) -> IntoFnCheck
unnecessaryOnSpecificFnCallCheck specificFn =
    { call = \checkInfo -> unnecessaryCallOnSpecificFnCallCheck specificFn checkInfo
    , composition = \checkInfo -> unnecessaryCompositionAfterSpecificFnValueOrCallCheck specificFn checkInfo
    }


unnecessaryCompositionAfterSpecificFnValueOrCallCheck : ( ModuleName, String ) -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
unnecessaryCompositionAfterSpecificFnValueOrCallCheck specificFn checkInfo =
    if
        onlyLastArgIsCurried checkInfo.later
            && (checkInfo.earlier.fn == specificFn)
    then
        Just
            { info =
                { message =
                    "Unnecessary "
                        ++ qualifiedToString checkInfo.later.fn
                        ++ " on a "
                        ++ qualifiedToString specificFn
                        ++ " call"
                , details =
                    [ "You can replace this composition by the given "
                        ++ qualifiedToString specificFn
                        ++ (case checkInfo.earlier.args of
                                [] ->
                                    " function"

                                _ :: _ ->
                                    " call"
                           )
                        ++ "."
                    ]
                }
            , fix =
                [ Fix.removeRange checkInfo.later.removeRange ]
            }

    else
        Nothing


unnecessaryCallOnSpecificFnCallCheck : ( ModuleName, String ) -> CallCheckInfo -> Maybe (Error {})
unnecessaryCallOnSpecificFnCallCheck specificFn checkInfo =
    case fullyAppliedLastArg checkInfo of
        Nothing ->
            Nothing

        Just fullyAppliedLastArgNode ->
            if
                trueInAllBranches
                    (\branch ->
                        AstHelpers.isSpecificUnreducedFnCall specificFn
                            checkInfo.lookupTable
                            branch
                    )
                    fullyAppliedLastArgNode
            then
                Just
                    (Rule.errorWithFix
                        { message =
                            "Unnecessary "
                                ++ qualifiedToString checkInfo.fn
                                ++ " on a "
                                ++ qualifiedToString specificFn
                                ++ " call"
                        , details =
                            [ "You can replace this call by the given "
                                ++ qualifiedToString specificFn
                                ++ " call."
                            ]
                        }
                        checkInfo.fnRange
                        (replaceCallBySubExpressionFix
                            checkInfo.parentRange
                            checkInfo.callStyle
                            fullyAppliedLastArgNode
                        )
                    )

            else
                Nothing


{-| The wrapper check

    fn .. (wrap a) --> wrap a
    fn .. << wrap --> wrap

So for example

    List.reverse [ a ] --> [ a ]

    List.reverse << List.singleton --> List.singleton

-}
unnecessaryOnWrappedCheck : WrapperProperties otherProperties -> IntoFnCheck
unnecessaryOnWrappedCheck wrapper =
    { call =
        \checkInfo ->
            unnecessaryCallOnCheck { specific = wrapper.wrap, kind = ConstructWithOneValue } checkInfo
    , composition = \checkInfo -> unnecessaryCompositionAfterCheck wrapper.wrap checkInfo
    }


unnecessaryCallOnCheck : TypeSubsetProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
unnecessaryCallOnCheck constructable checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just constructableArg ->
            if trueInAllBranches (\branch -> isInTypeSubset constructable checkInfo branch) constructableArg then
                Just
                    (Rule.errorWithFix
                        (operationDoesNotChangeSpecificLastArgErrorInfo { fn = checkInfo.fn, specific = constructable })
                        checkInfo.fnRange
                        (replaceCallBySubExpressionFix
                            checkInfo.parentRange
                            checkInfo.callStyle
                            constructableArg
                        )
                    )

            else
                Nothing

        Nothing ->
            Nothing


{-| The operation is equivalent to identity when applied on an empty value:

    fn .. empty --> empty

    -- only when a constructor fn exists
    fn .. << emptyConstructor --> emptyConstructor

Examples

    List.sort []
    --> []

    Json.Decode.map f (Json.Decode.fail x)
    --> Json.Decode.fail x

    Json.Decode.map f << Json.Decode.fail
    --> Json.Decode.fail

    Task.mapError f (Task.succeed a)
    --> Task.succeed a

    Task.mapError f << Task.succeed
    --> Task.succeed

-}
unnecessaryOnEmptyCheck : EmptiableProperties empty otherProperties -> IntoFnCheck
unnecessaryOnEmptyCheck emptiable =
    { call = \checkInfo -> unnecessaryCallOnCheck emptiable.empty checkInfo
    , composition =
        case emptiable.empty.kind emptiable.empty.specific of
            Constant _ ->
                \_ -> Nothing

            ConstructWithOneValue constructWithOneValue ->
                \checkInfo -> unnecessaryCompositionAfterCheck constructWithOneValue checkInfo
    }


callOnEmptyReturnsCheck :
    { resultAsString : QualifyResources {} -> String }
    -> EmptiableProperties empty otherProperties
    -> CallCheckInfo
    -> Maybe (Error {})
callOnEmptyReturnsCheck config collection checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just lastArg ->
            if isInTypeSubset collection.empty (extractNormalizeResources checkInfo) lastArg then
                let
                    resultDescription : String
                    resultDescription =
                        config.resultAsString defaultQualifyResources
                in
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " on " ++ typeSubsetDescriptionIndefinite collection.empty ++ " will result in " ++ resultDescription
                        , details = [ "You can replace this call by " ++ resultDescription ++ "." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy checkInfo.parentRange
                            (config.resultAsString (extractQualifyResources checkInfo))
                        ]
                    )

            else
                Nothing

        Nothing ->
            Nothing


unnecessaryCompositionAfterCheck :
    ConstructWithOneValueProperties
    -> CompositionIntoCheckInfo
    -> Maybe ErrorInfoAndFix
unnecessaryCompositionAfterCheck construct checkInfo =
    if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == construct.fn) then
        Just
            { info =
                { message = qualifiedToString checkInfo.later.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite construct.description ++ " will result in " ++ constructWithOneValueDescriptionDefinite "the unchanged" construct.description
                , details = [ "You can replace this composition by " ++ qualifiedToString (qualify construct.fn checkInfo) ++ "." ]
                }
            , fix =
                [ Fix.removeRange checkInfo.later.removeRange ]
            }

    else
        Nothing


callWithNonPositiveIntCanBeReplacedByCheck :
    { int : number
    , intDescription : String
    , replacement : QualifyResources {} -> String
    }
    -> CallCheckInfo
    -> Maybe (Error {})
callWithNonPositiveIntCanBeReplacedByCheck config checkInfo =
    callWithNonPositiveIntCheckErrorSituation { fn = checkInfo.fn, int = config.int, intDescription = config.intDescription }
        |> Maybe.map
            (\situation ->
                alwaysResultsInUnparenthesizedConstantError situation
                    { replacement = config.replacement }
                    checkInfo
            )


callWithNonPositiveIntCheckErrorSituation :
    { fn : ( ModuleName, String )
    , int : number
    , intDescription : String
    }
    -> Maybe String
callWithNonPositiveIntCheckErrorSituation config =
    if config.int <= 0 then
        let
            lengthDescription : String
            lengthDescription =
                if config.int < 0 then
                    "negative " ++ config.intDescription

                else
                    config.intDescription ++ " 0"
        in
        Just
            (qualifiedToString config.fn ++ " with " ++ lengthDescription)

    else
        Nothing


{-| The last argument of a fully applied function (the given `argCount` specifies what is considered "fully applied").

For example, `fullyAppliedLastArg` on `Array.set 3 "Hitagi"` would return `Nothing`
while `fullyAppliedLastArg` on `Array.set 3 "Hitagi" arr` would return `Just arr`.

-}
fullyAppliedLastArg : { callInfo | firstArg : Node Expression, argsAfterFirst : List (Node Expression), argCount : Int } -> Maybe (Node Expression)
fullyAppliedLastArg callInfo =
    case callInfo.argCount of
        1 ->
            Just callInfo.firstArg

        argCountAtLeast2 ->
            List.drop (argCountAtLeast2 - 2) callInfo.argsAfterFirst |> List.head


onlyLastArgIsCurried : { function | args : List (Node Expression), argCount : Int } -> Bool
onlyLastArgIsCurried functionInfo =
    List.length functionInfo.args == (functionInfo.argCount - 1)


{-| This operation is equivalent to identity when called on a wrapped value.

    operation (wrap a) --> a

    operation << wrap --> identity

For example

    List.sum [ a ] --> a

    Cmd.batch [ a ] --> a

    List.sum << List.singleton --> identity

    Cmd.batch << List.singleton --> identity

-}
onWrappedReturnsItsValueCheck : WrapperProperties otherProperties -> IntoFnCheck
onWrappedReturnsItsValueCheck wrapper =
    { call =
        \checkInfo ->
            case fullyAppliedLastArg checkInfo of
                Just wrapperArg ->
                    case sameInAllBranches (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) wrapperArg of
                        Nothing ->
                            Nothing

                        Just wraps ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will result in the value inside"
                                    , details = [ "You can replace this call by the value inside " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range wrapperArg }
                                        ++ List.concatMap (\wrap -> replaceBySubExpressionFix wrap.nodeRange wrap.value) wraps
                                    )
                                )

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == wrapper.wrap.fn) then
                Just
                    (compositionAlwaysReturnsIncomingError
                        (qualifiedToString (qualify checkInfo.later.fn defaultQualifyResources) ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will always result in the value inside")
                        checkInfo
                    )

            else
                Nothing
    }


{-| This operation is equivalent to Just when called on a wrapped value.

    operation (wrap a) --> Just a

    operation << wrap --> Just

For example

    List.minimum [ a ] --> Just a

    List.minimum << List.singleton --> Just

    Result.toMaybe (Ok a) --> Just a

    Result.toMaybe << Ok --> Just

-}
onWrappedReturnsJustItsValueCheck : WrapperProperties otherProperties -> IntoFnCheck
onWrappedReturnsJustItsValueCheck wrapper =
    { call =
        \checkInfo ->
            case fullyAppliedLastArg checkInfo of
                Just withWrapArg ->
                    case sameInAllBranches (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) withWrapArg of
                        Just wraps ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will result in Just the value inside"
                                    , details = [ "You can replace this call by Just the value inside " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = (Node.range withWrapArg).start }
                                        (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo) ++ " ")
                                        :: Fix.removeRange { start = (Node.range withWrapArg).end, end = checkInfo.parentRange.end }
                                        :: List.concatMap (\wrap -> replaceBySubExpressionFix wrap.nodeRange wrap.value) wraps
                                    )
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == wrapper.wrap.fn) then
                Just
                    { info =
                        { message = qualifiedToString checkInfo.later.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will always result in Just the value inside"
                        , details = [ "You can replace this call by Just." ]
                        }
                    , fix = compositionReplaceByFnFix Fn.Maybe.justVariant checkInfo
                    }

            else
                Nothing
    }


{-| The filter checks

    keepWhen f empty --> empty

    keepWhen f (keepWhen f emptiable) --> keepWhen f emptiable

    keepWhen (\_ -> True) emptiable --> emptiable

    keepWhen (\_ -> False) emptiable --> empty

If your function only takes two arguments like `Dict.filter`, use `emptiableKeepWhenWithExtraArgChecks`

-}
emptiableKeepWhenChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> IntoFnCheck
emptiableKeepWhenChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ operationDoesNotChangeResultOfOperationCheck
        , unnecessaryOnEmptyCheck emptiable
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case AstHelpers.getAlwaysResult checkInfo checkInfo.firstArg of
                    Just constantFunctionResult ->
                        keepWhenWithConstantFunctionResultChecks constantFunctionResult emptiable checkInfo

                    Nothing ->
                        Nothing
            )
        ]


keepWhenWithConstantFunctionResultChecks : Node Expression -> TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
keepWhenWithConstantFunctionResultChecks constantFunctionResult emptiable checkInfo =
    case Normalize.getBool checkInfo constantFunctionResult of
        Just True ->
            Just
                (alwaysReturnsLastArgError
                    (qualifiedToString checkInfo.fn ++ " with a function that will always return True")
                    emptiable
                    checkInfo
                )

        Just False ->
            Just
                (alwaysResultsInUnparenthesizedConstantError
                    (qualifiedToString checkInfo.fn ++ " with a function that will always return False")
                    { replacement = emptiable.empty.specific.asString }
                    checkInfo
                )

        Nothing ->
            Nothing


{-| Filter checks where the function takes 2 arguments

    keepWhenWithExtraArg f empty --> empty

    keepWhenWithExtraArg f (keepWhenWithExtraArg f emptiable) --> keepWhenWithExtraArg f emptiable

    keepWhenWithExtraArg (\_ _ -> True) emptiable --> emptiable

    keepWhenWithExtraArg (\_ _ -> False) emptiable --> empty

If your function only takes one argument like `List.filter`, use `emptiableKeepWhenChecks`

-}
emptiableKeepWhenWithExtraArgChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> IntoFnCheck
emptiableKeepWhenWithExtraArgChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ operationDoesNotChangeResultOfOperationCheck
        , unnecessaryOnEmptyCheck emptiable
        , intoFnCheckOnlyCall
            (\checkInfo ->
                let
                    maybeFilterFunctionResult : Maybe (Node Expression)
                    maybeFilterFunctionResult =
                        checkInfo.firstArg
                            |> AstHelpers.getAlwaysResult checkInfo
                            |> Maybe.andThen (\result -> AstHelpers.getAlwaysResult checkInfo result)
                in
                case maybeFilterFunctionResult of
                    Just constantFunctionResult ->
                        keepWhenWithConstantFunctionResultChecks constantFunctionResult emptiable checkInfo

                    Nothing ->
                        Nothing
            )
        ]


collectionRemoveElementChecks : CollectionProperties (EmptiableProperties empty otherProperties) -> IntoFnCheck
collectionRemoveElementChecks collection =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck collection
        , operationDoesNotChangeResultOfOperationCheck
        ]


collectionSliceChecks : EmptiableProperties ConstantProperties otherProperties -> IntoFnCheck
collectionSliceChecks collection =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck collection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Just endArg ->
                        if Normalize.areTheSame checkInfo checkInfo.firstArg endArg then
                            Just
                                (alwaysResultsInUnparenthesizedConstantError (qualifiedToString checkInfo.fn ++ " with equal start and end index")
                                    { replacement = collection.empty.specific.asString }
                                    checkInfo
                                )

                        else
                            case Normalize.getInt checkInfo endArg of
                                Just endInt ->
                                    case endInt of
                                        0 ->
                                            Just
                                                (alwaysResultsInUnparenthesizedConstantError (qualifiedToString checkInfo.fn ++ " with end index 0")
                                                    { replacement = collection.empty.specific.asString }
                                                    checkInfo
                                                )

                                        _ ->
                                            case Normalize.getInt checkInfo checkInfo.firstArg of
                                                Just startInt ->
                                                    if startInt > endInt then
                                                        if startInt >= 0 && endInt >= 0 then
                                                            Just
                                                                (alwaysResultsInUnparenthesizedConstantError (qualifiedToString checkInfo.fn ++ " with a start index greater than the end index")
                                                                    { replacement = collection.empty.specific.asString }
                                                                    checkInfo
                                                                )

                                                        else if startInt <= -1 && endInt <= -1 then
                                                            Just
                                                                (alwaysResultsInUnparenthesizedConstantError (qualifiedToString checkInfo.fn ++ " with a negative start index closer to the right than the negative end index")
                                                                    { replacement = collection.empty.specific.asString }
                                                                    checkInfo
                                                                )

                                                        else
                                                            Nothing

                                                    else
                                                        Nothing

                                                Nothing ->
                                                    Nothing

                                Nothing ->
                                    Nothing

                    Nothing ->
                        Nothing
            )
        ]


collectionIntersectChecks : TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties otherProperties)) -> IntoFnCheck
collectionIntersectChecks collection =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck collection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                if collection.empty.specific.is (extractNormalizeResources checkInfo) checkInfo.firstArg then
                    Just
                        (alwaysResultsInUnparenthesizedConstantError
                            (qualifiedToString checkInfo.fn ++ " on " ++ collection.empty.specific.description)
                            { replacement = collection.empty.specific.asString }
                            checkInfo
                        )

                else
                    Nothing
            )
        , callWithTwoEqualArgumentsReturnsEitherArgumentCheck collection
        ]


collectionDiffChecks : TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties otherProperties)) -> CallCheckInfo -> Maybe (Error {})
collectionDiffChecks collection checkInfo =
    if collection.empty.specific.is (extractNormalizeResources checkInfo) checkInfo.firstArg then
        Just
            (alwaysResultsInUnparenthesizedConstantError
                (qualifiedToString checkInfo.fn ++ " " ++ emptyAsString checkInfo collection)
                { replacement = collection.empty.specific.asString }
                checkInfo
            )

    else
        case secondArg checkInfo of
            Just collectionArg ->
                if isInTypeSubset collection.empty (extractNormalizeResources checkInfo) collectionArg then
                    Just
                        (Rule.errorWithFix
                            { message = "Unnecessary " ++ qualifiedToString checkInfo.fn ++ " with " ++ emptyAsString checkInfo collection
                            , details = [ "You can replace this call by the given first " ++ collection.represents ++ "." ]
                            }
                            checkInfo.fnRange
                            (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle checkInfo.firstArg)
                        )

                else
                    Nothing

            Nothing ->
                Nothing


collectionUnionChecks : { leftElementsStayOnTheLeft : Bool } -> TypeProperties (CollectionProperties (ConstructibleFromListProperties (EmptiableProperties empty otherProperties))) -> CallCheckInfo -> Maybe (Error {})
collectionUnionChecks config collection checkInfo =
    if isInTypeSubset collection.empty checkInfo checkInfo.firstArg then
        Just
            (alwaysReturnsLastArgError
                (qualifiedToString checkInfo.fn ++ " " ++ typeSubsetDescriptionWithoutArticle collection.empty)
                collection
                checkInfo
            )

    else
        case secondArg checkInfo of
            Just secondArg_ ->
                if isInTypeSubset collection.empty checkInfo secondArg_ then
                    Just
                        (Rule.errorWithFix
                            { message = "Unnecessary " ++ qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " with " ++ typeSubsetDescriptionIndefinite collection.empty
                            , details = [ "You can replace this call by the given first " ++ collection.represents ++ "." ]
                            }
                            checkInfo.fnRange
                            (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle checkInfo.firstArg)
                        )

                else
                    collectionUnionWithLiteralsChecks config
                        { first = checkInfo.firstArg
                        , second = secondArg_
                        , operationRange = checkInfo.fnRange
                        , operation = qualifiedToString (qualify checkInfo.fn defaultQualifyResources)
                        }
                        collection
                        checkInfo

            Nothing ->
                Nothing


constructionFromListOnLiteralDescription : ConstructionFromList -> String
constructionFromListOnLiteralDescription fromListConstruction =
    case fromListConstruction of
        ConstructionAsList ->
            "list literal"

        ConstructionFromListCall fn ->
            qualifiedToString fn ++ " call"


{-| The union check

    unionFn (wrapFn wrappedArguments)
    --> (combinedFn wrappedArguments)

    unionFn << wrapFn wrappedArgumentsExceptTheLast
    --> combinedFn wrappedArgumentsExceptTheLast

-}
unionWithFirstArgWrappedCanBeCombinedInto :
    { wrapFn : ( ModuleName, String )
    , wrapFnArgCount : Int
    , articleWrapperName : String
    , wrapFnArgumentsDescription : String
    , combinedFn : ( ModuleName, String )
    }
    -> IntoFnCheck
unionWithFirstArgWrappedCanBeCombinedInto config =
    { call =
        \checkInfo ->
            AstHelpers.getSpecificUnreducedFnCall config.wrapFn checkInfo.lookupTable checkInfo.firstArg
                |> Maybe.map
                    (\wrapCall ->
                        Rule.errorWithFix
                            { message =
                                qualifiedToString checkInfo.fn
                                    ++ " with "
                                    ++ config.articleWrapperName
                                    ++ " can be combined into "
                                    ++ qualifiedToString config.combinedFn
                            , details =
                                [ "You can replace this call by "
                                    ++ qualifiedToString config.combinedFn
                                    ++ " with the same "
                                    ++ config.wrapFnArgumentsDescription
                                    ++ " given to "
                                    ++ qualifiedToString config.wrapFn
                                    ++ " which is meant for this exact purpose."
                                ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy wrapCall.fnRange
                                (qualifiedToString (qualify config.combinedFn checkInfo))
                            , Fix.removeRange
                                (rangeFromInclusiveToExclusive
                                    { fromInclusive = checkInfo.fnRange
                                    , toExclusive = Node.range checkInfo.firstArg
                                    }
                                )
                            ]
                    )
    , composition =
        \checkInfo ->
            if
                List.isEmpty checkInfo.later.args
                    && (checkInfo.earlier.fn == config.wrapFn)
                    && (List.length checkInfo.earlier.args == config.wrapFnArgCount - 1)
            then
                Just
                    { info =
                        { message =
                            qualifiedToString checkInfo.later.fn
                                ++ " with "
                                ++ config.articleWrapperName
                                ++ " can be combined into "
                                ++ qualifiedToString config.combinedFn
                        , details =
                            [ "You can replace this composition by "
                                ++ qualifiedToString config.combinedFn
                                ++ (case config.wrapFnArgCount of
                                        1 ->
                                            ""

                                        2 ->
                                            " with the same argument given to "
                                                ++ qualifiedToString config.wrapFn

                                        _ ->
                                            " with the same arguments given to "
                                                ++ qualifiedToString config.wrapFn
                                   )
                                ++ " which is meant for this exact purpose."
                            ]
                        }
                    , fix =
                        [ Fix.replaceRangeBy checkInfo.earlier.fnRange
                            (qualifiedToString (qualify config.combinedFn checkInfo))
                        , Fix.removeRange
                            checkInfo.later.removeRange
                        ]
                    }

            else
                Nothing
    }


collectionUnionWithLiteralsChecks :
    { leftElementsStayOnTheLeft : Bool }
    ->
        { first : Node Expression
        , second : Node Expression
        , operationRange : Range
        , operation : String
        }
    -> CollectionProperties (ConstructibleFromListProperties otherProperties)
    ->
        { checkInfo
            | lookupTable : ModuleNameLookupTable
            , extractSourceCode : Range -> String
            , parentRange : Range
        }
    -> Maybe (Error {})
collectionUnionWithLiteralsChecks config operationInfo collection checkInfo =
    case fromListGetLiteral collection checkInfo.lookupTable operationInfo.second of
        Just literalListSecond ->
            case fromListGetLiteral collection checkInfo.lookupTable operationInfo.first of
                Just literalListFirst ->
                    if
                        (literalListFirst.literalRange.start.row == literalListSecond.literalRange.end.row)
                            || ((literalListFirst.literalRange.start.row /= literalListFirst.literalRange.end.row)
                                    && (literalListSecond.literalRange.start.row /= literalListSecond.literalRange.end.row)
                               )
                    then
                        let
                            fromListLiteralDescription : String
                            fromListLiteralDescription =
                                constructionFromListOnLiteralDescription collection.fromList
                        in
                        Just
                            (Rule.errorWithFix
                                { message = operationInfo.operation ++ " on " ++ fromListLiteralDescription ++ "s can be turned into a single " ++ fromListLiteralDescription
                                , details = [ "Try moving all the elements into a single " ++ fromListLiteralDescription ++ "." ]
                                }
                                operationInfo.operationRange
                                (if config.leftElementsStayOnTheLeft then
                                    Fix.insertAt
                                        (rangeWithoutBoundaries literalListSecond.literalRange).start
                                        (checkInfo.extractSourceCode (rangeWithoutBoundaries literalListFirst.literalRange) ++ ",")
                                        :: keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range operationInfo.second }

                                 else
                                    Fix.insertAt
                                        (rangeWithoutBoundaries literalListFirst.literalRange).start
                                        (checkInfo.extractSourceCode (rangeWithoutBoundaries literalListSecond.literalRange) ++ ",")
                                        :: keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range operationInfo.first }
                                )
                            )

                    else
                        Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


collectionInsertChecks : CollectionProperties (EmptiableProperties empty (WrapperProperties otherProperties)) -> IntoFnCheck
collectionInsertChecks collection =
    intoFnChecksFirstThatConstructsError
        [ operationDoesNotChangeResultOfOperationCheck
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Just collectionArg ->
                        if isInTypeSubset collection.empty checkInfo collectionArg then
                            Just
                                (Rule.errorWithFix
                                    { message = "Use " ++ qualifiedToString collection.wrap.fn ++ " instead of inserting in " ++ typeSubsetDescriptionIndefinite collection.empty
                                    , details = [ "You can replace this call by " ++ qualifiedToString collection.wrap.fn ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.insertAt checkInfo.parentRange.start
                                        (qualifiedToString (qualify collection.wrap.fn checkInfo) ++ " ")
                                        :: replaceBySubExpressionFix checkInfo.parentRange checkInfo.firstArg
                                    )
                                )

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )
        ]


collectionIsEmptyChecks : TypeProperties (CollectionProperties (EmptiableProperties empty otherProperties)) -> CallCheckInfo -> Maybe (Error {})
collectionIsEmptyChecks collection checkInfo =
    let
        collectionSize : CollectionSize
        collectionSize =
            collection.elements.determineCount (extractNormalizeResources checkInfo) checkInfo.firstArg
    in
    if collectionSize.min >= 1 then
        Just
            (resultsInConstantError
                (qualifiedToString checkInfo.fn ++ " on this " ++ collection.represents)
                (\res -> qualifiedToString (qualify Fn.Basics.falseVariant res))
                checkInfo
            )

    else
        case collectionSize.max of
            Just 0 ->
                Just
                    (resultsInConstantError
                        (qualifiedToString checkInfo.fn ++ " on " ++ typeSubsetDescriptionIndefinite collection.empty)
                        (\res -> qualifiedToString (qualify Fn.Basics.trueVariant res))
                        checkInfo
                    )

            _ ->
                Nothing


collectionSizeChecks : TypeProperties (CollectionProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
collectionSizeChecks collection checkInfo =
    let
        collectionSize : CollectionSize
        collectionSize =
            collection.elements.determineCount (extractNormalizeResources checkInfo) checkInfo.firstArg
    in
    case collectionSize.max of
        Nothing ->
            Nothing

        Just collectionSizeMax ->
            if collectionSize.min == collectionSizeMax then
                Just
                    (Rule.errorWithFix
                        { message = "The " ++ collection.elements.countDescription ++ " of the " ++ collection.represents ++ " is " ++ String.fromInt collectionSize.min
                        , details = [ "The " ++ collection.elements.countDescription ++ " of the " ++ collection.represents ++ " can be determined by looking at the code." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy checkInfo.parentRange
                            (String.fromInt collectionSize.min)
                        ]
                    )

            else
                Nothing


{-| On a "take" operation that returns a given number of elements _from the either side_
(for example `String.right` or `List.take`).
Checks:

    take n emptyCollection --> emptyCollection

    take n (take n collection) --> take n collection

    take nonPositiveLiteral collection --> emptyCollection

-}
collectionTakeChecks : CollectionProperties (EmptiableProperties ConstantProperties collection) -> IntoFnCheck
collectionTakeChecks collection =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck collection
        , operationDoesNotChangeResultOfOperationCheck
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case Normalize.getInt checkInfo checkInfo.firstArg of
                    Just elementCount ->
                        callWithNonPositiveIntCanBeReplacedByCheck
                            { int = elementCount
                            , intDescription = collection.elements.countDescription
                            , replacement = collection.empty.specific.asString
                            }
                            checkInfo

                    Nothing ->
                        Nothing
            )
        ]


emptiableFromListChecks : EmptiableProperties ConstantProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
emptiableFromListChecks collection checkInfo =
    callOnEmptyReturnsCheck { resultAsString = collection.empty.specific.asString } listCollection checkInfo


wrapperFromListSingletonChecks : WrapperProperties otherProperties -> IntoFnCheck
wrapperFromListSingletonChecks wrapper =
    { call =
        \checkInfo ->
            case listCollection.wrap.getValue checkInfo.lookupTable checkInfo.firstArg of
                Nothing ->
                    Nothing

                Just listSingletonValue ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " on a singleton list will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with the value inside"
                            , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ " with the value inside the singleton list." ]
                            }
                            checkInfo.fnRange
                            (Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) listSingletonValue
                            )
                        )
    , composition =
        \checkInfo ->
            if checkInfo.earlier.fn == listCollection.wrap.fn then
                Just
                    { info =
                        { message = qualifiedToString checkInfo.later.fn ++ " on a singleton list will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with the value inside"
                        , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ "." ]
                        }
                    , fix = compositionReplaceByFnFix wrapper.wrap.fn checkInfo
                    }

            else
                Nothing
    }


emptiableToListChecks :
    EmptiableProperties empty otherProperties
    -> CallCheckInfo
    -> Maybe (Error {})
emptiableToListChecks collection checkInfo =
    callOnEmptyReturnsCheck { resultAsString = listCollection.empty.specific.asString } collection checkInfo


{-| The partition checks

    partition f empty --> empty

    partition (\_ -> True) emptiable --> ( emptiable, empty )

    partition (\_ -> False) emptiable --> ( empty, emptiable )

If your function takes two arguments like `Dict.partition`, use `emptiablePartitionWithExtraArgChecks`.

-}
collectionPartitionChecks : TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties otherProperties)) -> CallCheckInfo -> Maybe (Error {})
collectionPartitionChecks collection checkInfo =
    partitionOnEmptyChecks collection checkInfo
        |> onNothing
            (\() ->
                case AstHelpers.getAlwaysResult checkInfo checkInfo.firstArg of
                    Just constantFunctionResult ->
                        partitionWithConstantFunctionResult constantFunctionResult collection checkInfo

                    Nothing ->
                        Nothing
            )


partitionOnEmptyChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
partitionOnEmptyChecks emptiable chheckInfo =
    callOnEmptyReturnsCheck
        { resultAsString = \res -> "( " ++ emptiable.empty.specific.asString res ++ ", " ++ emptiable.empty.specific.asString res ++ " )" }
        emptiable
        chheckInfo


partitionWithConstantFunctionResult : Node Expression -> TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
partitionWithConstantFunctionResult constantFunctionResult collection checkInfo =
    case Normalize.getBool checkInfo constantFunctionResult of
        Just True ->
            case secondArg checkInfo of
                Just (Node listArgRange _) ->
                    Just
                        (Rule.errorWithFix
                            { message = "All elements will go to the first " ++ collection.represents
                            , details = [ "Since the predicate function always returns True, the second " ++ collection.represents ++ " will always be " ++ collection.empty.specific.description ++ "." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = listArgRange.start } "( "
                            , Fix.insertAt listArgRange.end (", " ++ emptyAsString checkInfo collection ++ " )")
                            ]
                        )

                Nothing ->
                    Nothing

        Just False ->
            Just
                (Rule.errorWithFix
                    { message = "All elements will go to the second " ++ collection.represents
                    , details = [ "Since the predicate function always returns False, the first " ++ collection.represents ++ " will always be " ++ collection.empty.specific.description ++ "." ]
                    }
                    checkInfo.fnRange
                    (case secondArg checkInfo of
                        Just listArg ->
                            [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = (Node.range listArg).start } ("( " ++ emptyAsString checkInfo collection ++ ", ")
                            , Fix.insertAt (Node.range listArg).end " )"
                            ]

                        Nothing ->
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                ("("
                                    ++ qualifiedToString (qualify Fn.Tuple.pair checkInfo)
                                    ++ " "
                                    ++ emptyAsString checkInfo collection
                                    ++ ")"
                                )
                            ]
                    )
                )

        Nothing ->
            Nothing


{-| partition checks where the function takes two arguments

    partition f empty --> empty

    partition (\_ _ -> True) emptiable --> ( emptiable, empty )

    partition (\_ _ -> False) emptiable --> ( empty, emptiable )

If your function only takes one argument like `List.partition`, use `collectionPartitionChecks`

-}
emptiablePartitionWithExtraArgChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
emptiablePartitionWithExtraArgChecks emptiable checkInfo =
    partitionOnEmptyChecks emptiable checkInfo
        |> onNothing
            (\() ->
                let
                    maybePartitionFunctionResult : Maybe (Node Expression)
                    maybePartitionFunctionResult =
                        checkInfo.firstArg
                            |> AstHelpers.getAlwaysResult checkInfo
                            |> Maybe.andThen (\result -> AstHelpers.getAlwaysResult checkInfo result)
                in
                case maybePartitionFunctionResult of
                    Just constantFunctionResult ->
                        partitionWithConstantFunctionResult constantFunctionResult emptiable checkInfo

                    Nothing ->
                        Nothing
            )


type alias CollectionSize =
    { min : Int, max : Maybe Int }


collectionSizeUnknown : CollectionSize
collectionSizeUnknown =
    { min = 0, max = Nothing }


collectionSizeExact : Int -> CollectionSize
collectionSizeExact n =
    { min = n, max = Just n }


collectionSizeAdd1 : CollectionSize -> CollectionSize
collectionSizeAdd1 collectionSize =
    { min = collectionSize.min + 1
    , max = Maybe.map (\max -> max + 1) collectionSize.max
    }


collectionSizeCombineEachBoundWith : (Int -> Int -> Int) -> CollectionSize -> CollectionSize -> CollectionSize
collectionSizeCombineEachBoundWith combineABBounds aCollectionSize bCollectionSize =
    { min = combineABBounds aCollectionSize.min bCollectionSize.min
    , max = Maybe.map2 combineABBounds aCollectionSize.max bCollectionSize.max
    }


collectionSizesMin : CollectionSize -> CollectionSize -> CollectionSize
collectionSizesMin aCollectionSize bCollectionSize =
    { min = Basics.min aCollectionSize.min bCollectionSize.min
    , max = collectionMaxSizesMin aCollectionSize.max bCollectionSize.max
    }


collectionMaxSizesMin : Maybe Int -> Maybe Int -> Maybe Int
collectionMaxSizesMin aCollectionMaxSize bCollectionMaxSize =
    case aCollectionMaxSize of
        Nothing ->
            bCollectionMaxSize

        Just aCollectionSizeMax ->
            case bCollectionMaxSize of
                Nothing ->
                    aCollectionMaxSize

                Just bCollectionSizeMax ->
                    Just (Basics.min aCollectionSizeMax bCollectionSizeMax)


numberBoundsToCollectionSize : { min : Float, max : Float } -> CollectionSize
numberBoundsToCollectionSize numberBounds =
    { min = numberBoundToCollectionSizeMin numberBounds.min
    , max = numberBoundToCollectionSizeMax numberBounds.max
    }


numberBoundToCollectionSizeMin : Float -> Int
numberBoundToCollectionSizeMin numberBound =
    floor (max 0 numberBound)


numberBoundToCollectionSizeMax : Float -> Maybe Int
numberBoundToCollectionSizeMax numberBound =
    if numberBound == positiveInfinity then
        Nothing

    else
        Just (floor (max 0 numberBound))


collectionSizeToNumberBounds : CollectionSize -> { min : Float, max : Float }
collectionSizeToNumberBounds collectionSize =
    { min = Basics.toFloat collectionSize.min
    , max =
        case collectionSize.max of
            Nothing ->
                positiveInfinity

            Just max ->
                Basics.toFloat max
    }


determineCollectionSize : Normalize.Resources res -> Node Expression -> CollectionSize
determineCollectionSize resources expressionNode =
    normalDetermineCollectionSize
        (Normalize.normalizeExpression resources expressionNode)


normalDetermineCollectionSize : Expression -> CollectionSize
normalDetermineCollectionSize expression =
    case expression of
        Expression.ListExpr elements ->
            collectionSizeExact (List.length elements)

        Expression.OperatorApplication operator _ (Node _ left) (Node _ right) ->
            case operator of
                "::" ->
                    collectionSizeAdd1 (normalDetermineCollectionSize right)

                "++" ->
                    collectionSizeCombineEachBoundWith (+)
                        (normalDetermineCollectionSize left)
                        (normalDetermineCollectionSize right)

                _ ->
                    collectionSizeUnknown

        Expression.Literal string ->
            collectionSizeExact (String.length string)

        Expression.FunctionOrValue qualification name ->
            normalFnOrFnCallDetermineCollectionSize ( qualification, name ) []

        Expression.Application ((Node _ (Expression.FunctionOrValue qualification name)) :: args) ->
            normalFnOrFnCallDetermineCollectionSize ( qualification, name ) args

        Expression.LetExpression letIn ->
            normalDetermineCollectionSize (Node.value letIn.expression)

        Expression.IfBlock _ (Node _ onTrue) (Node _ onFalse) ->
            let
                onTrueCollectionSize : CollectionSize
                onTrueCollectionSize =
                    normalDetermineCollectionSize onTrue

                onFalseCollectionSize : CollectionSize
                onFalseCollectionSize =
                    normalDetermineCollectionSize onFalse
            in
            { min = Basics.min onTrueCollectionSize.min onFalseCollectionSize.min
            , max = Maybe.map2 Basics.max onTrueCollectionSize.max onFalseCollectionSize.max
            }

        Expression.CaseExpression caseOf ->
            case caseOf.cases of
                ( _, Node _ case0Result ) :: case1Up ->
                    List.foldl
                        (\( _, Node _ caseResult ) soFar ->
                            let
                                caseResultCollectionSize : CollectionSize
                                caseResultCollectionSize =
                                    normalDetermineCollectionSize caseResult
                            in
                            { min = Basics.min caseResultCollectionSize.min soFar.min
                            , max = Maybe.map2 Basics.max caseResultCollectionSize.max soFar.max
                            }
                        )
                        (normalDetermineCollectionSize case0Result)
                        case1Up

                -- invalid syntax
                [] ->
                    collectionSizeUnknown

        _ ->
            collectionSizeUnknown


normalFnOrFnCallDetermineCollectionSize : ( ModuleName, String ) -> List (Node Expression) -> CollectionSize
normalFnOrFnCallDetermineCollectionSize fn args =
    case Dict.get fn normalFnOrFnCallDetermineCollectionSizeDict of
        Just determineFnOrFnCallCollectionSize ->
            determineFnOrFnCallCollectionSize args

        Nothing ->
            collectionSizeUnknown


normalFnOrFnCallDetermineCollectionSizeDict : Dict ( ModuleName, String ) (List (Node Expression) -> CollectionSize)
normalFnOrFnCallDetermineCollectionSizeDict =
    Dict.fromList
        [ -- List
          ( Fn.List.repeat
          , \args ->
                case args of
                    (Node _ repeatCountArg) :: _ ->
                        numberBoundsToCollectionSize
                            (normalGetNumberBounds repeatCountArg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.List.take
          , \args ->
                case args of
                    [ Node _ countArg, Node _ listArg ] ->
                        collectionSizesMin
                            (numberBoundsToCollectionSize (normalGetNumberBounds countArg))
                            (normalDetermineCollectionSize listArg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.List.drop
          , \args ->
                case args of
                    [ Node _ countArg, Node _ listArg ] ->
                        numberBoundsToCollectionSize
                            (numberBoundsCombineEachBoundWith (+)
                                (collectionSizeToNumberBounds (normalDetermineCollectionSize listArg))
                                (numberBoundsNegate (normalGetNumberBounds countArg))
                            )

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.List.range
          , \args ->
                case args of
                    [ Node _ rangeStartArg, Node _ rangeEndArg ] ->
                        let
                            rangeStartBounds : { min : Float, max : Float }
                            rangeStartBounds =
                                normalGetNumberBounds rangeStartArg

                            rangeEndExclusiveBounds : { min : Float, max : Float }
                            rangeEndExclusiveBounds =
                                normalGetNumberBounds rangeEndArg
                                    |> -- because e.g List.length (List.range 1 1) is 1, not 0
                                       numberBoundsAlterEach (\n -> n + 1)
                        in
                        numberBoundsToCollectionSize
                            (numberBoundsCombineEachBoundWith (+)
                                rangeEndExclusiveBounds
                                (numberBoundsNegate rangeStartBounds)
                            )

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.List.intersperse
          , \args ->
                case args of
                    [ _, Node _ listArg ] ->
                        let
                            collectionSizeBeforeIntersperse : CollectionSize
                            collectionSizeBeforeIntersperse =
                                normalDetermineCollectionSize listArg
                        in
                        { min = Basics.max 0 (2 * collectionSizeBeforeIntersperse.min - 1)
                        , max =
                            Maybe.map (\max -> Basics.max 0 (2 * max - 1))
                                collectionSizeBeforeIntersperse.max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.List.append
          , \args ->
                case args of
                    [ Node _ leftArg, Node _ rightArg ] ->
                        collectionSizeCombineEachBoundWith (+)
                            (normalDetermineCollectionSize leftArg)
                            (normalDetermineCollectionSize rightArg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.List.sort
          , normalArgsDetermineCollectionSizeAtIndex 0
          )
        , ( Fn.List.sortBy
          , normalArgsDetermineCollectionSizeAtIndex 1
          )
        , ( Fn.List.sortWith
          , normalArgsDetermineCollectionSizeAtIndex 1
          )
        , ( Fn.List.reverse
          , normalArgsDetermineCollectionSizeAtIndex 0
          )
        , ( Fn.List.map
          , normalArgsDetermineCollectionSizeAtIndex 1
          )
        , ( Fn.List.indexedMap
          , normalArgsDetermineCollectionSizeAtIndex 1
          )
        , ( Fn.List.map2
          , \args ->
                case args of
                    [ _, Node _ list0Arg, Node _ list1Arg ] ->
                        collectionSizesMin
                            (normalDetermineCollectionSize list0Arg)
                            (normalDetermineCollectionSize list1Arg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.List.map3
          , \args ->
                case args of
                    [ _, Node _ list0Arg, Node _ list1Arg, Node _ list2Arg ] ->
                        normalDetermineCollectionSize list0Arg
                            |> collectionSizesMin
                                (normalDetermineCollectionSize list1Arg)
                            |> collectionSizesMin
                                (normalDetermineCollectionSize list2Arg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.List.map4
          , \args ->
                case args of
                    [ _, Node _ list0Arg, Node _ list1Arg, Node _ list2Arg, Node _ list3Arg ] ->
                        normalDetermineCollectionSize list0Arg
                            |> collectionSizesMin
                                (normalDetermineCollectionSize list1Arg)
                            |> collectionSizesMin
                                (normalDetermineCollectionSize list2Arg)
                            |> collectionSizesMin
                                (normalDetermineCollectionSize list3Arg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.List.map5
          , \args ->
                case args of
                    [ _, Node _ list0Arg, Node _ list1Arg, Node _ list2Arg, Node _ list3Arg, Node _ list4Arg ] ->
                        normalDetermineCollectionSize list0Arg
                            |> collectionSizesMin
                                (normalDetermineCollectionSize list1Arg)
                            |> collectionSizesMin
                                (normalDetermineCollectionSize list2Arg)
                            |> collectionSizesMin
                                (normalDetermineCollectionSize list3Arg)
                            |> collectionSizesMin
                                (normalDetermineCollectionSize list4Arg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.String.lines
          , \args ->
                case args of
                    [ Node _ stringArg ] ->
                        { min = 1
                        , max =
                            Maybe.map (\max -> max + 1)
                                (normalDetermineCollectionSize stringArg).max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.String.words
          , \args ->
                case args of
                    [ Node _ stringArg ] ->
                        { min = 1
                        , max =
                            Maybe.map (\max -> max + 1)
                                (normalDetermineCollectionSize stringArg).max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Array.toList
          , normalArgsDetermineCollectionSizeAtIndex 0
          )
        , ( Fn.Array.toIndexedList
          , normalArgsDetermineCollectionSizeAtIndex 0
          )
        , ( Fn.Set.toList
          , normalArgsDetermineCollectionSizeAtIndex 0
          )
        , ( Fn.Dict.toList
          , normalArgsDetermineCollectionSizeAtIndex 0
          )
        , ( Fn.String.toList
          , \args ->
                case args of
                    [ Node _ stringArg ] ->
                        let
                            stringLength : CollectionSize
                            stringLength =
                                normalDetermineCollectionSize stringArg
                        in
                        { min = stringLength.min // 2
                        , max = stringLength.max
                        }

                    _ ->
                        collectionSizeUnknown
          )

        -- Array
        , ( Fn.Array.empty, \_ -> collectionSizeExact 0 )
        , ( Fn.Array.fromList
          , normalArgsDetermineCollectionSizeAtIndex 0
          )
        , ( Fn.Array.repeat
          , \args ->
                case args of
                    (Node _ lengthArg) :: _ ->
                        numberBoundsToCollectionSize
                            (normalGetNumberBounds lengthArg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Array.initialize
          , \args ->
                case args of
                    (Node _ lengthArg) :: _ ->
                        numberBoundsToCollectionSize
                            (normalGetNumberBounds lengthArg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Array.push
          , \args ->
                case args of
                    [ _, Node _ arrayArg ] ->
                        collectionSizeAdd1 (normalDetermineCollectionSize arrayArg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Array.append
          , \args ->
                case args of
                    [ Node _ leftArg, Node _ rightArg ] ->
                        collectionSizeCombineEachBoundWith (+)
                            (normalDetermineCollectionSize leftArg)
                            (normalDetermineCollectionSize rightArg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Array.map
          , normalArgsDetermineCollectionSizeAtIndex 1
          )
        , ( Fn.Array.indexedMap
          , normalArgsDetermineCollectionSizeAtIndex 1
          )

        -- Set
        , ( Fn.Set.empty, \_ -> collectionSizeExact 0 )
        , ( Fn.Set.singleton, \_ -> collectionSizeExact 1 )
        , ( Fn.Set.insert
          , \args ->
                case args of
                    [ _, Node _ setArg ] ->
                        let
                            sizeBeforeInsert : CollectionSize
                            sizeBeforeInsert =
                                normalDetermineCollectionSize setArg
                        in
                        { min = Basics.max 1 sizeBeforeInsert.min
                        , max = Maybe.map (\max -> max + 1) sizeBeforeInsert.max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Set.remove
          , \args ->
                case args of
                    [ _, Node _ setArg ] ->
                        let
                            sizeBeforeRemove : CollectionSize
                            sizeBeforeRemove =
                                normalDetermineCollectionSize setArg
                        in
                        { min = Basics.max 0 (sizeBeforeRemove.min - 1)
                        , max = sizeBeforeRemove.max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Set.fromList
          , \args ->
                case args of
                    [ Node _ listArg ] ->
                        (case normalListGetElements listArg of
                            Just listElements ->
                                case traverse (\(Node _ element) -> expressionNormalToComparable element) listElements.known of
                                    Just comparableListElements ->
                                        let
                                            uniqueKnownElementCount : Int
                                            uniqueKnownElementCount =
                                                comparableListElements |> countUnique
                                        in
                                        if listElements.allKnown then
                                            Just (collectionSizeExact uniqueKnownElementCount)

                                        else
                                            Just { min = uniqueKnownElementCount, max = Nothing }

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing
                        )
                            |> maybeWithDefaultLazy
                                (\() ->
                                    let
                                        listLength : CollectionSize
                                        listLength =
                                            normalDetermineCollectionSize listArg
                                    in
                                    { min = min 1 listLength.min, max = listLength.max }
                                )

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Set.union
          , \args ->
                case args of
                    [ Node _ aSetArg, Node _ bSetArg ] ->
                        let
                            aSetSize : CollectionSize
                            aSetSize =
                                normalDetermineCollectionSize aSetArg

                            bSetSize : CollectionSize
                            bSetSize =
                                normalDetermineCollectionSize bSetArg
                        in
                        { min = Basics.max aSetSize.min bSetSize.min
                        , max = Maybe.map2 (+) aSetSize.max bSetSize.max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Set.intersect
          , \args ->
                case args of
                    [ Node _ aSetArg, Node _ bSetArg ] ->
                        { min = 0
                        , max =
                            collectionMaxSizesMin
                                (normalDetermineCollectionSize aSetArg).max
                                (normalDetermineCollectionSize bSetArg).max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Set.map
          , \args ->
                case args of
                    [ _, Node _ stringArg ] ->
                        let
                            originalSetLength : CollectionSize
                            originalSetLength =
                                normalDetermineCollectionSize stringArg
                        in
                        { min = Basics.min 1 originalSetLength.min
                        , max = originalSetLength.max
                        }

                    _ ->
                        collectionSizeUnknown
          )

        -- Dict
        , ( Fn.Dict.empty, \_ -> collectionSizeExact 0 )
        , ( Fn.Dict.singleton, \_ -> collectionSizeExact 1 )
        , ( Fn.Dict.insert
          , \args ->
                case args of
                    [ _, Node _ dictArg ] ->
                        let
                            sizeBeforeInsert : CollectionSize
                            sizeBeforeInsert =
                                normalDetermineCollectionSize dictArg
                        in
                        { min = Basics.max 1 sizeBeforeInsert.min
                        , max = Maybe.map (\max -> max + 1) sizeBeforeInsert.max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Dict.remove
          , \args ->
                case args of
                    [ _, Node _ dictArg ] ->
                        let
                            sizeBeforeRemove : CollectionSize
                            sizeBeforeRemove =
                                normalDetermineCollectionSize dictArg
                        in
                        { min = Basics.max 0 (sizeBeforeRemove.min - 1)
                        , max = sizeBeforeRemove.max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Dict.fromList
          , \args ->
                case args of
                    [ Node _ listArg ] ->
                        (case normalListGetElements listArg of
                            Just listElements ->
                                case traverse (\(Node _ element) -> normalGetTupleComparableFirst element) listElements.known of
                                    Just comparableKeyExpressions ->
                                        let
                                            uniqueKeyCount : Int
                                            uniqueKeyCount =
                                                comparableKeyExpressions |> countUnique
                                        in
                                        if listElements.allKnown then
                                            Just (collectionSizeExact uniqueKeyCount)

                                        else
                                            Just { min = uniqueKeyCount, max = Nothing }

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing
                        )
                            |> maybeWithDefaultLazy
                                (\() ->
                                    let
                                        listLength : CollectionSize
                                        listLength =
                                            normalDetermineCollectionSize listArg
                                    in
                                    { min = min 1 listLength.min, max = listLength.max }
                                )

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Dict.union
          , \args ->
                case args of
                    [ Node _ aDictArg, Node _ bDictArg ] ->
                        let
                            aDictSize : CollectionSize
                            aDictSize =
                                normalDetermineCollectionSize aDictArg

                            bDictSize : CollectionSize
                            bDictSize =
                                normalDetermineCollectionSize bDictArg
                        in
                        { min = Basics.max aDictSize.min bDictSize.min
                        , max = Maybe.map2 (+) aDictSize.max bDictSize.max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Dict.intersect
          , \args ->
                case args of
                    [ Node _ aDictArg, Node _ bDictArg ] ->
                        { min = 0
                        , max =
                            collectionMaxSizesMin
                                (normalDetermineCollectionSize aDictArg).max
                                (normalDetermineCollectionSize bDictArg).max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.Dict.map
          , normalArgsDetermineCollectionSizeAtIndex 1
          )

        -- String
        , ( Fn.String.fromChar, \_ -> { min = 1, max = Just 2 } )
        , ( Fn.String.fromInt, \_ -> { min = 1, max = Nothing } )
        , ( Fn.String.fromFloat, \_ -> { min = 1, max = Nothing } )
        , ( Fn.String.fromList
          , \args ->
                case args of
                    [ Node _ listArg ] ->
                        let
                            charCount : CollectionSize
                            charCount =
                                normalDetermineCollectionSize listArg
                        in
                        { min = charCount.min
                        , max = Maybe.map (\max -> max * 2) charCount.max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.String.append
          , \args ->
                case args of
                    [ Node _ leftArg, Node _ rightArg ] ->
                        collectionSizeCombineEachBoundWith (+)
                            (normalDetermineCollectionSize leftArg)
                            (normalDetermineCollectionSize rightArg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.String.cons
          , \args ->
                case args of
                    [ _, Node _ stringArg ] ->
                        let
                            tailCollectionSize : CollectionSize
                            tailCollectionSize =
                                normalDetermineCollectionSize stringArg
                        in
                        { min = tailCollectionSize.min + 1
                        , max = Maybe.map (\max -> max + 2) tailCollectionSize.max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.String.repeat
          , \args ->
                case args of
                    [ Node _ countArg, Node _ stringArg ] ->
                        collectionSizeCombineEachBoundWith (*)
                            (normalDetermineCollectionSize stringArg)
                            (numberBoundsToCollectionSize
                                (normalGetNumberBounds countArg)
                            )

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.String.left
          , \args ->
                case args of
                    [ Node _ countArg, Node _ stringArg ] ->
                        collectionSizesMin
                            (numberBoundsToCollectionSize (normalGetNumberBounds countArg))
                            (normalDetermineCollectionSize stringArg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.String.right
          , \args ->
                case args of
                    [ Node _ countArg, Node _ stringArg ] ->
                        collectionSizesMin
                            (numberBoundsToCollectionSize (normalGetNumberBounds countArg))
                            (normalDetermineCollectionSize stringArg)

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.String.dropLeft
          , \args ->
                case args of
                    [ Node _ countArg, Node _ stringArg ] ->
                        numberBoundsToCollectionSize
                            (numberBoundsCombineEachBoundWith (+)
                                (collectionSizeToNumberBounds (normalDetermineCollectionSize stringArg))
                                (numberBoundsNegate (normalGetNumberBounds countArg))
                            )

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.String.dropRight
          , \args ->
                case args of
                    [ Node _ countArg, Node _ stringArg ] ->
                        numberBoundsToCollectionSize
                            (numberBoundsCombineEachBoundWith (+)
                                (collectionSizeToNumberBounds (normalDetermineCollectionSize stringArg))
                                (numberBoundsNegate (normalGetNumberBounds countArg))
                            )

                    _ ->
                        collectionSizeUnknown
          )
        , ( Fn.String.reverse
          , normalArgsDetermineCollectionSizeAtIndex 0
          )
        , ( Fn.String.map
          , \args ->
                case args of
                    [ _, Node _ stringArg ] ->
                        let
                            originalStringLength : CollectionSize
                            originalStringLength =
                                normalDetermineCollectionSize stringArg
                        in
                        { min = originalStringLength.min
                        , max = Maybe.map (\max -> max * 2) originalStringLength.max
                        }

                    _ ->
                        collectionSizeUnknown
          )
        ]


normalArgsDetermineCollectionSizeAtIndex : Int -> List (Node Expression) -> CollectionSize
normalArgsDetermineCollectionSizeAtIndex collectionIndex =
    \args ->
        case List.drop collectionIndex args of
            (Node _ collectionArg) :: _ ->
                normalDetermineCollectionSize collectionArg

            _ ->
                collectionSizeUnknown


replaceSingleElementListBySingleValue : ModuleNameLookupTable -> Node Expression -> Maybe (List Fix)
replaceSingleElementListBySingleValue lookupTable expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.ListExpr (listElement :: []) ->
            Just (replaceBySubExpressionFix (Node.range expressionNode) listElement)

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ "singleton")) :: _ :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable fnRange of
                Just [ "List" ] ->
                    Just [ Fix.removeRange fnRange ]

                _ ->
                    Nothing

        Expression.IfBlock _ thenBranch elseBranch ->
            combineSingleElementFixes lookupTable [ thenBranch, elseBranch ] []

        Expression.CaseExpression caseOf ->
            combineSingleElementFixes lookupTable (List.map Tuple.second caseOf.cases) []

        _ ->
            Nothing


combineSingleElementFixes : ModuleNameLookupTable -> List (Node Expression) -> List Fix -> Maybe (List Fix)
combineSingleElementFixes lookupTable nodes soFar =
    case nodes of
        [] ->
            Just soFar

        node :: restOfNodes ->
            case replaceSingleElementListBySingleValue lookupTable node of
                Nothing ->
                    Nothing

                Just fixes ->
                    combineSingleElementFixes lookupTable restOfNodes (fixes ++ soFar)



-- RECORD UPDATE


recordUpdateChecks : Range -> Node String -> List (Node Expression.RecordSetter) -> Maybe (Error {})
recordUpdateChecks recordUpdateRange recordVariable fields =
    case findMapNeighboring (getUnnecessaryRecordUpdateSetter (Node.value recordVariable)) fields of
        Just unnecessarySetterAndNeighbors ->
            Just
                (Rule.errorWithFix
                    { message = "Unnecessary field assignment"
                    , details = [ "The field is being set to its own value." ]
                    }
                    unnecessarySetterAndNeighbors.found.valueAccessRange
                    (case unnecessarySetterAndNeighbors.before of
                        Just (Node prevRange _) ->
                            [ Fix.removeRange { start = prevRange.end, end = unnecessarySetterAndNeighbors.found.setterRange.end } ]

                        Nothing ->
                            case unnecessarySetterAndNeighbors.after of
                                Nothing ->
                                    -- it's the only setter
                                    keepOnlyFix { parentRange = recordUpdateRange, keep = Node.range recordVariable }

                                Just (Node afterRange _) ->
                                    -- It's the first setter, so we can remove until the second setter
                                    [ Fix.removeRange { start = unnecessarySetterAndNeighbors.found.setterRange.start, end = afterRange.start } ]
                    )
                )

        Nothing ->
            Nothing


getUnnecessaryRecordUpdateSetter : String -> Node ( Node String, Node Expression ) -> Maybe { valueAccessRange : Range, setterRange : Range }
getUnnecessaryRecordUpdateSetter recordVariableName (Node setterRange ( Node _ field, valueNode )) =
    case AstHelpers.getAccessingRecord valueNode of
        Just accessingRecord ->
            case accessingRecord.record of
                Node _ (Expression.FunctionOrValue [] recordVariable) ->
                    if field == accessingRecord.field && recordVariableName == recordVariable then
                        Just { setterRange = setterRange, valueAccessRange = accessingRecord.range }

                    else
                        Nothing

                _ ->
                    Nothing

        Nothing ->
            Nothing



-- IF


type alias IfCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , importLookup : ImportLookup
    , importCustomTypes :
        Dict
            ModuleName
            (Dict
                String
                { variantNames : Set String
                , allParametersAreUsedInVariants : Bool
                }
            )
    , moduleCustomTypes :
        Dict
            String
            { variantNames : Set String
            , allParametersAreUsedInVariants : Bool
            }
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , nodeRange : Range
    , condition : Node Expression
    , trueBranch : Node Expression
    , falseBranch : Node Expression
    }


targetIfKeyword : Range -> Range
targetIfKeyword ifExpressionRange =
    { start = ifExpressionRange.start
    , end =
        { row = ifExpressionRange.start.row
        , column = ifExpressionRange.start.column + 2
        }
    }


ifChecks : IfCheckInfo -> Maybe (Error {})
ifChecks checkInfo =
    (case Normalize.getBool checkInfo checkInfo.condition of
        Just determinedConditionResultIsTrue ->
            let
                branch : { expressionNode : Node Expression, name : String }
                branch =
                    if determinedConditionResultIsTrue then
                        { expressionNode = checkInfo.trueBranch, name = "then" }

                    else
                        { expressionNode = checkInfo.falseBranch, name = "else" }
            in
            Just
                (Rule.errorWithFix
                    { message = "The condition will always evaluate to " ++ AstHelpers.boolToString determinedConditionResultIsTrue
                    , details = [ "The expression can be replaced by what is inside the '" ++ branch.name ++ "' branch." ]
                    }
                    (targetIfKeyword checkInfo.nodeRange)
                    (replaceBySubExpressionFix checkInfo.nodeRange branch.expressionNode)
                )

        Nothing ->
            Nothing
    )
        |> onNothing
            (\() ->
                case Normalize.getBool checkInfo checkInfo.trueBranch of
                    Just True ->
                        case Normalize.getBool checkInfo checkInfo.falseBranch of
                            Just False ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "The if expression's value is the same as the condition"
                                        , details = [ "The expression can be replaced by the condition." ]
                                        }
                                        (targetIfKeyword checkInfo.nodeRange)
                                        (replaceBySubExpressionFix checkInfo.nodeRange checkInfo.condition)
                                    )

                            _ ->
                                Nothing

                    Just False ->
                        case Normalize.getBool checkInfo checkInfo.falseBranch of
                            Just True ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "The if expression's value is the inverse of the condition"
                                        , details = [ "The expression can be replaced by the condition wrapped by `not`." ]
                                        }
                                        (targetIfKeyword checkInfo.nodeRange)
                                        (Fix.insertAt checkInfo.nodeRange.start
                                            (qualifiedToString (qualify Fn.Basics.not checkInfo) ++ " ")
                                            :: replaceBySubExpressionFix checkInfo.nodeRange checkInfo.condition
                                        )
                                    )

                            _ ->
                                Nothing

                    Nothing ->
                        Nothing
            )
        |> onNothing
            (\() ->
                case Normalize.compare checkInfo checkInfo.trueBranch checkInfo.falseBranch of
                    Normalize.ConfirmedEquality ->
                        Just
                            (Rule.errorWithFix
                                { message = "The values in both branches is the same."
                                , details = [ "The expression can be replaced by the contents of either branch." ]
                                }
                                (targetIfKeyword checkInfo.nodeRange)
                                (replaceBySubExpressionFix checkInfo.nodeRange checkInfo.trueBranch)
                            )

                    _ ->
                        Nothing
            )



-- CASE OF


caseOfChecks : CaseOfCheckInfo -> Maybe (Error {})
caseOfChecks checkInfo =
    sameBodyForCaseOfChecks checkInfo
        |> onNothing (\() -> booleanCaseOfChecks checkInfo)
        |> onNothing (\() -> destructuringCaseOfChecks checkInfo)
        |> onNothing (\() -> caseOfWithUnreachableCasesChecks checkInfo)
        |> onNothing (\() -> caseElementCountOf0Checks checkInfo)


caseElementCountOf0Checks : CaseOfCheckInfo -> Maybe (Error {})
caseElementCountOf0Checks checkInfo =
    case checkInfo.caseOf.cases of
        [ ( firstCasePattern, firstCaseResult ), ( lastCasePattern, lastCaseResult ) ] ->
            if
                (AstHelpers.patternGetInt firstCasePattern == Just 0)
                    && (Node.value (AstHelpers.removeParensFromPattern lastCasePattern) == Pattern.AllPattern)
            then
                case AstHelpers.getUnreducedValueOrFnOrFnCall checkInfo.caseOf.expression of
                    Nothing ->
                        Nothing

                    Just call ->
                        case ModuleNameLookupTable.moduleNameAt checkInfo.lookupTable call.fnRange of
                            Nothing ->
                                Nothing

                            Just callFn ->
                                case
                                    compareElementCountChecks
                                        { isEmpty = True
                                        , operation = \fn -> fn ++ " matched against 0"
                                        , replaceByInstruction =
                                            \descriptions ->
                                                "replace this operation by an if-then-else testing for "
                                                    ++ descriptions.replacementOperation
                                                    ++ " on the "
                                                    ++ descriptions.collection
                                                    ++ " given to the "
                                                    ++ descriptions.elementCountFn
                                                    ++ " call, and returning the result of the case matching 0 in the then branch and the result of the branch matching on _ in the else branch"
                                        , fn = ( callFn, call.fnName )
                                        }
                                of
                                    Nothing ->
                                        Nothing

                                    Just error ->
                                        let
                                            thisCaseKeywordRange : Range
                                            thisCaseKeywordRange =
                                                caseKeyWordRange checkInfo.parentRange
                                        in
                                        Just
                                            (Rule.errorWithFix
                                                { message = error.message, details = error.details }
                                                thisCaseKeywordRange
                                                [ Fix.replaceRangeBy thisCaseKeywordRange "if"
                                                , Fix.replaceRangeBy call.fnRange
                                                    (qualifiedToString (qualify error.isEmptyFn checkInfo))
                                                , Fix.replaceRangeBy
                                                    { start = (Node.range checkInfo.caseOf.expression).end
                                                    , end = (Node.range firstCaseResult).start
                                                    }
                                                    (" then\n" ++ String.repeat ((Node.range firstCaseResult).start.column - 1) " ")
                                                , Fix.replaceRangeBy
                                                    { start = (Node.range firstCaseResult).end
                                                    , end = (Node.range lastCaseResult).start
                                                    }
                                                    ("\n\n"
                                                        ++ String.repeat (thisCaseKeywordRange.start.column - 1) " "
                                                        ++ "else\n"
                                                        ++ String.repeat ((Node.range lastCaseResult).start.column - 1) " "
                                                    )
                                                ]
                                            )

            else
                Nothing

        _ ->
            Nothing


type alias CaseOfCheckInfo =
    QualifyResources
        { lookupTable : ModuleNameLookupTable
        , customTypesToReportInCases : Set ( ModuleName, ConstructorName )
        , moduleCustomTypes :
            Dict
                String
                { variantNames : Set String
                , allParametersAreUsedInVariants : Bool
                }
        , importCustomTypes :
            Dict
                ModuleName
                (Dict
                    String
                    { variantNames : Set String
                    , allParametersAreUsedInVariants : Bool
                    }
                )
        , extractSourceCode : Range -> String
        , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
        , parentRange : Range
        , caseOf : Expression.CaseBlock
        }


sameBodyForCaseOfChecks :
    CaseOfCheckInfo
    -> Maybe (Error {})
sameBodyForCaseOfChecks context =
    case context.caseOf.cases of
        [] ->
            Nothing

        ( firstPattern, firstBody ) :: rest ->
            let
                restPatterns : List (Node Pattern)
                restPatterns =
                    List.map Tuple.first rest
            in
            if
                introducesVariableOrUsesTypeConstructor context (firstPattern :: restPatterns)
                    || not (Normalize.areAllTheSameAs context firstBody Tuple.second rest)
            then
                Nothing

            else
                let
                    firstBodyRange : Range
                    firstBodyRange =
                        Node.range firstBody
                in
                Just
                    (Rule.errorWithFix
                        { message = "Unnecessary case expression"
                        , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                        }
                        (caseKeyWordRange context.parentRange)
                        [ Fix.removeRange { start = context.parentRange.start, end = firstBodyRange.start }
                        , Fix.removeRange { start = firstBodyRange.end, end = context.parentRange.end }
                        ]
                    )


caseKeyWordRange : Range -> Range
caseKeyWordRange range =
    { start = range.start
    , end = { row = range.start.row, column = range.start.column + 4 }
    }


introducesVariableOrUsesTypeConstructor :
    { a | lookupTable : ModuleNameLookupTable, customTypesToReportInCases : Set ( ModuleName, ConstructorName ) }
    -> List (Node Pattern)
    -> Bool
introducesVariableOrUsesTypeConstructor resources nodesToLookAt =
    case nodesToLookAt of
        [] ->
            False

        node :: remaining ->
            case Node.value node of
                Pattern.VarPattern _ ->
                    True

                Pattern.RecordPattern _ ->
                    True

                Pattern.AsPattern _ _ ->
                    True

                Pattern.ParenthesizedPattern pattern ->
                    introducesVariableOrUsesTypeConstructor resources (pattern :: remaining)

                Pattern.TuplePattern nodes ->
                    introducesVariableOrUsesTypeConstructor resources (nodes ++ remaining)

                Pattern.UnConsPattern first rest ->
                    introducesVariableOrUsesTypeConstructor resources (first :: rest :: remaining)

                Pattern.ListPattern nodes ->
                    introducesVariableOrUsesTypeConstructor resources (nodes ++ remaining)

                Pattern.NamedPattern variantQualified nodes ->
                    case ModuleNameLookupTable.fullModuleNameFor resources.lookupTable node of
                        Just moduleName ->
                            if Set.member ( moduleName, variantQualified.name ) resources.customTypesToReportInCases then
                                introducesVariableOrUsesTypeConstructor resources (nodes ++ remaining)

                            else
                                True

                        Nothing ->
                            True

                _ ->
                    introducesVariableOrUsesTypeConstructor resources remaining


booleanCaseOfChecks : CaseOfCheckInfo -> Maybe (Error {})
booleanCaseOfChecks checkInfo =
    case checkInfo.caseOf.cases of
        ( firstPattern, Node firstRange _ ) :: ( Node secondPatternRange _, Node secondExprRange _ ) :: [] ->
            case AstHelpers.getBoolPattern checkInfo.lookupTable firstPattern of
                Just isTrueFirst ->
                    let
                        expressionRange : Range
                        expressionRange =
                            Node.range checkInfo.caseOf.expression
                    in
                    Just
                        (Rule.errorWithFix
                            { message = "Replace `case..of` by an `if` condition"
                            , details =
                                [ "The idiomatic way to check for a condition is to use an `if` expression."
                                , "Read more about it at: https://guide.elm-lang.org/core_language.html#if-expressions"
                                ]
                            }
                            (Node.range firstPattern)
                            (if isTrueFirst then
                                [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = expressionRange.start } "if "
                                , Fix.replaceRangeBy { start = expressionRange.end, end = firstRange.start } " then "
                                , Fix.replaceRangeBy { start = secondPatternRange.start, end = secondExprRange.start } "else "
                                ]

                             else
                                [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = expressionRange.start } "if not ("
                                , Fix.replaceRangeBy { start = expressionRange.end, end = firstRange.start } ") then "
                                , Fix.replaceRangeBy { start = secondPatternRange.start, end = secondExprRange.start } "else "
                                ]
                            )
                        )

                Nothing ->
                    Nothing

        _ ->
            Nothing


destructuringCaseOfChecks :
    CaseOfCheckInfo
    -> Maybe (Error {})
destructuringCaseOfChecks checkInfo =
    case checkInfo.caseOf.cases of
        ( rawSinglePattern, Node bodyRange _ ) :: [] ->
            let
                singlePattern : Node Pattern
                singlePattern =
                    AstHelpers.removeParensFromPattern rawSinglePattern
            in
            if isSimpleDestructurePattern singlePattern then
                let
                    exprRange : Range
                    exprRange =
                        Node.range checkInfo.caseOf.expression

                    caseIndentation : String
                    caseIndentation =
                        String.repeat (checkInfo.parentRange.start.column - 1) " "

                    bodyIndentation : String
                    bodyIndentation =
                        String.repeat (bodyRange.start.column - 1) " "
                in
                Just
                    (Rule.errorWithFix
                        { message = "Use a let expression to destructure data"
                        , details = [ "It is more idiomatic in Elm to use a let expression to define a new variable rather than to use pattern matching. This will also make the code less indented, therefore easier to read." ]
                        }
                        (Node.range singlePattern)
                        [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = exprRange.start }
                            ("let " ++ checkInfo.extractSourceCode (Node.range singlePattern) ++ " = ")
                        , Fix.replaceRangeBy { start = exprRange.end, end = bodyRange.start }
                            ("\n" ++ caseIndentation ++ "in\n" ++ bodyIndentation)
                        ]
                    )

            else
                Nothing

        _ ->
            Nothing


isSimpleDestructurePattern : Node Pattern -> Bool
isSimpleDestructurePattern (Node _ pattern) =
    case pattern of
        Pattern.TuplePattern _ ->
            True

        Pattern.RecordPattern _ ->
            True

        Pattern.VarPattern _ ->
            True

        _ ->
            False


caseOfWithUnreachableCasesChecks : CaseOfCheckInfo -> Maybe (Error {})
caseOfWithUnreachableCasesChecks checkInfo =
    caseOfWithUnreachableCasesChecksOn
        { casedExpressionNode = checkInfo.caseOf.expression
        , cases = checkInfo.caseOf.cases
        }
        checkInfo
        |> Maybe.map
            (\error ->
                let
                    maybeOnlyMatchingCase : Maybe { index : Int, expressionNode : Node Expression }
                    maybeOnlyMatchingCase =
                        case error.fix.casedExpressionReplace.replacement of
                            "()" ->
                                error.fix.cases
                                    |> indexedFindMap
                                        (\caseIndex fix ->
                                            case fix of
                                                UnreachableCaseRemove _ ->
                                                    Nothing

                                                UnreachableCaseReplace replace ->
                                                    Just { index = caseIndex, expressionNode = replace.expressionNode }
                                        )

                            _ ->
                                Nothing
                in
                case maybeOnlyMatchingCase of
                    Nothing ->
                        Rule.errorWithFix { message = error.message, details = error.details }
                            error.range
                            (unreachableCasesFixToReviewFixes error.fix)

                    Just onlyMatchingCase ->
                        Rule.errorWithFix
                            { message = error.message
                            , details =
                                error.details
                                    ++ [ "Since only a single case branch will remain after removing the unreachable ones, you can replace this case-of by the result of the "
                                            ++ indexthToString onlyMatchingCase.index
                                            ++ " case."
                                       ]
                            }
                            error.range
                            (replaceBySubExpressionFix checkInfo.parentRange onlyMatchingCase.expressionNode)
            )


type alias UnreachableCasesFix =
    { casedExpressionReplace : { range : Range, replacement : String }
    , cases : List UnreachableCaseFix
    }


type UnreachableCaseFix
    = UnreachableCaseRemove { patternRange : Range, expressionRange : Range }
    | UnreachableCaseReplace { range : Range, replacement : String, expressionNode : Node Expression }


unreachableCasesFixToReviewFixes : UnreachableCasesFix -> List Fix
unreachableCasesFixToReviewFixes unreachableCasesFix =
    Fix.replaceRangeBy unreachableCasesFix.casedExpressionReplace.range unreachableCasesFix.casedExpressionReplace.replacement
        :: List.map2
            (\unreachableCaseFix unreachableCaseFixBefore ->
                case unreachableCaseFix of
                    UnreachableCaseReplace unreachableCaseReplace ->
                        Fix.replaceRangeBy unreachableCaseReplace.range unreachableCaseReplace.replacement

                    UnreachableCaseRemove caseRanges ->
                        Fix.removeRange
                            { start =
                                { row =
                                    case unreachableCaseFixBefore of
                                        Nothing ->
                                            caseRanges.patternRange.start.row

                                        Just (UnreachableCaseRemove beforeCaseRanges) ->
                                            beforeCaseRanges.expressionRange.end.row + 1

                                        Just (UnreachableCaseReplace beforeUnreachableCaseReplace) ->
                                            (Node.range beforeUnreachableCaseReplace.expressionNode).end.row + 1
                                , column = 0
                                }
                            , end =
                                { row = caseRanges.expressionRange.end.row + 1, column = 0 }
                            }
            )
            unreachableCasesFix.cases
            (Nothing :: List.map Just unreachableCasesFix.cases)


caseOfWithUnreachableCasesChecksOn :
    { casedExpressionNode : Node Expression, cases : List ( Node Pattern, Node Expression ) }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseOfWithUnreachableCasesChecksOn config checkInfo =
    caseVariantOfWithUnreachableCasesChecks config checkInfo
        |> onNothing
            (\() -> caseListLiteralOfWithUnreachableCasesChecks config checkInfo)
        |> onNothing
            (\() -> caseConsOfWithUnreachableCasesChecks config checkInfo)
        |> onNothing
            (\() -> caseTuple2OfWithUnreachableCasesChecks config checkInfo)
        |> onNothing (\() -> caseTuple3OfWithUnreachableCasesChecks config checkInfo)


caseTuple2OfWithUnreachableCasesChecks :
    { casedExpressionNode : Node Expression, cases : List ( Node Pattern, Node Expression ) }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseTuple2OfWithUnreachableCasesChecks config checkInfo =
    case AstHelpers.getTuple2 checkInfo.lookupTable config.casedExpressionNode of
        Nothing ->
            Nothing

        Just casedTuple ->
            let
                maybeTuplePatternCases : Maybe (List { firstPatternNode : Node Pattern, secondPatternNode : Node Pattern, expressionNode : Node Expression })
                maybeTuplePatternCases =
                    traverse
                        (\( patternNode, expressionNode ) ->
                            case patternNode of
                                Node _ (Pattern.TuplePattern (firstPatternNode :: secondPatternNode :: [])) ->
                                    Just { firstPatternNode = firstPatternNode, secondPatternNode = secondPatternNode, expressionNode = expressionNode }

                                _ ->
                                    Nothing
                        )
                        config.cases
            in
            case maybeTuplePatternCases of
                Nothing ->
                    Nothing

                Just tuplePatternCases ->
                    caseOfWithUnreachableCasesChecksOn
                        { casedExpressionNode = casedTuple.first
                        , cases =
                            List.map (\case_ -> ( case_.firstPatternNode, case_.expressionNode ))
                                tuplePatternCases
                        }
                        checkInfo
                        |> onNothing
                            (\() ->
                                caseOfWithUnreachableCasesChecksOn
                                    { casedExpressionNode = casedTuple.second
                                    , cases =
                                        List.map (\case_ -> ( case_.secondPatternNode, case_.expressionNode ))
                                            tuplePatternCases
                                    }
                                    checkInfo
                            )


caseTuple3OfWithUnreachableCasesChecks :
    { casedExpressionNode : Node Expression, cases : List ( Node Pattern, Node Expression ) }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseTuple3OfWithUnreachableCasesChecks config checkInfo =
    case config.casedExpressionNode of
        Node _ (Expression.TupledExpression (casedTupleFirstNode :: casedTupleSecondNode :: casedTupleThirdNode :: [])) ->
            let
                maybeTuplePatternCases : Maybe (List { firstPatternNode : Node Pattern, secondPatternNode : Node Pattern, thirdPatternNode : Node Pattern, expressionNode : Node Expression })
                maybeTuplePatternCases =
                    traverse
                        (\( patternNode, expressionNode ) ->
                            case patternNode of
                                Node _ (Pattern.TuplePattern (firstPatternNode :: secondPatternNode :: thirdPatternNode :: [])) ->
                                    Just { firstPatternNode = firstPatternNode, secondPatternNode = secondPatternNode, thirdPatternNode = thirdPatternNode, expressionNode = expressionNode }

                                _ ->
                                    Nothing
                        )
                        config.cases
            in
            case maybeTuplePatternCases of
                Nothing ->
                    Nothing

                Just tuplePatternCases ->
                    caseOfWithUnreachableCasesChecksOn
                        { casedExpressionNode = casedTupleFirstNode
                        , cases =
                            List.map (\case_ -> ( case_.firstPatternNode, case_.expressionNode ))
                                tuplePatternCases
                        }
                        checkInfo
                        |> onNothing
                            (\() ->
                                caseOfWithUnreachableCasesChecksOn
                                    { casedExpressionNode = casedTupleSecondNode
                                    , cases =
                                        List.map (\case_ -> ( case_.secondPatternNode, case_.expressionNode ))
                                            tuplePatternCases
                                    }
                                    checkInfo
                            )
                        |> onNothing
                            (\() ->
                                caseOfWithUnreachableCasesChecksOn
                                    { casedExpressionNode = casedTupleThirdNode
                                    , cases =
                                        List.map (\case_ -> ( case_.thirdPatternNode, case_.expressionNode ))
                                            tuplePatternCases
                                    }
                                    checkInfo
                            )

        _ ->
            Nothing


caseVariantOfWithUnreachableCasesChecks :
    { casedExpressionNode : Node Expression, cases : List ( Node Pattern, Node Expression ) }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseVariantOfWithUnreachableCasesChecks config checkInfo =
    let
        maybeVariantCaseOf :
            Maybe
                { cased : { moduleName : ModuleName, name : String, attachments : List (Node Expression), customTypeVariantNames : Set String }
                , cases : List { patternRange : Range, name : String, attachments : List (Node Pattern), expressionNode : Node Expression }
                }
        maybeVariantCaseOf =
            case AstHelpers.getUnreducedValueOrFnOrFnCall config.casedExpressionNode of
                Nothing ->
                    Nothing

                Just valueOrCall ->
                    let
                        maybeValueModule : Maybe { name : ModuleName, customTypes : Dict String { variantNames : Set String, allParametersAreUsedInVariants : Bool } }
                        maybeValueModule =
                            case ModuleNameLookupTable.moduleNameAt checkInfo.lookupTable valueOrCall.fnRange of
                                Just [] ->
                                    Just { name = [], customTypes = checkInfo.moduleCustomTypes }

                                Just (moduleNamePart0 :: moduleNamePart1Up) ->
                                    Dict.get (moduleNamePart0 :: moduleNamePart1Up) checkInfo.importCustomTypes
                                        |> Maybe.map
                                            (\customTypes ->
                                                { name = moduleNamePart0 :: moduleNamePart1Up, customTypes = customTypes }
                                            )

                                Nothing ->
                                    Nothing
                    in
                    case maybeValueModule of
                        Nothing ->
                            Nothing

                        Just valueModule ->
                            case getCustomTypeWithVariant valueOrCall.fnName valueModule.customTypes of
                                Nothing ->
                                    Nothing

                                Just customTypeWithVariant ->
                                    Maybe.map
                                        (\cases ->
                                            { cased =
                                                { moduleName = valueModule.name
                                                , name = valueOrCall.fnName
                                                , attachments = valueOrCall.args
                                                , customTypeVariantNames = customTypeWithVariant.variantNames
                                                }
                                            , cases = cases
                                            }
                                        )
                                        (traverse getVariantCase config.cases)
    in
    case maybeVariantCaseOf of
        Nothing ->
            Nothing

        Just variantCaseOf ->
            case Set.size variantCaseOf.cased.customTypeVariantNames of
                1 ->
                    caseSingleVariantWithUnreachableCasesCheck
                        { cased = variantCaseOf.cased
                        , casedExpressionRange = Node.range config.casedExpressionNode
                        , cases = variantCaseOf.cases
                        }
                        checkInfo

                -- >= 2 possible variants
                _ ->
                    Just
                        (caseMultiVariantWithUnreachableCasesError
                            { cased = variantCaseOf.cased
                            , casedExpressionRange = Node.range config.casedExpressionNode
                            , cases = variantCaseOf.cases
                            }
                            checkInfo
                        )


caseSingleVariantWithUnreachableCasesCheck :
    { cased : { moduleName : ModuleName, name : String, attachments : List (Node Expression), customTypeVariantNames : Set String }
    , casedExpressionRange : Range
    , cases : List { patternRange : Range, name : String, attachments : List (Node Pattern), expressionNode : Node Expression }
    }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseSingleVariantWithUnreachableCasesCheck variantCaseOf checkInfo =
    variantCaseOf.cased.attachments
        |> List.foldl
            (\attachment soFar ->
                case traverse listFilledFromList soFar.remainingCaseVariantAttachmentListsFilled of
                    Just caseVariantAttachmentListsFilled ->
                        { index = soFar.index + 1
                        , remainingCaseVariantAttachmentListsFilled =
                            List.map listFilledTail caseVariantAttachmentListsFilled
                        , attachmentAndCasesList =
                            { index = soFar.index, attachment = attachment, cases = List.map listFilledHead caseVariantAttachmentListsFilled }
                                :: soFar.attachmentAndCasesList
                        }

                    -- case curried variant of → compiler error
                    Nothing ->
                        { index = 0
                        , remainingCaseVariantAttachmentListsFilled = []
                        , attachmentAndCasesList = []
                        }
            )
            { index = 0
            , attachmentAndCasesList = []
            , remainingCaseVariantAttachmentListsFilled =
                List.map
                    (\case_ ->
                        List.map (\patternNode -> ( patternNode, case_.expressionNode ))
                            case_.attachments
                    )
                    variantCaseOf.cases
            }
        |> .attachmentAndCasesList
        |> findMap
            (\casedVariantAttachmentAndCases ->
                caseOfWithUnreachableCasesChecksOn
                    { casedExpressionNode = casedVariantAttachmentAndCases.attachment
                    , cases = casedVariantAttachmentAndCases.cases
                    }
                    checkInfo
                    |> Maybe.map
                        (\attachmentError ->
                            { message = attachmentError.message
                            , details = attachmentError.details
                            , range = attachmentError.range
                            , fix =
                                { casedExpressionReplace =
                                    { range = variantCaseOf.casedExpressionRange
                                    , replacement =
                                        toNestedTupleFix
                                            (List.indexedMap
                                                (\i (Node argRange _) ->
                                                    if i == casedVariantAttachmentAndCases.index then
                                                        attachmentError.fix.casedExpressionReplace.replacement

                                                    else
                                                        checkInfo.extractSourceCode argRange
                                                )
                                                variantCaseOf.cased.attachments
                                            )
                                    }
                                , cases =
                                    List.map2
                                        (\variantPattern patternAttachmentError ->
                                            case patternAttachmentError of
                                                UnreachableCaseRemove remove ->
                                                    UnreachableCaseRemove remove

                                                UnreachableCaseReplace replace ->
                                                    UnreachableCaseReplace
                                                        { range = variantPattern.patternRange
                                                        , replacement =
                                                            toNestedTupleFix
                                                                (List.indexedMap
                                                                    (\i (Node attachmentRange _) ->
                                                                        if i == casedVariantAttachmentAndCases.index then
                                                                            replace.replacement

                                                                        else
                                                                            checkInfo.extractSourceCode attachmentRange
                                                                    )
                                                                    variantPattern.attachments
                                                                )
                                                        , expressionNode = replace.expressionNode
                                                        }
                                        )
                                        variantCaseOf.cases
                                        attachmentError.fix.cases
                                }
                            }
                        )
            )


caseMultiVariantWithUnreachableCasesError :
    { cased : { moduleName : ModuleName, name : String, attachments : List (Node Expression), customTypeVariantNames : Set String }
    , casedExpressionRange : Range
    , cases : List { patternRange : Range, name : String, attachments : List (Node Pattern), expressionNode : Node Expression }
    }
    ->
        { checkInfo
            | parentRange : Range
            , extractSourceCode : Range -> String
        }
    -> { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseMultiVariantWithUnreachableCasesError variantCaseOf checkInfo =
    { message = "Unreachable case branches"
    , details =
        [ "The value between case ... of is a known "
            ++ qualifiedToString (qualify ( variantCaseOf.cased.moduleName, variantCaseOf.cased.name ) defaultQualifyResources)
            ++ " variant. However, the "
            ++ (variantCaseOf.cases
                    |> listIndexedFilterMap
                        (\caseIndex variant ->
                            if variant.name /= variantCaseOf.cased.name then
                                Just (indexthToString caseIndex)

                            else
                                Nothing
                        )
                    |> String.join " and "
               )
            ++ " case matches on a different variant which means you can remove it."
        ]
    , range =
        findMap
            (\case_ ->
                if case_.name /= variantCaseOf.cased.name then
                    Just case_.patternRange

                else
                    Nothing
            )
            variantCaseOf.cases
            |> Maybe.withDefault (caseKeyWordRange checkInfo.parentRange)
    , fix =
        { casedExpressionReplace =
            { range = variantCaseOf.casedExpressionRange
            , replacement =
                toNestedTupleFix
                    (List.map (\(Node argRange _) -> checkInfo.extractSourceCode argRange)
                        variantCaseOf.cased.attachments
                    )
            }
        , cases =
            List.map
                (\variantCase ->
                    if variantCase.name == variantCaseOf.cased.name then
                        UnreachableCaseReplace
                            { range = variantCase.patternRange
                            , replacement =
                                toNestedTupleFix
                                    (List.map (\(Node argRange _) -> checkInfo.extractSourceCode argRange)
                                        variantCase.attachments
                                    )
                            , expressionNode = variantCase.expressionNode
                            }

                    else
                        UnreachableCaseRemove
                            { patternRange = variantCase.patternRange
                            , expressionRange = Node.range variantCase.expressionNode
                            }
                )
                variantCaseOf.cases
        }
    }


getVariantCase : ( Node Pattern, Node Expression ) -> Maybe { patternRange : Range, expressionNode : Node Expression, name : String, attachments : List (Node Pattern) }
getVariantCase ( patternNode, expressionNode ) =
    case AstHelpers.removeParensFromPattern patternNode of
        Node patternRange (Pattern.NamedPattern qualified attachments) ->
            Just { patternRange = patternRange, expressionNode = expressionNode, name = qualified.name, attachments = attachments }

        _ ->
            Nothing


getCustomTypeWithVariant :
    String
    -> Dict String { variantNames : Set String, allParametersAreUsedInVariants : Bool }
    -> Maybe { name : String, variantNames : Set String, allParametersAreUsedInVariants : Bool }
getCustomTypeWithVariant variantName customTypes =
    customTypes
        |> Dict.foldl
            (\customTypeName customTypeInfo soFar ->
                case soFar of
                    Just _ ->
                        soFar

                    Nothing ->
                        if Set.member variantName customTypeInfo.variantNames then
                            Just
                                { name = customTypeName
                                , variantNames = customTypeInfo.variantNames
                                , allParametersAreUsedInVariants = customTypeInfo.allParametersAreUsedInVariants
                                }

                        else
                            Nothing
            )
            Nothing


caseListLiteralOfWithUnreachableCasesChecks :
    { casedExpressionNode : Node Expression, cases : List ( Node Pattern, Node Expression ) }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseListLiteralOfWithUnreachableCasesChecks config checkInfo =
    case AstHelpers.getListLiteral config.casedExpressionNode of
        Nothing ->
            Nothing

        Just casedListLiteralElements ->
            let
                maybeListPatternCases : Maybe (List { patternRange : Range, expressionNode : Node Expression, pattern : ListPattern })
                maybeListPatternCases =
                    traverse
                        (\( patternNode, expressionNode ) ->
                            Maybe.map
                                (\listPattern ->
                                    { patternRange = Node.range patternNode, expressionNode = expressionNode, pattern = listPattern }
                                )
                                (getListPattern (Node.value patternNode))
                        )
                        config.cases
            in
            Maybe.map
                (\listPatternCases ->
                    caseListLiteralOfWithUnreachableCasesError
                        { casedExpressionRange = Node.range config.casedExpressionNode
                        , listPatternCases = listPatternCases
                        , casedListLiteralElements = casedListLiteralElements
                        }
                        checkInfo
                )
                maybeListPatternCases


caseListLiteralOfWithUnreachableCasesError :
    { casedExpressionRange : Range
    , casedListLiteralElements : List (Node Expression)
    , listPatternCases : List { patternRange : Range, expressionNode : Node Expression, pattern : ListPattern }
    }
    ->
        { checkInfo
            | extractSourceCode : Range -> String
            , parentRange : Range
        }
    -> { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseListLiteralOfWithUnreachableCasesError config checkInfo =
    let
        casedListLiteralLength : Int
        casedListLiteralLength =
            List.length config.casedListLiteralElements

        isUnnecessaryListPattern : ListPattern -> Bool
        isUnnecessaryListPattern listPattern =
            case listPattern of
                ListLiteralPattern listLiteralPatternELements ->
                    List.length listLiteralPatternELements /= casedListLiteralLength

                ConsPattern consPattern ->
                    List.length (listFilledToList consPattern.beginningElements) >= (casedListLiteralLength + 1)

        alwaysMatchedBeginningElementCount : Int
        alwaysMatchedBeginningElementCount =
            config.listPatternCases
                |> List.filterMap
                    (\listPatternCase ->
                        case listPatternCase.pattern of
                            ListLiteralPattern _ ->
                                Nothing

                            ConsPattern consPattern ->
                                Just (List.length (listFilledToList consPattern.beginningElements))
                    )
                |> List.minimum
                |> Maybe.withDefault casedListLiteralLength
    in
    { message = "Unreachable case branches"
    , details =
        [ "The value between case ... of is a known list of length "
            ++ String.fromInt casedListLiteralLength
            ++ ". However, the "
            ++ (config.listPatternCases
                    |> listIndexedFilterMap
                        (\caseIndex case_ ->
                            if isUnnecessaryListPattern case_.pattern then
                                Just (indexthToString caseIndex)

                            else
                                Nothing
                        )
                    |> String.join " and "
               )
            ++ " case matches on a list with a different length which means you can remove it."
        ]
    , range =
        findMap
            (\case_ ->
                if isUnnecessaryListPattern case_.pattern then
                    Just case_.patternRange

                else
                    Nothing
            )
            config.listPatternCases
            |> Maybe.withDefault (caseKeyWordRange checkInfo.parentRange)
    , fix =
        { casedExpressionReplace =
            { range = config.casedExpressionRange
            , replacement =
                listLiteralToNestedTupleFix
                    { tupledBeginningLength = alwaysMatchedBeginningElementCount
                    , elements =
                        List.map (\(Node elementRange _) -> checkInfo.extractSourceCode elementRange)
                            config.casedListLiteralElements
                    }
            }
        , cases =
            List.map
                (\listPatternCase ->
                    if isUnnecessaryListPattern listPatternCase.pattern then
                        UnreachableCaseRemove
                            { patternRange = listPatternCase.patternRange
                            , expressionRange = Node.range listPatternCase.expressionNode
                            }

                    else
                        UnreachableCaseReplace
                            { range = listPatternCase.patternRange
                            , expressionNode = listPatternCase.expressionNode
                            , replacement =
                                case listPatternCase.pattern of
                                    ListLiteralPattern listPatternElements ->
                                        listLiteralToNestedTupleFix
                                            { tupledBeginningLength = alwaysMatchedBeginningElementCount
                                            , elements =
                                                List.map (\(Node elementRange _) -> checkInfo.extractSourceCode elementRange)
                                                    listPatternElements
                                            }

                                    ConsPattern consPattern ->
                                        collapsedConsToNestedTupleFix
                                            { tupledBeginningLength = alwaysMatchedBeginningElementCount
                                            , beginningElements =
                                                listFilledMap (\(Node elementRange _) -> checkInfo.extractSourceCode elementRange)
                                                    consPattern.beginningElements
                                            , tail = checkInfo.extractSourceCode (Node.range consPattern.tail)
                                            }
                            }
                )
                config.listPatternCases
        }
    }


caseConsOfWithUnreachableCasesChecks :
    { casedExpressionNode : Node Expression, cases : List ( Node Pattern, Node Expression ) }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseConsOfWithUnreachableCasesChecks config checkInfo =
    case getCollapsedCons (Node.value config.casedExpressionNode) of
        Nothing ->
            Nothing

        Just casedCons ->
            let
                maybeListPatternCases : Maybe (List { patternRange : Range, expressionNode : Node Expression, pattern : ListPattern })
                maybeListPatternCases =
                    traverse
                        (\( patternNode, expressionNode ) ->
                            Maybe.map
                                (\listPattern ->
                                    { patternRange = Node.range patternNode, expressionNode = expressionNode, pattern = listPattern }
                                )
                                (getListPattern (Node.value patternNode))
                        )
                        config.cases
            in
            Maybe.map
                (\listPatternCases ->
                    caseConsOfWithUnreachableCasesError
                        { casedExpressionRange = Node.range config.casedExpressionNode
                        , casedCons = casedCons
                        , listPatternCases = listPatternCases
                        }
                        checkInfo
                )
                maybeListPatternCases


caseConsOfWithUnreachableCasesError :
    { casedExpressionRange : Range
    , casedCons : { beginningElements : ( Node Expression, List (Node Expression) ), tail : Node Expression }
    , listPatternCases : List { patternRange : Range, expressionNode : Node Expression, pattern : ListPattern }
    }
    ->
        { checkInfo
            | extractSourceCode : Range -> String
            , parentRange : Range
        }
    -> { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseConsOfWithUnreachableCasesError config checkInfo =
    let
        casedBeginningElementCount : Int
        casedBeginningElementCount =
            listFilledLength config.casedCons.beginningElements

        isUnnecessaryListPattern : ListPattern -> Bool
        isUnnecessaryListPattern listPattern =
            case listPattern of
                ListLiteralPattern listLiteralPatternELements ->
                    List.length listLiteralPatternELements < casedBeginningElementCount

                ConsPattern _ ->
                    False

        alwaysMatchedBeginningElementCount : Int
        alwaysMatchedBeginningElementCount =
            config.listPatternCases
                |> List.filterMap
                    (\listPatternCase ->
                        case listPatternCase.pattern of
                            ListLiteralPattern _ ->
                                Nothing

                            ConsPattern consPattern ->
                                Just (List.length (listFilledToList consPattern.beginningElements))
                    )
                |> List.minimum
                |> Maybe.withDefault casedBeginningElementCount
    in
    { message = "Unreachable case branches"
    , details =
        [ "The value between case ... of is a list of length >= "
            ++ String.fromInt casedBeginningElementCount
            ++ ". However, the "
            ++ (config.listPatternCases
                    |> listIndexedFilterMap
                        (\caseIndex case_ ->
                            if isUnnecessaryListPattern case_.pattern then
                                Just (indexthToString caseIndex)

                            else
                                Nothing
                        )
                    |> String.join " and "
               )
            ++ " case matches on a shorter list which means you can remove it."
        ]
    , range =
        findMap
            (\case_ ->
                if isUnnecessaryListPattern case_.pattern then
                    Just case_.patternRange

                else
                    Nothing
            )
            config.listPatternCases
            |> Maybe.withDefault (caseKeyWordRange checkInfo.parentRange)
    , fix =
        { casedExpressionReplace =
            { range = config.casedExpressionRange
            , replacement =
                collapsedConsToNestedTupleFix
                    { tupledBeginningLength = alwaysMatchedBeginningElementCount
                    , beginningElements =
                        listFilledMap (\(Node elementRange _) -> checkInfo.extractSourceCode elementRange)
                            config.casedCons.beginningElements
                    , tail = checkInfo.extractSourceCode (Node.range config.casedCons.tail)
                    }
            }
        , cases =
            List.map
                (\listPatternCase ->
                    if isUnnecessaryListPattern listPatternCase.pattern then
                        UnreachableCaseRemove
                            { patternRange = listPatternCase.patternRange
                            , expressionRange = Node.range listPatternCase.expressionNode
                            }

                    else
                        UnreachableCaseReplace
                            { range = listPatternCase.patternRange
                            , expressionNode = listPatternCase.expressionNode
                            , replacement =
                                case listPatternCase.pattern of
                                    ListLiteralPattern listPatternElements ->
                                        listLiteralToNestedTupleFix
                                            { tupledBeginningLength = alwaysMatchedBeginningElementCount
                                            , elements =
                                                List.map (\(Node elementRange _) -> checkInfo.extractSourceCode elementRange)
                                                    listPatternElements
                                            }

                                    ConsPattern consPattern ->
                                        collapsedConsToNestedTupleFix
                                            { tupledBeginningLength = alwaysMatchedBeginningElementCount
                                            , beginningElements =
                                                listFilledMap (\(Node elementRange _) -> checkInfo.extractSourceCode elementRange)
                                                    consPattern.beginningElements
                                            , tail = checkInfo.extractSourceCode (Node.range consPattern.tail)
                                            }
                            }
                )
                config.listPatternCases
        }
    }


type ListPattern
    = ListLiteralPattern (List (Node Pattern))
    | ConsPattern { beginningElements : ( Node Pattern, List (Node Pattern) ), tail : Node Pattern }


getListPattern : Pattern -> Maybe ListPattern
getListPattern pattern =
    case pattern of
        Pattern.ListPattern elementPatterns ->
            Just (ListLiteralPattern elementPatterns)

        nonListLiteralPattern ->
            Maybe.map ConsPattern (getCollapsedConsPattern nonListLiteralPattern)


getCollapsedConsPattern : Pattern -> Maybe { beginningElements : ( Node Pattern, List (Node Pattern) ), tail : Node Pattern }
getCollapsedConsPattern pattern =
    case pattern of
        Pattern.UnConsPattern head tail ->
            case getCollapsedConsPattern (Node.value tail) of
                Nothing ->
                    Just { beginningElements = ( head, [] ), tail = tail }

                Just tailCollapsed ->
                    Just { beginningElements = ( head, listFilledToList tailCollapsed.beginningElements ), tail = tailCollapsed.tail }

        _ ->
            Nothing


getCollapsedCons : Expression -> Maybe { beginningElements : ( Node Expression, List (Node Expression) ), tail : Node Expression }
getCollapsedCons expression =
    case expression of
        Expression.OperatorApplication "::" _ head tail ->
            case getCollapsedCons (Node.value tail) of
                Just tailCollapsed ->
                    Just { beginningElements = ( head, listFilledToList tailCollapsed.beginningElements ), tail = tailCollapsed.tail }

                Nothing ->
                    Just { beginningElements = ( head, [] ), tail = tail }

        _ ->
            Nothing


indexthToString : Int -> String
indexthToString index =
    let
        suffix : String
        suffix =
            case (index + 1) |> remainderBy 10 of
                1 ->
                    "st"

                2 ->
                    "nd"

                3 ->
                    "rd"

                _ ->
                    "th"
    in
    String.fromInt (index + 1) ++ suffix



-- NEGATION


negationChecks : { parentRange : Range, negatedExpression : Node Expression } -> Maybe (Error {})
negationChecks checkInfo =
    case AstHelpers.removeParens checkInfo.negatedExpression of
        Node range (Expression.Negation negatedValue) ->
            let
                doubleNegationRange : Range
                doubleNegationRange =
                    { start = checkInfo.parentRange.start
                    , end = { row = range.start.row, column = range.start.column + 1 }
                    }
            in
            Just
                (Rule.errorWithFix
                    { message = "Unnecessary double number negation"
                    , details = [ "Negating a number twice is the same as the number itself." ]
                    }
                    doubleNegationRange
                    (replaceBySubExpressionFix checkInfo.parentRange negatedValue)
                )

        _ ->
            Nothing



-- FULLY APPLIED PREFIX OPERATORS


fullyAppliedPrefixOperatorError :
    { operator : String
    , operatorRange : Range
    , left : Node Expression
    , right : Node Expression
    }
    -> Error {}
fullyAppliedPrefixOperatorError checkInfo =
    Rule.errorWithFix
        { message = "Use the infix form (a + b) over the prefix form ((+) a b)"
        , details = [ "The prefix form is generally more unfamiliar to Elm developers, and therefore it is nicer when the infix form is used." ]
        }
        checkInfo.operatorRange
        [ Fix.removeRange { start = checkInfo.operatorRange.start, end = (Node.range checkInfo.left).start }
        , Fix.insertAt (Node.range checkInfo.right).start (checkInfo.operator ++ " ")
        ]



-- APPLIED LAMBDA


appliedLambdaError :
    AstHelpers.ReduceLambdaResources { lambdaWithParens : Range, lambda : Expression.Lambda, lambdaNode : Node Expression, firstArgument : Node Expression, argsAfterFirst : List (Node Expression) }
    -> Maybe (Error {})
appliedLambdaError checkInfo =
    case AstHelpers.isIdentityWithKind checkInfo checkInfo.lambdaNode of
        Just identityKind ->
            let
                errorInfo : { message : String, details : List String }
                errorInfo =
                    case identityKind of
                        AstHelpers.IdentityFunction ->
                            { message = "`identity` should be removed"
                            , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
                            }

                        AstHelpers.IdentityLambda ->
                            { message = "Unnecessary identity function"
                            , details = [ "This function returns the argument it is given without any changes. Calling it with an argument is the same thing as writing the argument on its own." ]
                            }
            in
            Just
                (Rule.errorWithFix
                    errorInfo
                    (Node.range checkInfo.lambdaNode)
                    [ Fix.removeRange
                        (rangeFromInclusiveToExclusive
                            { fromInclusive = checkInfo.lambdaWithParens
                            , toExclusive = Node.range checkInfo.firstArgument
                            }
                        )
                    ]
                )

        Nothing ->
            appliedLambdaIgnoredPatternsCheck checkInfo


appliedLambdaIgnoredPatternsCheck :
    AstHelpers.ReduceLambdaResources { lambdaWithParens : Range, lambda : Expression.Lambda, lambdaNode : Node Expression, firstArgument : Node Expression, argsAfterFirst : List (Node Expression) }
    -> Maybe (Error {})
appliedLambdaIgnoredPatternsCheck checkInfo =
    appliedLambdaIgnoredPatternsCheckHelp
        Nothing
        Nothing
        checkInfo.lambda.args
        (checkInfo.firstArgument :: checkInfo.argsAfterFirst)
        |> Maybe.map
            (\result ->
                Rule.errorWithFix
                    result.info
                    (Node.range result.pattern)
                    (appliedLambdaErrorFix result checkInfo)
            )


appliedLambdaIgnoredPatternsCheckHelp :
    Maybe (Node Pattern)
    -> Maybe (Node Expression)
    -> List (Node Pattern)
    -> List (Node Expression)
    ->
        Maybe
            { pattern : Node Pattern
            , argument : Node Expression
            , previousPattern : Maybe (Node Pattern)
            , previousArgument : Maybe (Node Expression)
            , nextPattern : Maybe (Node Pattern)
            , nextArgument : Maybe (Node Expression)
            , info : { message : String, details : List String }
            }
appliedLambdaIgnoredPatternsCheckHelp previousPattern previousArgument patterns arguments =
    case ( patterns, arguments ) of
        ( [], _ ) ->
            Nothing

        ( _, [] ) ->
            Nothing

        ( ((Node _ patternValue) as pattern) :: restOfPatterns, argument :: restOfArguments ) ->
            case patternValue of
                Pattern.UnitPattern ->
                    Just
                        { pattern = pattern
                        , argument = argument
                        , previousPattern = previousPattern
                        , previousArgument = previousArgument
                        , nextPattern = List.head restOfPatterns
                        , nextArgument = List.head restOfArguments
                        , info =
                            { message = "Unnecessary unit argument"
                            , details =
                                [ "This function is expecting a unit, but also passing it directly."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            }
                        }

                Pattern.AllPattern ->
                    Just
                        { pattern = pattern
                        , argument = argument
                        , previousPattern = previousPattern
                        , previousArgument = previousArgument
                        , nextPattern = List.head restOfPatterns
                        , nextArgument = List.head restOfArguments
                        , info =
                            { message = "Unnecessary ignored argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            }
                        }

                _ ->
                    appliedLambdaIgnoredPatternsCheckHelp
                        (Just pattern)
                        (Just argument)
                        restOfPatterns
                        restOfArguments


appliedLambdaErrorFix :
    { params
        | pattern : Node Pattern
        , argument : Node Expression
        , previousPattern : Maybe (Node Pattern)
        , previousArgument : Maybe (Node Expression)
        , nextPattern : Maybe (Node Pattern)
        , nextArgument : Maybe (Node Expression)
    }
    -> { checkInfo | lambdaWithParens : Range, lambda : Expression.Lambda, firstArgument : Node Expression }
    -> List Fix
appliedLambdaErrorFix params checkInfo =
    let
        removeArgument : () -> List Fix
        removeArgument () =
            case params.nextArgument of
                Just (Node next _) ->
                    [ Fix.removeRange { start = (Node.range params.argument).start, end = next.start } ]

                Nothing ->
                    case params.previousArgument of
                        Just (Node previous _) ->
                            let
                                argRange : Range
                                argRange =
                                    Node.range params.argument
                            in
                            case Range.compareLocations argRange.end previous.end of
                                GT ->
                                    [ Fix.removeRange { start = previous.end, end = argRange.end } ]

                                _ ->
                                    -- We're in a `c |> (\a _ -> ...) b` situation
                                    -- `c` is the "next" argument but appears before `b` in the source code.
                                    [ Fix.removeRange { start = argRange.start, end = checkInfo.lambdaWithParens.start } ]

                        Nothing ->
                            -- No other args
                            keepOnlyFix
                                { parentRange = Range.combine [ checkInfo.lambdaWithParens, Node.range params.argument ]
                                , keep = checkInfo.lambdaWithParens
                                }
    in
    case params.nextPattern of
        Just (Node next _) ->
            Fix.removeRange { start = (Node.range params.pattern).start, end = next.start }
                :: removeArgument ()

        Nothing ->
            case params.previousPattern of
                Just (Node previous _) ->
                    Fix.removeRange { start = previous.end, end = (Node.range params.pattern).end }
                        :: removeArgument ()

                Nothing ->
                    -- No other patterns
                    replaceBySubExpressionFix (Range.combine [ checkInfo.lambdaWithParens, Node.range checkInfo.firstArgument ]) checkInfo.lambda.expression



-- LET IN


letInChecks : Expression.LetBlock -> Maybe (Error {})
letInChecks letBlock =
    case Node.value letBlock.expression of
        Expression.LetExpression _ ->
            let
                letRange : Range
                letRange =
                    letKeyWordRange (Node.range letBlock.expression)
            in
            Just
                (Rule.errorWithFix
                    { message = "Let blocks can be joined together"
                    , details = [ "Let blocks can contain multiple declarations, and there is no advantage to having multiple chained let expressions rather than one longer let expression." ]
                    }
                    letRange
                    (case listLast letBlock.declarations of
                        Just (Node lastDeclRange _) ->
                            [ Fix.replaceRangeBy { start = lastDeclRange.end, end = letRange.end } "\n" ]

                        Nothing ->
                            []
                    )
                )

        _ ->
            Nothing


letKeyWordRange : Range -> Range
letKeyWordRange range =
    { start = range.start
    , end = { row = range.start.row, column = range.start.column + 3 }
    }



-- RECORD ACCESS


accessingRecordChecks :
    { parentRange : Range
    , fieldName : String
    , fieldRange : Range
    , record : Node Expression
    , importRecordTypeAliases : Dict ModuleName (Dict String (List String))
    , moduleRecordTypeAliases : Dict String (List String)
    , importCustomTypes :
        Dict
            ModuleName
            (Dict
                String
                { variantNames : Set String
                , allParametersAreUsedInVariants : Bool
                }
            )
    , moduleCustomTypes :
        Dict
            String
            { variantNames : Set String
            , allParametersAreUsedInVariants : Bool
            }
    , lookupTable : ModuleNameLookupTable
    }
    -> Maybe ErrorInfoAndFix
accessingRecordChecks checkInfo =
    case Node.value (AstHelpers.removeParens checkInfo.record) of
        Expression.RecordExpr fields ->
            accessingRecordWithKnownFieldsChecks
                { nodeRange = checkInfo.parentRange
                , maybeRecordNameRange = Nothing
                , fieldName = checkInfo.fieldName
                , knownFields = fields
                }

        Expression.RecordUpdateExpression (Node recordNameRange _) setFields ->
            accessingRecordWithKnownFieldsChecks
                { nodeRange = checkInfo.parentRange
                , maybeRecordNameRange = Just recordNameRange
                , fieldName = checkInfo.fieldName
                , knownFields = setFields
                }

        Expression.LetExpression letIn ->
            Just
                { info =
                    { message = "Accessing a field outside a let...in will result in accessing it in its result"
                    , details = [ "You can replace accessing this record outside the let...in by accessing its result record after `in`." ]
                    }
                , fix =
                    keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.record }
                        ++ replaceSubExpressionByRecordAccessFix checkInfo.fieldName letIn.expression
                }

        Expression.IfBlock _ thenBranch elseBranch ->
            distributeFieldAccess "an if...then...else" [ thenBranch, elseBranch ] checkInfo

        Expression.CaseExpression caseOf ->
            distributeFieldAccess "a case...of" (List.map Tuple.second caseOf.cases) checkInfo

        _ ->
            case getRecordTypeAliasConstructorCall checkInfo.record checkInfo of
                Just recordTypeAliasConstructorCall ->
                    if List.length recordTypeAliasConstructorCall.fieldNames == List.length recordTypeAliasConstructorCall.args then
                        accessingRecordWithKnownFieldsChecks
                            { nodeRange = checkInfo.parentRange
                            , fieldName = checkInfo.fieldName
                            , maybeRecordNameRange = Nothing
                            , knownFields =
                                List.map2 (\name arg -> Node.empty ( Node.empty name, arg ))
                                    recordTypeAliasConstructorCall.fieldNames
                                    recordTypeAliasConstructorCall.args
                            }

                    else
                        Nothing

                Nothing ->
                    Nothing


pipingCompositionIntoLambdaChecks : CompositionCheckInfo -> Maybe (Error {})
pipingCompositionIntoLambdaChecks checkInfo =
    case AstHelpers.ignoresFirstLambdaResult checkInfo.later.node of
        Just wildcardRange ->
            Just
                (Rule.errorWithFix
                    { message = "Function composed with lambda will be ignored"
                    , details = [ "The lambda function ignores the first argument, meaning it will swallow the function composed into it." ]
                    }
                    wildcardRange
                    [ Fix.removeRange checkInfo.earlier.removeRange ]
                )

        Nothing ->
            Nothing


accessingRecordCompositionChecks : CompositionCheckInfo -> Maybe (Error {})
accessingRecordCompositionChecks checkInfo =
    case AstHelpers.getRecordAccessFunction checkInfo.later.node of
        Just accessFunctionFieldName ->
            (case
                checkInfo.earlier.node
                    |> toConstructedResult checkInfo.lookupTable
                    |> Maybe.andThen (\constructed -> getRecordWithKnownFields checkInfo constructed)
             of
                Just recordWithKnownField ->
                    accessingRecordWithKnownFieldsChecks
                        { nodeRange = recordWithKnownField.range
                        , fieldName = accessFunctionFieldName
                        , maybeRecordNameRange = recordWithKnownField.maybeRecordNameRange
                        , knownFields = recordWithKnownField.knownFields
                        }
                        |> Maybe.map
                            (\e ->
                                Rule.errorWithFix e.info
                                    (Node.range checkInfo.later.node)
                                    (Fix.removeRange checkInfo.later.removeRange :: e.fix)
                            )

                Nothing ->
                    Nothing
            )
                |> onNothing
                    (\() ->
                        case getRecordTypeAliasConstructorCall checkInfo.earlier.node checkInfo of
                            Just recordTypeAliasConstructorCall ->
                                if List.length recordTypeAliasConstructorCall.fieldNames == (List.length recordTypeAliasConstructorCall.args + 1) then
                                    case recordTypeAliasConstructorCall.fieldNames of
                                        -- alias to {} (which is actually allowed by elm)
                                        [] ->
                                            Nothing

                                        firstFirstName :: afterFirstFieldName ->
                                            if listFilledLast ( firstFirstName, afterFirstFieldName ) == accessFunctionFieldName then
                                                Just
                                                    (Rule.errorWithFix
                                                        { message = "Unnecessary constructing a record around a field that is then being accessed"
                                                        , details = [ "This composition will construct a record with the incoming value stored in the field " ++ wrapInBackticks accessFunctionFieldName ++ ". This exact field is then immediately accessed which means you can replace this composition by identity." ]
                                                        }
                                                        (Node.range checkInfo.later.node)
                                                        [ Fix.removeRange checkInfo.earlier.removeRange
                                                        , Fix.replaceRangeBy (Node.range checkInfo.later.node)
                                                            (qualifiedToString (qualify Fn.Basics.identity checkInfo))
                                                        ]
                                                    )

                                            else
                                                let
                                                    maybeAccessedFieldExpressionNode : Maybe (Node Expression)
                                                    maybeAccessedFieldExpressionNode =
                                                        findMap
                                                            (\field ->
                                                                if field.name == accessFunctionFieldName then
                                                                    Just field.arg

                                                                else
                                                                    Nothing
                                                            )
                                                            (List.map2 (\name arg -> { name = name, arg = arg })
                                                                recordTypeAliasConstructorCall.fieldNames
                                                                recordTypeAliasConstructorCall.args
                                                            )
                                                in
                                                case maybeAccessedFieldExpressionNode of
                                                    Just accessedFieldExpressionNode ->
                                                        Just
                                                            (Rule.errorWithFix
                                                                { message = "Accessing a field of a record where we know that field's value will return that field's value"
                                                                , details = [ "This composition will construct a record where we known the value of the field " ++ wrapInBackticks accessFunctionFieldName ++ ". This exact field is then immediately accessed which means you can replace this composition by `always` with that value." ]
                                                                }
                                                                (Node.range checkInfo.later.node)
                                                                (Fix.insertAt (Node.range checkInfo.earlier.node).start
                                                                    (qualifiedToString (qualify Fn.Basics.always checkInfo) ++ " ")
                                                                    :: Fix.removeRange checkInfo.later.removeRange
                                                                    :: replaceBySubExpressionFix (Node.range checkInfo.earlier.node) accessedFieldExpressionNode
                                                                )
                                                            )

                                                    -- compile-time error
                                                    Nothing ->
                                                        Nothing

                                else
                                    Nothing

                            Nothing ->
                                Nothing
                    )

        Nothing ->
            Nothing


getRecordWithKnownFields :
    AstHelpers.ReduceLambdaResources
        { checkInfo
            | importRecordTypeAliases : Dict ModuleName (Dict String (List String))
            , moduleRecordTypeAliases : Dict String (List String)
        }
    -> Node Expression
    -> Maybe { range : Range, maybeRecordNameRange : Maybe Range, knownFields : List (Node ( Node String, Node Expression )) }
getRecordWithKnownFields resources expressionNode =
    case AstHelpers.removeParens expressionNode of
        Node range (Expression.RecordExpr fields) ->
            Just
                { range = range
                , maybeRecordNameRange = Nothing
                , knownFields = fields
                }

        Node range (Expression.RecordUpdateExpression (Node recordNameRange _) setFields) ->
            Just
                { range = range
                , maybeRecordNameRange = Just recordNameRange
                , knownFields = setFields
                }

        nonRecordLiteralExpressionNode ->
            case getRecordTypeAliasConstructorCall nonRecordLiteralExpressionNode resources of
                Just recordTypeAliasConstructorCall ->
                    if List.length recordTypeAliasConstructorCall.fieldNames == List.length recordTypeAliasConstructorCall.args then
                        Just
                            { range = Node.range nonRecordLiteralExpressionNode
                            , maybeRecordNameRange = Nothing
                            , knownFields =
                                List.map2 (\name arg -> Node.empty ( Node.empty name, arg ))
                                    recordTypeAliasConstructorCall.fieldNames
                                    recordTypeAliasConstructorCall.args
                            }

                    else
                        Nothing

                Nothing ->
                    Nothing


accessingRecordWithKnownFieldsChecks :
    { nodeRange : Range
    , maybeRecordNameRange : Maybe Range
    , fieldName : String
    , knownFields : List (Node Expression.RecordSetter)
    }
    -> Maybe ErrorInfoAndFix
accessingRecordWithKnownFieldsChecks checkInfo =
    let
        maybeMatchingSetterValue : Maybe (Node Expression)
        maybeMatchingSetterValue =
            findMap
                (\(Node _ ( Node _ setterField, setterValue )) ->
                    if setterField == checkInfo.fieldName then
                        Just setterValue

                    else
                        Nothing
                )
                checkInfo.knownFields
    in
    case maybeMatchingSetterValue of
        Just setter ->
            Just
                { info =
                    { message = "Accessing a field of a record where we know that field's value will return that field's value"
                    , details = [ "You can replace accessing this record by just that field's value." ]
                    }
                , fix = replaceBySubExpressionFix checkInfo.nodeRange setter
                }

        Nothing ->
            case checkInfo.maybeRecordNameRange of
                Just recordNameRange ->
                    Just
                        { info =
                            { message = "Updating a record, then accessing an unchanged field will result in that field from the unchanged record"
                            , details = [ "You can replace accessing this record by just the original record variable inside the record update." ]
                            }
                        , fix =
                            [ Fix.removeRange { start = checkInfo.nodeRange.start, end = recordNameRange.start }
                            , Fix.replaceRangeBy { start = recordNameRange.end, end = checkInfo.nodeRange.end } ("." ++ checkInfo.fieldName)
                            ]
                        }

                Nothing ->
                    Nothing


distributeFieldAccess :
    String
    -> List (Node Expression)
    ->
        AstHelpers.ReduceLambdaResources
            { checkInfo
                | parentRange : Range
                , record : Node Expression
                , fieldName : String
                , importRecordTypeAliases : Dict ModuleName (Dict String (List String))
                , moduleRecordTypeAliases : Dict String (List String)
            }
    -> Maybe ErrorInfoAndFix
distributeFieldAccess kind branches checkInfo =
    let
        recordWithKnownFieldsInAllBranches : Maybe (List (Node Expression))
        recordWithKnownFieldsInAllBranches =
            traverseConcat
                (\surfaceBranch ->
                    sameInAllBranches
                        (\innerBranch ->
                            getRecordWithKnownFields checkInfo innerBranch
                                |> Maybe.map (\_ -> innerBranch)
                        )
                        surfaceBranch
                )
                branches
    in
    case recordWithKnownFieldsInAllBranches of
        Just records ->
            Just
                { info =
                    { message = "Accessing a field outside " ++ kind ++ " will result in accessing it in each branch"
                    , details = [ "You can replace accessing this record outside " ++ kind ++ " by accessing the record inside each branch." ]
                    }
                , fix =
                    keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.record }
                        ++ List.concatMap (\recordWithKnownFields -> replaceSubExpressionByRecordAccessFix checkInfo.fieldName recordWithKnownFields) records
                }

        Nothing ->
            Nothing


getRecordTypeAliasConstructorCall :
    Node Expression
    ->
        AstHelpers.ReduceLambdaResources
            { checkInfo
                | importRecordTypeAliases : Dict ModuleName (Dict String (List String))
                , moduleRecordTypeAliases : Dict String (List String)
            }
    -> Maybe { nodeRange : Range, args : List (Node Expression), fieldNames : List String }
getRecordTypeAliasConstructorCall expressionNode checkInfo =
    case AstHelpers.getValueOrFnOrFnCall checkInfo expressionNode of
        Nothing ->
            Nothing

        Just valueOrFnOrCall ->
            case ModuleNameLookupTable.moduleNameAt checkInfo.lookupTable valueOrFnOrCall.fnRange of
                Nothing ->
                    Nothing

                Just [] ->
                    Dict.get valueOrFnOrCall.fnName checkInfo.moduleRecordTypeAliases
                        |> Maybe.map
                            (\fieldNames ->
                                { nodeRange = valueOrFnOrCall.nodeRange
                                , args = valueOrFnOrCall.args
                                , fieldNames = fieldNames
                                }
                            )

                Just (moduleNamePart0 :: moduleNamePart1Up) ->
                    case Dict.get (moduleNamePart0 :: moduleNamePart1Up) checkInfo.importRecordTypeAliases of
                        Nothing ->
                            Nothing

                        Just importModuleRecordTypeAliases ->
                            Dict.get valueOrFnOrCall.fnName importModuleRecordTypeAliases
                                |> Maybe.map
                                    (\fieldNames ->
                                        { nodeRange = valueOrFnOrCall.nodeRange
                                        , args = valueOrFnOrCall.args
                                        , fieldNames = fieldNames
                                        }
                                    )


type BoundsOrder
    = BoundsLess MaybeEqual
    | BoundsGreater MaybeEqual


type MaybeEqual
    = CannotBeEqual
    | CanBeEqualInOnlyOneNumber


boundsMaybeEqual : BoundsOrder -> MaybeEqual
boundsMaybeEqual boundsOrder =
    case boundsOrder of
        BoundsLess maybeEqual ->
            maybeEqual

        BoundsGreater maybeEqual ->
            maybeEqual


boundsOrderToDescription : BoundsOrder -> String
boundsOrderToDescription boundsOrder =
    case boundsOrder of
        BoundsLess CannotBeEqual ->
            "less than"

        BoundsLess CanBeEqualInOnlyOneNumber ->
            "less than or equal to"

        BoundsGreater CannotBeEqual ->
            "greater than"

        BoundsGreater CanBeEqualInOnlyOneNumber ->
            "greater than or equal to"


numberBoundsCompare :
    { min : Float, max : Float }
    -> { min : Float, max : Float }
    -> Maybe BoundsOrder
numberBoundsCompare leftBounds rightBounds =
    if leftBounds.max < rightBounds.min then
        Just (BoundsLess CannotBeEqual)

    else if rightBounds.max < leftBounds.min then
        Just (BoundsGreater CannotBeEqual)

    else
    -- some overlap
    if
        leftBounds.max == rightBounds.min
    then
        Just (BoundsLess CanBeEqualInOnlyOneNumber)

    else if rightBounds.max == leftBounds.min then
        Just (BoundsGreater CanBeEqualInOnlyOneNumber)

    else
        -- overlap covers more than 1 number
        Nothing


normalGetNumberBounds : Expression -> { min : Float, max : Float }
normalGetNumberBounds expressionNormal =
    case expressionNormal of
        Expression.Floatable int ->
            { min = int, max = int }

        Expression.Negation (Node _ inNegation) ->
            numberBoundsNegate (normalGetNumberBounds inNegation)

        Expression.Application ((Node _ (Expression.FunctionOrValue moduleOrigin name)) :: args) ->
            let
                fn : ( ModuleName, String )
                fn =
                    ( moduleOrigin, name )
            in
            case args of
                [ Node _ arg0 ] ->
                    if fn == Fn.Array.length then
                        collectionSizeToNumberBounds
                            (normalDetermineCollectionSize arg0)

                    else if fn == Fn.List.length then
                        collectionSizeToNumberBounds
                            (normalDetermineCollectionSize arg0)

                    else if fn == Fn.String.length then
                        collectionSizeToNumberBounds
                            (normalDetermineCollectionSize arg0)

                    else if fn == Fn.Set.size then
                        collectionSizeToNumberBounds
                            (normalDetermineCollectionSize arg0)

                    else if fn == Fn.Dict.size then
                        collectionSizeToNumberBounds
                            (normalDetermineCollectionSize arg0)

                    else if fn == Fn.Basics.abs then
                        let
                            arg0NumberBounds : { min : Float, max : Float }
                            arg0NumberBounds =
                                normalGetNumberBounds arg0
                        in
                        if arg0NumberBounds.min >= 0 then
                            arg0NumberBounds

                        else if arg0NumberBounds.max <= 0 then
                            numberBoundsNegate arg0NumberBounds

                        else
                            -- min <= -1 && max >= 1
                            { min = 0, max = max -arg0NumberBounds.min arg0NumberBounds.max }

                    else
                        numberBoundsUnknown

                [ Node _ arg0, Node _ arg1 ] ->
                    if fn == Fn.Basics.min then
                        numberBoundsCombineEachBoundWith Basics.min
                            (normalGetNumberBounds arg0)
                            (normalGetNumberBounds arg1)

                    else if fn == Fn.Basics.max then
                        numberBoundsCombineEachBoundWith Basics.max
                            (normalGetNumberBounds arg0)
                            (normalGetNumberBounds arg1)

                    else
                        numberBoundsUnknown

                [ Node _ arg0, Node _ arg1, _ ] ->
                    if fn == Fn.Basics.clamp then
                        let
                            minNormal : { min : Float, max : Float }
                            minNormal =
                                normalGetNumberBounds arg0

                            maxNormal : { min : Float, max : Float }
                            maxNormal =
                                normalGetNumberBounds arg1
                        in
                        -- note that  clamp lo hi  is different from both
                        --     max lo (min hi n)
                        --     min hi (max lo)
                        -- as it can both return values greater than hi and less than lo
                        { min =
                            -- not minNormal.min because
                            -- e.g. with Basics.clamp 10 0, giving it 11 would return 0
                            Basics.min minNormal.min maxNormal.min
                        , max =
                            -- not maxNormal.max because
                            -- e.g. with Basics.clamp 10 0, giving it 1 would return 10
                            Basics.max minNormal.max maxNormal.max
                        }

                    else
                        numberBoundsUnknown

                _ ->
                    numberBoundsUnknown

        Expression.OperatorApplication operator _ (Node _ left) (Node _ right) ->
            case operator of
                "+" ->
                    numberBoundsCombineEachBoundWith (+)
                        (normalGetNumberBounds left)
                        (normalGetNumberBounds right)

                "-" ->
                    numberBoundsCombineEachBoundWith (+)
                        (normalGetNumberBounds left)
                        (numberBoundsNegate (normalGetNumberBounds right))

                "*" ->
                    numberBoundsMultiply
                        (normalGetNumberBounds left)
                        (normalGetNumberBounds right)

                _ ->
                    numberBoundsUnknown

        Expression.LetExpression letIn ->
            normalGetNumberBounds (Node.value letIn.expression)

        Expression.IfBlock _ (Node _ onTrue) (Node _ onFalse) ->
            let
                onTrueNumberBounds : { min : Float, max : Float }
                onTrueNumberBounds =
                    normalGetNumberBounds onTrue

                onFalseNumberBounds : { min : Float, max : Float }
                onFalseNumberBounds =
                    normalGetNumberBounds onFalse
            in
            { min = Basics.min onTrueNumberBounds.min onFalseNumberBounds.min
            , max = Basics.max onTrueNumberBounds.max onFalseNumberBounds.max
            }

        Expression.CaseExpression caseOf ->
            case caseOf.cases of
                ( _, Node _ case0Result ) :: case1Up ->
                    List.foldl
                        (\( _, Node _ caseResult ) soFar ->
                            let
                                caseResultNumberBounds : { min : Float, max : Float }
                                caseResultNumberBounds =
                                    normalGetNumberBounds caseResult
                            in
                            { min = Basics.min caseResultNumberBounds.min soFar.min
                            , max = Basics.max caseResultNumberBounds.max soFar.max
                            }
                        )
                        (normalGetNumberBounds case0Result)
                        case1Up

                -- invalid syntax
                [] ->
                    numberBoundsUnknown

        _ ->
            numberBoundsUnknown


numberBoundsMultiply : { max : Float, min : Float } -> { min : Float, max : Float } -> { max : Float, min : Float }
numberBoundsMultiply leftBounds rightBounds =
    let
        leftMinTimesRightMin : Float
        leftMinTimesRightMin =
            leftBounds.min * rightBounds.min

        leftMinTimesRightMax : Float
        leftMinTimesRightMax =
            leftBounds.min * rightBounds.max

        leftMaxTimesRightMin : Float
        leftMaxTimesRightMin =
            leftBounds.max * rightBounds.min

        leftMaxTimesRightMax : Float
        leftMaxTimesRightMax =
            leftBounds.max * rightBounds.max
    in
    { min =
        leftMinTimesRightMin
            |> min leftMinTimesRightMax
            |> min leftMaxTimesRightMin
            |> min leftMaxTimesRightMax
    , max =
        leftMinTimesRightMin
            |> max leftMinTimesRightMax
            |> max leftMaxTimesRightMin
            |> max leftMaxTimesRightMax
    }


numberBoundsNegate : { max : Float, min : Float } -> { min : Float, max : Float }
numberBoundsNegate numberBounds =
    { min = -numberBounds.max
    , max = -numberBounds.min
    }


numberBoundsAlterEach : (Float -> Float) -> { max : Float, min : Float } -> { min : Float, max : Float }
numberBoundsAlterEach boundChange numberBounds =
    { min = boundChange numberBounds.min
    , max = boundChange numberBounds.max
    }


numberBoundsCombineEachBoundWith :
    (Float -> Float -> Float)
    -> { min : Float, max : Float }
    -> { min : Float, max : Float }
    -> { min : Float, max : Float }
numberBoundsCombineEachBoundWith combineBound leftBounds rightBounds =
    { min = combineBound leftBounds.min rightBounds.min
    , max = combineBound leftBounds.max rightBounds.max
    }


numberBoundsUnknown : { min : Float, max : Float }
numberBoundsUnknown =
    { min = negativeInfinity, max = positiveInfinity }


positiveInfinity : Float
positiveInfinity =
    1 / 0.0


negativeInfinity : Float
negativeInfinity =
    -(1 / 0.0)



-- FIX HELPERS


parenthesizeIf : Bool -> String -> String
parenthesizeIf shouldParenthesize source =
    if shouldParenthesize then
        "(" ++ source ++ ")"

    else
        source


parenthesizeIfNeededFix : Node Expression -> List Fix
parenthesizeIfNeededFix (Node expressionRange expression) =
    if needsParens expression then
        parenthesizeFix expressionRange

    else
        []


parenthesizeFix : Range -> List Fix
parenthesizeFix toSurround =
    [ Fix.insertAt toSurround.start "("
    , Fix.insertAt toSurround.end ")"
    ]


keepOnlyFix : { parentRange : Range, keep : Range } -> List Fix
keepOnlyFix config =
    [ Fix.removeRange
        { start = config.parentRange.start
        , end = config.keep.start
        }
    , Fix.removeRange
        { start = config.keep.end
        , end = config.parentRange.end
        }
    ]


keepOnlyAndSurroundWithFix : { parentRange : Range, keep : Range, left : String, right : String } -> List Fix
keepOnlyAndSurroundWithFix config =
    [ Fix.replaceRangeBy
        { start = config.parentRange.start
        , end = config.keep.start
        }
        config.left
    , Fix.replaceRangeBy
        { start = config.keep.end
        , end = config.parentRange.end
        }
        config.right
    ]


keepOnlyAndParenthesizeFix : { parentRange : Range, keep : Range } -> List Fix
keepOnlyAndParenthesizeFix config =
    [ Fix.replaceRangeBy { start = config.parentRange.start, end = config.keep.start } "("
    , Fix.replaceRangeBy { start = config.keep.end, end = config.parentRange.end } ")"
    ]


replaceBySubPatternFix : Range -> Node Pattern -> List Fix
replaceBySubPatternFix outerRange (Node subRange sub) =
    if patternNeedsParens sub then
        keepOnlyAndParenthesizeFix { parentRange = outerRange, keep = subRange }

    else
        keepOnlyFix { parentRange = outerRange, keep = subRange }


replaceBySubExpressionFix : Range -> Node Expression -> List Fix
replaceBySubExpressionFix outerRange (Node exprRange exprValue) =
    if needsParens exprValue then
        keepOnlyAndParenthesizeFix { parentRange = outerRange, keep = exprRange }

    else
        keepOnlyFix { parentRange = outerRange, keep = exprRange }


replaceCallBySubExpressionFix : Range -> FunctionCallStyle -> Node Expression -> List Fix
replaceCallBySubExpressionFix outerRange callStyle expressionNode =
    let
        (Node replacementExpressionRange replacementExpression) =
            AstHelpers.removeParens expressionNode
    in
    if callReplacementNeedsParens callStyle replacementExpression then
        keepOnlyAndParenthesizeFix { parentRange = outerRange, keep = replacementExpressionRange }

    else
        keepOnlyFix { parentRange = outerRange, keep = replacementExpressionRange }


callReplaceOperationBySubExpressionsFix :
    Range
    ->
        { originalCallStyle : FunctionCallStyle
        , subFunctionArg : Node Expression
        , lastArg : Node Expression
        , parenthesize : Bool
        }
    -> List Fix
callReplaceOperationBySubExpressionsFix parentRange config =
    case config.originalCallStyle of
        CallStyle.Application ->
            let
                subFunctionArgNeedsParens : Bool
                subFunctionArgNeedsParens =
                    needsParens (Node.value config.subFunctionArg)
            in
            [ Fix.replaceRangeBy
                { start = parentRange.start
                , end = (Node.range config.subFunctionArg).start
                }
                ((if config.parenthesize then
                    "("

                  else
                    ""
                 )
                    ++ (if subFunctionArgNeedsParens then
                            "("

                        else
                            ""
                       )
                )
            , Fix.replaceRangeBy
                { start = (Node.range config.subFunctionArg).end
                , end = (Node.range config.lastArg).start
                }
                ((if subFunctionArgNeedsParens then
                    ")"

                  else
                    ""
                 )
                    ++ " "
                )
            , Fix.replaceRangeBy
                { start = (Node.range config.lastArg).end
                , end = parentRange.end
                }
                (if config.parenthesize then
                    ")"

                 else
                    ""
                )
            ]

        CallStyle.Pipe CallStyle.RightToLeft ->
            let
                subFunctionArgNeedsParens : Bool
                subFunctionArgNeedsParens =
                    needsParens (Node.value config.subFunctionArg)
            in
            [ Fix.replaceRangeBy
                { start = parentRange.start
                , end = (Node.range config.subFunctionArg).start
                }
                ((if config.parenthesize then
                    "("

                  else
                    ""
                 )
                    ++ (if subFunctionArgNeedsParens then
                            "("

                        else
                            ""
                       )
                )
            , Fix.replaceRangeBy
                { start = (Node.range config.subFunctionArg).end
                , end = (Node.range config.lastArg).start
                }
                ((if subFunctionArgNeedsParens then
                    ")"

                  else
                    ""
                 )
                    ++ " <| "
                )
            , Fix.replaceRangeBy
                { start = (Node.range config.lastArg).end
                , end = parentRange.end
                }
                (if config.parenthesize then
                    ")"

                 else
                    ""
                )
            ]

        CallStyle.Pipe CallStyle.LeftToRight ->
            let
                subFunctionArgNeedsParens : Bool
                subFunctionArgNeedsParens =
                    needsParens (Node.value config.subFunctionArg)
            in
            [ Fix.replaceRangeBy
                { start = parentRange.start
                , end = (Node.range config.lastArg).start
                }
                (if config.parenthesize then
                    "("

                 else
                    ""
                )
            , Fix.replaceRangeBy
                { start = (Node.range config.lastArg).end
                , end = (Node.range config.subFunctionArg).start
                }
                (" |> "
                    ++ (if subFunctionArgNeedsParens then
                            "("

                        else
                            ""
                       )
                )
            , Fix.replaceRangeBy
                { start = (Node.range config.subFunctionArg).end
                , end = parentRange.end
                }
                ((if subFunctionArgNeedsParens then
                    ")"

                  else
                    ""
                 )
                    ++ (if config.parenthesize then
                            ")"

                        else
                            ""
                       )
                )
            ]


replaceSubExpressionByRecordAccessFix : String -> Node Expression -> List Fix
replaceSubExpressionByRecordAccessFix fieldName (Node exprRange exprValue) =
    if needsParens exprValue then
        [ Fix.insertAt exprRange.start "("
        , Fix.insertAt exprRange.end (")." ++ fieldName)
        ]

    else
        [ Fix.insertAt exprRange.end ("." ++ fieldName) ]


rangeBetweenExclusive : Range -> Range -> Range
rangeBetweenExclusive aRange bRange =
    case Range.compareLocations aRange.start bRange.start of
        GT ->
            { start = bRange.end, end = aRange.start }

        -- EQ | LT
        _ ->
            { start = aRange.end, end = bRange.start }


{-| Create a new range that fully includes `fromInclusive`
and stops just before `toExclusive`.

Useful if you want to for example strip away the function name and everything up until the first argument.

-}
rangeFromInclusiveToExclusive : { fromInclusive : Range, toExclusive : Range } -> Range
rangeFromInclusiveToExclusive ranges =
    case Range.compareLocations ranges.fromInclusive.start ranges.toExclusive.start of
        GT ->
            { start = ranges.toExclusive.end, end = ranges.fromInclusive.end }

        -- EQ | LT
        _ ->
            { start = ranges.fromInclusive.start, end = ranges.toExclusive.start }


{-| Takes the ranges of two neighboring elements and
returns a range that includes the specified element and everything between them.

This is useful when you can't use `replaceBySubExpressionFix` and `keepOnlyFix` because there is no
existing node that could be kept.

For example, you might want to remove `|> identity` in `f |> g |> identity`. `elm-syntax` might represent this as (simplified)

    Op (Var "f") "|>" (Op (Var "g") "|>" (Var "identity"))

In practice, you will check this syntax tree recursively, leading to situations where we only know

  - the previous/next element which we want to keep
  - and the current element which we want to remove

-}
andBetweenRange : { excluded : Range, included : Range } -> Range
andBetweenRange ranges =
    case Range.compare ranges.excluded ranges.included of
        LT ->
            { start = ranges.excluded.end, end = ranges.included.end }

        -- GT | EQ ->
        _ ->
            { start = ranges.included.start, end = ranges.excluded.start }


listLiteralToNestedTupleFix : { tupledBeginningLength : Int, elements : List String } -> String
listLiteralToNestedTupleFix config =
    case config.elements of
        [] ->
            "()"

        element0 :: element1Up ->
            let
                tupledElements : List String
                tupledElements =
                    element0 :: List.take (config.tupledBeginningLength - 1) element1Up
            in
            case List.drop (config.tupledBeginningLength - 1) element1Up of
                [] ->
                    toNestedTupleFix tupledElements

                firstUntupledElementRange :: afterFirstUntupledElementRange ->
                    toNestedTupleFix
                        (tupledElements
                            ++ [ "[ "
                                    ++ String.join ", " (firstUntupledElementRange :: afterFirstUntupledElementRange)
                                    ++ " ]"
                               ]
                        )


collapsedConsToNestedTupleFix : { tupledBeginningLength : Int, beginningElements : ( String, List String ), tail : String } -> String
collapsedConsToNestedTupleFix config =
    toNestedTupleFix
        (listFilledHead config.beginningElements
            :: List.take (config.tupledBeginningLength - 1) (listFilledTail config.beginningElements)
            ++ [ String.join " :: "
                    (List.drop (config.tupledBeginningLength - 1) (listFilledTail config.beginningElements)
                        ++ [ config.tail ]
                    )
               ]
        )


toNestedTupleFix : List String -> String
toNestedTupleFix parts =
    case parts of
        [] ->
            "()"

        part0 :: parts1Up ->
            toNestedTupleFixFromPartial ( part0, parts1Up )


toNestedTupleFixFromPartial : ( String, List String ) -> String
toNestedTupleFixFromPartial ( firstPart, secondPartUp ) =
    case secondPartUp of
        [] ->
            firstPart

        second :: thirdUp ->
            "(" ++ firstPart ++ ", " ++ toNestedTupleFixFromPartial ( second, thirdUp ) ++ ")"


rangeSpansMultipleLines : Range -> Bool
rangeSpansMultipleLines range =
    range.start.row /= range.end.row


rangeContainsLocation : Location -> Range -> Bool
rangeContainsLocation location range =
    not
        ((Range.compareLocations location range.start == LT)
            || (Range.compareLocations location range.end == GT)
        )


rangeWithoutBoundaries : Range -> Range
rangeWithoutBoundaries range =
    { start = startWithoutBoundary range
    , end = endWithoutBoundary range
    }


startWithoutBoundary : Range -> Location
startWithoutBoundary range =
    { row = range.start.row, column = range.start.column + 1 }


endWithoutBoundary : Range -> Location
endWithoutBoundary range =
    { row = range.end.row, column = range.end.column - 1 }


removeBoundariesFix : Node a -> List Fix
removeBoundariesFix (Node range _) =
    removeRangeBoundariesFix range


removeRangeBoundariesFix : Range -> List Fix
removeRangeBoundariesFix range =
    keepOnlyFix { parentRange = range, keep = rangeWithoutBoundaries range }


leftBoundaryRange : Range -> Range
leftBoundaryRange range =
    { start = range.start
    , end = { row = range.start.row, column = range.start.column + 1 }
    }


{-| Shortcut for `alwaysResultsInConstantError` with `replacementNeedsParens = False`.

If you want to replace to something like `Just []`,
use `alwaysResultsInConstantError` with `replacementNeedsParens = True`.

-}
alwaysResultsInUnparenthesizedConstantError :
    String
    -> { replacement : QualifyResources {} -> String }
    -> CallCheckInfo
    -> Error {}
alwaysResultsInUnparenthesizedConstantError usingSituation config checkInfo =
    alwaysResultsInConstantError usingSituation
        { replacement = config.replacement
        , replacementNeedsParens = False
        }
        checkInfo


{-| Regardless of what the next incoming value will be, the result is already determined to be a given constant.

For example, `List.repeat 0` will always return [], whatever the argument will be.

If your function also always returns a constant but it does not have an irrelevant next argument,
like `List.range 1 0`, use `resultsInConstantError`

-}
alwaysResultsInConstantError :
    String
    ->
        { replacement : QualifyResources {} -> String
        , replacementNeedsParens : Bool
        }
    -> CallCheckInfo
    -> Error {}
alwaysResultsInConstantError usingSituation config checkInfo =
    let
        remainingArgCount : Int
        remainingArgCount =
            checkInfo.argCount - (1 + List.length checkInfo.argsAfterFirst)

        replacement : QualifyResources {} -> String
        replacement res =
            case remainingArgCount of
                -- fully applied
                0 ->
                    config.replacement res

                -- one arg curried
                1 ->
                    qualifiedToString (qualify Fn.Basics.always res)
                        ++ " "
                        ++ (if config.replacementNeedsParens then
                                "(" ++ config.replacement res ++ ")"

                            else
                                config.replacement res
                           )

                -- multiple args curried
                atLeast2 ->
                    "(\\" ++ String.repeat atLeast2 "_ " ++ "-> " ++ config.replacement res ++ ")"
    in
    Rule.errorWithFix
        { message = usingSituation ++ " will always result in " ++ config.replacement defaultQualifyResources
        , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ "." ]
        }
        checkInfo.fnRange
        [ Fix.replaceRangeBy checkInfo.parentRange (replacement (extractQualifyResources checkInfo)) ]


{-| The result in the given situation is determined to be a given constant.

For example, `List.range 1 0` will return [].

If your function also always returns a constant but has an irrelevant next argument,
like `List.repeat 0`, use `alwaysResultsInConstantError`

-}
resultsInConstantError : String -> (QualifyResources {} -> String) -> CallCheckInfo -> Error {}
resultsInConstantError usingSituation replacement checkInfo =
    Rule.errorWithFix
        { message = usingSituation ++ " will result in " ++ replacement defaultQualifyResources
        , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ "." ]
        }
        checkInfo.fnRange
        [ Fix.replaceRangeBy checkInfo.parentRange (replacement (extractQualifyResources checkInfo)) ]


compositionReplaceByFix :
    String
    -> { checkInfo | later : { later | range : Range }, earlier : { earlier | removeRange : Range } }
    -> List Fix
compositionReplaceByFix replacement checkInfo =
    [ Fix.replaceRangeBy checkInfo.later.range replacement
    , Fix.removeRange checkInfo.earlier.removeRange
    ]


compositionReplaceByFnFix :
    ( ModuleName, String )
    -> QualifyResources { checkInfo | later : { later | range : Range }, earlier : { earlier | removeRange : Range } }
    -> List Fix
compositionReplaceByFnFix replacementFn checkInfo =
    compositionReplaceByFix (qualifiedToString (qualify replacementFn checkInfo)) checkInfo


operationDoesNotChangeSpecificLastArgErrorInfo : { fn : ( ModuleName, String ), specific : TypeSubsetProperties otherTypeSubsetProperties } -> { message : String, details : List String }
operationDoesNotChangeSpecificLastArgErrorInfo config =
    let
        specificLastArgReference : String
        specificLastArgReference =
            typeSubsetDescriptionDefinite "the given" config.specific
    in
    { message = qualifiedToString config.fn ++ " on " ++ typeSubsetDescriptionIndefinite config.specific ++ " will result in " ++ specificLastArgReference
    , details = [ "You can replace this call by " ++ specificLastArgReference ++ "." ]
    }


{-| In your specific situation, the last incoming argument will always be returned unchanged.

For example, `List.map identity` will not change whatever list comes next. It is equivalent to `identity`

Use `returnsArgError` with the given last arg as `arg` when the last arg is already present.

-}
alwaysReturnsLastArgError :
    String
    -> { lastArgProperties | represents : String }
    -> CallCheckInfo
    -> Error {}
alwaysReturnsLastArgError usingSpecificSituation lastArgProperties checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just lastArg ->
            returnsArgError usingSpecificSituation { arg = lastArg, argRepresents = lastArgProperties.represents } checkInfo

        Nothing ->
            -- Not enough arguments
            let
                replacement : QualifyResources res -> String
                replacement =
                    case checkInfo.argCount - (1 + List.length checkInfo.argsAfterFirst) - 1 of
                        0 ->
                            \res -> qualifiedToString (qualify Fn.Basics.identity res)

                        1 ->
                            \res -> qualifiedToString (qualify Fn.Basics.always res) ++ " " ++ qualifiedToString (qualify Fn.Basics.identity res)

                        atLeast2 ->
                            \res -> "(\\" ++ String.repeat atLeast2 "_ " ++ "-> " ++ qualifiedToString (qualify Fn.Basics.identity res) ++ ")"
            in
            Rule.errorWithFix
                { message = usingSpecificSituation ++ " will always return the same given " ++ lastArgProperties.represents
                , details =
                    [ "You can replace this call by " ++ replacement defaultQualifyResources ++ "." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange (replacement checkInfo) ]


{-| In your specific situation, the given arg will always be returned unchanged.

Use `alwaysReturnsLastArgError` when the last arg could be absent and it would still not change, like with `List.map identity`.

-}
returnsArgError :
    String
    ->
        { argRepresents : String
        , arg : Node Expression
        }
    -> CallCheckInfo
    -> Error {}
returnsArgError usingSituation config checkInfo =
    Rule.errorWithFix
        { message = usingSituation ++ " will always return the same given " ++ config.argRepresents
        , details =
            [ "You can replace this call by the " ++ config.argRepresents ++ " itself." ]
        }
        checkInfo.fnRange
        (replaceCallBySubExpressionFix checkInfo.parentRange checkInfo.callStyle config.arg)


{-| `ErrorInfoAndFix` for when a specific composition is equivalent to identity, e.g. `Just >> Maybe.withDefault x`.
-}
compositionAlwaysReturnsIncomingError : String -> CompositionIntoCheckInfo -> ErrorInfoAndFix
compositionAlwaysReturnsIncomingError message checkInfo =
    if checkInfo.isEmbeddedInComposition then
        { info =
            { message = message
            , details = [ "You can remove these two functions." ]
            }
        , fix =
            [ Fix.removeRange checkInfo.earlier.removeRange
            , Fix.removeRange checkInfo.later.removeRange
            ]
        }

    else
        { info =
            { message = message
            , details = [ "You can replace this composition by identity." ]
            }
        , fix = compositionReplaceByFnFix Fn.Basics.identity checkInfo
        }


{-| Use in combination with
`findMapNeighboring` where finding returns the element's node.

Works for patterns and expressions.

-}
listLiteralRemoveElementFix : { before : Maybe (Node element), found : Node element, after : Maybe (Node element) } -> List Fix
listLiteralRemoveElementFix toRemove =
    case toRemove.before of
        -- found after first element
        Just (Node beforeRange _) ->
            [ Fix.removeRange
                { start = beforeRange.end
                , end = (Node.range toRemove.found).end
                }
            ]

        Nothing ->
            case toRemove.after of
                -- found the only element
                Nothing ->
                    [ Fix.removeRange (Node.range toRemove.found) ]

                -- found first element
                Just (Node afterRange _) ->
                    [ Fix.removeRange
                        { start = (Node.range toRemove.found).start
                        , end = afterRange.start
                        }
                    ]


{-| Use in combination with
`findMapNeighboring` where finding returns the element's node.

Works for patterns and expressions.

-}
collapsedConsRemoveElementFix :
    { toRemove : { before : Maybe (Node element), after : Maybe (Node element), found : Node element }
    , tailRange : Range
    }
    -> List Fix
collapsedConsRemoveElementFix config =
    case config.toRemove.before of
        -- found after first consed element
        Just (Node beforeRange _) ->
            [ Fix.removeRange
                { start = beforeRange.end
                , end = (Node.range config.toRemove.found).end
                }
            ]

        Nothing ->
            case config.toRemove.after of
                -- found the only consed element
                Nothing ->
                    [ Fix.removeRange
                        { start = (Node.range config.toRemove.found).start, end = config.tailRange.start }
                    ]

                -- found first consed element
                Just (Node afterRange _) ->
                    [ Fix.removeRange
                        { start = (Node.range config.toRemove.found).start
                        , end = afterRange.start
                        }
                    ]


{-| Detect and provide removing fixes for a function that will return a specific construct

    \a -> construct b
    --> \a -> b

    \a -> if cond then construct b else construct c -- and other branching
    --> \a -> if cond then b else c

    f << construct -- nd other compositions where construct is last
    --> f

-}
constructsOrComposesInto :
    { constructWithOneValue
        | fn : ( ModuleName, String )
        , getValue : ModuleNameLookupTable -> Node Expression -> Maybe (Node Expression)
    }
    -> AstHelpers.ReduceLambdaResources a
    -> Node Expression
    -> Maybe (List Fix)
constructsOrComposesInto constructWithOneValue context expressionNode =
    expressionNode
        |> toConstructedResult
            context.lookupTable
        |> Maybe.andThen
            (\constructed ->
                sameInAllBranches
                    (\branch ->
                        getValueWithNodeRange (constructWithOneValue.getValue context.lookupTable) branch
                    )
                    constructed
            )
        |> Maybe.map
            (\wrapCalls ->
                List.concatMap (\call -> replaceBySubExpressionFix call.nodeRange call.value) wrapCalls
            )
        |> onNothing
            (\() ->
                expressionNode
                    |> getCompositionToLast
                    |> Maybe.andThen
                        (\compositionToLast ->
                            if AstHelpers.isSpecificValueOrFn constructWithOneValue.fn context compositionToLast.last then
                                Just
                                    [ Fix.removeRange
                                        (andBetweenRange { included = Node.range compositionToLast.last, excluded = Node.range compositionToLast.earlier })
                                    ]

                            else
                                Nothing
                        )
            )



-- STRING


wrapInBackticks : String -> String
wrapInBackticks s =
    "`" ++ s ++ "`"



-- PARSERS


{-| Get the last function in `earlier` and the earliest function in `later` that's not itself a composition.

E.g. for `(i << h) << (g << f)`

    getInnerComposition { earlier = (g << f), later = (i << h) }
    --> { earlier = g, later = h }

which works for nested parens and any combination of `>>` and `<<`.

The returned `removeEarlier/LaterRange` can be used together with `Fix.removeRange` to only remove one side of the composition.
The returned `isEmbeddedInComposition` is true if there are other functions composed before `earlier` or after `later`.

-}
getInnerComposition :
    { compositionInfo
        | earlier : Node Expression
        , later : Node Expression
    }
    ->
        { earlier :
            { node : Node Expression
            , removeRange : Range
            }
        , later :
            { node : Node Expression
            , removeRange : Range
            }
        , isEmbeddedInComposition : Bool
        }
getInnerComposition compositionInfo =
    let
        laterAsComposition : Maybe { earliest : Node Expression, later : Node Expression }
        laterAsComposition =
            getCompositionFromEarliest compositionInfo.later

        earlierAsComposition : Maybe { earlier : Node Expression, last : Node Expression }
        earlierAsComposition =
            getCompositionToLast compositionInfo.earlier
    in
    { earlier =
        case earlierAsComposition of
            Just earlier ->
                { node = earlier.last
                , removeRange =
                    andBetweenRange { included = Node.range earlier.last, excluded = Node.range earlier.earlier }
                }

            Nothing ->
                { node = compositionInfo.earlier
                , removeRange =
                    andBetweenRange { included = Node.range compositionInfo.earlier, excluded = Node.range compositionInfo.later }
                }
    , later =
        case laterAsComposition of
            Just later ->
                { node = later.earliest
                , removeRange =
                    andBetweenRange { included = Node.range later.earliest, excluded = Node.range later.later }
                }

            Nothing ->
                { node = compositionInfo.later
                , removeRange =
                    andBetweenRange { included = Node.range compositionInfo.later, excluded = Node.range compositionInfo.earlier }
                }
    , isEmbeddedInComposition =
        isJust earlierAsComposition || isJust laterAsComposition
    }


{-| The function applied later than all the others in a composition chain and the function directly before.

E.g. `(f << g) << h` would return `Just { earlier = g, last = f }`

-}
getCompositionToLast : Node Expression -> Maybe { earlier : Node Expression, last : Node Expression }
getCompositionToLast expressionNode =
    case getFullComposition expressionNode of
        Just fullComposition ->
            case getCompositionToLast fullComposition.composedLater of
                (Just _) as justActualLast ->
                    justActualLast

                Nothing ->
                    Just { earlier = fullComposition.earlier, last = fullComposition.composedLater }

        Nothing ->
            Nothing


{-| The function applied earlier than all the others in a composition chain and the function directly before.

E.g. `f << (g << h)` would return `Just { later = g, earliest = h }`

-}
getCompositionFromEarliest : Node Expression -> Maybe { earliest : Node Expression, later : Node Expression }
getCompositionFromEarliest expressionNode =
    case getFullComposition expressionNode of
        Just fullComposition ->
            case getCompositionFromEarliest fullComposition.earlier of
                (Just _) as justActualLast ->
                    justActualLast

                Nothing ->
                    Just { earliest = fullComposition.earlier, later = fullComposition.composedLater }

        Nothing ->
            Nothing


{-| Unlike `AstHelpers.getComposition` which only looks at the earliest 2 composed functions, e.g. `f << g` for `f << g << h`.
`getFullComposition` returns the later part as an expression, e.g. `{ earlier = f, composedLater = g << h }`.
-}
getFullComposition : Node Expression -> Maybe { earlier : Node Expression, composedLater : Node Expression }
getFullComposition expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.OperatorApplication "<<" _ composedLater earlier ->
            Just { earlier = earlier, composedLater = composedLater }

        Expression.OperatorApplication ">>" _ earlier composedLater ->
            Just { earlier = earlier, composedLater = composedLater }

        _ ->
            Nothing


lastArgCallOfCallNeedsParens : { parent : FunctionCallStyle, lastArg : FunctionCallStyle } -> Bool
lastArgCallOfCallNeedsParens style =
    case style.lastArg of
        CallStyle.Application ->
            style.parent == CallStyle.Application

        CallStyle.Pipe lastArgCallPipeDirection ->
            style.parent /= CallStyle.Pipe lastArgCallPipeDirection


lastArgOfCallNeedsParens : FunctionCallStyle -> Expression -> Bool
lastArgOfCallNeedsParens callStyle lastArg =
    case lastArg of
        Expression.Application _ ->
            callStyle == CallStyle.Application

        Expression.OperatorApplication "<|" _ _ _ ->
            callStyle /= CallStyle.pipeRightToLeft

        Expression.OperatorApplication "|>" _ _ _ ->
            callStyle /= CallStyle.pipeLeftToRight

        _ ->
            needsParens lastArg


callReplacementNeedsParens : FunctionCallStyle -> Expression -> Bool
callReplacementNeedsParens originalCallStyle replacementExpression =
    case replacementExpression of
        Expression.ParenthesizedExpression inParens ->
            callReplacementNeedsParens originalCallStyle (Node.value inParens)

        Expression.Application _ ->
            False

        Expression.OperatorApplication "<|" _ _ _ ->
            originalCallStyle /= CallStyle.pipeRightToLeft

        Expression.OperatorApplication "|>" _ _ _ ->
            originalCallStyle /= CallStyle.pipeLeftToRight

        _ ->
            needsParens replacementExpression


patternNeedsParens : Pattern -> Bool
patternNeedsParens pattern =
    case pattern of
        Pattern.AllPattern ->
            False

        Pattern.UnitPattern ->
            False

        Pattern.HexPattern _ ->
            False

        Pattern.IntPattern _ ->
            False

        Pattern.CharPattern _ ->
            False

        Pattern.StringPattern _ ->
            False

        Pattern.VarPattern _ ->
            False

        Pattern.ParenthesizedPattern _ ->
            False

        Pattern.TuplePattern _ ->
            False

        Pattern.RecordPattern _ ->
            False

        Pattern.ListPattern _ ->
            False

        Pattern.AsPattern _ _ ->
            True

        Pattern.UnConsPattern _ _ ->
            True

        Pattern.NamedPattern _ values ->
            case values of
                [] ->
                    False

                _ :: _ ->
                    True

        -- invalid syntax
        Pattern.FloatPattern _ ->
            False


needsParens : Expression -> Bool
needsParens expr =
    case expr of
        Expression.Application _ ->
            True

        Expression.OperatorApplication _ _ _ _ ->
            True

        Expression.IfBlock _ _ _ ->
            True

        Expression.Negation _ ->
            False

        Expression.LetExpression _ ->
            True

        Expression.CaseExpression _ ->
            True

        Expression.LambdaExpression _ ->
            True

        Expression.UnitExpr ->
            False

        Expression.CharLiteral _ ->
            False

        Expression.Integer _ ->
            False

        Expression.Hex _ ->
            False

        Expression.Floatable _ ->
            False

        Expression.Literal _ ->
            False

        Expression.GLSLExpression _ ->
            False

        Expression.PrefixOperator _ ->
            False

        Expression.RecordAccessFunction _ ->
            False

        Expression.RecordAccess _ _ ->
            False

        Expression.FunctionOrValue _ _ ->
            False

        Expression.ParenthesizedExpression _ ->
            False

        Expression.TupledExpression _ ->
            False

        Expression.ListExpr _ ->
            False

        Expression.RecordExpr _ ->
            False

        Expression.RecordUpdateExpression _ _ ->
            False

        -- IMPOSSIBLE --
        Expression.Operator _ ->
            False


{-| The result of a lambda with one argument or the first argument of a `Basics.always` call
-}
toConstructedResult : ModuleNameLookupTable -> Node Expression -> Maybe (Node Expression)
toConstructedResult lookupTable expressionNode =
    case AstHelpers.getSpecificUnreducedFnCall Fn.Basics.always lookupTable expressionNode of
        Just alwaysCall ->
            Just alwaysCall.firstArg

        Nothing ->
            case Node.value (AstHelpers.removeParens expressionNode) of
                Expression.LambdaExpression lambda ->
                    case lambda.args of
                        [ _ ] ->
                            Just lambda.expression

                        _ ->
                            Nothing

                _ ->
                    Nothing


sameInAllBranches :
    (Node Expression -> Maybe info)
    -> Node Expression
    -> Maybe (List info)
sameInAllBranches getSpecific baseExpressionNode =
    case getSpecific baseExpressionNode of
        Just specific ->
            Just [ specific ]

        Nothing ->
            case Node.value (AstHelpers.removeParens baseExpressionNode) of
                Expression.LetExpression letIn ->
                    sameInAllBranches getSpecific letIn.expression

                Expression.IfBlock _ thenBranch elseBranch ->
                    case sameInAllBranches getSpecific thenBranch of
                        Nothing ->
                            Nothing

                        Just sameInThen ->
                            sameInAllBranches getSpecific elseBranch
                                |> Maybe.map (\sameInElse -> sameInThen ++ sameInElse)

                Expression.CaseExpression caseOf ->
                    traverseConcat
                        (\( _, caseExpression ) -> sameInAllBranches getSpecific caseExpression)
                        caseOf.cases

                _ ->
                    Nothing


{-| Same as `sameInAllBranches` without returning values from branches
-}
trueInAllBranches :
    (Node Expression -> Bool)
    -> Node Expression
    -> Bool
trueInAllBranches isSpecific baseExpressionNode =
    if isSpecific baseExpressionNode then
        True

    else
        case Node.value (AstHelpers.removeParens baseExpressionNode) of
            Expression.LetExpression letIn ->
                trueInAllBranches isSpecific letIn.expression

            Expression.IfBlock _ thenBranch elseBranch ->
                List.all
                    (\branchExpression -> trueInAllBranches isSpecific branchExpression)
                    [ thenBranch, elseBranch ]

            Expression.CaseExpression caseOf ->
                List.all
                    (\( _, caseExpression ) -> trueInAllBranches isSpecific caseExpression)
                    caseOf.cases

            _ ->
                False


expressionToComparable : Normalize.Resources a -> Node Expression -> Maybe ComparableExpression
expressionToComparable resources expressionNode =
    expressionNormalToComparable
        (Normalize.normalizeExpression resources expressionNode)


expressionNormalToComparable : Expression -> Maybe ComparableExpression
expressionNormalToComparable expressionNormal =
    normalizedExpressionToComparableWithSign 1 expressionNormal


type ComparableExpression
    = -- in theory also Time.Posix
      ComparableNumber Float
    | ComparableChar Char
    | ComparableString String
    | ComparableTuple ComparableExpression ComparableExpression
    | ComparableTriple ComparableExpression ComparableExpression ComparableExpression
    | ComparableList (List ComparableExpression)


normalizedExpressionToComparableWithSign : Int -> Expression -> Maybe ComparableExpression
normalizedExpressionToComparableWithSign sign expression =
    case expression of
        Expression.Integer int ->
            Just (ComparableNumber (Basics.toFloat (sign * int)))

        Expression.Hex hex ->
            Just (ComparableNumber (Basics.toFloat (sign * hex)))

        Expression.Floatable float ->
            Just (ComparableNumber (toFloat sign * float))

        Expression.Literal string ->
            Just (ComparableString string)

        Expression.CharLiteral char ->
            Just (ComparableChar char)

        Expression.TupledExpression [ Node _ first, Node _ second ] ->
            case normalizedExpressionToComparableWithSign 1 first of
                Nothing ->
                    Nothing

                Just firstComparable ->
                    normalizedExpressionToComparableWithSign 1 second
                        |> Maybe.map
                            (\secondComparable ->
                                ComparableTuple firstComparable secondComparable
                            )

        Expression.TupledExpression [ Node _ first, Node _ second, Node _ third ] ->
            case normalizedExpressionToComparableWithSign 1 first of
                Nothing ->
                    Nothing

                Just firstComparable ->
                    case normalizedExpressionToComparableWithSign 1 second of
                        Nothing ->
                            Nothing

                        Just secondComparable ->
                            normalizedExpressionToComparableWithSign 1 third
                                |> Maybe.map
                                    (\thirdComparable ->
                                        ComparableTriple firstComparable secondComparable thirdComparable
                                    )

        Expression.ListExpr elements ->
            Maybe.map ComparableList
                (traverse (\(Node _ element) -> normalizedExpressionToComparableWithSign 1 element) elements)

        Expression.ParenthesizedExpression (Node _ inParens) ->
            normalizedExpressionToComparableWithSign sign inParens

        Expression.Negation (Node _ inNegation) ->
            normalizedExpressionToComparableWithSign (-1 * sign) inNegation

        Expression.OperatorApplication _ _ _ _ ->
            -- any evaluation is covered by the Simplify checks themselves
            Nothing

        _ ->
            Nothing


listModuleName : List String
listModuleName =
    [ "List" ]
