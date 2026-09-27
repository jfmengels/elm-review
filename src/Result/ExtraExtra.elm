module Result.ExtraExtra exposing (combineMap, firstJustLazy, foldlWhileOk, merge)

{-| -}


{-| Try thunks until we find something other than Ok Nothing.
-}
firstJustLazy : List (() -> Result e (Maybe a)) -> Result e (Maybe a)
firstJustLazy lookups =
    case lookups of
        [] ->
            Ok Nothing

        lookup :: rest ->
            case lookup () of
                Ok Nothing ->
                    firstJustLazy rest

                found ->
                    found


{-| Folds over a list but stops at the first Err if one is encountered.

The two following function calls are equivalent, but `foldlWhileOk` will be more performant.

    Result.Extra.foldlWhileOk (\x res -> f x res) initial list

    List.foldl (\x res -> Result.andThen (f x) res) (Ok initial) list

-}
foldlWhileOk : (a -> value -> Result error value) -> value -> List a -> Result error value
foldlWhileOk f value list =
    case list of
        [] ->
            Ok value

        a :: rest ->
            case f a value of
                (Err _) as err_ ->
                    err_

                Ok newValue ->
                    foldlWhileOk f newValue rest


{-| Map a function producing results on a list
and combine those into a single result (holding a list).
Also known as `traverse` on lists.

    combineMap f xs == combine (List.map f xs)

-}
combineMap : (a -> Result x b) -> List a -> Result x (List b)
combineMap f ls =
    combineMapHelp f ls []


combineMapHelp : (a -> Result x b) -> List a -> List b -> Result x (List b)
combineMapHelp f list acc =
    case list of
        head :: tail ->
            case f head of
                Ok a ->
                    combineMapHelp f tail (a :: acc)

                Err x ->
                    Err x

        [] ->
            Ok (List.reverse acc)


{-| Eliminate Result when error and success have been mapped to the same
type, such as a message type.

    merge (Ok 4) == 4

    merge (Err -1) == -1

More pragmatically:

    type Msg
        = UserTypedInt Int
        | UserInputError String

    msgFromInput : String -> Msg
    msgFromInput =
        String.toInt
            >> Result.mapError UserInputError
            >> Result.map UserTypedInt
            >> Result.Extra.merge

-}
merge : Result a a -> a
merge r =
    case r of
        Ok rr ->
            rr

        Err rr ->
            rr
