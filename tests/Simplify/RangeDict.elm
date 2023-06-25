module Simplify.RangeDict exposing (RangeDict, any, empty, get, insert, mapFromList, member, remove, singleton, union)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)


type RangeDict v
    = RangeDict (Dict String v)


empty : RangeDict v
empty =
    RangeDict Dict.empty


singleton : Range -> v -> RangeDict v
singleton range value =
    RangeDict (Dict.singleton (rangeAsString range) value)


{-| Indirect conversion from a list to key-value pairs to avoid successive List.map calls.
-}
mapFromList : (a -> ( Range, v )) -> List a -> RangeDict v
mapFromList toAssociation list =
    List.foldl
        (\element acc ->
            let
                ( range, v ) =
                    toAssociation element
            in
            Dict.insert (rangeAsString range) v acc
        )
        Dict.empty
        list
        |> RangeDict


insert : Range -> v -> RangeDict v -> RangeDict v
insert range value (RangeDict rangeDict) =
    RangeDict (Dict.insert (rangeAsString range) value rangeDict)


remove : Range -> RangeDict v -> RangeDict v
remove range (RangeDict rangeDict) =
    RangeDict (Dict.remove (rangeAsString range) rangeDict)


get : Range -> RangeDict v -> Maybe v
get range (RangeDict rangeDict) =
    Dict.get (rangeAsString range) rangeDict


member : Range -> RangeDict v -> Bool
member range (RangeDict rangeDict) =
    Dict.member (rangeAsString range) rangeDict


foldl : (v -> folded -> folded) -> folded -> RangeDict v -> folded
foldl reduce initialFolded (RangeDict rangeDict) =
    Dict.foldl (\_ -> reduce) initialFolded rangeDict


any : (v -> Bool) -> RangeDict v -> Bool
any isFound rangeDict =
    foldl (\value soFar -> soFar || isFound value)
        False
        rangeDict


union : RangeDict v -> RangeDict v -> RangeDict v
union (RangeDict aRangeDict) (RangeDict bRangeDict) =
    RangeDict (Dict.union aRangeDict bRangeDict)


rangeAsString : Range -> String
rangeAsString range =
    [ range.start.row
    , range.start.column
    , range.end.row
    , range.end.column
    ]
        |> List.map String.fromInt
        |> String.join "_"
