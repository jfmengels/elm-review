module Simplify.RangeDict exposing (RangeDict, any, empty, get, insert, member, remove, singleton, union)

import Bitwise
import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)


type RangeDict v
    = RangeDict (Dict ( Int, Int ) v)


empty : RangeDict v
empty =
    RangeDict Dict.empty


singleton : Range -> v -> RangeDict v
singleton range value =
    RangeDict (Dict.singleton (rangeAsComparable range) value)


insert : Range -> v -> RangeDict v -> RangeDict v
insert range value (RangeDict rangeDict) =
    RangeDict (Dict.insert (rangeAsComparable range) value rangeDict)


remove : Range -> RangeDict v -> RangeDict v
remove range (RangeDict rangeDict) =
    RangeDict (Dict.remove (rangeAsComparable range) rangeDict)


get : Range -> RangeDict v -> Maybe v
get range (RangeDict rangeDict) =
    Dict.get (rangeAsComparable range) rangeDict


member : Range -> RangeDict v -> Bool
member range (RangeDict rangeDict) =
    Dict.member (rangeAsComparable range) rangeDict


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


rangeAsComparable : Range -> ( Int, Int )
rangeAsComparable range =
    ( Bitwise.shiftLeftBy 16 range.start.row + range.start.column
    , Bitwise.shiftLeftBy 16 range.end.row + range.end.column
    )
