module RangeDict exposing (RangeDict, empty, fromList, get, insert, member, modify, values)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)


type alias RangeDict v =
    Dict String v


empty : RangeDict v
empty =
    Dict.empty


fromList : List ( Range, v ) -> RangeDict v
fromList entries =
    List.foldl
        (\( range, v ) dict -> Dict.insert (rangeAsString range) v dict)
        Dict.empty
        entries


insert : Range -> v -> RangeDict v -> RangeDict v
insert range v dict =
    Dict.insert (rangeAsString range) v dict


modify : Range -> (v -> v) -> RangeDict v -> RangeDict v
modify range mapper dict =
    let
        key : String
        key =
            rangeAsString range
    in
    case Dict.get key dict of
        Just value ->
            Dict.insert key (mapper value) dict

        Nothing ->
            dict


values : RangeDict v -> List v
values =
    Dict.values


get : Range -> RangeDict v -> Maybe v
get range =
    Dict.get (rangeAsString range)


member : Range -> RangeDict v -> Bool
member range =
    Dict.member (rangeAsString range)


rangeAsString : Range -> String
rangeAsString range =
    [ range.start.row
    , range.start.column
    , range.end.row
    , range.end.column
    ]
        |> List.map String.fromInt
        |> String.join "_"
