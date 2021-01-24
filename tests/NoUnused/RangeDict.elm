module NoUnused.RangeDict exposing (RangeDict, empty, fromList, get, insert, member)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)


type alias RangeDict v =
    Dict String v


empty : RangeDict v
empty =
    Dict.empty


insert : Range -> v -> RangeDict v -> RangeDict v
insert range =
    Dict.insert (rangeAsString range)


fromList : List ( Range, v ) -> RangeDict v
fromList values =
    values
        |> List.map (Tuple.mapFirst rangeAsString)
        |> Dict.fromList


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
