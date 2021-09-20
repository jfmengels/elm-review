module Unicode exposing (slice, dropLeft, left)

{-| String functions that consider Unicode characters as a single characters, unlike `elm/core`'s `String` type.

@docs slice, dropLeft, left

-}


slice : Int -> Int -> String -> String
slice start end string =
    string
        |> String.toList
        |> List.drop start
        |> List.take (end - start)
        |> String.fromList


dropLeft : Int -> String -> String
dropLeft n string =
    string
        |> String.toList
        |> List.drop n
        |> String.fromList


left : Int -> String -> String
left n string =
    string
        |> String.toList
        |> List.take n
        |> String.fromList
