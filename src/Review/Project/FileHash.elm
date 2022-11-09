module Review.Project.FileHash exposing (FileHash, areEqual, areEqualForMaybe, hash)

import Murmur3


type FileHash
    = FileHash Int


hash : String -> FileHash
hash source =
    FileHash (Murmur3.hashString 0 source)


areEqual : FileHash -> FileHash -> Bool
areEqual (FileHash a) (FileHash b) =
    a == b


areEqualForMaybe : Maybe FileHash -> Maybe FileHash -> Bool
areEqualForMaybe a b =
    case ( a, b ) of
        ( Just a_, Just b_ ) ->
            a_ == b_

        ( Nothing, Nothing ) ->
            True

        _ ->
            False
