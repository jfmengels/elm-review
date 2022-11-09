module Review.Cache.ContentHash exposing (ContentHash, areEqual, areEqualForMaybe, hash)

import Vendor.Murmur3 as Murmur3


type ContentHash
    = ContentHash Int


hash : String -> ContentHash
hash source =
    ContentHash (Murmur3.hashString 0 source)


areEqual : ContentHash -> ContentHash -> Bool
areEqual (ContentHash a) (ContentHash b) =
    a == b


areEqualForMaybe : Maybe ContentHash -> Maybe ContentHash -> Bool
areEqualForMaybe a b =
    case ( a, b ) of
        ( Just a_, Just b_ ) ->
            a_ == b_

        ( Nothing, Nothing ) ->
            True

        _ ->
            False
