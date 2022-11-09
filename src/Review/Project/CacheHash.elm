module Review.Project.CacheHash exposing (CacheHash, areEqual, areEqualForMaybe, hash)

import Vendor.Murmur3 as Murmur3


type CacheHash
    = CacheHash Int


hash : String -> CacheHash
hash source =
    CacheHash (Murmur3.hashString 0 source)


areEqual : CacheHash -> CacheHash -> Bool
areEqual (CacheHash a) (CacheHash b) =
    a == b


areEqualForMaybe : Maybe CacheHash -> Maybe CacheHash -> Bool
areEqualForMaybe a b =
    case ( a, b ) of
        ( Just a_, Just b_ ) ->
            a_ == b_

        ( Nothing, Nothing ) ->
            True

        _ ->
            False
