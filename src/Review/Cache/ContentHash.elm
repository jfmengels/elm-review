module Review.Cache.ContentHash exposing (ContentHash, combine, hash, nil)

import Dict exposing (Dict)
import Vendor.Murmur3 as Murmur3


type ContentHash
    = ContentHash Int


nil : ContentHash
nil =
    ContentHash 0


hash : String -> ContentHash
hash source =
    ContentHash (Murmur3.hashString 0 source)


combine : Dict String ContentHash -> ContentHash
combine dict =
    ContentHash (Dict.foldl (\_ (ContentHash n) acc -> n + acc) 0 dict)
