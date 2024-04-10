module Review.Cache.ContentHash exposing (ContentHash, hash, nil)

import Vendor.Murmur3 as Murmur3


type ContentHash
    = ContentHash Int


nil : ContentHash
nil =
    ContentHash 0


hash : String -> ContentHash
hash source =
    ContentHash (Murmur3.hashString 0 source)
