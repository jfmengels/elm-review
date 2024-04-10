module Review.Cache.ContentHash exposing (ContentHash, hash)

import Vendor.Murmur3 as Murmur3


type ContentHash
    = ContentHash Int


hash : String -> ContentHash
hash source =
    ContentHash (Murmur3.hashString 0 source)
