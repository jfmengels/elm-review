module Review.Project.FileHash exposing (FileHash, hash)

import Murmur3


type FileHash
    = FileHash Int


hash : String -> FileHash
hash source =
    FileHash (Murmur3.hashString 0 source)
