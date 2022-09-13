module Review.Options.Internal exposing (ReviewOptionsInternal(..))

import Review.Logger exposing (Logger)


type ReviewOptionsInternal
    = ReviewOptionsInternal
        { extract : Bool
        , logger : Logger
        }
