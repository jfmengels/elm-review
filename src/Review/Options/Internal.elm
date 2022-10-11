module Review.Options.Internal exposing (ReviewOptionsData, ReviewOptionsInternal(..))

import Review.Logger exposing (Logger)


type ReviewOptionsInternal
    = ReviewOptionsInternal ReviewOptionsData


type alias ReviewOptionsData =
    { extract : Bool
    , logger : Logger
    , fixAll : Bool
    }
