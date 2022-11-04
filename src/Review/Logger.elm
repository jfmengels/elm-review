module Review.Logger exposing (Logger, fromFn, log, none)

import Json.Encode as Encode


type Logger
    = Logger (List ( String, Encode.Value ) -> List ( String, Encode.Value ))


fromFn : (List ( String, Encode.Value ) -> List ( String, Encode.Value )) -> Logger
fromFn =
    Logger


none : Logger
none =
    Logger identity


log : Logger -> List ( String, Encode.Value ) -> a -> a
log (Logger logFn) message data =
    always data <| logFn message
