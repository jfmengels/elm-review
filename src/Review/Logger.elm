module Review.Logger exposing (Logger, fromFn, log, none)


type Logger
    = Logger (String -> String)


fromFn : (String -> String) -> Logger
fromFn =
    Logger


none : Logger
none =
    Logger identity


log : Logger -> String -> a -> a
log (Logger logFn) message data =
    always data <| logFn message
