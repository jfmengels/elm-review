module Review.Logger exposing (Logger, fromFn, log, none)


type Logger
    = Logger (String -> String)
    | NoLogger


fromFn : (String -> String) -> Logger
fromFn =
    Logger


none : Logger
none =
    Logger identity


log : Logger -> String -> a -> a
log logger message data =
    case logger of
        Logger logFn ->
            always data <| logFn message

        NoLogger ->
            data
