module Review.Resources.BindingId exposing
    ( Counter
    , Id(..)
    , counter
    , new
    )


type Id
    = Id Int


type Counter
    = Counter Int


counter : Counter
counter =
    Counter 0


new : (Id -> a) -> Counter -> ( a, Counter )
new fn (Counter n) =
    ( fn (Id n), Counter (n + 1) )
