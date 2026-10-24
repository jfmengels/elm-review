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


new : Counter -> ( Id, Counter )
new (Counter n) =
    ( Id n, Counter (n + 1) )
