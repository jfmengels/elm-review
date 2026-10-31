module Review.Resources.ScopeId exposing
    ( Counter
    , counter
    , new
    , topLevel
    )

import Review.Resources.ScopeInternal exposing (ScopeId)


topLevel : ScopeId
topLevel =
    0


type Counter
    = Counter Int


counter : Counter
counter =
    Counter 1


new : Counter -> ( ScopeId, Counter )
new (Counter n) =
    ( n, Counter (n + 1) )
