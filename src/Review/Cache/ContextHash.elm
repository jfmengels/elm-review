module Review.Cache.ContextHash exposing
    ( ContextHash, create
    , ComparableContextHash, toComparable
    )

{-| Computes a hash of the `ProjectContext` defined for each rule.
Relies on patching the JS output to be fast.

@docs ContextHash, create

@docs ComparableContextHash, toComparable

-}


type ContextHash context
    = ContextHash context


create : context -> ContextHash context
create context =
    ContextHash (createContextHashMarker context)


type ComparableContextHash context
    = ComparableContextHash (List (ContextHash context))


toComparable : List (ContextHash projectContext) -> ComparableContextHash projectContext
toComparable list =
    ComparableContextHash (sort list)


{-| Will be replaced by a function that will sort the context hashes (which will be hashed values) in optimized mode,
as contexts are not comparable without patching.
-}
sort : a -> a
sort =
    identity


{-| Will be replaced by a function that will hash the context and turn it into a `comparable` that will very cheap to compare.
-}
createContextHashMarker : a -> a
createContextHashMarker context =
    context
