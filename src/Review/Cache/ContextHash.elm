module Review.Cache.ContextHash exposing (ComparableContextHash, ContextHash, create, toComparable)


type ContextHash context
    = ContextHash context


create : context -> ContextHash context
create context =
    ContextHash (createContextHashMarker context)


type ComparableContextHash context
    = ComparableContextHash (List (ContextHash context))


{-| Will be replaced by a function that will sort the context hashes (which will be hashed values) in optimized mode.
-}
toComparable : List (ContextHash projectContext) -> ComparableContextHash projectContext
toComparable list =
    ComparableContextHash list


createContextHashMarker : a -> a
createContextHashMarker context =
    context
