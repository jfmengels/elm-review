module Review.Cache.ContextHash exposing (ComparableContextHash, ContextHash, create, toComparable)


type ContextHash context
    = ContextHash context


create : context -> ContextHash context
create context =
    ContextHash (createContextHashMarker context)


type ComparableContextHash context
    = ComparableContextHash (List (ContextHash context))


toComparable : List (ContextHash projectContext) -> ComparableContextHash projectContext
toComparable list =
    ComparableContextHash list


createContextHashMarker : a -> a
createContextHashMarker context =
    context
