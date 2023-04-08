module Review.Cache.ContextHash exposing (ComparableContextHash, ContextHash, create, sortContextHashes)


type ContextHash context
    = ContextHash context


create : context -> ContextHash context
create context =
    ContextHash (createContextHashMarker context)


type ComparableContextHash context
    = ComparableContextHash (List (ContextHash context))


sortContextHashes : List (ContextHash projectContext) -> ComparableContextHash projectContext
sortContextHashes list =
    ComparableContextHash list


createContextHashMarker : a -> a
createContextHashMarker context =
    context
