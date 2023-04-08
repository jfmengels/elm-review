module Review.Cache.ContextHash exposing (ContextHash, create)


type ContextHash context
    = ContextHash context


create : context -> ContextHash context
create context =
    ContextHash (createContextHashMarker context)


createContextHashMarker : a -> a
createContextHashMarker context =
    context
