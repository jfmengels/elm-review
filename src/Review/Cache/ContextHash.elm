module Review.Cache.ContextHash exposing (ContextHash, areEqual, create)


type ContextHash context
    = ContextHash context


create : context -> ContextHash context
create context =
    ContextHash (createContextHashMarker context)


createContextHashMarker : a -> a
createContextHashMarker context =
    context


areEqual : ContextHash context -> ContextHash context -> Bool
areEqual (ContextHash a) (ContextHash b) =
    a == b
