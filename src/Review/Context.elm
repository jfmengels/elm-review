module Review.Context exposing (Context, Metadata, RequestedData, availableData, init, withMetadata)

--init : Rule.Context Context
--init =
--    Context.succeed (\metadata scope -> {})
--        |> Context.withMetadata
--        |> Context.withScope


type Context context
    = Context (AvailableData -> context) RequestedData


init : context -> Context context
init context =
    Context
        (always context)
        (RequestedData { metadata = False })


type RequestedData
    = RequestedData
        { metadata : Bool
        }


type Metadata
    = Metadata


withMetadata : Context (Metadata -> a) -> Context a
withMetadata (Context fn (RequestedData requested)) =
    Context
        (\data -> fn data data.metadata)
        (RequestedData { requested | metadata = True })


type alias AvailableData =
    { metadata : Metadata
    }


availableData : AvailableData
availableData =
    { metadata = Metadata
    }


apply : AvailableData -> Context context -> context
apply data (Context fn _) =
    fn data


requestedData : AvailableData -> Context context -> context
requestedData data (Context fn _) =
    fn data


test1 : Context Int
test1 =
    init 1


test2 : Context Int
test2 =
    init (\Metadata -> 1)
        |> withMetadata
