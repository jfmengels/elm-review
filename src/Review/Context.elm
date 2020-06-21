module Review.Context exposing (AvailableData, Context, RequestedData, apply, init, withMetadata, withModuleKey)

import Elm.Syntax.Node as Node
import Elm.Syntax.Range as Range
import Review.Metadata as Metadata exposing (Metadata)
import Review.Rule exposing (ModuleKey)


type Context projectContext moduleContext
    = Context (AvailableData -> projectContext -> moduleContext) RequestedData


init : (projectContext -> moduleContext) -> Context projectContext moduleContext
init fromProjectToModule =
    Context
        (always fromProjectToModule)
        (RequestedData { metadata = False })


type RequestedData
    = RequestedData
        { metadata : Bool
        }


withMetadata : Context Metadata (projectContext -> moduleContext) -> Context projectContext moduleContext
withMetadata (Context fn (RequestedData requested)) =
    Context
        (\data -> fn data data.metadata)
        (RequestedData { requested | metadata = True })


withModuleKey : Context ModuleKey (projectContext -> moduleContext) -> Context projectContext moduleContext
withModuleKey (Context fn (RequestedData requested)) =
    Context
        (\data -> fn data data.moduleKey)
        (RequestedData { requested | metadata = True })


type alias AvailableData =
    { metadata : Metadata
    , moduleKey : ModuleKey
    }


apply : AvailableData -> projectContext -> Context projectContext moduleContext -> moduleContext
apply data projectContext (Context fn _) =
    fn data projectContext


requestedData : Context projectContext moduleContext -> RequestedData
requestedData (Context _ requested) =
    requested



-- TESTING
--test1 : Context () Int
--test1 =
--    init (\() -> 1)
--
--
--test1Applied : Int
--test1Applied =
--    test1
--        |> apply availableData ()
--        |> Debug.log "test 1"
--
--
--test2 : Context () Int
--test2 =
--    init (\Metadata () -> 1)
--        |> withMetadata
--
--
--test2Applied : Int
--test2Applied =
--    test2
--        |> apply availableData ()
--        |> Debug.log "test 2"
--
--
--test3 : Context () Int
--test3 =
--    init (\Metadata int () -> 1 + int)
--        |> withMetadata
--        |> withInt
--
--
--test3Applied : Int
--test3Applied =
--    test3
--        |> apply availableData ()
--        |> Debug.log "test 3"
