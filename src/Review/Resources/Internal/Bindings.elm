module Review.Resources.Internal.Bindings exposing
    ( Binding(..)
    , ImportBinding(..)
    , Realm
    , typeRealm
    , valueRealm
    )

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Review.Resources.BindingId as BindingId


type ImportBinding
    = ImportBinding
        { id : BindingId.Id
        , name : String
        , moduleName : ModuleName
        }


type Binding
    = Binding
        { id : BindingId.Id
        , name : String
        , range : Range
        , realm : Realm
        }


type alias Realm =
    -- valueRealm for values
    -- | typeRealm for types
    -- can be both
    Int


valueRealm : Realm
valueRealm =
    1


typeRealm : Realm
typeRealm =
    2
