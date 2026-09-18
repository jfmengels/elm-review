module TypeLookupTable.Internal exposing (TypeLookupTable(..), empty)

import Array exposing (Array)
import Dict exposing (Dict)
import Elm.TypeInference.ModuleIds as ModuleIds
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type exposing (Type)
import Elm.TypeInference.Type.Internal exposing (Id, MonoType)
import RangeLike exposing (RangeLike)


type TypeLookupTable
    = TLT
        { nodeIds : Dict RangeLike Id
        , subst : SubstitutionMap.SubstitutionMap
        , moduleMapping : ModuleIds.Mapping
        , cache : Array (Maybe Type)
        , pool : Dict String Type
        , annotationFor : Dict Id MonoType
        }


empty : TypeLookupTable
empty =
    TLT
        { nodeIds = Dict.empty
        , subst = SubstitutionMap.empty
        , moduleMapping = ModuleIds.empty
        , cache = Array.empty
        , pool = Dict.empty
        , annotationFor = Dict.empty
        }
