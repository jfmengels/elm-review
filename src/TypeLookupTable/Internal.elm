module TypeLookupTable.Internal exposing (TypeLookupTable(..))

import Dict exposing (Dict)
import Elm.TypeInference.Type exposing (Type)
import RangeLike exposing (RangeLike)


type TypeLookupTable
    = TLT (Dict RangeLike Type)
