module TypeLookupTable exposing
    ( TypeLookupTable, get
    , empty
    )

{-|

@docs TypeLookupTable, get
@docs empty

-}

import Dict
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.Type exposing (Type)
import RangeLike
import TypeLookupTable.Internal as Internal


{-| Types for Ranges belonging to declarations or expressions from a parsed Elm module.
-}
type alias TypeLookupTable =
    Internal.TypeLookupTable


empty : Internal.TypeLookupTable
empty =
    Internal.TLT Dict.empty


{-| Look up the inferred type for a source range.

Only exact ranges given by `Node.range` of AST nodes inside the input
`Elm.Syntax.File`s are recorded and there is no fuzzy/overlap matching.

-}
get : Range -> TypeLookupTable -> Maybe Type
get range (Internal.TLT tlt) =
    Dict.get (RangeLike.fromRange range) tlt
