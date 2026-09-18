module TypeLookupTable exposing (TypeLookupTable, get, empty)

{-|

@docs TypeLookupTable, get, empty

-}

import Array exposing (Array)
import Dict
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type exposing (Type)
import Elm.TypeInference.Type.Internal as TypeI
import RangeLike
import TypeLookupTable.Internal as Internal


{-| A lazy lookup table: after the initial collection of type equations is done
in `Elm.TypeInference.inferModule`, we compute the type on demand when the user
calls `TypeLookupTable.get`.
-}
type alias TypeLookupTable =
    Internal.TypeLookupTable


{-| An empty lookup table. Not for direct use.

Useful for when a value of this type is needed to satisfy the compiler but will never be used.

-}
empty : TypeLookupTable
empty =
    Internal.empty


{-| Look up the inferred `Type` for a source `Range`.

Only exact ranges given by `Node.range` of AST nodes inside the input
`Elm.Syntax.File`s are recorded. There is no overlap matching.

-}
get : Range -> TypeLookupTable -> ( Maybe Type, TypeLookupTable )
get range (Internal.TLT tlt) =
    let
        rangeLike : RangeLike.RangeLike
        rangeLike =
            RangeLike.fromRange range
    in
    case Dict.get rangeLike tlt.nodeIds of
        Nothing ->
            ( Nothing, Internal.TLT tlt )

        Just id ->
            case Array.get id tlt.cache |> Maybe.andThen identity of
                Just cached ->
                    ( Just cached, Internal.TLT tlt )

                Nothing ->
                    let
                        ( monoType0, _, subst1 ) =
                            SubstitutionMap.substituteMono tlt.subst (TypeI.id_ id)

                        monoType : TypeI.MonoType
                        monoType =
                            case Dict.get id tlt.annotationFor of
                                Nothing ->
                                    monoType0

                                Just annoMono ->
                                    monoType0
                                        |> TypeI.renameToAnnotation annoMono
                                        |> Maybe.withDefault monoType0

                        key : String
                        key =
                            TypeI.monoPublicKey { alreadyNormalized = False } monoType

                        ( pubType, pool1 ) =
                            case Dict.get key tlt.pool of
                                Just canonical ->
                                    ( canonical, tlt.pool )

                                Nothing ->
                                    let
                                        fresh : Type
                                        fresh =
                                            TypeI.toPublicType tlt.moduleMapping { alreadyNormalized = False } monoType
                                    in
                                    ( fresh, Dict.insert key fresh tlt.pool )
                    in
                    ( Just pubType
                    , Internal.TLT
                        { nodeIds = tlt.nodeIds
                        , subst = subst1
                        , moduleMapping = tlt.moduleMapping
                        , cache = arraySetGrowing Nothing id (Just pubType) tlt.cache
                        , pool = pool1
                        , annotationFor = tlt.annotationFor
                        }
                    )


{-| `Array.set` no-ops when the index is out of bounds.
This function grows the array instead.

Kept inline instead of in Array.ExtraExtra: somehow it's ~4% slower there - weird!
This sits on the hottest path in the library.

-}
arraySetGrowing : a -> Int -> a -> Array a -> Array a
arraySetGrowing default index value array =
    let
        len : Int
        len =
            Array.length array
    in
    if index < len then
        Array.set index value array

    else
        Array.push value (Array.append array (Array.repeat (index - len) default))
