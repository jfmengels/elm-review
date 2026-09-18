module Elm.TypeInference.SubstitutionMap exposing
    ( Flags
    , LetRank
    , SubstitutionMap
    , bindRoot
    , empty
    , forLookup
    , letRankOf
    , linkTo
    , setIdLetRank
    , stampIdAtLetRank
    , substitute
    , substituteMono
    , test_fromList
    , union
    , unionFindRankOf
    )

{-| A dict mapping type variables to the inferred types.

Once created, `substitute*` functions are the way to consume it.

-}

import Array exposing (Array)
import Bitwise
import Dict exposing (Dict)
import Elm.TypeInference.Type exposing (VarName)
import Elm.TypeInference.Type.Internal as TypeI
    exposing
        ( Id
        , MonoType(..)
        , Type(..)
        )
import Elm.TypeInference.TypeVar as TypeVar exposing (TypeVar, TypeVarStyle(..))
import Elm.TypeInference.VarSet as VarSet exposing (NamedKey)


type alias SubstitutionMap =
    { -- Each var's data, indexed by `Id`.
      slotsGen : Array (Maybe Slot)
    , slotsNamed : Dict NamedKey Slot
    , -- Union-find rank per root (missing = 0): approximate tree height,
      -- bumped on equal-union-find-rank merge to keep `find` chains short.
      -- Performance heuristic.
      unionFindRanksGen : Array Int
    , unionFindRanksNamed : Dict NamedKey Int
    , -- Let-rank of each generated ID at the moment it was created.
      -- Lowered to min(side1,side2) on unify.
      -- Important for "business logic": `State.generalize` quantifies vars above current let-rank.
      letRanks : Array LetRank
    }


empty : SubstitutionMap
empty =
    { slotsGen = Array.empty
    , slotsNamed = Dict.empty
    , unionFindRanksGen = Array.empty
    , unionFindRanksNamed = Dict.empty
    , letRanks = Array.empty
    }


{-| Prepare for TypeLookupTable: drop state `substitute*` doesn't need.
-}
forLookup : SubstitutionMap -> SubstitutionMap
forLookup store =
    { slotsGen = store.slotsGen
    , slotsNamed = store.slotsNamed
    , unionFindRanksGen = Array.empty
    , unionFindRanksNamed = Dict.empty
    , letRanks = Array.empty
    }


{-| `Array.set` no-ops when the index is out of bounds.
This function grows the array instead.

Kept inline instead of in Array.ExtraExtra: somehow it's ~4% slower there - weird!
This sits on the hottest path in the library.

-}
arraySetGrowing : a -> Int -> a -> Array a -> Array a
arraySetGrowing default index value array =
    let
        indexMinusLength : Int
        indexMinusLength =
            index - Array.length array
    in
    if indexMinusLength < 0 then
        Array.set index value array

    else
        Array.push value (Array.append array (Array.repeat indexMinusLength default))


getSlot : TypeVar -> SubstitutionMap -> Maybe Slot
getSlot ( style, super ) store =
    case style of
        Generated theId ->
            Array.get theId store.slotsGen
                |> Maybe.andThen identity

        Named name ->
            Dict.get (VarSet.namedKeyFrom name super) store.slotsNamed


insertSlot : TypeVar -> Slot -> SubstitutionMap -> SubstitutionMap
insertSlot ( style, super ) slot store =
    case style of
        Generated theId ->
            { slotsGen = arraySetGrowing Nothing theId (Just slot) store.slotsGen
            , slotsNamed = store.slotsNamed
            , unionFindRanksGen = store.unionFindRanksGen
            , unionFindRanksNamed = store.unionFindRanksNamed
            , letRanks = store.letRanks
            }

        Named name ->
            { slotsNamed = Dict.insert (VarSet.namedKeyFrom name super) slot store.slotsNamed
            , slotsGen = store.slotsGen
            , unionFindRanksGen = store.unionFindRanksGen
            , unionFindRanksNamed = store.unionFindRanksNamed
            , letRanks = store.letRanks
            }


removeSlot : TypeVar -> SubstitutionMap -> SubstitutionMap
removeSlot ( style, super ) store =
    case style of
        Generated theId ->
            { slotsGen = Array.set theId Nothing store.slotsGen
            , slotsNamed = store.slotsNamed
            , unionFindRanksGen = store.unionFindRanksGen
            , unionFindRanksNamed = store.unionFindRanksNamed
            , letRanks = store.letRanks
            }

        Named name ->
            { slotsNamed = Dict.remove (VarSet.namedKeyFrom name super) store.slotsNamed
            , slotsGen = store.slotsGen
            , unionFindRanksGen = store.unionFindRanksGen
            , unionFindRanksNamed = store.unionFindRanksNamed
            , letRanks = store.letRanks
            }


memberSlot : TypeVar -> SubstitutionMap -> Bool
memberSlot ( style, super ) store =
    case style of
        Generated theId ->
            case Array.get theId store.slotsGen of
                Just (Just _) ->
                    True

                _ ->
                    False

        Named name ->
            Dict.member (VarSet.namedKeyFrom name super) store.slotsNamed


getRank : TypeVar -> SubstitutionMap -> Int
getRank ( style, super ) store =
    case style of
        Generated theId ->
            Array.get theId store.unionFindRanksGen
                |> Maybe.withDefault 0

        Named name ->
            Dict.get (VarSet.namedKeyFrom name super) store.unionFindRanksNamed
                |> Maybe.withDefault 0


insertRank : TypeVar -> Int -> SubstitutionMap -> SubstitutionMap
insertRank ( style, super ) rank store =
    case style of
        Generated theId ->
            { unionFindRanksGen = arraySetGrowing 0 theId rank store.unionFindRanksGen
            , slotsGen = store.slotsGen
            , slotsNamed = store.slotsNamed
            , unionFindRanksNamed = store.unionFindRanksNamed
            , letRanks = store.letRanks
            }

        Named name ->
            { unionFindRanksNamed = Dict.insert (VarSet.namedKeyFrom name super) rank store.unionFindRanksNamed
            , slotsGen = store.slotsGen
            , slotsNamed = store.slotsNamed
            , unionFindRanksGen = store.unionFindRanksGen
            , letRanks = store.letRanks
            }


{-| How deeply nested inside `let`/binding groups a type variable was created.
The let-rank of newly created vars is remembered into SubstitutionMap.letRanks.
Generalization only touches vars above the current let-rank.

This is unrelated to the union-find rank (`unionFindRanks`), which only
approximates tree height to keep `find` chains short.

-}
type alias LetRank =
    Int


type Slot
    = Link TypeVar -- same type as the other variable (step in the right direction)
    | Bound MonoType -- resolves to mono-type but that one could still mention unresolved vars
    | Ground MonoType -- var-free, as specific as can be.



-- FLAGS


{-| `isGround` and `isChanged` as an Int.

This is on the hottest path in the whole library, so ... integers are worth it.

-}
type alias Flags =
    Int


noFlags : Flags
noFlags =
    0


groundFlag : Flags
groundFlag =
    1


changedFlag : Flags
changedFlag =
    2


groundAndChanged : Flags
groundAndChanged =
    3


isGround : Flags -> Bool
isGround flags =
    Bitwise.and flags groundFlag /= 0


isChanged : Flags -> Bool
isChanged flags =
    Bitwise.and flags changedFlag /= 0


{-| Node built from two children:

  - ground if both are
  - changed if either is

-}
both : Flags -> Flags -> Flags
both a b =
    Bitwise.or
        (Bitwise.and groundFlag (Bitwise.and a b))
        (Bitwise.and changedFlag (Bitwise.or a b))



-- UNION-FIND


{-| Follow `Link`s to the root + compress the path.

Ignores union-find ranks; they are only a merge heuristic, not needed for lookup.

-}
findRoot : SubstitutionMap -> TypeVar -> ( TypeVar, SubstitutionMap )
findRoot store var =
    let
        go : List TypeVar -> TypeVar -> ( TypeVar, SubstitutionMap )
        go path current =
            case getSlot current store of
                Just (Link next) ->
                    go (current :: path) next

                _ ->
                    if List.isEmpty path then
                        -- No chain walked; skip work
                        ( current, store )

                    else
                        ( current
                        , List.foldl
                            (\pathVar acc -> insertSlot pathVar (Link current) acc)
                            store
                            path
                        )
    in
    go [] var


{-| Bind a root variable to a non-variable type.

The caller must have run the occurs check first (`Unify.bind` does).

-}
bindRoot : TypeVar -> MonoType -> SubstitutionMap -> SubstitutionMap
bindRoot var type_ store =
    insertSlot var (Bound type_) store
        |> lowerLetRanksTo (letRankOf var store) type_


{-| Point one root at another.

Used when the two variables carry different typeclass constraints
and the more constrained one has to win.

-}
linkTo : { child : TypeVar, parent : TypeVar } -> SubstitutionMap -> SubstitutionMap
linkTo { child, parent } store =
    insertSlot child (Link parent) store
        |> setVarLetRank parent (min (letRankOf child store) (letRankOf parent store))


{-| Merge two distinct unbound roots, letting union-find rank pick the representative.
-}
union : TypeVar -> TypeVar -> SubstitutionMap -> SubstitutionMap
union a b store =
    let
        unionFindRankA : Int
        unionFindRankA =
            unionFindRankOf store a

        unionFindRankB : Int
        unionFindRankB =
            unionFindRankOf store b

        mergedLetRank : LetRank
        mergedLetRank =
            min (letRankOf a store) (letRankOf b store)
    in
    if unionFindRankA < unionFindRankB then
        insertSlot a (Link b) store
            |> setVarLetRank b mergedLetRank

    else if unionFindRankB < unionFindRankA then
        insertSlot b (Link a) store
            |> setVarLetRank a mergedLetRank

    else
        insertSlot b (Link a) store
            |> insertRank a (unionFindRankA + 1)
            |> setVarLetRank a mergedLetRank


{-| Union-find rank of a root (missing = 0). Tree-height heuristic only.
-}
unionFindRankOf : SubstitutionMap -> TypeVar -> Int
unionFindRankOf store var =
    getRank var store


{-| Record the let-rank of a freshly allocated generated id.
-}
stampIdAtLetRank : Id -> LetRank -> SubstitutionMap -> SubstitutionMap
stampIdAtLetRank id letRank store =
    { letRanks = arraySetGrowing 0 id letRank store.letRanks
    , slotsGen = store.slotsGen
    , slotsNamed = store.slotsNamed
    , unionFindRanksGen = store.unionFindRanksGen
    , unionFindRanksNamed = store.unionFindRanksNamed
    }


{-| Overwrite an id's let-rank. Used when a binding-group placeholder was
allocated before `State.withDeeperLetRank`.
-}
setIdLetRank : Id -> LetRank -> SubstitutionMap -> SubstitutionMap
setIdLetRank =
    stampIdAtLetRank


letRankOf : TypeVar -> SubstitutionMap -> LetRank
letRankOf ( var, _ ) store =
    case var of
        TypeVar.Generated id ->
            Array.get id store.letRanks
                |> Maybe.withDefault 0

        TypeVar.Named _ ->
            0


setVarLetRank : TypeVar -> LetRank -> SubstitutionMap -> SubstitutionMap
setVarLetRank ( var, _ ) letRank store =
    case var of
        TypeVar.Generated id ->
            { letRanks = arraySetGrowing 0 id letRank store.letRanks
            , slotsGen = store.slotsGen
            , slotsNamed = store.slotsNamed
            , unionFindRanksGen = store.unionFindRanksGen
            , unionFindRanksNamed = store.unionFindRanksNamed
            }

        TypeVar.Named _ ->
            store


{-| Lower every unbound generated var in `type_` whose let-rank is above
`targetLetRank`. The type is assumed already substituted, so remaining `TypeVar`s are
roots.
-}
lowerLetRanksTo : LetRank -> MonoType -> SubstitutionMap -> SubstitutionMap
lowerLetRanksTo targetLetRank type_ store =
    case type_ of
        TypeVar var ->
            if letRankOf var store > targetLetRank then
                setVarLetRank var targetLetRank store

            else
                store

        Function { from, to } ->
            store
                |> lowerLetRanksTo targetLetRank from
                |> lowerLetRanksTo targetLetRank to

        Int ->
            store

        Float ->
            store

        Char ->
            store

        String ->
            store

        Bool ->
            store

        List listItemType ->
            lowerLetRanksTo targetLetRank listItemType store

        Unit ->
            store

        Tuple2 t1 t2 ->
            store
                |> lowerLetRanksTo targetLetRank t1
                |> lowerLetRanksTo targetLetRank t2

        Tuple3 t1 t2 t3 ->
            store
                |> lowerLetRanksTo targetLetRank t1
                |> lowerLetRanksTo targetLetRank t2
                |> lowerLetRanksTo targetLetRank t3

        Record { fields } ->
            lowerLetRanksInFieldsTo targetLetRank fields store

        ExtensibleRecord r ->
            store
                |> lowerLetRanksTo targetLetRank r.extensionTypevar
                |> lowerLetRanksInFieldsTo targetLetRank r.fields

        UserDefinedType r ->
            List.foldl (\arg storeAcc -> lowerLetRanksTo targetLetRank arg storeAcc) store r.args

        WebGLShader r ->
            store
                |> lowerLetRanksInFieldsTo targetLetRank r.attributes
                |> lowerLetRanksTo targetLetRank r.attributesExtension
                |> lowerLetRanksInFieldsTo targetLetRank r.uniforms
                |> lowerLetRanksTo targetLetRank r.uniformsExtension
                |> lowerLetRanksInFieldsTo targetLetRank r.varyings
                |> lowerLetRanksTo targetLetRank r.varyingsExtension


lowerLetRanksInFieldsTo : LetRank -> Dict VarName MonoType -> SubstitutionMap -> SubstitutionMap
lowerLetRanksInFieldsTo targetLetRank fields store =
    Dict.foldl (\_ fieldType acc -> lowerLetRanksTo targetLetRank fieldType acc) store fields



-- SUBSTITUTION


{-| Substitute a whole (possibly quantified) `Type`.
-}
substitute : SubstitutionMap -> Type -> ( Type, SubstitutionMap )
substitute store (Forall boundVars monoType) =
    case boundVars of
        [] ->
            let
                ( monoType_, _, store1 ) =
                    substituteMono store monoType
            in
            ( Forall [] monoType_, store1 )

        _ ->
            let
                ( didIntersect, restricted ) =
                    List.foldl
                        (\var ( found, acc ) ->
                            if memberSlot var acc then
                                ( True
                                , removeSlot var acc
                                )

                            else
                                ( found, acc )
                        )
                        ( False, store )
                        boundVars
            in
            if didIntersect then
                let
                    ( monoType_, _, _ ) =
                        substituteMono restricted monoType
                in
                ( Forall boundVars monoType_, store )

            else
                let
                    ( monoType_, _, store1 ) =
                        substituteMono store monoType
                in
                ( Forall boundVars monoType_, store1 )


{-| Substitute all typevars in the given monotype for their inferred types.
During this traversal we remember newly discovered ground resolutions,
and chains get path-compressed.
-}
substituteMono : SubstitutionMap -> MonoType -> ( MonoType, Flags, SubstitutionMap )
substituteMono store monoType =
    case monoType of
        -- The main interesting part
        TypeVar var ->
            case getSlot var store of
                Nothing ->
                    -- Unbound root.
                    ( monoType
                    , noFlags
                    , store
                    )

                Just (Ground groundType) ->
                    ( groundType
                    , groundAndChanged
                    , store
                    )

                Just (Bound bound) ->
                    resolveBound store var bound

                Just (Link _) ->
                    let
                        ( root, store1 ) =
                            findRoot store var
                    in
                    case getSlot root store1 of
                        Just (Bound bound) ->
                            resolveBound store1 var bound

                        Just (Ground groundType) ->
                            ( groundType
                            , groundAndChanged
                            , insertSlot var (Ground groundType) store1
                            )

                        _ ->
                            -- Unbound root: the best we can say is which var this one has merged into.
                            ( TypeVar root
                            , changedFlag
                            , store1
                            )

        -- The rest are just recursion
        Function { from, to } ->
            let
                ( from_, f1, s1 ) =
                    substituteMono store from

                ( to_, f2, s2 ) =
                    substituteMono s1 to

                flags : Flags
                flags =
                    both f1 f2
            in
            if isChanged flags then
                ( Function { from = from_, to = to_ }, flags, s2 )

            else
                ( monoType, flags, s2 )

        Int ->
            ( monoType, groundFlag, store )

        Float ->
            ( monoType, groundFlag, store )

        Char ->
            ( monoType, groundFlag, store )

        String ->
            ( monoType, groundFlag, store )

        Bool ->
            ( monoType, groundFlag, store )

        List listItemType ->
            let
                ( listItemType_, flags, s1 ) =
                    substituteMono store listItemType
            in
            if isChanged flags then
                ( List listItemType_, flags, s1 )

            else
                ( monoType, flags, s1 )

        Unit ->
            ( monoType, groundFlag, store )

        Tuple2 t1 t2 ->
            let
                ( t1_, f1, s1 ) =
                    substituteMono store t1

                ( t2_, f2, s2 ) =
                    substituteMono s1 t2

                flags : Flags
                flags =
                    both f1 f2
            in
            if isChanged flags then
                ( Tuple2 t1_ t2_, flags, s2 )

            else
                ( monoType, flags, s2 )

        Tuple3 t1 t2 t3 ->
            let
                ( t1_, f1, s1 ) =
                    substituteMono store t1

                ( t2_, f2, s2 ) =
                    substituteMono s1 t2

                ( t3_, f3, s3 ) =
                    substituteMono s2 t3

                flags : Flags
                flags =
                    both f1 (both f2 f3)
            in
            if isChanged flags then
                ( Tuple3 t1_ t2_ t3_, flags, s3 )

            else
                ( monoType, flags, s3 )

        Record { fields } ->
            let
                ( fields_, flags, s1 ) =
                    substituteRecordFields store fields
            in
            if isChanged flags then
                ( Record { fields = fields_ }, flags, s1 )

            else
                ( monoType, flags, s1 )

        ExtensibleRecord r ->
            let
                ( extensionTypevar_, f1, s1 ) =
                    substituteMono store r.extensionTypevar

                ( fields_, f2, s2 ) =
                    substituteRecordFields s1 r.fields

                flags : Flags
                flags =
                    both f1 f2

                needsCollapse : Bool
                needsCollapse =
                    Dict.isEmpty fields_
                        || (case extensionTypevar_ of
                                Record _ ->
                                    True

                                ExtensibleRecord _ ->
                                    True

                                _ ->
                                    False
                           )
            in
            if needsCollapse then
                ( TypeI.collapseExtensible
                    { extensionTypevar = extensionTypevar_
                    , fields = fields_
                    }
                , Bitwise.or flags changedFlag
                , s2
                )

            else if isChanged flags then
                ( ExtensibleRecord
                    { extensionTypevar = extensionTypevar_
                    , fields = fields_
                    }
                , flags
                , s2
                )

            else
                ( monoType, flags, s2 )

        UserDefinedType r ->
            let
                ( args_, flags, s1 ) =
                    substituteTypeArgs store r.args
            in
            if isChanged flags then
                ( UserDefinedType
                    { package = r.package
                    , moduleId = r.moduleId
                    , name = r.name
                    , args = args_
                    }
                , flags
                , s1
                )

            else
                ( monoType, flags, s1 )

        WebGLShader r ->
            let
                ( attributesExtension_, f1, s1 ) =
                    substituteMono store r.attributesExtension

                ( attributes_, f2, s2 ) =
                    substituteRecordFields s1 r.attributes

                ( uniformsExtension_, f3, s3 ) =
                    substituteMono s2 r.uniformsExtension

                ( uniforms_, f4, s4 ) =
                    substituteRecordFields s3 r.uniforms

                ( varyingsExtension_, f5, s5 ) =
                    substituteMono s4 r.varyingsExtension

                ( varyings_, f6, s6 ) =
                    substituteRecordFields s5 r.varyings

                flags : Flags
                flags =
                    both f1 (both f2 (both f3 (both f4 (both f5 f6))))
            in
            if isChanged flags then
                ( WebGLShader
                    { attributesExtension = attributesExtension_
                    , attributes = attributes_
                    , uniformsExtension = uniformsExtension_
                    , uniforms = uniforms_
                    , varyingsExtension = varyingsExtension_
                    , varyings = varyings_
                    }
                , flags
                , s6
                )

            else
                ( monoType, flags, s6 )


{-| Resolve as much as you can from a Bound type.
If we manage to get to a Ground type, cache the answer.
If not, at least compress the path you walked (it will still have to be walked again later).
-}
resolveBound : SubstitutionMap -> TypeVar -> MonoType -> ( MonoType, Flags, SubstitutionMap )
resolveBound store var bound =
    let
        ( resolved, flags, store1 ) =
            substituteMono store bound
    in
    if isChanged flags then
        let
            slot : Slot
            slot =
                if isGround flags then
                    Ground resolved

                else
                    Bound resolved
        in
        ( resolved
        , Bitwise.or flags changedFlag
        , insertSlot var slot store1
        )

    else
        ( resolved
        , Bitwise.or flags changedFlag
        , store1
        )


substituteRecordFields : SubstitutionMap -> Dict VarName MonoType -> ( Dict VarName MonoType, Flags, SubstitutionMap )
substituteRecordFields store fields =
    let
        ( reversed, flags, store1 ) =
            Dict.foldl
                (\name type_ ( accList, accFlags, accSubst ) ->
                    let
                        ( type__, fieldFlags, accSubst1 ) =
                            substituteMono accSubst type_
                    in
                    ( ( name, type__ ) :: accList, both accFlags fieldFlags, accSubst1 )
                )
                ( [], groundFlag, store )
                fields
    in
    if isChanged flags then
        ( Dict.fromList reversed, flags, store1 )

    else
        -- Nothing moved: hand back the very same `Dict` instead of rebuilding
        -- an identical one.
        ( fields, flags, store1 )


substituteTypeArgs : SubstitutionMap -> List MonoType -> ( List MonoType, Flags, SubstitutionMap )
substituteTypeArgs store args =
    let
        ( args_, flags, store1 ) =
            List.foldr
                (\type_ ( accArgs, accFlags, accSubst ) ->
                    let
                        ( type__, argFlags, accSubst1 ) =
                            substituteMono accSubst type_
                    in
                    ( type__ :: accArgs, both accFlags argFlags, accSubst1 )
                )
                ( [], groundFlag, store )
                args
    in
    if isChanged flags then
        ( args_, flags, store1 )

    else
        ( args, flags, store1 )



-- TEST HELPERS


{-| Test-only helper. Can produce cycles (doesn't validate).

Leaves both union-find ranks and let-ranks empty (all zero).

-}
test_fromList : List ( TypeVar, MonoType ) -> SubstitutionMap
test_fromList list =
    List.foldl
        (\( var, type_ ) acc -> insertSlot var (test_slotFor type_) acc)
        empty
        list


test_slotFor : MonoType -> Slot
test_slotFor type_ =
    case type_ of
        TypeVar other ->
            Link other

        _ ->
            Bound type_
