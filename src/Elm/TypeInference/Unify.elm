module Elm.TypeInference.Unify exposing
    ( TypeAlias
    , UnifyConfig
    , expandAliasDeep
    , unifyMany
    )

import Dict exposing (Dict)
import Dict.Extra
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.TypeInference.InferError exposing (InferError, InferErrorDetails(..))
import Elm.TypeInference.ModuleIds as ModuleIds exposing (ModuleId)
import Elm.TypeInference.State as State exposing (StateM)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type exposing (PackageName, VarName)
import Elm.TypeInference.Type.Internal as TypeI exposing (MonoType(..))
import Elm.TypeInference.TypeVar
    exposing
        ( SuperType(..)
        , TypeVar
        , TypeVarStyle(..)
        )


type alias TypeAlias =
    { type_ : MonoType
    , args : List TypeVar
    }


type alias TypeAliases =
    Dict ( ModuleId, PackageName, VarName ) TypeAlias


type alias UnifyConfig =
    { typeAliases : TypeAliases
    , moduleName : FullModuleName
    , declarationNames : List VarName
    , moduleMapping : ModuleIds.Mapping
    }


unifyMany : UnifyConfig -> List ( MonoType, MonoType ) -> StateM ()
unifyMany cfg eqs =
    \state -> unifyManyHelp cfg eqs state


{-| Intentionally not a State.foldl to reduce GC pressure.
-}
unifyManyHelp : UnifyConfig -> List ( MonoType, MonoType ) -> State.State -> ( Result InferError (), State.State )
unifyManyHelp cfg eqs state =
    case eqs of
        [] ->
            ( State.okUnit, state )

        ( t1, t2 ) :: rest ->
            let
                ( st1, _, subst1 ) =
                    SubstitutionMap.substituteMono state.subst t1

                ( st2, _, subst2 ) =
                    SubstitutionMap.substituteMono subst1 t2

                state1 : State.State
                state1 =
                    { nextId = state.nextId
                    , nodeIds = state.nodeIds
                    , lexicalEnv = state.lexicalEnv
                    , globalEnv = state.globalEnv
                    , subst = subst2
                    , letRank = state.letRank
                    }
            in
            case
                unifyMono
                    cfg
                    st1
                    st2
                    state1
            of
                ( Err err, newState ) ->
                    ( Err err, newState )

                ( Ok (), newState ) ->
                    unifyManyHelp cfg rest newState


{-| Expand alias (substitute its args) recursively, then collapse extensible records.

There is a possibility of infinite cycles. We use `fuel` to stop the expansion
after a while and provide a type mismatch instead of a hang.

Intentionally shallow to preserve inferred types to be as high-level (aliases)
as possible, instead of the low-level records underneath.
Full expansion only happens in error reporting.

-}
expandAlias : TypeAliases -> MonoType -> MonoType
expandAlias typeAliases type_ =
    collapseTop (expandAliasHelp maxAliasDepth typeAliases type_)


{-| `expandAlias` which says if the top type was an alias application.
-}
expandAliasTracked : TypeAliases -> MonoType -> ( Bool, MonoType )
expandAliasTracked typeAliases type_ =
    case type_ of
        UserDefinedType ut ->
            case Dict.get ( ut.moduleId, ut.package, ut.name ) typeAliases of
                Nothing ->
                    ( False, type_ )

                Just alias_ ->
                    case zipAliasArgs alias_.args ut.args of
                        Nothing ->
                            ( False, type_ )

                        Just mappings ->
                            ( True
                            , collapseTop
                                (expandAliasHelp (maxAliasDepth - 1)
                                    typeAliases
                                    (substituteAliasArgs mappings alias_.type_)
                                )
                            )

        _ ->
            ( False, expandAlias typeAliases type_ )


collapseTop : MonoType -> MonoType
collapseTop type_ =
    case type_ of
        ExtensibleRecord extensibleRecordUncollapsed ->
            TypeI.collapseExtensible extensibleRecordUncollapsed

        notExtensibleRecord ->
            notExtensibleRecord


{-| This should be enough (any real alias chain like that should be
unreadable/unusable in real code).
-}
maxAliasDepth : Int
maxAliasDepth =
    1000


expandAliasHelp : Int -> TypeAliases -> MonoType -> MonoType
expandAliasHelp fuel typeAliases type_ =
    case type_ of
        UserDefinedType ut ->
            if fuel <= 0 then
                type_

            else
                case Dict.get ( ut.moduleId, ut.package, ut.name ) typeAliases of
                    Nothing ->
                        type_

                    Just alias_ ->
                        case zipAliasArgs alias_.args ut.args of
                            {- Imagine:

                               type alias Pair first second =
                                   ( first, second )

                               x : Pair Int
                               x = ( 1, "oops" )

                            -}
                            Nothing ->
                                type_

                            Just mappings ->
                                expandAliasHelp (fuel - 1) typeAliases (substituteAliasArgs mappings alias_.type_)

        ExtensibleRecord r ->
            if fuel <= 0 then
                type_

            else
                TypeI.collapseExtensible
                    { extensionTypevar = expandAliasHelp (fuel - 1) typeAliases r.extensionTypevar
                    , fields = r.fields
                    }

        _ ->
            type_


{-| Expand nested record aliases for error display.

(Unification is shallow to allow inferred types to be shown as the high-level
aliases instead of as the low-level records underneath them).

-}
expandAliasDeep : TypeAliases -> MonoType -> MonoType
expandAliasDeep typeAliases type_ =
    collapseTop (expandAliasDeepHelp maxAliasDepth typeAliases type_)


expandAliasDeepHelp : Int -> TypeAliases -> MonoType -> MonoType
expandAliasDeepHelp fuel typeAliases type_ =
    case type_ of
        UserDefinedType ut ->
            if fuel <= 0 then
                expandDeepChildren fuel typeAliases type_

            else
                case Dict.get ( ut.moduleId, ut.package, ut.name ) typeAliases of
                    Nothing ->
                        expandDeepChildren fuel typeAliases type_

                    Just alias_ ->
                        case zipAliasArgs alias_.args ut.args of
                            Nothing ->
                                expandDeepChildren fuel typeAliases type_

                            Just mappings ->
                                expandAliasDeepHelp (fuel - 1) typeAliases (substituteAliasArgs mappings alias_.type_)

        _ ->
            expandDeepChildren fuel typeAliases type_


{-| Expand aliases fully. Fuel counts depth instead of breadth.
-}
expandDeepChildren : Int -> TypeAliases -> MonoType -> MonoType
expandDeepChildren fuel typeAliases type_ =
    case type_ of
        TypeI.TypeVar _ ->
            type_

        TypeI.Function f ->
            TypeI.Function
                { from = expandAliasDeepHelp fuel typeAliases f.from
                , to = expandAliasDeepHelp fuel typeAliases f.to
                }

        TypeI.Int ->
            type_

        TypeI.Float ->
            type_

        TypeI.Char ->
            type_

        TypeI.String ->
            type_

        TypeI.Bool ->
            type_

        TypeI.List listItemType ->
            TypeI.List (expandAliasDeepHelp fuel typeAliases listItemType)

        TypeI.Unit ->
            type_

        TypeI.Tuple2 t1 t2 ->
            TypeI.Tuple2
                (expandAliasDeepHelp fuel typeAliases t1)
                (expandAliasDeepHelp fuel typeAliases t2)

        TypeI.Tuple3 t1 t2 t3 ->
            TypeI.Tuple3
                (expandAliasDeepHelp fuel typeAliases t1)
                (expandAliasDeepHelp fuel typeAliases t2)
                (expandAliasDeepHelp fuel typeAliases t3)

        TypeI.Record fields ->
            TypeI.Record
                (Dict.map (\_ v -> expandAliasDeepHelp fuel typeAliases v) fields)

        TypeI.ExtensibleRecord r ->
            TypeI.ExtensibleRecord
                { extensionTypevar = expandAliasDeepHelp fuel typeAliases r.extensionTypevar
                , fields = Dict.map (\_ v -> expandAliasDeepHelp fuel typeAliases v) r.fields
                }

        TypeI.UserDefinedType r ->
            TypeI.UserDefinedType
                { package = r.package
                , moduleId = r.moduleId
                , name = r.name
                , args = List.map (\arg -> expandAliasDeepHelp fuel typeAliases arg) r.args
                }

        TypeI.WebGLShader r ->
            TypeI.WebGLShader
                { attributes = expandAliasDeepHelp fuel typeAliases r.attributes
                , uniforms = expandAliasDeepHelp fuel typeAliases r.uniforms
                , varyings = expandAliasDeepHelp fuel typeAliases r.varyings
                }


{-| Replace type alias' arguments with the supplied types, verbatim.
-}
substituteAliasArgs : List ( TypeVar, MonoType ) -> MonoType -> MonoType
substituteAliasArgs mappings type_ =
    case type_ of
        TypeVar v ->
            case findAliasArg v mappings of
                Just arg ->
                    arg

                Nothing ->
                    type_

        -- The rest is recursion
        Function f ->
            Function
                { from = substituteAliasArgs mappings f.from
                , to = substituteAliasArgs mappings f.to
                }

        Int ->
            type_

        Float ->
            type_

        Char ->
            type_

        String ->
            type_

        Bool ->
            type_

        List listItemType ->
            List (substituteAliasArgs mappings listItemType)

        Unit ->
            type_

        Tuple2 t1 t2 ->
            Tuple2 (substituteAliasArgs mappings t1) (substituteAliasArgs mappings t2)

        Tuple3 t1 t2 t3 ->
            Tuple3
                (substituteAliasArgs mappings t1)
                (substituteAliasArgs mappings t2)
                (substituteAliasArgs mappings t3)

        Record fields ->
            Record (Dict.map (\_ v -> substituteAliasArgs mappings v) fields)

        ExtensibleRecord r ->
            ExtensibleRecord
                { extensionTypevar = substituteAliasArgs mappings r.extensionTypevar
                , fields = Dict.map (\_ v -> substituteAliasArgs mappings v) r.fields
                }

        UserDefinedType r ->
            UserDefinedType
                { package = r.package
                , moduleId = r.moduleId
                , name = r.name
                , args = List.map (\arg -> substituteAliasArgs mappings arg) r.args
                }

        WebGLShader r ->
            WebGLShader
                { attributes = substituteAliasArgs mappings r.attributes
                , uniforms = substituteAliasArgs mappings r.uniforms
                , varyings = substituteAliasArgs mappings r.varyings
                }


{-| Find the var in the alias argument list.
List deemed acceptable (aliases don't have many arguments).
-}
findAliasArg : TypeVar -> List ( TypeVar, MonoType ) -> Maybe MonoType
findAliasArg needle mappings =
    case mappings of
        [] ->
            Nothing

        ( param, argType ) :: rest ->
            if sameVar param needle then
                Just argType

            else
                findAliasArg needle rest


zipAliasArgs : List TypeVar -> List MonoType -> Maybe (List ( TypeVar, MonoType ))
zipAliasArgs params args =
    case params of
        [] ->
            case args of
                [] ->
                    Just []

                _ :: _ ->
                    Nothing

        param :: restParams ->
            case args of
                argType :: restArgs ->
                    case zipAliasArgs restParams restArgs of
                        Just restZipped ->
                            Just (( param, argType ) :: restZipped)

                        Nothing ->
                            Nothing

                [] ->
                    Nothing


zipArgs : List MonoType -> List MonoType -> Maybe (List ( MonoType, MonoType ))
zipArgs args1 args2 =
    case args1 of
        [] ->
            case args2 of
                [] ->
                    Just []

                _ :: _ ->
                    Nothing

        a1 :: rest1 ->
            case args2 of
                a2 :: rest2 ->
                    case zipArgs rest1 rest2 of
                        Just lst ->
                            Just (( a1, a2 ) :: lst)

                        Nothing ->
                            Nothing

                [] ->
                    Nothing


{-| Pair the two record field dicts up _by name_.
`Nothing` if the key sets differ at all.
-}
zipRecordFields : Dict VarName MonoType -> Dict VarName MonoType -> Maybe (List ( MonoType, MonoType ))
zipRecordFields bindings1 bindings2 =
    Dict.merge
        (\_ _ _ -> Nothing)
        (\_ v1 v2 acc -> Maybe.map (\eqs -> ( v1, v2 ) :: eqs) acc)
        (\_ _ _ -> Nothing)
        bindings1
        bindings2
        (Just [])


shaderSlotParts :
    TypeAliases
    -> MonoType
    ->
        Maybe
            { extensionTypevar : MonoType
            , fields : Dict VarName MonoType
            }
shaderSlotParts typeAliases slot =
    case expandAlias typeAliases slot of
        Record fields ->
            Just
                { extensionTypevar = Record Dict.empty
                , fields = fields
                }

        ExtensibleRecord er ->
            Just er

        TypeVar v ->
            Just
                { extensionTypevar = TypeVar v
                , fields = Dict.empty
                }

        _ ->
            Nothing


{-| Faster than `param == needle`
-}
sameVar : TypeVar -> TypeVar -> Bool
sameVar ( style1, super1 ) ( style2, super2 ) =
    case style1 of
        Generated id1 ->
            case style2 of
                Generated id2 ->
                    id1 == id2 && super1 == super2

                Named _ ->
                    False

        Named name1 ->
            case style2 of
                Named name2 ->
                    name1 == name2 && super1 == super2

                Generated _ ->
                    False


shallowEqual : MonoType -> MonoType -> Bool
shallowEqual t1 t2 =
    case t1 of
        TypeVar v1 ->
            case t2 of
                TypeVar v2 ->
                    sameVar v1 v2

                _ ->
                    False

        Int ->
            case t2 of
                Int ->
                    True

                _ ->
                    False

        Float ->
            case t2 of
                Float ->
                    True

                _ ->
                    False

        Char ->
            case t2 of
                Char ->
                    True

                _ ->
                    False

        String ->
            case t2 of
                String ->
                    True

                _ ->
                    False

        Bool ->
            case t2 of
                Bool ->
                    True

                _ ->
                    False

        Unit ->
            case t2 of
                Unit ->
                    True

                _ ->
                    False

        _ ->
            False


typeMismatch : UnifyConfig -> MonoType -> MonoType -> StateM ()
typeMismatch cfg t1 t2 =
    let
        ( pubT1, pubT2 ) =
            TypeI.toPublicPair cfg.moduleMapping
                (expandAliasDeep cfg.typeAliases t1)
                (expandAliasDeep cfg.typeAliases t2)
    in
    State.error
        { moduleName = FullModuleName.toModuleName cfg.moduleName
        , declarationNames = cfg.declarationNames
        , details = TypeMismatch pubT1 pubT2
        }


recordBindings : UnifyConfig -> MonoType -> MonoType -> Dict VarName MonoType -> Dict VarName MonoType -> StateM ()
recordBindings cfg t1 t2 bindings1 bindings2 =
    case zipRecordFields bindings1 bindings2 of
        Nothing ->
            typeMismatch cfg t1 t2

        Just eqs ->
            unifyMany cfg eqs


unifyRecordVsExtensible :
    UnifyConfig
    -> MonoType
    -> MonoType
    -> Dict VarName MonoType
    ->
        { extensionTypevar : MonoType
        , fields : Dict VarName MonoType
        }
    -> StateM ()
unifyRecordVsExtensible cfg t1 t2 recordFields er =
    case expandAlias cfg.typeAliases er.extensionTypevar of
        Record extFields ->
            let
                overlapEqs : List ( MonoType, MonoType )
                overlapEqs =
                    Dict.merge
                        (\_ _ eqs -> eqs)
                        (\_ v1 v2 eqs -> ( v1, v2 ) :: eqs)
                        (\_ _ eqs -> eqs)
                        extFields
                        er.fields
                        []

                combined : Dict VarName MonoType
                combined =
                    Dict.union er.fields extFields
            in
            State.do (unifyMany cfg overlapEqs) <|
                \() ->
                    recordBindings cfg t1 t2 combined recordFields

        ExtensibleRecord extEr ->
            let
                overlapEqs : List ( MonoType, MonoType )
                overlapEqs =
                    Dict.merge
                        (\_ _ eqs -> eqs)
                        (\_ v1 v2 eqs -> ( v1, v2 ) :: eqs)
                        (\_ _ eqs -> eqs)
                        extEr.fields
                        er.fields
                        []

                merged : Dict VarName MonoType
                merged =
                    Dict.union er.fields extEr.fields
            in
            State.do (unifyMany cfg overlapEqs) <|
                \() ->
                    unifyRecordVsExtensible cfg
                        t1
                        t2
                        recordFields
                        { extensionTypevar = extEr.extensionTypevar
                        , fields = merged
                        }

        _ ->
            let
                ( residual, matchedEqs, matchedCount ) =
                    Dict.foldl
                        (\k v ( res, eqs, n ) ->
                            case Dict.get k er.fields of
                                Just ev ->
                                    ( res
                                    , ( v, ev ) :: eqs
                                    , n + 1
                                    )

                                Nothing ->
                                    ( Dict.insert k v res
                                    , eqs
                                    , n
                                    )
                        )
                        ( Dict.empty, [], 0 )
                        recordFields
            in
            -- matchedCount /= Dict.size er.fields
            if matchedCount - Dict.size er.fields /= 0 then
                typeMismatch cfg t1 t2

            else
                unifyMany cfg
                    (( er.extensionTypevar, Record residual )
                        :: matchedEqs
                    )


unifyMono : UnifyConfig -> MonoType -> MonoType -> StateM ()
unifyMono cfg rawT1 rawT2 =
    -- `shallowEqual` is a cheap fast check; `==` calls `_Utils_eq` and short-circuits on `===`
    if shallowEqual rawT1 rawT2 || rawT1 == rawT2 then
        State.pureUnit

    else
        case rawT1 of
            UserDefinedType ut1 ->
                case rawT2 of
                    UserDefinedType ut2 ->
                        if sameNamedType ut1 ut2 then
                            case zipArgs ut1.args ut2.args of
                                Just eqs ->
                                    unifyMany cfg eqs

                                Nothing ->
                                    typeMismatch cfg rawT1 rawT2

                        else
                            unifyAliased cfg rawT1 rawT2

                    _ ->
                        unifyAliased cfg rawT1 rawT2

            _ ->
                unifyAliased cfg rawT1 rawT2


sameNamedType :
    { a | package : PackageName, moduleId : ModuleId, name : VarName }
    -> { b | package : PackageName, moduleId : ModuleId, name : VarName }
    -> Bool
sameNamedType ut1 ut2 =
    (ut1.moduleId == ut2.moduleId)
        && (ut1.name == ut2.name)
        && (ut1.package == ut2.package)


{-| Expand aliases at the top of both sides and unify what's underneath.
-}
unifyAliased : UnifyConfig -> MonoType -> MonoType -> StateM ()
unifyAliased cfg rawT1 rawT2 =
    let
        ( isAlias1, t1 ) =
            expandAliasTracked cfg.typeAliases rawT1

        ( isAlias2, t2 ) =
            expandAliasTracked cfg.typeAliases rawT2
    in
    case t1 of
        TypeVar v ->
            if isAlias2 then
                bindAlias cfg v rawT2 t2

            else
                bind cfg v t2

        _ ->
            case t2 of
                TypeVar v ->
                    if isAlias1 then
                        bindAlias cfg v rawT1 t1

                    else
                        bind cfg v t1

                _ ->
                    unifyExpanded cfg t1 t2


{-| Bind a variable to an alias application `raw` (which expands to `expanded`),
keeping the alias if we can.
-}
bindAlias : UnifyConfig -> TypeVar -> MonoType -> MonoType -> StateM ()
bindAlias cfg typeVar raw expanded =
    if isTypeVar expanded || occursCheck typeVar raw then
        bind cfg typeVar expanded

    else
        -- `raw` passed the occurs check above.
        bindChecked cfg typeVar raw expanded


isTypeVar : MonoType -> Bool
isTypeVar type_ =
    case type_ of
        TypeVar _ ->
            True

        _ ->
            False


unifyExpanded : UnifyConfig -> MonoType -> MonoType -> StateM ()
unifyExpanded cfg t1 t2 =
    case t1 of
        TypeVar v ->
            bind cfg v t2

        Int ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                Int ->
                    -- no substitution needed
                    State.pureUnit

                _ ->
                    typeMismatch cfg t1 t2

        Float ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                Float ->
                    -- no substitution needed
                    State.pureUnit

                _ ->
                    typeMismatch cfg t1 t2

        String ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                String ->
                    -- no substitution needed
                    State.pureUnit

                _ ->
                    typeMismatch cfg t1 t2

        Char ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                Char ->
                    -- no substitution needed
                    State.pureUnit

                _ ->
                    typeMismatch cfg t1 t2

        Bool ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                Bool ->
                    -- no substitution needed
                    State.pureUnit

                _ ->
                    typeMismatch cfg t1 t2

        Unit ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                Unit ->
                    -- no substitution needed
                    State.pureUnit

                _ ->
                    typeMismatch cfg t1 t2

        Function a ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                Function b ->
                    unifyMany
                        cfg
                        [ ( a.from, b.from )
                        , ( a.to, b.to )
                        ]

                _ ->
                    typeMismatch cfg t1 t2

        List list1 ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                List list2 ->
                    unifyMany cfg [ ( list1, list2 ) ]

                _ ->
                    typeMismatch cfg t1 t2

        Tuple2 t1e1 t1e2 ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                Tuple2 t2e1 t2e2 ->
                    unifyMany
                        cfg
                        [ ( t1e1, t2e1 )
                        , ( t1e2, t2e2 )
                        ]

                _ ->
                    typeMismatch cfg t1 t2

        Tuple3 t1e1 t1e2 t1e3 ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                Tuple3 t2e1 t2e2 t2e3 ->
                    unifyMany
                        cfg
                        [ ( t1e1, t2e1 )
                        , ( t1e2, t2e2 )
                        , ( t1e3, t2e3 )
                        ]

                _ ->
                    typeMismatch cfg t1 t2

        Record r1 ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                Record r2 ->
                    recordBindings cfg t1 t2 r1 r2

                ExtensibleRecord er2 ->
                    unifyRecordVsExtensible cfg t1 t2 r1 er2

                _ ->
                    typeMismatch cfg t1 t2

        ExtensibleRecord r1 ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                ExtensibleRecord r2 ->
                    {- Fields that only one side mentions must be added to the other
                       side's required fields.
                       Both sides' extension typevars (the r in { r | ... })
                       now need to be the same var.

                       ie.
                       - getX : { row1 | x : Float } -> Float
                       - getY : { row2 | y : Float } -> Float
                       - sum r = getX r + getY r
                       Use them both on the same record and you get
                       - sum : { commonVar | x : Float, y : Float } -> Float
                    -}
                    let
                        ( onlyIn1, onlyIn2, sharedEqs ) =
                            Dict.merge
                                (\k v ( o1, o2, eqs ) ->
                                    ( Dict.insert k v o1
                                    , o2
                                    , eqs
                                    )
                                )
                                (\_ v1 v2 ( o1, o2, eqs ) ->
                                    ( o1
                                    , o2
                                    , ( v1, v2 ) :: eqs
                                    )
                                )
                                (\k v ( o1, o2, eqs ) ->
                                    ( o1
                                    , Dict.insert k v o2
                                    , eqs
                                    )
                                )
                                r1.fields
                                r2.fields
                                ( Dict.empty, Dict.empty, [] )
                    in
                    if Dict.isEmpty onlyIn1 && Dict.isEmpty onlyIn2 then
                        {- Same field set on both sides -> the `r` in `{r | ...}`
                           must be the same for both sides.
                        -}
                        unifyMany cfg (( r1.extensionTypevar, r2.extensionTypevar ) :: sharedEqs)

                    else
                        State.do State.getNextIdAndTick <|
                            \tailId ->
                                let
                                    tail : MonoType
                                    tail =
                                        TypeI.id_ tailId
                                in
                                unifyMany
                                    cfg
                                    (( r1.extensionTypevar
                                     , ExtensibleRecord
                                        { extensionTypevar = tail
                                        , fields = onlyIn2
                                        }
                                     )
                                        :: ( r2.extensionTypevar
                                           , ExtensibleRecord
                                                { extensionTypevar = tail
                                                , fields = onlyIn1
                                                }
                                           )
                                        :: sharedEqs
                                    )

                Record r2 ->
                    unifyRecordVsExtensible cfg t1 t2 r2 r1

                _ ->
                    typeMismatch cfg t1 t2

        UserDefinedType ut1 ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                UserDefinedType ut2 ->
                    if
                        (ut1.package /= ut2.package)
                            || (ut1.moduleId /= ut2.moduleId)
                            || (ut1.name /= ut2.name)
                    then
                        typeMismatch cfg t1 t2

                    else
                        case zipArgs ut1.args ut2.args of
                            Nothing ->
                                typeMismatch cfg t1 t2

                            Just eqs ->
                                unifyMany cfg eqs

                _ ->
                    typeMismatch cfg t1 t2

        WebGLShader webgl1 ->
            case t2 of
                TypeVar v ->
                    bind cfg v t1

                WebGLShader webgl2 ->
                    let
                        {- Unify one of a shader's attribute/uniform/varying sets.

                           Shader sets are extensible records (can be narrowed
                           down to normal records with a type annotation).
                        -}
                        webglSet : MonoType -> MonoType -> StateM ()
                        webglSet slot1 slot2 =
                            case ( shaderSlotParts cfg.typeAliases slot1, shaderSlotParts cfg.typeAliases slot2 ) of
                                ( Just set1, Just set2 ) ->
                                    webglSetParts set1 set2

                                _ ->
                                    typeMismatch cfg t1 t2

                        webglSetParts :
                            { extensionTypevar : MonoType, fields : Dict VarName MonoType }
                            -> { extensionTypevar : MonoType, fields : Dict VarName MonoType }
                            -> StateM ()
                        webglSetParts set1 set2 =
                            let
                                ( only1, only2, sharedEqs ) =
                                    Dict.merge
                                        (\k v ( o1, o2, eqs ) ->
                                            ( Dict.insert k v o1, o2, eqs )
                                        )
                                        (\_ v1 v2 ( o1, o2, eqs ) ->
                                            ( o1, o2, ( v1, v2 ) :: eqs )
                                        )
                                        (\k v ( o1, o2, eqs ) ->
                                            ( o1, Dict.insert k v o2, eqs )
                                        )
                                        set1.fields
                                        set2.fields
                                        ( Dict.empty, Dict.empty, [] )

                                isClosed : MonoType -> Bool
                                isClosed extensionTypevar =
                                    case extensionTypevar of
                                        Record _ ->
                                            True

                                        _ ->
                                            False

                                closed1 : Bool
                                closed1 =
                                    isClosed set1.extensionTypevar

                                closed2 : Bool
                                closed2 =
                                    isClosed set2.extensionTypevar
                            in
                            if closed1 && closed2 && not (Dict.isEmpty only1 && Dict.isEmpty only2) then
                                typeMismatch cfg t1 t2

                            else
                                State.do State.getNextIdAndTick <|
                                    \tailId ->
                                        let
                                            tail : MonoType
                                            tail =
                                                TypeI.id_ tailId

                                            absorb : MonoType -> Dict VarName MonoType -> List ( MonoType, MonoType )
                                            absorb extensionTypevar fields =
                                                [ ( extensionTypevar
                                                  , ExtensibleRecord
                                                        { extensionTypevar = tail
                                                        , fields = fields
                                                        }
                                                  )
                                                ]
                                        in
                                        if not closed1 && not closed2 then
                                            unifyMany cfg (absorb set1.extensionTypevar only2 ++ absorb set2.extensionTypevar only1 ++ sharedEqs)

                                        else if not closed1 then
                                            unifyMany cfg (absorb set1.extensionTypevar only2 ++ sharedEqs)

                                        else if not closed2 then
                                            unifyMany cfg (absorb set2.extensionTypevar only1 ++ sharedEqs)

                                        else
                                            unifyMany cfg sharedEqs
                    in
                    State.do (webglSet webgl1.attributes webgl2.attributes) <|
                        \() ->
                            State.do (webglSet webgl1.uniforms webgl2.uniforms) <|
                                \() ->
                                    webglSet webgl1.varyings webgl2.varyings

                _ ->
                    typeMismatch cfg t1 t2


{-| Binds an unbound typeVar root with a given monotype.
Both are already substituted.
-}
bind : UnifyConfig -> TypeVar -> MonoType -> StateM ()
bind cfg typeVar type_ =
    if shallowEqual type_ (TypeVar typeVar) then
        State.pureUnit

    else if occursCheck typeVar type_ then
        let
            ( pubVar, pubType ) =
                TypeI.toPublicPair cfg.moduleMapping (TypeVar typeVar) type_
        in
        State.error
            { moduleName = FullModuleName.toModuleName cfg.moduleName
            , declarationNames = cfg.declarationNames
            , details = InfiniteType pubVar pubType
            }

    else
        bindChecked cfg typeVar type_ type_


bindChecked : UnifyConfig -> TypeVar -> MonoType -> MonoType -> StateM ()
bindChecked cfg typeVar type_ expanded =
    \state ->
        if SubstitutionMap.isRigid typeVar state.subst then
            bindRigid cfg typeVar type_ state

        else
            case type_ of
                TypeVar otherVar ->
                    if SubstitutionMap.isRigid otherVar state.subst then
                        bindRigid cfg otherVar (TypeVar typeVar) state

                    else
                        bindFlexVars cfg typeVar otherVar state

                _ ->
                    if accepts cfg.typeAliases (Tuple.second typeVar) expanded then
                        State.modifySubst (\subst -> SubstitutionMap.bindRoot typeVar type_ subst) state

                    else
                        constraintMismatch cfg (TypeVar typeVar) type_ state


{-| Rigid var from annotation stands for any type, so inside the decl it can't
be made more specific.
-}
bindRigid : UnifyConfig -> TypeVar -> MonoType -> StateM ()
bindRigid cfg (( _, rigidSuper ) as rigidVar) other =
    \state ->
        case other of
            TypeVar (( _, otherSuper ) as otherVar) ->
                if SubstitutionMap.isRigid otherVar state.subst then
                    -- Two distinct rigid vars (the same var never gets here, see `bind`)
                    typeMismatch cfg (TypeVar rigidVar) other state

                else if meet rigidSuper otherSuper == Just rigidSuper then
                    State.modifySubst
                        (\subst -> SubstitutionMap.linkTo { child = otherVar, parent = rigidVar } subst)
                        state

                else
                    constraintMismatch cfg (TypeVar rigidVar) other state

            _ ->
                typeMismatch cfg (TypeVar rigidVar) other state


constraintMismatch : UnifyConfig -> MonoType -> MonoType -> StateM ()
constraintMismatch cfg t1 t2 =
    let
        ( pubT1, pubT2 ) =
            TypeI.toPublicPair cfg.moduleMapping t1 t2
    in
    State.error
        { moduleName = FullModuleName.toModuleName cfg.moduleName
        , declarationNames = cfg.declarationNames
        , details = ConstraintMismatch pubT1 pubT2
        }


{-| Merge two flexible variables, letting the more constrained one win.
-}
bindFlexVars : UnifyConfig -> TypeVar -> TypeVar -> StateM ()
bindFlexVars cfg (( typeVarStyle, super ) as typeVar) (( otherStyle, otherSuper ) as otherVar) =
    case meet super otherSuper of
        Nothing ->
            constraintMismatch cfg (TypeVar typeVar) (TypeVar otherVar)

        Just m ->
            if m == super && m == otherSuper then
                -- Either could be chosen as then parent (linked to),
                -- but we prefer Generated ids as they can't collide.
                State.modifySubst <|
                    \subst ->
                        case typeVarStyle of
                            Named _ ->
                                case otherStyle of
                                    Generated _ ->
                                        subst |> SubstitutionMap.linkTo { child = typeVar, parent = otherVar }

                                    Named _ ->
                                        subst |> SubstitutionMap.union typeVar otherVar

                            Generated _ ->
                                case otherStyle of
                                    Named _ ->
                                        subst |> SubstitutionMap.linkTo { child = otherVar, parent = typeVar }

                                    Generated _ ->
                                        subst |> SubstitutionMap.union typeVar otherVar

            else if m == otherSuper then
                -- otherVar is more constrained -> it will be the `parent` representative.
                State.modifySubst (\subst -> subst |> SubstitutionMap.linkTo { child = typeVar, parent = otherVar })

            else if m == super then
                State.modifySubst (\subst -> subst |> SubstitutionMap.linkTo { child = otherVar, parent = typeVar })

            else
                -- eg. Comparable and Appendable
                -- introduce fresh var with combined constraint
                -- point both at it
                State.do State.getNextIdAndTick <|
                    \freshId ->
                        let
                            fresh : TypeVar
                            fresh =
                                ( Generated freshId, m )
                        in
                        State.modifySubst
                            (\subst ->
                                subst
                                    |> SubstitutionMap.linkTo { child = typeVar, parent = fresh }
                                    |> SubstitutionMap.linkTo { child = otherVar, parent = fresh }
                            )


{-| The most specific supertype that satisfies both constraints, if any.
-}
meet : SuperType -> SuperType -> Maybe SuperType
meet a b =
    case a of
        Normal ->
            Just b

        Number ->
            case b of
                Normal ->
                    Just Number

                Number ->
                    Just Number

                Comparable ->
                    Just Number

                Appendable ->
                    Nothing

                CompAppend ->
                    Nothing

        Comparable ->
            case b of
                Normal ->
                    Just Comparable

                Number ->
                    Just Number

                Comparable ->
                    Just Comparable

                Appendable ->
                    Just CompAppend

                CompAppend ->
                    Just CompAppend

        Appendable ->
            case b of
                Normal ->
                    Just Appendable

                Number ->
                    Nothing

                Comparable ->
                    Just CompAppend

                Appendable ->
                    Just Appendable

                CompAppend ->
                    Just CompAppend

        CompAppend ->
            case b of
                Normal ->
                    Just CompAppend

                Number ->
                    Nothing

                Comparable ->
                    Just CompAppend

                Appendable ->
                    Just CompAppend

                CompAppend ->
                    Just CompAppend


accepts : TypeAliases -> SuperType -> MonoType -> Bool
accepts typeAliases super type_ =
    case super of
        Normal ->
            True

        Number ->
            case type_ of
                Int ->
                    True

                Float ->
                    True

                _ ->
                    False

        Comparable ->
            isComparable typeAliases type_

        Appendable ->
            isAppendable typeAliases type_

        CompAppend ->
            isComparable typeAliases type_ && isAppendable typeAliases type_


isComparable : TypeAliases -> MonoType -> Bool
isComparable typeAliases type_ =
    case expandAlias typeAliases type_ of
        Int ->
            True

        Float ->
            True

        Char ->
            True

        String ->
            True

        List inner ->
            isComparable typeAliases inner

        Tuple2 a b ->
            isComparable typeAliases a && isComparable typeAliases b

        Tuple3 a b c ->
            isComparable typeAliases a && isComparable typeAliases b && isComparable typeAliases c

        TypeVar _ ->
            True

        Function _ ->
            False

        Bool ->
            False

        Unit ->
            False

        Record _ ->
            False

        ExtensibleRecord _ ->
            False

        UserDefinedType _ ->
            False

        WebGLShader _ ->
            False


isAppendable : TypeAliases -> MonoType -> Bool
isAppendable typeAliases type_ =
    case expandAlias typeAliases type_ of
        String ->
            True

        List _ ->
            True

        TypeVar _ ->
            True

        Int ->
            False

        Float ->
            False

        Char ->
            False

        Tuple2 _ _ ->
            False

        Tuple3 _ _ _ ->
            False

        Function _ ->
            False

        Bool ->
            False

        Unit ->
            False

        Record _ ->
            False

        ExtensibleRecord _ ->
            False

        UserDefinedType _ ->
            False

        WebGLShader _ ->
            False


{-| Does `typeVar` occur anywhere in `type_`?
-}
occursCheck : TypeVar -> MonoType -> Bool
occursCheck typeVar type_ =
    case type_ of
        TypeVar var ->
            sameVar var typeVar

        Function { from, to } ->
            occursCheck typeVar from || occursCheck typeVar to

        Int ->
            False

        Float ->
            False

        Char ->
            False

        String ->
            False

        Bool ->
            False

        List listItemType ->
            occursCheck typeVar listItemType

        Unit ->
            False

        Tuple2 t1 t2 ->
            occursCheck typeVar t1 || occursCheck typeVar t2

        Tuple3 t1 t2 t3 ->
            occursCheck typeVar t1
                || occursCheck typeVar t2
                || occursCheck typeVar t3

        Record fields ->
            Dict.Extra.any (\_ v -> occursCheck typeVar v) fields

        ExtensibleRecord r ->
            occursCheck typeVar r.extensionTypevar
                || Dict.Extra.any (\_ v -> occursCheck typeVar v) r.fields

        UserDefinedType r ->
            List.any (\arg -> occursCheck typeVar arg) r.args

        WebGLShader r ->
            occursCheck typeVar r.attributes
                || occursCheck typeVar r.uniforms
                || occursCheck typeVar r.varyings
