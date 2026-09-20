module Elm.TypeInference.Unify exposing (TypeAlias, UnifyConfig, unifyMany)

import Dict exposing (Dict)
import Dict.Extra
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.TypeInference.Error exposing (Error, ErrorDetails(..))
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
unifyManyHelp : UnifyConfig -> List ( MonoType, MonoType ) -> State.State -> ( Result Error (), State.State )
unifyManyHelp cfg eqs state =
    case eqs of
        [] ->
            ( Ok (), state )

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
    expandAliasHelp maxAliasDepth typeAliases type_
        |> TypeI.collapseExtensible


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
                    (ExtensibleRecord
                        { extensionTypevar = expandAliasHelp (fuel - 1) typeAliases r.extensionTypevar
                        , fields = r.fields
                        }
                    )

        _ ->
            type_


{-| Expand nested record aliases for error display.

(Unification is shallow to allow inferred types to be shown as the high-level
aliases instead of as the low-level records underneath them).

-}
expandAliasDeep : TypeAliases -> MonoType -> MonoType
expandAliasDeep typeAliases type_ =
    expandAliasDeepHelp maxAliasDepth typeAliases type_
        |> TypeI.collapseExtensible


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

        TypeI.Record r ->
            TypeI.Record
                { fields = Dict.map (\_ v -> expandAliasDeepHelp fuel typeAliases v) r.fields }

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
                { attributesExtension = expandAliasDeepHelp fuel typeAliases r.attributesExtension
                , attributes = Dict.map (\_ v -> expandAliasDeepHelp fuel typeAliases v) r.attributes
                , uniformsExtension = expandAliasDeepHelp fuel typeAliases r.uniformsExtension
                , uniforms = Dict.map (\_ v -> expandAliasDeepHelp fuel typeAliases v) r.uniforms
                , varyingsExtension = expandAliasDeepHelp fuel typeAliases r.varyingsExtension
                , varyings = Dict.map (\_ v -> expandAliasDeepHelp fuel typeAliases v) r.varyings
                }


{-| Replace type alias' arguments with the supplied types, verbatim.
-}
substituteAliasArgs : List ( TypeVar, MonoType ) -> MonoType -> MonoType
substituteAliasArgs mappings type_ =
    let
        go : MonoType -> MonoType
        go =
            substituteAliasArgs mappings
    in
    case type_ of
        TypeVar v ->
            findAliasArg v mappings
                |> Maybe.withDefault type_

        -- The rest is recursion
        Function f ->
            Function { from = go f.from, to = go f.to }

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
            List (go listItemType)

        Unit ->
            type_

        Tuple2 t1 t2 ->
            Tuple2 (go t1) (go t2)

        Tuple3 t1 t2 t3 ->
            Tuple3 (go t1) (go t2) (go t3)

        Record { fields } ->
            Record { fields = Dict.map (\_ v -> go v) fields }

        ExtensibleRecord r ->
            ExtensibleRecord
                { extensionTypevar = go r.extensionTypevar
                , fields = Dict.map (\_ v -> go v) r.fields
                }

        UserDefinedType r ->
            UserDefinedType
                { package = r.package
                , moduleId = r.moduleId
                , name = r.name
                , args = List.map go r.args
                }

        WebGLShader r ->
            WebGLShader
                { attributesExtension = go r.attributesExtension
                , attributes = Dict.map (\_ v -> go v) r.attributes
                , uniformsExtension = go r.uniformsExtension
                , uniforms = Dict.map (\_ v -> go v) r.uniforms
                , varyingsExtension = go r.varyingsExtension
                , varyings = Dict.map (\_ v -> go v) r.varyings
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
    case ( params, args ) of
        ( [], [] ) ->
            Just []

        ( param :: restParams, argType :: restArgs ) ->
            zipAliasArgs restParams restArgs
                |> Maybe.map (\restZipped -> ( param, argType ) :: restZipped)

        _ ->
            Nothing


zipArgs : List MonoType -> List MonoType -> Maybe (List ( MonoType, MonoType ))
zipArgs args1 args2 =
    case ( args1, args2 ) of
        ( [], [] ) ->
            Just []

        ( a1 :: rest1, a2 :: rest2 ) ->
            case zipArgs rest1 rest2 of
                Just lst ->
                    Just (( a1, a2 ) :: lst)

                Nothing ->
                    Nothing

        ( _ :: _, [] ) ->
            Nothing

        ( [], _ :: _ ) ->
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


{-| A `Shader` annotation can mention a type alias:

    type alias Vertex =
        { position : Vec2 }

    shader : Shader Vertex { view : Mat4 } { vcoord : Vec2 }

`fromTypeAnnotation` can't expand that alias (it doesn't know them yet).
Expand the args and retry the collapse here, where aliases are known.

-}
collapseNamedShader : TypeAliases -> MonoType -> MonoType
collapseNamedShader typeAliases type_ =
    case type_ of
        UserDefinedType ut ->
            case TypeI.collapsePrimitive ut.package ut.moduleId ut.name (List.map (\arg -> expandAlias typeAliases arg) ut.args) of
                Just collapsed ->
                    collapsed

                Nothing ->
                    type_

        _ ->
            type_


{-| Faster than `param == needle`
-}
sameVar : TypeVar -> TypeVar -> Bool
sameVar ( style1, super1 ) ( style2, super2 ) =
    case ( style1, style2 ) of
        ( Generated id1, Generated id2 ) ->
            id1 == id2 && super1 == super2

        ( Named name1, Named name2 ) ->
            name1 == name2 && super1 == super2

        _ ->
            False


shallowEqual : MonoType -> MonoType -> Bool
shallowEqual t1 t2 =
    case ( t1, t2 ) of
        ( TypeVar v1, TypeVar v2 ) ->
            sameVar v1 v2

        ( Int, Int ) ->
            True

        ( Float, Float ) ->
            True

        ( Char, Char ) ->
            True

        ( String, String ) ->
            True

        ( Bool, Bool ) ->
            True

        ( Unit, Unit ) ->
            True

        _ ->
            False


unifyMono : UnifyConfig -> MonoType -> MonoType -> StateM ()
unifyMono cfg rawT1 rawT2 =
    -- `shallowEqual` is a cheap fast check; `==` calls `_Utils_eq` and short-circuits on `===`
    if shallowEqual rawT1 rawT2 || rawT1 == rawT2 then
        State.pure ()

    else
        let
            t1 : MonoType
            t1 =
                expandAlias cfg.typeAliases rawT1
                    |> collapseNamedShader cfg.typeAliases

            t2 : MonoType
            t2 =
                expandAlias cfg.typeAliases rawT2
                    |> collapseNamedShader cfg.typeAliases

            noSubstitutionNeeded : StateM ()
            noSubstitutionNeeded =
                State.pure ()

            typeMismatch : () -> StateM ()
            typeMismatch () =
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

            recordBindings : Dict VarName MonoType -> Dict VarName MonoType -> StateM ()
            recordBindings bindings1 bindings2 =
                if Dict.size bindings1 /= Dict.size bindings2 then
                    typeMismatch ()

                else
                    case zipRecordFields bindings1 bindings2 of
                        Nothing ->
                            typeMismatch ()

                        Just eqs ->
                            unifyMany cfg eqs

            recordVsExtensible :
                Dict VarName MonoType
                ->
                    { extensionTypevar : MonoType
                    , fields : Dict VarName MonoType
                    }
                -> StateM ()
            recordVsExtensible recordFields er =
                case expandAlias cfg.typeAliases er.extensionTypevar of
                    Record extFields ->
                        let
                            ( _, _, overlapEqs ) =
                                Dict.merge
                                    (\_ _ ( o1, o2, eqs ) -> ( o1, o2, eqs ))
                                    (\_ v1 v2 ( o1, o2, eqs ) -> ( o1, o2, ( v1, v2 ) :: eqs ))
                                    (\_ _ ( o1, o2, eqs ) -> ( o1, o2, eqs ))
                                    extFields.fields
                                    er.fields
                                    ( Dict.empty, Dict.empty, [] )

                            combined : Dict VarName MonoType
                            combined =
                                Dict.union er.fields extFields.fields
                        in
                        State.do (unifyMany cfg overlapEqs) <|
                            \() ->
                                recordBindings combined recordFields

                    ExtensibleRecord extEr ->
                        let
                            ( _, _, overlapEqs ) =
                                Dict.merge
                                    (\_ _ ( o1, o2, eqs ) -> ( o1, o2, eqs ))
                                    (\_ v1 v2 ( o1, o2, eqs ) -> ( o1, o2, ( v1, v2 ) :: eqs ))
                                    (\_ _ ( o1, o2, eqs ) -> ( o1, o2, eqs ))
                                    extEr.fields
                                    er.fields
                                    ( Dict.empty, Dict.empty, [] )

                            merged : Dict VarName MonoType
                            merged =
                                Dict.union er.fields extEr.fields
                        in
                        State.do (unifyMany cfg overlapEqs) <|
                            \() ->
                                recordVsExtensible recordFields
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
                        if matchedCount /= Dict.size er.fields then
                            typeMismatch ()

                        else
                            unifyMany cfg
                                (( er.extensionTypevar, Record { fields = residual } )
                                    :: matchedEqs
                                )

            {- Unify one of a shader's attribute/uniform/varying sets.

               Shader sets are special: the GLSL literal opens them, and a type
               annotation can narrow them down to a closed record. So a closed
               side only requires that its fields are present (with matching
               types) in the other side; the open side absorbs the difference.
               Two closed sides still have to agree on the field set.
            -}
            webglSet :
                { extensionTypevar : MonoType, fields : Dict VarName MonoType }
                -> { extensionTypevar : MonoType, fields : Dict VarName MonoType }
                -> StateM ()
            webglSet set1 set2 =
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
                    typeMismatch ()

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
        case ( t1, t2 ) of
            ( TypeVar v, _ ) ->
                bind cfg v t2

            ( _, TypeVar v ) ->
                bind cfg v t1

            ( Int, Int ) ->
                noSubstitutionNeeded

            ( Int, _ ) ->
                typeMismatch ()

            ( Float, Float ) ->
                noSubstitutionNeeded

            ( Float, _ ) ->
                typeMismatch ()

            ( String, String ) ->
                noSubstitutionNeeded

            ( String, _ ) ->
                typeMismatch ()

            ( Char, Char ) ->
                noSubstitutionNeeded

            ( Char, _ ) ->
                typeMismatch ()

            ( Bool, Bool ) ->
                noSubstitutionNeeded

            ( Bool, _ ) ->
                typeMismatch ()

            ( Unit, Unit ) ->
                noSubstitutionNeeded

            ( Unit, _ ) ->
                typeMismatch ()

            ( Function a, Function b ) ->
                unifyMany
                    cfg
                    [ ( a.from, b.from )
                    , ( a.to, b.to )
                    ]

            ( Function _, _ ) ->
                typeMismatch ()

            ( List list1, List list2 ) ->
                unifyMany cfg [ ( list1, list2 ) ]

            ( List _, _ ) ->
                typeMismatch ()

            ( Tuple2 t1e1 t1e2, Tuple2 t2e1 t2e2 ) ->
                unifyMany
                    cfg
                    [ ( t1e1, t2e1 )
                    , ( t1e2, t2e2 )
                    ]

            ( Tuple2 _ _, _ ) ->
                typeMismatch ()

            ( Tuple3 t1e1 t1e2 t1e3, Tuple3 t2e1 t2e2 t2e3 ) ->
                unifyMany
                    cfg
                    [ ( t1e1, t2e1 )
                    , ( t1e2, t2e2 )
                    , ( t1e3, t2e3 )
                    ]

            ( Tuple3 _ _ _, _ ) ->
                typeMismatch ()

            ( Record r1, Record r2 ) ->
                recordBindings r1.fields r2.fields

            ( Record r, ExtensibleRecord er ) ->
                recordVsExtensible r.fields er

            ( Record _, _ ) ->
                typeMismatch ()

            ( ExtensibleRecord r1, ExtensibleRecord r2 ) ->
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

            ( ExtensibleRecord er, Record r ) ->
                recordVsExtensible r.fields er

            ( ExtensibleRecord _, _ ) ->
                typeMismatch ()

            ( UserDefinedType ut1, UserDefinedType ut2 ) ->
                if
                    (ut1.package /= ut2.package)
                        || (ut1.moduleId /= ut2.moduleId)
                        || (ut1.name /= ut2.name)
                then
                    typeMismatch ()

                else
                    case zipArgs ut1.args ut2.args of
                        Nothing ->
                            typeMismatch ()

                        Just eqs ->
                            unifyMany cfg eqs

            ( UserDefinedType _, _ ) ->
                typeMismatch ()

            ( WebGLShader webgl1, WebGLShader webgl2 ) ->
                State.do
                    (webglSet
                        { extensionTypevar = webgl1.attributesExtension
                        , fields = webgl1.attributes
                        }
                        { extensionTypevar = webgl2.attributesExtension
                        , fields = webgl2.attributes
                        }
                    )
                <|
                    \() ->
                        State.do
                            (webglSet
                                { extensionTypevar = webgl1.uniformsExtension
                                , fields = webgl1.uniforms
                                }
                                { extensionTypevar = webgl2.uniformsExtension
                                , fields = webgl2.uniforms
                                }
                            )
                        <|
                            \() ->
                                webglSet
                                    { extensionTypevar = webgl1.varyingsExtension
                                    , fields = webgl1.varyings
                                    }
                                    { extensionTypevar = webgl2.varyingsExtension
                                    , fields = webgl2.varyings
                                    }

            ( WebGLShader _, _ ) ->
                typeMismatch ()


{-| Binds an unbound typeVar root with a given monotype.
Both are already substituted.
-}
bind : UnifyConfig -> TypeVar -> MonoType -> StateM ()
bind cfg typeVar type_ =
    if shallowEqual type_ (TypeVar typeVar) then
        State.pure ()

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
        let
            ( _, super ) =
                typeVar
        in
        case type_ of
            TypeVar (( _, otherSuper ) as otherVar) ->
                case meet super otherSuper of
                    Nothing ->
                        let
                            ( pubVar, pubOther ) =
                                TypeI.toPublicPair cfg.moduleMapping (TypeVar typeVar) type_
                        in
                        State.error
                            { moduleName = FullModuleName.toModuleName cfg.moduleName
                            , declarationNames = cfg.declarationNames
                            , details = ConstraintMismatch pubVar pubOther
                            }

                    Just m ->
                        if m == super && m == otherSuper then
                            -- Either could be chosen as then parent (linked to),
                            -- but we prefer Generated ids as they can't collide.
                            State.modifySubst <|
                                case ( Tuple.first typeVar, Tuple.first otherVar ) of
                                    ( Named _, Generated _ ) ->
                                        SubstitutionMap.linkTo { child = typeVar, parent = otherVar }

                                    ( Generated _, Named _ ) ->
                                        SubstitutionMap.linkTo { child = otherVar, parent = typeVar }

                                    _ ->
                                        SubstitutionMap.union typeVar otherVar

                        else if m == otherSuper then
                            -- otherVar is more constrained -> it will be the `parent` representative.
                            State.modifySubst (SubstitutionMap.linkTo { child = typeVar, parent = otherVar })

                        else if m == super then
                            State.modifySubst (SubstitutionMap.linkTo { child = otherVar, parent = typeVar })

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

            _ ->
                if accepts cfg.typeAliases super type_ then
                    State.modifySubst (SubstitutionMap.bindRoot typeVar type_)

                else
                    let
                        ( pubVar, pubType ) =
                            TypeI.toPublicPair cfg.moduleMapping (TypeVar typeVar) type_
                    in
                    State.error
                        { moduleName = FullModuleName.toModuleName cfg.moduleName
                        , declarationNames = cfg.declarationNames
                        , details = ConstraintMismatch pubVar pubType
                        }


{-| The most specific supertype that satisfies both constraints, if any.
-}
meet : SuperType -> SuperType -> Maybe SuperType
meet a b =
    if a == b then
        Just a

    else
        case ( a, b ) of
            ( Normal, other ) ->
                Just other

            ( other, Normal ) ->
                Just other

            ( Number, Comparable ) ->
                Just Number

            ( Comparable, Number ) ->
                Just Number

            ( Comparable, Appendable ) ->
                Just CompAppend

            ( Appendable, Comparable ) ->
                Just CompAppend

            ( Comparable, CompAppend ) ->
                Just CompAppend

            ( CompAppend, Comparable ) ->
                Just CompAppend

            ( Appendable, CompAppend ) ->
                Just CompAppend

            ( CompAppend, Appendable ) ->
                Just CompAppend

            ( Number, CompAppend ) ->
                Nothing

            ( CompAppend, Number ) ->
                Nothing

            ( Number, Appendable ) ->
                Nothing

            ( Appendable, Number ) ->
                Nothing

            -- The a == b guard above makes these diagonal branches unreachable
            -- but let's not use wildcards anyways
            ( Number, Number ) ->
                Just a

            ( Comparable, Comparable ) ->
                Just a

            ( Appendable, Appendable ) ->
                Just a

            ( CompAppend, CompAppend ) ->
                Just a


accepts : TypeAliases -> SuperType -> MonoType -> Bool
accepts typeAliases super type_ =
    case super of
        Normal ->
            True

        Number ->
            case expandAlias typeAliases type_ of
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
    let
        inFields : Dict VarName MonoType -> Bool
        inFields fields =
            Dict.Extra.any (\_ valueType -> occursCheck typeVar valueType) fields
    in
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

        Record { fields } ->
            inFields fields

        ExtensibleRecord r ->
            occursCheck typeVar r.extensionTypevar || inFields r.fields

        UserDefinedType r ->
            List.any (\arg -> occursCheck typeVar arg) r.args

        WebGLShader r ->
            occursCheck typeVar r.attributesExtension
                || inFields r.attributes
                || occursCheck typeVar r.uniformsExtension
                || inFields r.uniforms
                || occursCheck typeVar r.varyingsExtension
                || inFields r.varyings
