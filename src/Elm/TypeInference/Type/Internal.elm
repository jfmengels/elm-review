module Elm.TypeInference.Type.Internal exposing
    ( Id
    , MonoType(..)
    , Type(..)
    , TypeResolver
    , closeOver
    , collapseExtensible
    , collapsePrimitive
    , external
    , fromTypeAnnotation
    , fromTypeAnnotationError
    , id_
    , mapVarsMono
    , mono
    , monoPublicKey
    , monoTypeVars
    , number_
    , renameToAnnotation
    , toPublicPair
    , toPublicType
    )

import Dict exposing (Dict)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.TypeInference.Error exposing (ErrorDetails(..))
import Elm.TypeInference.Error.Internal exposing (FromTypeAnnotationError(..), ResolverAmbiguity)
import Elm.TypeInference.ImplicitImports as ImplicitImports
import Elm.TypeInference.ModuleIds as ModuleIds exposing (ModuleId)
import Elm.TypeInference.Type as Public exposing (PackageName, Type, VarName)
import Elm.TypeInference.TypeVar as TypeVar
    exposing
        ( SuperType(..)
        , TypeVar
        , TypeVarStyle(..)
        )
import Elm.TypeInference.VarSet as VarSet
    exposing
        ( VarSet
        , superTypeTag
        )
import Result.Extra
import Set exposing (Set)


type alias Id =
    Int


type alias TypeResolver =
    List String -> String -> Result ResolverAmbiguity ( PackageName, ModuleId )


id_ : Id -> MonoType
id_ theId =
    TypeVar ( Generated theId, Normal )


number_ : Id -> MonoType
number_ theId =
    TypeVar ( Generated theId, Number )


{-| A more truthful / detailed representation of types.
For example, it deals with type schemes (these `forall`s)!

These are mainly helpful for let polymorphism and not much more. Stupid feature.
It brings baggage like generalization and instantiation, so that your
`id : x -> x` can be used for two calls `(id 0, id "x")` separately without
throwing an error that type of 0 !== type of "x".

-}
type Type
    = Forall (List TypeVar) MonoType


{-| At least monotypes always only deal with monotypes...
-}
type MonoType
    = TypeVar TypeVar
    | Function
        { from : MonoType
        , to : MonoType
        }
    | Int
    | Float
    | Char
    | String
    | Bool
    | List MonoType
    | Unit
    | Tuple2 MonoType MonoType
    | Tuple3 MonoType MonoType MonoType
    | Record { fields : Dict VarName MonoType }
    | ExtensibleRecord
        { extensionTypevar : MonoType
        , fields : Dict VarName MonoType
        }
    | UserDefinedType
        { package : PackageName
        , moduleId : ModuleId
        , name : VarName
        , args : List MonoType
        }
    | WebGLShader
        { attributesExtension : MonoType
        , attributes : Dict VarName MonoType
        , uniformsExtension : MonoType
        , uniforms : Dict VarName MonoType
        , varyingsExtension : MonoType
        , varyings : Dict VarName MonoType
        }


external : PackageName -> ModuleId -> VarName -> MonoType
external package moduleId typeName =
    UserDefinedType
        { package = package
        , moduleId = moduleId
        , name = typeName
        , args = []
        }


mono : MonoType -> Type
mono t =
    Forall [] t


{-| Canonicalize an extensible-record chain:

  - `{ r | }` (no fields) is just `r`
  - `{ { b : Char } | a : Float }` is `{ a : Float, b : Char }`
  - `{ { s | b : Char } | a : Float }` is `{ s | a : Float, b : Char }`

Bias towards the outer fields.

-}
collapseExtensible :
    { extensionTypevar : MonoType
    , fields : Dict VarName MonoType
    }
    -> MonoType
collapseExtensible r1 =
    if Dict.isEmpty r1.fields then
        case r1.extensionTypevar of
            ExtensibleRecord extensionExtensible ->
                collapseExtensible extensionExtensible

            extension ->
                extension

    else
        case r1.extensionTypevar of
            Record r2 ->
                Record { fields = Dict.union r1.fields r2.fields }

            ExtensibleRecord r2 ->
                collapseExtensible <|
                    { extensionTypevar = r2.extensionTypevar
                    , fields = Dict.union r1.fields r2.fields
                    }

            _ ->
                ExtensibleRecord r1


{-| Converts special `UserDefinedType`s into dedicated `MonoType`s:

  - `elm/core` Int, Float, Bool, Char, String, List
  - `elm-explorations/webgl` Shader

-}
collapsePrimitive : PackageName -> ModuleId -> VarName -> List MonoType -> Maybe MonoType
collapsePrimitive package moduleId name args =
    if package == ImplicitImports.elmCorePackage then
        collapseElmCoreType moduleId name args

    else if package == webGLPackage then
        collapseWebGLShader moduleId name args

    else
        Nothing


webGLPackage : PackageName
webGLPackage =
    "elm-explorations/webgl"


collapseElmCoreType : ModuleId -> VarName -> List MonoType -> Maybe MonoType
collapseElmCoreType moduleId name args =
    case args of
        [] ->
            if moduleId == ModuleIds.basicsId then
                case name of
                    "Int" ->
                        Just Int

                    "Float" ->
                        Just Float

                    "Bool" ->
                        Just Bool

                    _ ->
                        Nothing

            else if moduleId == ModuleIds.charId && name == "Char" then
                Just Char

            else if moduleId == ModuleIds.stringId && name == "String" then
                Just String

            else
                Nothing

        [ inner ] ->
            if moduleId == ModuleIds.listId && name == "List" then
                Just (List inner)

            else
                Nothing

        _ ->
            Nothing


collapseWebGLShader : ModuleId -> VarName -> List MonoType -> Maybe MonoType
collapseWebGLShader moduleId name args =
    if moduleId == ModuleIds.webGLId && name == "Shader" then
        case args of
            [ attributes, uniforms, varyings ] ->
                Maybe.map3 makeWebGLShader
                    (shaderSetSlot attributes)
                    (shaderSetSlot uniforms)
                    (shaderSetSlot varyings)

            _ ->
                Nothing

    else
        Nothing


{-| WebGL Shader typevars can be of three shapes:

  - `{ position : Vec3 }`
  - `{ attributes | position : Vec3 }`
  - `a`

Anythign else, we return Nothing and let downstream code report a mismatch.

-}
shaderSetSlot : MonoType -> Maybe ( MonoType, Dict VarName MonoType )
shaderSetSlot arg =
    case arg of
        Record { fields } ->
            Just ( Record { fields = Dict.empty }, fields )

        ExtensibleRecord er ->
            Just ( er.extensionTypevar, er.fields )

        TypeVar v ->
            Just ( TypeVar v, Dict.empty )

        _ ->
            Nothing


makeWebGLShader :
    ( MonoType, Dict VarName MonoType )
    -> ( MonoType, Dict VarName MonoType )
    -> ( MonoType, Dict VarName MonoType )
    -> MonoType
makeWebGLShader ( attributesExtension, attributes ) ( uniformsExtension, uniforms ) ( varyingsExtension, varyings ) =
    WebGLShader
        { attributesExtension = attributesExtension
        , attributes = attributes
        , uniformsExtension = uniformsExtension
        , uniforms = uniforms
        , varyingsExtension = varyingsExtension
        , varyings = varyings
        }



-- RECURSION HELPERS


{-| Apply `f` to the direct children of a type, keeping the type's shape.
-}
recurse : (MonoType -> MonoType) -> MonoType -> MonoType
recurse f type_ =
    case type_ of
        TypeVar _ ->
            type_

        Function { from, to } ->
            Function
                { from = f from
                , to = f to
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
            List <| f listItemType

        Unit ->
            type_

        Tuple2 t1 t2 ->
            Tuple2 (f t1) (f t2)

        Tuple3 t1 t2 t3 ->
            Tuple3 (f t1) (f t2) (f t3)

        Record { fields } ->
            Record { fields = Dict.map (\_ value -> f value) fields }

        ExtensibleRecord r ->
            ExtensibleRecord
                { extensionTypevar = f r.extensionTypevar
                , fields = Dict.map (\_ value -> f value) r.fields
                }

        UserDefinedType r ->
            UserDefinedType
                { package = r.package
                , moduleId = r.moduleId
                , name = r.name
                , args = List.map f r.args
                }

        WebGLShader r ->
            WebGLShader
                { attributesExtension = f r.attributesExtension
                , attributes = Dict.map (\_ t -> f t) r.attributes
                , uniformsExtension = f r.uniformsExtension
                , uniforms = Dict.map (\_ t -> f t) r.uniforms
                , varyingsExtension = f r.varyingsExtension
                , varyings = Dict.map (\_ t -> f t) r.varyings
                }


{-| Collect every type variable occurring in a monotype.

Returns them backwards to later insert into VarSet backwards,
so that they're there in order of first appearance,
SO THAT `normalize` can give us `a -> b -> a` instead of `b -> a -> b`.

Used to decide which variables to quantify in `generalize` (those not already
free in the environment) and in `State.generalize` (those above the current
let-rank).

-}
monoTypeVars : MonoType -> List TypeVar
monoTypeVars type_ =
    monoTypeVarsHelp type_ []


monoTypeVarsHelp : MonoType -> List TypeVar -> List TypeVar
monoTypeVarsHelp type_ acc =
    case type_ of
        TypeVar typeVar ->
            typeVar :: acc

        Function { from, to } ->
            acc
                |> monoTypeVarsHelp to
                |> monoTypeVarsHelp from

        Int ->
            acc

        Float ->
            acc

        Char ->
            acc

        String ->
            acc

        Bool ->
            acc

        List listItemType ->
            monoTypeVarsHelp listItemType acc

        Unit ->
            acc

        Tuple2 t1 t2 ->
            acc
                |> monoTypeVarsHelp t2
                |> monoTypeVarsHelp t1

        Tuple3 t1 t2 t3 ->
            acc
                |> monoTypeVarsHelp t3
                |> monoTypeVarsHelp t2
                |> monoTypeVarsHelp t1

        Record { fields } ->
            monoTypeVarsInFieldsHelp fields acc

        ExtensibleRecord r ->
            acc
                |> monoTypeVarsInFieldsHelp r.fields
                |> monoTypeVarsHelp r.extensionTypevar

        UserDefinedType r ->
            List.foldr monoTypeVarsHelp acc r.args

        WebGLShader r ->
            acc
                |> monoTypeVarsInFieldsHelp r.varyings
                |> monoTypeVarsHelp r.varyingsExtension
                |> monoTypeVarsInFieldsHelp r.uniforms
                |> monoTypeVarsHelp r.uniformsExtension
                |> monoTypeVarsInFieldsHelp r.attributes
                |> monoTypeVarsHelp r.attributesExtension


monoTypeVarsInFieldsHelp : Dict VarName MonoType -> List TypeVar -> List TypeVar
monoTypeVarsInFieldsHelp fields acc_ =
    Dict.foldr (\_ fieldType accAcrossFields -> monoTypeVarsHelp fieldType accAcrossFields) acc_ fields


{-|

     a -> List b
     --> Forall [ a, b ] (a -> List b)

Useful for types that can't interact with a lexical environment, eg. type
annotations, constructors, aliases, ports, dependency types.

Global environment is supposed to only ever hold closed schemes (no free
typevars), for State.lookupGlobalEnv to be able to instantiate them directly
without substitution.

Note that State.generalize (used for let-bound locals) uses let-rank to
decide what to close over.

-}
closeOver : MonoType -> Type
closeOver monoType =
    monoType
        |> generalize VarSet.empty


{-| Put bound vars into the Forall.

    generalize {a} (a -> b)
    --> Forall [b] (a -> b)

Meaning `a` stays free (belongs to the environment) but `b` is bound.

-}
generalize : VarSet -> MonoType -> Type
generalize envFreeVars monoType =
    let
        boundIds : List TypeVar
        boundIds =
            VarSet.diff
                (monoTypeVars monoType)
                envFreeVars
                |> VarSet.toList
    in
    Forall boundIds monoType


{-| Rename IDs to be as minimal as possible.

     a -> x
     --> a -> b

     b -> c -> b
     --> a -> b -> a

     number3 -> number4
     --> number -> number1

-}
normalize : Type -> Type
normalize ((Forall boundVars monoType) as type_) =
    let
        allVars : List TypeVar
        allVars =
            (VarSet.fromList boundVars).order
                ++ monoTypeVars monoType
                |> VarSet.toList

        -- eg. `number` and `comparable` get their own slot sequence independent of the `Normal` one
        usedNamesBySuper : Dict Int (Set String)
        usedNamesBySuper =
            allVars
                |> List.foldl
                    (\( style, super ) acc ->
                        case style of
                            Named name ->
                                Dict.update
                                    (superTypeTag super)
                                    (\existing -> Just (Set.insert name (Maybe.withDefault Set.empty existing)))
                                    acc

                            Generated _ ->
                                acc
                    )
                    Dict.empty

        -- slot 0 is the bare word ("a", or "" for supertypes -> just "number");
        -- slot >= 1 is "b", "c", ... or "1", "2", ... for supertypes.
        nameForSlot : SuperType -> Int -> String
        nameForSlot super slot =
            case super of
                Normal ->
                    ordToName slot

                _ ->
                    if slot == 0 then
                        ""

                    else
                        String.fromInt slot

        nextFreeSlot : SuperType -> Int -> Int
        nextFreeSlot super slot =
            let
                used : Set String
                used =
                    Dict.get (superTypeTag super) usedNamesBySuper
                        |> Maybe.withDefault Set.empty
            in
            if Set.member (nameForSlot super slot) used then
                nextFreeSlot super (slot + 1)

            else
                slot

        newVars : List TypeVar
        newVars =
            allVars
                |> List.foldl
                    (\(( style, super ) as var) ( nextSlotBySuper, acc ) ->
                        case style of
                            Named _ ->
                                -- Leave it exactly as it is.
                                ( nextSlotBySuper, var :: acc )

                            Generated _ ->
                                let
                                    key : Int
                                    key =
                                        superTypeTag super

                                    startSlot : Int
                                    startSlot =
                                        Dict.get key nextSlotBySuper |> Maybe.withDefault 0

                                    slot : Int
                                    slot =
                                        nextFreeSlot super startSlot
                                in
                                ( Dict.insert key (slot + 1) nextSlotBySuper
                                , ( Named (nameForSlot super slot), super ) :: acc
                                )
                    )
                    ( Dict.empty, [] )
                |> (\( _, vars ) -> List.reverse vars)

        ( substGen, substNamed ) =
            List.map2 Tuple.pair allVars newVars
                |> List.foldl
                    (\( ( style, super ), newVar ) ( genAcc, namedAcc ) ->
                        case style of
                            Generated theId ->
                                ( Dict.insert (VarSet.genKeyFrom theId super) newVar genAcc
                                , namedAcc
                                )

                            Named name ->
                                ( genAcc
                                , Dict.insert (VarSet.namedKeyFrom name super) newVar namedAcc
                                )
                    )
                    ( Dict.empty, Dict.empty )
    in
    type_
        |> mapVars
            (\(( style, super ) as var) ->
                case style of
                    Generated theId ->
                        case Dict.get (VarSet.genKeyFrom theId super) substGen of
                            Nothing ->
                                var

                            Just newVar ->
                                newVar

                    Named name ->
                        case Dict.get (VarSet.namedKeyFrom name super) substNamed of
                            Nothing ->
                                var

                            Just newVar ->
                                newVar
            )


mapVars : (TypeVar -> TypeVar) -> Type -> Type
mapVars fn (Forall boundVars monoType) =
    Forall (List.map fn boundVars) (mapVarsMono fn monoType)


{-| Map every var **once**, simultaneously, without chain-following.

This atomicity is important for instantiation; two overlapping ID spaces could
interact weirdly otherwise.

-}
mapVarsMono : (TypeVar -> TypeVar) -> MonoType -> MonoType
mapVarsMono fn type_ =
    case type_ of
        TypeVar var ->
            TypeVar (fn var)

        _ ->
            recurse (\child -> child |> mapVarsMono fn) type_


{-|

    0 -> a
    1 -> b
    25 -> z
    26 -> aa
    27 -> ab

-}
ordToName : Int -> String
ordToName n =
    let
        radix : Int
        radix =
            26

        go : Int -> String
        go i =
            if i < radix then
                String.fromChar <| charFromInt i

            else
                go ((i // radix) - 1) ++ (String.fromChar <| charFromInt (modBy radix i))
    in
    go n


{-| The functions below are stolen from fredcy/elm-parseint and tweaked
to work similar to:

<https://en.wikipedia.org/wiki/Bijective_numeration#The_bijective_base-26_system>

-}
charFromInt : Int -> Char
charFromInt i =
    Char.fromCode <| i + Char.toCode 'a'


fromTypeAnnotation : TypeResolver -> TypeAnnotation -> Result FromTypeAnnotationError MonoType
fromTypeAnnotation resolver typeAnnotation =
    let
        f : TypeAnnotation -> Result FromTypeAnnotationError MonoType
        f annotation =
            fromTypeAnnotation resolver annotation

        recordBindings :
            List (Node ( Node String, Node TypeAnnotation ))
            -> Result FromTypeAnnotationError (Dict VarName MonoType)
        recordBindings fields =
            fields
                |> Result.Extra.foldlWhileOk
                    (\fieldNode acc ->
                        let
                            ( fieldNameNode, annotationNode ) =
                                Node.value fieldNode

                            type_ : Result FromTypeAnnotationError MonoType
                            type_ =
                                f (Node.value annotationNode)
                        in
                        type_
                            |> Result.map (\type__ -> Dict.insert (Node.value fieldNameNode) type__ acc)
                    )
                    Dict.empty
    in
    case typeAnnotation of
        TypeAnnotation.GenericType name ->
            Ok <| TypeVar (TypeVar.parse name)

        TypeAnnotation.Typed name annotations ->
            let
                args : Result FromTypeAnnotationError (List MonoType)
                args =
                    annotations
                        |> Result.Extra.combineMap (\(Node.Node _ arg) -> f arg)
            in
            -- Resolve names before collapsing primitives: local or imported
            -- types can shadow implicit names such as List, Int, and String.
            args
                |> Result.andThen
                    (\args_ ->
                        let
                            ( moduleName, typeName ) =
                                Node.value name
                        in
                        resolver moduleName typeName
                            |> Result.mapError AmbiguousModuleName
                            |> Result.map
                                (\( package, moduleId ) ->
                                    case collapsePrimitive package moduleId typeName args_ of
                                        Just collapsed ->
                                            collapsed

                                        Nothing ->
                                            UserDefinedType
                                                { package = package
                                                , moduleId = moduleId
                                                , name = typeName
                                                , args = args_
                                                }
                                )
                    )

        TypeAnnotation.Unit ->
            Ok Unit

        TypeAnnotation.Tupled [ a, b ] ->
            Result.map2 Tuple2
                (f (Node.value a))
                (f (Node.value b))

        TypeAnnotation.Tupled [ a, b, c ] ->
            Result.map3 Tuple3
                (f (Node.value a))
                (f (Node.value b))
                (f (Node.value c))

        TypeAnnotation.Tupled _ ->
            Err (ImpossibleAnnotation typeAnnotation)

        TypeAnnotation.Record fields ->
            recordBindings fields
                |> Result.map (\fields_ -> Record { fields = fields_ })

        TypeAnnotation.GenericRecord name fields ->
            recordBindings (Node.value fields)
                |> Result.map
                    (\fields_ ->
                        ExtensibleRecord
                            { extensionTypevar = TypeVar (TypeVar.parse (Node.value name))
                            , fields = fields_
                            }
                    )

        TypeAnnotation.FunctionTypeAnnotation from to ->
            Result.map2
                (\from_ to_ ->
                    Function
                        { from = from_
                        , to = to_
                        }
                )
                (f (Node.value from))
                (f (Node.value to))


{-| Convert a type-annotation conversion failure into an inference error.
-}
fromTypeAnnotationError : FromTypeAnnotationError -> ErrorDetails
fromTypeAnnotationError err =
    case err of
        ImpossibleAnnotation typeAnnotation ->
            ImpossibleType typeAnnotation

        AmbiguousModuleName ambiguity ->
            AmbiguousModuleOwner ambiguity


toPublicType : ModuleIds.Mapping -> { alreadyNormalized : Bool } -> MonoType -> Public.Type
toPublicType moduleMapping { alreadyNormalized } origMono =
    let
        mono_ : MonoType
        mono_ =
            if alreadyNormalized then
                origMono

            else
                let
                    (Forall _ normalizedMono) =
                        normalize (Forall [] origMono)
                in
                normalizedMono
    in
    toPublicTypeNormalized moduleMapping mono_


moduleIdToModuleName : ModuleIds.Mapping -> ModuleId -> List String
moduleIdToModuleName moduleMapping moduleId =
    ModuleIds.moduleNameForDisplay moduleId moduleMapping


{-| Convert two `MonoType`s to public `Type`s with a shared normalization.

Normalizing each side independently would name distinct variables identically
(`a` on both sides) and suggest sharing where there is none, or rename a
shared variable differently on each side.

Used for type errors, where types come in pairs.

-}
toPublicPair : ModuleIds.Mapping -> MonoType -> MonoType -> ( Public.Type, Public.Type )
toPublicPair moduleMapping t1 t2 =
    let
        (Forall _ normalizedCombined) =
            normalize (Forall [] (Tuple2 t1 t2))
    in
    case normalizedCombined of
        Tuple2 nt1 nt2 ->
            ( toPublicType moduleMapping { alreadyNormalized = True } nt1
            , toPublicType moduleMapping { alreadyNormalized = True } nt2
            )

        _ ->
            -- Shouldn't happen
            ( toPublicType moduleMapping { alreadyNormalized = False } t1
            , toPublicType moduleMapping { alreadyNormalized = False } t2
            )


toPublicTypeNormalized : ModuleIds.Mapping -> MonoType -> Public.Type
toPublicTypeNormalized moduleMapping mono_ =
    case mono_ of
        TypeVar typeVar ->
            Public.TypeVar (TypeVar.toString typeVar)

        Function { from, to } ->
            Public.Function
                { from = toPublicType moduleMapping { alreadyNormalized = True } from
                , to = toPublicType moduleMapping { alreadyNormalized = True } to
                }

        Int ->
            Public.Int

        Float ->
            Public.Float

        Char ->
            Public.Char

        String ->
            Public.String

        Bool ->
            Public.Bool

        List ts ->
            Public.List (toPublicType moduleMapping { alreadyNormalized = True } ts)

        Unit ->
            Public.Unit

        Tuple2 t1 t2 ->
            Public.Tuple2
                (toPublicType moduleMapping { alreadyNormalized = True } t1)
                (toPublicType moduleMapping { alreadyNormalized = True } t2)

        Tuple3 t1 t2 t3 ->
            Public.Tuple3
                (toPublicType moduleMapping { alreadyNormalized = True } t1)
                (toPublicType moduleMapping { alreadyNormalized = True } t2)
                (toPublicType moduleMapping { alreadyNormalized = True } t3)

        Record { fields } ->
            Public.Record { fields = Dict.map (\_ v -> toPublicType moduleMapping { alreadyNormalized = True } v) fields }

        ExtensibleRecord extensibleRecordUncollapsed ->
            case collapseExtensible extensibleRecordUncollapsed of
                ExtensibleRecord { extensionTypevar, fields } ->
                    Public.ExtensibleRecord
                        { extensionTypevar =
                            case
                                extensionTypevar
                            of
                                TypeVar var ->
                                    TypeVar.toString var

                                _ ->
                                    -- Should be impossible to trigger for users of the
                                    -- library, as they don't have access to MonoType
                                    -- constructors.
                                    "<elm-syntax-type-inference bug: non-var as extensible record base>"
                        , fields = fields |> Dict.map (\_ v -> toPublicType moduleMapping { alreadyNormalized = True } v)
                        }

                collapsed ->
                    toPublicTypeNormalized moduleMapping collapsed

        UserDefinedType r ->
            Public.Named
                { package = r.package
                , moduleName = moduleIdToModuleName moduleMapping r.moduleId
                , name = r.name
                , arguments = List.map (\arg -> toPublicType moduleMapping { alreadyNormalized = True } arg) r.args
                }

        WebGLShader r ->
            let
                ( attributesFields, attributesExtensionTypevar ) =
                    shaderSlotToPublic (\t -> toPublicType moduleMapping { alreadyNormalized = True } t) r.attributesExtension r.attributes

                ( uniformsFields, uniformsExtensionTypevar ) =
                    shaderSlotToPublic (\t -> toPublicType moduleMapping { alreadyNormalized = True } t) r.uniformsExtension r.uniforms

                ( varyingsFields, varyingsExtensionTypevar ) =
                    shaderSlotToPublic (\t -> toPublicType moduleMapping { alreadyNormalized = True } t) r.varyingsExtension r.varyings
            in
            Public.WebGLShader
                { attributesFields = attributesFields
                , attributesExtensionTypevar = attributesExtensionTypevar
                , uniformsFields = uniformsFields
                , uniformsExtensionTypevar = uniformsExtensionTypevar
                , varyingsFields = varyingsFields
                , varyingsExtensionTypevar = varyingsExtensionTypevar
                }


shaderSlotToPublic : (MonoType -> Public.Type) -> MonoType -> Dict VarName MonoType -> ( Dict VarName Public.Type, Maybe String )
shaderSlotToPublic f extensionTypevar fields =
    case
        collapseExtensible
            { extensionTypevar = extensionTypevar
            , fields = fields
            }
    of
        Record r ->
            ( Dict.map (\_ v -> f v) r.fields
            , Nothing
            )

        TypeVar var ->
            ( Dict.empty
            , Just (TypeVar.toString var)
            )

        ExtensibleRecord r ->
            case r.extensionTypevar of
                TypeVar var ->
                    ( Dict.map (\_ v -> f v) r.fields
                    , Just (TypeVar.toString var)
                    )

                _ ->
                    -- Should be impossible to trigger for users of the
                    -- library, as they don't have access to MonoType
                    -- constructors.
                    ( Dict.map (\_ v -> f v) r.fields
                    , Just "<elm-syntax-type-inference bug: non-var as extensible record base>"
                    )

        _ ->
            -- Shouldn't happen: shader slots are always record-like.
            -- Fall back to a closed record holding nothing, to avoid crashing.
            ( Dict.empty
            , Nothing
            )


{-| Rename inferred vars to those from a type annotation.

We walk both types in parallel - we need to see a Named (in annotation) and
Generated (in inferred) var at the same time (with the same SuperType
constraint) -> then we rename.

Returns `Nothing` when the shapes don't line up. Shouldn't happen for
type-checked code.

-}
renameToAnnotation : MonoType -> MonoType -> Maybe MonoType
renameToAnnotation annoMono inferredMono =
    case collectAnnotationNames annoMono inferredMono Dict.empty of
        Nothing ->
            Nothing

        Just mapping ->
            Just (mapVarsMono (\var -> var |> applyAnnotationNames mapping) inferredMono)


applyAnnotationNames : Dict Int TypeVar -> TypeVar -> TypeVar
applyAnnotationNames mapping (( style, super ) as var) =
    case style of
        Generated theId ->
            Dict.get (VarSet.genKeyFrom theId super) mapping
                |> Maybe.withDefault var

        Named _ ->
            var


collectAnnotationNames : MonoType -> MonoType -> Dict Int TypeVar -> Maybe (Dict Int TypeVar)
collectAnnotationNames annoMono inferredMono acc =
    case ( annoMono, inferredMono ) of
        ( TypeVar ( Named annoName, annoSuper ), TypeVar ( Generated inferredId, inferredSuper ) ) ->
            if annoSuper /= inferredSuper then
                Nothing

            else
                let
                    key : Int
                    key =
                        VarSet.genKeyFrom inferredId inferredSuper

                    wanted : TypeVar
                    wanted =
                        ( Named annoName, annoSuper )
                in
                case Dict.get key acc of
                    Nothing ->
                        Just (Dict.insert key wanted acc)

                    Just existing ->
                        if existing == wanted then
                            Just acc

                        else
                            Nothing

        ( TypeVar ( Named annoName, annoSuper ), TypeVar ( Named inferredName, inferredSuper ) ) ->
            if ( Named annoName, annoSuper ) == ( Named inferredName, inferredSuper ) then
                Just acc

            else
                Nothing

        ( TypeVar _, TypeVar _ ) ->
            -- Should be impossible (annotations shouldn't contain generated vars)
            Nothing

        ( TypeVar _, _ ) ->
            Nothing

        ( _, TypeVar _ ) ->
            Nothing

        ( Function a1, Function b1 ) ->
            collectAnnotationNames a1.from b1.from acc
                |> Maybe.andThen (\a -> a |> collectAnnotationNames a1.to b1.to)

        ( List a, List b ) ->
            collectAnnotationNames a b acc

        ( Tuple2 a1 a2, Tuple2 b1 b2 ) ->
            collectAnnotationNames a1 b1 acc
                |> Maybe.andThen (\a -> a |> collectAnnotationNames a2 b2)

        ( Tuple3 a1 a2 a3, Tuple3 b1 b2 b3 ) ->
            collectAnnotationNames a1 b1 acc
                |> Maybe.andThen (\a -> a |> collectAnnotationNames a2 b2)
                |> Maybe.andThen (\a -> a |> collectAnnotationNames a3 b3)

        ( Record r1, Record r2 ) ->
            collectRecordFields r1.fields r2.fields acc

        ( ExtensibleRecord r1Uncollapsed, ExtensibleRecord r2Uncollapsed ) ->
            case ( collapseExtensible r1Uncollapsed, collapseExtensible r2Uncollapsed ) of
                ( ExtensibleRecord r1, ExtensibleRecord r2 ) ->
                    collectAnnotationNames r1.extensionTypevar r2.extensionTypevar acc
                        |> Maybe.andThen (\a -> a |> collectRecordFields r1.fields r2.fields)

                ( r1, r2 ) ->
                    collectAnnotationNames r1 r2 acc

        ( UserDefinedType u1, UserDefinedType u2 ) ->
            if u1.package /= u2.package || u1.moduleId /= u2.moduleId || u1.name /= u2.name then
                Nothing

            else
                collectAnnotationArgs u1.args u2.args acc

        ( WebGLShader s1, WebGLShader s2 ) ->
            collectAnnotationNames s1.attributesExtension s2.attributesExtension acc
                |> Maybe.andThen (\a -> a |> collectRecordFields s1.attributes s2.attributes)
                |> Maybe.andThen (\a -> a |> collectAnnotationNames s1.uniformsExtension s2.uniformsExtension)
                |> Maybe.andThen (\a -> a |> collectRecordFields s1.uniforms s2.uniforms)
                |> Maybe.andThen (\a -> a |> collectAnnotationNames s1.varyingsExtension s2.varyingsExtension)
                |> Maybe.andThen (\a -> a |> collectRecordFields s1.varyings s2.varyings)

        ( Int, Int ) ->
            Just acc

        ( Float, Float ) ->
            Just acc

        ( Char, Char ) ->
            Just acc

        ( String, String ) ->
            Just acc

        ( Bool, Bool ) ->
            Just acc

        ( Unit, Unit ) ->
            Just acc

        _ ->
            Nothing


collectRecordFields : Dict VarName MonoType -> Dict VarName MonoType -> Dict Int TypeVar -> Maybe (Dict Int TypeVar)
collectRecordFields fields1 fields2 acc =
    Dict.merge
        (\_ _ _ -> Nothing)
        (\_ annoField inferredField maybeAccAcrossFields ->
            case maybeAccAcrossFields of
                Nothing ->
                    Nothing

                Just accAcrossFields ->
                    case collectAnnotationNames annoField inferredField accAcrossFields of
                        Nothing ->
                            Nothing

                        Just acc1 ->
                            Just acc1
        )
        (\_ _ _ -> Nothing)
        fields1
        fields2
        (Just acc)


collectAnnotationArgs : List MonoType -> List MonoType -> Dict Int TypeVar -> Maybe (Dict Int TypeVar)
collectAnnotationArgs annos inferreds acc =
    case annos of
        [] ->
            case inferreds of
                [] ->
                    Just acc

                _ :: _ ->
                    Nothing

        a :: restA ->
            case inferreds of
                b :: restB ->
                    case collectAnnotationNames a b acc of
                        Nothing ->
                            Nothing

                        Just acc1 ->
                            collectAnnotationArgs restA restB acc1

                [] ->
                    Nothing


{-| A deduplication key for a normalized monotype inside a single `TypeLookupTable`.
-}
monoPublicKey : { alreadyNormalized : Bool } -> MonoType -> String
monoPublicKey { alreadyNormalized } origMono =
    if alreadyNormalized then
        monoPublicKeyNormalized origMono

    else
        monoPublicKeyAlpha origMono


{-| Alpha-equivalence deduplication key.
-}
monoPublicKeyAlpha : MonoType -> String
monoPublicKeyAlpha mono_ =
    Tuple.first
        (monoPublicKeyAlphaHelp
            (case mono_ of
                ExtensibleRecord extensibleRecord ->
                    collapseExtensible extensibleRecord

                notExtensibleRecord ->
                    notExtensibleRecord
            )
            alphaStateEmpty
        )


alphaStateEmpty : AlphaState
alphaStateEmpty =
    { next = 0, mapping = Dict.empty }


type alias AlphaState =
    { next : Int
    , mapping : Dict Int Int
    }


superTagString : SuperType -> String
superTagString super =
    case super of
        Normal ->
            "0"

        Number ->
            "1"

        Comparable ->
            "2"

        Appendable ->
            "3"

        CompAppend ->
            "4"


alphaVarCode : TypeVar -> AlphaState -> ( String, AlphaState )
alphaVarCode ( style, super ) state =
    case style of
        Named name ->
            ( "n" ++ superTagString super ++ ";" ++ strKey name
            , state
            )

        Generated theId ->
            let
                k : Int
                k =
                    theId * 5 + superTypeTag super
            in
            case Dict.get k state.mapping of
                Just i ->
                    ( "g" ++ superTagString super ++ ";" ++ String.fromInt i ++ ";"
                    , state
                    )

                Nothing ->
                    let
                        i : Int
                        i =
                            state.next
                    in
                    ( "g" ++ superTagString super ++ ";" ++ String.fromInt i ++ ";"
                    , { next = i + 1, mapping = Dict.insert k i state.mapping }
                    )


monoPublicKeyAlphaHelp : MonoType -> AlphaState -> ( String, AlphaState )
monoPublicKeyAlphaHelp mono_ state =
    case mono_ of
        TypeVar var ->
            let
                ( code, state1 ) =
                    alphaVarCode var state
            in
            ( "0;" ++ strKey code, state1 )

        Function { from, to } ->
            let
                ( k1, s1 ) =
                    monoPublicKeyAlphaHelp from state

                ( k2, s2 ) =
                    monoPublicKeyAlphaHelp to s1
            in
            ( "1;" ++ strKey k1 ++ strKey k2, s2 )

        Int ->
            ( "2;", state )

        Float ->
            ( "3;", state )

        Char ->
            ( "4;", state )

        String ->
            ( "5;", state )

        Bool ->
            ( "6;", state )

        List inner ->
            let
                ( k, s1 ) =
                    monoPublicKeyAlphaHelp inner state
            in
            ( "7;" ++ strKey k, s1 )

        Unit ->
            ( "8;", state )

        Tuple2 t1 t2 ->
            let
                ( k1, s1 ) =
                    monoPublicKeyAlphaHelp t1 state

                ( k2, s2 ) =
                    monoPublicKeyAlphaHelp t2 s1
            in
            ( "9;" ++ strKey k1 ++ strKey k2, s2 )

        Tuple3 t1 t2 t3 ->
            let
                ( k1, s1 ) =
                    monoPublicKeyAlphaHelp t1 state

                ( k2, s2 ) =
                    monoPublicKeyAlphaHelp t2 s1

                ( k3, s3 ) =
                    monoPublicKeyAlphaHelp t3 s2
            in
            ( "10;" ++ strKey k1 ++ strKey k2 ++ strKey k3, s3 )

        Record { fields } ->
            let
                ( rk, s1 ) =
                    recordKeyAlpha fields state
            in
            ( "11;" ++ strKey rk, s1 )

        ExtensibleRecord extensibleRecordUncollapsed ->
            case collapseExtensible extensibleRecordUncollapsed of
                ExtensibleRecord { extensionTypevar, fields } ->
                    let
                        ( ek, s1 ) =
                            extNameAlpha extensionTypevar state

                        ( rk, s2 ) =
                            recordKeyAlpha fields s1
                    in
                    ( "12;" ++ strKey ek ++ strKey rk, s2 )

                collapsed ->
                    monoPublicKeyAlphaHelp collapsed state

        UserDefinedType r ->
            let
                ( ak, s1 ) =
                    argsKeyAlpha r.args state
            in
            ( "13;"
                ++ strKey r.package
                ++ strKey (String.fromInt r.moduleId)
                ++ strKey r.name
                ++ strKey ak
            , s1
            )

        WebGLShader r ->
            let
                ( a, s1 ) =
                    shaderSlotKeyAlpha r.attributesExtension r.attributes state

                ( b, s2 ) =
                    shaderSlotKeyAlpha r.uniformsExtension r.uniforms s1

                ( c, s3 ) =
                    shaderSlotKeyAlpha r.varyingsExtension r.varyings s2
            in
            ( "14;" ++ a ++ b ++ c, s3 )


recordKeyAlpha : Dict VarName MonoType -> AlphaState -> ( String, AlphaState )
recordKeyAlpha fields state =
    let
        step : VarName -> MonoType -> ( String, AlphaState ) -> ( String, AlphaState )
        step k v ( acc, st ) =
            let
                ( vk, st2 ) =
                    monoPublicKeyAlphaHelp v st
            in
            ( acc ++ (strKey k ++ strKey vk), st2 )

        ( partsConcatenated, finalState ) =
            Dict.foldl step ( "", state ) fields
    in
    ( String.fromInt (Dict.size fields) ++ ";" ++ partsConcatenated
    , finalState
    )


argsKeyAlpha : List MonoType -> AlphaState -> ( String, AlphaState )
argsKeyAlpha args state =
    let
        go : List MonoType -> AlphaState -> String -> ( String, AlphaState )
        go remaining st acc =
            case remaining of
                [] ->
                    ( acc, st )

                a :: rest ->
                    let
                        ( ak, st2 ) =
                            monoPublicKeyAlphaHelp a st
                    in
                    go rest st2 (acc ++ strKey ak)

        ( partsConcatenated, finalState ) =
            go args state ""
    in
    ( String.fromInt (List.length args) ++ ";" ++ partsConcatenated
    , finalState
    )


extNameAlpha : MonoType -> AlphaState -> ( String, AlphaState )
extNameAlpha extensionTypevar state =
    case extensionTypevar of
        TypeVar var ->
            alphaVarCode var state

        _ ->
            ( "<elm-syntax-type-inference bug: non-var as extensible record base>"
            , state
            )


shaderSlotKeyAlpha : MonoType -> Dict VarName MonoType -> AlphaState -> ( String, AlphaState )
shaderSlotKeyAlpha extensionTypevar fields state =
    case
        collapseExtensible
            { extensionTypevar = extensionTypevar
            , fields = fields
            }
    of
        Record r ->
            let
                ( rk, s1 ) =
                    recordKeyAlpha r.fields state
            in
            ( strKey rk ++ maybeStrKey Nothing, s1 )

        TypeVar var ->
            let
                ( vc, s1 ) =
                    alphaVarCode var state
            in
            ( strKey "0;" ++ maybeStrKey (Just vc), s1 )

        ExtensibleRecord r ->
            let
                ( rk, s1 ) =
                    recordKeyAlpha r.fields state

                ( ek, s2 ) =
                    extNameAlpha r.extensionTypevar s1
            in
            ( strKey rk ++ maybeStrKey (Just ek), s2 )

        _ ->
            ( strKey "0;" ++ maybeStrKey Nothing, state )


monoPublicKeyNormalized : MonoType -> String
monoPublicKeyNormalized mono_ =
    case mono_ of
        TypeVar typeVar ->
            "0;" ++ strKey (TypeVar.toString typeVar)

        Function { from, to } ->
            "1;"
                ++ strKey (monoPublicKeyNormalized from)
                ++ strKey (monoPublicKeyNormalized to)

        Int ->
            "2;"

        Float ->
            "3;"

        Char ->
            "4;"

        String ->
            "5;"

        Bool ->
            "6;"

        List inner ->
            "7;" ++ strKey (monoPublicKeyNormalized inner)

        Unit ->
            "8;"

        Tuple2 t1 t2 ->
            "9;"
                ++ strKey (monoPublicKeyNormalized t1)
                ++ strKey (monoPublicKeyNormalized t2)

        Tuple3 t1 t2 t3 ->
            "10;"
                ++ strKey (monoPublicKeyNormalized t1)
                ++ strKey (monoPublicKeyNormalized t2)
                ++ strKey (monoPublicKeyNormalized t3)

        Record { fields } ->
            "11;" ++ strKey (recordKeyOf fields)

        ExtensibleRecord extensibleRecordNotCollapsed ->
            case collapseExtensible extensibleRecordNotCollapsed of
                ExtensibleRecord { extensionTypevar, fields } ->
                    "12;"
                        ++ strKey (extNameOf extensionTypevar)
                        ++ strKey (recordKeyOf fields)

                collapsed ->
                    monoPublicKeyNormalized collapsed

        UserDefinedType r ->
            "13;"
                ++ strKey r.package
                ++ strKey (String.fromInt r.moduleId)
                ++ strKey r.name
                ++ strKey (argsKeyOf r.args)

        WebGLShader r ->
            "14;"
                ++ shaderSlotKey r.attributesExtension r.attributes
                ++ shaderSlotKey r.uniformsExtension r.uniforms
                ++ shaderSlotKey r.varyingsExtension r.varyings


extNameOf : MonoType -> String
extNameOf extensionTypevar =
    case extensionTypevar of
        TypeVar var ->
            TypeVar.toString var

        _ ->
            "<elm-syntax-type-inference bug: non-var as extensible record base>"


recordKeyOf : Dict VarName MonoType -> String
recordKeyOf fields =
    String.fromInt (Dict.size fields)
        ++ ";"
        ++ Dict.foldl
            (\k v acc -> acc ++ (strKey k ++ strKey (monoPublicKeyNormalized v)))
            ""
            fields


argsKeyOf : List MonoType -> String
argsKeyOf args =
    String.fromInt (List.length args)
        ++ ";"
        ++ List.foldl (\arg acc -> acc ++ strKey (monoPublicKeyNormalized arg)) "" args


shaderSlotKey : MonoType -> Dict VarName MonoType -> String
shaderSlotKey extensionTypevar fields =
    case
        collapseExtensible
            { extensionTypevar = extensionTypevar
            , fields = fields
            }
    of
        Record r ->
            strKey (recordKeyOf r.fields)
                ++ maybeStrKey Nothing

        TypeVar var ->
            strKey "0;"
                ++ maybeStrKey (Just (TypeVar.toString var))

        ExtensibleRecord r ->
            strKey (recordKeyOf r.fields)
                ++ maybeStrKey (Just (extNameOf r.extensionTypevar))

        _ ->
            strKey "0;"
                ++ maybeStrKey Nothing


strKey : String -> String
strKey s =
    String.fromInt (String.length s)
        ++ ":"
        ++ s


maybeStrKey : Maybe String -> String
maybeStrKey m =
    case m of
        Nothing ->
            "0;"

        Just s ->
            "1;" ++ strKey s
