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
    , monoTypeVars
    , number_
    , toPublicPair
    , toPublicType
    )

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.TypeInference.Error exposing (ErrorDetails(..))
import Elm.TypeInference.Error.Internal exposing (FromTypeAnnotationError(..), ResolverAmbiguity)
import Elm.TypeInference.ImplicitImports as ImplicitImports
import Elm.TypeInference.Type as Public exposing (PackageName, VarName)
import Elm.TypeInference.TypeVar as TypeVar
    exposing
        ( SuperType(..)
        , TypeVar
        , TypeVarStyle(..)
        )
import Elm.TypeInference.VarSet as VarSet
    exposing
        ( VarKey
        , VarSet
        , varKey
        )
import Result.Extra
import Set exposing (Set)


type alias Id =
    Int


type alias TypeResolver =
    List String -> String -> Result ResolverAmbiguity ( PackageName, FullModuleName )


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
        , moduleName : FullModuleName
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


external : PackageName -> FullModuleName -> VarName -> MonoType
external package moduleName typeName =
    UserDefinedType
        { package = package
        , moduleName = moduleName
        , name = typeName
        , args = []
        }


mono : MonoType -> Type
mono =
    Forall []


{-| Canonicalize an extensible-record chain:

  - `{ r | }` (no fields) is just `r`
  - `{ { b : Char } | a : Float }` is `{ a : Float, b : Char }`
  - `{ { s | b : Char } | a : Float }` is `{ s | a : Float, b : Char }`

Bias towards the outer fields.

-}
collapseExtensible : MonoType -> MonoType
collapseExtensible type_ =
    case type_ of
        ExtensibleRecord r1 ->
            if Dict.isEmpty r1.fields then
                collapseExtensible r1.extensionTypevar

            else
                case r1.extensionTypevar of
                    Record r2 ->
                        Record { fields = Dict.union r1.fields r2.fields }

                    ExtensibleRecord r2 ->
                        collapseExtensible <|
                            ExtensibleRecord
                                { extensionTypevar = r2.extensionTypevar
                                , fields = Dict.union r1.fields r2.fields
                                }

                    _ ->
                        type_

        _ ->
            type_


{-| Converts special `UserDefinedType`s into dedicated `MonoType`s:

  - `elm/core` Int, Float, Bool, Char, String, List
  - `elm-explorations/webgl` Shader

-}
collapsePrimitive : PackageName -> FullModuleName -> VarName -> List MonoType -> Maybe MonoType
collapsePrimitive package moduleName name args =
    let
        moduleNameStr : String
        moduleNameStr =
            FullModuleName.toString moduleName
    in
    if package == ImplicitImports.elmCorePackage then
        collapseElmCoreType moduleNameStr name args

    else if package == webGLPackage then
        collapseWebGLShader moduleNameStr name args

    else
        Nothing


webGLPackage : PackageName
webGLPackage =
    "elm-explorations/webgl"


collapseElmCoreType : String -> VarName -> List MonoType -> Maybe MonoType
collapseElmCoreType moduleNameStr name args =
    case args of
        [] ->
            if moduleNameStr == "Basics" then
                case name of
                    "Int" ->
                        Just Int

                    "Float" ->
                        Just Float

                    "Bool" ->
                        Just Bool

                    _ ->
                        Nothing

            else if moduleNameStr == "Char" && name == "Char" then
                Just Char

            else if moduleNameStr == "String" && name == "String" then
                Just String

            else
                Nothing

        [ inner ] ->
            if moduleNameStr == "List" && name == "List" then
                Just (List inner)

            else
                Nothing

        _ ->
            Nothing


collapseWebGLShader : String -> VarName -> List MonoType -> Maybe MonoType
collapseWebGLShader moduleNameStr name args =
    if moduleNameStr == "WebGL" && name == "Shader" then
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
            Record { fields = Dict.map (always f) fields }

        ExtensibleRecord r ->
            ExtensibleRecord
                { extensionTypevar = f r.extensionTypevar
                , fields = Dict.map (always f) r.fields
                }

        UserDefinedType r ->
            UserDefinedType
                { package = r.package
                , moduleName = r.moduleName
                , name = r.name
                , args = List.map f r.args
                }

        WebGLShader r ->
            WebGLShader
                { attributesExtension = f r.attributesExtension
                , attributes = Dict.map (always f) r.attributes
                , uniformsExtension = f r.uniformsExtension
                , uniforms = Dict.map (always f) r.uniforms
                , varyingsExtension = f r.varyingsExtension
                , varyings = Dict.map (always f) r.varyings
                }


{-| Collect every type variable occurring in a monotype.

Returns them backwards to later insert into VarSet backwards,
so that they're there in order of first appearance,
SO THAT `normalize` can give us `a -> b -> a` instead of `b -> a -> b`.

Used to decide which variables to quantify in `generalize` (those not already
free in the environment) and in `State.generalize` (those above the current
let-rank).

-}
monoTypeVars : MonoType -> VarSet
monoTypeVars type_ =
    monoTypeVarsHelp type_ VarSet.empty


monoTypeVarsHelp : MonoType -> VarSet -> VarSet
monoTypeVarsHelp type_ acc =
    let
        inFields : Dict VarName MonoType -> VarSet -> VarSet
        inFields fields acc_ =
            Dict.foldr (\_ fieldType -> monoTypeVarsHelp fieldType) acc_ fields
    in
    case type_ of
        TypeVar typeVar ->
            VarSet.insert typeVar acc

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
            inFields fields acc

        ExtensibleRecord r ->
            acc
                |> inFields r.fields
                |> monoTypeVarsHelp r.extensionTypevar

        UserDefinedType r ->
            List.foldr monoTypeVarsHelp acc r.args

        WebGLShader r ->
            acc
                |> inFields r.varyings
                |> monoTypeVarsHelp r.varyingsExtension
                |> inFields r.uniforms
                |> monoTypeVarsHelp r.uniformsExtension
                |> inFields r.attributes
                |> monoTypeVarsHelp r.attributesExtension


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
            VarSet.union
                (monoTypeVars monoType)
                (VarSet.fromList boundVars)
                |> VarSet.toList

        -- eg. `number` and `comparable` get their own slot sequence independent of the `Normal` one
        usedNamesBySuper : Dict String (Set String)
        usedNamesBySuper =
            allVars
                |> List.foldl
                    (\( style, super ) acc ->
                        case style of
                            Named name ->
                                Dict.update
                                    (TypeVar.superTypeToString super)
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
                    Dict.get (TypeVar.superTypeToString super) usedNamesBySuper
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
                                    key : String
                                    key =
                                        TypeVar.superTypeToString super

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

        subst : Dict VarKey TypeVar
        subst =
            List.map2 (\var newVar -> ( varKey var, newVar ))
                allVars
                newVars
                |> Dict.fromList
    in
    type_
        |> mapVars
            (\var ->
                case Dict.get (varKey var) subst of
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
            recurse (mapVarsMono fn) type_


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

        {- The functions below are stolen from fredcy/elm-parseint and tweaked
           to work similar to:

           https://en.wikipedia.org/wiki/Bijective_numeration#The_bijective_base-26_system
        -}
        charFromInt : Int -> Char
        charFromInt i =
            Char.fromCode <| i + Char.toCode 'a'

        go : Int -> String
        go i =
            if i < radix then
                String.fromChar <| charFromInt i

            else
                go ((i // radix) - 1) ++ (String.fromChar <| charFromInt (modBy radix i))
    in
    go n


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
                |> List.map
                    (\fieldNode ->
                        let
                            ( fieldNameNode, annotationNode ) =
                                Node.value fieldNode

                            type_ : Result FromTypeAnnotationError MonoType
                            type_ =
                                f (Node.value annotationNode)
                        in
                        type_
                            |> Result.map (\type__ -> ( Node.value fieldNameNode, type__ ))
                    )
                |> Result.Extra.combine
                |> Result.map Dict.fromList
    in
    case typeAnnotation of
        TypeAnnotation.GenericType name ->
            Ok <| TypeVar (TypeVar.parse name)

        TypeAnnotation.Typed name annotations ->
            let
                ( moduleName, typeName ) =
                    Node.value name

                args : Result FromTypeAnnotationError (List MonoType)
                args =
                    annotations
                        |> List.map (Node.value >> f)
                        |> Result.Extra.combine
            in
            -- Resolve names before collapsing primitives: local or imported
            -- types can shadow implicit names such as List, Int, and String.
            args
                |> Result.andThen
                    (\args_ ->
                        resolver moduleName typeName
                            |> Result.mapError AmbiguousModuleName
                            |> Result.map
                                (\( package, fullModuleName ) ->
                                    collapsePrimitive package fullModuleName typeName args_
                                        |> Maybe.withDefault
                                            (UserDefinedType
                                                { package = package
                                                , moduleName = fullModuleName
                                                , name = typeName
                                                , args = args_
                                                }
                                            )
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


toPublicType : { alreadyNormalized : Bool } -> MonoType -> Public.Type
toPublicType { alreadyNormalized } origMono =
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
    toPublicTypeNormalized mono_


{-| Convert two `MonoType`s to public `Type`s with a shared normalization.

Normalizing each side independently would name distinct variables identically
(`a` on both sides) and suggest sharing where there is none, or rename a
shared variable differently on each side.

Used for type errors, where types come in pairs.

-}
toPublicPair : MonoType -> MonoType -> ( Public.Type, Public.Type )
toPublicPair t1 t2 =
    let
        (Forall _ normalizedCombined) =
            normalize (Forall [] (Tuple2 t1 t2))
    in
    case normalizedCombined of
        Tuple2 nt1 nt2 ->
            ( toPublicType { alreadyNormalized = True } nt1
            , toPublicType { alreadyNormalized = True } nt2
            )

        _ ->
            -- Shouldn't happen
            ( toPublicType { alreadyNormalized = False } t1
            , toPublicType { alreadyNormalized = False } t2
            )


toPublicTypeNormalized : MonoType -> Public.Type
toPublicTypeNormalized mono_ =
    let
        collapsed : MonoType
        collapsed =
            collapseExtensible mono_

        f : MonoType -> Public.Type
        f =
            toPublicType { alreadyNormalized = True }
    in
    case collapsed of
        TypeVar typeVar ->
            Public.TypeVar (TypeVar.toString typeVar)

        Function { from, to } ->
            Public.Function
                { from = f from
                , to = f to
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
            Public.List (f ts)

        Unit ->
            Public.Unit

        Tuple2 t1 t2 ->
            Public.Tuple2
                (f t1)
                (f t2)

        Tuple3 t1 t2 t3 ->
            Public.Tuple3
                (f t1)
                (f t2)
                (f t3)

        Record { fields } ->
            Public.Record { fields = Dict.map (\_ v -> f v) fields }

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
                , fields = fields |> Dict.map (\_ v -> f v)
                }

        UserDefinedType r ->
            Public.Named
                { package = r.package
                , moduleName = FullModuleName.toModuleName r.moduleName
                , name = r.name
                , arguments = List.map f r.args
                }

        WebGLShader r ->
            let
                ( attributesFields, attributesExtensionTypevar ) =
                    shaderSlotToPublic f r.attributesExtension r.attributes

                ( uniformsFields, uniformsExtensionTypevar ) =
                    shaderSlotToPublic f r.uniformsExtension r.uniforms

                ( varyingsFields, varyingsExtensionTypevar ) =
                    shaderSlotToPublic f r.varyingsExtension r.varyings
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
            (ExtensibleRecord
                { extensionTypevar = extensionTypevar
                , fields = fields
                }
            )
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
