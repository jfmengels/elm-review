module Elm.TypeInference.BindingGroup exposing (Member, solveGroup)

{-| Solve a binding group (SCC of mutually-referencing bindings).
-}

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName as FullModuleName
import Elm.TypeInference.Error exposing (ErrorDetails(..))
import Elm.TypeInference.State as State exposing (StateM)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type.Internal as TypeI exposing (Id, MonoType(..), Type(..))
import Elm.TypeInference.TypeEquation as TypeEquation exposing (TypeEquation)
import Elm.TypeInference.Unify as Unify exposing (UnifyConfig)
import Elm.TypeInference.VarSet as VarSet


{-| One binding in the binding group.
-}
type alias Member =
    { -- fresh type ID, registered against the declaration Range
      id : Id
    , annotation : Maybe Type
    , -- top-level decls get installed into `globalEnv`
      -- let..in bindings get installed into `lexicalEnv`
      install : Type -> StateM ()
    , -- monadic action to generate equations. Must be run after members'
      -- placeholders/annotations have been installed as they can reference each
      -- other.
      equations : StateM (List TypeEquation)
    }


solveGroup : UnifyConfig -> List Member -> StateM ()
solveGroup cfg members =
    State.do
        (State.withDeeperLetRank
            (State.do
                (State.traverse
                    (\member ->
                        State.do (State.setIdToCurrentLetRank member.id) <|
                            \() ->
                                case member.annotation of
                                    Just scheme ->
                                        -- Trust the annotation
                                        member.install scheme

                                    Nothing ->
                                        member.install (TypeI.mono (TypeI.id_ member.id))
                    )
                    members
                )
             <|
                \_ ->
                    State.do (State.traverse .equations members) <|
                        \eqLists ->
                            State.do
                                (eqLists
                                    |> List.concat
                                    |> List.map TypeEquation.dropLabel
                                    |> Unify.unifyMany cfg
                                )
                            <|
                                \() ->
                                    checkAnnotations cfg members
            )
        )
    <|
        \() ->
            State.do
                (State.traverse
                    (\member ->
                        case member.annotation of
                            Just _ ->
                                State.pure ()

                            Nothing ->
                                State.do (State.generalize (TypeI.id_ member.id)) <|
                                    \scheme ->
                                        member.install scheme
                    )
                    members
                )
            <|
                \_ ->
                    State.pure ()


{-| A declaration body must be at least as general as its annotation.

Motivating example:

    x : number
    x =
        1.0

This shouldn't typecheck: we know 1.0 must be a Float, so `number` is too
general.

-}
checkAnnotations : UnifyConfig -> List Member -> StateM ()
checkAnnotations cfg members =
    State.traverse (checkOne cfg) members
        |> State.map (always ())


checkOne : UnifyConfig -> Member -> StateM ()
checkOne cfg member =
    case member.annotation of
        Nothing ->
            State.pure ()

        Just (Forall boundVars annoMono) ->
            if List.isEmpty boundVars then
                State.pure ()

            else
                State.do State.getSubst <|
                    \subst ->
                        let
                            ( finalMono, _, _ ) =
                                SubstitutionMap.substituteMono subst (TypeI.id_ member.id)
                        in
                        if shaderSlotsTooGeneral annoMono finalMono then
                            let
                                ( pubAnno, pubFinal ) =
                                    TypeI.toPublicPair annoMono finalMono
                            in
                            State.error
                                { moduleName = FullModuleName.toModuleName cfg.moduleName
                                , declarationNames = cfg.declarationNames
                                , details = TypeMismatch pubAnno pubFinal
                                }

                        else if List.isEmpty (VarSet.toList (TypeI.monoTypeVars finalMono)) then
                            let
                                ( pubAnno, pubFinal ) =
                                    TypeI.toPublicPair annoMono finalMono
                            in
                            State.error
                                { moduleName = FullModuleName.toModuleName cfg.moduleName
                                , declarationNames = cfg.declarationNames
                                , details = TypeMismatch pubAnno pubFinal
                                }

                        else
                            State.pure ()


{-|

    shader : Shader a b c
    shader =
        [glsl|
        attribute vec3 position;
    |]

is an error because the annotation is more general than the body.

-}
shaderSlotsTooGeneral : MonoType -> MonoType -> Bool
shaderSlotsTooGeneral annoMono finalMono =
    case ( annoMono, finalMono ) of
        ( WebGLShader annoShader, WebGLShader finalShader ) ->
            let
                slots :
                    { attributesExtension : MonoType
                    , attributes : Dict String MonoType
                    , uniformsExtension : MonoType
                    , uniforms : Dict String MonoType
                    , varyingsExtension : MonoType
                    , varyings : Dict String MonoType
                    }
                    -> List { extensionTypevar : MonoType, fields : Dict String MonoType }
                slots shader =
                    [ { extensionTypevar = shader.attributesExtension, fields = shader.attributes }
                    , { extensionTypevar = shader.uniformsExtension, fields = shader.uniforms }
                    , { extensionTypevar = shader.varyingsExtension, fields = shader.varyings }
                    ]
            in
            List.map2 Tuple.pair (slots annoShader) (slots finalShader)
                |> List.any (\( annoSlot, finalSlot ) -> slotTooGeneral annoSlot finalSlot)

        _ ->
            False


slotTooGeneral :
    { extensionTypevar : MonoType, fields : Dict String MonoType }
    -> { extensionTypevar : MonoType, fields : Dict String MonoType }
    -> Bool
slotTooGeneral annoSlot finalSlot =
    let
        collapsedFields : { extensionTypevar : MonoType, fields : Dict String MonoType } -> Dict String MonoType
        collapsedFields slot =
            case
                TypeI.collapseExtensible
                    (ExtensibleRecord
                        { extensionTypevar = slot.extensionTypevar
                        , fields = slot.fields
                        }
                    )
            of
                ExtensibleRecord r ->
                    r.fields

                Record r ->
                    r.fields

                _ ->
                    Dict.empty
    in
    case annoSlot.extensionTypevar of
        TypeVar _ ->
            if Dict.isEmpty annoSlot.fields then
                not (Dict.isEmpty (collapsedFields finalSlot))

            else
                not (Dict.isEmpty (Dict.diff annoSlot.fields (collapsedFields finalSlot)))

        _ ->
            not (Dict.isEmpty (Dict.diff annoSlot.fields (collapsedFields finalSlot)))
