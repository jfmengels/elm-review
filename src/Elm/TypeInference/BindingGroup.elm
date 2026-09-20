module Elm.TypeInference.BindingGroup exposing (Member, solveGroup)

{-| Solve a binding group (SCC of mutually-referencing bindings).
-}

import Elm.TypeInference.State as State exposing (StateM)
import Elm.TypeInference.Type.Internal as TypeI exposing (Id, Type)
import Elm.TypeInference.TypeEquation as TypeEquation exposing (TypeEquation)
import Elm.TypeInference.Unify as Unify exposing (UnifyConfig)


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
                (State.traverseUnit
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
                \() ->
                    State.do
                        (State.foldl
                            (\member accAcrossMembers ->
                                State.map
                                    (\memberEqs ->
                                        List.foldr
                                            (\memberEq acc ->
                                                TypeEquation.dropLabel memberEq :: acc
                                            )
                                            accAcrossMembers
                                            memberEqs
                                    )
                                    member.equations
                            )
                            []
                            members
                        )
                    <|
                        \eqLists ->
                            Unify.unifyMany cfg eqLists
            )
        )
    <|
        \() ->
            State.traverseUnit
                (\member ->
                    case member.annotation of
                        Just _ ->
                            State.pureUnit

                        Nothing ->
                            State.do (State.generalize (TypeI.id_ member.id)) <|
                                \scheme ->
                                    member.install scheme
                )
                members
