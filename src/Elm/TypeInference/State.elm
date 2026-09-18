module Elm.TypeInference.State exposing
    ( GlobalKey
    , State
    , StateM
    , addBinding
    , addGlobalBinding
    , aliasNodeId
    , andThen
    , do
    , empty
    , error
    , existsInEnv
    , fromResult
    , generalize
    , getGlobalEnv
    , getNextIdAndTick
    , getNodeIds
    , getSubst
    , idForNode
    , init
    , instantiate
    , lookupEnv
    , lookupGlobalEnv
    , map
    , map2
    , modifySubst
    , pure
    , run
    , setIdToCurrentLetRank
    , test_initFull
    , traverse
    , withDeeperLetRank
    , withScopedEnv
    )

{-| State monad for the type inference.
-}

import Dict exposing (Dict)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.TypeInference.Error exposing (Error, ErrorDetails(..))
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (LetRank, SubstitutionMap)
import Elm.TypeInference.Type exposing (PackageName, VarName)
import Elm.TypeInference.Type.Internal as TypeI exposing (Id, MonoType, Type(..))
import Elm.TypeInference.TypeVar as TypeVar exposing (TypeVar)
import Elm.TypeInference.VarSet as VarSet
import RangeLike exposing (RangeLike)



-- GENERAL


{-| Key into `globalEnv`.

We need to qualify by package as well because of situations where full qualified
module names are not unique (eg. `stil4m/elm-syntax` has its own `Char.Extra`
while a project using it might use `Char.Extra` from `elmcraft/core-extra`).

-}
type alias GlobalKey =
    ( PackageName, FullModuleName, VarName )


type alias State =
    { {- ID counter, making sure every expression gets its own unique ID
         number. As long as we only expose `getNextIdAndTick` as a way to get
         a new ID, they'll automatically increment.
      -}
      nextId : Id
    , {- Type ID for each AST node of the module being inferred. Ends up being
         its TypeLookupTable. Inference runs one module at a time, so we don't
         need to specify the module name.
      -}
      nodeIds : Dict RangeLike Id
    , {- Types for lexical bindings: lambda args, let..in, case branch patterns.
         Scoped into and out of via `withScopedEnv`.
      -}
      lexicalEnv : Dict VarName Type
    , {- Top-level declarations, constructors, ports, and dependency values.
         Once a binding group gets added, it's here forever.
      -}
      globalEnv : Dict GlobalKey Type
    , -- Map from typevars to types (being built).
      subst : SubstitutionMap
    , -- Enclosing let-rank.
      letRank : LetRank
    }


{-| State monad. A function from state to something + a new state.
-}
type alias StateM a =
    State -> ( Result Error a, State )


pure : a -> StateM a
pure a =
    \s -> ( Ok a, s )


error : Error -> StateM a
error error_ =
    \s -> ( Err error_, s )


fromResult : Result Error a -> StateM a
fromResult result =
    case result of
        Err err ->
            error err

        Ok value ->
            pure value


run : State -> StateM a -> ( Result Error a, State )
run state stateFn =
    stateFn state


map : (a -> b) -> StateM a -> StateM b
map userFn stateFn =
    \state1 ->
        let
            ( value, state2 ) =
                stateFn state1
        in
        ( case value of
            Ok ok ->
                Ok (userFn ok)

            Err err ->
                Err err
        , state2
        )


map2 : (a -> b -> c) -> StateM a -> StateM b -> StateM c
map2 userFn aM bM =
    \state ->
        case aM state of
            ( Err err, aState ) ->
                ( Err err, aState )

            ( Ok a, aState ) ->
                case bM aState of
                    ( Err err, bState ) ->
                        ( Err err, bState )

                    ( Ok b, bState ) ->
                        ( Ok (userFn a b), bState )


andThen : (a -> StateM b) -> StateM a -> StateM b
andThen userFn stateFn =
    \state ->
        let
            ( result, nextState ) =
                stateFn state
        in
        case result of
            Err err ->
                ( Err err, nextState )

            Ok a ->
                userFn a nextState


do : StateM a -> (a -> StateM b) -> StateM b
do m fn =
    andThen fn m


{-| Tail-recursive instead of List.foldr (which blew the stack in the past).
-}
traverse : (a -> StateM b) -> List a -> StateM (List b)
traverse f list =
    \state -> traverseHelp f [] list state


traverseHelp : (a -> StateM b) -> List b -> List a -> State -> ( Result Error (List b), State )
traverseHelp f acc list state =
    case list of
        [] ->
            ( Ok (List.reverse acc), state )

        x :: rest ->
            case f x state of
                ( Err err, newState ) ->
                    ( Err err, newState )

                ( Ok b, newState ) ->
                    traverseHelp f (b :: acc) rest newState


get : StateM State
get =
    \state -> ( Ok state, state )


modify : (State -> State) -> StateM ()
modify fn =
    \state -> ( Ok (), fn state )


empty : State
empty =
    { nextId = 0
    , nodeIds = Dict.empty
    , lexicalEnv = Dict.empty
    , globalEnv = Dict.empty
    , subst = SubstitutionMap.empty
    , letRank = 0
    }


init : Dict GlobalKey Type -> State
init globalEnv =
    { nextId = 0
    , nodeIds = Dict.empty
    , lexicalEnv = Dict.empty
    , globalEnv = globalEnv
    , subst = SubstitutionMap.empty
    , letRank = 0
    }


test_initFull :
    { lexicalEnv : Dict VarName Type
    , globalEnv : Dict GlobalKey Type
    }
    -> State
test_initFull env =
    { nextId = 0
    , nodeIds = Dict.empty
    , lexicalEnv = env.lexicalEnv
    , globalEnv = env.globalEnv
    , subst = SubstitutionMap.empty
    , letRank = 0
    }


getNextIdAndTick : StateM Id
getNextIdAndTick =
    \state ->
        ( Ok state.nextId
        , { nextId = state.nextId + 1
          , nodeIds = state.nodeIds
          , lexicalEnv = state.lexicalEnv
          , globalEnv = state.globalEnv
          , subst = SubstitutionMap.stampIdAtLetRank state.nextId state.letRank state.subst
          , letRank = state.letRank
          }
        )


{-| Run `action` one let-rank deeper, then restore the let-rank.
-}
withDeeperLetRank : StateM a -> StateM a
withDeeperLetRank action =
    \state ->
        let
            ( result, newState ) =
                action
                    { nextId = state.nextId
                    , nodeIds = state.nodeIds
                    , lexicalEnv = state.lexicalEnv
                    , globalEnv = state.globalEnv
                    , subst = state.subst
                    , letRank = state.letRank + 1
                    }
        in
        ( result
        , { nextId = newState.nextId
          , nodeIds = newState.nodeIds
          , lexicalEnv = newState.lexicalEnv
          , globalEnv = newState.globalEnv
          , subst = newState.subst
          , letRank = state.letRank
          }
        )


{-| Move a previously allocated id to the current let-rank.
-}
setIdToCurrentLetRank : Id -> StateM ()
setIdToCurrentLetRank id =
    do get <|
        \state ->
            modifySubst (SubstitutionMap.setIdLetRank id state.letRank)



-- NODE IDS


getNodeIds : StateM (Dict RangeLike Id)
getNodeIds =
    get
        |> map .nodeIds


{-| Give the node a fresh type ID and remember it under the node's range.
-}
idForNode : Node a -> StateM Id
idForNode node =
    do getNextIdAndTick <|
        \theId ->
            do (aliasNodeId (Node.range node) theId) <|
                \() ->
                    pure theId


{-| Make another range point to an already assigned ID.

Needed for when elm-syntax gives two nodes the same range:

  - `Declaration` and its `FunctionImplementation` when there's no documentation and no signature
  - similarly for `LetDeclaration`

-}
aliasNodeId : Range -> Id -> StateM ()
aliasNodeId range theId =
    modify
        (\state ->
            { nextId = state.nextId
            , nodeIds = Dict.insert (RangeLike.fromRange range) theId state.nodeIds
            , lexicalEnv = state.lexicalEnv
            , globalEnv = state.globalEnv
            , subst = state.subst
            , letRank = state.letRank
            }
        )



-- THE ACCUMULATED SOLUTION


getSubst : StateM SubstitutionMap
getSubst =
    get
        |> map .subst


modifySubst : (SubstitutionMap -> SubstitutionMap) -> StateM ()
modifySubst fn =
    modify
        (\state ->
            { nextId = state.nextId
            , nodeIds = state.nodeIds
            , lexicalEnv = state.lexicalEnv
            , globalEnv = state.globalEnv
            , subst = fn state.subst
            , letRank = state.letRank
            }
        )


{-| Substitute a `MonoType`.
Remember the substitution map advancement (newly discovered ground resolutions,
or path compression) into the state.
-}
substituteMono : MonoType -> StateM MonoType
substituteMono monoType =
    \state ->
        let
            ( monoType_, _, subst1 ) =
                SubstitutionMap.substituteMono state.subst monoType
        in
        ( Ok monoType_
        , { nextId = state.nextId
          , nodeIds = state.nodeIds
          , lexicalEnv = state.lexicalEnv
          , globalEnv = state.globalEnv
          , subst = subst1
          , letRank = state.letRank
          }
        )


{-| Same as `substituteMono`, but for a whole `Type` scheme.
-}
substitute : Type -> StateM Type
substitute type_ =
    \state ->
        let
            ( type__, subst1 ) =
                SubstitutionMap.substitute state.subst type_
        in
        ( Ok type__
        , { nextId = state.nextId
          , nodeIds = state.nodeIds
          , lexicalEnv = state.lexicalEnv
          , globalEnv = state.globalEnv
          , subst = subst1
          , letRank = state.letRank
          }
        )



-- LEXICAL ENV


getLexicalEnv : StateM (Dict VarName Type)
getLexicalEnv =
    get
        |> map .lexicalEnv


modifyLexicalEnv : (Dict VarName Type -> Dict VarName Type) -> StateM ()
modifyLexicalEnv fn =
    modify
        (\state ->
            { nextId = state.nextId
            , nodeIds = state.nodeIds
            , lexicalEnv = fn state.lexicalEnv
            , globalEnv = state.globalEnv
            , subst = state.subst
            , letRank = state.letRank
            }
        )


addBinding : VarName -> Type -> StateM ()
addBinding var type_ =
    modifyLexicalEnv (Dict.insert var type_)


{-| Run `action`, then restore `lexicalEnv` back, but update the rest.
This makes args, let bindings etc. not leak into the rest of the program.
-}
withScopedEnv : StateM a -> StateM a
withScopedEnv action =
    \state ->
        let
            ( result, newState ) =
                action state
        in
        ( result
        , { nextId = newState.nextId
          , nodeIds = newState.nodeIds
          , lexicalEnv = state.lexicalEnv
          , globalEnv = newState.globalEnv
          , subst = newState.subst
          , letRank = newState.letRank
          }
        )


existsInEnv : VarName -> StateM Bool
existsInEnv varName =
    getLexicalEnv
        |> map (Dict.member varName)


{-| Look up a var in lexical env (let..in var, lambda arg, ...), substituting
all typevars that we can.
-}
lookupEnv : FullModuleName -> VarName -> StateM MonoType
lookupEnv thisModule var =
    do getLexicalEnv <|
        \env ->
            case Dict.get var env of
                Nothing ->
                    error
                        { moduleName = FullModuleName.toModuleName thisModule
                        , declarationNames = []
                        , details =
                            VarNotFound
                                { usedIn = FullModuleName.toModuleName thisModule
                                , varName = var
                                }
                        }

                Just type_ ->
                    do (substitute type_) <|
                        \substituted ->
                            instantiate substituted



-- GLOBAL ENV


getGlobalEnv : StateM (Dict GlobalKey Type)
getGlobalEnv =
    get
        |> map .globalEnv


addGlobalBinding : GlobalKey -> Type -> StateM ()
addGlobalBinding key type_ =
    modify
        (\state ->
            { nextId = state.nextId
            , nodeIds = state.nodeIds
            , lexicalEnv = state.lexicalEnv
            , globalEnv = Dict.insert key type_ state.globalEnv
            , subst = state.subst
            , letRank = state.letRank
            }
        )


{-| Look up a global name (top-level/constructor/port/dependency).
-}
lookupGlobalEnv : PackageName -> FullModuleName -> VarName -> StateM MonoType
lookupGlobalEnv package moduleName var =
    do getGlobalEnv <|
        \env ->
            case Dict.get ( package, moduleName, var ) env of
                Nothing ->
                    error
                        { moduleName = FullModuleName.toModuleName moduleName
                        , declarationNames = []
                        , details =
                            VarNotFound
                                { usedIn = FullModuleName.toModuleName moduleName
                                , varName = var
                                }
                        }

                Just type_ ->
                    instantiate type_


{-| Give a scheme's quantified variables fresh IDs.

This allows let-polymorphism: each of `id`s in `(id 0, id "x")` is its own
separate `someID -> someID`, and they don't touch each other.

-}
instantiate : Type -> StateM MonoType
instantiate (Forall boundVars monoType) =
    case boundVars of
        [] ->
            pure monoType

        _ ->
            do (traverse (always getNextIdAndTick) boundVars) <|
                \varIds ->
                    let
                        renaming : Dict VarSet.VarKey TypeVar
                        renaming =
                            List.map2
                                (\(( _, super ) as var) freshId ->
                                    ( VarSet.varKey var
                                    , -- keep the constraint (eg. `number`)
                                      ( TypeVar.Generated freshId, super )
                                    )
                                )
                                boundVars
                                varIds
                                |> Dict.fromList
                    in
                    monoType
                        |> TypeI.mapVarsMono
                            (\var ->
                                Dict.get (VarSet.varKey var) renaming
                                    |> Maybe.withDefault var
                            )
                        |> pure


generalize : MonoType -> StateM Type
generalize monoType =
    do (substituteMono monoType) <|
        \substitutedMono ->
            do get <|
                \state ->
                    let
                        boundIds : List TypeVar
                        boundIds =
                            TypeI.monoTypeVars substitutedMono
                                |> VarSet.toList
                                |> List.filter
                                    (\var -> SubstitutionMap.letRankOf var state.subst > state.letRank)
                    in
                    pure (Forall boundIds substitutedMono)
