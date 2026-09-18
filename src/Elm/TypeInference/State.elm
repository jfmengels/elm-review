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
    , foldl
    , fromResult
    , generalize
    , generalizeBinding
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
    , okUnit
    , pure
    , pureUnit
    , run
    , setIdToCurrentLetRank
    , test_initFull
    , traverse
    , traverseUnit
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
import Elm.TypeInference.ModuleIds as ModuleIds exposing (ModuleId)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap exposing (LetRank, SubstitutionMap)
import Elm.TypeInference.Type exposing (PackageName, VarName)
import Elm.TypeInference.Type.Internal as TypeI exposing (Id, MonoType, Type(..))
import Elm.TypeInference.TypeVar as TypeVar exposing (TypeVar, TypeVarStyle(..))
import Elm.TypeInference.VarSet as VarSet
import RangeLike exposing (RangeLike)



-- GENERAL


{-| Key into `globalEnv`.

We need to qualify by package as well because of situations where full qualified
module names are not unique (eg. `stil4m/elm-syntax` has its own `Char.Extra`
while a project using it might use `Char.Extra` from `elmcraft/core-extra`).

-}
type alias GlobalKey =
    ( ModuleId, PackageName, VarName )


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


{-| prefer `pureUnit` over `pure ()`
-}
pure : a -> StateM a
pure a =
    \s -> ( Ok a, s )


{-| Equivalent to `pure ()` but more memory efficient
-}
pureUnit : StateM ()
pureUnit =
    \s -> ( okUnit, s )


{-| Prefer over `Ok ()` to not construct new instances of it again and again
-}
okUnit : Result error ()
okUnit =
    Ok ()


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


foldl : (a -> foldState -> StateM foldState) -> foldState -> List a -> StateM foldState
foldl reduce initialFoldState list =
    \state -> foldlHelp reduce initialFoldState list state


foldlHelp : (a -> foldState -> StateM foldState) -> foldState -> List a -> State -> ( Result Error foldState, State )
foldlHelp reduce acc list state =
    case list of
        [] ->
            ( Ok acc, state )

        x :: rest ->
            case reduce x acc state of
                ( (Err _) as err, newState ) ->
                    ( err, newState )

                ( Ok b, newState ) ->
                    foldlHelp reduce b rest newState


{-| Tail-recursive instead of List.foldr (which blew the stack in the past).
Prefer `State.traverseUnit` if the function returns a `StateM ()`
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


traverseUnit : (a -> StateM ()) -> List a -> StateM ()
traverseUnit f list =
    \state -> traverseUnitHelp f list state


traverseUnitHelp : (a -> StateM ()) -> List a -> State -> ( Result Error (), State )
traverseUnitHelp f list state =
    case list of
        [] ->
            ( okUnit, state )

        x :: rest ->
            case f x state of
                ( (Err _) as err, newState ) ->
                    ( err, newState )

                ( Ok (), newState ) ->
                    traverseUnitHelp f rest newState


get : StateM State
get =
    \state -> ( Ok state, state )


modify : (State -> State) -> StateM ()
modify fn =
    \state -> ( okUnit, fn state )


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
    do get <| \state ->
    modifySubst (\subst -> subst |> SubstitutionMap.setIdLetRank id state.letRank)



-- NODE IDS


getNodeIds : StateM (Dict RangeLike Id)
getNodeIds =
    get
        |> map .nodeIds


{-| Give the node a fresh type ID and remember it under the node's range.
-}
idForNode : Node a -> StateM Id
idForNode node =
    do getNextIdAndTick <| \theId ->
    do (aliasNodeId (Node.range node) theId) <| \() ->
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
    \state -> ( Ok state.subst, state )


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
substituteMono : MonoType -> State -> ( MonoType, State )
substituteMono monoType state =
    let
        ( monoType_, _, subst1 ) =
            SubstitutionMap.substituteMono state.subst monoType
    in
    ( monoType_
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
substitute : Type -> State -> ( Type, State )
substitute type_ =
    \state ->
        let
            ( type__, subst1 ) =
                SubstitutionMap.substitute state.subst type_
        in
        ( type__
        , { nextId = state.nextId
          , nodeIds = state.nodeIds
          , lexicalEnv = state.lexicalEnv
          , globalEnv = state.globalEnv
          , subst = subst1
          , letRank = state.letRank
          }
        )



-- LEXICAL ENV


stateAddBinding : VarName -> Type -> State -> State
stateAddBinding var type_ state =
    { nextId = state.nextId
    , nodeIds = state.nodeIds
    , lexicalEnv = state.lexicalEnv |> Dict.insert var type_
    , globalEnv = state.globalEnv
    , subst = state.subst
    , letRank = state.letRank
    }


addBinding : VarName -> Type -> StateM ()
addBinding var type_ =
    modify (\state -> state |> stateAddBinding var type_)


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
    \state ->
        ( Ok (Dict.member varName state.lexicalEnv)
        , state
        )


{-| Look up a var in lexical env (let..in var, lambda arg, ...), substituting
all typevars that we can.
-}
lookupEnv : FullModuleName -> VarName -> StateM MonoType
lookupEnv thisModule var =
    \state0 ->
        case Dict.get var state0.lexicalEnv of
            Nothing ->
                ( Err
                    { moduleName = FullModuleName.toModuleName thisModule
                    , declarationNames = []
                    , details =
                        VarNotFound
                            { usedIn = FullModuleName.toModuleName thisModule
                            , varName = var
                            }
                    }
                , state0
                )

            Just type_ ->
                let
                    ( substituted, state1 ) =
                        substitute type_ state0
                in
                instantiate substituted state1



-- GLOBAL ENV


getGlobalEnv : StateM (Dict GlobalKey Type)
getGlobalEnv =
    \state -> ( Ok state.globalEnv, state )


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
lookupGlobalEnv : ModuleIds.Mapping -> PackageName -> ModuleId -> VarName -> StateM MonoType
lookupGlobalEnv moduleMapping package moduleId var =
    \state ->
        case Dict.get ( moduleId, package, var ) state.globalEnv of
            Nothing ->
                let
                    moduleName : List String
                    moduleName =
                        moduleIdToModuleName moduleMapping moduleId
                in
                ( Err
                    { moduleName = moduleName
                    , declarationNames = []
                    , details =
                        VarNotFound
                            { usedIn = moduleName
                            , varName = var
                            }
                    }
                , state
                )

            Just type_ ->
                instantiate type_ state


moduleIdToModuleName : ModuleIds.Mapping -> ModuleId -> List String
moduleIdToModuleName moduleMapping moduleId =
    ModuleIds.moduleNameForDisplay moduleId moduleMapping


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
            do (traverse (\var -> map (\id -> ( var, id )) getNextIdAndTick) boundVars) <| \varIds ->
            let
                ( renamingGen, renamingNamed ) =
                    varIds
                        |> List.foldl
                            (\( ( style, super ), freshId ) ( genAcc, namedAcc ) ->
                                case style of
                                    Generated theId ->
                                        ( Dict.insert (VarSet.genKeyFrom theId super)
                                            ( TypeVar.Generated freshId, super )
                                            genAcc
                                        , namedAcc
                                        )

                                    Named name ->
                                        ( genAcc
                                        , Dict.insert (VarSet.namedKeyFrom name super)
                                            ( TypeVar.Generated freshId, super )
                                            namedAcc
                                        )
                            )
                            ( Dict.empty, Dict.empty )
            in
            monoType
                |> TypeI.mapVarsMono
                    (\(( style, super ) as var) ->
                        case style of
                            Generated theId ->
                                Dict.get (VarSet.genKeyFrom theId super) renamingGen
                                    |> Maybe.withDefault var

                            Named name ->
                                Dict.get (VarSet.namedKeyFrom name super) renamingNamed
                                    |> Maybe.withDefault var
                    )
                |> pure


generalize : MonoType -> StateM Type
generalize monoType =
    \state0 ->
        let
            ( substitutedMono, state1 ) =
                substituteMono monoType state0

            boundIds : List TypeVar
            boundIds =
                TypeI.monoTypeVars substitutedMono
                    |> VarSet.toList
                    |> List.filter
                        (\var -> SubstitutionMap.letRankOf var state1.subst > state1.letRank)
        in
        ( Ok (Forall boundIds substitutedMono), state1 )


{-| Generalize a lexical binding in place (for `let` destructurings).

Each name gets its own scheme - they are independent.

-}
generalizeBinding : VarName -> StateM ()
generalizeBinding var =
    \state0 ->
        case Dict.get var state0.lexicalEnv of
            Nothing ->
                ( okUnit, state0 )

            Just (Forall _ mono) ->
                let
                    ( schemeResult, state1 ) =
                        generalize mono state0
                in
                case schemeResult of
                    Err err ->
                        -- impossible
                        ( Err err, state1 )

                    Ok scheme ->
                        ( okUnit, stateAddBinding var scheme state1 )
