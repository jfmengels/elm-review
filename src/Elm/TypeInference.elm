module Elm.TypeInference exposing
    ( inferProject
    , DependencyEnv, DependencyEnvOutcome(..), dependencyEnv
    , Dependency
    , ModuleInterface, ProjectAcc, inferModule_, inferOne
    )

{-| Type inference for [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/)
ASTs.


# Whole project at once

@docs inferProject


# Dependencies

@docs DependencyEnv, DependencyEnvOutcome, dependencyEnv
@docs Dependency

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Expression.Extra
import Elm.Syntax.File exposing (File)
import Elm.Syntax.FullModuleName as FullModuleName exposing (FullModuleName)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as SyntaxType
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Elm.TypeInference.BindingGroup as BindingGroup
import Elm.TypeInference.Dependencies as Dependencies exposing (Dependencies)
import Elm.TypeInference.DependencySources as DependencySources
import Elm.TypeInference.Error exposing (Error, ErrorDetails(..))
import Elm.TypeInference.Error.Internal exposing (FromTypeAnnotationError)
import Elm.TypeInference.Infer as Infer
import Elm.TypeInference.ModuleIndex as ModuleIndex exposing (ModuleIndex)
import Elm.TypeInference.ModuleLookup as ModuleLookup
import Elm.TypeInference.SCC as SCC
import Elm.TypeInference.State as State exposing (GlobalKey, StateM)
import Elm.TypeInference.SubstitutionMap as SubstitutionMap
import Elm.TypeInference.Type as Type exposing (PackageName, Type, VarName)
import Elm.TypeInference.Type.Internal as TypeI exposing (MonoType(..), TypeResolver)
import Elm.TypeInference.TypeVar as TypeVar
import Elm.TypeInference.Unify exposing (TypeAlias)
import List.ExtraExtra
import Maybe.Extra
import Result.Extra
import Set exposing (Set)
import TypeLookupTable exposing (TypeLookupTable)
import TypeLookupTable.Internal



-- WHOLE-PROJECT ENTRY POINTS


{-| Infer every module of a project.
-}
inferProject :
    DependencyEnv
    -> Dict ModuleName File
    -> { tables : Dict ModuleName TypeLookupTable, errors : Dict ModuleName Error }
inferProject depEnv files =
    let
        modules : List ProjectModule
        modules =
            files
                |> Dict.toList
                |> List.filterMap
                    (\( key, file ) ->
                        -- The caller's key is what the result is keyed by; the
                        -- file's own `module Foo exposing (..)` line is what
                        -- imports elsewhere refer to. They agree in practice.
                        if FullModuleName.fromModuleName key == Nothing then
                            Nothing

                        else
                            let
                                index : ModuleIndex
                                index =
                                    ModuleIndex.fromFile file
                            in
                            Just { key = key, index = ModuleIndex.fromFile file, file = file }
                    )

        missingModuleName : Bool
        missingModuleName =
            List.length modules /= Dict.size files

        byName : Dict FullModuleName ProjectModule
        byName =
            modules
                |> List.map (\m -> ( m.index.moduleName, m ))
                |> Dict.fromList

        firstPartyImports : FullModuleName -> List FullModuleName
        firstPartyImports moduleName =
            case Dict.get moduleName byName of
                Nothing ->
                    []

                Just m ->
                    m.index.imports
                        |> List.filterMap
                            (\import_ ->
                                if Dict.member import_.moduleName byName then
                                    Just import_.moduleName

                                else
                                    Nothing
                            )
    in
    if missingModuleName then
        { tables = Dict.empty
        , errors =
            Dict.singleton []
                { moduleName = [ "<Missing>" ]
                , declarationNames = []
                , details = MissingModuleName
                }
        }

    else
        let
            {- Tarjan emits a component only after everything it can reach, so this
               is already in dependency-first order. Elm forbids import cycles, so
               each component is a single module -- but if the caller hands us one
               anyway we still infer every module in it, just without the benefit
               of its cyclic partners' interfaces.
            -}
            order : List ProjectModule
            order =
                SCC.stronglyConnectedComponents (Dict.keys byName) firstPartyImports
                    |> List.ExtraExtra.fastConcatMap (List.filterMap (\name -> Dict.get name byName))
        in
        order
            |> List.foldl (inferOne depEnv)
                { tables = Dict.empty
                , errors = Dict.empty
                , interfaces = Dict.empty
                }
            |> (\acc -> { tables = acc.tables, errors = acc.errors })



-- DEPENDENCIES


{-| A dependency package with its type information.

  - `name` -- the package identifier (e.g. `"elm/core"`).
  - `dependencies` -- names of the package's _immediate_ `elm.json` dependencies (eg. "elm/json").
  - `modules` -- the decoded `docs.json` modules

-}
type alias Dependency =
    { name : PackageName
    , dependencies : List PackageName
    , modules : List Elm.Docs.Module
    }


{-| Dependency types and other info computed from dependencies' docs.json files.

This cache doesn't change as user's project code changes - only invalidate it
when elm.json changes.

-}
type DependencyEnv
    = DependencyEnv
        { globalEnv : Dict GlobalKey TypeI.Type
        , typeAliases : Dict GlobalKey TypeAlias
        , index : ModuleLookup.Index
        }


{-| Did dependencies process correctly?
-}
type DependencyEnvOutcome
    = Ready DependencyEnv
    | NeedSources { neededPackages : List PackageName }
    | Failed Error


{-| Build a `DependencyEnv`.

Start by running `dependencyEnv` with empty `sourcesToResolveAmbiguity`.

If you get `NeedSources` back, read those Elm files from the dependencies in
your ELM\_HOME and supply them in `sourcesToResolveAmbiguity` in the next call.

If you get `Failed` back, the dependencies' `docs.json` types could not be
resolved.

-}
dependencyEnv :
    { directDependencies : List PackageName
    , allDependencies : List Dependency
    , sourcesToResolveAmbiguity : Dict PackageName (List File)
    }
    -> DependencyEnvOutcome
dependencyEnv { directDependencies, allDependencies, sourcesToResolveAmbiguity } =
    let
        deps : Dependencies
        deps =
            Dependencies.fromList allDependencies

        directVisibleDeps : Dependencies
        directVisibleDeps =
            allDependencies
                |> List.filter (\pkg -> List.member pkg.name directDependencies)
                |> Dependencies.fromList

        baseEnv : Result Error DependencyEnv
        baseEnv =
            (State.do (Dependencies.register deps) <|
                \depAliases ->
                    State.do State.getGlobalEnv <|
                        \globalEnv ->
                            State.pure <|
                                DependencyEnv
                                    { globalEnv = globalEnv
                                    , typeAliases = depAliases
                                    , index = ModuleLookup.buildIndex directVisibleDeps
                                    }
            )
                |> State.run State.empty
                |> Tuple.first
    in
    case baseEnv of
        Err err ->
            Failed err

        Ok (DependencyEnv env) ->
            let
                reachable : Set PackageName
                reachable =
                    reachablePackages deps directDependencies

                needed : List PackageName
                needed =
                    DependencySources.neededPackages deps sourcesToResolveAmbiguity
                        |> List.filter (\pkg -> Set.member pkg reachable)
                        |> List.sort
            in
            case needed of
                _ :: _ ->
                    NeedSources { neededPackages = needed }

                [] ->
                    case DependencySources.aliases deps sourcesToResolveAmbiguity of
                        Err err ->
                            Failed err

                        Ok sourceAliases ->
                            Ready
                                (DependencyEnv { env | typeAliases = Dict.union sourceAliases env.typeAliases })


reachablePackages : Dependencies -> List PackageName -> Set PackageName
reachablePackages deps roots =
    reachablePackagesHelp deps roots Set.empty


reachablePackagesHelp : Dependencies -> List PackageName -> Set PackageName -> Set PackageName
reachablePackagesHelp deps queue seen =
    case queue of
        [] ->
            seen

        name :: rest ->
            if Set.member name seen then
                reachablePackagesHelp deps rest seen

            else
                case Dict.get name deps of
                    Nothing ->
                        reachablePackagesHelp deps rest (Set.insert name seen)

                    Just pkg ->
                        reachablePackagesHelp deps (rest ++ pkg.dependencies) (Set.insert name seen)



-- PER-MODULE INFERENCE (internal)


{-| What one module contributes to the modules that import it.
-}
type alias ModuleInterface =
    { moduleIndex : ModuleIndex
    , values : Dict VarName TypeI.Type
    , typeAliases : Dict GlobalKey TypeAlias
    }


type alias ProjectModule =
    { key : ModuleName
    , index : ModuleIndex
    , file : File
    }


type alias ProjectAcc =
    { tables : Dict ModuleName TypeLookupTable
    , errors : Dict ModuleName Error
    , interfaces : Dict FullModuleName ModuleInterface
    }


inferOne : DependencyEnv -> ProjectModule -> ProjectAcc -> ProjectAcc
inferOne depEnv m acc =
    let
        imported : Dict FullModuleName ModuleInterface
        imported =
            m.index.imports
                |> List.foldl
                    (\import_ inner ->
                        case Dict.get import_.moduleName acc.interfaces of
                            Just interface ->
                                Dict.insert import_.moduleName interface inner

                            Nothing ->
                                inner
                    )
                    Dict.empty
    in
    case inferModule_ depEnv imported m.file of
        Ok { table, interface } ->
            { acc
                | tables = Dict.insert m.key table acc.tables
                , interfaces = Dict.insert m.index.moduleName interface acc.interfaces
            }

        Err err ->
            { acc
                | errors = Dict.insert m.key err acc.errors
                , interfaces =
                    Dict.insert m.index.moduleName
                        { moduleIndex = m.index
                        , values = Dict.empty
                        , typeAliases = Dict.empty
                        }
                        acc.interfaces
            }



-- THE CORE


{-| Everything a single module's inference needs, derived once from the
`DependencyEnv` and the imported interfaces.
-}
type alias ModuleCtx =
    { thisIndex : ModuleIndex
    , modules : Dict FullModuleName ModuleIndex
    , resolver : TypeResolver
    , index : ModuleLookup.Index
    , -- what this module passes on to its own importers
      inheritedAliases : Dict GlobalKey TypeAlias
    , depTypeAliases : Dict GlobalKey TypeAlias
    , globalEnv : Dict GlobalKey TypeI.Type
    }


moduleCtx : DependencyEnv -> Dict FullModuleName ModuleInterface -> File -> ModuleCtx
moduleCtx (DependencyEnv depEnv) importedInterfaces file =
    let
        thisIndex : ModuleIndex
        thisIndex =
            ModuleIndex.fromFile file

        modules : Dict FullModuleName ModuleIndex
        modules =
            importedInterfaces
                |> Dict.map (\_ interface -> interface.moduleIndex)
                |> Dict.insert thisIndex.moduleName thisIndex

        imported :
            { inheritedAliases : Dict GlobalKey TypeAlias
            , globalEnv : Dict GlobalKey TypeI.Type
            }
        imported =
            Dict.foldl
                (\moduleName interface acc ->
                    { inheritedAliases = Dict.union interface.typeAliases acc.inheritedAliases
                    , globalEnv =
                        Dict.foldl
                            (\name scheme inner -> Dict.insert ( "", moduleName, name ) scheme inner)
                            acc.globalEnv
                            interface.values
                    }
                )
                { inheritedAliases = Dict.empty
                , globalEnv = depEnv.globalEnv
                }
                importedInterfaces
    in
    { thisIndex = thisIndex
    , modules = modules
    , resolver = ModuleLookup.typeResolverFor depEnv.index modules thisIndex
    , index = depEnv.index
    , inheritedAliases = imported.inheritedAliases
    , depTypeAliases = depEnv.typeAliases
    , globalEnv = imported.globalEnv
    }


inferModule_ :
    DependencyEnv
    -> Dict FullModuleName ModuleInterface
    -> File
    -> Result Error { table : TypeLookupTable, interface : ModuleInterface }
inferModule_ depEnv importedInterfaces file =
    let
        ctx : ModuleCtx
        ctx =
            moduleCtx depEnv importedInterfaces file
    in
    (State.do (gatherTypeAliases ctx file) <|
        \ownAliases ->
            let
                outgoingAliases : Dict GlobalKey TypeAlias
                outgoingAliases =
                    Dict.union ownAliases ctx.inheritedAliases

                typeAliases : Dict GlobalKey TypeAlias
                typeAliases =
                    Dict.union outgoingAliases ctx.depTypeAliases
            in
            State.do (registerConstructorsAndPorts ctx file) <|
                \() ->
                    State.do (solveModule ctx typeAliases file) <|
                        \() ->
                            State.do (moduleResult ctx outgoingAliases) <|
                                \result ->
                                    State.pure result
    )
        |> State.run (State.init ctx.globalEnv)
        |> Tuple.first


moduleResult :
    ModuleCtx
    -> Dict GlobalKey TypeAlias
    ->
        StateM
            { table : TypeLookupTable
            , interface : ModuleInterface
            }
moduleResult ctx outgoingAliases =
    State.do State.getNodeIds <|
        \nodeIds ->
            State.do State.getSubst <|
                \substitutionMap ->
                    State.do State.getGlobalEnv <|
                        \globalEnv ->
                            let
                                ( typesByRange, _, _ ) =
                                    nodeIds
                                        |> Dict.foldl
                                            (\rangeLike id ( accDict, accPool, accSubst ) ->
                                                let
                                                    ( monoType, _, accSubst1 ) =
                                                        SubstitutionMap.substituteMono accSubst (TypeI.id_ id)

                                                    pubType : Type
                                                    pubType =
                                                        TypeI.toPublicType { alreadyNormalized = False } monoType

                                                    key : String
                                                    key =
                                                        publicTypeKey pubType
                                                in
                                                case Dict.get key accPool of
                                                    Just canonical ->
                                                        ( Dict.insert rangeLike canonical accDict
                                                        , accPool
                                                        , accSubst1
                                                        )

                                                    Nothing ->
                                                        ( Dict.insert rangeLike pubType accDict
                                                        , Dict.insert key pubType accPool
                                                        , accSubst1
                                                        )
                                            )
                                            ( Dict.empty, Dict.empty, substitutionMap )

                                exposedValues : Dict VarName TypeI.Type
                                exposedValues =
                                    ctx.thisIndex.exposedValues
                                        |> Set.foldl
                                            (\name acc ->
                                                case Dict.get ( "", ctx.thisIndex.moduleName, name ) globalEnv of
                                                    Just scheme ->
                                                        Dict.insert name scheme acc

                                                    Nothing ->
                                                        acc
                                            )
                                            Dict.empty
                            in
                            State.pure
                                { table = TypeLookupTable.Internal.TLT typesByRange
                                , interface =
                                    { moduleIndex = ctx.thisIndex
                                    , values = exposedValues
                                    , typeAliases = outgoingAliases
                                    }
                                }



-- SOLVING ONE MODULE'S TOP-LEVEL DECLARATIONS


solveModule :
    ModuleCtx
    -> Dict GlobalKey TypeAlias
    -> File
    -> StateM ()
solveModule ctx typeAliases file =
    let
        topLevelFunctions : List ( VarName, ( Node Declaration, Expression.Function ) )
        topLevelFunctions =
            file.declarations
                |> List.filterMap
                    (\declNode ->
                        case Node.value declNode of
                            Declaration.FunctionDeclaration fn ->
                                Just
                                    ( Elm.Syntax.Expression.Extra.functionName fn
                                    , ( declNode, fn )
                                    )

                            _ ->
                                Nothing
                    )

        ( nodeSet, byKey ) =
            List.foldl
                (\( name, member ) ( names, dict ) ->
                    ( Set.insert name names
                    , Dict.insert name member dict
                    )
                )
                ( Set.empty, Dict.empty )
                topLevelFunctions

        edges : VarName -> List VarName
        edges key =
            case Dict.get key byKey of
                Nothing ->
                    []

                Just ( _, fn ) ->
                    Elm.Syntax.Expression.Extra.referencedNames (Node.value (Node.value fn.declaration).expression)
                        -- Resolve operator aliases to the underlying functions
                        |> List.filterMap
                            (\( maybeModuleName, varName ) ->
                                case ModuleLookup.moduleOfVar ctx.index ctx.modules ctx.thisIndex (Maybe.andThen FullModuleName.fromModuleName maybeModuleName) varName of
                                    Ok (Just ( "", fullModuleName )) ->
                                        let
                                            ( resolvedModule, resolvedName ) =
                                                ModuleLookup.resolveOperatorFunction ctx.modules fullModuleName varName
                                                    |> Result.withDefault Nothing
                                                    |> Maybe.withDefault ( fullModuleName, varName )
                                        in
                                        -- Only this module's own declarations
                                        -- are being ordered here; everything
                                        -- else is already in `globalEnv`.
                                        if resolvedModule == ctx.thisIndex.moduleName && Set.member resolvedName nodeSet then
                                            Just resolvedName

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )

        sccs : List (List VarName)
        sccs =
            SCC.stronglyConnectedComponents (Set.toList nodeSet) edges

        inferCtx : Infer.Ctx
        inferCtx =
            { modules = ctx.modules
            , thisModule = ctx.thisIndex
            , typeAliases = typeAliases
            , index = ctx.index
            }
    in
    sccs
        |> State.traverse
            (\group ->
                group
                    |> List.filterMap (\key -> Dict.get key byKey)
                    |> State.traverse (\( declNode, fn ) -> Infer.topLevelMember inferCtx declNode fn)
                    |> State.andThen
                        (BindingGroup.solveGroup
                            (Infer.unifyConfigForGroup inferCtx group)
                        )
            )
        |> State.map (always ())



-- REGISTERING A MODULE'S DECLARATIONS


gatherTypeAliases : ModuleCtx -> File -> StateM (Dict GlobalKey TypeAlias)
gatherTypeAliases ctx file =
    let
        resolver : TypeResolver
        resolver =
            ctx.resolver

        moduleName : FullModuleName
        moduleName =
            ctx.thisIndex.moduleName
    in
    file.declarations
        |> State.traverse
            (\declarationNode ->
                case Node.value declarationNode of
                    Declaration.AliasDeclaration typeAlias ->
                        let
                            toError : ErrorDetails -> Error
                            toError details =
                                { moduleName = FullModuleName.toModuleName moduleName
                                , declarationNames = [ Node.value typeAlias.name ]
                                , details = details
                                }

                            type_ : StateM MonoType
                            type_ =
                                typeAlias.typeAnnotation
                                    |> Node.value
                                    |> TypeI.fromTypeAnnotation resolver
                                    |> Result.mapError (State.error << toError << TypeI.fromTypeAnnotationError)
                                    |> Result.map State.pure
                                    |> Result.Extra.merge

                            -- A record type alias also gets a constructor function
                            -- (eg. `type alias Foo = { a : Int }` lets you write `Foo 1`).
                            registerConstructor : MonoType -> StateM ()
                            registerConstructor aliasMono =
                                case Node.value typeAlias.typeAnnotation of
                                    TypeAnnotation.Record fields ->
                                        fields
                                            |> State.traverse
                                                (\fieldNode ->
                                                    Tuple.second (Node.value fieldNode)
                                                        |> Node.value
                                                        |> TypeI.fromTypeAnnotation resolver
                                                        |> Result.mapError (State.error << toError << TypeI.fromTypeAnnotationError)
                                                        |> Result.map State.pure
                                                        |> Result.Extra.merge
                                                )
                                            |> State.map
                                                (\fieldTypes ->
                                                    fieldTypes
                                                        |> List.foldr (\fieldT acc -> Function { from = fieldT, to = acc }) aliasMono
                                                )
                                            |> State.andThen
                                                (\ctorType ->
                                                    State.addGlobalBinding
                                                        ( "", moduleName, Node.value typeAlias.name )
                                                        (TypeI.closeOver ctorType)
                                                )

                                    _ ->
                                        State.pure ()
                        in
                        State.do type_ <|
                            \type__ ->
                                State.do (registerConstructor type__) <|
                                    \() ->
                                        State.pure <|
                                            Just
                                                ( ( "", moduleName, Node.value typeAlias.name )
                                                , { args = List.map (Node.value >> TypeVar.parse) typeAlias.generics
                                                  , type_ = type__
                                                  }
                                                )

                    _ ->
                        State.pure Nothing
            )
        |> State.map (Maybe.Extra.values >> Dict.fromList)


registerConstructorsAndPorts : ModuleCtx -> File -> StateM ()
registerConstructorsAndPorts ctx file =
    file.declarations
        |> State.traverse
            (\declNode ->
                case Node.value declNode of
                    Declaration.CustomTypeDeclaration customType ->
                        registerCustomType ctx.resolver ctx.thisIndex.moduleName customType

                    Declaration.PortDeclaration sig ->
                        registerPort ctx.resolver ctx.thisIndex.moduleName sig

                    _ ->
                        State.pure ()
            )
        |> State.map (always ())


registerCustomType :
    TypeResolver
    -> FullModuleName
    -> SyntaxType.Type
    -> StateM ()
registerCustomType resolver moduleName customType =
    let
        typeName : String
        typeName =
            Node.value customType.name

        toError : ErrorDetails -> Error
        toError details =
            { moduleName = FullModuleName.toModuleName moduleName
            , declarationNames = [ typeName ]
            , details = details
            }

        resultType : MonoType
        resultType =
            UserDefinedType
                { package = ""
                , moduleName = moduleName
                , name = typeName
                , args =
                    customType.generics
                        |> List.map
                            (\g ->
                                TypeVar
                                    (TypeVar.parse (Node.value g))
                            )
                }
    in
    customType.constructors
        |> State.traverse
            (\ctorNode ->
                let
                    ctor : SyntaxType.ValueConstructor
                    ctor =
                        Node.value ctorNode

                    argTypes : Result FromTypeAnnotationError (List MonoType)
                    argTypes =
                        ctor.arguments
                            |> List.map (Node.value >> TypeI.fromTypeAnnotation resolver)
                            |> Result.Extra.combine
                in
                argTypes
                    |> Result.mapError (State.error << toError << TypeI.fromTypeAnnotationError)
                    |> Result.map
                        (\args ->
                            let
                                ctorName : String
                                ctorName =
                                    Node.value ctor.name

                                ctorType : MonoType
                                ctorType =
                                    List.foldr (\argT acc -> Function { from = argT, to = acc }) resultType args
                            in
                            State.addGlobalBinding ( "", moduleName, ctorName ) (TypeI.closeOver ctorType)
                        )
                    |> Result.Extra.merge
            )
        |> State.map (always ())


registerPort : TypeResolver -> FullModuleName -> Signature -> StateM ()
registerPort resolver moduleName sig =
    let
        toError : ErrorDetails -> Error
        toError details =
            { moduleName = FullModuleName.toModuleName moduleName
            , declarationNames = [ Node.value sig.name ]
            , details = details
            }
    in
    sig.typeAnnotation
        |> Node.value
        |> TypeI.fromTypeAnnotation resolver
        |> Result.mapError (State.error << toError << TypeI.fromTypeAnnotationError)
        |> Result.map
            (\t ->
                State.addGlobalBinding
                    ( "", moduleName, Node.value sig.name )
                    (TypeI.closeOver t)
            )
        |> Result.Extra.merge



-- TYPE INTERNING


{-| Lossless string key for a public `Type`, used to deduplicate identical
types inside one module's `TypeLookupTable` so the table stores one shared
object per distinct type instead of one copy per range.

`Dict.toList` is already sorted, so equal field sets give equal keys
regardless of insertion order. Every embedded string is length-prefixed
(`strKey`) so concatenations stay injective.

Examples:

    publicTypeKey Int
    --> "2;"

    publicTypeKey (TypeVar "a")
    --> "0;1:a"

    publicTypeKey (Function { from = Int, to = Bool })
    --> "1;2:2;2:6;"

    publicTypeKey (List Int)
    --> "7;2:2;"

    publicTypeKey (Record { fields = Dict.fromList [ ( "x", Int ) ] })
    --> "11;9:1;1:x2:2;"

    publicTypeKey (ExtensibleRecord { fields = Dict.fromList [ ( "x", Int ) ], extensionTypevar = "r" })
    --> "12;1:r9:1;1:x2:2;"

    publicTypeKey (Named { package = "elm/core", moduleName = [ "Maybe" ], name = "Maybe", arguments = [ Int ] })
    --> "13;8:elm/core9:1;5:Maybe5:Maybe6:1;2:2;"

Field order doesn't matter — both give the same key:

    publicTypeKey (Record { fields = Dict.fromList [ ( "a", Bool ), ( "b", Int ) ] })
        == publicTypeKey (Record { fields = Dict.fromList [ ( "b", Int ), ( "a", Bool ) ] })
    --> True

-}
publicTypeKey : Type -> String
publicTypeKey t =
    case t of
        Type.TypeVar n ->
            "0;" ++ strKey n

        Type.Function { from, to } ->
            "1;" ++ strKey (publicTypeKey from) ++ strKey (publicTypeKey to)

        Type.Int ->
            "2;"

        Type.Float ->
            "3;"

        Type.Char ->
            "4;"

        Type.String ->
            "5;"

        Type.Bool ->
            "6;"

        Type.List inner ->
            "7;" ++ strKey (publicTypeKey inner)

        Type.Unit ->
            "8;"

        Type.Tuple2 a b ->
            "9;" ++ strKey (publicTypeKey a) ++ strKey (publicTypeKey b)

        Type.Tuple3 a b c ->
            "10;" ++ strKey (publicTypeKey a) ++ strKey (publicTypeKey b) ++ strKey (publicTypeKey c)

        Type.Record { fields } ->
            "11;" ++ strKey (recordFieldsKey fields)

        Type.ExtensibleRecord { fields, extensionTypevar } ->
            "12;" ++ strKey extensionTypevar ++ strKey (recordFieldsKey fields)

        Type.Named { package, moduleName, name, arguments } ->
            "13;"
                ++ strKey package
                ++ strKey (moduleNameKey moduleName)
                ++ strKey name
                ++ strKey (typeArgsKey arguments)

        Type.WebGLShader r ->
            "14;"
                ++ strKey (recordFieldsKey r.attributesFields)
                ++ maybeStrKey r.attributesExtensionTypevar
                ++ strKey (recordFieldsKey r.uniformsFields)
                ++ maybeStrKey r.uniformsExtensionTypevar
                ++ strKey (recordFieldsKey r.varyingsFields)
                ++ maybeStrKey r.varyingsExtensionTypevar


{-| Length-prefix a string so concatenated keys stay injective.

Examples:

    strKey "Int"
    --> "3:Int"

    strKey ""
    --> "0:"

The prefix keeps splits unambiguous:

    strKey "ab" ++ strKey "c"
    --> "2:ab1:c"

    strKey "a" ++ strKey "bc"
    --> "1:a2:bc"

-}
strKey : String -> String
strKey s =
    String.fromInt (String.length s)
        ++ ":"
        ++ s


{-| Key for a module name: segment count plus length-prefixed segments.

Examples:

    moduleNameKey [ "List" ]
    --> "1;4:List"

    moduleNameKey []
    --> "0;"

    moduleNameKey [ "Maybe", "Extra" ]
    --> "2;5:Maybe5:Extra"

-}
moduleNameKey : ModuleName -> String
moduleNameKey parts =
    String.fromInt (List.length parts)
        ++ ";"
        ++ String.concat (List.map strKey parts)


{-| Key for type arguments: argument count plus length-prefixed `publicTypeKey`s.

Examples:

    typeArgsKey []
    --> "0;"

    typeArgsKey [ Int ]
    --> "1;2:2;"

    typeArgsKey [ Int, Bool ]
    --> "2;2:2;2:6;"

-}
typeArgsKey : List Type -> String
typeArgsKey args =
    String.fromInt (List.length args)
        ++ ";"
        ++ String.concat (List.map (\a -> strKey (publicTypeKey a)) args)


{-| Key for record fields: field count plus sorted length-prefixed pairs.

Examples:

    recordFieldsKey Dict.empty
    --> "0;"

    recordFieldsKey (Dict.fromList [ ( "x", Type.Int ) ])
    --> "1;1:x2:2;"

Insertion order doesn't matter:

    recordFieldsKey (Dict.fromList [ ( "a", Type.Bool ), ( "b", Type.Int ) ])
    --> "2;1:a2:6;1:b2:2;"

    recordFieldsKey (Dict.fromList [ ( "a", Type.Bool ), ( "b", Type.Int ) ])
        == recordFieldsKey (Dict.fromList [ ( "b", Type.Int ), ( "a", Type.Bool ) ])
    --> True

-}
recordFieldsKey : Dict String Type -> String
recordFieldsKey fields =
    String.fromInt (Dict.size fields)
        ++ ";"
        ++ String.concat
            (List.map
                (\( k, v ) -> strKey k ++ strKey (publicTypeKey v))
                (Dict.toList fields)
            )


{-| Key for an optional extension type variable, with a tag so `Nothing`
can't collide with `Just`.

Examples:

    maybeStrKey Nothing
    --> "0;"

    maybeStrKey (Just "r")
    --> "1;1:r"

-}
maybeStrKey : Maybe String -> String
maybeStrKey m =
    case m of
        Nothing ->
            "0;"

        Just s ->
            "1;" ++ strKey s
