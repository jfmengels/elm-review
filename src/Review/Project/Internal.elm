module Review.Project.Internal exposing
    ( Project(..)
    , ProjectModule
    , buildModuleGraph
    , computeModuleNameLookupTables
    , getModuleName
    , moduleGraph
    , moduleNameLookupTables
    , sourceDirectories
    )

{-| Holds all the information related to the project such as the contents of
the `elm.json` file, the project modules and the project dependencies.
-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.File
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.ModuleNameLookupTable.Internal as ModuleNameLookupTableInternal
import Review.Project.Dependency as Dependency exposing (Dependency)
import Set exposing (Set)
import Vendor.Graph as Graph exposing (Graph)


type Project
    = Project
        { modules : Dict String ProjectModule
        , modulesThatFailedToParse : List { path : String, source : String }
        , elmJson : Maybe { path : String, raw : String, project : Elm.Project.Project }
        , readme : Maybe { path : String, content : String }
        , dependencies : Dict String Dependency
        , moduleGraph : Maybe (Graph ModuleName ())
        , sourceDirectories : List String
        , moduleNameLookupTables : Maybe (Dict ModuleName ModuleNameLookupTable)
        }


{-| Represents a parsed file.
-}
type alias ProjectModule =
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    , isInSourceDirectories : Bool
    }


{-| Get the module graph for the project in the form of a
[`elm-community/graph` Graph]. This is used by `Review.Rule` internally.

The dependency is actually copied into the project, which means that you won't
be able to use this value, even if you add `elm-community/graph` as a dependency.
This is an unfortunately visible implementation detail, it is not meant for you
to use.

[`elm-community/graph` Graph]: https://package.elm-lang.org/packages/elm-community/graph/6.0.0/Graph#Graph

-}
moduleGraph : Project -> Graph (List String) ()
moduleGraph (Project project) =
    case project.moduleGraph of
        Just graph ->
            graph

        Nothing ->
            buildModuleGraph <| Dict.values project.modules


moduleNameLookupTables : Graph (List String) () -> Project -> Dict ModuleName ModuleNameLookupTable
moduleNameLookupTables graph ((Project project) as rawProject) =
    case project.moduleNameLookupTables of
        Nothing ->
            graph
                |> Graph.checkAcyclic
                |> Result.map Graph.topologicalSort
                |> Result.withDefault []
                |> computeModuleNameLookupTables rawProject

        Just moduleNameLookupTables_ ->
            moduleNameLookupTables_


sourceDirectories : Project -> List String
sourceDirectories (Project project) =
    project.sourceDirectories


buildModuleGraph : List ProjectModule -> Graph ModuleName ()
buildModuleGraph mods =
    let
        moduleIds : Dict ModuleName Int
        moduleIds =
            mods
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( index, module_ ) dict ->
                        Dict.insert
                            (getModuleName module_)
                            index
                            dict
                    )
                    Dict.empty

        getModuleId : ModuleName -> Int
        getModuleId moduleName =
            case Dict.get moduleName moduleIds of
                Just moduleId ->
                    moduleId

                Nothing ->
                    getModuleId moduleName

        ( nodes, edges ) =
            mods
                |> List.foldl
                    (\module_ ( resNodes, resEdges ) ->
                        let
                            ( moduleNode, modulesEdges ) =
                                nodesAndEdges
                                    (\moduleName -> Dict.get moduleName moduleIds)
                                    module_
                                    (getModuleId <| getModuleName module_)
                        in
                        ( moduleNode :: resNodes, List.concat [ modulesEdges, resEdges ] )
                    )
                    ( [], [] )
    in
    Graph.fromNodesAndEdges nodes edges


nodesAndEdges : (ModuleName -> Maybe Int) -> ProjectModule -> Int -> ( Graph.Node ModuleName, List (Graph.Edge ()) )
nodesAndEdges getModuleId module_ moduleId =
    let
        moduleName : ModuleName
        moduleName =
            getModuleName module_
    in
    ( Graph.Node moduleId moduleName
    , importedModules module_
        |> List.filterMap getModuleId
        |> List.map
            (\importedModuleId ->
                Graph.Edge importedModuleId moduleId ()
            )
    )


importedModules : ProjectModule -> List ModuleName
importedModules module_ =
    module_.ast.imports
        |> List.map (Node.value >> .moduleName >> Node.value)


getModuleName : ProjectModule -> ModuleName
getModuleName module_ =
    module_.ast.moduleDefinition
        |> Node.value
        |> Elm.Syntax.Module.moduleName


type alias ProjectContext =
    { dependenciesModules : Dict String Elm.Docs.Module
    , modules : Dict ModuleName Elm.Docs.Module
    }


computeModuleNameLookupTables : Project -> List (Graph.NodeContext ModuleName ()) -> Dict ModuleName ModuleNameLookupTable
computeModuleNameLookupTables (Project project) nodeContexts =
    let
        dependenciesModules : Dict String Elm.Docs.Module
        dependenciesModules =
            project.dependencies
                |> Dict.values
                |> List.concatMap Dependency.modules
                |> List.map (\dependencyModule -> ( dependencyModule.name, dependencyModule ))
                |> Dict.fromList

        projectContext : ProjectContext
        projectContext =
            { dependenciesModules = dependenciesModules
            , modules = Dict.empty
            }

        modules : Dict ModuleName ProjectModule
        modules =
            List.foldl
                (\module_ dict ->
                    Dict.insert
                        (getModuleName module_)
                        module_
                        dict
                )
                Dict.empty
                (Dict.values project.modules)
    in
    nodeContexts
        |> List.filterMap
            (\nodeContext ->
                case Dict.get nodeContext.node.label modules of
                    Just module_ ->
                        Just ( nodeContext.node.label, computeModuleNameLookupTable projectContext module_.ast nodeContext )

                    Nothing ->
                        -- TODO Fail here?
                        Nothing
            )
        |> Dict.fromList


computeModuleNameLookupTable : ProjectContext -> Elm.Syntax.File.File -> Graph.NodeContext ModuleName () -> ModuleNameLookupTable
computeModuleNameLookupTable projectContext ast { node, incoming } =
    let
        exposedForModule : ExposedForModule
        exposedForModule =
            case Node.value ast.moduleDefinition |> Elm.Syntax.Module.exposingList of
                Exposing.All _ ->
                    Everything

                Exposing.Explicit list ->
                    Specific (exposedElements list)

        moduleContext : ModuleContext
        moduleContext =
            { scopes = Nonempty emptyScope []
            , localTypes = Set.empty
            , importAliases = Dict.empty
            , importedFunctions = Dict.empty
            , importedTypes = Dict.empty
            , dependenciesModules = projectContext.dependenciesModules
            , modules = projectContext.modules
            , exposesEverything = False
            , exposedNames = Dict.empty
            , exposedUnions = []
            , exposedAliases = []
            , exposedValues = []
            , exposedBinops = []
            }
                |> registerPrelude
    in
    ModuleNameLookupTableInternal.empty


registerPrelude : ModuleContext -> ModuleContext
registerPrelude innerContext =
    List.foldl registerImportExposed innerContext elmCorePrelude


elmCorePrelude : List Import
elmCorePrelude =
    let
        explicit : List TopLevelExpose -> Maybe Exposing
        explicit exposed =
            exposed
                |> List.map (Node Range.emptyRange)
                |> Exposing.Explicit
                |> Just
    in
    -- These are the default imports implicitly added by the Elm compiler
    -- https://package.elm-lang.org/packages/elm/core/latest
    [ createFakeImport
        { moduleName = [ "Basics" ]
        , moduleAlias = Nothing
        , exposingList = Just <| Exposing.All Range.emptyRange
        }
    , createFakeImport
        { moduleName = [ "List" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "List", open = Nothing }
                , Exposing.InfixExpose "::"
                ]
        }
    , createFakeImport
        { moduleName = [ "Maybe" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Maybe", open = Just Range.emptyRange }
                ]
        }
    , createFakeImport
        { moduleName = [ "Result" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Result", open = Just Range.emptyRange }
                ]
        }
    , createFakeImport
        { moduleName = [ "String" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Char", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Char" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Tuple" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Debug" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Platform" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Program", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Platform", "Cmd" ]
        , moduleAlias = Just "Cmd"
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Cmd", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Platform", "Sub" ]
        , moduleAlias = Just "Sub"
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Sub", open = Nothing }
                ]
        }
    ]


createFakeImport : { moduleName : List String, exposingList : Maybe Exposing, moduleAlias : Maybe String } -> Import
createFakeImport { moduleName, moduleAlias, exposingList } =
    { moduleName = Node Range.emptyRange moduleName
    , moduleAlias = moduleAlias |> Maybe.map (List.singleton >> Node Range.emptyRange)
    , exposingList = exposingList |> Maybe.map (Node Range.emptyRange)
    }


type alias ModuleContext =
    { scopes : Nonempty Scope
    , localTypes : Set String
    , importAliases : Dict String (List ModuleName)
    , importedFunctions : Dict String (List String)
    , importedTypes : Dict String (List String)
    , dependenciesModules : Dict String Elm.Docs.Module
    , modules : Dict ModuleName Elm.Docs.Module
    , exposesEverything : Bool
    , exposedNames : Dict String Range
    , exposedUnions : List Elm.Docs.Union
    , exposedAliases : List Elm.Docs.Alias
    , exposedValues : List Elm.Docs.Value
    , exposedBinops : List Elm.Docs.Binop
    }


type alias Scope =
    { names : Dict String VariableInfo
    , cases : List ( Node Expression, Dict String VariableInfo )
    , caseToExit : Node Expression
    }


emptyScope : Scope
emptyScope =
    { names = Dict.empty
    , cases = []
    , caseToExit = Node Range.emptyRange Expression.UnitExpr
    }


type alias VariableInfo =
    { variableType : VariableType
    , node : Node String
    }


type VariableType
    = TopLevelVariable
    | CustomTypeConstructor
    | FunctionParameter
    | LetVariable
    | PatternVariable
    | Port


type ExposedForModule
    = Everything
    | Specific (Set String)


exposedElements : List (Node Exposing.TopLevelExpose) -> Set String
exposedElements nodes =
    nodes
        |> List.filterMap
            (\node ->
                case Node.value node of
                    Exposing.FunctionExpose name ->
                        Just name

                    Exposing.TypeOrAliasExpose name ->
                        Just name

                    Exposing.TypeExpose { name } ->
                        Just name

                    Exposing.InfixExpose _ ->
                        Nothing
            )
        |> Set.fromList


registerImportExposed : Import -> ModuleContext -> ModuleContext
registerImportExposed import_ innerContext =
    case import_.exposingList |> Maybe.map Node.value of
        Nothing ->
            innerContext

        Just exposing_ ->
            let
                moduleName : List String
                moduleName =
                    Node.value import_.moduleName

                module_ : Elm.Docs.Module
                module_ =
                    (case Dict.get (joinModuleName moduleName) innerContext.dependenciesModules of
                        Just m ->
                            Just m

                        Nothing ->
                            Dict.get moduleName innerContext.modules
                    )
                        |> Maybe.withDefault
                            { name = joinModuleName moduleName
                            , comment = ""
                            , unions = []
                            , values = []
                            , aliases = []
                            , binops = []
                            }
            in
            case exposing_ of
                Exposing.All _ ->
                    let
                        nameWithModuleName : { r | name : String } -> ( String, List String )
                        nameWithModuleName { name } =
                            ( name, moduleName )

                        exposedValues : Dict String (List String)
                        exposedValues =
                            List.concat
                                [ List.concatMap
                                    (\union ->
                                        List.map (\( name, _ ) -> ( name, moduleName )) union.tags
                                    )
                                    module_.unions
                                , List.map nameWithModuleName module_.values
                                , List.map nameWithModuleName module_.aliases
                                , List.map nameWithModuleName module_.binops
                                ]
                                |> Dict.fromList

                        exposedTypes : Dict String (List String)
                        exposedTypes =
                            List.concat
                                [ List.map nameWithModuleName module_.unions
                                , List.map nameWithModuleName module_.aliases
                                ]
                                |> Dict.fromList
                    in
                    { innerContext
                        | importedFunctions = Dict.union innerContext.importedFunctions exposedValues
                        , importedTypes = Dict.union innerContext.importedTypes exposedTypes
                    }

                Exposing.Explicit topLevelExposeList ->
                    let
                        exposedValues : Dict String (List String)
                        exposedValues =
                            topLevelExposeList
                                |> List.concatMap (valuesFromExposingList module_)
                                |> List.map (\name -> ( name, moduleName ))
                                |> Dict.fromList

                        exposedTypes : Dict String (List String)
                        exposedTypes =
                            topLevelExposeList
                                |> List.filterMap typesFromExposingList
                                |> List.map (\name -> ( name, moduleName ))
                                |> Dict.fromList
                    in
                    { innerContext
                        | importedFunctions = Dict.union innerContext.importedFunctions exposedValues
                        , importedTypes = Dict.union innerContext.importedTypes exposedTypes
                    }


valuesFromExposingList : Elm.Docs.Module -> Node TopLevelExpose -> List String
valuesFromExposingList module_ topLevelExpose =
    case Node.value topLevelExpose of
        Exposing.InfixExpose operator ->
            [ operator ]

        Exposing.FunctionExpose function ->
            [ function ]

        Exposing.TypeOrAliasExpose name ->
            if List.any (\alias_ -> alias_.name == name) module_.aliases then
                [ name ]

            else
                -- Type is a custom type
                []

        Exposing.TypeExpose { name, open } ->
            case open of
                Just _ ->
                    module_.unions
                        |> List.filter (\union -> union.name == name)
                        |> List.concatMap .tags
                        |> List.map Tuple.first

                Nothing ->
                    []


typesFromExposingList : Node TopLevelExpose -> Maybe String
typesFromExposingList topLevelExpose =
    case Node.value topLevelExpose of
        Exposing.InfixExpose _ ->
            Nothing

        Exposing.FunctionExpose _ ->
            Nothing

        Exposing.TypeOrAliasExpose name ->
            Just name

        Exposing.TypeExpose { name } ->
            Just name


declarationEnterVisitor : Node Declaration -> ModuleContext -> ModuleContext
declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                newScope : Scope
                newScope =
                    { emptyScope | names = parameters <| .arguments <| Node.value function.declaration }
            in
            context.scopes
                |> nonemptyList_cons newScope
                |> updateScope context

        _ ->
            context


updateScope : ModuleContext -> Nonempty Scope -> ModuleContext
updateScope innerContext scopes =
    { innerContext | scopes = scopes }


registerVariable : VariableInfo -> String -> Nonempty Scope -> Nonempty Scope
registerVariable variableInfo name scopes =
    nonemptyList_mapHead
        (\scope -> { scope | names = Dict.insert name variableInfo scope.names })
        scopes


declarationExitVisitor : Node Declaration -> ModuleContext -> ModuleContext
declarationExitVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            { context | scopes = nonemptyList_pop context.scopes }

        _ ->
            context


parameters : List (Node Pattern) -> Dict String VariableInfo
parameters patterns =
    List.concatMap collectNamesFromPattern patterns
        |> List.map
            (\node ->
                ( Node.value node
                , { node = node
                  , variableType = FunctionParameter
                  }
                )
            )
        |> Dict.fromList


collectNamesFromPattern : Node Pattern -> List (Node String)
collectNamesFromPattern pattern =
    case Node.value pattern of
        Pattern.TuplePattern subPatterns ->
            List.concatMap collectNamesFromPattern subPatterns

        Pattern.RecordPattern names ->
            names

        Pattern.UnConsPattern left right ->
            List.concatMap collectNamesFromPattern [ left, right ]

        Pattern.ListPattern subPatterns ->
            List.concatMap collectNamesFromPattern subPatterns

        Pattern.VarPattern name ->
            [ Node (Node.range pattern) name ]

        Pattern.NamedPattern _ subPatterns ->
            List.concatMap collectNamesFromPattern subPatterns

        Pattern.AsPattern subPattern alias_ ->
            alias_ :: collectNamesFromPattern subPattern

        Pattern.ParenthesizedPattern subPattern ->
            collectNamesFromPattern subPattern

        Pattern.AllPattern ->
            []

        Pattern.UnitPattern ->
            []

        Pattern.CharPattern _ ->
            []

        Pattern.StringPattern _ ->
            []

        Pattern.IntPattern _ ->
            []

        Pattern.HexPattern _ ->
            []

        Pattern.FloatPattern _ ->
            []


popScopeEnter : Node Expression -> ModuleContext -> ModuleContext
popScopeEnter node context =
    let
        currentScope : Scope
        currentScope =
            nonemptyList_head context.scopes

        caseExpression : Maybe ( Node Expression, Dict String VariableInfo )
        caseExpression =
            findInList (\( expressionNode, _ ) -> node == expressionNode) currentScope.cases
    in
    case caseExpression of
        Nothing ->
            context

        Just ( _, names ) ->
            { context | scopes = nonemptyList_cons { emptyScope | names = names, caseToExit = node } context.scopes }


popScopeExit : Node Expression -> ModuleContext -> ModuleContext
popScopeExit node context =
    let
        currentScope : Scope
        currentScope =
            nonemptyList_head context.scopes
    in
    if node == currentScope.caseToExit then
        { context | scopes = nonemptyList_pop context.scopes }

    else
        context


expressionEnterVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionEnterVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations, expression } ->
            List.foldl
                (\declaration scopes ->
                    case Node.value declaration of
                        Expression.LetFunction function ->
                            let
                                nameNode : Node String
                                nameNode =
                                    function.declaration
                                        |> Node.value
                                        |> .name
                            in
                            registerVariable
                                { variableType = LetVariable, node = nameNode }
                                -- TODO Check if the name as 2nd arg is not redundant with the 1st argument's node field
                                (Node.value nameNode)
                                scopes

                        Expression.LetDestructuring _ _ ->
                            scopes
                )
                (nonemptyList_cons emptyScope context.scopes)
                declarations
                |> updateScope context

        Expression.CaseExpression caseBlock ->
            let
                cases : List ( Node Expression, Dict String VariableInfo )
                cases =
                    caseBlock.cases
                        |> List.map
                            (\( pattern, expression ) ->
                                ( expression
                                , collectNamesFromPattern pattern
                                    |> List.map
                                        (\node_ ->
                                            ( Node.value node_
                                            , { node = node_
                                              , variableType = PatternVariable
                                              }
                                            )
                                        )
                                    |> Dict.fromList
                                )
                            )
            in
            { context | scopes = nonemptyList_mapHead (\scope -> { scope | cases = cases }) context.scopes }

        _ ->
            context


expressionExitVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionExitVisitor node context =
    case Node.value node of
        Expression.LetExpression _ ->
            { context | scopes = nonemptyList_pop context.scopes }

        Expression.CaseExpression _ ->
            { context | scopes = nonemptyList_mapHead (\scope -> { scope | cases = [] }) context.scopes }

        _ ->
            context


findInList : (a -> Bool) -> List a -> Maybe a
findInList predicate list =
    case list of
        [] ->
            Nothing

        a :: rest ->
            if predicate a then
                Just a

            else
                findInList predicate rest



-- ACCESS


{-| Get the name of the module where a value was defined.
A value can be either a function, a constant, a custom type constructor or a type alias (used as a function).

  - The second argument (`String`) is the name of the value
  - The third argument (`List String`) is the module name that was used next to the value's name where you found it

If the element was defined in the current module, then the result will be `[]`.

    expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
    expressionVisitor node context =
        case Node.value node of
            Expression.FunctionOrValue moduleName "button" ->
                if Scope.moduleNameForValue context.scope "button" moduleName == [ "Html" ] then
                    ( [ createError node ], context )

                else
                    ( [], context )

            _ ->
                ( [], context )

-}
moduleNameForValue : ModuleContext -> String -> List String -> List String
moduleNameForValue context valueName moduleName =
    case moduleName of
        [] ->
            if isInScope valueName context.scopes then
                []

            else
                Dict.get valueName context.importedFunctions
                    |> Maybe.withDefault []

        _ :: [] ->
            case Dict.get (joinModuleName moduleName) context.importAliases of
                Just [ aliasedModuleName ] ->
                    aliasedModuleName

                Just aliases ->
                    case
                        findInList
                            (\aliasedModuleName ->
                                case Dict.get aliasedModuleName context.modules of
                                    Just module_ ->
                                        isValueDeclaredInModule valueName module_

                                    Nothing ->
                                        False
                            )
                            aliases
                    of
                        Just aliasedModuleName ->
                            aliasedModuleName

                        Nothing ->
                            List.head aliases
                                |> Maybe.withDefault moduleName

                Nothing ->
                    moduleName

        _ ->
            moduleName


{-| Get the name of the module where a type was defined.
A type can be either a custom type or a type alias.

  - The second argument (`String`) is the name of the type
  - The third argument (`List String`) is the module name that was used next to the type name where you found it

-}
moduleNameForType : ModuleContext -> String -> List String -> List String
moduleNameForType context typeName moduleName =
    case moduleName of
        [] ->
            if Set.member typeName context.localTypes then
                []

            else
                Dict.get typeName context.importedTypes
                    |> Maybe.withDefault []

        _ :: [] ->
            case Dict.get (joinModuleName moduleName) context.importAliases of
                Just [ aliasedModuleName ] ->
                    aliasedModuleName

                Just aliases ->
                    case
                        findInList
                            (\aliasedModuleName ->
                                case Dict.get aliasedModuleName context.modules of
                                    Just module_ ->
                                        isTypeDeclaredInModule typeName module_

                                    Nothing ->
                                        False
                            )
                            aliases
                    of
                        Just aliasedModuleName ->
                            aliasedModuleName

                        Nothing ->
                            List.head aliases
                                |> Maybe.withDefault moduleName

                Nothing ->
                    moduleName

        _ ->
            moduleName


isValueDeclaredInModule : String -> Elm.Docs.Module -> Bool
isValueDeclaredInModule valueName module_ =
    List.any (.name >> (==) valueName) module_.values
        || List.any (.name >> (==) valueName) module_.aliases
        || List.any
            (\union -> List.any (Tuple.first >> (==) valueName) union.tags)
            module_.unions


isTypeDeclaredInModule : String -> Elm.Docs.Module -> Bool
isTypeDeclaredInModule typeName module_ =
    List.any (.name >> (==) typeName) module_.aliases
        || List.any (.name >> (==) typeName) module_.unions


isInScope : String -> Nonempty Scope -> Bool
isInScope name scopes =
    nonemptyList_any (.names >> Dict.member name) scopes



-- MISC


joinModuleName : List String -> String
joinModuleName name =
    String.join "." name



{- INLINED NONEMPTYLIST

   Copied contents of mgold/elm-nonempty-list, and trimmed down unused functions.

   This is to avoid dependency conflicts when mgold/elm-nonempty-list would release a new major version.

   A list that cannot be empty. The head and tail can be accessed without Maybes. Most other list functions are
   available.


   # Definition

   @docs Nonempty


   # Create

   @docs fromElement


   # Access

   @docs head


   # Inspect

   @docs any


   # Convert

   @docs cons, pop


   # Map

   @docs mapHead


   # Original copyright notice

   Copyright (c) 2015, Max Goldstein

   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

       * Redistributions of source code must retain the above copyright
         notice, this list of conditions and the following disclaimer.

       * Redistributions in binary form must reproduce the above
         copyright notice, this list of conditions and the following
         disclaimer in the documentation and/or other materials provided
         with the distribution.

       * Neither the name of Max Goldstein nor the names of other
         contributors may be used to endorse or promote products derived
         from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}


{-| The Nonempty type. If you have both a head and tail, you can construct a
nonempty list directly. Otherwise use the helpers below instead.
-}
type Nonempty a
    = Nonempty a (List a)


{-| Create a singleton list with the given element.
-}
nonemptyList_fromElement : a -> Nonempty a
nonemptyList_fromElement x =
    Nonempty x []


{-| Return the head of the list.
-}
nonemptyList_head : Nonempty a -> a
nonemptyList_head (Nonempty x _) =
    x


{-| Determine if any elements satisfy the predicate.
-}
nonemptyList_any : (a -> Bool) -> Nonempty a -> Bool
nonemptyList_any f (Nonempty x xs) =
    f x || List.any f xs


{-| Add another element as the head of the list, pushing the previous head to the tail.
-}
nonemptyList_cons : a -> Nonempty a -> Nonempty a
nonemptyList_cons y (Nonempty x xs) =
    Nonempty y (x :: xs)


{-| Pop and discard the head, or do nothing for a singleton list. Useful if you
want to exhaust a list but hang on to the last item indefinitely.
pop (Nonempty 3 [ 2, 1 ]) --> Nonempty 2 [1]
pop (Nonempty 1 []) --> Nonempty 1 []
-}
nonemptyList_pop : Nonempty a -> Nonempty a
nonemptyList_pop (Nonempty x xs) =
    case xs of
        [] ->
            Nonempty x xs

        y :: ys ->
            Nonempty y ys


{-| Map the head to a value of the same type
-}
nonemptyList_mapHead : (a -> a) -> Nonempty a -> Nonempty a
nonemptyList_mapHead fn (Nonempty x xs) =
    Nonempty (fn x) xs
