module Scope exposing
    ( ModuleContext, addModuleVisitors, initialModuleContext
    , ProjectContext, addProjectVisitors
    , initialProjectContext, fromProjectToModule, fromModuleToProject, foldProjectContexts
    , moduleNameForValue, moduleNameForType
    )

{-| Collect and infer information automatically for you


# Adding to a module rule

@docs ModuleContext, addModuleVisitors, initialModuleContext


# Adding to a project rule

@docs ProjectContext, addProjectVisitors
@docs initialProjectContext, fromProjectToModule, fromModuleToProject, foldProjectContexts


# Access

@docs moduleNameForValue, moduleNameForType

-}

{- Copied over from https://github.com/jfmengels/elm-review-scope

   Version: 0.2.0

   Copyright (c) 2020, Jeroen Engels
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice, this
     list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

   * Neither the name of elm-review-scope nor the names of its
     contributors may be used to endorse or promote products derived from
     this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Direction)
import Set exposing (Set)



-- MODULE VISITOR


{-| The context the Scope visitors will collect and store in your `moduleContext`.
-}
type ModuleContext
    = ModuleContext InnerModuleContext


type alias InnerModuleContext =
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


{-| Create an initial `moduleContext` for the scope for module rules. Use this value when
initializing the scope inside your `initialModuleContext`.

Using [`Scope.addModuleVisitors`](#addModuleVisitors) requires your module context
to be a record with a `scope : Scope.ModuleContext` field.

    type alias ModuleContext =
        { scope : Scope.ModuleContext

        -- ...other fields
        }

    initialModuleContext : ModuleContext
    initialModuleContext =
        { scope = Scope.initialModuleContext

        -- ...other fields
        }

**NOTE**: If you are building a project rule, don't use this value inside your
`fromProjectToModule` function. Instead, use [`Scope.fromProjectToModule`](#fromProjectToModule).

-}
initialModuleContext : ModuleContext
initialModuleContext =
    fromProjectToModule initialProjectContext



-- PROJECT VISITOR


{-| The context the Scope visitors will collect and store in your `projectContext`.
-}
type ProjectContext
    = ProjectContext InnerProjectContext


type alias InnerProjectContext =
    { dependenciesModules : Dict String Elm.Docs.Module
    , modules : Dict ModuleName Elm.Docs.Module
    }


{-| Create an initial `projectContext` for the scope for project rules. Use this value when
initializing the scope inside your `initialProjectContext`.

Using [`Scope.addProjectVisitors`](#addProjectVisitors) requires your project context
to be a record with a `scope : Scope.ProjectContext` field.

Look at the [`Scope.addProjectVisitors`](#addProjectVisitors) example for the
wiring logic related to `withModuleContext` that you can copy-paste then adapt to your needs.

    type alias ProjectContext =
        { scope : Scope.ProjectContext

        -- ...other fields
        }

    initialProjectContext : ProjectContext
    initialProjectContext =
        { scope = Scope.initialProjectContext
        , otherFields = ()
        }

-}
initialProjectContext : ProjectContext
initialProjectContext =
    ProjectContext
        { dependenciesModules = Dict.empty
        , modules = Dict.empty
        }


{-| Get a `Scope.ModuleContext` from a `Scope.ProjectContext`. Use this in your own
`fromProjectToModule`.

    fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
    fromProjectToModule moduleKey moduleName projectContext =
        { scope = Scope.fromProjectToModule projectContext.scope

        -- ...other fields
        }

-}
fromProjectToModule : ProjectContext -> ModuleContext
fromProjectToModule (ProjectContext projectContext) =
    { scopes = nonemptyList_fromElement emptyScope
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
        |> ModuleContext


{-| Get a `Scope.ProjectContext` from a `Scope.ModuleContext`. Use this in your own
`fromModuleToProject`.

    fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
    fromModuleToProject moduleKey moduleName moduleContext =
        { scope = Scope.fromModuleToProject moduleName moduleContext.scope

        -- ...other fields
        }

-}
fromModuleToProject : Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleName (ModuleContext moduleContext) =
    ProjectContext
        { dependenciesModules = moduleContext.dependenciesModules
        , modules =
            Dict.insert (Node.value moduleName)
                { name = String.join "." (Node.value moduleName)
                , comment = ""
                , unions = moduleContext.exposedUnions
                , aliases = moduleContext.exposedAliases
                , values = moduleContext.exposedValues
                , binops = moduleContext.exposedBinops
                }
                moduleContext.modules
        }


{-| Fold `Scope.ProjectContext`s. Use this in your own `foldProjectContexts`.

    foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
    foldProjectContexts newContext previousContext =
        { scope = Scope.foldProjectContexts newContext.scope previousContext.scope

        -- ...other fields
        }

-}
foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts (ProjectContext a) (ProjectContext b) =
    ProjectContext
        { dependenciesModules = Dict.union b.dependenciesModules a.dependenciesModules
        , modules = Dict.union b.modules a.modules
        }



-- SCOPE


type alias Scope =
    { names : Dict String VariableInfo
    , cases : List ( Node Expression, Dict String VariableInfo )
    , caseToExit : Node Expression
    }


emptyScope : Scope
emptyScope =
    { names = Dict.empty
    , cases = []
    , caseToExit = Node Range.emptyRange (Expression.Literal "root")
    }


{-| Adds the scope visitors to your project rule.

Using `addProjectVisitors` requires your project context
to be a record with a `scope : Scope.ProjectContext` field.

**NOTE**: You need to use this function **before** your other visitors, otherwise
the scope may not be up-to-date when you access it.

Adding project visitors adds a bit of wiring, but you can pretty much copy-paste
the code below and adapt it to your needs.

    rule : Rule
    rule =
        Rule.newProjectRuleSchema "RuleName" initialProjectContext
            |> Scope.addProjectVisitors
            -- |> addOtherVisitors
            |> Rule.withModuleContext
                { fromProjectToModule = fromProjectToModule
                , fromModuleToProject = fromModuleToProject
                , foldProjectContexts = foldProjectContexts
                }
            |> Rule.fromProjectRuleSchema

    type alias ProjectContext =
        { scope : Scope.ProjectContext

        -- ...other fields
        }

    type alias ModuleContext =
        { scope : Scope.ModuleContext

        -- ...other fields
        }

    initialProjectContext : ProjectContext
    initialProjectContext =
        { scope = Scope.initialProjectContext

        -- ...other fields
        }

    fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
    fromProjectToModule moduleKey moduleName projectContext =
        { scope = Scope.fromProjectToModule projectContext.scope

        -- ...other fields
        }

    fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
    fromModuleToProject moduleKey moduleName moduleContext =
        { scope = Scope.fromModuleToProject moduleName moduleContext.scope

        -- ...other fields
        }

    foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
    foldProjectContexts newContext previousContext =
        { scope = Scope.foldProjectContexts newContext.scope previousContext.scope

        -- ...other fields
        }

-}
addProjectVisitors :
    Rule.ProjectRuleSchema { schemaState | canAddModuleVisitor : () } { projectContext | scope : ProjectContext } { moduleContext | scope : ModuleContext }
    -> Rule.ProjectRuleSchema { schemaState | canAddModuleVisitor : (), hasAtLeastOneVisitor : (), withModuleContext : Rule.Required } { projectContext | scope : ProjectContext } { moduleContext | scope : ModuleContext }
addProjectVisitors schema =
    schema
        |> Rule.withContextFromImportedModules
        |> Rule.withDependenciesProjectVisitor (mapInnerProjectContext dependenciesProjectVisitor)
        |> Rule.withModuleVisitor internalAddModuleVisitors


{-| Adds the scope visitors to your module rule.

Using `addModuleVisitors` requires your module context
to be a record with a `scope : Scope.ModuleContext` field.

**NOTE**: You need to use this function **before** your other visitors, otherwise
the scope may not be up-to-date when you access it.

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "RuleName" initialContext
            -- Scope.addModuleVisitors needs to be added before your own visitors
            |> Scope.addModuleVisitors
            -- |> addOtherVisitors
            |> Rule.fromModuleRuleSchema

    type alias Context =
        -- Scope expects a context with a record, containing the `scope` field.
        { scope : Scope.ModuleContext

        -- ...other fields
        }

    initialContext : Context
    initialContext =
        { scope = Scope.initialModuleContext

        -- ...other fields
        }

-}
addModuleVisitors :
    Rule.ModuleRuleSchema { schemaState | canCollectProjectData : () } { moduleContext | scope : ModuleContext }
    -> Rule.ModuleRuleSchema { schemaState | canCollectProjectData : (), hasAtLeastOneVisitor : () } { moduleContext | scope : ModuleContext }
addModuleVisitors schema =
    schema
        |> Rule.withDependenciesModuleVisitor (mapInnerModuleContext dependenciesModuleVisitor)
        |> internalAddModuleVisitors


internalAddModuleVisitors : Rule.ModuleRuleSchema schemaState { moduleContext | scope : ModuleContext } -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } { moduleContext | scope : ModuleContext }
internalAddModuleVisitors schema =
    schema
        |> Rule.withModuleDefinitionVisitor
            (mapInnerModuleContext moduleDefinitionVisitor |> pairWithNoErrors)
        |> Rule.withImportVisitor
            (mapInnerModuleContext importVisitor |> pairWithNoErrors)
        |> Rule.withDeclarationListVisitor
            (mapInnerModuleContext declarationListVisitor |> pairWithNoErrors)
        |> Rule.withDeclarationVisitor
            (\visitedElement direction outerContext ->
                let
                    innerContext : InnerModuleContext
                    innerContext =
                        outerContext.scope
                            |> unboxModule
                            |> declarationVisitor visitedElement direction
                in
                ( [], { outerContext | scope = ModuleContext innerContext } )
            )
        |> Rule.withExpressionVisitor
            (\visitedElement direction outerContext ->
                let
                    innerContext : InnerModuleContext
                    innerContext =
                        outerContext.scope
                            |> unboxModule
                            |> popScope visitedElement direction
                            |> expressionVisitor visitedElement direction
                in
                ( [], { outerContext | scope = ModuleContext innerContext } )
            )


mapInnerProjectContext : (visitedElement -> InnerProjectContext -> InnerProjectContext) -> visitedElement -> { projectContext | scope : ProjectContext } -> ( List nothing, { projectContext | scope : ProjectContext } )
mapInnerProjectContext visitor visitedElement outerContext =
    let
        innerContext : InnerProjectContext
        innerContext =
            outerContext.scope
                |> unboxProjectContext
                |> visitor visitedElement
    in
    ( [], { outerContext | scope = ProjectContext innerContext } )


mapInnerModuleContext : (visitedElement -> InnerModuleContext -> InnerModuleContext) -> visitedElement -> { moduleContext | scope : ModuleContext } -> { moduleContext | scope : ModuleContext }
mapInnerModuleContext visitor visitedElement outerContext =
    let
        innerContext : InnerModuleContext
        innerContext =
            outerContext.scope
                |> unboxModule
                |> visitor visitedElement
    in
    { outerContext | scope = ModuleContext innerContext }


pairWithNoErrors : (visited -> context -> context) -> visited -> context -> ( List nothing, context )
pairWithNoErrors fn visited context =
    ( [], fn visited context )



-- DEPENDENCIES


dependenciesProjectVisitor : Dict String Dependency -> InnerProjectContext -> InnerProjectContext
dependenciesProjectVisitor dependencies innerContext =
    internalDependenciesVisitor dependencies innerContext


dependenciesModuleVisitor : Dict String Dependency -> InnerModuleContext -> InnerModuleContext
dependenciesModuleVisitor dependencies innerContext =
    internalDependenciesVisitor dependencies innerContext
        |> registerPrelude


internalDependenciesVisitor : Dict String Dependency -> { context | dependenciesModules : Dict String Elm.Docs.Module } -> { context | dependenciesModules : Dict String Elm.Docs.Module }
internalDependenciesVisitor dependencies innerContext =
    let
        dependenciesModules : Dict String Elm.Docs.Module
        dependenciesModules =
            dependencies
                |> Dict.values
                |> List.concatMap Dependency.modules
                |> List.map (\dependencyModule -> ( dependencyModule.name, dependencyModule ))
                |> Dict.fromList
    in
    { innerContext | dependenciesModules = dependenciesModules }


registerPrelude : InnerModuleContext -> InnerModuleContext
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


declarationListVisitor : List (Node Declaration) -> InnerModuleContext -> InnerModuleContext
declarationListVisitor declarations innerContext =
    List.foldl registerDeclaration innerContext declarations


registerDeclaration : Node Declaration -> InnerModuleContext -> InnerModuleContext
registerDeclaration declaration innerContext =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            let
                nameNode : Node String
                nameNode =
                    function.declaration
                        |> Node.value
                        |> .name
            in
            innerContext
                |> addToScope
                    { variableType = TopLevelVariable
                    , node = nameNode
                    }
                |> registerIfExposed (registerExposedValue function) (Node.value nameNode)

        Declaration.AliasDeclaration alias_ ->
            { innerContext | localTypes = Set.insert (Node.value alias_.name) innerContext.localTypes }
                |> addToScope
                    { variableType = TopLevelVariable
                    , node = alias_.name
                    }
                |> registerIfExposed (registerExposedTypeAlias alias_) (Node.value alias_.name)

        Declaration.CustomTypeDeclaration { name, constructors } ->
            List.foldl
                (\constructor innerContext_ ->
                    let
                        constructorName : Node String
                        constructorName =
                            constructor |> Node.value |> .name
                    in
                    addToScope
                        { variableType = CustomTypeConstructor
                        , node = constructorName
                        }
                        innerContext_
                )
                { innerContext | localTypes = Set.insert (Node.value name) innerContext.localTypes }
                constructors
                |> registerIfExposed (registerExposedCustomType constructors) (Node.value name)

        Declaration.PortDeclaration signature ->
            addToScope
                { variableType = Port
                , node = signature.name
                }
                innerContext

        Declaration.InfixDeclaration _ ->
            -- TODO Support operators
            -- I could use help adding this.
            innerContext

        Declaration.Destructuring _ _ ->
            -- Not possible in 0.19 code
            innerContext


addToScope : { variableType : VariableType, node : Node String } -> InnerModuleContext -> InnerModuleContext
addToScope variableData innerContext =
    let
        newScopes : Nonempty Scope
        newScopes =
            registerVariable
                variableData
                (Node.value variableData.node)
                innerContext.scopes
    in
    { innerContext | scopes = newScopes }


registerExposedValue : Expression.Function -> String -> InnerModuleContext -> InnerModuleContext
registerExposedValue function name innerContext =
    { innerContext
        | exposedValues =
            { name = name
            , comment =
                case Maybe.map Node.value function.documentation of
                    Just str ->
                        str

                    Nothing ->
                        ""
            , tipe = convertTypeSignatureToDocsType function.signature
            }
                :: innerContext.exposedValues
    }


registerExposedCustomType : List (Node Elm.Syntax.Type.ValueConstructor) -> String -> InnerModuleContext -> InnerModuleContext
registerExposedCustomType constructors name innerContext =
    { innerContext
        | exposedUnions =
            { name = name
            , comment = ""

            -- TODO
            , args = []
            , tags =
                constructors
                    -- TODO Constructor args?
                    |> List.map (\constructor -> ( Node.value (Node.value constructor).name, [] ))
            }
                :: innerContext.exposedUnions
    }


registerExposedTypeAlias : Elm.Syntax.TypeAlias.TypeAlias -> String -> InnerModuleContext -> InnerModuleContext
registerExposedTypeAlias alias_ name innerContext =
    { innerContext
        | exposedAliases =
            { name = name
            , comment = ""
            , args = []
            , tipe = Elm.Type.Tuple []
            }
                :: innerContext.exposedAliases
    }


registerIfExposed : (String -> InnerModuleContext -> InnerModuleContext) -> String -> InnerModuleContext -> InnerModuleContext
registerIfExposed registerFn name innerContext =
    if innerContext.exposesEverything || Dict.member name innerContext.exposedNames then
        registerFn name innerContext

    else
        innerContext


convertTypeSignatureToDocsType : Maybe (Node Signature) -> Elm.Type.Type
convertTypeSignatureToDocsType maybeSignature =
    case maybeSignature |> Maybe.map (Node.value >> .typeAnnotation) of
        Just typeAnnotation ->
            syntaxTypeAnnotationToDocsType typeAnnotation

        Nothing ->
            Elm.Type.Tuple []


syntaxTypeAnnotationToDocsType : Node TypeAnnotation -> Elm.Type.Type
syntaxTypeAnnotationToDocsType (Node _ typeAnnotation) =
    case typeAnnotation of
        TypeAnnotation.GenericType name ->
            Elm.Type.Var name

        TypeAnnotation.Typed (Node _ ( moduleName, typeName )) typeParameters ->
            -- Elm.Type.Type (String.join "." moduleName ++ "." ++ typeName) (List.map syntaxTypeAnnotationToDocsType typeParameters)
            Elm.Type.Tuple []

        TypeAnnotation.Unit ->
            Elm.Type.Tuple []

        TypeAnnotation.Tupled typeAnnotationTypeAnnotationSyntaxElmNodeNodeSyntaxElmListList ->
            Elm.Type.Tuple []

        TypeAnnotation.Record recordDefinitionTypeAnnotationSyntaxElm ->
            Elm.Type.Tuple []

        TypeAnnotation.GenericRecord stringStringNodeNodeSyntaxElm recordDefinitionTypeAnnotationSyntaxElmNodeNodeSyntaxElm ->
            Elm.Type.Tuple []

        TypeAnnotation.FunctionTypeAnnotation typeAnnotationTypeAnnotationSyntaxElmNodeNodeSyntaxElm typeAnnotationTypeAnnotationSyntaxElmNodeNodeSyntaxElm2 ->
            Elm.Type.Tuple []


registerVariable : VariableInfo -> String -> Nonempty Scope -> Nonempty Scope
registerVariable variableInfo name scopes =
    nonemptyList_mapHead
        (\scope -> { scope | names = Dict.insert name variableInfo scope.names })
        scopes


updateScope : InnerModuleContext -> Nonempty Scope -> InnerModuleContext
updateScope innerContext scopes =
    { innerContext | scopes = scopes }



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> InnerModuleContext -> InnerModuleContext
moduleDefinitionVisitor node innerContext =
    case Module.exposingList (Node.value node) of
        Exposing.All _ ->
            { innerContext | exposesEverything = True }

        Exposing.Explicit list ->
            { innerContext | exposedNames = exposedElements list }


exposedElements : List (Node Exposing.TopLevelExpose) -> Dict String Range
exposedElements nodes =
    nodes
        |> List.filterMap
            (\node ->
                case Node.value node of
                    Exposing.FunctionExpose name ->
                        Just ( name, Node.range node )

                    Exposing.TypeOrAliasExpose name ->
                        Just ( name, Node.range node )

                    Exposing.TypeExpose { name } ->
                        Just ( name, Node.range node )

                    Exposing.InfixExpose name ->
                        Nothing
            )
        |> Dict.fromList



-- IMPORT VISITOR


importVisitor : Node Import -> InnerModuleContext -> InnerModuleContext
importVisitor (Node _ import_) innerContext =
    innerContext
        |> registerImportAlias import_
        |> registerImportExposed import_


registerImportAlias : Import -> InnerModuleContext -> InnerModuleContext
registerImportAlias import_ innerContext =
    case import_.moduleAlias of
        Nothing ->
            let
                moduleName : List String
                moduleName =
                    Node.value import_.moduleName
            in
            case moduleName of
                singleSegmentModuleName :: [] ->
                    { innerContext
                        | importAliases =
                            Dict.update
                                singleSegmentModuleName
                                (\previousValue -> Just <| moduleName :: Maybe.withDefault [] previousValue)
                                innerContext.importAliases
                    }

                _ ->
                    innerContext

        Just alias_ ->
            { innerContext
                | importAliases =
                    Dict.update
                        (Node.value alias_ |> getModuleName)
                        (\previousValue -> Just <| Node.value import_.moduleName :: Maybe.withDefault [] previousValue)
                        innerContext.importAliases
            }


registerImportExposed : Import -> InnerModuleContext -> InnerModuleContext
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
                    (case Dict.get (getModuleName moduleName) innerContext.dependenciesModules of
                        Just m ->
                            Just m

                        Nothing ->
                            Dict.get moduleName innerContext.modules
                    )
                        |> Maybe.withDefault
                            { name = getModuleName moduleName
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


unboxProjectContext : ProjectContext -> InnerProjectContext
unboxProjectContext (ProjectContext context) =
    context


unboxModule : ModuleContext -> InnerModuleContext
unboxModule (ModuleContext context) =
    context


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


declarationVisitor : Node Declaration -> Rule.Direction -> InnerModuleContext -> InnerModuleContext
declarationVisitor declaration direction context =
    case ( direction, Node.value declaration ) of
        ( Rule.OnEnter, Declaration.FunctionDeclaration function ) ->
            let
                newScope : Scope
                newScope =
                    { emptyScope | names = parameters <| .arguments <| Node.value function.declaration }
            in
            context.scopes
                |> nonemptyList_cons newScope
                |> updateScope context

        ( Rule.OnExit, Declaration.FunctionDeclaration function ) ->
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


popScope : Node Expression -> Direction -> InnerModuleContext -> InnerModuleContext
popScope node direction context =
    let
        currentScope : Scope
        currentScope =
            nonemptyList_head context.scopes
    in
    case direction of
        Rule.OnEnter ->
            let
                caseExpression : Maybe ( Node Expression, Dict String VariableInfo )
                caseExpression =
                    findInList (\( expressionNode, _ ) -> node == expressionNode) currentScope.cases
            in
            case caseExpression of
                Nothing ->
                    context

                Just ( _, names ) ->
                    { context | scopes = nonemptyList_cons { emptyScope | names = names, caseToExit = node } context.scopes }

        Rule.OnExit ->
            if node == currentScope.caseToExit then
                { context | scopes = nonemptyList_pop context.scopes }

            else
                context


expressionVisitor : Node Expression -> Direction -> InnerModuleContext -> InnerModuleContext
expressionVisitor (Node _ value) direction context =
    case ( direction, value ) of
        ( Rule.OnEnter, Expression.LetExpression { declarations, expression } ) ->
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

                        Expression.LetDestructuring pattern _ ->
                            scopes
                )
                (nonemptyList_cons emptyScope context.scopes)
                declarations
                |> updateScope context

        ( Rule.OnExit, Expression.LetExpression _ ) ->
            { context | scopes = nonemptyList_pop context.scopes }

        ( Rule.OnEnter, Expression.CaseExpression caseBlock ) ->
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

        ( Rule.OnExit, Expression.CaseExpression caseBlock ) ->
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

    expressionVisitor : Node Expression -> Direction -> Context -> ( List (Error {}), Context )
    expressionVisitor node direction context =
        case ( direction, Node.value node ) of
            ( Rule.OnEnter, Expression.FunctionOrValue moduleName "button" ) ->
                if Scope.moduleNameForValue context.scope "button" moduleName == [ "Html" ] then
                    ( [ createError node ], context )

                else
                    ( [], context )

            _ ->
                ( [], context )

-}
moduleNameForValue : ModuleContext -> String -> List String -> List String
moduleNameForValue (ModuleContext context) valueName moduleName =
    case moduleName of
        [] ->
            if isInScope valueName context.scopes then
                []

            else
                Dict.get valueName context.importedFunctions
                    |> Maybe.withDefault []

        _ :: [] ->
            case Dict.get (getModuleName moduleName) context.importAliases of
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
moduleNameForType (ModuleContext context) typeName moduleName =
    case moduleName of
        [] ->
            if Set.member typeName context.localTypes then
                []

            else
                Dict.get typeName context.importedTypes
                    |> Maybe.withDefault []

        _ :: [] ->
            case Dict.get (getModuleName moduleName) context.importAliases of
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


getModuleName : List String -> String
getModuleName name =
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
nonemptyList_head (Nonempty x xs) =
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
