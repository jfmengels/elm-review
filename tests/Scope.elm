module Scope exposing
    ( ModuleContext, addModuleVisitors, initialModuleContext
    , ProjectContext, addProjectVisitors
    , initialProjectContext, fromProjectToModule, fromModuleToProject, foldProjectContexts
    , realModuleName
    )

{-| Collect and infer information automatically for you


# Adding to a module rule

@docs ModuleContext, addModuleVisitors, initialModuleContext


# Adding to a project rule

@docs ProjectContext, addProjectVisitors
@docs initialProjectContext, fromProjectToModule, fromModuleToProject, foldProjectContexts


# Access

@docs realModuleName

-}

{- Copied over from https://github.com/jfmengels/elm-review-scope

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
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Direction)



-- MODULE VISITOR


type ModuleContext
    = ModuleContext InnerModuleContext


type alias InnerModuleContext =
    { scopes : Nonempty Scope
    , importAliases : Dict String (List String)
    , importedFunctionOrTypes : Dict String (List String)
    , dependenciesModules : Dict String Elm.Docs.Module
    , modules : Dict ModuleName Elm.Docs.Module
    , exposesEverything : Bool
    , exposedNames : Dict String Range
    , exposedUnions : List Elm.Docs.Union
    , exposedAliases : List Elm.Docs.Alias
    , exposedValues : List Elm.Docs.Value
    , exposedBinops : List Elm.Docs.Binop
    }


initialModuleContext : ModuleContext
initialModuleContext =
    fromProjectToModule initialProjectContext



-- PROJECT VISITOR


type ProjectContext
    = ProjectContext InnerProjectContext


type alias InnerProjectContext =
    { dependenciesModules : Dict String Elm.Docs.Module
    , modules : Dict ModuleName Elm.Docs.Module
    }


initialProjectContext : ProjectContext
initialProjectContext =
    ProjectContext
        { dependenciesModules = Dict.empty
        , modules = Dict.empty
        }


fromProjectToModule : ProjectContext -> ModuleContext
fromProjectToModule (ProjectContext projectContext) =
    { scopes = nonemptyList_fromElement emptyScope
    , importAliases = Dict.empty
    , importedFunctionOrTypes = Dict.empty
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


addProjectVisitors :
    Rule.ProjectRuleSchema { schemaState | canAddModuleVisitor : () } { projectContext | scope : ProjectContext } { moduleContext | scope : ModuleContext }
    -> Rule.ProjectRuleSchema { schemaState | canAddModuleVisitor : (), hasAtLeastOneVisitor : (), withModuleContext : Rule.Required } { projectContext | scope : ProjectContext } { moduleContext | scope : ModuleContext }
addProjectVisitors schema =
    schema
        |> Rule.withContextFromImportedModules
        |> Rule.withDependenciesProjectVisitor (mapInnerProjectContext dependenciesProjectVisitor)
        |> Rule.withModuleVisitor internalAddModuleVisitors


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
        |> (\newInnerContext -> List.foldl registerExposed newInnerContext declarations)


registerDeclaration : Node Declaration -> InnerModuleContext -> InnerModuleContext
registerDeclaration declaration innerContext =
    case declarationNameNode declaration of
        Just ( variableType, nameNode ) ->
            innerContext.scopes
                |> registerVariable
                    { variableType = variableType
                    , node = nameNode
                    }
                    (Node.value nameNode)
                |> updateScope innerContext

        Nothing ->
            innerContext


declarationNameNode : Node Declaration -> Maybe ( VariableType, Node String )
declarationNameNode (Node _ declaration) =
    case declaration of
        Declaration.FunctionDeclaration function ->
            Just
                ( TopLevelVariable
                , function.declaration
                    |> Node.value
                    |> .name
                )

        Declaration.CustomTypeDeclaration type_ ->
            Just ( TopLevelVariable, type_.name )

        Declaration.AliasDeclaration alias_ ->
            Just ( TopLevelVariable, alias_.name )

        Declaration.PortDeclaration port_ ->
            Just ( Port, port_.name )

        Declaration.InfixDeclaration _ ->
            Nothing

        Declaration.Destructuring _ _ ->
            Nothing


registerExposed : Node Declaration -> InnerModuleContext -> InnerModuleContext
registerExposed declaration innerContext =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            let
                name : String
                name =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            if innerContext.exposesEverything || Dict.member name innerContext.exposedNames then
                { innerContext
                    | exposedValues =
                        { name = name
                        , comment = ""
                        , tipe = convertTypeSignatureToDocsType function.signature
                        }
                            :: innerContext.exposedValues
                }

            else
                innerContext

        Declaration.CustomTypeDeclaration type_ ->
            if innerContext.exposesEverything || Dict.member (Node.value type_.name) innerContext.exposedNames then
                { innerContext
                    | exposedUnions =
                        { name = Node.value type_.name
                        , comment = ""

                        -- TODO
                        , args = []
                        , tags =
                            type_.constructors
                                -- TODO Constructor args?
                                |> List.map (\constructor -> ( Node.value (Node.value constructor).name, [] ))
                        }
                            :: innerContext.exposedUnions
                }

            else
                innerContext

        Declaration.AliasDeclaration alias_ ->
            if innerContext.exposesEverything || Dict.member (Node.value alias_.name) innerContext.exposedNames then
                { innerContext
                    | exposedAliases =
                        { name = Node.value alias_.name
                        , comment = ""
                        , args = []
                        , tipe = Elm.Type.Tuple []
                        }
                            :: innerContext.exposedAliases
                }

            else
                innerContext

        Declaration.PortDeclaration _ ->
            innerContext

        Declaration.InfixDeclaration _ ->
            innerContext

        Declaration.Destructuring _ _ ->
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
updateScope context scopes =
    { context | scopes = scopes }



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
            innerContext

        Just alias_ ->
            { innerContext
                | importAliases =
                    Dict.insert
                        (Node.value alias_ |> getModuleName)
                        (Node.value import_.moduleName)
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
                                        nameWithModuleName union
                                            :: List.map (\( name, _ ) -> ( name, moduleName )) union.tags
                                    )
                                    module_.unions
                                , List.map nameWithModuleName module_.values
                                , List.map nameWithModuleName module_.aliases
                                , List.map nameWithModuleName module_.binops
                                ]
                                |> Dict.fromList
                    in
                    { innerContext
                        | importedFunctionOrTypes =
                            Dict.union innerContext.importedFunctionOrTypes exposedValues
                    }

                Exposing.Explicit topLevelExposeList ->
                    let
                        exposedValues : Dict String (List String)
                        exposedValues =
                            topLevelExposeList
                                |> List.concatMap (namesFromExposingList module_)
                                |> List.map (\name -> ( name, moduleName ))
                                |> Dict.fromList
                    in
                    { innerContext
                        | importedFunctionOrTypes =
                            Dict.union innerContext.importedFunctionOrTypes exposedValues
                    }


namesFromExposingList : Elm.Docs.Module -> Node TopLevelExpose -> List String
namesFromExposingList module_ topLevelExpose =
    case Node.value topLevelExpose of
        Exposing.InfixExpose operator ->
            [ operator ]

        Exposing.FunctionExpose function ->
            [ function ]

        Exposing.TypeOrAliasExpose type_ ->
            [ type_ ]

        Exposing.TypeExpose { name, open } ->
            case open of
                Just _ ->
                    name
                        :: (module_.unions
                                |> List.filter (\union -> union.name == name)
                                |> List.concatMap .tags
                                |> List.map Tuple.first
                           )

                Nothing ->
                    [ name ]


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
popScope ((Node range value) as node) direction context =
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


realModuleName : ModuleContext -> String -> List String -> List String
realModuleName (ModuleContext context) functionOrType moduleName =
    if List.length moduleName == 0 then
        if isInScope functionOrType context.scopes then
            []

        else
            Dict.get functionOrType context.importedFunctionOrTypes
                |> Maybe.withDefault []

    else if List.length moduleName == 1 then
        Dict.get (getModuleName moduleName) context.importAliases
            |> Maybe.withDefault moduleName

    else
        moduleName


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
