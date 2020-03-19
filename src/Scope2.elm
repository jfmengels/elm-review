module Scope2 exposing
    ( ProjectContext, ModuleContext
    , addProjectVisitors, initialProjectContext, fromProjectToModule, fromModuleToProject, foldProjectContexts
    , realFunctionOrType
    )

{-| Report variables or types that are declared or imported but never used.


# Definition

@docs ProjectContext, ModuleContext


# Usage

@docs addProjectVisitors, addModuleVisitors, initialProjectContext, fromProjectToModule, fromModuleToProject, foldProjectContexts


# Access

@docs realFunctionOrType

-}

-- TODO Re-add the nice "can't make mistakes" addVisitors

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
import NonemptyList exposing (Nonempty)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Direction)



-- DEFINITION
{-
   TODO To make everything less error-prone:

   Wrap the following in a helper from Scope:

      Scope.addVisitors setterGetter
       ({ moduleVisitor =
           \schema ->
               schema
                   |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor

       , fromProjectToModule = fromProjectToModule
       , fromModuleToProject = fromModuleToProject
       , foldProjectContexts = foldProjectContexts
       })

    Need to fine-tune the details on how that would work obviously.

-}


type ProjectContext
    = ProjectContext InnerProjectContext


type alias InnerProjectContext =
    { dependenciesModules : Dict String Elm.Docs.Module
    , modules : Dict ModuleName Elm.Docs.Module
    }


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


type alias Scope =
    { names : Dict String VariableInfo
    , cases : List ( Node Expression, Dict String VariableInfo )
    , caseToExit : Node Expression
    }



-- USAGE


initialProjectContext : ProjectContext
initialProjectContext =
    ProjectContext
        { dependenciesModules = Dict.empty
        , modules = Dict.empty
        }


fromProjectToModule : ProjectContext -> ModuleContext
fromProjectToModule (ProjectContext projectContext) =
    { scopes = NonemptyList.fromElement emptyScope
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
        { dependenciesModules = a.dependenciesModules
        , modules = Dict.union a.modules b.modules
        }


emptyScope : Scope
emptyScope =
    { names = Dict.empty
    , cases = []
    , caseToExit = Node Range.emptyRange (Expression.Literal "root")
    }


addProjectVisitors :
    Rule.ProjectRuleSchema { projectContext | scope : ProjectContext } { moduleContext | scope : ModuleContext } { schemaState | canAddModuleVisitor : () }
    -> Rule.ProjectRuleSchema { projectContext | scope : ProjectContext } { moduleContext | scope : ModuleContext } { schemaState | canAddModuleVisitor : (), withModuleContext : Rule.Required }
addProjectVisitors schema =
    schema
        |> Rule.withContextFromImportedModules
        |> Rule.withDependenciesProjectVisitor (mapInnerProjectContext dependenciesVisitor)
        |> Rule.withModuleVisitor addModuleVisitors


addModuleVisitors : Rule.ModuleRuleSchema anything { moduleContext | scope : ModuleContext } -> Rule.ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } { moduleContext | scope : ModuleContext }
addModuleVisitors schema =
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



-- scopedRule :
--     String
--     ->
--         { forGlobal :
--             { get : projectContext -> ProjectContext
--             , set : ProjectContext -> projectContext -> projectContext
--             }
--         , forModule :
--             { get : moduleContext -> ModuleContext
--             , set : ModuleContext -> moduleContext -> moduleContext
--             }
--         }
--     ->
--         { moduleVisitor : Rule.ModuleRuleSchema Rule.ForLookingAtSeveralFiles { hasNoVisitor : () } moduleContext -> Rule.ModuleRuleSchema Rule.ForLookingAtSeveralFiles { hasAtLeastOneVisitor : () } moduleContext
--         , fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> projectContext -> moduleContext
--         , fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> moduleContext -> projectContext
--         , foldProjectContexts : projectContext -> projectContext -> projectContext
--         }
--     -> Rule.ProjectRuleSchema projectContext moduleContext
-- scopedRule name setterGetters context =
--     Rule.newProjectRuleSchema name


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


dependenciesVisitor : Dict String Dependency -> InnerProjectContext -> InnerProjectContext
dependenciesVisitor dependencies innerContext =
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

        Declaration.PortDeclaration port_ ->
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
    NonemptyList.mapHead
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
importVisitor (Node range import_) innerContext =
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
                                -- TODO HERE We notice that the module has no tags or union, even though it should
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
                |> NonemptyList.cons newScope
                |> updateScope context

        ( Rule.OnExit, Declaration.FunctionDeclaration function ) ->
            { context | scopes = NonemptyList.pop context.scopes }

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
            NonemptyList.head context.scopes
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
                    { context | scopes = NonemptyList.cons { emptyScope | names = names, caseToExit = node } context.scopes }

        Rule.OnExit ->
            if node == currentScope.caseToExit then
                { context | scopes = NonemptyList.pop context.scopes }

            else
                context


expressionVisitor : Node Expression -> Direction -> InnerModuleContext -> InnerModuleContext
expressionVisitor ((Node range value) as node) direction context =
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
                (NonemptyList.cons emptyScope context.scopes)
                declarations
                |> updateScope context

        ( Rule.OnExit, Expression.LetExpression _ ) ->
            { context | scopes = NonemptyList.pop context.scopes }

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
            { context | scopes = NonemptyList.mapHead (\scope -> { scope | cases = cases }) context.scopes }

        ( Rule.OnExit, Expression.CaseExpression caseBlock ) ->
            { context | scopes = NonemptyList.mapHead (\scope -> { scope | cases = [] }) context.scopes }

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


realFunctionOrType : List String -> String -> ModuleContext -> ( List String, String )
realFunctionOrType moduleName functionOrType (ModuleContext context) =
    if List.length moduleName == 0 then
        ( if isInScope functionOrType context.scopes then
            []

          else
            Dict.get functionOrType context.importedFunctionOrTypes
                |> Maybe.withDefault []
        , functionOrType
        )

    else if List.length moduleName == 1 then
        ( Dict.get (getModuleName moduleName) context.importAliases
            |> Maybe.withDefault moduleName
        , functionOrType
        )

    else
        ( moduleName, functionOrType )


isInScope : String -> Nonempty Scope -> Bool
isInScope name scopes =
    NonemptyList.any (.names >> Dict.member name) scopes



-- MISC


getModuleName : List String -> String
getModuleName name =
    String.join "." name
