module NoUnused.Exports exposing (rule)

{-| Forbid the use of exposed elements that are never used in your project.


# Rule

@docs rule

-}

-- TODO Don't report type or type aliases (still `A(..)` though) if they are
-- used in exposed function arguments/return values.

import Dict exposing (Dict)
import Elm.Module
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Error, Rule)
import Scope
import Set exposing (Set)


{-| Report functions and types that are exposed from a module but that are never
used in other modules.

If the project is a package and the module that declared the element is exposed,
then nothing will be reported.

    config =
        [ NoUnused.Exports.rule
        ]

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.Exports" initialProjectContext
        |> Scope.addProjectVisitors
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withFinalProjectEvaluation finalEvaluationForProject
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withExpressionVisitor expressionVisitor



-- CONTEXT


type alias ProjectContext =
    { scope : Scope.ProjectContext
    , projectType : ProjectType
    , modules :
        Dict ModuleName
            { moduleKey : Rule.ModuleKey
            , exposed : Dict String ExposedElement
            }
    , used : Set ( ModuleName, String )
    }


type alias ExposedElement =
    { range : Range
    , rangeToRemove : Maybe Range
    , elementType : ExposedElementType
    }


type ProjectType
    = IsApplication
    | IsPackage (Set (List String))


type ExposedElementType
    = Function
    | TypeOrTypeAlias
    | ExposedType


type alias ModuleContext =
    { scope : Scope.ModuleContext
    , exposesEverything : Bool
    , exposed : Dict String ExposedElement
    , used : Set ( ModuleName, String )
    , elementsNotToReport : Set String
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { scope = Scope.initialProjectContext
    , projectType = IsApplication
    , modules = Dict.empty
    , used = Set.empty
    }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule _ _ projectContext =
    { scope = Scope.fromProjectToModule projectContext.scope
    , exposesEverything = False
    , exposed = Dict.empty
    , used = Set.empty
    , elementsNotToReport = Set.empty
    }


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey moduleName moduleContext =
    { scope = Scope.fromModuleToProject moduleName moduleContext.scope
    , projectType = IsApplication
    , modules =
        Dict.singleton
            (Node.value moduleName)
            { moduleKey = moduleKey
            , exposed = moduleContext.exposed
            }
    , used =
        moduleContext.elementsNotToReport
            |> Set.map (Tuple.pair <| Node.value moduleName)
            |> Set.union moduleContext.used
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { scope = Scope.foldProjectContexts newContext.scope previousContext.scope
    , projectType = previousContext.projectType
    , modules = Dict.union previousContext.modules newContext.modules
    , used = Set.union newContext.used previousContext.used
    }


registerAsUsed : ( ModuleName, String ) -> ModuleContext -> ModuleContext
registerAsUsed ( moduleName, name ) moduleContext =
    { moduleContext | used = Set.insert ( moduleName, name ) moduleContext.used }


registerMultipleAsUsed : List ( ModuleName, String ) -> ModuleContext -> ModuleContext
registerMultipleAsUsed usedElements moduleContext =
    { moduleContext | used = Set.union (Set.fromList usedElements) moduleContext.used }



-- ELM JSON VISITOR


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject projectContext =
    case maybeProject |> Maybe.map .project of
        Just (Elm.Project.Package { exposed }) ->
            let
                exposedModuleNames : List Elm.Module.Name
                exposedModuleNames =
                    case exposed of
                        Elm.Project.ExposedList names ->
                            names

                        Elm.Project.ExposedDict fakeDict ->
                            List.concatMap Tuple.second fakeDict
            in
            ( []
            , { projectContext
                | projectType =
                    exposedModuleNames
                        |> List.map (Elm.Module.toString >> String.split ".")
                        |> Set.fromList
                        |> IsPackage
              }
            )

        _ ->
            ( [], { projectContext | projectType = IsApplication } )



-- PROJECT EVALUATION


finalEvaluationForProject : ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluationForProject projectContext =
    projectContext.modules
        |> removeExposedPackages projectContext
        |> Dict.toList
        |> List.concatMap
            (\( moduleName, { moduleKey, exposed } ) ->
                exposed
                    |> removeApplicationExceptions projectContext
                    |> removeReviewConfig moduleName
                    |> Dict.filter (\name _ -> not <| Set.member ( moduleName, name ) projectContext.used)
                    |> Dict.toList
                    |> List.concatMap
                        (\( name, element ) ->
                            let
                                what : String
                                what =
                                    case element.elementType of
                                        Function ->
                                            "Exposed function or value"

                                        TypeOrTypeAlias ->
                                            "Exposed type or type alias"

                                        ExposedType ->
                                            "Exposed type"

                                fixes : List Fix
                                fixes =
                                    case element.rangeToRemove of
                                        Just rangeToRemove ->
                                            [ Fix.removeRange rangeToRemove ]

                                        Nothing ->
                                            []
                            in
                            [ Rule.errorForModuleWithFix moduleKey
                                { message = what ++ " `" ++ name ++ "` is never used outside this module."
                                , details = [ "This exposed element is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
                                }
                                element.range
                                fixes
                            ]
                        )
            )


removeExposedPackages : ProjectContext -> Dict ModuleName a -> Dict ModuleName a
removeExposedPackages projectContext dict =
    case projectContext.projectType of
        IsApplication ->
            dict

        IsPackage exposedModuleNames ->
            Dict.filter (\name _ -> not <| Set.member name exposedModuleNames) dict


removeApplicationExceptions : ProjectContext -> Dict String a -> Dict String a
removeApplicationExceptions projectContext dict =
    case projectContext.projectType of
        IsApplication ->
            Dict.remove "main" dict

        IsPackage _ ->
            dict


removeReviewConfig : ModuleName -> Dict String a -> Dict String a
removeReviewConfig moduleName dict =
    if moduleName == [ "ReviewConfig" ] then
        Dict.remove "config" dict

    else
        dict



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List nothing, ModuleContext )
moduleDefinitionVisitor moduleNode moduleContext =
    case Module.exposingList (Node.value moduleNode) of
        Exposing.All _ ->
            ( [], { moduleContext | exposesEverything = True } )

        Exposing.Explicit list ->
            ( [], { moduleContext | exposed = collectExposedElements list } )


collectExposedElements : List (Node Exposing.TopLevelExpose) -> Dict String ExposedElement
collectExposedElements nodes =
    let
        listWithPreviousRange : List (Maybe Range)
        listWithPreviousRange =
            Nothing
                :: (nodes
                        |> List.map (Node.range >> Just)
                        |> List.take (List.length nodes - 1)
                   )

        listWithNextRange : List Range
        listWithNextRange =
            (nodes
                |> List.map Node.range
                |> List.drop 1
            )
                ++ [ { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } } ]
    in
    nodes
        |> List.map3 (\prev next current -> ( prev, current, next )) listWithPreviousRange listWithNextRange
        |> List.indexedMap
            (\index ( maybePreviousRange, Node range value, nextRange ) ->
                let
                    rangeToRemove : Maybe Range
                    rangeToRemove =
                        if List.length nodes == 1 then
                            Nothing

                        else if index == 0 then
                            Just { range | end = nextRange.start }

                        else
                            case maybePreviousRange of
                                Nothing ->
                                    Just range

                                Just previousRange ->
                                    Just { range | start = previousRange.end }
                in
                case value of
                    Exposing.FunctionExpose name ->
                        Just
                            ( name
                            , { range = untilEndOfVariable name range
                              , rangeToRemove = rangeToRemove
                              , elementType = Function
                              }
                            )

                    Exposing.TypeOrAliasExpose name ->
                        Just
                            ( name
                            , { range = untilEndOfVariable name range
                              , rangeToRemove = rangeToRemove
                              , elementType = TypeOrTypeAlias
                              }
                            )

                    Exposing.TypeExpose { name } ->
                        Just
                            ( name
                            , { range = untilEndOfVariable name range
                              , rangeToRemove = Nothing
                              , elementType = ExposedType
                              }
                            )

                    Exposing.InfixExpose _ ->
                        Nothing
            )
        |> List.filterMap identity
        |> Dict.fromList


untilEndOfVariable : String -> Range -> Range
untilEndOfVariable name range =
    if range.start.row == range.end.row then
        range

    else
        { range | end = { row = range.start.row, column = range.start.column + String.length name } }



-- IMPORT VISITOR


importVisitor : Node Import -> ModuleContext -> ( List nothing, ModuleContext )
importVisitor node moduleContext =
    case (Node.value node).exposingList |> Maybe.map Node.value of
        Just (Exposing.Explicit list) ->
            let
                moduleName : ModuleName
                moduleName =
                    Node.value (Node.value node).moduleName

                usedElements : List ( ModuleName, String )
                usedElements =
                    list
                        |> List.filterMap
                            (Node.value
                                >> (\element ->
                                        case element of
                                            Exposing.FunctionExpose name ->
                                                Just ( moduleName, name )

                                            Exposing.TypeOrAliasExpose name ->
                                                Just ( moduleName, name )

                                            Exposing.TypeExpose { name } ->
                                                Just ( moduleName, name )

                                            Exposing.InfixExpose _ ->
                                                Nothing
                                   )
                            )
            in
            ( [], registerMultipleAsUsed usedElements moduleContext )

        Just (Exposing.All _) ->
            ( [], moduleContext )

        Nothing ->
            ( [], moduleContext )



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor declarations moduleContext =
    let
        declaredNames : Set String
        declaredNames =
            declarations
                |> List.filterMap (Node.value >> declarationName)
                |> Set.fromList

        typesUsedInDeclaration_ : List ( List ( ModuleName, String ), Bool )
        typesUsedInDeclaration_ =
            declarations
                |> List.map (typesUsedInDeclaration moduleContext)

        testFunctions : List String
        testFunctions =
            declarations
                |> List.filterMap (testFunctionName moduleContext.scope)

        allUsedTypes : List ( ModuleName, String )
        allUsedTypes =
            typesUsedInDeclaration_
                |> List.concatMap Tuple.first

        contextWithUsedElements : ModuleContext
        contextWithUsedElements =
            registerMultipleAsUsed allUsedTypes moduleContext
    in
    ( []
    , { contextWithUsedElements
        | exposed =
            contextWithUsedElements.exposed
                |> (if moduleContext.exposesEverything then
                        identity

                    else
                        Dict.filter (\name _ -> Set.member name declaredNames)
                   )
        , elementsNotToReport =
            typesUsedInDeclaration_
                |> List.concatMap
                    (\( list, comesFromCustomTypeWithHiddenConstructors ) ->
                        if comesFromCustomTypeWithHiddenConstructors then
                            []

                        else
                            List.filter (\( moduleName, name ) -> isType name && moduleName == []) list
                    )
                |> List.map Tuple.second
                |> List.append testFunctions
                |> Set.fromList
      }
    )


isType : String -> Bool
isType string =
    case String.uncons string of
        Nothing ->
            False

        Just ( char, _ ) ->
            Char.isUpper char


declarationName : Declaration -> Maybe String
declarationName declaration =
    case declaration of
        Declaration.FunctionDeclaration function ->
            function.declaration
                |> Node.value
                |> .name
                |> Node.value
                |> Just

        Declaration.CustomTypeDeclaration type_ ->
            Just <| Node.value type_.name

        Declaration.AliasDeclaration alias_ ->
            Just <| Node.value alias_.name

        Declaration.PortDeclaration port_ ->
            Just <| Node.value port_.name

        Declaration.InfixDeclaration { operator } ->
            Just <| Node.value operator

        Declaration.Destructuring _ _ ->
            Nothing


testFunctionName : Scope.ModuleContext -> Node Declaration -> Maybe String
testFunctionName scope declaration =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            case
                function.signature
                    |> Maybe.map (Node.value >> .typeAnnotation >> Node.value)
            of
                Just (TypeAnnotation.Typed (Node _ ( moduleName, name )) _) ->
                    if name == "Test" && Scope.moduleNameForType scope name moduleName == [ "Test" ] then
                        function.declaration
                            |> Node.value
                            |> .name
                            |> Node.value
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


typesUsedInDeclaration : ModuleContext -> Node Declaration -> ( List ( ModuleName, String ), Bool )
typesUsedInDeclaration moduleContext declaration =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            ( function.signature
                |> Maybe.map (Node.value >> .typeAnnotation >> collectTypesFromTypeAnnotation moduleContext.scope)
                |> Maybe.withDefault []
            , False
            )

        Declaration.CustomTypeDeclaration type_ ->
            ( type_.constructors
                |> List.concatMap (Node.value >> .arguments)
                |> List.concatMap (collectTypesFromTypeAnnotation moduleContext.scope)
            , not <|
                case Dict.get (Node.value type_.name) moduleContext.exposed |> Maybe.map .elementType of
                    Just ExposedType ->
                        True

                    _ ->
                        False
            )

        Declaration.AliasDeclaration alias_ ->
            ( collectTypesFromTypeAnnotation moduleContext.scope alias_.typeAnnotation, False )

        Declaration.PortDeclaration _ ->
            ( [], False )

        Declaration.InfixDeclaration _ ->
            ( [], False )

        Declaration.Destructuring _ _ ->
            ( [], False )


collectTypesFromTypeAnnotation : Scope.ModuleContext -> Node TypeAnnotation -> List ( ModuleName, String )
collectTypesFromTypeAnnotation scope node =
    case Node.value node of
        TypeAnnotation.FunctionTypeAnnotation a b ->
            collectTypesFromTypeAnnotation scope a ++ collectTypesFromTypeAnnotation scope b

        TypeAnnotation.Typed (Node _ ( moduleName, name )) params ->
            ( Scope.moduleNameForType scope name moduleName, name )
                :: List.concatMap (collectTypesFromTypeAnnotation scope) params

        TypeAnnotation.Record list ->
            list
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap (collectTypesFromTypeAnnotation scope)

        TypeAnnotation.GenericRecord name list ->
            list
                |> Node.value
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap (collectTypesFromTypeAnnotation scope)

        TypeAnnotation.Tupled list ->
            List.concatMap (collectTypesFromTypeAnnotation scope) list

        TypeAnnotation.GenericType _ ->
            []

        TypeAnnotation.Unit ->
            []



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> Rule.Direction -> ModuleContext -> ( List nothing, ModuleContext )
expressionVisitor node direction moduleContext =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Expression.FunctionOrValue moduleName name ) ->
            ( []
            , registerAsUsed
                ( Scope.moduleNameForValue moduleContext.scope name moduleName, name )
                moduleContext
            )

        _ ->
            ( [], moduleContext )
