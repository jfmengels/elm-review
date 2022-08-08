module NoUnused.Exports exposing (rule)

{-| Forbid the use of exposed elements that are never used in your project.

@docs rule

-}

-- TODO Don't report type or type aliases (still `A(..)` though) if they are
-- used in exposed function arguments/return values.

import Dict exposing (Dict)
import Elm.Module
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import List.Extra
import NoUnused.LamderaSupport as LamderaSupport
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Report functions and types that are exposed from a module but that are never
used in other modules.

🔧 Running with `--fix` will automatically remove all the reported errors.

If the project is a package and the module that declared the element is exposed,
then nothing will be reported.

    config =
        [ NoUnused.Exports.rule
        ]


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.Exports
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.Exports" initialProjectContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.withElmJsonProjectVisitor (\elmJson context -> ( [], elmJsonVisitor elmJson context ))
        |> Rule.withFinalProjectEvaluation finalEvaluationForProject
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withImportVisitor (\node context -> ( [], importVisitor node context ))
        |> Rule.withDeclarationEnterVisitor (\node context -> ( [], declarationVisitor node context ))
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionVisitor node context ))



-- CONTEXT


type alias ProjectContext =
    { projectType : ProjectType
    , modules :
        Dict
            ModuleName
            { moduleKey : Rule.ModuleKey
            , exposed : Dict String ExposedElement
            }
    , used : Set ( ModuleName, String )
    , constructors : Dict ( ModuleName, String ) String
    }


type alias ExposedElement =
    { range : Range
    , rangesToRemove : List Range
    , elementType : ExposedElementType
    }


type ProjectType
    = IsApplication ElmApplicationType
    | IsPackage (Set (List String))


type ElmApplicationType
    = ElmApplication
    | LamderaApplication


type ExposedElementType
    = Function
    | TypeOrTypeAlias
    | ExposedType (List String)


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , exposed : Dict String ExposedElement
    , used : Set ( ModuleName, String )
    , elementsNotToReport : Set String
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { projectType = IsApplication ElmApplication
    , modules = Dict.empty
    , used = Set.empty
    , constructors = Dict.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable ast moduleDocumentation _ ->
            let
                exposed : Dict String ExposedElement
                exposed =
                    case Module.exposingList (Node.value ast.moduleDefinition) of
                        Exposing.All _ ->
                            Dict.empty

                        Exposing.Explicit explicitlyExposed ->
                            collectExposed moduleDocumentation explicitlyExposed ast.declarations
            in
            { lookupTable = lookupTable
            , exposed = exposed
            , used = Set.empty
            , elementsNotToReport = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withFullAst
        |> Rule.withModuleDocumentation


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleKey moduleName moduleContext ->
            { projectType = IsApplication ElmApplication
            , modules =
                Dict.singleton
                    moduleName
                    { moduleKey = moduleKey
                    , exposed = moduleContext.exposed
                    }
            , used =
                moduleContext.elementsNotToReport
                    |> Set.map (Tuple.pair moduleName)
                    |> Set.union moduleContext.used
            , constructors =
                Dict.foldl
                    (\name element acc ->
                        case element.elementType of
                            ExposedType constructorNames ->
                                List.foldl
                                    (\constructorName listAcc -> Dict.insert ( moduleName, constructorName ) name listAcc)
                                    acc
                                    constructorNames

                            _ ->
                                acc
                    )
                    Dict.empty
                    moduleContext.exposed
            }
        )
        |> Rule.withModuleKey
        |> Rule.withModuleName


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { projectType = previousContext.projectType
    , modules = Dict.union previousContext.modules newContext.modules
    , used = Set.union newContext.used previousContext.used
    , constructors = Dict.union previousContext.constructors newContext.constructors
    }


registerAsUsed : ( ModuleName, String ) -> ModuleContext -> ModuleContext
registerAsUsed ( moduleName, name ) moduleContext =
    { moduleContext | used = Set.insert ( moduleName, name ) moduleContext.used }



-- ELM JSON VISITOR


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ProjectContext
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
            { projectContext
                | projectType =
                    exposedModuleNames
                        |> List.foldr
                            (\moduleName acc ->
                                Set.insert (Elm.Module.toString moduleName |> String.split ".") acc
                            )
                            Set.empty
                        |> IsPackage
            }

        Just (Elm.Project.Application { depsDirect }) ->
            let
                elmApplicationType : ElmApplicationType
                elmApplicationType =
                    if LamderaSupport.isLamderaApplication depsDirect then
                        LamderaApplication

                    else
                        ElmApplication
            in
            { projectContext | projectType = IsApplication elmApplicationType }

        Nothing ->
            { projectContext | projectType = IsApplication ElmApplication }



-- PROJECT EVALUATION


finalEvaluationForProject : ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluationForProject projectContext =
    let
        used : Set ( ModuleName, String )
        used =
            Set.foldl
                (\(( moduleName, _ ) as key) acc ->
                    case Dict.get key projectContext.constructors of
                        Just typeName ->
                            Set.insert ( moduleName, typeName ) acc

                        Nothing ->
                            acc
                )
                projectContext.used
                projectContext.used
    in
    projectContext.modules
        |> removeExposedPackages projectContext
        |> Dict.toList
        |> List.concatMap (errorsForModule projectContext used)


errorsForModule : ProjectContext -> Set ( ModuleName, String ) -> ( ModuleName, { moduleKey : Rule.ModuleKey, exposed : Dict String ExposedElement } ) -> List (Error scope)
errorsForModule projectContext used ( moduleName, { moduleKey, exposed } ) =
    exposed
        |> removeApplicationExceptions projectContext
        |> removeReviewConfig moduleName
        |> Dict.filter (\name _ -> not <| Set.member ( moduleName, name ) used)
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

                            ExposedType _ ->
                                "Exposed type"
                in
                [ Rule.errorForModuleWithFix moduleKey
                    { message = what ++ " `" ++ name ++ "` is never used outside this module."
                    , details = [ "This exposed element is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
                    }
                    element.range
                    (List.map Fix.removeRange element.rangesToRemove)
                ]
            )


removeExposedPackages : ProjectContext -> Dict ModuleName a -> Dict ModuleName a
removeExposedPackages projectContext dict =
    case projectContext.projectType of
        IsApplication _ ->
            dict

        IsPackage exposedModuleNames ->
            Dict.filter (\name _ -> not <| Set.member name exposedModuleNames) dict


removeApplicationExceptions : ProjectContext -> Dict String a -> Dict String a
removeApplicationExceptions projectContext dict =
    case projectContext.projectType of
        IsPackage _ ->
            dict

        IsApplication ElmApplication ->
            Dict.remove "main" dict

        IsApplication LamderaApplication ->
            dict
                |> Dict.remove "main"
                |> Dict.remove "app"


removeReviewConfig : ModuleName -> Dict String a -> Dict String a
removeReviewConfig moduleName dict =
    if moduleName == [ "ReviewConfig" ] then
        Dict.remove "config" dict

    else
        dict


getRangesToRemove : List ( Int, String ) -> List a -> String -> Int -> Maybe Range -> Range -> Range -> List Range
getRangesToRemove comments nodes name index maybePreviousRange range nextRange =
    if List.length nodes == 1 then
        []

    else
        let
            exposeRemoval : Range
            exposeRemoval =
                if index == 0 then
                    { range | end = nextRange.start }

                else
                    case maybePreviousRange of
                        Nothing ->
                            range

                        Just previousRange ->
                            { range | start = previousRange.end }
        in
        List.filterMap identity
            [ Just exposeRemoval
            , findMap (findDocsRangeToRemove name) comments
            ]


findDocsRangeToRemove : String -> ( Int, String ) -> Maybe Range
findDocsRangeToRemove name fullComment =
    case findCommentInMiddle name fullComment of
        Just range ->
            Just range

        Nothing ->
            findCommentAtEnd name fullComment


findCommentInMiddle : String -> ( Int, String ) -> Maybe Range
findCommentInMiddle name ( row, comment ) =
    String.indexes (" " ++ name ++ ", ") comment
        |> List.head
        |> Maybe.map
            (\index ->
                { start = { row = row, column = index + 2 }
                , end = { row = row, column = index + String.length name + 4 }
                }
            )


findCommentAtEnd : String -> ( Int, String ) -> Maybe Range
findCommentAtEnd name ( row, comment ) =
    if comment == "@docs " ++ name then
        Just
            { start = { row = row, column = 1 }
            , end = { row = row + 1, column = 1 }
            }

    else
        String.indexes (", " ++ name) comment
            |> List.head
            |> Maybe.map
                (\index ->
                    { start = { row = row, column = index + 1 }
                    , end = { row = row, column = index + String.length name + 3 }
                    }
                )


findMap : (a -> Maybe b) -> List a -> Maybe b
findMap mapper list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            case mapper first of
                Just value ->
                    Just value

                Nothing ->
                    findMap mapper rest


untilEndOfVariable : String -> Range -> Range
untilEndOfVariable name range =
    if range.start.row == range.end.row then
        range

    else
        { range | end = { row = range.start.row, column = range.start.column + String.length name } }



-- IMPORT VISITOR


importVisitor : Node Import -> ModuleContext -> ModuleContext
importVisitor node moduleContext =
    case (Node.value node).exposingList |> Maybe.map Node.value of
        Just (Exposing.Explicit list) ->
            let
                moduleName : ModuleName
                moduleName =
                    Node.value (Node.value node).moduleName

                usedElements : Set ( ModuleName, String )
                usedElements =
                    List.foldl
                        (\(Node _ element) acc ->
                            case element of
                                Exposing.FunctionExpose name ->
                                    Set.insert ( moduleName, name ) acc

                                Exposing.TypeOrAliasExpose name ->
                                    Set.insert ( moduleName, name ) acc

                                Exposing.TypeExpose { name } ->
                                    Set.insert ( moduleName, name ) acc

                                Exposing.InfixExpose _ ->
                                    acc
                        )
                        moduleContext.used
                        list
            in
            { moduleContext | used = usedElements }

        Just (Exposing.All _) ->
            moduleContext

        Nothing ->
            moduleContext


collectExposed : Maybe (Node String) -> List (Node TopLevelExpose) -> List (Node Declaration) -> Dict String ExposedElement
collectExposed moduleDocumentation explicitlyExposed declarations =
    let
        docsReferences : List ( Int, String )
        docsReferences =
            collectDocsReferences moduleDocumentation
    in
    collectExposedElements docsReferences explicitlyExposed declarations
        |> filterOut declarations


collectDocsReferences : Maybe (Node String) -> List ( Int, String )
collectDocsReferences maybeModuleDocumentation =
    case maybeModuleDocumentation of
        Just (Node range moduleDocumentation) ->
            let
                lines : List String
                lines =
                    moduleDocumentation
                        |> String.lines
                        |> List.drop 1
            in
            List.Extra.indexedFilterMap
                (\lineNumber line ->
                    if String.startsWith "@docs " line then
                        Just ( lineNumber, line )

                    else
                        Nothing
                )
                (range.start.row + 1)
                lines
                []

        Nothing ->
            []


collectExposedElements : List ( Int, String ) -> List (Node Exposing.TopLevelExpose) -> List (Node Declaration) -> Dict String ExposedElement
collectExposedElements docsReferences exposingNodes declarations =
    let
        listWithPreviousRange : List (Maybe Range)
        listWithPreviousRange =
            Nothing
                :: (exposingNodes
                        |> List.take (List.length exposingNodes - 1)
                        |> List.map (\(Node range _) -> Just range)
                   )

        listWithNextRange : List Range
        listWithNextRange =
            (exposingNodes
                |> List.map Node.range
                |> List.drop 1
            )
                ++ [ { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } } ]
    in
    exposingNodes
        |> List.map3 (\prev next current -> ( prev, current, next )) listWithPreviousRange listWithNextRange
        |> List.indexedMap
            (\index ( maybePreviousRange, Node range value, nextRange ) ->
                case value of
                    Exposing.FunctionExpose name ->
                        Just
                            ( name
                            , { range = untilEndOfVariable name range
                              , rangesToRemove = getRangesToRemove docsReferences exposingNodes name index maybePreviousRange range nextRange
                              , elementType = Function
                              }
                            )

                    Exposing.TypeOrAliasExpose name ->
                        Just
                            ( name
                            , { range = untilEndOfVariable name range
                              , rangesToRemove = getRangesToRemove docsReferences exposingNodes name index maybePreviousRange range nextRange
                              , elementType = TypeOrTypeAlias
                              }
                            )

                    Exposing.TypeExpose { name } ->
                        Just
                            ( name
                            , { range = untilEndOfVariable name range
                              , rangesToRemove = []
                              , elementType = ExposedType (findConstructorsForExposedCustomType name declarations)
                              }
                            )

                    Exposing.InfixExpose _ ->
                        Nothing
            )
        |> List.filterMap identity
        |> Dict.fromList


filterOut : List (Node Declaration) -> Dict String ExposedElement -> Dict String ExposedElement
filterOut declarations exposed =
    let
        declaredNames : Set String
        declaredNames =
            declarations
                |> List.filterMap (\(Node _ declaration) -> declarationName declaration)
                |> Set.fromList
    in
    Dict.filter (\name _ -> Set.member name declaredNames) exposed


declarationVisitor : Node Declaration -> ModuleContext -> ModuleContext
declarationVisitor node moduleContext =
    let
        ( allUsedTypes, comesFromCustomTypeWithHiddenConstructors ) =
            typesUsedInDeclaration moduleContext node

        elementsNotToReport : Set String
        elementsNotToReport =
            (if comesFromCustomTypeWithHiddenConstructors then
                moduleContext.elementsNotToReport

             else
                List.foldl (\( _, name ) acc -> Set.insert name acc) moduleContext.elementsNotToReport allUsedTypes
            )
                |> maybeSetInsert (testFunctionName moduleContext node)

        used : Set ( ModuleName, String )
        used =
            List.foldl Set.insert moduleContext.used allUsedTypes
    in
    { moduleContext
        | elementsNotToReport = elementsNotToReport
        , used = used
    }


maybeSetInsert : Maybe comparable -> Set comparable -> Set comparable
maybeSetInsert maybeValue set =
    case maybeValue of
        Just value ->
            Set.insert value set

        Nothing ->
            set


findConstructorsForExposedCustomType : String -> List (Node Declaration) -> List String
findConstructorsForExposedCustomType typeName declarations =
    findMap
        (\node ->
            case Node.value node of
                Declaration.CustomTypeDeclaration type_ ->
                    if Node.value type_.name /= typeName then
                        Nothing

                    else
                        List.map (\c -> c |> Node.value |> .name |> Node.value) type_.constructors
                            |> Just

                _ ->
                    Nothing
        )
        declarations
        |> Maybe.withDefault []


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


testFunctionName : ModuleContext -> Node Declaration -> Maybe String
testFunctionName moduleContext node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            case Maybe.map (\(Node _ value) -> Node.value value.typeAnnotation) function.signature of
                Just (TypeAnnotation.Typed typeNode _) ->
                    if
                        (Tuple.second (Node.value typeNode) == "Test")
                            && (ModuleNameLookupTable.moduleNameFor moduleContext.lookupTable typeNode == Just [ "Test" ])
                    then
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
            ( case function.signature of
                Just signature ->
                    []
                        |> collectTypesFromTypeAnnotation moduleContext [ (Node.value signature).typeAnnotation ]
                        |> findUsedConstructors moduleContext.lookupTable (Node.value function.declaration).arguments

                Nothing ->
                    findUsedConstructors moduleContext.lookupTable (Node.value function.declaration).arguments []
            , False
            )

        Declaration.CustomTypeDeclaration type_ ->
            let
                arguments : List (Node TypeAnnotation)
                arguments =
                    List.concatMap (\constructor -> (Node.value constructor).arguments) type_.constructors
            in
            ( collectTypesFromTypeAnnotation moduleContext arguments []
            , case Dict.get (Node.value type_.name) moduleContext.exposed |> Maybe.map .elementType of
                Just (ExposedType _) ->
                    False

                _ ->
                    True
            )

        Declaration.AliasDeclaration alias_ ->
            ( collectTypesFromTypeAnnotation moduleContext [ alias_.typeAnnotation ] [], False )

        Declaration.PortDeclaration signature ->
            ( collectTypesFromTypeAnnotation moduleContext [ signature.typeAnnotation ] [], False )

        Declaration.InfixDeclaration _ ->
            ( [], False )

        Declaration.Destructuring _ _ ->
            ( [], False )


collectTypesFromTypeAnnotation : ModuleContext -> List (Node TypeAnnotation) -> List ( ModuleName, String ) -> List ( ModuleName, String )
collectTypesFromTypeAnnotation moduleContext nodes acc =
    case nodes of
        [] ->
            acc

        node :: restOfNodes ->
            case Node.value node of
                TypeAnnotation.FunctionTypeAnnotation left right ->
                    collectTypesFromTypeAnnotation moduleContext (left :: right :: restOfNodes) acc

                TypeAnnotation.Typed (Node range ( _, name )) params ->
                    case ModuleNameLookupTable.moduleNameAt moduleContext.lookupTable range of
                        Just moduleName ->
                            collectTypesFromTypeAnnotation moduleContext (params ++ restOfNodes) (( moduleName, name ) :: acc)

                        Nothing ->
                            collectTypesFromTypeAnnotation moduleContext (params ++ restOfNodes) acc

                TypeAnnotation.Record fields ->
                    let
                        subNodes : List (Node TypeAnnotation)
                        subNodes =
                            List.map (\(Node _ ( _, value )) -> value) fields
                    in
                    collectTypesFromTypeAnnotation moduleContext (subNodes ++ restOfNodes) acc

                TypeAnnotation.GenericRecord _ (Node _ fields) ->
                    let
                        subNodes : List (Node TypeAnnotation)
                        subNodes =
                            List.map (\(Node _ ( _, value )) -> value) fields
                    in
                    collectTypesFromTypeAnnotation moduleContext (subNodes ++ restOfNodes) acc

                TypeAnnotation.Tupled list ->
                    collectTypesFromTypeAnnotation moduleContext (list ++ restOfNodes) acc

                _ ->
                    collectTypesFromTypeAnnotation moduleContext restOfNodes acc



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionVisitor node moduleContext =
    case Node.value node of
        Expression.FunctionOrValue _ name ->
            case ModuleNameLookupTable.moduleNameFor moduleContext.lookupTable node of
                Just moduleName ->
                    registerAsUsed
                        ( moduleName, name )
                        moduleContext

                Nothing ->
                    moduleContext

        Expression.RecordUpdateExpression (Node range name) _ ->
            case ModuleNameLookupTable.moduleNameAt moduleContext.lookupTable range of
                Just moduleName ->
                    registerAsUsed
                        ( moduleName, name )
                        moduleContext

                Nothing ->
                    moduleContext

        Expression.LetExpression { declarations } ->
            let
                used : List ( ModuleName, String )
                used =
                    List.foldl
                        (\declaration acc ->
                            case Node.value declaration of
                                Expression.LetFunction function ->
                                    case function.signature of
                                        Just signature ->
                                            acc
                                                |> collectTypesFromTypeAnnotation moduleContext [ (Node.value signature).typeAnnotation ]
                                                |> findUsedConstructors moduleContext.lookupTable (Node.value function.declaration).arguments

                                        Nothing ->
                                            findUsedConstructors moduleContext.lookupTable (Node.value function.declaration).arguments acc

                                Expression.LetDestructuring pattern _ ->
                                    findUsedConstructors moduleContext.lookupTable [ pattern ] acc
                        )
                        []
                        declarations
            in
            { moduleContext | used = List.foldl Set.insert moduleContext.used used }

        Expression.CaseExpression { cases } ->
            let
                usedConstructors : List ( ModuleName, String )
                usedConstructors =
                    findUsedConstructors
                        moduleContext.lookupTable
                        (List.map Tuple.first cases)
                        []
            in
            { moduleContext | used = List.foldl Set.insert moduleContext.used usedConstructors }

        _ ->
            moduleContext


findUsedConstructors : ModuleNameLookupTable -> List (Node Pattern) -> List ( ModuleName, String ) -> List ( ModuleName, String )
findUsedConstructors lookupTable patterns acc =
    case patterns of
        [] ->
            acc

        pattern :: restOfPatterns ->
            case Node.value pattern of
                Pattern.NamedPattern qualifiedNameRef newPatterns ->
                    let
                        newAcc : List ( ModuleName, String )
                        newAcc =
                            case ModuleNameLookupTable.moduleNameFor lookupTable pattern of
                                Just moduleName ->
                                    ( moduleName, qualifiedNameRef.name ) :: acc

                                Nothing ->
                                    acc
                    in
                    findUsedConstructors lookupTable (newPatterns ++ restOfPatterns) newAcc

                Pattern.TuplePattern newPatterns ->
                    findUsedConstructors lookupTable (newPatterns ++ restOfPatterns) acc

                Pattern.UnConsPattern left right ->
                    findUsedConstructors lookupTable (left :: right :: restOfPatterns) acc

                Pattern.ListPattern newPatterns ->
                    findUsedConstructors lookupTable (newPatterns ++ restOfPatterns) acc

                Pattern.AsPattern node _ ->
                    findUsedConstructors lookupTable (node :: restOfPatterns) acc

                Pattern.ParenthesizedPattern node ->
                    findUsedConstructors lookupTable (node :: restOfPatterns) acc

                _ ->
                    findUsedConstructors lookupTable restOfPatterns acc
