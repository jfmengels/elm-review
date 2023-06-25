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
import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import List.Extra
import NoUnused.LamderaSupport as LamderaSupport
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Report functions and types that are exposed from a module but that are never
used in other modules. Also reports when a module is entirely unused.

🔧 Running with `--fix` will automatically remove all the reported errors.
It won't automatically remove unused modules though.

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
        |> Rule.withElmJsonProjectVisitor (\elmJson context -> ( [], elmJsonVisitor elmJson context ))
        |> Rule.withFinalProjectEvaluation finalEvaluationForProject
        |> Rule.providesFixesForProjectRule
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
            , moduleNameLocation : Range
            }
    , usedModules : Set ModuleName
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
    , importedModules : Set ModuleName
    , containsMainFunction : Bool
    , projectType : ProjectType
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { projectType = IsApplication ElmApplication
    , modules = Dict.empty
    , usedModules = Set.singleton [ "ReviewConfig" ]
    , used = Set.empty
    , constructors = Dict.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable ast moduleDocumentation projectContext ->
            let
                exposed : Dict String ExposedElement
                exposed =
                    case Module.exposingList (Node.value ast.moduleDefinition) of
                        Exposing.All _ ->
                            Dict.empty

                        Exposing.Explicit explicitlyExposed ->
                            collectExposedElements moduleDocumentation explicitlyExposed ast.declarations
            in
            { lookupTable = lookupTable
            , exposed = exposed
            , used = Set.empty
            , elementsNotToReport = Set.empty
            , importedModules = Set.empty
            , containsMainFunction = False
            , projectType = projectContext.projectType
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withFullAst
        |> Rule.withModuleDocumentation


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleKey (Node moduleNameRange moduleName) moduleContext ->
            { projectType = IsApplication ElmApplication
            , modules =
                Dict.singleton
                    moduleName
                    { moduleKey = moduleKey
                    , exposed = moduleContext.exposed
                    , moduleNameLocation = moduleNameRange
                    }
            , used =
                Set.foldl
                    (\element acc -> Set.insert ( moduleName, element ) acc)
                    moduleContext.used
                    moduleContext.elementsNotToReport
            , usedModules =
                if Set.member [ "Test" ] moduleContext.importedModules || moduleContext.containsMainFunction then
                    Set.insert moduleName moduleContext.importedModules

                else
                    moduleContext.importedModules
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
        |> Rule.withModuleNameNode


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { projectType = previousContext.projectType
    , modules = Dict.union newContext.modules previousContext.modules
    , usedModules = Set.union newContext.usedModules previousContext.usedModules
    , used = Set.union newContext.used previousContext.used
    , constructors = Dict.union newContext.constructors previousContext.constructors
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

        filterExposedPackage_ : ModuleName -> Bool
        filterExposedPackage_ =
            filterExposedPackage projectContext
    in
    Dict.foldl
        (\moduleName module_ acc ->
            if not (filterExposedPackage_ moduleName) then
                acc

            else if Set.member moduleName projectContext.usedModules then
                errorsForModule projectContext used moduleName module_ acc

            else
                unusedModuleError moduleName module_ :: acc
        )
        []
        projectContext.modules


unusedModuleError : ModuleName -> { a | moduleKey : Rule.ModuleKey, moduleNameLocation : Range } -> Error scope
unusedModuleError moduleName { moduleKey, moduleNameLocation } =
    Rule.errorForModule moduleKey
        { message = "Module `" ++ String.join "." moduleName ++ "` is never used."
        , details = [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
        }
        moduleNameLocation


errorsForModule : ProjectContext -> Set ( ModuleName, String ) -> ModuleName -> { a | moduleKey : Rule.ModuleKey, exposed : Dict String ExposedElement } -> List (Error scope) -> List (Error scope)
errorsForModule projectContext used moduleName { moduleKey, exposed } acc =
    Dict.foldl
        (\name element subAcc ->
            if isUsedOrException projectContext used moduleName name then
                subAcc

            else
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
                Rule.errorForModuleWithFix moduleKey
                    { message = what ++ " `" ++ name ++ "` is never used outside this module."
                    , details = [ "This exposed element is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
                    }
                    element.range
                    (List.map Fix.removeRange element.rangesToRemove)
                    :: subAcc
        )
        acc
        exposed


filterExposedPackage : ProjectContext -> ModuleName -> Bool
filterExposedPackage projectContext =
    case projectContext.projectType of
        IsApplication _ ->
            always True

        IsPackage exposedModuleNames ->
            \moduleName -> not <| Set.member moduleName exposedModuleNames


isUsedOrException : ProjectContext -> Set ( ModuleName, String ) -> List String -> String -> Bool
isUsedOrException projectContext used moduleName name =
    Set.member ( moduleName, name ) used
        || isApplicationException projectContext name
        || (moduleName == [ "ReviewConfig" ])


isApplicationException : ProjectContext -> String -> Bool
isApplicationException projectContext name =
    case projectContext.projectType of
        IsPackage _ ->
            False

        IsApplication ElmApplication ->
            name == "main"

        IsApplication LamderaApplication ->
            name == "main" || name == "app"


getRangesToRemove : List ( Int, String ) -> Bool -> String -> Int -> Maybe Range -> Range -> Range -> List Range
getRangesToRemove comments canRemoveExposed name index maybePreviousRange range nextRange =
    if canRemoveExposed then
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

    else
        []


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
importVisitor (Node _ import_) moduleContext =
    let
        moduleName : ModuleName
        moduleName =
            Node.value import_.moduleName
    in
    { moduleContext
        | used = collectUsedFromImport moduleName import_.exposingList moduleContext.used
        , importedModules = Set.insert moduleName moduleContext.importedModules
    }


collectUsedFromImport : ModuleName -> Maybe (Node Exposing) -> Set ( ModuleName, String ) -> Set ( ModuleName, String )
collectUsedFromImport moduleName exposingList used =
    case Maybe.map Node.value exposingList of
        Just (Exposing.Explicit list) ->
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
                used
                list

        Just (Exposing.All _) ->
            used

        Nothing ->
            used


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


collectExposedElements : Maybe (Node String) -> List (Node Exposing.TopLevelExpose) -> List (Node Declaration) -> Dict String ExposedElement
collectExposedElements moduleDocumentation exposingNodes declarations =
    let
        docsReferences : List ( Int, String )
        docsReferences =
            collectDocsReferences moduleDocumentation

        declaredNames : Set String
        declaredNames =
            List.foldl
                (\(Node _ declaration) acc ->
                    case declarationName declaration of
                        Just name ->
                            Set.insert name acc

                        Nothing ->
                            acc
                )
                Set.empty
                declarations
    in
    collectExposedElementsHelp docsReferences declarations declaredNames (List.length exposingNodes /= 1) Nothing exposingNodes 0 Dict.empty


collectExposedElementsHelp : List ( Int, String ) -> List (Node Declaration) -> Set String -> Bool -> Maybe Range -> List (Node TopLevelExpose) -> Int -> Dict String ExposedElement -> Dict String ExposedElement
collectExposedElementsHelp docsReferences declarations declaredNames canRemoveExposed maybePreviousRange exposingNodes index acc =
    case exposingNodes of
        [] ->
            acc

        (Node range value) :: rest ->
            let
                nextRange : Range
                nextRange =
                    case List.head rest of
                        Just nextNode ->
                            Node.range nextNode

                        Nothing ->
                            Range.emptyRange

                newAcc : Dict String ExposedElement
                newAcc =
                    case value of
                        Exposing.FunctionExpose name ->
                            if Set.member name declaredNames then
                                Dict.insert name
                                    { range = untilEndOfVariable name range
                                    , rangesToRemove = getRangesToRemove docsReferences canRemoveExposed name index maybePreviousRange range nextRange
                                    , elementType = Function
                                    }
                                    acc

                            else
                                acc

                        Exposing.TypeOrAliasExpose name ->
                            if Set.member name declaredNames then
                                Dict.insert name
                                    { range = untilEndOfVariable name range
                                    , rangesToRemove = getRangesToRemove docsReferences canRemoveExposed name index maybePreviousRange range nextRange
                                    , elementType = TypeOrTypeAlias
                                    }
                                    acc

                            else
                                acc

                        Exposing.TypeExpose { name } ->
                            if Set.member name declaredNames then
                                Dict.insert name
                                    { range = untilEndOfVariable name range
                                    , rangesToRemove = []
                                    , elementType = ExposedType (findConstructorsForExposedCustomType name declarations)
                                    }
                                    acc

                            else
                                acc

                        Exposing.InfixExpose _ ->
                            acc
            in
            collectExposedElementsHelp
                docsReferences
                declarations
                declaredNames
                canRemoveExposed
                (Just range)
                rest
                (index + 1)
                newAcc


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
        , containsMainFunction =
            moduleContext.containsMainFunction
                || doesModuleContainMainFunction moduleContext.projectType node
    }


doesModuleContainMainFunction : ProjectType -> Node Declaration -> Bool
doesModuleContainMainFunction projectType declaration =
    case projectType of
        IsPackage _ ->
            False

        IsApplication elmApplicationType ->
            case Node.value declaration of
                Declaration.FunctionDeclaration function ->
                    isMainFunction elmApplicationType (function.declaration |> Node.value |> .name |> Node.value)

                _ ->
                    False


isMainFunction : ElmApplicationType -> String -> Bool
isMainFunction elmApplicationType name =
    case elmApplicationType of
        ElmApplication ->
            name == "main"

        LamderaApplication ->
            name == "main" || name == "app"


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
                typesUsedInArguments : List ( ModuleName, String )
                typesUsedInArguments =
                    List.foldl
                        (\constructor acc -> collectTypesFromTypeAnnotation moduleContext (Node.value constructor).arguments acc)
                        []
                        type_.constructors
            in
            ( typesUsedInArguments
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
