module NoUnused.Exports exposing
    ( rule
    , Configuration, defaults, toRule
    , reportUnusedProductionExports
    , Exception, annotatedBy, suffixedBy, prefixedBy, definedInModule
    )

{-| Forbid the use of exposed elements (functions, values or types) that are never used in your project.

🔧 Running with `--fix` will automatically remove all the reported errors,
except for the ones reported when using [`reportUnusedProductionExports`](#reportUnusedProductionExports).
It won't automatically remove unused modules though.

If the project is a package and the module that declared the element is exposed,
then nothing will be reported.

@docs rule


## Going one step further

This rule can be configured to report more unused elements than the default configuration.

@docs Configuration, defaults, toRule

By default, this rule only reports exposed elements that are never imported in other modules.
It is however pretty common to have elements imported and used in non-production parts of the codebase,
such as in tests or in a styleguide.

For instance, let's say there is a module `A` that exposes a function `someFunction`:

    module A exposing (someFunction)

    someFunction input =
        doSomethingComplexWith input

And there is this test module to test `A.someFunction`:

    module ATest exposing (tests)

    import A
    import Test exposing (Test, describe, test)

    tests : Test
    tests =
        describe "A.someFunction"
            [ test "does something complex" <|
                \() ->
                    A.someFunction someInput
                        |> Expect.equal someExpectedOutput
            ]

Let's say this is the only use of `A.someFunction` in the entire project.
Because `A.someFunction` is technically used in the project, this rule won't report it.

But since the function is not used in production code, it is a good practice to remove it, as that will remove the
amount of code that needs to be maintained unnecessarily. We can detect that using [`reportUnusedProductionExports`](#reportUnusedProductionExports).

@docs reportUnusedProductionExports

@docs Exception, annotatedBy, suffixedBy, prefixedBy, definedInModule


## Try it out

You can try this rule out by running the following commands:

Using the default configuration:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.Exports
```

Using `reportUnusedProductionExports` with the following configuration:

    NoUnused.Exports.defaults
        |> NoUnused.Exports.reportUnusedProductionExports
            { isProductionFile = \{ moduleName, filePath, isInSourceDirectories } -> isInSourceDirectories
            , exceptionsAre = [ annotatedBy "@test-helper", suffixedBy "_FOR_TESTS" ]
            }
        |> NoUnused.Exports.toRule

```bash
elm-review --template jfmengels/elm-review-unused/example-ignore-tests --rules NoUnused.Exports
```

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

    config =
        [ NoUnused.Exports.rule
        ]

This is equivalent to `NoUnused.Exports.toRule NoUnused.Exports.defaults`.

-}
rule : Rule
rule =
    toRule defaults


{-| Configuration for the rule. Use [`defaults`](#defaults) to get a default configuration and use [`toRule`](#toRule) to turn it into a rule.
You can change the configuration using [`reportUnusedProductionExports`](#reportUnusedProductionExports).
-}
type Configuration
    = Configuration Config


type alias Config =
    { isProductionFile : { moduleName : ModuleName, filePath : String, isInSourceDirectories : Bool } -> Bool
    , exceptionModules : List ({ moduleName : ModuleName, filePath : String } -> Bool)
    , exceptionTags : List String
    , exceptionByName : Maybe (String -> Bool)
    , exceptionExplanation : Maybe String
    }


{-| Default configuration. This will only report exported elements that are never used in other modules.
-}
defaults : Configuration
defaults =
    Configuration
        { isProductionFile = always True
        , exceptionModules = []
        , exceptionTags = []
        , exceptionByName = Nothing
        , exceptionExplanation = Nothing
        }


{-| Configures the rule to report elements defined in production code but only used in non-production files.

    import NoUnused.Exports exposing (annotatedBy)

    config =
        [ NoUnused.Exports.defaults
            |> NoUnused.Exports.reportUnusedProductionExports
                { isProductionFile =
                    \{ moduleName, filePath, isInSourceDirectories } ->
                        isInSourceDirectories
                            && not (String.endsWith "/Example.elm" filePath)
                , exceptionsAre = [ annotatedBy "@test-helper" ]
                }
            |> NoUnused.Exports.toRule
        ]

Elements reported using this configuration won't be automatically fixed as they require removing the code
that uses the element.

This function needs to know two things:

1.  Which files are considered to be production files, which is determined by a function that you provide.
    Generally, production files are in the `"source-directories"`, which is indicated by
    `isInSourceDirectories` (given as an argument to the function) being `True`. If you want to exclude
    more files, you can use the `filePath` or `moduleName` of the Elm module, whichever is more practical for you to use.
    `filePath` is relative to the folder containing the `elm.json` file and is written in a UNIX format (`/`, no `\`).

2.  How to identify exceptions. See [`Exception`](#Exception) for more information.

-}
reportUnusedProductionExports :
    { isProductionFile : { moduleName : ModuleName, filePath : String, isInSourceDirectories : Bool } -> Bool
    , exceptionsAre : List Exception
    }
    -> Configuration
    -> Configuration
reportUnusedProductionExports { isProductionFile, exceptionsAre } _ =
    let
        affixMatches : List (String -> Bool)
        affixMatches =
            List.filterMap
                (\helper ->
                    case helper of
                        AnnotatedBy _ ->
                            Nothing

                        SuffixedBy suffix ->
                            Just (\name -> String.endsWith suffix name)

                        PrefixedBy prefix ->
                            Just (\name -> String.startsWith prefix name)

                        DefinedInModule _ ->
                            Nothing
                )
                exceptionsAre

        exceptionModules : List ({ moduleName : ModuleName, filePath : String } -> Bool)
        exceptionModules =
            List.filterMap
                (\helper ->
                    case helper of
                        AnnotatedBy _ ->
                            Nothing

                        SuffixedBy _ ->
                            Nothing

                        PrefixedBy _ ->
                            Nothing

                        DefinedInModule predicate ->
                            Just predicate
                )
                exceptionsAre

        exceptionTags : List String
        exceptionTags =
            List.filterMap
                (\helper ->
                    case helper of
                        AnnotatedBy tag ->
                            Just tag

                        SuffixedBy _ ->
                            Nothing

                        PrefixedBy _ ->
                            Nothing

                        DefinedInModule _ ->
                            Nothing
                )
                exceptionsAre

        exceptionByName : Maybe (String -> Bool)
        exceptionByName =
            if List.isEmpty affixMatches then
                Nothing

            else
                Just (\name -> List.any (\predicate -> predicate name) affixMatches)
    in
    Configuration
        { isProductionFile = isProductionFile
        , exceptionModules = exceptionModules
        , exceptionTags = exceptionTags
        , exceptionByName = exceptionByName
        , exceptionExplanation = createExceptionsExplanation exceptionsAre
        }


createExceptionsExplanation : List Exception -> Maybe String
createExceptionsExplanation exceptions =
    if List.isEmpty exceptions then
        Nothing

    else
        let
            options : List String
            options =
                List.map
                    (\helper ->
                        case helper of
                            AnnotatedBy tag ->
                                "Include " ++ tag ++ " in the documentation of the element"

                            SuffixedBy suffix ->
                                "Rename the element to end with " ++ suffix

                            PrefixedBy prefix ->
                                "Rename the element to start with " ++ prefix

                            DefinedInModule _ ->
                                "Adapt your configuration to mark the whole module to as an exception"
                    )
                    exceptions
        in
        Just ("- " ++ String.join "\n- " options)


{-| Predicate to identify exceptions (that shouldn't be reported) for elements defined in production code that are only used in non-production code.

A problem with reporting these elements is that it's going to produce false positives, as there are legitimate use-cases
for exporting these elements, hence the need for the rule to be able to identify them.

For instance, while it's generally discouraged, you might want to test the internals of an API (to make sure some properties hold
given very specific situations). In this case, your module then needs to expose a way to gain insight to the internals.

Another example is giving the means for tests to create opaque types that are impossible or very hard to create in a test
environment. This can be the case for types that can only be created through the decoding of an HTTP request.

Note that another common way to handle these use-cases is to move the internals to another module that exposes everything
while making sure only specific production modules import it.

-}
type Exception
    = AnnotatedBy String
    | SuffixedBy String
    | PrefixedBy String
    | DefinedInModule ({ moduleName : ModuleName, filePath : String } -> Bool)


{-| Prevents reporting usages of elements that contain a specific tag in their documentation.

Given the following configuration

    NoUnused.Exports.defaults
        |> NoUnused.Exports.reportUnusedProductionExports
            { isProductionFile = isProductionFile
            , exceptionsAre = [ annotatedBy "@test-helper" ]
            }
        |> NoUnused.Exports.toRule

any element that has `@test-helper` in its documentation will not be reported as unused (as long as its used at least once in the project):

    {-| @test-helper
    -}
    someFunction input =
        doSomethingComplexWith input

A recommended practice is to have annotations start with `@`.

You can use this function several times to define multiple annotations.

-}
annotatedBy : String -> Exception
annotatedBy =
    AnnotatedBy


{-| Prevents reporting usages of elements whose name end with a specific string.

Given the following configuration

    NoUnused.Exports.defaults
        |> NoUnused.Exports.reportUnusedProductionExports
            { isProductionFile = isProductionFile
            , exceptionsAre = [ suffixedBy "_FOR_TESTS" ]
            }
        |> NoUnused.Exports.toRule

any element that ends with `"_FOR_TESTS"` will not be reported as unused (as long as its used at least once in the project):

    someFunction_FOR_TESTS input =
        doSomethingComplexWith input

You can use this function several times to define multiple suffixes.

-}
suffixedBy : String -> Exception
suffixedBy =
    SuffixedBy


{-| Prevents reporting usages of elements whose name start with a specific string.

Given the following configuration

    NoUnused.Exports.defaults
        |> NoUnused.Exports.reportUnusedProductionExports
            { isProductionFile = isProductionFile
            , exceptionsAre = [ prefixedBy "test_" ]
            }
        |> NoUnused.Exports.toRule

any element that starts with `"test_"` will not be reported as unused (as long as its used at least once in the project):

    test_someFunction input =
        doSomethingComplexWith input

You can use this function several times to define multiple prefixes.

-}
prefixedBy : String -> Exception
prefixedBy =
    PrefixedBy


{-| Prevents reporting usages of elements in some modules.

Given the following configuration

    NoUnused.Exports.defaults
        |> NoUnused.Exports.reportUnusedProductionExports
            { isProductionFile = isProductionFile
            , exceptionsAre =
                [ definedInModule
                    (\{ moduleName, filePath } ->
                        List.member "Util" moduleName
                            || String.startsWith "src/test-helpers/" filePath
                    )
                ]
            }
        |> NoUnused.Exports.toRule

no elements from modules named `*.Util.*` or modules inside `src/test-helpers/` will be reported.

The provided `filePath` is relative to the project's `elm.json` and is in a UNIX style (`/`, no `\`).

-}
definedInModule : ({ moduleName : ModuleName, filePath : String } -> Bool) -> Exception
definedInModule =
    DefinedInModule


{-| Creates a rule that reports unused exports using a [`Configuration`](#Configuration).
-}
toRule : Configuration -> Rule
toRule (Configuration config) =
    Rule.newProjectRuleSchema "NoUnused.Exports" initialProjectContext
        |> Rule.withModuleVisitor (moduleVisitor config)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject config
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withElmJsonProjectVisitor (\elmJson context -> ( [], elmJsonVisitor elmJson context ))
        |> Rule.withFinalProjectEvaluation (finalEvaluationForProject config.exceptionExplanation)
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


moduleVisitor : Config -> Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor config schema =
    schema
        |> Rule.withImportVisitor (\node context -> ( [], importVisitor node context ))
        |> Rule.withDeclarationEnterVisitor (\node context -> ( [], declarationVisitor config node context ))
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionVisitor node context ))



-- CONTEXT


type alias ProjectContext =
    { projectType : ProjectType
    , modules :
        Dict
            ModuleNameStr
            { moduleKey : Rule.ModuleKey
            , exposed : Dict String ExposedElement
            , moduleNameLocation : Range
            , isProductionFile : Bool
            , isProductionFileNotToReport : Bool
            , ignoredElementsNotToReport : Set String
            }
    , usedModules : Set ModuleNameStr
    , used : Set ( ModuleNameStr, String )
    , usedInIgnoredModules : Set ( ModuleNameStr, String )
    , constructors : Dict ( ModuleNameStr, String ) String
    }


type alias ExposedElement =
    { range : Range
    , rangesToRemove : List Range
    , elementType : ExposedElementType
    }


type ProjectType
    = IsApplication ElmApplicationType
    | IsPackage (Set ModuleNameStr)


type ElmApplicationType
    = ElmApplication
    | LamderaApplication


type ExposedElementType
    = Function
    | TypeOrTypeAlias
    | ExposedType (List String)


type alias ModuleNameStr =
    String


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , exposed : Dict String ExposedElement
    , used : Set ( ModuleNameStr, String )
    , elementsNotToReport : Set String
    , ignoredElementsNotToReport : Set String
    , importedModules : Set ModuleNameStr
    , containsMainFunction : Bool
    , projectType : ProjectType
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { projectType = IsApplication ElmApplication
    , modules = Dict.empty
    , usedModules = Set.singleton "ReviewConfig"
    , used = Set.empty
    , usedInIgnoredModules = Set.empty
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
            , ignoredElementsNotToReport = Set.empty
            , importedModules = Set.empty
            , containsMainFunction = False
            , projectType = projectContext.projectType
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withFullAst
        |> Rule.withModuleDocumentation


fromModuleToProject : Config -> Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject config =
    Rule.initContextCreator
        (\moduleKey (Node moduleNameRange moduleName) filePath isInSourceDirectories moduleContext ->
            let
                moduleNameStr : ModuleNameStr
                moduleNameStr =
                    String.join "." moduleName

                used : Set ( ModuleNameStr, String )
                used =
                    Set.foldl
                        (\element acc -> Set.insert ( moduleNameStr, element ) acc)
                        moduleContext.used
                        moduleContext.elementsNotToReport

                isProductionFile : Bool
                isProductionFile =
                    config.isProductionFile { moduleName = moduleName, filePath = filePath, isInSourceDirectories = isInSourceDirectories }
            in
            { projectType = IsApplication ElmApplication
            , modules =
                Dict.singleton
                    moduleNameStr
                    { moduleKey = moduleKey
                    , exposed = moduleContext.exposed
                    , moduleNameLocation = moduleNameRange
                    , isProductionFile = isProductionFile
                    , isProductionFileNotToReport = any config.exceptionModules { moduleName = moduleName, filePath = filePath }
                    , ignoredElementsNotToReport = moduleContext.ignoredElementsNotToReport
                    }
            , used =
                if isProductionFile then
                    used

                else
                    Set.empty
            , usedInIgnoredModules =
                if isProductionFile then
                    Set.empty

                else
                    used
            , usedModules =
                if Set.member "Test" moduleContext.importedModules || moduleContext.containsMainFunction then
                    Set.insert moduleNameStr moduleContext.importedModules

                else
                    moduleContext.importedModules
            , constructors =
                Dict.foldl
                    (\name element acc ->
                        case element.elementType of
                            ExposedType constructorNames ->
                                List.foldl
                                    (\constructorName listAcc -> Dict.insert ( moduleNameStr, constructorName ) name listAcc)
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
        |> Rule.withFilePath
        |> Rule.withIsInSourceDirectories


any : List (a -> Bool) -> a -> Bool
any list a =
    case list of
        [] ->
            False

        head :: tail ->
            if head a then
                True

            else
                any tail a


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { projectType = previousContext.projectType
    , modules = Dict.union newContext.modules previousContext.modules
    , usedModules = Set.union newContext.usedModules previousContext.usedModules
    , used = Set.union newContext.used previousContext.used
    , usedInIgnoredModules = Set.union newContext.usedInIgnoredModules previousContext.usedInIgnoredModules
    , constructors = Dict.union newContext.constructors previousContext.constructors
    }


registerAsUsed : ( ModuleNameStr, String ) -> ModuleContext -> ModuleContext
registerAsUsed key moduleContext =
    { moduleContext | used = Set.insert key moduleContext.used }



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
                                Set.insert (Elm.Module.toString moduleName) acc
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


finalEvaluationForProject : Maybe String -> ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluationForProject exceptionExplanation projectContext =
    let
        used : Set ( ModuleNameStr, String )
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

        usedInIgnoredModules : Set ( ModuleNameStr, String )
        usedInIgnoredModules =
            Set.foldl
                (\(( moduleName, _ ) as key) acc ->
                    case Dict.get key projectContext.constructors of
                        Just typeName ->
                            Set.insert ( moduleName, typeName ) acc

                        Nothing ->
                            acc
                )
                projectContext.usedInIgnoredModules
                projectContext.usedInIgnoredModules

        filterExposedPackage_ : ModuleNameStr -> Bool
        filterExposedPackage_ =
            filterExposedPackage projectContext
    in
    Dict.foldl
        (\moduleName module_ acc ->
            if not (filterExposedPackage_ moduleName) then
                acc

            else if Set.member moduleName projectContext.usedModules then
                errorsForModule exceptionExplanation projectContext { used = used, usedInIgnoredModules = usedInIgnoredModules } moduleName module_ acc

            else
                unusedModuleError moduleName module_ :: acc
        )
        []
        projectContext.modules


unusedModuleError : ModuleNameStr -> { a | moduleKey : Rule.ModuleKey, moduleNameLocation : Range } -> Error scope
unusedModuleError moduleName { moduleKey, moduleNameLocation } =
    Rule.errorForModule moduleKey
        { message = "Module `" ++ moduleName ++ "` is never used."
        , details = [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
        }
        moduleNameLocation


errorsForModule :
    Maybe String
    -> ProjectContext
    -> { used : Set ( ModuleNameStr, String ), usedInIgnoredModules : Set ( ModuleNameStr, String ) }
    -> ModuleNameStr
    ->
        { a
            | moduleKey : Rule.ModuleKey
            , exposed : Dict String ExposedElement
            , isProductionFile : Bool
            , isProductionFileNotToReport : Bool
            , ignoredElementsNotToReport : Set String
        }
    -> List (Error scope)
    -> List (Error scope)
errorsForModule exceptionExplanation projectContext { used, usedInIgnoredModules } moduleName { moduleKey, exposed, isProductionFile, isProductionFileNotToReport, ignoredElementsNotToReport } acc =
    Dict.foldl
        (\name element subAcc ->
            if isUsedOrException projectContext used moduleName name then
                subAcc

            else if Set.member ( moduleName, name ) usedInIgnoredModules then
                if not isProductionFile || isProductionFileNotToReport || Set.member name ignoredElementsNotToReport then
                    subAcc

                else
                    Rule.errorForModule moduleKey
                        { message = what element.elementType ++ " `" ++ name ++ "` is never used in production code."
                        , details =
                            "This exposed element is only used in files you have marked as non-production code (e.g. the tests folder), and should therefore be removed along with the places it's used in. This will help reduce the amount of code you will need to maintain."
                                :: (case exceptionExplanation of
                                        Nothing ->
                                            [ "It is possible that this element is meant to enable work in your ignored folder (test helpers for instance), in which case you should keep it. To avoid this problem being reported again, please read the documentation on how to configure the rule."
                                            ]

                                        Just explanation ->
                                            [ "It is possible that this element is meant to enable work in your ignored folder (test helpers for instance), in which case you should keep it. To avoid this problem being reported again, you can:"
                                            , explanation
                                            ]
                                   )
                        }
                        element.range
                        :: subAcc

            else
                Rule.errorForModuleWithFix moduleKey
                    { message = what element.elementType ++ " `" ++ name ++ "` is never used outside this module."
                    , details = [ "This exposed element is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
                    }
                    element.range
                    (List.map Fix.removeRange element.rangesToRemove)
                    :: subAcc
        )
        acc
        exposed


what : ExposedElementType -> String
what elementType =
    case elementType of
        Function ->
            "Exposed function or value"

        TypeOrTypeAlias ->
            "Exposed type or type alias"

        ExposedType _ ->
            "Exposed type"


filterExposedPackage : ProjectContext -> ModuleNameStr -> Bool
filterExposedPackage projectContext =
    case projectContext.projectType of
        IsApplication _ ->
            always True

        IsPackage exposedModuleNames ->
            \moduleName -> not <| Set.member moduleName exposedModuleNames


isUsedOrException : ProjectContext -> Set ( ModuleNameStr, String ) -> ModuleNameStr -> String -> Bool
isUsedOrException projectContext used moduleName name =
    Set.member ( moduleName, name ) used
        || isApplicationException projectContext name
        || (moduleName == "ReviewConfig")


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
        moduleName : ModuleNameStr
        moduleName =
            Node.value import_.moduleName
                |> String.join "."
    in
    { moduleContext
        | used = collectUsedFromImport moduleName import_.exposingList moduleContext.used
        , importedModules = Set.insert moduleName moduleContext.importedModules
    }


collectUsedFromImport : ModuleNameStr -> Maybe (Node Exposing) -> Set ( ModuleNameStr, String ) -> Set ( ModuleNameStr, String )
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


declarationVisitor : Config -> Node Declaration -> ModuleContext -> ModuleContext
declarationVisitor config node moduleContext =
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

        ignoredElementsNotToReport : Set String
        ignoredElementsNotToReport =
            case isException config node of
                Just name ->
                    Set.insert name moduleContext.ignoredElementsNotToReport

                Nothing ->
                    moduleContext.ignoredElementsNotToReport

        used : Set ( ModuleNameStr, String )
        used =
            List.foldl Set.insert moduleContext.used allUsedTypes
    in
    { moduleContext
        | elementsNotToReport = elementsNotToReport
        , ignoredElementsNotToReport = ignoredElementsNotToReport
        , used = used
        , containsMainFunction =
            moduleContext.containsMainFunction
                || doesModuleContainMainFunction moduleContext.projectType node
    }


isException : Config -> Node Declaration -> Maybe String
isException config node =
    if config.exceptionByName == Nothing && List.isEmpty config.exceptionTags then
        Nothing

    else
        case getDeclarationName node of
            Just name ->
                case config.exceptionByName of
                    Just exceptionByName ->
                        if exceptionByName name then
                            Just name

                        else
                            isExceptionByAnnotation config name node

                    Nothing ->
                        isExceptionByAnnotation config name node

            Nothing ->
                Nothing


isExceptionByAnnotation : Config -> b -> Node Declaration -> Maybe b
isExceptionByAnnotation config name node =
    if List.isEmpty config.exceptionTags then
        Nothing

    else
        case getDeclarationDocumentation node of
            Just documentation ->
                if List.any (\exceptionTag -> String.contains exceptionTag documentation) config.exceptionTags then
                    Just name

                else
                    Nothing

            Nothing ->
                Nothing


getDeclarationName : Node Declaration -> Maybe String
getDeclarationName node =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            Just (declaration |> Node.value |> .name |> Node.value)

        Declaration.AliasDeclaration { name } ->
            Just (Node.value name)

        Declaration.CustomTypeDeclaration { name } ->
            Just (Node.value name)

        Declaration.PortDeclaration { name } ->
            Just (Node.value name)

        _ ->
            Nothing


getDeclarationDocumentation : Node Declaration -> Maybe String
getDeclarationDocumentation node =
    case Node.value node of
        Declaration.FunctionDeclaration { documentation } ->
            case documentation of
                Just doc ->
                    Just (Node.value doc)

                Nothing ->
                    Nothing

        Declaration.AliasDeclaration { documentation } ->
            case documentation of
                Just doc ->
                    Just (Node.value doc)

                Nothing ->
                    Nothing

        Declaration.CustomTypeDeclaration { documentation } ->
            case documentation of
                Just doc ->
                    Just (Node.value doc)

                Nothing ->
                    Nothing

        Declaration.PortDeclaration _ ->
            -- TODO When we have documentation syntax for ports
            Nothing

        _ ->
            Nothing


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


typesUsedInDeclaration : ModuleContext -> Node Declaration -> ( List ( ModuleNameStr, String ), Bool )
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
                typesUsedInArguments : List ( ModuleNameStr, String )
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


collectTypesFromTypeAnnotation : ModuleContext -> List (Node TypeAnnotation) -> List ( ModuleNameStr, String ) -> List ( ModuleNameStr, String )
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
                            collectTypesFromTypeAnnotation moduleContext (params ++ restOfNodes) (( String.join "." moduleName, name ) :: acc)

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
            registerLocalValue (Node.range node) name moduleContext

        Expression.RecordUpdateExpression (Node range name) _ ->
            registerLocalValue range name moduleContext

        Expression.LetExpression { declarations } ->
            let
                used : List ( ModuleNameStr, String )
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
                usedConstructors : List ( ModuleNameStr, String )
                usedConstructors =
                    findUsedConstructors
                        moduleContext.lookupTable
                        (List.map Tuple.first cases)
                        []
            in
            { moduleContext | used = List.foldl Set.insert moduleContext.used usedConstructors }

        _ ->
            moduleContext


registerLocalValue : Range -> String -> ModuleContext -> ModuleContext
registerLocalValue range name moduleContext =
    case ModuleNameLookupTable.moduleNameAt moduleContext.lookupTable range of
        Just [] ->
            if Dict.member name moduleContext.exposed then
                { moduleContext | ignoredElementsNotToReport = Set.insert name moduleContext.ignoredElementsNotToReport }

            else
                moduleContext

        Just moduleName ->
            registerAsUsed ( String.join "." moduleName, name ) moduleContext

        Nothing ->
            moduleContext


findUsedConstructors : ModuleNameLookupTable -> List (Node Pattern) -> List ( ModuleNameStr, String ) -> List ( ModuleNameStr, String )
findUsedConstructors lookupTable patterns acc =
    case patterns of
        [] ->
            acc

        pattern :: restOfPatterns ->
            case Node.value pattern of
                Pattern.NamedPattern qualifiedNameRef newPatterns ->
                    let
                        newAcc : List ( ModuleNameStr, String )
                        newAcc =
                            case ModuleNameLookupTable.moduleNameFor lookupTable pattern of
                                Just moduleName ->
                                    ( String.join "." moduleName, qualifiedNameRef.name ) :: acc

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
