module NoUnused.Dependencies exposing (rule)

{-| Forbid the use of dependencies that are never used in your project.

@docs rule

-}

import Dict exposing (Dict)
import Elm.Constraint
import Elm.Package
import Elm.Project exposing (Project)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Version
import List.Extra
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbid the use of dependencies that are never used in your project.

🔧 Running with `--fix` will automatically remove all the reported errors.

A dependency is considered unused if none of its modules are imported in the project.

    config =
        [ NoUnused.Dependencies.rule
        ]


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.Dependencies
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.Dependencies" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluationForProject
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withImportVisitor importVisitor


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependenciesVisitor dependencies projectContext =
    let
        moduleNameToDependency : Dict String String
        moduleNameToDependency =
            dependencies
                |> Dict.filter
                    (\packageName _ ->
                        Set.member packageName projectContext.directProjectDependencies
                            || Set.member packageName projectContext.directTestDependencies
                    )
                |> Dict.toList
                |> List.concatMap
                    (\( packageName, dependency ) ->
                        List.map (\{ name } -> ( name, packageName )) (Dependency.modules dependency)
                    )
                |> Dict.fromList
    in
    ( []
    , { projectContext
        | dependencies = Dict.map (\_ dep -> getDependencyList dep) dependencies
        , moduleNameToDependency = moduleNameToDependency
      }
    )



-- CONTEXT


type alias ProjectContext =
    { moduleNameToDependency : Dict String String
    , dependencies : Dict String DependencyList
    , directProjectDependencies : Set String
    , directTestDependencies : Set String
    , usedDependencies : Set String
    , usedDependenciesFromTest : Set String
    , elmJsonKey : Maybe Rule.ElmJsonKey
    }


type alias DependencyList =
    List Elm.Package.Name


type alias ModuleContext =
    { moduleNameToDependency : Dict String String
    , usedDependencies : Set String
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { moduleNameToDependency = Dict.empty
    , dependencies = Dict.empty
    , directProjectDependencies = Set.empty
    , directTestDependencies = Set.empty
    , usedDependencies = Set.empty
    , usedDependenciesFromTest = Set.empty
    , elmJsonKey = Nothing
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\projectContext ->
            { moduleNameToDependency = projectContext.moduleNameToDependency
            , usedDependencies = Set.empty
            }
        )


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\isInSourceDirectories { usedDependencies } ->
            { moduleNameToDependency = Dict.empty
            , dependencies = Dict.empty
            , directProjectDependencies = Set.empty
            , directTestDependencies = Set.empty
            , usedDependencies =
                if isInSourceDirectories then
                    usedDependencies

                else
                    Set.empty
            , usedDependenciesFromTest =
                if isInSourceDirectories then
                    Set.empty

                else
                    usedDependencies
            , elmJsonKey = Nothing
            }
        )
        |> Rule.withIsInSourceDirectories


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { moduleNameToDependency = previousContext.moduleNameToDependency
    , dependencies = previousContext.dependencies
    , directProjectDependencies = previousContext.directProjectDependencies
    , directTestDependencies = previousContext.directTestDependencies
    , usedDependencies = Set.union newContext.usedDependencies previousContext.usedDependencies
    , usedDependenciesFromTest = Set.union newContext.usedDependenciesFromTest previousContext.usedDependenciesFromTest
    , elmJsonKey = previousContext.elmJsonKey
    }



-- PROJECT VISITORS


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject projectContext =
    case maybeProject of
        Just { elmJsonKey, project } ->
            let
                ( directProjectDependencies, directTestDependencies ) =
                    case project of
                        Elm.Project.Package { deps, testDeps } ->
                            ( listDependencies deps
                            , listDependencies testDeps
                            )

                        Elm.Project.Application { depsDirect, testDepsDirect } ->
                            ( listDependencies depsDirect
                            , listDependencies testDepsDirect
                            )
            in
            ( []
            , { projectContext
                | elmJsonKey = Just elmJsonKey
                , directProjectDependencies = directProjectDependencies
                , directTestDependencies = directTestDependencies
              }
            )

        Nothing ->
            ( [], projectContext )


getDependencyList : Dependency -> DependencyList
getDependencyList dependency =
    case Dependency.elmJson dependency of
        Elm.Project.Application _ ->
            []

        Elm.Project.Package package ->
            List.map (\( depName, _ ) -> depName) package.deps


listDependencies : List ( Elm.Package.Name, a ) -> Set String
listDependencies deps =
    List.foldl
        (\( name, _ ) acc -> Set.insert (Elm.Package.toString name) acc)
        Set.empty
        deps



-- IMPORT VISITOR


importVisitor : Node Import -> ModuleContext -> ( List nothing, ModuleContext )
importVisitor node context =
    ( []
    , case Dict.get (moduleNameForImport node) context.moduleNameToDependency of
        Just dependency ->
            { context | usedDependencies = Set.insert dependency context.usedDependencies }

        Nothing ->
            context
    )


moduleNameForImport : Node Import -> String
moduleNameForImport node =
    node
        |> Node.value
        |> .moduleName
        |> Node.value
        |> String.join "."



-- FINAL EVALUATION


finalEvaluationForProject : ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluationForProject projectContext =
    case projectContext.elmJsonKey of
        Just elmJsonKey ->
            let
                depsNotUsedInSrc : Set String
                depsNotUsedInSrc =
                    Set.diff projectContext.directProjectDependencies projectContext.usedDependencies

                depsNotUsedInSrcButUsedInTests : Set String
                depsNotUsedInSrcButUsedInTests =
                    Set.intersect depsNotUsedInSrc projectContext.usedDependenciesFromTest

                depsNotUsedInSrcErrors : List String
                depsNotUsedInSrcErrors =
                    Set.diff
                        depsNotUsedInSrc
                        (Set.union packagesNotToReport depsNotUsedInSrcButUsedInTests)
                        |> Set.toList

                testDepsNotUsed : List String
                testDepsNotUsed =
                    Set.diff
                        projectContext.directTestDependencies
                        (Set.union projectContext.usedDependenciesFromTest projectContext.usedDependencies)
                        |> Set.toList
            in
            List.map (unusedProjectDependencyError elmJsonKey projectContext.dependencies) depsNotUsedInSrcErrors
                ++ List.map (unusedTestDependencyError elmJsonKey projectContext.dependencies) testDepsNotUsed
                ++ List.map (moveDependencyToTestError elmJsonKey projectContext.dependencies) (Set.toList depsNotUsedInSrcButUsedInTests)

        Nothing ->
            []


packagesNotToReport : Set String
packagesNotToReport =
    Set.fromList [ "elm/core", "lamdera/core", "lamdera/codecs" ]



-- ERROR FUNCTIONS


unusedProjectDependencyError : Rule.ElmJsonKey -> Dict String DependencyList -> String -> Error scope
unusedProjectDependencyError elmJsonKey dependencies packageName =
    Rule.errorForElmJsonWithFix elmJsonKey
        (\elmJson ->
            { message = "Unused dependency `" ++ packageName ++ "`"
            , details =
                [ "To remove it, I recommend running the following command:"
                , "    elm-json uninstall " ++ packageName
                ]
            , range = findPackageNameInElmJson packageName elmJson
            }
        )
        (fromProject dependencies InProjectDeps packageName >> Maybe.map (removeProjectDependency >> toProject))


moveDependencyToTestError : Rule.ElmJsonKey -> Dict String DependencyList -> String -> Error scope
moveDependencyToTestError elmJsonKey dependencies packageName =
    Rule.errorForElmJsonWithFix elmJsonKey
        (\elmJson ->
            { message = "`" ++ packageName ++ "` should be moved to test-dependencies"
            , details =
                [ "This package is not used in the source code, but it is used in tests, and should therefore be moved to the test dependencies. To do so, I recommend running the following commands:"
                , "    elm-json uninstall " ++ packageName ++ "\n" ++ "    elm-json install --test " ++ packageName
                ]
            , range = findPackageNameInElmJson packageName elmJson
            }
        )
        (fromProject dependencies InProjectDeps packageName >> Maybe.map (removeProjectDependency >> addTestDependency >> toProject))


unusedTestDependencyError : Rule.ElmJsonKey -> Dict String DependencyList -> String -> Error scope
unusedTestDependencyError elmJsonKey dependencies packageName =
    Rule.errorForElmJsonWithFix elmJsonKey
        (\elmJson ->
            { message = "Unused test dependency `" ++ packageName ++ "`"
            , details =
                [ "To remove it, I recommend running the following command:"
                , "    elm-json uninstall " ++ packageName
                ]
            , range = findPackageNameInElmJson packageName elmJson
            }
        )
        (\project ->
            case fromProject dependencies InTestDeps packageName project of
                Just projectAndDependencyIdentifier ->
                    Just (toProject (removeTestDependency projectAndDependencyIdentifier))

                Nothing ->
                    Nothing
        )


findPackageNameInElmJson : String -> String -> Range
findPackageNameInElmJson packageName elmJson =
    elmJson
        |> String.lines
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( row, line ) ->
                case String.indexes ("\"" ++ packageName ++ "\"") line of
                    [] ->
                        Nothing

                    column :: _ ->
                        Just
                            { start =
                                { row = row + 1
                                , column = column + 2
                                }
                            , end =
                                { row = row + 1
                                , column = column + String.length packageName + 2
                                }
                            }
            )
        |> List.head
        |> Maybe.withDefault { start = { row = 1, column = 1 }, end = { row = 10000, column = 1 } }



-- FIX


type ProjectAndDependencyIdentifier
    = ApplicationProject
        { application : Elm.Project.ApplicationInfo
        , name : Elm.Package.Name
        , version : Elm.Version.Version
        , getDependenciesAndVersion : Elm.Package.Name -> Elm.Project.Deps Elm.Version.Version
        }
    | PackageProject
        { package : Elm.Project.PackageInfo
        , name : Elm.Package.Name
        , constraint : Elm.Constraint.Constraint
        }


type DependencyLocation
    = InProjectDeps
    | InTestDeps


fromProject : Dict String DependencyList -> DependencyLocation -> String -> Project -> Maybe ProjectAndDependencyIdentifier
fromProject dependenciesDict dependencyLocation packageNameStr project =
    case project of
        Elm.Project.Application application ->
            fromApplication dependenciesDict dependencyLocation packageNameStr application

        Elm.Project.Package packageInfo ->
            let
                dependencies : Elm.Project.Deps Elm.Constraint.Constraint
                dependencies =
                    case dependencyLocation of
                        InProjectDeps ->
                            packageInfo.deps

                        InTestDeps ->
                            packageInfo.testDeps
            in
            case List.Extra.find (isPackageWithName packageNameStr) dependencies of
                Just ( packageName, constraint ) ->
                    Just (PackageProject { package = packageInfo, name = packageName, constraint = constraint })

                Nothing ->
                    Nothing


fromApplication : Dict String DependencyList -> DependencyLocation -> String -> Elm.Project.ApplicationInfo -> Maybe ProjectAndDependencyIdentifier
fromApplication dependenciesDict dependencyLocation packageNameStr application =
    let
        dependencies : Elm.Project.Deps Elm.Version.Version
        dependencies =
            case dependencyLocation of
                InProjectDeps ->
                    application.depsDirect

                InTestDeps ->
                    application.testDepsDirect

        dependencyVersionDict : Dict String Elm.Version.Version
        dependencyVersionDict =
            [ application.depsDirect
            , application.depsIndirect
            , application.testDepsDirect
            , application.testDepsIndirect
            ]
                |> List.concat
                |> List.map (\( name, version ) -> ( Elm.Package.toString name, version ))
                |> Dict.fromList

        getDependenciesAndVersion : Elm.Package.Name -> Elm.Project.Deps Elm.Version.Version
        getDependenciesAndVersion name =
            case Dict.get (Elm.Package.toString name) dependenciesDict of
                Just deps ->
                    packageDependencies dependencyVersionDict deps

                Nothing ->
                    []
    in
    case List.Extra.find (isPackageWithName packageNameStr) dependencies of
        Just ( packageName, version ) ->
            Just
                (ApplicationProject
                    { application = application
                    , name = packageName
                    , version = version
                    , getDependenciesAndVersion = getDependenciesAndVersion
                    }
                )

        Nothing ->
            Nothing


toProject : ProjectAndDependencyIdentifier -> Elm.Project.Project
toProject projectAndDependencyIdentifier =
    case projectAndDependencyIdentifier of
        ApplicationProject { application } ->
            Elm.Project.Application application

        PackageProject { package } ->
            Elm.Project.Package package


removeProjectDependency : ProjectAndDependencyIdentifier -> ProjectAndDependencyIdentifier
removeProjectDependency projectAndDependencyIdentifier =
    case projectAndDependencyIdentifier of
        ApplicationProject ({ application } as project) ->
            let
                depsDirect : List ( Elm.Package.Name, Elm.Version.Version )
                depsDirect =
                    List.filter (\pkg -> pkg |> isPackageWithName (Elm.Package.toString project.name) |> not) application.depsDirect

                depsIndirect : Elm.Project.Deps Elm.Version.Version
                depsIndirect =
                    listIndirectDependencies project.getDependenciesAndVersion depsDirect
            in
            ApplicationProject
                { project
                    | application =
                        { application
                            | depsDirect = depsDirect
                            , depsIndirect = depsIndirect
                            , testDepsIndirect =
                                listIndirectDependencies project.getDependenciesAndVersion application.testDepsDirect
                                    |> List.filter (\dep -> not (List.member dep depsDirect || List.member dep depsIndirect))
                        }
                }

        PackageProject ({ package } as project) ->
            PackageProject
                { project
                    | package =
                        { package
                            | deps = List.filter (\pkg -> pkg |> isPackageWithName (Elm.Package.toString project.name) |> not) package.deps
                        }
                }


listIndirectDependencies : (Elm.Package.Name -> Elm.Project.Deps Elm.Version.Version) -> Elm.Project.Deps Elm.Version.Version -> Elm.Project.Deps Elm.Version.Version
listIndirectDependencies getDependenciesAndVersion baseDependencies =
    listIndirectDependenciesHelp getDependenciesAndVersion baseDependencies [] []
        |> List.filter (\dep -> not (List.member dep baseDependencies))


listIndirectDependenciesHelp : (Elm.Package.Name -> Elm.Project.Deps Elm.Version.Version) -> Elm.Project.Deps Elm.Version.Version -> List Elm.Package.Name -> Elm.Project.Deps Elm.Version.Version -> Elm.Project.Deps Elm.Version.Version
listIndirectDependenciesHelp getDependenciesAndVersion dependenciesToLookAt visited indirectDependencies =
    case List.filter (\( name, _ ) -> not (List.member name visited)) dependenciesToLookAt of
        [] ->
            indirectDependencies

        ( name, version ) :: restOfDependenciesToLookAt ->
            listIndirectDependenciesHelp
                getDependenciesAndVersion
                (getDependenciesAndVersion name ++ restOfDependenciesToLookAt)
                (name :: visited)
                (( name, version ) :: indirectDependencies)


packageDependencies : Dict String Elm.Version.Version -> DependencyList -> List ( Elm.Package.Name, Elm.Version.Version )
packageDependencies dependencyVersionDict dependencies =
    dependencies
        |> List.filterMap
            (\name ->
                Dict.get (Elm.Package.toString name) dependencyVersionDict
                    |> Maybe.map (Tuple.pair name)
            )


addTestDependency : ProjectAndDependencyIdentifier -> ProjectAndDependencyIdentifier
addTestDependency projectAndDependencyIdentifier =
    case projectAndDependencyIdentifier of
        ApplicationProject ({ application } as project) ->
            let
                testDepsDirect : List ( Elm.Package.Name, Elm.Version.Version )
                testDepsDirect =
                    ( project.name, project.version ) :: application.testDepsDirect
            in
            ApplicationProject
                { project
                    | application =
                        { application
                            | testDepsDirect = testDepsDirect
                            , testDepsIndirect =
                                listIndirectDependencies
                                    project.getDependenciesAndVersion
                                    testDepsDirect
                                    |> List.filter (\dep -> not (List.member dep application.depsDirect || List.member dep application.depsIndirect))
                        }
                }

        PackageProject ({ package } as project) ->
            PackageProject
                { project
                    | package =
                        { package
                            | testDeps = ( project.name, project.constraint ) :: package.testDeps
                        }
                }


removeTestDependency : ProjectAndDependencyIdentifier -> ProjectAndDependencyIdentifier
removeTestDependency projectAndDependencyIdentifier =
    case projectAndDependencyIdentifier of
        ApplicationProject ({ application } as project) ->
            let
                testDepsDirect : List ( Elm.Package.Name, Elm.Version.Version )
                testDepsDirect =
                    List.filter (\pkg -> pkg |> isPackageWithName (Elm.Package.toString project.name) |> not) application.testDepsDirect
            in
            ApplicationProject
                { project
                    | application =
                        { application
                            | testDepsDirect = testDepsDirect
                            , testDepsIndirect =
                                listIndirectDependencies
                                    project.getDependenciesAndVersion
                                    testDepsDirect
                                    |> List.filter (\dep -> not (List.member dep application.depsDirect || List.member dep application.depsIndirect))
                        }
                }

        PackageProject ({ package } as project) ->
            PackageProject
                { project
                    | package =
                        { package
                            | testDeps = List.filter (\pkg -> pkg |> isPackageWithName (Elm.Package.toString project.name) |> not) package.testDeps
                        }
                }


isPackageWithName : String -> ( Elm.Package.Name, a ) -> Bool
isPackageWithName packageName ( packageName_, _ ) =
    packageName == Elm.Package.toString packageName_
