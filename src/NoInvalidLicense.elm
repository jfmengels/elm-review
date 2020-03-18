module NoInvalidLicense exposing (rule)

{-| Forbid the use of dependencies that use unknown or forbidden licenses.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.License
import Elm.Package
import Elm.Project
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbid the use of dependencies that use unknown or forbidden licenses.

    config =
        [ NoInvalidLicense.rule
            { allowed = [ "BSD-3-Clause", "MIT" ]
            , forbidden = [ "GPL-3.0-only", "GPL-3.0-or-later" ]
            }
        ]

-}
rule : Configuration -> Rule
rule configuration =
    Rule.newProjectRuleSchema "NoInvalidLicense"
        { moduleVisitor = moduleVisitor
        , initProjectContext = initProjectContext
        , fromProjectToModule = fromProjectToModule
        , fromModuleToProject = fromModuleToProject
        , foldProjectContexts = foldProjectContexts
        }
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withFinalProjectEvaluation (finalEvaluationForProject configuration)
        |> Rule.fromProjectRuleSchema


type alias Configuration =
    { allowed : List String
    , forbidden : List String
    }


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor (\_ context -> ( [], context ))


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependenciesVisitor dependencies projectContext =
    let
        licenses : Dict String String
        licenses =
            dependencies
                |> Dict.toList
                |> List.filterMap
                    (\( packageName, dependency ) ->
                        case Dependency.elmJson dependency of
                            Elm.Project.Package { license } ->
                                Just ( packageName, Elm.License.toString license )

                            Elm.Project.Application _ ->
                                Nothing
                    )
                |> Dict.fromList
    in
    ( [], { projectContext | licenses = licenses } )



-- PROJECT VISITORS


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject projectContext =
    case maybeProject of
        Just { elmJsonKey, project } ->
            let
                directProjectDependencies : Set String
                directProjectDependencies =
                    case project of
                        Elm.Project.Package { deps } ->
                            deps
                                |> List.map (Tuple.first >> Elm.Package.toString)
                                |> Set.fromList

                        Elm.Project.Application { depsDirect } ->
                            depsDirect
                                |> List.map (Tuple.first >> Elm.Package.toString)
                                |> Set.fromList
            in
            ( []
            , { projectContext
                | elmJsonKey = Just elmJsonKey
                , directProjectDependencies = directProjectDependencies
              }
            )

        Nothing ->
            ( [], projectContext )



-- CONTEXT


type alias ProjectContext =
    { elmJsonKey : Maybe Rule.ElmJsonKey
    , licenses : Dict String String
    , directProjectDependencies : Set String
    }


type alias ModuleContext =
    ProjectContext


initProjectContext : ProjectContext
initProjectContext =
    { elmJsonKey = Nothing
    , licenses = Dict.empty
    , directProjectDependencies = Set.empty
    }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule _ _ projectContext =
    projectContext


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject _ _ moduleContext =
    moduleContext


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts _ previousContext =
    previousContext



-- FINAL EVALUATION


finalEvaluationForProject : Configuration -> ProjectContext -> List Error
finalEvaluationForProject configuration projectContext =
    case projectContext.elmJsonKey of
        Just elmJsonKey ->
            let
                allowed : Set String
                allowed =
                    Set.fromList configuration.allowed

                forbidden : Set String
                forbidden =
                    Set.fromList configuration.forbidden

                unknownOrForbiddenLicenses : Dict String String
                unknownOrForbiddenLicenses =
                    projectContext.licenses
                        |> Dict.filter (\_ license -> not <| Set.member license allowed)
            in
            unknownOrForbiddenLicenses
                |> Dict.toList
                |> List.map
                    (\( name, license ) ->
                        if Set.member license forbidden then
                            Rule.errorForElmJson elmJsonKey
                                (\elmJson ->
                                    { message = "Forbidden license `" ++ license ++ "` for dependency `" ++ name ++ "`"
                                    , details = [ "This license has been marked as forbidden and you should therefore not use this package." ]
                                    , range = findPackageNameInElmJson name elmJson
                                    }
                                )

                        else
                            Rule.errorForElmJson elmJsonKey
                                (\elmJson ->
                                    { message = "Unknown license `" ++ license ++ "` for dependency `" ++ name ++ "`"
                                    , details =
                                        [ "Talk to your legal team and see if this license is allowed. If it is allowed, add it to the list of allowed licenses. Otherwise, add it to the list of forbidden licenses and remove this dependency."
                                        , "More info about licenses at https://spdx.org/licenses."
                                        ]
                                    , range = findPackageNameInElmJson name elmJson
                                    }
                                )
                    )

        Nothing ->
            []


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
