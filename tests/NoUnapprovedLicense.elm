module NoUnapprovedLicense exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.License
import Elm.Project
import Elm.Syntax.Range exposing (Range)
import Json.Encode as Encode
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbid the use of dependencies that use unknown or forbidden licenses.

    config =
        [ NoUnapprovedLicense.rule
            { allowed = [ "BSD-3-Clause", "MIT" ]
            , forbidden = [ "GPL-3.0-only", "GPL-3.0-or-later" ]
            }
        ]

If the license of a dependency is in the `allowed` list, the dependency will not be reported.
If it's in the `forbidden` list, the dependency will be reported as an error.
If it's in neither, the dependency will be reported but with a different message asking you
to add the license to either list.


## Usage as an insight rule

If instead of enforcing a restriction on the licenses, you wish to have an overview of the licenses used in your project,
you can run the rule as an insight rule (using `elm-review --report=json --extract`), which would yield an output like
the following:

```json
{
  "NoRedInk/elm-json-decode-pipeline": "BSD-3-Clause",
  "elm-explorations/markdown": "BSD-3-Clause",
  "elm-explorations/test": "BSD-3-Clause",
  "elm/browser": "BSD-3-Clause",
  "elm/core": "BSD-3-Clause",
  "elm/html": "BSD-3-Clause",
  "elm/http": "BSD-3-Clause",
  "elm/json": "BSD-3-Clause",
  "elm/parser": "BSD-3-Clause",
  "elm/random": "BSD-3-Clause",
  "elm/time": "BSD-3-Clause",
  "elm/url": "BSD-3-Clause",
  "elm/virtual-dom": "BSD-3-Clause",
  "rtfeldman/elm-iso8601-date-strings": "BSD-3-Clause"
}
```

-}
rule : { allowed : List String, forbidden : List String } -> Rule
rule configuration =
    Rule.newProjectRuleSchema "NoUnapprovedLicense" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withFinalProjectEvaluation
            (finalEvaluationForProject
                { allowed = Set.fromList configuration.allowed
                , forbidden = Set.fromList configuration.forbidden
                }
            )
        |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependenciesVisitor dependencies projectContext =
    let
        licenses : Dict String String
        licenses =
            Dict.foldl
                (\packageName dependency acc ->
                    case Dependency.elmJson dependency of
                        Elm.Project.Package { license } ->
                            Dict.insert packageName (Elm.License.toString license) acc

                        Elm.Project.Application _ ->
                            acc
                )
                Dict.empty
                dependencies
    in
    ( [], { projectContext | licenses = licenses } )



-- PROJECT VISITORS


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject projectContext =
    case maybeProject of
        Just { elmJsonKey } ->
            ( [], { projectContext | elmJsonKey = Just elmJsonKey } )

        Nothing ->
            ( [], projectContext )



-- CONTEXT


type alias ProjectContext =
    { elmJsonKey : Maybe Rule.ElmJsonKey
    , licenses : Dict String String
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { elmJsonKey = Nothing
    , licenses = Dict.empty
    }



-- FINAL EVALUATION


finalEvaluationForProject : { allowed : Set String, forbidden : Set String } -> ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluationForProject { allowed, forbidden } projectContext =
    case projectContext.elmJsonKey of
        Just elmJsonKey ->
            Dict.foldl
                (\name license acc ->
                    if Set.member license allowed then
                        acc

                    else if Set.member license forbidden then
                        Rule.errorForElmJson elmJsonKey
                            (\elmJson ->
                                { message = "Forbidden license `" ++ license ++ "` for dependency `" ++ name ++ "`"
                                , details = [ "This license has been marked as forbidden and you should therefore not use this package." ]
                                , range = findPackageNameInElmJson name elmJson
                                }
                            )
                            :: acc

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
                            :: acc
                )
                []
                projectContext.licenses

        Nothing ->
            []


dataExtractor : ProjectContext -> Encode.Value
dataExtractor projectContext =
    Encode.dict identity Encode.string projectContext.licenses


findPackageNameInElmJson : String -> String -> Range
findPackageNameInElmJson packageName elmJson =
    findPackageNameInElmJsonHelp packageName (String.lines elmJson) 0


findPackageNameInElmJsonHelp : String -> List String -> Int -> Range
findPackageNameInElmJsonHelp packageName lines row =
    case lines of
        [] ->
            { start = { row = 1, column = 1 }, end = { row = 10000, column = 1 } }

        line :: rest ->
            case String.indexes ("\"" ++ packageName ++ "\"") line of
                [] ->
                    findPackageNameInElmJsonHelp packageName rest (row + 1)

                column :: _ ->
                    { start =
                        { row = row + 1
                        , column = column + 2
                        }
                    , end =
                        { row = row + 1
                        , column = column + String.length packageName + 2
                        }
                    }
