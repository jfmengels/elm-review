module Docs.NoMissingChangelogEntry exposing
    ( rule
    , Configuration, defaults, withPathToChangelog
    )

{-|

@docs rule

    config =
        [ Docs.NoMissingChangelogEntry.rule Docs.NoMissingChangelogEntry.defaults
        ]

@docs Configuration, defaults, withPathToChangelog


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review/example --rules Docs.NoMissingChangelogEntry
```

-}

import Elm.Project exposing (Project)
import Elm.Version
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME
-}
rule : Configuration -> Rule
rule (Configuration { changelogPath }) =
    Rule.newProjectRuleSchema "Docs.NoMissingChangelogEntry" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withExtraFilesProjectVisitor [ Maybe.withDefault defaultPath changelogPath ] (extraFilesVisitor changelogPath)
        |> Rule.fromProjectRuleSchema


type Configuration
    = Configuration { changelogPath : Maybe String }


defaults : Configuration
defaults =
    Configuration { changelogPath = Nothing }


defaultPath : String
defaultPath =
    "CHANGELOG.md"


withPathToChangelog : String -> Configuration -> Configuration
withPathToChangelog changelogPath _ =
    Configuration { changelogPath = Just changelogPath }


type alias ProjectContext =
    { elmJsonVersion : String
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { elmJsonVersion = "1.0.0"
    }


elmJsonVisitor : Maybe { a | project : Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeElmJsonData context =
    case maybeElmJsonData of
        Just { project } ->
            case project of
                Elm.Project.Package { version } ->
                    ( [], { context | elmJsonVersion = Elm.Version.toString version } )

                Elm.Project.Application _ ->
                    -- TODO Report an error
                    ( [], context )

        Nothing ->
            ( [], context )


extraFilesVisitor : Maybe String -> List { path : String, content : String } -> ProjectContext -> ( List (Rule.Error { useErrorForModule : () }), ProjectContext )
extraFilesVisitor changelogPath files context =
    case List.head files of
        Just { content } ->
            if String.contains context.elmJsonVersion content then
                ( [], context )

            else
                ( [ Rule.globalError
                        { message = "Missing entry in CHANGELOG.md for version " ++ context.elmJsonVersion
                        , details = [ "It seems you have or are ready to release a new version of your package, but forgot to include releases notes for it in your CHANGELOG.md file." ]
                        }
                  ]
                , context
                )

        Nothing ->
            case changelogPath of
                Nothing ->
                    ( [ Rule.globalError
                            { message = "Could not find the CHANGELOG.md file"
                            , details =
                                [ "I was looking for the CHANGELOG.md file next to your project's elm.json file but couldn't find it. Please make sure that the spelling is correct."
                                , "If your changelog is named differently or is in a different location, then you can configure this rule to look for it in a different location:"
                                , """    config =
        [ Docs.NoMissingChangelogEntry.defaults
            |> Docs.NoMissingChangelogEntry.withPathToChangelog "path/to/your/changelog.md"
            |> Docs.NoMissingChangelogEntry.rule
        ]"""
                                , "Note that the path is relative your project's elm.json file."
                                ]
                            }
                      ]
                    , context
                    )

                Just customPath ->
                    ( [ Rule.globalError
                            { message = "Could not find the " ++ customPath ++ " changelog file"
                            , details =
                                [ "I was looking for the " ++ customPath ++ " changelog file but couldn't find it. Please make sure that the path you specified through Docs.NoMissingChangelogEntry.withPathToChangelog is correct."
                                , "Also note that the path you specify has to be relative to your project's elm.json file."
                                ]
                            }
                      ]
                    , context
                    )
