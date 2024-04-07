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
import Elm.Syntax.Range exposing (Range)
import Elm.Version
import Review.FilePattern as FilePattern
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME
-}
rule : Configuration -> Rule
rule (Configuration { changelogPath }) =
    Rule.newProjectRuleSchema "Docs.NoMissingChangelogEntry" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withExtraFilesProjectVisitor [ FilePattern.include (Maybe.withDefault defaultPath changelogPath) ] (extraFilesVisitor changelogPath)
        |> Rule.providesFixesForProjectRule
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
    { elmJsonVersion : Maybe String
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { elmJsonVersion = Nothing
    }


elmJsonVisitor : Maybe { a | project : Project } -> ProjectContext -> ( List (Rule.Error scope), ProjectContext )
elmJsonVisitor maybeElmJsonData context =
    case maybeElmJsonData of
        Just { project } ->
            case project of
                Elm.Project.Package { version } ->
                    ( [], { context | elmJsonVersion = Just (Elm.Version.toString version) } )

                Elm.Project.Application _ ->
                    ( [ Rule.globalError
                            { message = "The Elm project is unexpectedly an application"
                            , details = [ "This rule only supports Elm packages, but doesn't support Elm applications as they don't have a version number. I recommend that you remove this rule from your review configuration." ]
                            }
                      ]
                    , context
                    )

        Nothing ->
            ( [], context )


extraFilesVisitor : Maybe String -> List { fileKey : Rule.ExtraFileKey, path : String, content : String } -> ProjectContext -> ( List (Rule.Error { useErrorForModule : () }), ProjectContext )
extraFilesVisitor changelogPath files context =
    case List.head files of
        Just { fileKey, content } ->
            case context.elmJsonVersion of
                Nothing ->
                    ( [], context )

                Just "1.0.0" ->
                    ( [], context )

                Just elmJsonVersion ->
                    if String.contains elmJsonVersion content then
                        ( [], context )

                    else
                        ( [ reportError fileKey elmJsonVersion content ]
                        , context
                        )

        Nothing ->
            case context.elmJsonVersion of
                Nothing ->
                    ( [], context )

                Just "1.0.0" ->
                    -- TODO Report an error with a changelog skeleton
                    ( [], context )

                Just _ ->
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


reportError : Rule.ExtraFileKey -> String -> String -> Rule.Error scope
reportError fileKey elmJsonVersion content =
    let
        lines : List String
        lines =
            String.lines content

        unreleased : Maybe ( Int, String )
        unreleased =
            findLineWithUnreleased 0 lines
    in
    Rule.errorForExtraFileWithFix
        fileKey
        { message = "Missing entry in CHANGELOG.md for version " ++ elmJsonVersion
        , details = [ "It seems you have or are ready to release a new version of your package, but forgot to include releases notes for it in your CHANGELOG.md file." ]
        }
        (case unreleased of
            Just ( lineNumber, line ) ->
                { start = { row = lineNumber, column = 1 }, end = { row = lineNumber, column = String.length line + 1 } }

            Nothing ->
                { start = { row = 1, column = 1 }, end = { row = 1, column = String.length (List.head lines |> Maybe.withDefault "") + 1 } }
        )
        (case unreleased of
            Just ( lineNumber, _ ) ->
                [ Fix.insertAt { row = lineNumber + 1, column = 1 } ("\n## [" ++ elmJsonVersion ++ "]\n\n") ]

            Nothing ->
                []
        )


findLineWithUnreleased : Int -> List String -> Maybe ( Int, String )
findLineWithUnreleased index lines =
    case lines of
        [] ->
            Nothing

        line :: rest ->
            if String.contains "# [Unreleased]" line then
                Just ( index + 1, line )

            else
                findLineWithUnreleased (index + 1) rest
