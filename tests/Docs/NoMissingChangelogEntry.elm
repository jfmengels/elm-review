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
        |> Rule.withExtraFilesProjectVisitor [ changelogPath ] extraFilesVisitor
        |> Rule.fromProjectRuleSchema


type Configuration
    = Configuration { changelogPath : String }


defaults : Configuration
defaults =
    Configuration { changelogPath = "CHANGELOG.md" }


withPathToChangelog : String -> Configuration -> Configuration
withPathToChangelog changelogPath _ =
    Configuration { changelogPath = changelogPath }


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


extraFilesVisitor : List { path : String, content : String } -> ProjectContext -> ( List (Rule.Error { useErrorForModule : () }), ProjectContext )
extraFilesVisitor files context =
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
            ( [], context )
