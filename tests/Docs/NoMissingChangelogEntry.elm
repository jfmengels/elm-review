module Docs.NoMissingChangelogEntry exposing (rule)

{-|

@docs rule

-}

import Elm.Project exposing (Project)
import Elm.Version
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ Docs.NoMissingChangelogEntry.rule
        ]


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
rule : Rule
rule =
    Rule.newProjectRuleSchema "Docs.NoMissingChangelogEntry" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.fromProjectRuleSchema


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
