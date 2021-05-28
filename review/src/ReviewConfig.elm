module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Documentation.ReadmeLinksPointToCurrentVersion
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoSimpleLetBody
import NoUnnecessaryTrailingUnderscore
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ Documentation.ReadmeLinksPointToCurrentVersion.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoExposingEverything.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]

    --, NoMissingTypeExpose.rule
    --    |> Rule.ignoreErrorsForFiles [ "src/Review/Rule.elm" ]
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoSimpleLetBody.rule

    --, NoUnnecessaryTrailingUnderscore.rule
    , NoUnused.Variables.rule
    , NoSimpleLetBody.rule
    ]
        |> List.map (Rule.ignoreErrorsForDirectories [ "src/Vendor/", "tests/Vendor/" ])
        |> List.map (Rule.ignoreErrorsForFiles [ "tests/NoUnused/Patterns/NameVisitor.elm" ])
