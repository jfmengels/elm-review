module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.NoMissingChangelogEntry
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoForbiddenWords
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoSimpleLetBody
import NoUnnecessaryTrailingUnderscore
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Review.FilePattern as FilePattern
import NoUnused.CustomTypeConstructorArgs
import Css.NoUnknownClasses

config : List Rule
config =
    [ Docs.UpToDateReadmeLinks.rule
    , Docs.ReviewLinksAndSections.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , Docs.ReviewAtDocs.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , Docs.NoMissing.rule { document = onlyExposed, from = exposedModules }
    , Docs.NoMissingChangelogEntry.rule Docs.NoMissingChangelogEntry.defaults
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoExposingEverything.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoMissingTypeExpose.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/Review/Rule.elm"
            , "src/Review/Fix.elm"
            ]
    , NoUnused.CustomTypeConstructorArgs.rule
        |> Rule.ignoreErrorsForFiles [ "src/Review/Cache/ContextHash.elm" ]
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule

    --, NoUnnecessaryTrailingUnderscore.rule
    , NoUnused.Variables.rule
    , NoSimpleLetBody.rule
    , NoPrematureLetComputation.rule
    , NoForbiddenWords.rule [ "REPLACEME" ]
    , Css.NoUnknownClasses.cssFiles [ FilePattern.include "src/**/*.css"]
            -- |> Css.NoUnknownClasses.withCssUsingFunctions cssUsingFunctions
            |> Css.NoUnknownClasses.rule
    ]
        |> List.map (Rule.ignoreErrorsForDirectories [ "src/Vendor/", "tests/Vendor/" ])
        |> List.map (Rule.ignoreErrorsForFiles [ "tests/NoUnused/Patterns/NameVisitor.elm" ])
