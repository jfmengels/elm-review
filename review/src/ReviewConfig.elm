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
import NoMissingTypeExpose
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
    [ NoUnused.Variables.rule
    ]
        |> List.map (Rule.ignoreErrorsForDirectories [ "src-gen", "src/Vendor/", "tests/Vendor/" ])
        |> List.map (Rule.ignoreErrorsForFiles [ "tests/NoUnused/Patterns/NameVisitor.elm" ])
