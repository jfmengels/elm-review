module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import RemoveTypeAnnotations
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ -- Documentation.ReadmeLinksPointToCurrentVersion.rule
      -- , NoDebug.Log.rule
      -- , NoDebug.TodoOrToString.rule
      --     |> Rule.ignoreErrorsForDirectories [ "tests/" ]
      -- , NoExposingEverything.rule
      --     |> Rule.ignoreErrorsForDirectories [ "tests/" ]
      -- , NoImportingEverything.rule []
      -- , NoMissingTypeAnnotation.rule
      --NoMissingTypeAnnotationInLetIn.rule
      RemoveTypeAnnotations.rule

    --     |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    --
    -- --, NoMissingTypeExpose.rule
    -- --    |> Rule.ignoreErrorsForFiles [ "src/Review/Rule.elm" ]
    -- , NoUnused.CustomTypeConstructors.rule []
    -- , NoUnused.Dependencies.rule
    -- , NoUnused.Exports.rule
    -- , NoUnused.Modules.rule
    -- , NoUnused.Parameters.rule
    -- , NoUnused.Patterns.rule
    -- , NoUnused.Variables.rule
    ]
        -- |> List.map (Rule.ignoreErrorsForDirectories [ "src/Vendor/", "tests/Vendor/" ])
        -- |> List.map (Rule.ignoreErrorsForFiles [ "tests/NoUnused/Patterns/NameVisitor.elm" ])
        |> List.map (Rule.ignoreErrorsForDirectories [ "src-gen/" ])
