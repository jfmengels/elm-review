module LintConfig exposing (config)

{-| Do not rename the LintConfig module or the config function, because
`elm-lint` will look for these.

To add packages that contain rules, add them to this lint project using

    `elm install author/packagename`

-}

-- import Lint.Rule.NoDebug
-- import Lint.Rule.NoExtraBooleanComparison
-- import Lint.Rule.NoImportingEverything

import Lint.Rule exposing (Rule)
import NoUnused.CustomTypeConstructors
import NoUnused.Variables


config : List Rule
config =
    [ -- Lint.Rule.NoDebug.rule
      -- , Lint.Rule.NoExtraBooleanComparison.rule
      -- , Lint.Rule.NoImportingEverything.rule { exceptions = [] }
      NoUnused.Variables.rule
    , NoUnused.CustomTypeConstructors.rule
    ]
