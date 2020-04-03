module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoDebug.Log
import NoDebug.TodoOrToString
import NoTodoComment
import NoUnused.CustomTypeConstructors2
import NoUnused.Variables
import NoUnusedDependencies
import NoUnusedExports
import NoUnusedModules
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoUnused.CustomTypeConstructors2.rule
    , NoUnused.Variables.rule
    , NoUnusedDependencies.rule
    , NoUnusedExports.rule
    , NoUnusedModules.rule

    --, NoTodoComment.rule
    --    |> Rule.ignoreErrorsForFiles [ "NoTodoComment" ]
    ]
        |> List.map (Rule.ignoreErrorsForDirectories [ "src/Vendor/" ])
