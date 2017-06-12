module TestUtil exposing (ruleTester)

import Regex
import Lint.Types exposing (LintRule, Error)


spacesRegex : Regex.Regex
spacesRegex =
    Regex.regex "\n              "


ruleTester : (LintRule) -> LintRule
ruleTester rule str =
    Regex.replace Regex.All spacesRegex (\_ -> "\n") str
        |> rule
