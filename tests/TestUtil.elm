module TestUtil exposing (ruleTester)

import Regex
import Lint.Types exposing (Error)


spacesRegex : Regex.Regex
spacesRegex =
    Regex.regex "\n              "


ruleTester : (String -> List Error) -> String -> List Error
ruleTester rule str =
    Regex.replace Regex.All spacesRegex (\_ -> "\n") str
        |> rule
