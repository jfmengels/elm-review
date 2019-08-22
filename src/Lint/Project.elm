module Lint.Project exposing
    ( Project
    , elmJson
    , new, withElmJson
    )

{-| Represents project-related data, that a rule can access to get more information.


# Definition

@docs Project


# Access

@docs elmJson


# Build

@docs new, withElmJson

-}

import Elm.Project



-- DEFINITION


{-| Represents all kinds of details about the project, such as the contents of
the `elm.json` file.
-}
type Project
    = Project { elmJson : Maybe Elm.Project.Project }



-- ACCESS


{-| Get the contents of the `elm.json` file, if available.

This will give you a `Project` type from the
[`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.0/Elm-Project)
package, so you will need to install and use it to gain access to the
information inside the `elm.json` file.

-}
elmJson : Project -> Maybe Elm.Project.Project
elmJson (Project data) =
    data.elmJson



-- BUILD


{-| Create a new Project.
-}
new : Project
new =
    Project { elmJson = Nothing }


{-| Add the contents of the `elm.json` file to the project details.
-}
withElmJson : Elm.Project.Project -> Project -> Project
withElmJson elmJson_ (Project data) =
    Project { data | elmJson = Just elmJson_ }
