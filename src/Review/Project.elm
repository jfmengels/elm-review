module Review.Project exposing
    ( Project, ElmJson
    , elmJson, interfaces
    , new, withElmJson
    )

{-| Represents project-related data, that a rule can access to get more information.

These will be accessible in rules with functions like [`Review.Rule.withElmJsonVisitor`](./Review-Rule#withElmJsonVisitor).
This module is made to build all of the project-related data that we want
rules to have access to, to later pass it to the [`Review.review`](./Review#review) function.


# Definition

@docs Project, ElmJson


# Access

@docs elmJson, interfaces


# Build

@docs new, withElmJson

-}

import Dict exposing (Dict)
import Elm.Interface exposing (Interface)
import Elm.Project
import Elm.Syntax.ModuleName exposing (ModuleName)



-- DEFINITION


{-| Represents all kinds of details about the project, such as the contents of
the `elm.json` file.
-}
type Project
    = Project
        { elmJson : Maybe ElmJson
        , interfaces : Dict ModuleName Interface
        }


{-| Contents of the `elm.json` file. Alias to
[`elm/project-metadata-utils`'s Project data structure](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Project).
-}
type alias ElmJson =
    Elm.Project.Project



-- ACCESS


{-| Get the contents of the `elm.json` file, if available.

This will give you a `Project` type from the
[`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.0/Elm-Project)
package, so you will need to install and use it to gain access to the
information inside the `elm.json` file.

-}
elmJson : Project -> Maybe ElmJson
elmJson (Project data) =
    data.elmJson


{-| Get the interfaces for every dependency in the project.

This will give you a `Project` type from the
[`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.0/Elm-Project)
package, so you will need to install and use it to gain access to the
information inside the `elm.json` file.

-}
interfaces : Project -> Dict ModuleName Interface
interfaces (Project data) =
    data.interfaces



-- BUILD


{-| Create a new Project.
-}
new : Project
new =
    Project
        { elmJson = Nothing
        , interfaces = Dict.empty
        }


{-| Add the contents of the `elm.json` file to the project details.
-}
withElmJson : ElmJson -> Project -> Project
withElmJson elmJson_ (Project data) =
    Project { data | elmJson = Just elmJson_ }
