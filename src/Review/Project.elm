module Review.Project exposing
    ( Project, ElmJson
    , elmJson, interfaces
    , new, withElmJson, withDependency
    )

{-| Represents project-related data, that a rule can access to get more information.

These will be accessible in rules with functions like [`Review.Rule.withElmJsonVisitor`](./Review-Rule#withElmJsonVisitor).
This module is made to build all of the project-related data that we want
rules to have access to, to later pass it to the [`Review.review`](./Review#review) function.

This module is useful if you try to run `elm-review` by yourself. You can safely
ignore it if you just want to write a review rule.


# Definition

@docs Project, ElmJson


# Access

@docs elmJson, interfaces


# Build

@docs new, withElmJson, withDependency

-}

import Dict exposing (Dict)
import Elm.Docs
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
        , interfaces : Dict String Elm.Docs.Module
        , moduleToDependency : Dict String String
        }


{-| Contents of the `elm.json` file. Alias to
[`elm/project-metadata-utils`'s Project project structure](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Project).
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
elmJson (Project project) =
    project.elmJson


{-| Get the interfaces for every dependency in the project.

This will give you a `Project` type from the
[`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.0/Elm-Project)
package, so you will need to install and use it to gain access to the
information inside the `elm.json` file.

-}
interfaces : Project -> Dict String Elm.Docs.Module
interfaces (Project project) =
    project.interfaces



-- BUILD


{-| Create a new Project.
-}
new : Project
new =
    Project
        { elmJson = Nothing
        , interfaces = Dict.empty
        , moduleToDependency = Dict.empty
        }


{-| Add the content of the `elm.json` file to the project details, making it
available for rules to access using
[`Review.Rule.withElmJsonVisitor`](./Review-Rule#withElmJsonVisitor).
-}
withElmJson : ElmJson -> Project -> Project
withElmJson elmJson_ (Project project) =
    Project { project | elmJson = Just elmJson_ }


{-| Add a dependency to the project. These will be available for rules to make
better assumptions on what is happening in the code.

Knowing the dependencies of the project will also help better parse the source
files, since the dependencies will allow us to know the precedence and
associativity of operators, which has an impact on the resulting AST when
parsing a file.

-}
withDependency : { r | packageName : String, interfaces : List Elm.Docs.Module } -> Project -> Project
withDependency dependency (Project project) =
    Project
        { project
            | interfaces =
                dependency.interfaces
                    |> List.map (\module_ -> ( module_.name, module_ ))
                    |> Dict.fromList
                    |> Dict.union project.interfaces
            , moduleToDependency =
                dependency.interfaces
                    |> List.map (\module_ -> ( module_.name, dependency.packageName ))
                    |> Dict.fromList
                    |> Dict.union project.moduleToDependency
        }
