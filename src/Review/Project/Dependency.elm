module Review.Project.Dependency exposing
    ( Dependency
    , name, elmJson, modules
    , create
    )

{-| Functions to create and read data about the project's dependencies.

You may need to use use this module if you want

  - to create test cases where the project has specific dependencies
  - to make `elm-review` run in a new environment

You can safely ignore this module if you just want to write a review rule that
does not look at the project's dependencies.

@docs Dependency


# Access

@docs name, elmJson, modules


# Create

@docs create

-}

import Elm.Docs
import Elm.Project



-- DEFINITION


{-| Represents a dependency of the project.
-}
type Dependency
    = Dependency
        { name : String
        , elmJson : Elm.Project.Project
        , modules : List Elm.Docs.Module
        }


{-| Create a dependency.

You will need to use this if you try to make `elm-review` run in a new environment,
but you can safely ignore it if you just want to write a review rule or run it
in existing environments like the CLI tool.

You could something like the following to create a test project in your tests.

    import Elm.Docs
    import Elm.Project
    import Elm.Type as Type
    import Json.Decode as Decode
    import Review.Project as Project exposing (Project)
    import Review.Project.Dependency as Dependency exposing (Dependency)

    testProject : Project
    testProject =
        Project.new
            |> Project.addDependency

    dependency : String -> Dependency
    dependency license =
        Dependency.create
            "author/dependency"
            (createElmJson rawElmJson)
            modules

    modules : List Elm.Docs.Module
    modules =
        [ { name = "Foo"
          , comment = ""
          , unions = []
          , aliases = []
          , values =
                [ { name = "someFunction"
                  , comment = ""
                  , tipe = Type.Lambda (Type.Var "a") (Type.Var "b")
                  }
                ]
          , binops = []
          }
        ]

    createElmJson : String -> Elm.Project.Project
    createElmJson rawElmJson =
        case Decode.decodeString Elm.Project.decoder rawElmJson of
            Ok elmJson ->
                elmJson

            Err _ ->
                Debug.todo "Invalid elm.json contents in tests"

    rawElmJson : String
    rawElmJson =
        """
    {
      "type": "package",
      "name": "author/dependency",
      "summary": "Summary",
      "license": "MIT",
      "version": "1.0.0",
      "exposed-modules": [
        "Foo"
      ],
      "elm-version": "0.19.0 <= v < 0.20.0",
      "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
      },
      "test-dependencies": {}
    }
    """

-}
create : String -> Elm.Project.Project -> List Elm.Docs.Module -> Dependency
create name_ elmJson_ modules_ =
    Dependency
        { name = name_
        , elmJson = elmJson_
        , modules = modules_
        }



-- ACCESS


{-| Get the name of the dependency.
-}
name : Dependency -> String
name (Dependency dependency) =
    dependency.name


{-| Get the [contents of the project's `elm.json`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.1/Elm-Project#Project)
file.
-}
elmJson : Dependency -> Elm.Project.Project
elmJson (Dependency dependency) =
    dependency.elmJson


{-| Get the list of [modules](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.1/Elm-Docs#Module)
contained in the dependency.
-}
modules : Dependency -> List Elm.Docs.Module
modules (Dependency dependency) =
    dependency.modules
