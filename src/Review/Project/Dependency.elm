module Review.Project.Dependency exposing
    ( Dependency, create
    , name, elmJson, modules
    )

{-| TODO Documentation

@docs Dependency, create


# Access

@docs name, elmJson, modules

-}

import Elm.Docs
import Elm.Project
import Elm.Version



-- DEFINITION


{-| TODO Documentation
-}
type Dependency
    = Dependency
        { name : String
        , version : Elm.Version.Version
        , elmJson : Elm.Project.Project
        , modules : List Elm.Docs.Module
        }


{-| TODO Documentation
-}
create : String -> Elm.Version.Version -> Elm.Project.Project -> List Elm.Docs.Module -> Dependency
create name_ version_ elmJson_ modules_ =
    Dependency
        { name = name_
        , version = version_
        , elmJson = elmJson_
        , modules = modules_
        }



-- ACCESS


{-| TODO Documentation
-}
name : Dependency -> String
name (Dependency dependency) =
    dependency.name


{-| TODO Documentation
-}
modules : Dependency -> List Elm.Docs.Module
modules (Dependency dependency) =
    dependency.modules


{-| TODO Documentation
-}
elmJson : Dependency -> Elm.Project.Project
elmJson (Dependency dependency) =
    dependency.elmJson
