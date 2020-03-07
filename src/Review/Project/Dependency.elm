module Review.Project.Dependency exposing (Dependency, create)

{-| TODO Documentation

@docs Dependency, create

-}

import Elm.Docs
import Elm.Project
import Elm.Version



-- DEFINITION


{-| TODO Documentation
-}
type alias Dependency =
    { name : String
    , version : Elm.Version.Version
    , elmJson : Elm.Project.Project
    , modules : List Elm.Docs.Module
    }


{-| TODO Documentation
-}
create : String -> Elm.Version.Version -> Elm.Project.Project -> List Elm.Docs.Module -> Dependency
create name_ version_ elmJson_ modules_ =
    { name = name_
    , version = version_
    , elmJson = elmJson_
    , modules = modules_
    }
