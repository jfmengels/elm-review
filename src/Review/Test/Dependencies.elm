module Review.Test.Dependencies exposing (elmCore, elmParser, elmUrl)

import Review.Project.Dependency exposing (Dependency)
import Review.Test.Dependencies.ElmCore


elmCore : Dependency
elmCore =
    Review.Test.Dependencies.ElmCore
