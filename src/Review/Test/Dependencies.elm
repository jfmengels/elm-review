module Review.Test.Dependencies exposing (elmCore, elmHtml)

import Review.Project.Dependency exposing (Dependency)
import Review.Test.Dependencies.ElmCore
import Review.Test.Dependencies.ElmHtml


elmCore : Dependency
elmCore =
    Review.Test.Dependencies.ElmCore


elmHtml : Dependency
elmHtml =
    Review.Test.Dependencies.ElmHtml
