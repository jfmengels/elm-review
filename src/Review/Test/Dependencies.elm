module Review.Test.Dependencies exposing (elmCore, elmHtml, elmParser, elmUrl)

import Review.Project.Dependency exposing (Dependency)
import Review.Test.Dependencies.ElmCore
import Review.Test.Dependencies.ElmHtml
import Review.Test.Dependencies.ElmParser
import Review.Test.Dependencies.ElmUrl


elmCore : Dependency
elmCore =
    Review.Test.Dependencies.ElmCore.dependency


elmHtml : Dependency
elmHtml =
    Review.Test.Dependencies.ElmHtml.dependency


elmParser : Dependency
elmParser =
    Review.Test.Dependencies.ElmParser.dependency


elmUrl : Dependency
elmUrl =
    Review.Test.Dependencies.ElmUrl.dependency
