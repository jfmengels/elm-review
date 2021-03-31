module Review.Test.Dependencies exposing
    ( projectWithElmCore
    , elmCore, elmHtml, elmParser, elmUrl
    )

{-| Pre-built dependencies that you can use for your tests.

If you are looking for a specific dependency not provided by this list, you can create one yourself.
If you only care about a few types or functions, you can re-create them
manually using the API in [`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.0/Elm-Project).
If you need the complete dependency of an existing package, with comments and everything, that is surprisingly difficult, so I made [this
script to generate dependencies](https://github.com/jfmengels/elm-review/blob/master/create-dependency/index.js#L1-L12) to make that easier.


## Dependencies

@docs projectWithElmCore
@docs elmCore, elmHtml, elmParser, elmUrl

-}

import Review.Project as Project exposing (Project)
import Review.Project.Dependency exposing (Dependency)
import Review.Test.Dependencies.ElmCore
import Review.Test.Dependencies.ElmHtml
import Review.Test.Dependencies.ElmParser
import Review.Test.Dependencies.ElmUrl


{-| A project that only contains the `elm/core` dependency.
-}
projectWithElmCore : Project
projectWithElmCore =
    Project.addDependency elmCore Project.new


{-| Dependency for `elm/core`. It contains operators.

It is present by default in `elm-review` tests when you use [`Review.Test.run`](./Review-Test#run) or
[`Review.Test.runOnModules`](./Review-Test#runOnModules), or when you create a new project starting with [`projectWithElmCore`](#projectWithElmCore).

Note that if you create a new project using [`Review.Project.new`](./Review-Project#new), you'll have to manually add it
again with [`Review.Project.addDependency`](./Review-Project#addDependency) if you so wish to.

-}
elmCore : Dependency
elmCore =
    Review.Test.Dependencies.ElmCore.dependency


{-| Dependency for `elm/html`.
-}
elmHtml : Dependency
elmHtml =
    Review.Test.Dependencies.ElmHtml.dependency


{-| Dependency for `elm/parser`. It contains operators.
-}
elmParser : Dependency
elmParser =
    Review.Test.Dependencies.ElmParser.dependency


{-| Dependency for `elm/url`. It contains operators.
-}
elmUrl : Dependency
elmUrl =
    Review.Test.Dependencies.ElmUrl.dependency
