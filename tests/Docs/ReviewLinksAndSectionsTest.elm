module Docs.ReviewLinksAndSectionsTest exposing (all)

import Docs.ReviewLinksAndSections exposing (rule)
import Elm.Project
import Json.Decode
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)



-- TODO Report links to dependencies?
-- TODO Force links to dependencies to be for the minimal version?
-- TODO Report unmatched `[foo]`?
-- TODO Report unused `[foo]: #b` links? (only if there are no "unmatched errors")


all : Test
all =
    describe "Docs.ReviewLinksAndSections"
        [ localLinkTests
        , linksToOtherFilesTest
        , linksDependingOnExposition
        , linksThroughPackageRegistryTest
        , duplicateSectionsTests
        , unnecessaryLinksTests
        , externalResourcesTests
        , linksWithAltTextTests
        ]


localLinkTests : Test
localLinkTests =
    describe "Local links"
        [ test "should not report link to an existing sibling section from declaration documentation" <|
            \() ->
                """module A exposing (a, b)
b = 1

{-| [link](#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report a link to an unknown sibling section from declaration documentation" <|
            \() ->
                """module A exposing (a)
{-| [link](#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#b"
                            }
                        ]
        , test "should not report link to an existing sibling section from module documentation" <|
            \() ->
                """module A exposing (a, b)
{-| [link](#b)
-}

import B
b = 1
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report a link to an unknown sibling section from module documentation" <|
            \() ->
                """module A exposing (a)
{-| [link](#b)
-}

import B
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#b"
                            }
                        ]
        , test "should not report link to an existing sibling section from port documentation" <|
            \() ->
                """module A exposing (a, b)
b = 1
{-| [link](#b)
-}
port a : String -> Cmd msg
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report a link to an unknown sibling section from port documentation" <|
            \() ->
                """module A exposing (a)
{-| [link](#b)
-}
port a : String -> Cmd msg
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#b"
                            }
                        ]
        , test "should not report a link to a known sibling section from declaration documentation when everything is exposed" <|
            \() ->
                """module A exposing (..)
b = 1

{-| [link](#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report a link to an unknown sibling section from declaration documentation when everything is exposed" <|
            \() ->
                """module A exposing (..)
{-| [link](#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#b"
                            }
                        ]
        , test "should not report link references that target existing sections" <|
            \() ->
                """module A exposing (a, b)
b = 1
{-| this is a [link] reference

[link]: #b
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report link references that link to missing sections" <|
            \() ->
                """module A exposing (..)
{-| this is a [link] reference

[link]: #b

-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#b"
                            }
                        ]
        , test "should consider an exposed type alias to be an existing section" <|
            \() ->
                """module A exposing (..)
type alias B = {}
{-| [link](#B)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should consider an exposed custom type to be an existing section" <|
            \() ->
                """module A exposing (..)
type B = C
{-| [link](#B)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should consider an exposed port to be an existing section" <|
            \() ->
                """module A exposing (..)
port b : Int -> Cmd msg

{-| [link](#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should consider an infix declaration to be an existing section (operator exists)" <|
            \() ->
                """module A exposing (..)
infix right 5 (++) = append

{-| [link](#++)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should consider an infix declaration to be an existing section (operator doesn't exist)" <|
            \() ->
                """module A exposing (..)
{-| [link](#++)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#++"
                            }
                        ]
        , test "should consider a header inside a function documentation comment to be an existing section (h1)" <|
            \() ->
                """module A exposing (..)
{-|
# Section
-}
b = 1
{-| [link](#section)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should consider a header inside a function documentation comment to be an existing section (h2)" <|
            \() ->
                """module A exposing (..)
{-|
## Section
-}
b = 1
{-| [link](#section)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not consider a 'h7' header inside a function documentation comment to be an existing section" <|
            \() ->
                """module A exposing (..)
{-|
####### Section
-}
b = 1
{-| [link](#section)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#section"
                            }
                        ]
        , test "should consider a header inside a module documentation comment to be an existing section" <|
            \() ->
                """module A exposing (..)
{-|
# Section
-}

import B

b = 1
{-| [link](#section)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should slugify complex headings" <|
            \() ->
                """module A exposing (..)
{-|
# Section *with* ~some~ _spaces_ and\\_ $thi.ngs . links

### `section`
### question?

-}
b = 1
{-|
[1](#section-_with_-some-_spaces_-and-_-thi-ngs-links)
[2](#-section-)
[3](#question-)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report links to unknown modules" <|
            \() ->
                """module A exposing (..)
{-| [link](B-C)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to non-existing module B.C"
                            , details = [ "This is a dead link." ]
                            , under = "B-C"
                            }
                        ]
        , test "should report multiple links on the same line" <|
            \() ->
                """module A exposing (a)
{-| [link](#b) [link](#c) [link](#d) [link](#e)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#b"
                            }
                        , Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#c"
                            }
                        , Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#d"
                            }
                        , Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#e"
                            }
                        ]
        ]


linksToOtherFilesTest : Test
linksToOtherFilesTest =
    describe "Links to other files"
        [ test "should report links to sections in unknown modules" <|
            \() ->
                """module A exposing (..)
{-| [link](B#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to non-existing module B"
                            , details = [ "This is a dead link." ]
                            , under = "B#b"
                            }
                        ]
        , test "should not report links to existing sections in a different module" <|
            \() ->
                [ """module A exposing (..)
{-| [link](B#b)
-}
a = 2
""", """module B exposing (b)
b = 1
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should report links to missing sections in a different module" <|
            \() ->
                [ """module A exposing (..)
{-| [link](B#b)
-}
a = 2
""", """module B exposing (c)
c = 1
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Link points to a non-existing section or element"
                                , details = [ "This is a dead link." ]
                                , under = "B#b"
                                }
                            ]
                          )
                        ]
        , test "should not report links to existing sections in a different module (using ./)" <|
            \() ->
                [ """module A exposing (..)
{-| [link](./B#b)
-}
a = 2
""", """module B exposing (b)
b = 1
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should report links to missing sections in a different module (using ./)" <|
            \() ->
                [ """module A exposing (..)
{-| [link](./B#b)
-}
a = 2
""", """module B exposing (c)
c = 1
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Link points to a non-existing section or element"
                                , details = [ "This is a dead link." ]
                                , under = "./B#b"
                                }
                            ]
                          )
                        ]
        , test "should not report links to existing sections inside the README" <|
            \() ->
                """module A exposing (a)
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "# A\n[link](#a)" } Project.new)
                        rule
                    |> Review.Test.expectNoErrors
        , test "should report links to missing sections inside the README" <|
            \() ->
                """module A exposing (a)
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "# A\n[link](#b)" } Project.new)
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "#b"
                            }
                        ]
        , test "should not report links to existing sections inside the README (using ./)" <|
            \() ->
                """module A exposing (a)
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "# A\n[link](./#a)" } Project.new)
                        rule
                    |> Review.Test.expectNoErrors
        , test "should report links to missing sections inside the README (using ./)" <|
            \() ->
                """module A exposing (a)
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "# A\n[link](./#b)" } Project.new)
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "./#b"
                            }
                        ]
        , test "should not report links to existing sections from another module inside the README" <|
            \() ->
                """module A exposing (a)
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "[link](./A#a)" } Project.new)
                        rule
                    |> Review.Test.expectNoErrors
        , test "should report links to missing sections from another module inside the README" <|
            \() ->
                """module A exposing (a)
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "[link](./A#b)" } Project.new)
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "./A#b"
                            }
                        ]
        , test "should report links to README when there is no README (without slug)" <|
            \() ->
                """module A exposing (a)              
{-| [link](./)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to missing README"
                            , details = [ "elm-review only looks for a 'README.md' located next to your 'elm.json'. Maybe it's positioned elsewhere or named differently?" ]
                            , under = "./"
                            }
                        ]
        , test "should report links to README when there is no README (with slug)" <|
            \() ->
                """module A exposing (a)
{-| [link](./#b)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to missing README"
                            , details = [ "elm-review only looks for a 'README.md' located next to your 'elm.json'. Maybe it's positioned elsewhere or named differently?" ]
                            , under = "./#b"
                            }
                        ]
        ]


linksThroughPackageRegistryTest : Test
linksThroughPackageRegistryTest =
    describe "Links through the package registry"
        [ test "should report links to unknown modules for the current version" <|
            \() ->
                [ """module A exposing (..)
{-| [link](https://package.elm-lang.org/packages/author/package/1.0.0/Unknown)
-}
a = 2
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to non-existing module Unknown"
                            , details = [ "This is a dead link." ]
                            , under = "https://package.elm-lang.org/packages/author/package/1.0.0/Unknown"
                            }
                        ]
        , test "should not report links to known modules for the current version" <|
            \() ->
                [ """module A exposing (..)
{-| [link](https://package.elm-lang.org/packages/author/package/1.0.0/A)
-}
a = 2
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectNoErrors
        , test "should not report links to known modules for the current version (with trailing slash)" <|
            \() ->
                [ """module A exposing (..)
{-| [link](https://package.elm-lang.org/packages/author/package/1.0.0/A/)
-}
a = 2
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectNoErrors
        , test "should report links to unknown modules for latest" <|
            \() ->
                """module A exposing (..)
{-| [link](https://package.elm-lang.org/packages/author/package/latest/Unknown)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to non-existing module Unknown"
                            , details = [ "This is a dead link." ]
                            , under = "https://package.elm-lang.org/packages/author/package/latest/Unknown"
                            }
                        ]
        , test "should not report links for different version of the package" <|
            \() ->
                """module A exposing (..)
{-| [link](https://package.elm-lang.org/packages/author/package/2.3.4/Unknown)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectNoErrors
        , test "should report links to unknown section of README" <|
            \() ->
                """module A exposing (..)
{-| [link](https://package.elm-lang.org/packages/author/package/1.0.0/#unknown)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "# A" } packageProjectWithoutFiles)
                        rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to a non-existing section or element"
                            , details = [ "This is a dead link." ]
                            , under = "https://package.elm-lang.org/packages/author/package/1.0.0/#unknown"
                            }
                        ]
        , test "should not report links to known sections of README" <|
            \() ->
                """module A exposing (..)
{-| [link](https://package.elm-lang.org/packages/author/package/2.3.4/#a)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "# A" } packageProjectWithoutFiles)
                        rule
                    |> Review.Test.expectNoErrors
        , test "should not report links for different packages" <|
            \() ->
                """module A exposing (..)
{-| [link](https://package.elm-lang.org/packages/other-author/package/latest/Unknown/)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectNoErrors
        ]


linksDependingOnExposition : Test
linksDependingOnExposition =
    describe "Links depending on exposition"
        [ test "should not report links from non-exposed modules to non-exposed modules" <|
            \() ->
                [ """module NotExposed exposing (a)
{-| [link](./AlsoNotExposed)
-}
a = 2
""", """module AlsoNotExposed exposing (b)
b = 1
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report links from non-exposed modules to exposed modules" <|
            \() ->
                """module NotExposed exposing (a)
{-| [link](./Exposed)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report links from exposed modules to exposed modules" <|
            \() ->
                """module Exposed2 exposing (a)
{-| [link](./Exposed)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should report links from exposed modules to non-exposed modules" <|
            \() ->
                [ """module Exposed2 exposing (a)
{-| [link](./NotExposed)
-}
a = 2
""", """module NotExposed exposing (b)
b = 1
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProject rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Exposed2"
                          , [ Review.Test.error
                                { message = "Link in public documentation points to non-exposed module"
                                , details = [ "Users will not be able to follow the link." ]
                                , under = "./NotExposed"
                                }
                            ]
                          )
                        ]
        , test "should not report links from non-exposed sections to non-exposed sections" <|
            \() ->
                [ """module Exposed2 exposing (b)
b = a

{-| [link](./Exposed3#hidden)
-}
a = 1
""", """module Exposed3 exposing (exposed)
exposed = 2

{-|
# Hidden
-}
a = 3
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report links from non-exposed sections to exposed sections" <|
            \() ->
                [ """module Exposed2 exposing (b)
b = a

{-| [link](./Exposed3#exposed)
-}
a = 1
""", """module Exposed3 exposing (exposed)
exposed = 2

{-|
# Hidden
-}
a = 3
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should report links from exposed sections to non-exposed sections" <|
            \() ->
                [ """module Exposed2 exposing (b)
{-| [link](./Exposed3#hidden)
-}
b = 1
""", """module Exposed3 exposing (exposed)
exposed = 2

{-|
# Hidden
-}
a = 3
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProject rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Exposed2"
                          , [ Review.Test.error
                                { message = "Link in public documentation points to non-exposed section"
                                , details = [ "Users will not be able to follow the link." ]
                                , under = "./Exposed3#hidden"
                                }
                            ]
                          )
                        ]
        , test "should report links from README to non-exposed modules" <|
            \() ->
                """module NotExposed exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "[link](./NotExposed)" } packageProject)
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Link in public documentation points to non-exposed module"
                            , details = [ "Users will not be able to follow the link." ]
                            , under = "./NotExposed"
                            }
                        ]
        , test "should not report links from README to exposed modules" <|
            \() ->
                """module NotExposed exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "[link](./Exposed)" } packageProject)
                        rule
                    |> Review.Test.expectNoErrors
        , test "should report links from README to non-exposed sections" <|
            \() ->
                """module Exposed2 exposing (a)
a = 1
{-|
# Section
-}
b = 1
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "[link](./Exposed2#section)" } packageProject)
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Link in public documentation points to non-exposed section"
                            , details = [ "Users will not be able to follow the link." ]
                            , under = "./Exposed2#section"
                            }
                        ]
        , test "should not report links from README to non-exposed sections in non-package projects" <|
            \() ->
                """module Exposed2 exposing (a)
a = 1
{-|
# Section
-}
b = 1
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = "[link](./Exposed2#section)" } Project.new)
                        rule
                    |> Review.Test.expectNoErrors
        ]


duplicateSectionsTests : Test
duplicateSectionsTests =
    describe "Duplicate sections"
        [ test "should report duplicate sections (declaration doc comment)" <|
            \() ->
                """module Exposed2 exposing (error, exposed)
{-|
# Error
-}
exposed = 1

error = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Duplicate section"
                            , details = [ "There are multiple sections that will result in the same id, meaning that links may point towards the wrong element." ]
                            , under = "# Error"
                            }
                        ]
        , test "should report duplicate sections (module doc comment)" <|
            \() ->
                """module Exposed2 exposing (error)
{-|
# Error
-}

error = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Duplicate section"
                            , details = [ "There are multiple sections that will result in the same id, meaning that links may point towards the wrong element." ]
                            , under = "# Error"
                            }
                        ]
        , test "should report duplicate sections (all in declaration doc comments)" <|
            \() ->
                """module Exposed2 exposing (error)
{-|
# Some section

# Some Section
-}

error = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Duplicate section"
                            , details = [ "There are multiple sections that will result in the same id, meaning that links may point towards the wrong element." ]
                            , under = "# Some Section"
                            }
                        ]
        , test "should report duplicate sections in README" <|
            \() ->
                """module Exposed2 exposing (a)
a = 1
"""
                    |> Review.Test.runWithProjectData
                        (Project.addReadme { path = "README.md", content = """
# Some section

# Some Section
""" } packageProject)
                        rule
                    |> Review.Test.expectErrorsForReadme
                        [ Review.Test.error
                            { message = "Duplicate section"
                            , details = [ "There are multiple sections that will result in the same id, meaning that links may point towards the wrong element." ]
                            , under = "# Some Section"
                            }
                        ]
        ]


unnecessaryLinksTests : Test
unnecessaryLinksTests =
    describe "Unnecessary links"
        [ test "should report links to #" <|
            \() ->
                """module A exposing (..)
{-|
[link](#)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link to empty section is unnecessary"
                            , details = [ "Links to # not followed by an id don't provide any value to the user. I suggest to either strip the # or remove the link." ]
                            , under = "#"
                            }
                        ]
        , test "should report links to a module with #" <|
            \() ->
                """module A exposing (..)
{-|
[link](A#)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link to empty section is unnecessary"
                            , details = [ "Links to # not followed by an id don't provide any value to the user. I suggest to either strip the # or remove the link." ]
                            , under = "A#"
                            }
                        ]
        ]


externalResourcesTests : Test
externalResourcesTests =
    describe "External resources"
        [ test "should not report links to external resources without a protocol when the project is not a package" <|
            \() ->
                """module A exposing (..)
{-|
[link](www.google.com)
![](./image.png)
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report links to external resources with a protocol" <|
            \() ->
                """module A exposing (..)
{-|
[link](https://foo.com)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectNoErrors
        , test "should report links to an external resource without a protocol" <|
            \() ->
                """module A exposing (..)
{-|
[link](foo.com)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link to unknown resource without a protocol"
                            , details =
                                [ "I have trouble figuring out what kind of resource is linked here."
                                , "If it should link to a module, then they should be in the form 'Some-Module-Name'."
                                , "If it's a link to an external resource, they should start with a protocol, like `https://www.fruits.com`, otherwise the link will point to an unknown resource on package.elm-lang.org."
                                ]
                            , under = "foo.com"
                            }
                        ]
        , test "should not report links to images with a protocol" <|
            \() ->
                """module A exposing (..)
{-|
[link](https://www.image.com/image.png)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectNoErrors
        , test "should report links to images without a protocol" <|
            \() ->
                """module A exposing (..)
{-|
[link](./image.png)
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link to unknown resource without a protocol"
                            , details =
                                [ "I have trouble figuring out what kind of resource is linked here."
                                , "If it should link to a module, then they should be in the form 'Some-Module-Name'."
                                , "If it's a link to an external resource, they should start with a protocol, like `https://www.fruits.com`, otherwise the link will point to an unknown resource on package.elm-lang.org."
                                ]
                            , under = "./image.png"
                            }
                        ]
        ]


linksWithAltTextTests : Test
linksWithAltTextTests =
    describe "Alt text in links"
        [ test "should report links to unknown modules in links with alt text after a slug" <|
            \() ->
                """module A exposing (a, b)
b = 1

{-| [link](Unknown#section "alt-text")
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to non-existing module Unknown"
                            , details = [ "This is a dead link." ]
                            , under = "Unknown#section"
                            }
                        ]
        , test "should ignore alt text in links with a slug" <|
            \() ->
                """module A exposing (a, b)
b = 1

{-| [link](#b "alt-text")
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report links to unknown modules in links with alt text without a slug" <|
            \() ->
                """module A exposing (a, b)
b = 1

{-| [link](Unknown "alt-text")
-}
a = 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to non-existing module Unknown"
                            , details = [ "This is a dead link." ]
                            , under = "Unknown"
                            }
                        ]
        , test "should report absolute links to unknown modules in links with alt text with a slug" <|
            \() ->
                """module A exposing (a, b)
b = 1

{-| [link](https://package.elm-lang.org/packages/author/package/1.0.0/Unknown "alt-text")
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to non-existing module Unknown"
                            , details = [ "This is a dead link." ]
                            , under = "https://package.elm-lang.org/packages/author/package/1.0.0/Unknown"
                            }
                        ]
        , test "should report absolute links to unknown modules in links with alt text without a slug" <|
            \() ->
                """module A exposing (a, b)
b = 1

{-| [link](https://package.elm-lang.org/packages/author/package/1.0.0/Unknown#section "alt-text")
-}
a = 2
"""
                    |> Review.Test.runWithProjectData packageProjectWithoutFiles rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Link points to non-existing module Unknown"
                            , details = [ "This is a dead link." ]
                            , under = "https://package.elm-lang.org/packages/author/package/1.0.0/Unknown#section"
                            }
                        ]
        ]


packageProject : Project
packageProject =
    Project.addModule
        { path = "src/Exposed", source = "module Exposed exposing (exposed)\nexposed = 1" }
        packageProjectWithoutFiles


packageProjectWithoutFiles : Project
packageProjectWithoutFiles =
    case Json.Decode.decodeString Elm.Project.decoder elmJson of
        Ok project ->
            Project.new
                |> Project.addElmJson
                    { path = "elm.json"
                    , raw = elmJson
                    , project = project
                    }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


elmJson : String
elmJson =
    """{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed",
        "Exposed2",
        "Exposed3"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""
