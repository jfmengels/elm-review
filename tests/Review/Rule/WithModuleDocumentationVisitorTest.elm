module Review.Rule.WithModuleDocumentationVisitorTest exposing (all)

import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Rule.withModuleDocumentationVisitor"
        [ test "should pass Nothing if there is no module documentation" <|
            \() ->
                """module ModuleName exposing (a)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should pass the module documentation if there is one" <|
            \() ->
                """module ModuleName exposing (a)
{-| module documentation
-}

{-| function doc
-}
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "{-| module documentation\n-}"
                            , details = [ "No details" ]
                            , under = "{-| module documentation\n-}"
                            }
                        ]
        , test "should pass the module documentation if there is one, even if it looks like a port documentation" <|
            \() ->
                """port module ModuleName exposing (a)
{-| module documentation
-}
port a : String -> Cmd msg
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "{-| module documentation\n-}"
                            , details = [ "No details" ]
                            , under = "{-| module documentation\n-}"
                            }
                        ]
        , test "should not mis-categorize the documentation for a port for the module documentation (doc after import)" <|
            \() ->
                """port module ModuleName exposing (a)
import A

{-| port documentation
-}
port a : String -> Cmd msg
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not mis-categorize the documentation for a port for the module documentation (doc after a declaration)" <|
            \() ->
                """port module ModuleName exposing (a)

b = 1

{-| port documentation
-}
port a : String -> Cmd msg
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


rule : Rule
rule =
    Rule.newModuleRuleSchema "WithModuleDocumentationVisitorTestRule" ()
        |> Rule.withModuleDocumentationVisitor (\node () -> ( moduleDocumentationVisitor node, () ))
        |> Rule.fromModuleRuleSchema


moduleDocumentationVisitor : Maybe (Node String) -> List (Error {})
moduleDocumentationVisitor maybeModuleDocumentation =
    case maybeModuleDocumentation of
        Just moduleDocumentation ->
            [ Rule.error
                { message = Node.value moduleDocumentation
                , details = [ "No details" ]
                }
                (Node.range moduleDocumentation)
            ]

        Nothing ->
            []
