module Tests.Vendor.NameVisitor exposing (all)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Fuzz
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, describe, fuzz, test)
import Vendor.NameVisitor as NameVisitor


all : Test
all =
    describe "NameVisitor"
        [ describe "Context" contextTests
        , describe "Declarations" declarationTests
        , describe "Expressions" expressionTests
        , describe "Patterns" patternTests
        , describe "Type Annotations" typeAnnotationTests
        , describe "Visitors" visitorTests
        , describe "Project Rules" projectRuleTests
        ]


declarationTests : List Test
declarationTests =
    [ fuzz Fuzz.string "FunctionDeclaration" <|
        \context ->
            """
module Page exposing (view)
view : Node (Html.Attribute msg) -> Html msg
view (Node attribute) = Html.div [ attribute ] []
"""
                |> Review.Test.run (valueOrTypeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedTypeError context "Node"
                        |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                    , expectedTypeError context "Html.Attribute"
                    , expectedTypeError context "Html"
                        |> Review.Test.atExactly { start = { row = 3, column = 37 }, end = { row = 3, column = 41 } }
                    , expectedValueError context "Node"
                        |> Review.Test.atExactly { start = { row = 4, column = 7 }, end = { row = 4, column = 11 } }
                    , expectedValueError context "Html.div"
                    , expectedValueError context "attribute"
                        |> Review.Test.atExactly { start = { row = 4, column = 36 }, end = { row = 4, column = 45 } }
                    ]
    , fuzz Fuzz.string "AliasDeclaration" <|
        \context ->
            """
module Page exposing (Page)
type alias Page =
    { title : String
    , body : List (Html msg)
    }
"""
                |> Review.Test.run (valueOrTypeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedTypeError context "String"
                    , expectedTypeError context "List"
                    , expectedTypeError context "Html"
                    ]
    , fuzz Fuzz.string "CustomTypeDeclaration" <|
        \context ->
            """
module Page exposing (Page)
type Page
    = Home Route.Home
"""
                |> Review.Test.run (valueOrTypeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedTypeError context "Route.Home"
                    ]
    , fuzz Fuzz.string "PortDeclaration" <|
        \context ->
            """
port module Ports exposing (alarm)
port alarm : Json.Encode.Value -> Cmd msg
"""
                |> Review.Test.run (valueOrTypeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedTypeError context "Json.Encode.Value"
                    , expectedTypeError context "Cmd"
                    ]
    ]


expressionTests : List Test
expressionTests =
    [ fuzz Fuzz.string "FunctionOrValue" <|
        \context ->
            """
module Page exposing (view)
view = Html.div [] []
"""
                |> Review.Test.run (valueOrTypeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "Html.div"
                    ]
    , fuzz Fuzz.string "LetDestructuring" <|
        \context ->
            """
module Page exposing (view)
view =
    let
        html = Html.span [] []
    in
    html
"""
                |> Review.Test.run (valueOrTypeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "Html.span"
                    , expectedValueError context "html"
                        |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 9 } }
                    ]
    , fuzz Fuzz.string "LetFunction" <|
        \context ->
            """
module Page exposing (view)
view =
    let
        html : Nested.Html msg
        html (Nested.Node name) = span [] []
    in
    html
"""
                |> Review.Test.run (valueOrTypeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedTypeError context "Nested.Html"
                    , expectedValueError context "Nested.Node"
                    , expectedValueError context "span"
                    , expectedValueError context "html"
                        |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 9 } }
                    ]
    , fuzz Fuzz.string "CaseExpression" <|
        \context ->
            """
module Rule exposing (visitor)
visitor node =
    case Node.value node of
        Expression.FunctionOrValue _ _ ->
            []
"""
                |> Review.Test.run (valueOrTypeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "Node.value"
                    , expectedValueError context "node"
                        |> Review.Test.atExactly { start = { row = 4, column = 21 }, end = { row = 4, column = 25 } }
                    , expectedValueError context "Expression.FunctionOrValue"
                    ]
    , fuzz Fuzz.string "LambdaExpression" <|
        \context ->
            """
module Page exposing (view)
view nodes =
    Html.div [] (List.map (\\(Node name) -> Html.text name))
"""
                |> Review.Test.run (valueOrTypeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "Html.div"
                    , expectedValueError context "List.map"
                    , expectedValueError context "Node"
                    , expectedValueError context "Html.text"
                    , expectedValueError context "name"
                        |> Review.Test.atExactly { start = { row = 4, column = 54 }, end = { row = 4, column = 58 } }
                    ]
    , fuzz Fuzz.string "RecordAccess" <|
        \context ->
            """
module Page exposing (view)
value model =
    model.value
"""
                |> Review.Test.run (valueOrTypeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "model"
                        |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 10 } }
                    ]
    , fuzz Fuzz.string "RecordUpdateExpression" <|
        \context ->
            """
module Page exposing (view)
update model =
    { model | key = value }
"""
                |> Review.Test.run (valueOrTypeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "model"
                        |> Review.Test.atExactly { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } }
                    , expectedValueError context "value"
                    ]
    ]


patternTests : List Test
patternTests =
    [ fuzz Fuzz.string "TuplePattern" <|
        \context ->
            """
module Page exposing (view)
view ( Nested.Node name, Nested.Value value ) =
    Html.div [] [ Html.text (name ++ value) ]
"""
                |> Review.Test.run (valueVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "Nested.Node"
                    , expectedValueError context "Nested.Value"
                    , expectedValueError context "Html.div"
                    , expectedValueError context "Html.text"
                    , expectedValueError context "name"
                        |> Review.Test.atExactly { start = { row = 4, column = 30 }, end = { row = 4, column = 34 } }
                    , expectedValueError context "value"
                        |> Review.Test.atExactly { start = { row = 4, column = 38 }, end = { row = 4, column = 43 } }
                    ]
    , fuzz Fuzz.string "UnConsPattern" <|
        \context ->
            """
module Page exposing (list)
list children =
    case children of
        (Page.First first) :: (Page.Second second) :: _ ->
            Html.text ""
"""
                |> Review.Test.run (valueVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "children"
                        |> Review.Test.atExactly { start = { row = 4, column = 10 }, end = { row = 4, column = 18 } }
                    , expectedValueError context "Page.First"
                    , expectedValueError context "Page.Second"
                    , expectedValueError context "Html.text"
                    ]
    , fuzz Fuzz.string "ListPattern" <|
        \context ->
            """
module Page exposing (list)
list children =
    case children of
        [ Page.First first, Page.Second second ] ->
            Html.text ""
"""
                |> Review.Test.run (valueVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "children"
                        |> Review.Test.atExactly { start = { row = 4, column = 10 }, end = { row = 4, column = 18 } }
                    , expectedValueError context "Page.First"
                    , expectedValueError context "Page.Second"
                    , expectedValueError context "Html.text"
                    ]
    , fuzz Fuzz.string "NamedPattern" <|
        \context ->
            """
module Page exposing (view)
view (Node name) = Html.text name
"""
                |> Review.Test.run (valueVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "Node"
                    , expectedValueError context "Html.text"
                    , expectedValueError context "name"
                        |> Review.Test.atExactly { start = { row = 3, column = 30 }, end = { row = 3, column = 34 } }
                    ]
    , fuzz Fuzz.string "AsPattern" <|
        \context ->
            """
module Page exposing (view)
view ((Page.Document title _) as doc) = Html.text title
"""
                |> Review.Test.run (valueVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "Page.Document"
                    , expectedValueError context "Html.text"
                    , expectedValueError context "title"
                        |> Review.Test.atExactly { start = { row = 3, column = 51 }, end = { row = 3, column = 56 } }
                    ]
    , fuzz Fuzz.string "ParenthesizedPattern" <|
        \context ->
            """
module Page exposing (view)
view (Nested.Node name) = Html.text name
"""
                |> Review.Test.run (valueVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "Nested.Node"
                    , expectedValueError context "Html.text"
                    , expectedValueError context "name"
                        |> Review.Test.atExactly { start = { row = 3, column = 37 }, end = { row = 3, column = 41 } }
                    ]
    ]


typeAnnotationTests : List Test
typeAnnotationTests =
    [ fuzz Fuzz.string "Types" <|
        \context ->
            """
module Page exposing (view)
view : Nested.Node Page.Document msg -> Html msg
view node = div [] []
"""
                |> Review.Test.run (typeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedTypeError context "Nested.Node"
                    , expectedTypeError context "Page.Document"
                    , expectedTypeError context "Html"
                    ]
    , fuzz Fuzz.string "Tupled" <|
        \context ->
            """
module Page exposing (Page)
type Page
    = Page ( Document.Title, Document.List Document.Item )
"""
                |> Review.Test.run (typeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedTypeError context "Document.Title"
                    , expectedTypeError context "Document.List"
                    , expectedTypeError context "Document.Item"
                    ]
    , fuzz Fuzz.string "Record" <|
        \context ->
            """
module Page exposing (Page)
type Page
    = Page { title : Document.Title, body : Document.List Document.Item }
"""
                |> Review.Test.run (typeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedTypeError context "Document.Title"
                    , expectedTypeError context "Document.List"
                    , expectedTypeError context "Document.Item"
                    ]
    , fuzz Fuzz.string "GenericRecord" <|
        \context ->
            """
module Page exposing (Page)
type Page
    = Page { page | title : Document.Title, body : Document.List Document.Item }
"""
                |> Review.Test.run (typeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedTypeError context "Document.Title"
                    , expectedTypeError context "Document.List"
                    , expectedTypeError context "Document.Item"
                    ]
    , fuzz Fuzz.string "FunctionTypeAnnotation" <|
        \context ->
            """
module Page exposing (Viewer)
type Viewer msg
    = Viewer (Document.Title -> Document.List Document.Item -> Html msg)
"""
                |> Review.Test.run (typeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedTypeError context "Document.Title"
                    , expectedTypeError context "Document.List"
                    , expectedTypeError context "Document.Item"
                    , expectedTypeError context "Html"
                    ]
    ]


visitorTests : List Test
visitorTests =
    [ fuzz Fuzz.string "withNameVisitor" <|
        \context ->
            """
module Page exposing (view)
view : Page -> Html msg
view page =
    Html.div [] (Page.body page)
"""
                |> Review.Test.run (nameVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedNameError context "Page"
                        |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                    , expectedNameError context "Html"
                        |> Review.Test.atExactly { start = { row = 3, column = 16 }, end = { row = 3, column = 20 } }
                    , expectedNameError context "Html.div"
                    , expectedNameError context "Page.body"
                    , expectedNameError context "page"
                        |> Review.Test.atExactly { start = { row = 5, column = 28 }, end = { row = 5, column = 32 } }
                    ]
    , fuzz Fuzz.string "withValueVisitor" <|
        \context ->
            """
module Page exposing (view)
view : Page -> Html msg
view page =
    Html.div [] (Page.body page)
"""
                |> Review.Test.run (valueVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedValueError context "Html.div"
                    , expectedValueError context "Page.body"
                    , expectedValueError context "page"
                        |> Review.Test.atExactly { start = { row = 5, column = 28 }, end = { row = 5, column = 32 } }
                    ]
    , fuzz Fuzz.string "withTypeVisitor" <|
        \context ->
            """
module Page exposing (view)
view : Page -> Html msg
view page =
    Html.div [] (Page.body page)
"""
                |> Review.Test.run (typeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedTypeError context "Page"
                        |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                    , expectedTypeError context "Html"
                        |> Review.Test.atExactly { start = { row = 3, column = 16 }, end = { row = 3, column = 20 } }
                    ]
    , fuzz Fuzz.string "withValueOrTypeVisitor" <|
        \context ->
            """
module Page exposing (view)
view : Page -> Html msg
view page =
    Html.div [] (Page.body page)
"""
                |> Review.Test.run (valueOrTypeVisitorRule context)
                |> Review.Test.expectErrors
                    [ expectedTypeError context "Page"
                        |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                    , expectedTypeError context "Html"
                        |> Review.Test.atExactly { start = { row = 3, column = 16 }, end = { row = 3, column = 20 } }
                    , expectedValueError context "Html.div"
                    , expectedValueError context "Page.body"
                    , expectedValueError context "page"
                        |> Review.Test.atExactly { start = { row = 5, column = 28 }, end = { row = 5, column = 32 } }
                    ]
    ]


contextTests : List Test
contextTests =
    [ test "mutates context" <|
        \_ ->
            """
module Page exposing (Page)
view : Page -> Html msg
view page =
    Html.div [] (Page.body page)
"""
                |> Review.Test.run (countingVisitorRule 0)
                |> Review.Test.expectErrors
                    [ expectedNameError "0" "Page"
                        |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                    , expectedNameError "1" "Html"
                        |> Review.Test.atExactly { start = { row = 3, column = 16 }, end = { row = 3, column = 20 } }
                    , expectedNameError "2" "Html.div"
                    , expectedNameError "3" "Page.body"
                    , expectedNameError "4" "page"
                        |> Review.Test.atExactly { start = { row = 5, column = 28 }, end = { row = 5, column = 32 } }
                    ]
    ]


projectRuleTests : List Test
projectRuleTests =
    [ test "works in a project rule schema" <|
        \() ->
            let
                rule : Rule
                rule =
                    Rule.newProjectRuleSchema "TestProjectRule" ()
                        |> Rule.withModuleVisitor
                            (\schema ->
                                schema
                                    |> NameVisitor.withNameVisitor nameVisitor
                            )
                        |> Rule.withModuleContext
                            { foldProjectContexts = \() () -> ()
                            , fromProjectToModule = \_ _ () -> "context"
                            , fromModuleToProject = \_ _ _ -> ()
                            }
                        |> Rule.fromProjectRuleSchema
            in
            """
module A exposing (..)
foo = bar
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ expectedNameError "context" "bar"
                    ]
    ]



-- TEST HELPERS


expectedNameError : String -> String -> Review.Test.ExpectedError
expectedNameError context name =
    Review.Test.error
        { message = "Error for name `" ++ name ++ "`."
        , details = [ "The context of this error was:", context ]
        , under = name
        }


expectedValueError : String -> String -> Review.Test.ExpectedError
expectedValueError context name =
    Review.Test.error
        { message = "Error for value `" ++ name ++ "`."
        , details = [ "The context of this error was:", context ]
        , under = name
        }


expectedTypeError : String -> String -> Review.Test.ExpectedError
expectedTypeError context name =
    Review.Test.error
        { message = "Error for type `" ++ name ++ "`."
        , details = [ "The context of this error was:", context ]
        , under = name
        }



-- RULE HELPERS


nameVisitorRule : String -> Rule
nameVisitorRule context =
    Rule.newModuleRuleSchema "NameVisitor" context
        |> NameVisitor.withNameVisitor nameVisitor
        |> Rule.fromModuleRuleSchema


valueOrTypeVisitorRule : String -> Rule
valueOrTypeVisitorRule context =
    Rule.newModuleRuleSchema "ValueOrTypeVisitor" context
        |> NameVisitor.withValueAndTypeVisitors
            { valueVisitor = valueVisitor
            , typeVisitor = typeVisitor
            }
        |> Rule.fromModuleRuleSchema


valueVisitorRule : String -> Rule
valueVisitorRule context =
    Rule.newModuleRuleSchema "ValueVisitor" context
        |> NameVisitor.withValueVisitor valueVisitor
        |> Rule.fromModuleRuleSchema


typeVisitorRule : String -> Rule
typeVisitorRule context =
    Rule.newModuleRuleSchema "TypeVisitor" context
        |> NameVisitor.withTypeVisitor typeVisitor
        |> Rule.fromModuleRuleSchema


nameVisitor : Node ( ModuleName, String ) -> String -> ( List (Error {}), String )
nameVisitor node context =
    ( [ nameError context node ], context )


valueVisitor : Node ( ModuleName, String ) -> String -> ( List (Error {}), String )
valueVisitor node context =
    ( [ valueError context node ], context )


typeVisitor : Node ( ModuleName, String ) -> String -> ( List (Error {}), String )
typeVisitor node context =
    ( [ typeError context node ], context )


countingVisitorRule : Int -> Rule
countingVisitorRule counter =
    Rule.newModuleRuleSchema "NameVisitor" counter
        |> NameVisitor.withNameVisitor countingVisitor
        |> Rule.fromModuleRuleSchema


countingVisitor : Node ( ModuleName, String ) -> Int -> ( List (Error {}), Int )
countingVisitor node counter =
    ( [ nameError (String.fromInt counter) node ], counter + 1 )


nameError : String -> Node ( ModuleName, String ) -> Error {}
nameError context node =
    Rule.error
        { message = "Error for name `" ++ formatNode node ++ "`."
        , details = [ "The context of this error was:", context ]
        }
        (Node.range node)


valueError : String -> Node ( ModuleName, String ) -> Error {}
valueError context node =
    Rule.error
        { message = "Error for value `" ++ formatNode node ++ "`."
        , details = [ "The context of this error was:", context ]
        }
        (Node.range node)


typeError : String -> Node ( ModuleName, String ) -> Error {}
typeError context node =
    Rule.error
        { message = "Error for type `" ++ formatNode node ++ "`."
        , details = [ "The context of this error was:", context ]
        }
        (Node.range node)


formatNode : Node ( ModuleName, String ) -> String
formatNode node =
    node
        |> Node.value
        |> formatTuple


formatTuple : ( ModuleName, String ) -> String
formatTuple ( moduleName, name ) =
    String.join "." (moduleName ++ [ name ])
