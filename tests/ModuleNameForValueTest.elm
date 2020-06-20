module ModuleNameForValueTest exposing (all)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Fixtures.Dependencies as Dependencies
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Scope
import Test exposing (Test, test)


all : Test
all =
    Test.describe "Scope.moduleNameForValue"
        [ forModuleRule
        , forProjectRule
        ]


forModuleRule : Test
forModuleRule =
    Test.describe "module rule"
        [ test "should return the module that defined the value" <|
            \() ->
                """module A exposing (..)
import Bar as Baz exposing (baz)
import ExposesSomeThings exposing (..)
import ExposesEverything exposing (..)
import Foo.Bar
import Html exposing (..)
import Http exposing (get)

localValue = 1

a = localValue
    unknownValue
    exposedElement
    nonExposedElement
    elementFromExposesEverything
    VariantA
    Foo.bar
    Foo.Bar
    Baz.foo
    baz
    button
    Http.get
    get
    always
    True
    Just
"""
                    |> Review.Test.runWithProjectData project moduleRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = """
<nothing>.localValue -> <nothing>.localValue
<nothing>.unknownValue -> <nothing>.unknownValue
<nothing>.exposedElement -> <nothing>.exposedElement
<nothing>.nonExposedElement -> <nothing>.nonExposedElement
<nothing>.elementFromExposesEverything -> <nothing>.elementFromExposesEverything
<nothing>.VariantA -> <nothing>.VariantA
Foo.bar -> Foo.bar
Foo.Bar -> Foo.Bar
Baz.foo -> Bar.foo
<nothing>.baz -> Bar.baz
<nothing>.button -> Html.button
Http.get -> Http.get
<nothing>.get -> Http.get
<nothing>.always -> Basics.always
<nothing>.True -> Basics.True
<nothing>.Just -> Maybe.Just"""
                            , details = [ "details" ]
                            , under = "module"
                            }
                        ]
        ]


forProjectRule : Test
forProjectRule =
    Test.describe "project rule"
        [ test "should return the module that defined the value" <|
            \() ->
                [ """module A exposing (..)
import Bar as Baz exposing (baz)
import ExposesSomeThings exposing (..)
import ExposesEverything exposing (..)
import Foo.Bar
import Html exposing (..)
import Http exposing (get)
import Something.B as Something
import Something.C as Something

localValue = 1
localValueValueToBeShadowed = 1
type Msg = SomeMsgToBeShadowed

a = localValue
    localValueValueToBeShadowed
    SomeMsgToBeShadowed
    SomeOtherMsg
    Something.b
    Something.c
    Something.BAlias
    Something.Bar
    unknownValue
    exposedElement
    nonExposedElement
    elementFromExposesEverything
    VariantA
    Foo.bar
    Foo.Bar
    Baz.foo
    baz
    button
    Http.get
    get
    always
    True
    Just
""", """module ExposesSomeThings exposing (SomeOtherTypeAlias, exposedElement)
type NonExposedCustomType = Variant
type alias SomeOtherTypeAlias = {}
exposedElement = 1
nonExposedElement = 2
""", """module ExposesEverything exposing (..)
type SomeCustomType = VariantA | VariantB
type alias SomeTypeAlias = {}
type Msg = SomeMsgToBeShadowed | SomeOtherMsg
elementFromExposesEverything = 1
localValueValueToBeShadowed = 1
""", """module Something.B exposing (..)
b = 1
type Foo = Bar
type alias BAlias = {}
""", """module Something.C exposing (..)
c = 1
""" ]
                    |> Review.Test.runOnModulesWithProjectData project projectRule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = """
<nothing>.localValue -> <nothing>.localValue
<nothing>.localValueValueToBeShadowed -> <nothing>.localValueValueToBeShadowed
<nothing>.SomeMsgToBeShadowed -> <nothing>.SomeMsgToBeShadowed
<nothing>.SomeOtherMsg -> ExposesEverything.SomeOtherMsg
Something.b -> Something.B.b
Something.c -> Something.C.c
Something.BAlias -> Something.B.BAlias
Something.Bar -> Something.B.Bar
<nothing>.unknownValue -> <nothing>.unknownValue
<nothing>.exposedElement -> ExposesSomeThings.exposedElement
<nothing>.nonExposedElement -> <nothing>.nonExposedElement
<nothing>.elementFromExposesEverything -> ExposesEverything.elementFromExposesEverything
<nothing>.VariantA -> ExposesEverything.VariantA
Foo.bar -> Foo.bar
Foo.Bar -> Foo.Bar
Baz.foo -> Bar.foo
<nothing>.baz -> Bar.baz
<nothing>.button -> Html.button
Http.get -> Http.get
<nothing>.get -> Http.get
<nothing>.always -> Basics.always
<nothing>.True -> Basics.True
<nothing>.Just -> Maybe.Just"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        ]


type alias ModuleContext =
    { scope : Scope.ModuleContext
    , texts : List String
    }


project : Project
project =
    Project.new
        |> Project.addDependency Dependencies.elmCore
        |> Project.addDependency Dependencies.elmHtml


projectRule : Rule
projectRule =
    Rule.newProjectRuleSchema "TestRule" { scope = Scope.initialProjectContext }
        |> Scope.addProjectVisitors
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule =
                \_ _ projectContext ->
                    { scope = Scope.fromProjectToModule projectContext.scope
                    , texts = []
                    }
            , fromModuleToProject =
                \_ moduleNameNode moduleContext ->
                    { scope = Scope.fromModuleToProject moduleNameNode moduleContext.scope
                    }
            , foldProjectContexts = \a b -> { scope = Scope.foldProjectContexts a.scope b.scope }
            }
        |> Rule.fromProjectRuleSchema


moduleRule : Rule
moduleRule =
    Rule.newModuleRuleSchema "TestRule" { scope = Scope.initialModuleContext, texts = [] }
        |> Scope.addModuleVisitors
        |> moduleVisitor
        |> Rule.fromModuleRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


expressionVisitor : Node Expression -> Rule.Direction -> ModuleContext -> ( List nothing, ModuleContext )
expressionVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Expression.FunctionOrValue moduleName name ) ->
            let
                nameInCode : String
                nameInCode =
                    case moduleName of
                        [] ->
                            "<nothing>." ++ name

                        _ ->
                            String.join "." moduleName ++ "." ++ name

                realName : String
                realName =
                    case Scope.moduleNameForValue context.scope name moduleName of
                        [] ->
                            "<nothing>." ++ name

                        moduleName_ ->
                            String.join "." moduleName_ ++ "." ++ name
            in
            ( [], { context | texts = context.texts ++ [ nameInCode ++ " -> " ++ realName ] } )

        _ ->
            ( [], context )


finalEvaluation : ModuleContext -> List (Error {})
finalEvaluation context =
    if List.isEmpty context.texts then
        []

    else
        [ Rule.error
            { message = "\n" ++ String.join "\n" context.texts
            , details = [ "details" ]
            }
            { start = { row = 1, column = 1 }
            , end = { row = 1, column = 7 }
            }
        ]
