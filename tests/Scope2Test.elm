module Scope2Test exposing (all)

import Dependencies
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Rule)
import Review.Test exposing (ReviewResult)
import Scope2 as Scope
import Test exposing (Test, test)


all : Test
all =
    Test.only <|
        Test.describe "Scope"
            [ realFunctionOrTypeTests
            ]


realFunctionOrTypeTests : Test
realFunctionOrTypeTests =
    Test.describe "Scope.realFunctionOrType"
        [ test "should indicate that module from which a function or value comes from" <|
            \() ->
                """module A exposing (..)
import Bar as Baz exposing (baz)
import Foo exposing (..)
import Foo.Bar
import Html exposing (..)
import Http exposing (get)

a = b
    somethingFromFoo
    Foo.bar
    Foo.Bar
    Baz.foo
    baz
    button
    Http.get
    get
    always
    Just
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = """
<nothing>.b -> <nothing>.b
<nothing>.somethingFromFoo -> <nothing>.somethingFromFoo
Foo.bar -> Foo.bar
Foo.Bar -> Foo.Bar
Baz.foo -> Bar.foo
<nothing>.baz -> Bar.baz
<nothing>.button -> Html.button
Http.get -> Http.get
<nothing>.get -> Http.get
<nothing>.always -> Basics.always
<nothing>.Just -> Maybe.Just"""
                            , details = [ "details" ]
                            , under = "module"
                            }
                        ]
        , test "should indicate that module from which a function or value comes from, with knowledge of what is in other modules" <|
            \() ->
                [ """module A exposing (..)
import Bar as Baz exposing (baz)
import Foo exposing (..)
import Foo.Bar
import Html exposing (..)
import Http exposing (get)

a = b
    somethingFromFoo
    Foo.bar
    Foo.Bar
    Baz.foo
    baz
    button
    Http.get
    get
    always
    Just
""", """module Foo exposing (somethingFromFoo)
somethingFromFoo = 1
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = """
<nothing>.b -> <nothing>.b
<nothing>.somethingFromFoo -> Foo.somethingFromFoo
Foo.bar -> Foo.bar
Foo.Bar -> Foo.Bar
Baz.foo -> Bar.foo
<nothing>.baz -> Bar.baz
<nothing>.button -> Html.button
Http.get -> Http.get
<nothing>.get -> Http.get
<nothing>.always -> Basics.always
<nothing>.Just -> Maybe.Just"""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        , ( "Foo"
                          , [ Review.Test.error
                                { message = ""
                                , details = [ "details" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        ]


type alias GlobalContext =
    { scope : Scope.GlobalContext
    }


type alias ModuleContext =
    { scope : Scope.ModuleContext
    , text : String
    }


project : Project
project =
    Project.new
        |> Project.withDependency Dependencies.elmCore
        |> Project.withDependency Dependencies.elmHtml


scopeGetterSetter =
    { set = \scope context -> { context | scope = scope }
    , get = .scope
    }


rule : Rule
rule =
    Rule.newMultiSchema "TestRule"
        { moduleVisitorSchema =
            \schema ->
                schema
                    |> Scope.addModuleVisitors scopeGetterSetter
                    |> Rule.withExpressionVisitor expressionVisitor
                    |> Rule.withFinalEvaluation finalEvaluation
        , initGlobalContext = { scope = Scope.initGlobalContext }
        , fromGlobalToModule =
            \fileKey moduleNameNode globalContext ->
                { scope = Scope.fromGlobalToModule globalContext.scope
                , text = ""
                }
        , fromModuleToGlobal =
            \fileKey moduleNameNode moduleContext ->
                { scope = Scope.fromModuleToGlobal moduleContext.scope
                }
        , foldGlobalContexts = \a b -> { scope = Scope.foldGlobalContexts a.scope b.scope }
        }
        |> Scope.addGlobalVisitors scopeGetterSetter
        |> Rule.traversingImportedModulesFirst
        |> Rule.fromMultiSchema


expressionVisitor : Node Expression -> Rule.Direction -> ModuleContext -> ( List Rule.Error, ModuleContext )
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
                    case Scope.realFunctionOrType moduleName name context.scope of
                        ( [], name_ ) ->
                            "<nothing>." ++ name_

                        ( moduleName_, name_ ) ->
                            String.join "." moduleName_ ++ "." ++ name_
            in
            ( [], { context | text = context.text ++ "\n" ++ nameInCode ++ " -> " ++ realName } )

        _ ->
            ( [], context )


finalEvaluation : ModuleContext -> List Rule.Error
finalEvaluation context =
    [ Rule.error { message = context.text, details = [ "details" ] }
        { start = { row = 1, column = 1 }
        , end = { row = 1, column = 7 }
        }
    ]
