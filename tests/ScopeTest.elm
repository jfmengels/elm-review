module ScopeTest exposing (all)

import Dependencies
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Rule)
import Review.Test exposing (ReviewResult)
import Scope
import Test exposing (Test, test)


type alias Context =
    { scope : Scope.Context
    , text : String
    }


project : Project
project =
    Project.new
        |> Project.withDependency Dependencies.elmCore
        |> Project.withDependency Dependencies.elmHtml


testRule : Rule -> String -> ReviewResult
testRule rule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Review.Test.runWithProjectData project rule


baseRule :
    Rule.ModuleRuleSchema
        { hasAtLeastOneVisitor : ()
        , canCollectProjectData : ()
        }
        Context
baseRule =
    Rule.newModuleRuleSchema "TestRule" initialContext
        |> Scope.addVisitors


initialContext : Context
initialContext =
    { scope = Scope.initialContext
    , text = ""
    }


all : Test
all =
    Test.describe "Scope"
        [ Test.describe "Scope.realFunctionOrType"
            [ test "should indicate that module from which a function or value comes from" <|
                \() ->
                    let
                        rule : Rule
                        rule =
                            baseRule
                                |> Rule.withExpressionVisitor expressionVisitor
                                |> Rule.withFinalModuleEvaluation finalEvaluation
                                |> Rule.fromModuleRuleSchema

                        expressionVisitor : Node Expression -> Rule.Direction -> Context -> ( List Rule.Error, Context )
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

                        finalEvaluation : Context -> List Rule.Error
                        finalEvaluation context =
                            [ Rule.error { message = context.text, details = [ "details" ] }
                                { start = { row = 1, column = 1 }
                                , end = { row = 1, column = 7 }
                                }
                            ]
                    in
                    testRule rule """
import Bar as Baz exposing (baz)
import Foo
import Foo.Bar
import Html exposing (..)
import Http exposing (get)

a = b
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
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = """
<nothing>.b -> <nothing>.b
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
            ]
        ]
