module ModuleNameForValueTest exposing (all)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Fixtures.Dependencies as Dependencies
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project as Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, test)


all : Test
all =
    Test.describe "Scope.moduleNameForValue"
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
    { baz | a = 1 }
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
    { lookupTable : ModuleNameLookupTable
    , texts : List String
    }


project : Project
project =
    Project.new
        |> Project.addDependency Dependencies.elmCore
        |> Project.addDependency Dependencies.elmHtml


projectRule : Rule
projectRule =
    Rule.newModuleRuleSchemaUsingContextCreator "TestRule" contextCreator
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


contextCreator : Rule.ContextCreator () ModuleContext
contextCreator =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , texts = []
            }
        )
        |> Rule.withModuleNameLookupTable


expressionVisitor : Node Expression -> ModuleContext -> ( List nothing, ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue moduleName name ->
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
                    case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                        Just [] ->
                            "<nothing>." ++ name

                        Just moduleName_ ->
                            String.join "." moduleName_ ++ "." ++ name

                        Nothing ->
                            "!!! UNKNOWN !!!"
            in
            ( [], { context | texts = context.texts ++ [ nameInCode ++ " -> " ++ realName ] } )

        Expression.RecordUpdateExpression (Node range name) _ ->
            let
                realName : String
                realName =
                    case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                        Just [] ->
                            "<nothing>." ++ name

                        Just moduleName_ ->
                            String.join "." moduleName_ ++ "." ++ name

                        Nothing ->
                            "!!! UNKNOWN !!!"
            in
            ( [], { context | texts = context.texts ++ [ "<nothing>." ++ name ++ " -> " ++ realName ] } )

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
