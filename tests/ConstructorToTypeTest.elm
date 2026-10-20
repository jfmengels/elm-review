module ConstructorToTypeTest exposing (all)

import Dict exposing (Dict)
import Elm.Docs
import Elm.License
import Elm.Package
import Elm.Project
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Version
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Rule)
import Review.Test
import Review.Test.Dependencies
import Review.Test.Dependencies.Unsafe as Unsafe
import Test exposing (Test, test)


all : Test
all =
    test "should return the module that defined the value" <|
        \() ->
            [ """module A exposing (LocalType(..))
import Bar as Baz
import ExposesSomeThings exposing (..)
import ExposesEverything exposing (..)
import ExposesEverything as ExposesEverythingAlias
import Html exposing (..)
import Http exposing (get)
import Something.B as Something
import Something.C as Something
import Url.Parser exposing (..)

type Msg = SomeMsgToBeShadowed
type LocalType = LocalConstructor
type HiddenType = HiddenConstructor
type alias LocalAlias = { a : Bool }

a = SomeOtherMsg
 Something.BAlias
 Something.Bar
 VariantA
 True
 Just
 LocalConstructor
 HiddenConstructor
 LocalAlias
b = case () of
  VariantA -> ()
  ExposesEverything.VariantA -> ()
  ExposesEverythingAlias.VariantA -> ()

someFunction Something.B.Bar =
    let Something.B.Bar = {}
    in ()

notListed = 1
""", """module ExposesSomeThings exposing (SomeOtherTypeAlias, exposedElement)
type NonExposedCustomType = Variant
type alias SomeOtherTypeAlias = {}
""", """module ExposesEverything exposing (..)
type SomeCustomType = VariantA | VariantB
type alias SomeTypeAlias = {}
type Msg = SomeMsgToBeShadowed | SomeOtherMsg
""", """module Something.B exposing (..)
b = 1
type FooType = Bar
type alias BAlias = {}
""" ]
                |> Review.Test.runOnModulesWithProjectData project rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "message"
                            , details = [ """
<nothing>.SomeOtherMsg -> ExposesEverything.Msg
Something.BAlias -> Something.B.BAlias
Something.Bar -> Something.B.FooType
<nothing>.VariantA -> ExposesEverything.SomeCustomType
<nothing>.True -> Basics.Bool
<nothing>.Just -> Maybe.Maybe
<nothing>.LocalConstructor -> <nothing>.LocalType
<nothing>.HiddenConstructor -> <nothing>.HiddenType
<nothing>.LocalAlias -> <nothing>.LocalAlias
<nothing>.VariantA -> ExposesEverything.SomeCustomType
ExposesEverything.VariantA -> ExposesEverything.SomeCustomType
ExposesEverythingAlias.VariantA -> ExposesEverything.SomeCustomType
Something.B.Bar -> Something.B.FooType
""" ]
                            , under = "module"
                            }
                        ]
                      )
                    ]


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , constructorToType : Dict ModuleName (Dict String String)
    , texts : List String
    }


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "TestRule" contextCreator
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionVisitor node context ))
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


{-| TODO Rename
-}
xyz : ( ModuleName, String ) -> ( ModuleName, String ) -> String
xyz ( baseModuleName, constructor ) ( realModuleName, typeName ) =
    join baseModuleName constructor ++ " -> " ++ join realModuleName typeName


{-| TODO Rename
-}
abc : ModuleContext -> ( ModuleName, String ) -> Range -> List String
abc context (( _, constructor ) as base) range =
    case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
        Just realModuleName ->
            case Dict.get realModuleName context.constructorToType |> Maybe.andThen (Dict.get constructor) of
                Just typeName ->
                    [ xyz base ( realModuleName, typeName ) ]

                Nothing ->
                    [ xyz base ( realModuleName, "UNKNOWN" ) ]

        Nothing ->
            [ xyz base ( [ "Unknown-Module" ], "UNKNOWN" ) ]


expressionVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionVisitor (Node range expr) context =
    case expr of
        Expression.FunctionOrValue baseModuleName constructor ->
            { context
                | texts =
                    context.texts
                        ++ abc context ( baseModuleName, constructor ) range
            }

        Expression.CaseExpression { cases } ->
            { context
                | texts =
                    context.texts
                        ++ List.concatMap (Tuple.first >> collectPatterns context) cases
            }

        Expression.LetExpression { declarations } ->
            let
                texts : List String
                texts =
                    List.concatMap
                        (\(Node _ declaration) ->
                            case declaration of
                                Expression.LetFunction function ->
                                    function.declaration
                                        |> Node.value
                                        |> .arguments
                                        |> List.concatMap (collectPatterns context)

                                Expression.LetDestructuring pattern _ ->
                                    collectPatterns context pattern
                        )
                        declarations
            in
            { context | texts = context.texts ++ texts }

        _ ->
            context


collectPatterns : ModuleContext -> Node Pattern -> List String
collectPatterns context (Node range pattern) =
    case pattern of
        Pattern.NamedPattern { moduleName, name } _ ->
            abc context ( moduleName, name ) range

        Pattern.ParenthesizedPattern subPattern ->
            collectPatterns context subPattern

        Pattern.AsPattern subPattern _ ->
            collectPatterns context subPattern

        Pattern.VarPattern _ ->
            []

        Pattern.TuplePattern subPatterns ->
            List.concatMap (collectPatterns context) subPatterns

        Pattern.AllPattern ->
            []

        _ ->
            Debug.todo ("Other patterns in case expressions are not handled: " ++ Debug.toString pattern)


join : ModuleName -> String -> String
join moduleName name =
    if List.isEmpty moduleName then
        "<nothing>." ++ name

    else
        String.join "." moduleName ++ "." ++ name


contextCreator : Rule.ContextCreator () ModuleContext
contextCreator =
    Rule.initContextCreator
        (\lookupTable constructorToType () ->
            { lookupTable = lookupTable
            , constructorToType = constructorToType
            , texts = []
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withConstructorToType


finalEvaluation : ModuleContext -> List (Rule.Error {})
finalEvaluation context =
    if List.isEmpty context.texts then
        []

    else
        [ Rule.error
            { message = "message"
            , details = [ "\n" ++ String.join "\n" context.texts ++ "\n" ]
            }
            { start = { row = 1, column = 1 }
            , end = { row = 1, column = 7 }
            }
        ]


project : Project
project =
    Review.Test.Dependencies.projectWithElmCore
        |> Project.addElmJson applicationElmJson
        |> Project.addDependency Review.Test.Dependencies.elmHtml
        |> Project.addDependency Review.Test.Dependencies.elmParser
        |> Project.addDependency Review.Test.Dependencies.elmUrl
        |> Project.addDependency xyz2dependency


applicationElmJson : { path : String, raw : String, project : Elm.Project.Project }
applicationElmJson =
    { path = "elm.json"
    , raw = """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.0",
            "elm/html": "1.0.0",
            "elm/parser": "1.0.0",
            "elm/url": "1.0.0",
            "abc/xyz": "1.0.0"
        },
        "indirect": {
            "abc/xyz2": "1.0.0"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}"""
    , project =
        Elm.Project.Application
            { elm = Elm.Version.one
            , dirs = []
            , depsDirect =
                [ ( unsafePackageName "elm/core", Elm.Version.one )
                , ( unsafePackageName "elm/html", Elm.Version.one )
                , ( unsafePackageName "elm/parser", Elm.Version.one )
                , ( unsafePackageName "elm/url", Elm.Version.one )
                , ( unsafePackageName "abc/xyz", Elm.Version.one )
                ]
            , depsIndirect = [ ( unsafePackageName "abc/xyz2", Elm.Version.one ) ]
            , testDepsDirect = []
            , testDepsIndirect = []
            }
    }


xyz2dependency : Dependency
xyz2dependency =
    Dependency.create "abc/xyz2"
        xyz2elmJson
        xyz2dependencyModules


xyz2elmJson : Elm.Project.Project
xyz2elmJson =
    Elm.Project.Package
        { elm = Unsafe.constraint "0.19.0 <= v < 0.20.0"
        , exposed = Elm.Project.ExposedList [ Unsafe.moduleName "Element" ]
        , license = Elm.License.fromString "BSD-3-Clause" |> Maybe.withDefault Elm.License.bsd3
        , name = Unsafe.packageName "elm/xyz"
        , summary = "Fake stuff"
        , deps = [ ( Unsafe.packageName "elm/core", Unsafe.constraint "1.0.0 <= v < 2.0.0" ) ]
        , testDeps = []
        , version = Elm.Version.fromString "1.0.0" |> Maybe.withDefault Elm.Version.one
        }


xyz2dependencyModules : List Elm.Docs.Module
xyz2dependencyModules =
    [ { name = "Element"
      , comment = ""
      , unions = []
      , aliases = []
      , values = []
      , binops = []
      }
    ]


unsafePackageName : String -> Elm.Package.Name
unsafePackageName packageName =
    case Elm.Package.fromString packageName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafePackageName packageName
