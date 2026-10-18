module ModuleDocsTest exposing (availableModulesTest, contentsTest)

import Dict
import Elm.Docs
import Elm.License
import Elm.Package
import Elm.Project
import Elm.Type
import Elm.Version
import Json.Encode as Encode
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Rule)
import Review.Test
import Review.Test.Dependencies
import Review.Test.Dependencies.Unsafe as Unsafe
import Test exposing (Test, describe, test)


contentsTest : Test
contentsTest =
    test "should return the module that defined the value" <|
        \() ->
            [ """module A exposing (..)
{-| Module docs -}
import Parser.Advanced exposing (Parser, map2)
{-| Docs Type1 -}
type Type1
    = A
    | B ()
    | Wrapper Type2 String (List Int)

{- No docs -}
type Type2 a b
    = Type2 b Int a

{-| Docs TypeAlias1 -}
type alias TypeAlias1 =
    { field : String
    }

{- No docs -}
type alias TypeAlias2 a =
    { a | field : Type2 () ()
    }

type alias TypeAlias3 = String

{-| Docs value -}
value : Int
value = 1

{- No docs -}
valueInferred = "string"

function : String -> String
function arg =
    arg ++ "string"

functionInferred arg =
    arg ++ "string"

infix left 6 (|.) = ignorer

{-| Just like the [`(|.)`](Parser#|.) from the `Parser` module.-}
ignorer : Parser c x keep -> Parser c x ignore -> Parser c x keep
ignorer keepParser ignoreParser =
  map2 always keepParser ignoreParser
"""
            , """module B exposing (..)
import A
a = 1
"""
            ]
                |> Review.Test.runOnModulesWithProjectData project ruleThatListsContents
                |> Review.Test.expectErrorsForModules
                    [ ( "B"
                      , [ Review.Test.error
                            { message = "B imports A"
                            , details =
                                [ String.trim """
{
  "name": "A",
  "comment": " Module docs ",
  "unions": [
    {
      "name": "Type1",
      "comment": " Docs Type1 ",
      "args": [],
      "tag": [
        {
          "tag": "A",
          "args": []
        },
        {
          "tag": "B",
          "args": [
            "Tuple []"
          ]
        },
        {
          "tag": "Wrapper",
          "args": [
            "Type "A.Type2" []",
            "Type "String.String" []",
            "Type "List.List" [Type "Basics.Int" []]"
          ]
        }
      ]
    },
    {
      "name": "Type2",
      "comment": "",
      "args": [
        "a",
        "b"
      ],
      "tag": [
        {
          "tag": "Type2",
          "args": [
            "Var "b"",
            "Type "Basics.Int" []",
            "Var "a""
          ]
        }
      ]
    }
  ],
  "aliases": [
    {
      "name": "TypeAlias1",
      "comment": " Docs TypeAlias1 ",
      "args": [],
      "type": "Record [("field",Type "String.String" [])] Nothing"
    },
    {
      "name": "TypeAlias2",
      "comment": "",
      "args": [
        "a"
      ],
      "type": "Record [("field",Type "A.Type2" [Tuple [],Tuple []])] (Just "a")"
    },
    {
      "name": "TypeAlias3",
      "comment": "",
      "args": [],
      "type": "Type "String.String" []"
    }
  ],
  "values": [
    {
      "name": "function",
      "comment": "",
      "type": "Lambda (Type "String.String" []) (Type "String.String" [])"
    },
    {
      "name": "functionInferred",
      "comment": "",
      "type": "Tuple []"
    },
    {
      "name": "ignorer",
      "comment": " Just like the [`(|.)`](Parser#|.) from the `Parser` module.",
      "type": "Lambda (Type "Parser.Advanced.Parser" [Var "c",Var "x",Var "keep"]) (Lambda (Type "Parser.Advanced.Parser" [Var "c",Var "x",Var "ignore"]) (Type "Parser.Advanced.Parser" [Var "c",Var "x",Var "keep"]))"
    },
    {
      "name": "value",
      "comment": " Docs value ",
      "type": "Type "Basics.Int" []"
    },
    {
      "name": "valueInferred",
      "comment": "",
      "type": "Tuple []"
    },
    {
      "name": "|.",
      "comment": " Just like the [`(|.)`](Parser#|.) from the `Parser` module.",
      "type": "Lambda (Type "Parser.Advanced.Parser" [Var "c",Var "x",Var "keep"]) (Lambda (Type "Parser.Advanced.Parser" [Var "c",Var "x",Var "ignore"]) (Type "Parser.Advanced.Parser" [Var "c",Var "x",Var "keep"]))"
    }
  ],
  "binops": []
}
"""
                                ]
                            , under = "module"
                            }
                        ]
                      )
                    ]


availableModulesTest : Test
availableModulesTest =
    describe "Available module docs"
        [ test "should only include project dependencies if dependencies is False" <|
            \() ->
                [ """module A exposing (..)
import B
import Html
import Unknown
a = 1
"""
                , """module B exposing (..)
a = 1"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project
                        (ruleThatListsModuleNames { implicitImports = False, dependencies = False })
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Docs for A"
                                , details = [ "B" ]
                                , under = "module"
                                }
                            ]
                          )
                        , ( "B"
                          , [ Review.Test.error
                                { message = "Docs for B"
                                , details = [ "" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        , test "should include project dependencies if dependencies is True" <|
            \() ->
                [ """module A exposing (..)
import B
import Html
import Unknown
a = 1
"""
                , """module B exposing (..)
a = 1"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project
                        (ruleThatListsModuleNames { implicitImports = False, dependencies = True })
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Docs for A"
                                , details = [ "B, Html" ]
                                , under = "module"
                                }
                            ]
                          )
                        , ( "B"
                          , [ Review.Test.error
                                { message = "Docs for B"
                                , details = [ "" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        , test "should include implicit imports if implicitImports and dependencies are True" <|
            \() ->
                [ """module A exposing (..)
import B
import Html
import Unknown
a = 1
"""
                , """module B exposing (..)
a = 1"""
                ]
                    |> Review.Test.runOnModulesWithProjectData project
                        (ruleThatListsModuleNames { implicitImports = True, dependencies = True })
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Docs for A"
                                , details = [ "B, Basics, Char, Debug, Html, List, Maybe, Platform, Platform.Cmd, Platform.Sub, Result, String, Tuple" ]
                                , under = "module"
                                }
                            ]
                          )
                        , ( "B"
                          , [ Review.Test.error
                                { message = "Docs for B"
                                , details = [ "Basics, Char, Debug, List, Maybe, Platform, Platform.Cmd, Platform.Sub, Result, String, Tuple" ]
                                , under = "module"
                                }
                            ]
                          )
                        ]
        ]


type alias ModuleContext =
    List (Rule.Error {})


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


ruleThatListsContents : Rule
ruleThatListsContents =
    Rule.newModuleRuleSchemaUsingContextCreator "TestRule" contextCreatorThatListsContents
        |> Rule.withSimpleModuleDefinitionVisitor (\_ -> [])
        |> Rule.withFinalModuleEvaluation identity
        |> Rule.fromModuleRuleSchema


contextCreatorThatListsContents : Rule.ContextCreator () ModuleContext
contextCreatorThatListsContents =
    Rule.initContextCreator
        (\currentModuleName importedModuleDocs () ->
            importedModuleDocs
                |> Dict.toList
                |> List.map
                    (\( importedModuleName, doc ) ->
                        Rule.error
                            { message = String.join "." currentModuleName ++ " imports " ++ String.join "." importedModuleName
                            , details =
                                [ encodeElmDocs doc
                                    |> Encode.encode 2
                                    |> String.replace "\\\"" "\""
                                ]
                            }
                            { start = { row = 1, column = 1 }
                            , end = { row = 1, column = 7 }
                            }
                    )
        )
        |> Rule.withModuleName
        |> Rule.withImportedModuleDocs { implicitImports = False, dependencies = False }


encodeElmDocs : Elm.Docs.Module -> Encode.Value
encodeElmDocs mod =
    Encode.object
        [ ( "name", Encode.string mod.name )
        , ( "comment", Encode.string mod.comment )
        , ( "unions", Encode.list unionEncoder (List.sortBy .name mod.unions) )
        , ( "aliases", Encode.list aliasEncoder (List.sortBy .name mod.aliases) )
        , ( "values", Encode.list valueEncoder (List.sortBy .name mod.values) )
        , ( "binops", Encode.list binopEncoder (List.sortBy .name mod.binops) )
        ]


aliasEncoder : Elm.Docs.Alias -> Encode.Value
aliasEncoder alias =
    Encode.object
        [ ( "name", Encode.string alias.name )
        , ( "comment", Encode.string alias.comment )
        , ( "args", Encode.list Encode.string alias.args )
        , ( "type", encodeType alias.tipe )
        ]


unionEncoder : Elm.Docs.Union -> Encode.Value
unionEncoder union =
    Encode.object
        [ ( "name", Encode.string union.name )
        , ( "comment", Encode.string union.comment )
        , ( "args", Encode.list Encode.string union.args )
        , ( "tag", Encode.list tagEncoder union.tags )
        ]


tagEncoder : ( String, List Elm.Type.Type ) -> Encode.Value
tagEncoder ( name, types ) =
    Encode.object
        [ ( "tag", Encode.string name )
        , ( "args", Encode.list encodeType types )
        ]


valueEncoder : Elm.Docs.Value -> Encode.Value
valueEncoder value =
    Encode.object
        [ ( "name", Encode.string value.name )
        , ( "comment", Encode.string value.comment )
        , ( "type", encodeType value.tipe )
        ]


binopEncoder : Elm.Docs.Binop -> Encode.Value
binopEncoder value =
    Encode.object
        [ ( "name", Encode.string value.name )
        , ( "comment", Encode.string value.comment )
        , ( "type", encodeType value.tipe )
        , ( "associativity", assocEncoder value.associativity )
        , ( "precedence", Encode.int value.precedence )
        ]


assocEncoder : Elm.Docs.Associativity -> Encode.Value
assocEncoder assoc =
    case assoc of
        Elm.Docs.Left ->
            Encode.string "left"

        Elm.Docs.None ->
            Encode.string "non"

        Elm.Docs.Right ->
            Encode.string "right"


encodeType : Elm.Type.Type -> Encode.Value
encodeType tipe =
    Debug.toString tipe |> Encode.string


ruleThatListsModuleNames : { implicitImports : Bool, dependencies : Bool } -> Rule
ruleThatListsModuleNames params =
    Rule.newModuleRuleSchemaUsingContextCreator "TestRule" (contextCreatorThatListsModuleNames params)
        |> Rule.withSimpleModuleDefinitionVisitor (\_ -> [])
        |> Rule.withFinalModuleEvaluation identity
        |> Rule.fromModuleRuleSchema


contextCreatorThatListsModuleNames : { implicitImports : Bool, dependencies : Bool } -> Rule.ContextCreator () ModuleContext
contextCreatorThatListsModuleNames params =
    Rule.initContextCreator
        (\currentModuleName importedModuleDocs () ->
            [ Rule.error
                { message = "Docs for " ++ String.join "." currentModuleName
                , details =
                    [ Dict.keys importedModuleDocs
                        |> List.map (String.join ".")
                        |> String.join ", "
                    ]
                }
                { start = { row = 1, column = 1 }
                , end = { row = 1, column = 7 }
                }
            ]
        )
        |> Rule.withModuleName
        |> Rule.withImportedModuleDocs params
