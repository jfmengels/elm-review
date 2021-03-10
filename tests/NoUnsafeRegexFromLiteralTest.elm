module NoUnsafeRegexFromLiteralTest exposing (all)

import Elm.Project
import Expect exposing (Expectation)
import Json.Decode as Decode
import NoUnsafeRegexFromLiteral exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


helpersRegexSourceCode : String
helpersRegexSourceCode =
    """module SomeModule.Regex exposing (fromLiteralFunc)
import Regex exposing (Regex)
fromLiteralFunc : String -> Regex
fromLiteralFunc = something
"""


configuration : { unsafeFunction : String, moduleAlias : Maybe String }
configuration =
    { unsafeFunction = "SomeModule.Regex.fromLiteralFunc"
    , moduleAlias = Just "RegexUtil"
    }


expectErrors : List Review.Test.ExpectedError -> Review.Test.ReviewResult -> Expectation
expectErrors expectedErrors =
    Review.Test.expectErrorsForModules [ ( "A", expectedErrors ) ]


all : Test
all =
    describe "NoUnsafeRegexFromLiteral"
        [ test "should not report calls to target function with a valid literal regex" <|
            \_ ->
                [ """module A exposing (..)
import SomeModule.Regex
a = SomeModule.Regex.fromLiteralFunc "^abc$"
""", helpersRegexSourceCode ]
                    |> Review.Test.runOnModulesWithProjectData project (rule configuration)
                    |> Review.Test.expectNoErrors
        , test "should report calls to target function with an invalid literal regex" <|
            \_ ->
                [ """module A exposing (..)
import SomeModule.Regex
a = SomeModule.Regex.fromLiteralFunc "^ab($cd"
""", helpersRegexSourceCode ]
                    |> Review.Test.runOnModulesWithProjectData project (rule configuration)
                    |> expectErrors
                        [ Review.Test.error
                            { message = "SomeModule.Regex.fromLiteralFunc needs to be called with a valid regex."
                            , details =
                                [ "This function serves to give you more guarantees about creating regular expressions, but if the argument is dynamic or too complex, I won't be able to tell you."
                                , "Note that if you are using a triple-quote string, elm-syntax has a bug where it mis-parses the string and this could be a false report. Switch to using a single-quote string and everything should be fine."
                                ]
                            , under = """SomeModule.Regex.fromLiteralFunc "^ab($cd\""""
                            }
                        ]
        , test "should not report calls to target function with an valid literal regex containing back-slashes" <|
            \_ ->
                [ """module A exposing (..)
import SomeModule.Regex
a = SomeModule.Regex.fromLiteralFunc "\\\\s"
""", helpersRegexSourceCode ]
                    |> Review.Test.runOnModulesWithProjectData project (rule configuration)
                    |> Review.Test.expectNoErrors
        , test "should report calls to target function with a non-literal value" <|
            \_ ->
                [ """module A exposing (..)
import SomeModule.Regex
a = SomeModule.Regex.fromLiteralFunc dynamicValue
""", helpersRegexSourceCode ]
                    |> Review.Test.runOnModulesWithProjectData project (rule configuration)
                    |> expectErrors
                        [ Review.Test.error
                            { message = "SomeModule.Regex.fromLiteralFunc needs to be called with a static string literal."
                            , details =
                                [ "This function serves to give you more guarantees about creating regular expressions, but if the argument is dynamic or too complex, I won't be able to tell you."
                                , "Either make the argument static or use Regex.fromString instead."
                                ]
                            , under = "SomeModule.Regex.fromLiteralFunc dynamicValue"
                            }
                        ]
        , test "should report invalid calls if the function is called through a module alias" <|
            \_ ->
                [ """module A exposing (..)
import SomeModule.Regex as R
a = R.fromLiteralFunc dynamicValue
""", helpersRegexSourceCode ]
                    |> Review.Test.runOnModulesWithProjectData project (rule configuration)
                    |> expectErrors
                        [ Review.Test.error
                            { message = "SomeModule.Regex.fromLiteralFunc needs to be called with a static string literal."
                            , details =
                                [ "This function serves to give you more guarantees about creating regular expressions, but if the argument is dynamic or too complex, I won't be able to tell you."
                                , "Either make the argument static or use Regex.fromString instead."
                                ]
                            , under = "R.fromLiteralFunc dynamicValue"
                            }
                        ]
        , test "should report invalid calls if the function is called through a direct import" <|
            \_ ->
                [ """module A exposing (..)
import SomeModule.Regex exposing (fromLiteralFunc)
a = fromLiteralFunc dynamicValue
""", helpersRegexSourceCode ]
                    |> Review.Test.runOnModulesWithProjectData project (rule configuration)
                    |> expectErrors
                        [ Review.Test.error
                            { message = "SomeModule.Regex.fromLiteralFunc needs to be called with a static string literal."
                            , details =
                                [ "This function serves to give you more guarantees about creating regular expressions, but if the argument is dynamic or too complex, I won't be able to tell you."
                                , "Either make the argument static or use Regex.fromString instead."
                                ]
                            , under = "fromLiteralFunc dynamicValue"
                            }
                        ]
        , test "should report invalid calls if the function is called through an import that exposes all" <|
            \_ ->
                [ """module A exposing (..)
import SomeModule.Regex exposing (..)
a = fromLiteralFunc dynamicValue
""", helpersRegexSourceCode ]
                    |> Review.Test.runOnModulesWithProjectData project (rule configuration)
                    |> expectErrors
                        [ Review.Test.error
                            { message = "SomeModule.Regex.fromLiteralFunc needs to be called with a static string literal."
                            , details =
                                [ "This function serves to give you more guarantees about creating regular expressions, but if the argument is dynamic or too complex, I won't be able to tell you."
                                , "Either make the argument static or use Regex.fromString instead."
                                ]
                            , under = "fromLiteralFunc dynamicValue"
                            }
                        ]
        , test "should not report invalid calls if the function was not imported" <|
            \_ ->
                [ """module A exposing (..)
import SomeModule.Regex
a = fromLiteralFunc dynamicValue
""", helpersRegexSourceCode ]
                    |> Review.Test.runOnModulesWithProjectData project (rule configuration)
                    |> Review.Test.expectNoErrors
        , test "should report when function is used is used in a non 'function call' context" <|
            \_ ->
                [ """module A exposing (..)
import SomeModule.Regex
fromLiteralAlias = SomeModule.Regex.fromLiteralFunc
""", helpersRegexSourceCode ]
                    |> Review.Test.runOnModulesWithProjectData project (rule configuration)
                    |> expectErrors
                        [ Review.Test.error
                            { message = "SomeModule.Regex.fromLiteralFunc must be called directly."
                            , details =
                                [ "This function serves to give you more guarantees about creating regular expressions, but I can't determine how it is used if you do something else than calling it directly."
                                ]
                            , under = "SomeModule.Regex.fromLiteralFunc"
                            }
                        ]
        , test "should report a global error when the target function could not be found in the project" <|
            \_ ->
                """module A exposing (..)
import SomeModule.Regex
a = SomeModule.Regex.fromLiteralFunc "^abc$"
"""
                    |> Review.Test.runWithProjectData project (rule configuration)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find SomeModule.Regex.fromLiteralFunc"
                          , details =
                                [ "I want to provide guarantees on the use of this function, but I can't find it. It is likely that it was renamed, which prevents me from giving you these guarantees."
                                , "You should rename it back or update this rule to the new name. If you do not use the function anymore, remove the rule."
                                ]
                          }
                        ]
        , test "should not report native Regex.fromString with a non-literal string" <|
            \_ ->
                [ """module A exposing (..)
import Regex
a = Regex.fromString value
""", helpersRegexSourceCode ]
                    |> Review.Test.runOnModulesWithProjectData project (rule configuration)
                    |> Review.Test.expectNoErrors
        , test "should report native Regex.fromString with a literal string (with a preferred module alias)" <|
            \_ ->
                [ """module A exposing (..)
import Regex
a = Regex.fromString "(abc|"
""", helpersRegexSourceCode ]
                    |> Review.Test.runOnModulesWithProjectData project (rule configuration)
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Use `RegexUtil.fromLiteralFunc` instead"
                                , details =
                                    [ "By using `RegexUtil.fromLiteralFunc`, I can make sure that the regex you generated was valid."
                                    , "    import SomeModule.Regex as RegexUtil\n    myRegex = RegexUtil.fromLiteralFunc \"(abc|def)\""
                                    ]
                                , under = "Regex.fromString"
                                }
                            ]
                          )
                        ]
        , test "should report native Regex.fromString with a literal string (without a preferred module alias)" <|
            \_ ->
                [ """module A exposing (..)
import Regex
a = Regex.fromString "(abc|"
""", helpersRegexSourceCode ]
                    |> Review.Test.runOnModulesWithProjectData project (rule { configuration | moduleAlias = Nothing })
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Use `SomeModule.Regex.fromLiteralFunc` instead"
                                , details =
                                    [ "By using `SomeModule.Regex.fromLiteralFunc`, I can make sure that the regex you generated was valid."
                                    , "    import SomeModule.Regex\n    myRegex = SomeModule.Regex.fromLiteralFunc \"(abc|def)\""
                                    ]
                                , under = "Regex.fromString"
                                }
                            ]
                          )
                        ]
        , test "should report a configuration error when module name is invalid" <|
            \_ ->
                rule
                    { unsafeFunction = "invalid name"
                    , moduleAlias = Nothing
                    }
                    |> Review.Test.expectConfigurationError
                        { message = "invalid name is not a valid function name"
                        , details = [ "Some details" ]
                        }
        ]



-- PROJECT DATA


project : Project
project =
    Project.new
        |> Project.addElmJson (createElmJson applicationElmJson)


createElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok elmJson ->
            { path = "elm.json"
            , raw = rawElmJson
            , project = elmJson
            }

        Err _ ->
            Debug.todo "Invalid elm.json supplied to test"


applicationElmJson : String
applicationElmJson =
    """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.0"
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}"""
