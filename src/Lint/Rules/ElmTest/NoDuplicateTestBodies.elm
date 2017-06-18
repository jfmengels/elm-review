module Lint.Rules.ElmTest.NoDuplicateTestBodies exposing (rule)

{-|
@docs rule

# Fail

    module Addition exposing (..)

    import Test exposing (test)

    tests =
        [ test "foo" <|
            \() -> 1 + 1
                |> Expect.equal 2
        , test "bar" <|
            \() -> 1 + 1
                |> Expect.equal 2
        ]

# Success

    module Addition exposing (..)

    import Test exposing (test)

    tests =
        [ test "foo" <|
            \() -> 1 + 1
                |> Expect.equal 2
        , test "bar" <|
            \() -> 1 + 2
                |> Expect.equal 3
        ]
-}

import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Lint exposing (lint, doNothing)
import Lint.Types exposing (LintRule, LintRuleImplementation, LintError, Direction(..))
import Dict exposing (Dict)


type alias Context =
    { availableTestAliases : List String
    }


{-| Forbid dupicate test bodies.
When copy-pasting tests, it can happen that the title is changed but the developer forgets to update the test body.
This may result in specifications that are thought to be implemented but are not enforced by tests.

    rules =
        [ ElmTest.NoDuplicateTestBodies.rule
        ]
-}
rule : LintRule
rule input =
    lint input implementation


implementation : LintRuleImplementation Context
implementation =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = expressionFn
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , initialContext = Context []
    }


error : ( String, String ) -> LintError
error ( title1, title2 ) =
    LintError
        "ElmTest.NoDuplicateTestBodies"
        ("Test `" ++ title1 ++ "` has the same body as test `" ++ title2 ++ "`")


isTestFunctionCall : List String -> Expression -> Bool
isTestFunctionCall availableTestAliases expr =
    case expr of
        Variable fnName ->
            List.member (String.join "." fnName) availableTestAliases

        Access (Variable object) fields ->
            List.member (String.join "." <| object ++ fields) availableTestAliases

        _ ->
            False


filterTests : List String -> List Expression -> List ( String, Expression )
filterTests availableTestAliases listItems =
    List.concatMap
        (\item ->
            case item of
                BinOp (Variable [ "<|" ]) (Application fn (String title)) testBody ->
                    if isTestFunctionCall availableTestAliases fn then
                        [ ( title, testBody ) ]
                    else
                        []

                _ ->
                    []
        )
        listItems


expressionFn : Context -> Direction Expression -> ( List LintError, Context )
expressionFn ctx node =
    case node of
        Enter (List listItems) ->
            let
                tests =
                    filterTests ctx.availableTestAliases listItems

                redundantTests =
                    List.foldl
                        (\( title, testBody ) { dict, redundant } ->
                            let
                                testBodyAsString =
                                    toString testBody

                                existingTest =
                                    Dict.get testBodyAsString dict
                            in
                                case existingTest of
                                    Nothing ->
                                        { dict = Dict.insert testBodyAsString title dict, redundant = redundant }

                                    Just existingTestTitle ->
                                        { dict = dict, redundant = redundant ++ [ ( title, existingTestTitle ) ] }
                        )
                        { dict = Dict.empty, redundant = [] }
                        tests
            in
                ( List.map error redundantTests.redundant, ctx )

        _ ->
            ( [], ctx )


extractImported : ExportSet -> List String
extractImported exportSet =
    case exportSet of
        AllExport ->
            [ "test" ]

        SubsetExport list ->
            List.concatMap extractImported list

        FunctionExport name ->
            if name == "test" then
                [ name ]
            else
                []

        _ ->
            []


computeAlias : Maybe String -> String
computeAlias =
    Maybe.withDefault "Test"


statementFn : Context -> Direction Statement -> ( List LintError, Context )
statementFn ctx node =
    case node of
        Enter (ImportStatement [ "Test" ] testAlias exportSet) ->
            let
                moduleFnAccess =
                    computeAlias testAlias ++ ".test"
            in
                case exportSet of
                    Nothing ->
                        ( [], { availableTestAliases = [ moduleFnAccess ] } )

                    Just subExportSet ->
                        ( [], { availableTestAliases = [ moduleFnAccess ] ++ extractImported subExportSet } )

        _ ->
            ( [], ctx )
