module NoTestValuesInProductionCode exposing
    ( rule
    , Configuration, startsWith, endsWith
    )

{-|

@docs rule
@docs Configuration, startsWith, endsWith

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when functions or values meant to be used only in tests are used in production source code.

    config =
        [ NoTestValuesInProductionCodeTest.rule
            (NoTestValuesInProductionCodeTest.startsWith "test_")

        -- or
        , NoTestValuesInProductionCodeTest.rule
            (NoTestValuesInProductionCodeTest.endsWith "_TESTS_ONLY")
        ]

This rule is meant to allow you to expose values from your module that you need for writing tests, while preserving the
making sure they are not misused in production code. You can read about the [problem and solution more in detail](https://jfmengels.net//test-only-values/).


## Fail

    -- NoTestValuesInProductionCodeTest.startsWith "test_"
    grantAdminRights user =
        { user | role = Role.test_admin }

    -- NoTestValuesInProductionCodeTest.endsWith "_TESTS_ONLY"
    grantAdminRights user =
        { user | role = Role.admin_TESTS_ONLY }


## Success

    -- module RoleTest exposing (roleTest)
    roleTest =
        Test.describe "Role"
            [ Test.test "admins should be able to delete database " <|
                \() -> Expect.true (Role.canDeleteDatabase Role.test_admin)
            , Test.test "users should not be able to delete database " <|
                \() -> Expect.false (Role.canDeleteDatabase Role.user)
            ]

Values marked as test-only can be used in the declaration of other test values.

    -- module User exposing (test_admin_user)
    test_admin_user =
        { id = "001"
        , role = Role.test_admin
        }


## When (not) to enable this rule

This rule is useful only if you have instances where you wish to add guarantees to the usage of your data types, but
need to access internals in the context of your tests.
Also, for this rule to work well, the naming convention for test-only values needs to be communicated to the rest of the
team or project.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-test-values/example --rules NoTestValuesInProductionCodeTest
```

The example uses the following configuration:

    config =
        [ NoTestValuesInProductionCodeTest.rule
            (NoTestValuesInProductionCodeTest.startsWith "test_")
        ]

-}
rule : Configuration -> Rule
rule configuration =
    let
        isTestValue : String -> Bool
        isTestValue =
            buildTestValuePredicate configuration
    in
    Rule.newModuleRuleSchemaUsingContextCreator "NoTestValuesInProductionCode" initialContext
        |> Rule.withDeclarationEnterVisitor (declarationVisitor isTestValue)
        |> Rule.withExpressionEnterVisitor (expressionVisitor configuration isTestValue)
        |> Rule.fromModuleRuleSchema


{-| Configure how values should be tagged.
-}
type Configuration
    = StartsWith String
    | EndsWith String


{-| A test-only value's name starts with the given string.
-}
startsWith : String -> Configuration
startsWith =
    StartsWith


{-| A test-only value's name ends with the given string.
-}
endsWith : String -> Configuration
endsWith =
    EndsWith


type alias Context =
    { inDeclarationOfNonTestValue : Bool
    , isInSourceDirectories : Bool
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\isInSourceDirectories () ->
            { inDeclarationOfNonTestValue = False
            , isInSourceDirectories = isInSourceDirectories
            }
        )
        |> Rule.withIsInSourceDirectories



-- CONFIGURATION


buildTestValuePredicate : Configuration -> String -> Bool
buildTestValuePredicate configuration =
    case configuration of
        StartsWith string ->
            String.startsWith string

        EndsWith string ->
            String.endsWith string



-- VISITORS


declarationVisitor : (String -> Bool) -> Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor isTestValue node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                functionName : String
                functionName =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            ( [], { context | inDeclarationOfNonTestValue = not (isTestValue functionName) } )

        _ ->
            ( [], { context | inDeclarationOfNonTestValue = False } )


expressionVisitor : Configuration -> (String -> Bool) -> Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor configuration isTestValue node context =
    if context.inDeclarationOfNonTestValue && context.isInSourceDirectories then
        case Node.value node of
            Expression.FunctionOrValue _ name ->
                if isTestValue name then
                    ( [ error configuration name (Node.range node) ]
                    , context
                    )

                else
                    ( [], context )

            _ ->
                ( [], context )

    else
        ( [], context )


error : Configuration -> String -> Range -> Error {}
error configuration name range =
    let
        ( configWord, matchText ) =
            case configuration of
                StartsWith str ->
                    ( "start", str )

                EndsWith str ->
                    ( "end", str )
    in
    Rule.error
        { message = "Forbidden use of test-only value `" ++ name ++ "` in production source code"
        , details =
            [ "This value was marked as being meant to only be used in test-related code, but I found it being used in code that will go to production."
            , "You should either stop using it or rename it to not " ++ configWord ++ " with `" ++ matchText ++ "`."
            ]
        }
        range
