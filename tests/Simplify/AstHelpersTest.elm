module Simplify.AstHelpersTest exposing (all)

import Elm.Parser
import Elm.Syntax.Declaration
import Elm.Syntax.Node as Node exposing (Node(..))
import Expect exposing (Expectation)
import Simplify.AstHelpers as AstHelpers
import Test exposing (Test, describe, test)


all : Test
all =
    describe "AstHelpers.couldBeValueContainingNaN"
        [ test "String literals don't contain NaN" <|
            \() ->
                "\"a\""
                    |> couldBeValueContainingNaN False
        , test "Char literals don't contain NaN" <|
            \() ->
                "'a'"
                    |> couldBeValueContainingNaN False
        , test "Integer literals don't contain NaN" <|
            \() ->
                "0"
                    |> couldBeValueContainingNaN False
        , test "Float literals don't contain NaN" <|
            \() ->
                "0.0"
                    |> couldBeValueContainingNaN False
        , test "Number addition don't contain literals if all sub-expression can't contain NaN" <|
            \() ->
                "0.0 + 0"
                    |> couldBeValueContainingNaN False
        , test "Float division can contain NaN" <|
            \() ->
                "0 / 0"
                    |> couldBeValueContainingNaN True
        , test "Float division can't contain NaN" <|
            \() ->
                "0 // 0"
                    |> couldBeValueContainingNaN False
        , test "Applied || can't contain NaN" <|
            \() ->
                "a || b"
                    |> couldBeValueContainingNaN False
        , test "Applied && can't contain NaN" <|
            \() ->
                "a && b"
                    |> couldBeValueContainingNaN False
        , test "Applied ++ with string literals can't contain NaN" <|
            \() ->
                "\"a\" ++ \"b\""
                    |> couldBeValueContainingNaN False
        , test "Applied ++ with unknown values can contain NaN" <|
            \() ->
                "a ++ b"
                    |> couldBeValueContainingNaN True
        , test "Applied <?> can't contain NaN" <|
            \() ->
                "a <?> b"
                    |> couldBeValueContainingNaN False
        , test "References can contain NaN" <|
            \() ->
                "reference"
                    |> couldBeValueContainingNaN True
        , test "Lists with only non-NaN can't contain NaN" <|
            \() ->
                "[ 0, 1, 2 ]"
                    |> couldBeValueContainingNaN False
        , test "Lists with only potential NaN can contain NaN" <|
            \() ->
                "[ 0, reference, 2 ]"
                    |> couldBeValueContainingNaN True
        , test "Function calls can return NaN" <|
            \() ->
                "fn 0"
                    |> couldBeValueContainingNaN True
        , test "Function calls with <| can return NaN" <|
            \() ->
                "fn <| 0"
                    |> couldBeValueContainingNaN True
        , test "Function calls with |> can return NaN" <|
            \() ->
                "0 |> fn"
                    |> couldBeValueContainingNaN True
        , test "Record access functions can't return NaN" <|
            \() ->
                ".field"
                    |> couldBeValueContainingNaN False
        , test "Record access can return NaN" <|
            \() ->
                "record.field"
                    |> couldBeValueContainingNaN True
        , test "let expression checks for the `in` value (literal)" <|
            \() ->
                "let _ = 1 in 0"
                    |> couldBeValueContainingNaN False
        , test "let expression checks for the `in` value (reference)" <|
            \() ->
                "let _ = 1 in reference"
                    |> couldBeValueContainingNaN True
        ]


couldBeValueContainingNaN : Bool -> String -> Expectation
couldBeValueContainingNaN expected source =
    case Elm.Parser.parseToFile (wrapSource source) of
        Ok ast ->
            case List.head ast.declarations of
                Just (Node _ (Elm.Syntax.Declaration.FunctionDeclaration { declaration })) ->
                    (Node.value declaration).expression
                        |> AstHelpers.couldBeValueContainingNaN
                        |> Expect.equal expected

                _ ->
                    Debug.todo "Test setup failed"

        Err errors ->
            Debug.todo ("Could not parse source code: " ++ Debug.toString errors)


wrapSource : String -> String
wrapSource source =
    """module A exposing (a)
a = """ ++ source
