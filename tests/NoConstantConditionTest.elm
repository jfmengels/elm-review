port module NoConstantConditionTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import Lint.Rules.NoConstantCondition exposing (rule)
import Lint.Types exposing (LintRule, Error)


error : Error
error =
    Error "NoConstantCondition" "Useless condition: It will always evaluate to the same value"


condition : String -> String
condition expr =
    "a = if " ++ expr ++ " then 1 else 0"


comparisonOperators : List String
comparisonOperators =
    [ "==", "/=", "<", "<=", ">", ">=" ]


createComparisonSource : String -> String
createComparisonSource op =
    String.join "\n"
        [ condition ("b " ++ op ++ " c")
        , condition ("b " ++ op ++ " 0")
        , condition ("0 " ++ op ++ " b")
        ]


validComparisonTests : List Test
validComparisonTests =
    List.map
        (\op ->
            test ("should not report a condition that compares a variable (" ++ op ++ ")") <|
                \() ->
                    rule (createComparisonSource op)
                        |> Expect.equal []
        )
        comparisonOperators


tests : List Test
tests =
    [ test "should not report a condition that accesses a variable" <|
        \() ->
            condition "b"
                |> rule
                |> Expect.equal []
    , test "should not report a condition that calls a function" <|
        \() ->
            condition "b c"
                |> rule
                |> Expect.equal []
    , test "should not report a condition that compares different lists" <|
        \() ->
            condition "[a] == [b]"
                |> rule
                |> Expect.equal []
    , test "should not report a condition that compares different records" <|
        \() ->
            condition "{a = 1} == {a = 2}"
                |> rule
                |> Expect.equal []
    , test "should report condition that only has True" <|
        \() ->
            condition "True"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that only has False" <|
        \() ->
            condition "False"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares True to False (==)" <|
        \() ->
            condition "True == False"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares True to False (/=)" <|
        \() ->
            condition "True /= False"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares False to True (==)" <|
        \() ->
            condition "False == True"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares False to True (/=)" <|
        \() ->
            condition "False /= True"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two integers (==)" <|
        \() ->
            condition "1 == 1"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two integers (/=)" <|
        \() ->
            condition "1 /= 1"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two floats (==)" <|
        \() ->
            condition "1.1 == 1.1"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two floats (/=)" <|
        \() ->
            condition "1.1 /= 1.1"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two strings (==)" <|
        \() ->
            condition "\"foo\" == \"foo\""
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two strings (/=)" <|
        \() ->
            condition "\"foo\" /= \"foo\""
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two similar Lists (==)" <|
        \() ->
            condition "[1, 2] == [1, 2]"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two similar Lists (/=)" <|
        \() ->
            condition "[1, 2] /= [1, 2]"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two similar Lists, even with variables" <|
        \() ->
            condition "[1, a] == [1, a]"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two similar records" <|
        \() ->
            condition "{ a = 2 } == { a = 2 }"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two identical elements (==)" <|
        \() ->
            condition "b == b"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two identical elements (/=)" <|
        \() ->
            condition "b /= b"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two identical complex elements (==)" <|
        \() ->
            condition "((foo bar) ++ List.map a <| List.reduce b c d) == ((foo bar) ++ List.map a <| List.reduce b c d)"
                |> rule
                |> Expect.equal [ error ]
    , test "should report condition that compares two identical complex elements (/=)" <|
        \() ->
            condition "((foo bar) ++ List.map a <| List.reduce b c d) /= ((foo bar) ++ List.map a <| List.reduce b c d)"
                |> rule
                |> Expect.equal [ error ]
    ]
        ++ validComparisonTests


all : Test
all =
    describe "NoConstantCondition" tests
