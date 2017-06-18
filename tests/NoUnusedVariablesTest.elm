module NoUnusedVariablesTest exposing (all)

import Lint.Rules.NoUnusedVariables exposing (rule)
import Test exposing (describe, test, Test)
import TestUtil exposing (ruleTester, expectErrors)
import Lint.Types exposing (LintRule, LintError, LintResult)
import TestUtil exposing (ruleTester, expectErrors)


testRule : String -> LintResult
testRule =
    ruleTester rule


error : String -> LintError
error =
    LintError "NoUnusedVariables"


tests : List Test
tests =
    [ test "should not report used top-level variables" <|
        \() ->
            testRule """module A exposing (b)
              a n = 1
              b = a 1
              """
                |> expectErrors []
    , test "should report unused top-level variables" <|
        \() ->
            testRule "a = 1"
                |> expectErrors [ error "Variable `a` is not used" ]
    , test "should report unused top-level variables even if they are annotated" <|
        \() ->
            testRule """
              a: Int
              a = 1
              """
                |> expectErrors [ error "Variable `a` is not used" ]
    , test "should not report unused top-level variables if everything is exposed" <|
        \() ->
            testRule """module A exposing (..)
              a n = 1
              b = a 1
              """
                |> expectErrors []
    , test "should not report unused top-level variables that are exposed by name" <|
        \() ->
            testRule """module A exposing (a, b)
              a = 1
              b = 2
              """
                |> expectErrors []
    , test "should not report unused top-level variables that are exposed by name, but report others" <|
        \() ->
            testRule """module A exposing (a, b)
              a = 1
              b = 2
              c = 3
              """
                |> expectErrors [ error "Variable `c` is not used" ]
    , test "should not report unused top-level variables if everything is exposed (port module)" <|
        \() ->
            testRule """port module A exposing (..)
              a n = 1
              b = a 1
              """
                |> expectErrors []
    , test "should not report unused top-level variables that are exposed by name (port module)" <|
        \() ->
            testRule """port module A exposing (a, b)
              a = 1
              b = 2
              """
                |> expectErrors []
    , test "should not report unused top-level variables that are exposed by name, but report others (port module)" <|
        \() ->
            testRule """port module A exposing (a, b)
              a = 1
              b = 2
              c = 3
              """
                |> expectErrors [ error "Variable `c` is not used" ]
    , test "should report unused variables from let declarations" <|
        \() ->
            testRule """module A exposing (a)
              a = let b = 1
                  in 2
              """
                |> expectErrors [ error "Variable `b` is not used" ]
    , test "should report unused variables from let even if they are exposed by name" <|
        \() ->
            testRule """module A exposing (a, b)
              a = let b = 1
                  in 2
              """
                |> expectErrors [ error "Variable `b` is not used" ]
    , test "should report unused functions from let even if they are exposed by name" <|
        \() ->
            testRule """module A exposing (a)
              a = let b param = 1
                  in 2
              """
                |> expectErrors [ error "Variable `b` is not used" ]
    , test "should report unused variables from let even if everything is exposed" <|
        \() ->
            testRule """module A exposing (..)
              a = let b = 1
                  in 2
              """
                |> expectErrors [ error "Variable `b` is not used" ]
    , test "should not report top-level variables used inside a let body" <|
        \() ->
            testRule """module A exposing (a)
              b = 1
              a = let c = 1
                  in b + c
              """
                |> expectErrors []
    , test "should not report top-level variables used inside let declarations" <|
        \() ->
            testRule """module A exposing (a)
              b = 1
              a = let c = b
                  in c
              """
                |> expectErrors []
    , test "should not report top-level variables used in nested lets" <|
        \() ->
            testRule """module A exposing (a)
              b = 1
              a = let
                    c = b
                    d = let
                          e = 1
                        in
                          b + c + e
                  in
                    d
              """
                |> expectErrors []
    , test "should not report variables from let declarations that are used in the body" <|
        \() ->
            testRule """module A exposing (a)
                a = let c = 1
                    in c
                """
                |> expectErrors []
    , test "should not report unused function parameters" <|
        \() ->
            testRule """module A exposing (a)
                a n = 1
                """
                |> expectErrors []
    , test "should report unused imported functions" <|
        \() ->
            testRule "import Foo exposing (a)"
                |> expectErrors [ error "Variable `a` is not used" ]
    , test "should report unused imported functions (multiple imports)" <|
        \() ->
            testRule "import Foo exposing (a, b, C)"
                |> expectErrors
                    [ error "Variable `a` is not used"
                    , error "Variable `b` is not used"
                    ]
      -- Needs to be improved, every case should create a new scope stack
      -- Right now, every parameter is considered used, which is not great
    , test "should not report unused pattern matching parameters" <|
        \() ->
            testRule """module A exposing (a)
                a = case thing of
                  Foo b c -> []
                """
                |> expectErrors []
      -- What to do with types needs to be determined when I understand Type exports better (wrt sub-types)
    , test "should report unused type declarations" <|
        \() ->
            testRule """
                type A = B | C -- Should B and C be reported if they are not used? Probably.
                type alias D = { a : B }
                """
                |> expectErrors []
      -- , test "should not report unused type declarations if everything is exposed" <|
      --     \() ->
      --         testRule """module A exposing (a, b)
      --            """
      --             |> expectErrors [ error "Variable `a` is not used" ]
      -- , test "should not report unused type declarations that are exposed by name" <|
      --     \() ->
      --         testRule """module A exposing (a, b)
      --           a = 1
      --           b = 2
      --           c = 3
      --           """
      --             |> expectErrors [ error "Variable `a` is not used" ]
      -- , test "should report unused named imports `import A exposing (a)`" <|
      --     \() ->
      --         testRule """module A exposing (a, b)
      --           a = 1
      --           b = 2
      --           c = 3
      --           """
      --             |> expectErrors [ error "Variable `a` is not used" ]
      -- , test "should not report used named imports `import A exposing (a)`" <|
      --     \() ->
      --         testRule """module A exposing (a, b)
      --           a = 1
      --           b = 2
      --           c = 3
      --           """
      --             |> expectErrors [ error "Variable `a` is not used" ]
      -- , test "should not report unused union imports `import A exposing (B(..))`" <|
      --     \() ->
      --         testRule """module A exposing (a, b)
      --           a = 1
      --           b = 2
      --           c = 3
      --           """
      --             |> expectErrors [ error "Variable `a` is not used" ]
      -- , test "should report unused union imports `import A exposing (B(B))`" <|
      --     \() ->
      --         testRule """module A exposing (a, b)
      --           a = 1
      --           b = 2
      --           c = 3
      --           """
      --             |> expectErrors [ error "Variable `a` is not used" ]
      -- , test "should not report unused union imports `import A exposing (B(B))` (with more nesting?)" <|
      --     \() ->
      --         testRule """module A exposing (a, b)
      --           a = 1
      --           b = 2
      --           c = 3
      --           """
      --             |> expectErrors [ error "Variable `a` is not used" ]
    ]


all : Test
all =
    describe "NoUnusedVariables" tests
