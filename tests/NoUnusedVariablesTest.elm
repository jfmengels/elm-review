port module NoUnusedVariablesTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import Lint.Rules.NoUnusedVariables exposing (rule)
import Lint.Types exposing (Error)


error : String -> Error
error =
    Error "NoUnusedVariables"


tests : List Test
tests =
    [ test "should not report used top-level variables" <|
        \() ->
            rule """module A exposing (b)
              a n = 1
              b = a 1
              """
                |> Expect.equal []
    , test "should report unused top-level variables" <|
        \() ->
            rule "a = 1"
                |> Expect.equal [ error "Variable `a` is not used" ]
    , test "should report unused top-level variables even if they are annotated" <|
        \() ->
            rule """
              a: Int
              a = 1
              """
                |> Expect.equal [ error "Variable `a` is not used" ]
    , test "should not report unused top-level variables if everything is exposed" <|
        \() ->
            rule """module A exposing (..)
              a n = 1
              b = a 1
              """
                |> Expect.equal []
    , test "should not report unused top-level variables that are exposed by name" <|
        \() ->
            rule """module A exposing (a, b)
              a = 1
              b = 2
              """
                |> Expect.equal []
    , test "should not report unused top-level variables that are exposed by name, but report others" <|
        \() ->
            rule """module A exposing (a, b)
              a = 1
              b = 2
              c = 3
              """
                |> Expect.equal [ error "Variable `c` is not used" ]
    , test "should report unused variables from let declarations" <|
        \() ->
            rule """module A exposing (a)
              a = let b = 1
                  in 2
              """
                |> Expect.equal [ error "Variable `b` is not used" ]
    , test "should report unused variables from let even if they are exposed by name" <|
        \() ->
            rule """module A exposing (a, b)
              a = let b = 1
                  in 2
              """
                |> Expect.equal [ error "Variable `b` is not used" ]
    , test "should report unused variables from let even if everything is exposed" <|
        \() ->
            rule """module A exposing (..)
              a = let b = 1
                  in 2
              """
                |> Expect.equal [ error "Variable `b` is not used" ]
    , test "should not report top-level variables used inside a let body" <|
        \() ->
            rule """module A exposing (a)
              b = 1
              a = let c = 1
                  in b + c
              """
                |> Expect.equal []
    , test "should not report top-level variables used inside let declarations" <|
        \() ->
            rule """module A exposing (a)
              b = 1
              a = let c = b
                  in c
              """
                |> Expect.equal []
    , test "should not report top-level variables used in nested lets" <|
        \() ->
            rule """module A exposing (a)
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
                |> Expect.equal []
    , test "should not report variables from let declarations that are used in the body" <|
        \() ->
            rule """module A exposing (a)
                a = let c = 1
                    in c
                """
                |> Expect.equal []
    , test "should not report unused function parameters" <|
        \() ->
            rule """module A exposing (a)
                a n = 1
                """
                |> Expect.equal []
    , test "should report unused imported functions" <|
        \() ->
            rule "import Foo exposing (a)"
                |> Expect.equal [ error "Variable `a` is not used" ]
    , test "should report unused imported functions (multiple imports)" <|
        \() ->
            rule "import Foo exposing (a, b, C)"
                |> Expect.equal
                    [ error "Variable `a` is not used"
                    , error "Variable `b` is not used"
                    ]
      -- Needs to be improved, every case should create a new scope stack
      -- Right now, every parameter is considered used, which is not great
    , test "should not report unused pattern matching parameters" <|
        \() ->
            rule """module A exposing (a)
                a = case thing of
                  Foo b c -> []
                """
                |> Expect.equal []
      -- What to do with types needs to be determined when I understand Type exports better (wrt sub-types)
    , test "should report unused type declarations" <|
        \() ->
            rule """
                type A = B | C -- Should B and C be reported if they are not used? Probably.
                type alias D = { a : B }
                """
                |> Expect.equal []
      -- , test "should not report unused type declarations if everything is exposed" <|
      --     \() ->
      --         rule """module A exposing (a, b)
      --            """
      --             |> Expect.equal [ error "Variable `a` is not used" ]
      -- , test "should not report unused type declarations that are exposed by name" <|
      --     \() ->
      --         rule """module A exposing (a, b)
      --           a = 1
      --           b = 2
      --           c = 3
      --           """
      --             |> Expect.equal [ error "Variable `a` is not used" ]
      -- , test "should report unused named imports `import A exposing (a)`" <|
      --     \() ->
      --         rule """module A exposing (a, b)
      --           a = 1
      --           b = 2
      --           c = 3
      --           """
      --             |> Expect.equal [ error "Variable `a` is not used" ]
      -- , test "should not report used named imports `import A exposing (a)`" <|
      --     \() ->
      --         rule """module A exposing (a, b)
      --           a = 1
      --           b = 2
      --           c = 3
      --           """
      --             |> Expect.equal [ error "Variable `a` is not used" ]
      -- , test "should not report unused union imports `import A exposing (B(..))`" <|
      --     \() ->
      --         rule """module A exposing (a, b)
      --           a = 1
      --           b = 2
      --           c = 3
      --           """
      --             |> Expect.equal [ error "Variable `a` is not used" ]
      -- , test "should report unused union imports `import A exposing (B(B))`" <|
      --     \() ->
      --         rule """module A exposing (a, b)
      --           a = 1
      --           b = 2
      --           c = 3
      --           """
      --             |> Expect.equal [ error "Variable `a` is not used" ]
      -- , test "should not report unused union imports `import A exposing (B(B))` (with more nesting?)" <|
      --     \() ->
      --         rule """module A exposing (a, b)
      --           a = 1
      --           b = 2
      --           c = 3
      --           """
      --             |> Expect.equal [ error "Variable `a` is not used" ]
    ]


all : Test
all =
    describe "NoUnusedVariables" tests
