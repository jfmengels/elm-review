module Lint.Rules.NoDuplicateImports exposing (rule)

{-|
@docs rule

# Fail

    import Set
    import Set exposing (Set)

# Success

    import Set exposing (Set)
-}

import Ast.Statement exposing (..)
import Lint exposing (lint, doNothing)
import Lint.Types exposing (LintRule, LintRuleImplementation, LintError, Direction(..))
import Set exposing (Set)


type alias Context =
    { imports : Set String
    , duplicates : Set String
    }


{-| Forbid importing the same module several times in a file.

    rules =
        [ NoDuplicateImports.rule
        ]
-}
rule : LintRule
rule input =
    lint input implementation


implementation : LintRuleImplementation Context
implementation =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = doNothing
    , moduleEndFn = moduleEndFn
    , initialContext = Context Set.empty Set.empty
    }


error : String -> LintError
error name =
    LintError "NoDuplicateImports" (name ++ " was imported several times")


statementFn : Context -> Direction Statement -> ( List LintError, Context )
statementFn ctx node =
    case node of
        Enter (ImportStatement names alias exportSet) ->
            let
                name =
                    String.join "." names
            in
                if Set.member name ctx.imports then
                    ( [], { ctx | duplicates = Set.insert name ctx.duplicates } )
                else
                    ( [], { ctx | imports = Set.insert name ctx.imports } )

        _ ->
            ( [], ctx )


moduleEndFn : Context -> ( List LintError, Context )
moduleEndFn ctx =
    let
        errors =
            Set.toList ctx.duplicates
                |> List.map error
    in
        ( errors, ctx )
