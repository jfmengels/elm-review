module NoDuplicateImports exposing (rule)

import Set exposing (Set)
import Lint exposing (lint, doNothing)
import Types exposing (LintRule, Error, Direction(..))
import Ast.Statement exposing (..)


type alias Context =
    { imports : Set String
    , duplicates : Set String
    }


rule : String -> List Error
rule input =
    lint input implementation


implementation : LintRule Context
implementation =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = doNothing
    , moduleEndFn = moduleEndFn
    , initialContext = Context Set.empty Set.empty
    }


error : String -> Error
error name =
    Error "NoDuplicateImports" (name ++ " was imported several times")


statementFn : Context -> Direction Statement -> ( List Error, Context )
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


moduleEndFn : Context -> ( List Error, Context )
moduleEndFn ctx =
    let
        errors =
            Set.toList ctx.duplicates
                |> List.map error
    in
        ( errors, ctx )
