module Lint exposing (lint, doNothing)

import Ast
import Types exposing (Error, LintImplementation, LintRule, Direction, Visitor)
import Visitor exposing (transformStatementsIntoVisitors)


doNothing : LintImplementation a context
doNothing ctx _ =
    ( [], ctx )


visitAndAccumulate : LintRule context -> Visitor context -> ( List Error, context ) -> ( List Error, context )
visitAndAccumulate rule visitor ( errors, ctx ) =
    visitor rule ctx
        |> Tuple.mapFirst (\errors_ -> errors ++ errors_)


lintWithVisitors : List (Visitor context) -> LintRule context -> List Error
lintWithVisitors visitors rule =
    visitors
        |> List.foldl (visitAndAccumulate rule) ( [], rule.initialContext )
        |> Tuple.first


lint : String -> LintRule context -> List Error
lint source =
    source
        |> Ast.parse
        |> Result.map (\( _, _, statements ) -> statements)
        |> Result.withDefault []
        |> transformStatementsIntoVisitors
        |> lintWithVisitors
