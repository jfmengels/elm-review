module Lint exposing (lint, doNothing)

import Ast.Statement exposing (..)
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
        |> List.foldl (visitAndAccumulate rule) ( [], rule.context )
        |> Tuple.first


lint : List Statement -> LintRule context -> List Error
lint statements =
    transformStatementsIntoVisitors statements
        |> lintWithVisitors
