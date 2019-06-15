module Lint.Rule.NoImportingEverything exposing (rule, Configuration)

{-|

@docs rule, Configuration


# Fail

    import Html exposing (..)


# Success

    import Html exposing (div, p, textarea)

-}

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Lint exposing (Rule, lint)
import Lint.Error exposing (Error)
import Lint.Rule as Rule exposing (Implementation)
import Lint.Util as Util


type alias Context =
    ()


{-| Configuration for the rule.
-}
type alias Configuration =
    { exceptions : List String }


{-| Forbid importing everything from your module. This can especially be confusing to newcomers when the exposed
functions and types are unknown to them.

    rules =
        [ NoImportingEverything.rule { exceptions = [ "Html" ] }
        ]

-}
rule : Configuration -> Rule
rule config input =
    lint input (implementation config)


implementation : Configuration -> Implementation Context
implementation config =
    Rule.create ()
        |> Rule.withImportVisitor (visitImport config)


error : Range -> String -> Error
error range name =
    Error "NoImportingEverything" ("Do not expose everything from " ++ name) range


visitImport : Configuration -> Context -> Node Import -> ( List Error, Context )
visitImport config context node =
    let
        { moduleName, exposingList } =
            Node.value node

        name : String
        name =
            Util.moduleName moduleName
    in
    if List.member name config.exceptions then
        ( [], context )

    else
        case exposingList |> Maybe.map Node.value of
            Just (Exposing.All range) ->
                ( [ error range name ], context )

            _ ->
                ( [], context )
