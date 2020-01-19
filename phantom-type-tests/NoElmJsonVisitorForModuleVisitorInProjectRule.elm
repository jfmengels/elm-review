module NoWithInitialContextAfterHavingAddedAVisitor exposing (rule)

{-| We want to forbid module visitors from using `withElmJsonVisitor`.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Module
import Elm.Project exposing (Project)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newSchema "NoWithInitialContextAfterHavingAddedAVisitor"
        |> Rule.withDeclarationListVisitor (\_ context -> ( [], context ))
        |> Rule.withInitialContext {}
        |> Rule.fromSchema
