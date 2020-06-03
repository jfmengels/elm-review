module NoUnused.Patterns.NameVisitor exposing (withValueVisitor)

{-| Visit each name in the module.

A "name" is a `Node ( ModuleName, String )` and represents a value or type reference. Here are some value examples:

  - `Html.Attributes.class` -> `( [ "Html", "Attributes" ], "class" )`
  - `view` -> `( [], "view" )`

These can appear in many places throughout declarations and expressions, and picking them out each time is a lot of work. Instead of writing 1000 lines of code and tests each time, you can write one `nameVisitor` and plug it straight into your module schema, or separate `valueVisitor` and `typeVisitor`s.

@docs withValueVisitor


## Scope

This makes no attempt to resolve module names from imports, it just returns what's written in the code. It would be trivial to connect [elm-review-scope] with the name visitor if you want to do this.

[elm-review-scope]: http://github.com/jfmengels/elm-review-scope/

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Review.Rule as Rule exposing (Error)


type alias VisitorFunction context =
    Node ( ModuleName, String ) -> context -> ( List (Error {}), context )


type Name
    = Value (Node ( ModuleName, String ))


withValueVisitor :
    (Node ( ModuleName, String ) -> context -> ( List (Error {}), context ))
    -> Rule.ModuleRuleSchema { schemaState | canCollectProjectData : () } context
    -> Rule.ModuleRuleSchema { schemaState | canCollectProjectData : (), hasAtLeastOneVisitor : () } context
withValueVisitor valueVisitor rule =
    rule
        |> Rule.withDeclarationListVisitor (declarationListVisitor valueVisitor)
        |> Rule.withExpressionVisitor (expressionVisitor valueVisitor)



--- VISITORS


declarationListVisitor :
    VisitorFunction context
    -> (List (Node Declaration) -> context -> ( List (Error {}), context ))
declarationListVisitor visitor list context =
    visitDeclarationList list
        |> folder visitor context


expressionVisitor :
    VisitorFunction context
    -> (Node Expression -> Rule.Direction -> context -> ( List (Error {}), context ))
expressionVisitor visitor node direction context =
    case direction of
        Rule.OnEnter ->
            visitExpression node
                |> folder visitor context

        Rule.OnExit ->
            ( [], context )



--- FOLDER


folder :
    VisitorFunction context
    -> (context -> List Name -> ( List (Error {}), context ))
folder visitor context list =
    case list of
        [] ->
            ( [], context )

        head :: rest ->
            let
                ( headErrors, headContext ) =
                    applyVisitor visitor head context

                ( restErrors, restContext ) =
                    folder visitor headContext rest
            in
            ( headErrors ++ restErrors, restContext )


applyVisitor : VisitorFunction context -> Name -> context -> ( List (Error {}), context )
applyVisitor visitor (Value node) context =
    visitor node context



--- PRIVATE


visitDeclarationList : List (Node Declaration) -> List Name
visitDeclarationList nodes =
    List.concatMap visitDeclaration nodes


visitDeclaration : Node Declaration -> List Name
visitDeclaration node =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            visitFunctionImplementation declaration

        _ ->
            []


visitFunctionImplementation : Node Expression.FunctionImplementation -> List Name
visitFunctionImplementation node =
    visitPatternList (node |> Node.value |> .arguments)


visitExpression : Node Expression -> List Name
visitExpression (Node range expression) =
    case expression of
        Expression.FunctionOrValue moduleName function ->
            visitValue (Node range ( moduleName, function ))

        Expression.LetExpression { declarations } ->
            visitLetDeclarationList declarations

        Expression.CaseExpression { cases } ->
            visitCaseList cases

        Expression.LambdaExpression { args } ->
            visitPatternList args

        Expression.RecordUpdateExpression name _ ->
            visitValue (Node.map (\function -> ( [], function )) name)

        _ ->
            []


visitLetDeclarationList : List (Node Expression.LetDeclaration) -> List Name
visitLetDeclarationList list =
    List.concatMap visitLetDeclaration list


visitLetDeclaration : Node Expression.LetDeclaration -> List Name
visitLetDeclaration node =
    case Node.value node of
        Expression.LetFunction { declaration } ->
            visitFunctionImplementation declaration

        Expression.LetDestructuring pattern _ ->
            visitPattern pattern


visitCaseList : List Expression.Case -> List Name
visitCaseList list =
    List.concatMap visitCase list


visitCase : Expression.Case -> List Name
visitCase ( pattern, _ ) =
    visitPattern pattern


visitPatternList : List (Node Pattern) -> List Name
visitPatternList list =
    List.concatMap visitPattern list


visitPattern : Node Pattern -> List Name
visitPattern node =
    case Node.value node of
        Pattern.TuplePattern patterns ->
            visitPatternList patterns

        Pattern.UnConsPattern head rest ->
            visitPattern head ++ visitPattern rest

        Pattern.ListPattern list ->
            visitPatternList list

        Pattern.NamedPattern { moduleName, name } _ ->
            let
                { start } =
                    Node.range node

                newEnd =
                    { start | column = start.column + (name :: moduleName |> String.join "." |> String.length) }

                range =
                    { start = start, end = newEnd }
            in
            visitValue (Node range ( moduleName, name ))

        Pattern.AsPattern pattern _ ->
            visitPattern pattern

        Pattern.ParenthesizedPattern pattern ->
            visitPattern pattern

        _ ->
            []


visitValue : Node ( ModuleName, String ) -> List Name
visitValue node =
    [ Value node ]
