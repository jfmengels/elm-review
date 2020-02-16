module NoHtmlButton exposing (rule)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Direction, Error, Rule)
import Scope


type alias Context =
    { scope : Scope.Context
    , allowed : Allowed
    }


type Allowed
    = HtmlButtonIsAllowed
    | HtmlButtonIsForbidden


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoHtmlButton" initialContext
        |> Scope.addVisitors
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


initialContext : Context
initialContext =
    { scope = Scope.initialContext
    , allowed = HtmlButtonIsForbidden
    }


moduleDefinitionVisitor : Node Module -> Context -> ( List Error, Context )
moduleDefinitionVisitor node context =
    if (Node.value node |> Module.moduleName) == [ "Button" ] then
        ( [], { context | allowed = HtmlButtonIsAllowed } )

    else
        ( [], { context | allowed = HtmlButtonIsForbidden } )


expressionVisitor : Node Expression -> Direction -> Context -> ( List Error, Context )
expressionVisitor node direction context =
    case ( direction, context.allowed ) of
        ( Rule.OnEnter, HtmlButtonIsAllowed ) ->
            ( [], context )

        ( Rule.OnEnter, HtmlButtonIsForbidden ) ->
            case Node.value node of
                FunctionOrValue moduleName name ->
                    if Scope.realFunctionOrType moduleName name context.scope == ( [ "Html" ], "button" ) then
                        ( [ Rule.error
                                { message = "Do not use `Html.button` directly"
                                , details = [ "At fruits.com, we've built a nice `Button` module that suits our needs better. Using this module instead of `Html.button` ensures we have a consistent button experience across the website." ]
                                }
                                (Node.range node)
                          ]
                        , context
                        )

                    else
                        ( [], context )

                _ ->
                    ( [], context )

        ( _, _ ) ->
            ( [], context )
