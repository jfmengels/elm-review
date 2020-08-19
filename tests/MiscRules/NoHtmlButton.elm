module MiscRules.NoHtmlButton exposing (rule)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Scope


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoHtmlButton" contextCreator
        -- Scope.addModuleVisitors should be added before your own visitors
        --|> Scope.addModuleVisitors
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema
        |> Rule.ignoreErrorsForFiles [ "src/Button.elm" ]


type alias Context =
    -- Scope expects a context with a record, containing the `scope` field.
    { scope : Scope.ModuleContext
    , moduleNameLookupTable : ModuleNameLookupTable
    }


contextCreator : Rule.ContextCreator () Context
contextCreator =
    Rule.initContextCreator
        (\moduleNameLookupTable () ->
            { scope = Scope.initialModuleContext
            , moduleNameLookupTable = moduleNameLookupTable
            }
        )
        |> Rule.withModuleNameLookupTable


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        FunctionOrValue _ "button" ->
            if ModuleNameLookupTable.moduleNameFor context.moduleNameLookupTable node == Just [ "Html" ] then
                -- TODO Remove
                --if Scope.moduleNameForValue context.scope "button" moduleName == [ "Html" ] then
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
