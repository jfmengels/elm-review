module Lint.Direction exposing (Direction(..))

{-| Represents whether a Node is being traversed before having seen it's children (`Enter`ing the Node), or after (`Exit`ing the Node).


# Definition

@docs Direction

-}


{-| When visiting the AST, nodes are visited twice:

  - on Enter, before the children of the node will be visited

  - on Exit, after the children of the node have been visited

    expressionVisitor : Context -> Direction -> Node Expression -> ( List Error, Context )
    expressionVisitor context direction node =
    case (direction, node) of
    ( Enter, Expression.FunctionOrValue moduleName name) ->
    ( [], markVariableAsUsed context name )

              -- Find variables declared in `let .. in ..` expression
              ( Enter, LetExpression letBlock ) ->
                  ( [], registerVariables context letBlock )

              -- When exiting the `let .. in ..` expression, report the variables that were not used.
              ( Enter, LetExpression _ ) ->
                  ( unusedVariables context |> List.map createError, context )

-}
type Direction
    = Enter
    | Exit
