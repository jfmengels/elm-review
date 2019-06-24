module Lint.Direction exposing (Direction(..))

{-| Represents whether a Node is being traversed before having seen it's children (`Enter`ing the Node), or after (`Exit`ing the Node).


# Definition

@docs Direction

-}


{-| When visiting the AST, nodes are visited twice: once on `Enter`, before the
children of the node will be visited, and once on `Exit`, after the children of
the node have been visited.

In most cases, you'll only want to handle the `Enter` case, but in some cases,
you'll want to visit a `Node` after having seen it's children. For instance, if
you're trying to detect the unused variables defined inside of a `let in` expression,
you'll want to collect the declaration of variables, note which ones are used,
and at the end of the block, report the ones that weren't used.

    expressionVisitor : Context -> Direction -> Node Expression -> ( List Error, Context )
    expressionVisitor context direction node =
        case ( direction, node ) of
            ( Direction., Expression.FunctionOrValue moduleName name ) ->
            ( [], markVariableAsUsed context name )

            -- Find variables declared in `let in` expression
            ( Direction.Enter, LetExpression letBlock ) ->
                ( [], registerVariables context letBlock )

            -- When exiting the `let in expression, report the variables that were not used.
            ( Direction.Exit, LetExpression _ ) ->
                ( unusedVariables context |> List.map createError, context )

-}
type Direction
    = Enter
    | Exit
