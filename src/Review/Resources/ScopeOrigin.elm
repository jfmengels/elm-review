module Review.Resources.ScopeOrigin exposing (ScopeOrigin(..))

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)


type ScopeOrigin
    = TopLevel
    | Function (Node Expression.Function)
    | LetBlock (Node Expression.LetBlock)
    | CaseBranch ( Node Pattern, Node Expression )
    | Lambda (Node Expression.Lambda)
