module NoTodoComment exposing (rule)

import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoTodoComment" ()
        |> Rule.withSimpleCommentsVisitor commentsVisitor
        |> Rule.fromModuleRuleSchema


commentsVisitor : List (Node String) -> List Error
commentsVisitor comments =
    comments
        |> List.concatMap
            (\commentNode ->
                String.indexes "TODO" (Node.value commentNode)
                    |> List.map (errorAtPosition (Node.range commentNode))
            )


errorAtPosition : Range -> Int -> Error
errorAtPosition range index =
    Rule.error
        { message = "TODO needs to be handled"
        , details = [ "At fruits.com, we prefer not to have lingering TODO comments. Either fix the TODO now or create an issue for it." ]
        }
        -- Here you would ideally only target the TODO keyword
        -- or the line it appears on,
        -- so you would change `range` using `index`.
        range
