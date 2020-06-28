module NoTodoComment exposing (rule)

import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoTodoComment" ()
        |> Rule.withSimpleCommentsVisitor commentsVisitor
        |> Rule.fromModuleRuleSchema


commentsVisitor : List (Node String) -> List (Error {})
commentsVisitor comments =
    comments
        |> List.concatMap
            (\commentNode ->
                String.indexes "TODO" (Node.value commentNode)
                    |> List.map (errorAtPosition commentNode)
            )


errorAtPosition : Node String -> Int -> Error {}
errorAtPosition node index =
    Rule.error
        { message = "TODO needs to be handled"
        , details = [ "At fruits.com, we prefer not to have lingering TODO comments. Either fix the TODO now or create an issue for it." ]
        }
        -- Here you would ideally only target the TODO keyword
        -- or the rest of the line it appears on,
        -- so you would change `range` using `index`.
        (untilEndOfLine node index)


untilEndOfLine : Node String -> Int -> Range
untilEndOfLine node index =
    let
        range : Range
        range =
            Node.range node

        linesBeforeComment : List String
        linesBeforeComment =
            node
                |> Node.value
                |> String.left index
                |> String.split "\n"

        startColumn : Int
        startColumn =
            if List.length linesBeforeComment == 1 then
                range.start.column + index

            else
                linesBeforeComment
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault ""
                    |> String.length
                    |> (+) 1

        endColumn : Int
        endColumn =
            node
                |> Node.value
                |> String.dropLeft index
                |> String.split "\n"
                |> List.head
                |> Maybe.withDefault ""
                |> String.length
                |> (+) startColumn

        startRow : Int
        startRow =
            range.start.row + (List.length linesBeforeComment - 1)
    in
    { start = { row = startRow, column = startColumn }
    , end = { row = startRow, column = endColumn }
    }
