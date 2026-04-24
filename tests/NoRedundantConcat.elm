module NoRedundantConcat exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Elm.Writer as Writer
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbids using concatenation (`++` and `List.concat`) when it's not needed.

Expressions like `[ a, b ] ++ c` will be flagged, with a fix proposing to
rewrite that to somethine like `a :: b :: c`. This is more performant and makes
it clear that we're talking about consing items to the head of a list. Easy!

Expressions like `[ a, b ] ++ [ c, d ]` could be rewritten to a single literal
list: no need to perform the concatenation at runtime when we can just write it
ourselves! So, the fix will propose writing that as `[ a, b, c, d ]`.

Expression like `List.concat [ [ a ], [ b ], [ c ] ]` could become `[ a, b c ]`.

Finally, expressions like `"foo" ++ "bar"` can also be `"foobar"`.

To use this rule, add it to your `elm-review` config like so:

    module ReviewConfig exposing (config)

    import NoRedundantConcat
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ NoRedundantConcat.rule
        ]

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoRedundantConcat" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor (Node.Node range expression) =
    case expression of
        Expression.OperatorApplication "++" _ (Node.Node _ (Expression.ListExpr leftItems)) (Node.Node _ (Expression.ListExpr rightItems)) ->
            [ Rule.errorWithFix
                { message = "Concatenating a literal list with another literal list can be written as a single list literal"
                , details =
                    [ "Expressions like `[ foo ] ++ [ bar ]` can be written as `[ foo, bar ]`."
                    , "Using 'complex' expressions when not necessary can make code look a lot more complex than it really is. When you need to put two literal lists together, you can just put them together! No need to have that happen at runtime."
                    ]
                }
                range
                [ combineLists range leftItems rightItems ]
            ]

        Expression.OperatorApplication "++" _ (Node.Node _ (Expression.Literal left)) (Node.Node _ (Expression.Literal right)) ->
            [ Rule.errorWithFix
                { message = "Concatenating a literal string with another literal string is redundant"
                , details =
                    [ "Expressions like `\"foo\" ++ \"bar\"` are harder to read than `\"foobar\"`. Consider simplifying this expression."
                    ]
                }
                range
                [ Fix.replaceRangeBy range (expressionToString range (Expression.Literal (left ++ right))) ]
            ]

        Expression.OperatorApplication "++" _ (Node.Node _ (Expression.ListExpr items)) right ->
            [ Rule.errorWithFix
                { message = "Concatenating a literal list with something else can be written using cons operators"
                , details =
                    [ "Expressions like `[ foo ] ++ b` can be written as `foo :: b`."
                    , "This preserves the mental model that `List` is a linked list, with the performance considerations associated with those."
                    ]
                }
                range
                [ concatItems range (List.reverse items) right ]
            ]

        Expression.Application [ Node.Node _ (Expression.FunctionOrValue [ "List" ] "concat"), Node.Node _ (Expression.ListExpr items) ] ->
            attemptToCombineItems range items []

        _ ->
            []


attemptToCombineItems : Range -> List (Node Expression) -> List (Node Expression) -> List (Error {})
attemptToCombineItems range expressions acc =
    case expressions of
        [] ->
            [ Rule.errorWithFix
                { message = "Using List.concat to concatenate list literals is redundant"
                , details = [ "Rather than using `List.concat`, consider putting the elements into a single list literal" ]
                }
                range
                [ Fix.replaceRangeBy range
                    (expressionToString range (Expression.ListExpr acc))
                ]
            ]

        (Node.Node _ (Expression.ListExpr items)) :: rest ->
            attemptToCombineItems range rest (acc ++ items)

        _ ->
            []


combineLists : Range -> List (Node Expression) -> List (Node Expression) -> Fix.Fix
combineLists range left right =
    (left ++ right)
        |> Expression.ListExpr
        |> expressionToString range
        |> Fix.replaceRangeBy range


concatItems : Range -> List (Node Expression) -> Node Expression -> Fix.Fix
concatItems range items ((Node.Node ontoRange ontoExpr) as onto) =
    case items of
        [] ->
            Fix.replaceRangeBy range (expressionToString ontoRange ontoExpr)

        item :: rest ->
            concatItems range
                rest
                (Node.Node ontoRange
                    (Expression.OperatorApplication "::" Infix.Non item onto)
                )


expressionToString : Range -> Expression -> String
expressionToString range expr =
    Node.Node range expr
        |> fixAst
        |> Writer.writeExpression
        |> Writer.write
        -- To make things more parseable by our dear old elm-format, we indent all extra
        -- lines so they appear to the right of where we're starting at
        |> reindent range.start.column


fixAst : Node Expression -> Node Expression
fixAst ((Node.Node range expr) as node) =
    case expr of
        Expression.Application nodes ->
            Node.Node range (Expression.Application (List.map fixAst nodes))

        Expression.OperatorApplication op dir left right ->
            Node.Node range
                (Expression.OperatorApplication op dir (fixAst left) (fixAst right))

        Expression.IfBlock cond trueBranch falseBranch ->
            Node.Node range
                (Expression.IfBlock (fixAst cond) (fixAst trueBranch) (fixAst falseBranch))

        Expression.Negation inner ->
            Node.Node range (Expression.Negation (fixAst inner))

        Expression.TupledExpression nodes ->
            Node.Node range (Expression.TupledExpression (List.map fixAst nodes))

        Expression.ParenthesizedExpression inner ->
            Node.Node range (Expression.ParenthesizedExpression (fixAst inner))

        Expression.LetExpression { declarations, expression } ->
            Node.Node range
                (Expression.LetExpression
                    { declarations = List.map fixLetDeclarationAst declarations
                    , expression = fixAst expression
                    }
                )

        Expression.CaseExpression { expression, cases } ->
            Node.Node range
                (Expression.CaseExpression
                    { expression = fixAst expression
                    , cases = List.map fixCaseAst cases
                    }
                )

        Expression.LambdaExpression { args, expression } ->
            Node.Node range
                (Expression.LambdaExpression
                    { args = args
                    , expression = fixAst expression
                    }
                )

        Expression.RecordExpr setters ->
            Node.Node range
                (Expression.RecordExpr (List.map fixSetterAst setters))

        Expression.ListExpr nodes ->
            Node.Node range (Expression.ListExpr (List.map fixAst nodes))

        Expression.RecordAccess which field ->
            Node.Node range (Expression.RecordAccess (fixAst which) field)

        Expression.RecordAccessFunction field ->
            -- Writer expects a record-access function to be `"foo"` whereas the parser produces `".foo"`
            Node.Node range (Expression.RecordAccessFunction (String.dropLeft 1 field))

        Expression.RecordUpdateExpression record setters ->
            Node.Node range (Expression.RecordUpdateExpression record (List.map fixSetterAst setters))

        _ ->
            node


fixCaseAst : Expression.Case -> Expression.Case
fixCaseAst ( pat, node ) =
    ( pat, fixAst node )


fixLetDeclarationAst : Node Expression.LetDeclaration -> Node Expression.LetDeclaration
fixLetDeclarationAst (Node.Node range decl) =
    case decl of
        Expression.LetFunction ({ declaration } as f) ->
            let
                (Node.Node iRange iDecl) =
                    declaration
            in
            Node.Node range
                (Expression.LetFunction
                    { f
                        | declaration = Node.Node iRange { iDecl | expression = fixAst iDecl.expression }
                    }
                )

        Expression.LetDestructuring pat node ->
            Node.Node range (Expression.LetDestructuring pat (fixAst node))


fixSetterAst : Node Expression.RecordSetter -> Node Expression.RecordSetter
fixSetterAst (Node.Node range ( name, node )) =
    Node.Node range ( name, fixAst node )


reindent : Int -> String -> String
reindent amount =
    String.lines >> String.join ("\n" ++ String.repeat (amount - 1) " ")
