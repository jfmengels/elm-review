module Simplify.HashExpression exposing (hash)

import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Simplify.CoreHelpers exposing (listMapToStringsThenJoin)


hash : Node Expression -> String
hash node =
    hashHelp [ node ] ""


hashHelp : List (Node Expression) -> String -> String
hashHelp nodes acc =
    case nodes of
        [] ->
            acc

        (Node _ expr) :: rest ->
            case expr of
                FunctionOrValue moduleName name ->
                    hashHelp
                        rest
                        (acc ++ "$0" ++ String.join "." (name :: moduleName) ++ ";")

                Application l ->
                    hashHelp
                        (l ++ rest)
                        (acc ++ "$1;")

                OperatorApplication op _ left right ->
                    hashHelp
                        (left :: right :: rest)
                        (acc ++ "$2" ++ op ++ ";")

                IfBlock c t e ->
                    hashHelp
                        (c :: t :: e :: rest)
                        (acc ++ "$3;")

                Hex h ->
                    hashHelp
                        rest
                        (acc ++ "$4" ++ String.fromInt h ++ ";")

                Integer x ->
                    hashHelp
                        rest
                        (acc ++ "$5" ++ String.fromInt x ++ ";")

                Floatable x ->
                    hashHelp
                        rest
                        (acc ++ "$6" ++ String.fromFloat x ++ ";")

                Negation x ->
                    hashHelp
                        (x :: rest)
                        (acc ++ "$7;")

                Literal x ->
                    hashHelp
                        rest
                        (acc ++ "$8" ++ x ++ ";")

                CharLiteral c ->
                    hashHelp
                        rest
                        (acc ++ "$9" ++ String.fromChar c ++ ";")

                TupledExpression xs ->
                    hashHelp
                        (xs ++ rest)
                        (acc ++ "$10;")

                ListExpr xs ->
                    hashHelp
                        (xs ++ rest)
                        (acc ++ "$11;")

                LetExpression { declarations, expression } ->
                    hashHelp
                        (expression :: rest)
                        (acc ++ "$12(" ++ list_ encodeLetDeclaration declarations ++ ");")

                CaseExpression { expression, cases } ->
                    hashHelp
                        (expression :: rest)
                        (acc ++ "$13(" ++ list_ encodeCase cases ++ ");")

                LambdaExpression { args, expression } ->
                    hashHelp
                        (expression :: rest)
                        (acc ++ "$14(" ++ list_ encodePattern args ++ ");")

                RecordAccess expression (Node _ name) ->
                    hashHelp
                        (expression :: rest)
                        (acc ++ "$15" ++ name ++ ");")

                RecordExpr xs ->
                    hashHelp
                        rest
                        (acc ++ "$16{" ++ list_ encodeRecordSetter xs ++ "};")

                RecordUpdateExpression (Node _ name) updates ->
                    hashHelp
                        rest
                        (acc ++ "$17(n=" ++ name ++ ",u=" ++ list_ encodeRecordSetter updates ++ ");")

                RecordAccessFunction name ->
                    hashHelp
                        rest
                        (acc ++ "$18" ++ name ++ ";")

                UnitExpr ->
                    hashHelp
                        rest
                        (acc ++ "$19;")

                PrefixOperator x ->
                    hashHelp
                        rest
                        (acc ++ "$20" ++ x ++ ";")

                Operator x ->
                    hashHelp
                        rest
                        (acc ++ "$21" ++ x ++ ";")

                GLSLExpression x ->
                    hashHelp
                        rest
                        (acc ++ "$22" ++ x ++ ";")

                ParenthesizedExpression x ->
                    hashHelp
                        (x :: rest)
                        acc


encodeRecordSetter : Node Expression.RecordSetter -> String
encodeRecordSetter (Node _ ( Node _ field, expression )) =
    object_
        [ ( "field", field )
        , ( "expression", hash expression )
        ]


encodeLetDeclaration : Node Expression.LetDeclaration -> String
encodeLetDeclaration (Node _ letDeclaration) =
    case letDeclaration of
        Expression.LetFunction { declaration } ->
            "£0" ++ encodeFunctionDeclaration declaration

        Expression.LetDestructuring pattern expression ->
            "£1" ++ encodeDestructuring pattern expression


encodeFunctionDeclaration : Node Expression.FunctionImplementation -> String
encodeFunctionDeclaration (Node _ { name, arguments, expression }) =
    object_
        [ ( "name", Node.value name )
        , ( "arguments", list_ encodePattern arguments )
        , ( "expression", hash expression )
        ]


encodeDestructuring : Node Pattern -> Node Expression -> String
encodeDestructuring pattern expression =
    object_
        [ ( "pattern", encodePattern pattern )
        , ( "expression", hash expression )
        ]


encodeCase : Expression.Case -> String
encodeCase ( pattern, expression ) =
    object_
        [ ( "pattern", encodePattern pattern )
        , ( "expression", hash expression )
        ]


{-| Encode a `Pattern` syntax element to JSON.
-}
encodePattern : Node Pattern -> String
encodePattern (Node _ pattern) =
    case pattern of
        VarPattern name ->
            "€0" ++ name

        UnitPattern ->
            "€1"

        CharPattern c ->
            "€2" ++ String.fromChar c

        StringPattern v ->
            "€3" ++ v

        NamedPattern ref patterns ->
            "€4" ++ String.join "." (ref.name :: ref.moduleName) ++ wrap "(" ")" (list_ encodePattern patterns)

        TuplePattern patterns ->
            "€5" ++ wrap "(" ")" (list_ encodePattern patterns)

        RecordPattern fields ->
            "€6" ++ wrap "{" "}" (list_ Node.value fields)

        UnConsPattern p1 p2 ->
            "€7" ++ encodePattern p1 ++ "::" ++ encodePattern p2

        ListPattern patterns ->
            "€8" ++ wrap "[" "]" (list_ encodePattern patterns)

        AsPattern p (Node _ name) ->
            "€9" ++ name ++ ";" ++ encodePattern p

        HexPattern int ->
            "€10" ++ String.fromInt int

        IntPattern int ->
            "€11" ++ String.fromInt int

        FloatPattern f ->
            "€12" ++ String.fromFloat f

        AllPattern ->
            "€13"

        ParenthesizedPattern p ->
            "€14" ++ encodePattern p


list_ : (a -> String) -> List a -> String
list_ f list =
    list
        |> listMapToStringsThenJoin f ","
        |> wrap "[" "]"


wrap : String -> String -> String -> String
wrap before after str =
    before ++ str ++ after ++ ""


object_ : List ( String, String ) -> String
object_ list =
    list
        |> listMapToStringsThenJoin
            (\( k, v ) -> "(" ++ k ++ "=" ++ v ++ ")")
            ","
