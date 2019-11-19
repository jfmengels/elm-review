module Review.ModuleInterface exposing (Exposed(..), fromDocsJson)

import Elm.Interface as Interface
import Elm.Syntax.Infix as Infix exposing (InfixDirection)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range
import Json.Decode as Decode



-- DEFINITION


type Exposed
    = CustomType
        { name : String
        , arguments : List String
        , cases : List Case
        }
    | TypeAlias
        { name : String
        , arguments : List String
        , type_ : String
        }
    | Value
        { name : String
        , type_ : String
        }
    | Operator
        { name : String
        , type_ : String
        , precedence : Int
        , associativity : InfixDirection
        }


type alias Case =
    { name : String
    , arguments : List String
    }



-- DECODE


fromDocsJson : Decode.Decoder (List ( String, List Exposed ))
fromDocsJson =
    Decode.list decodeItem


decodeItem : Decode.Decoder ( String, List Exposed )
decodeItem =
    Decode.map5
        (\name customTypes aliases values operators ->
            ( name, List.concat [ customTypes, aliases, values, operators ] )
        )
        (Decode.field "name" Decode.string)
        (Decode.field "unions" (Decode.list decodeCustomType))
        (Decode.field "aliases" (Decode.list decodeTypeAlias))
        (Decode.field "values" (Decode.list decodeValue))
        (Decode.field "binops" (Decode.list decodeOperator))


decodeCustomType : Decode.Decoder Exposed
decodeCustomType =
    Decode.map3
        (\name arguments cases ->
            CustomType
                { name = name
                , arguments = arguments
                , cases = cases
                }
        )
        (Decode.field "name" Decode.string)
        (Decode.field "args" (Decode.list Decode.string))
        (Decode.field "cases" (Decode.list decodeCase))


decodeCase : Decode.Decoder Case
decodeCase =
    Decode.map2 Case
        (Decode.field "0" Decode.string)
        (Decode.field "1" (Decode.list Decode.string))


decodeTypeAlias : Decode.Decoder Exposed
decodeTypeAlias =
    Decode.map3
        (\name arguments type_ ->
            TypeAlias
                { name = name
                , arguments = arguments
                , type_ = type_
                }
        )
        (Decode.field "name" Decode.string)
        (Decode.field "args" (Decode.list Decode.string))
        (Decode.field "type" Decode.string)


decodeValue : Decode.Decoder Exposed
decodeValue =
    Decode.map2
        (\name type_ ->
            Value
                { name = name
                , type_ = type_
                }
        )
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)


decodeOperator : Decode.Decoder Exposed
decodeOperator =
    Decode.map4
        (\name type_ precedence associativity ->
            Operator
                { name = name
                , type_ = type_
                , precedence = precedence
                , associativity = associativity
                }
        )
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)
        (Decode.field "precedence" Decode.int)
        (Decode.field "associativity" decodeAssociativity)


decodeAssociativity : Decode.Decoder InfixDirection
decodeAssociativity =
    Decode.string
        |> Decode.andThen
            (\infix ->
                case infix of
                    "left" ->
                        Decode.succeed Infix.Left

                    "non" ->
                        Decode.succeed Infix.Non

                    "right" ->
                        Decode.succeed Infix.Right

                    _ ->
                        Decode.fail "Unknown associativity for operator"
            )



-- ELM-SYNTAX


toElmSyntaxInterface : List Exposed -> Interface.Interface
toElmSyntaxInterface exposedList =
    List.map toElmSyntaxExposed exposedList


toElmSyntaxExposed : Exposed -> Interface.Exposed
toElmSyntaxExposed exposed =
    case exposed of
        CustomType { name, cases } ->
            Interface.CustomType ( name, List.map .name cases )

        TypeAlias { name } ->
            Interface.Alias name

        Value { name } ->
            Interface.Function name

        Operator { name, associativity, precedence } ->
            Interface.Operator
                { operator = Node.Node Range.emptyRange name
                , direction = Node.Node Range.emptyRange associativity
                , precedence = Node.Node Range.emptyRange precedence
                , function = Node.Node Range.emptyRange name
                }
