module NoMissingTypeAnnotation.Print exposing (Context, Print, insertTypeAnnotation)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Location)
import Elm.TypeInference.Type exposing (Type(..))
import Review.Fix as Fix exposing (Edit)
import Set exposing (Set)


type Print
    = Print (Set String) String


type alias Context a =
    { a
        | importLine : Int
        , availableTypes : Set ( String, String )
        , moduleNameAliases : Dict String String
    }


insertTypeAnnotation : Context a -> Location -> String -> Type -> List Edit
insertTypeAnnotation context insertLocation name t =
    let
        (Print missingImports stringifiedType) =
            toStringHelp context t

        missingImportsStr : String
        missingImportsStr =
            Set.foldr (\moduleName imports -> "import " ++ moduleName ++ "\n" ++ imports) "" missingImports
    in
    [ Fix.insertAt insertLocation (name ++ " : " ++ stringifiedType ++ "\n")
    , Fix.insertAt { row = context.importLine, column = 1 } missingImportsStr
    ]


pure : String -> Print
pure =
    Print Set.empty


map : (String -> String) -> Print -> Print
map fn (Print set str) =
    Print set (fn str)


map2 : (String -> String -> String) -> Print -> Print -> Print
map2 fn (Print set1 str1) (Print set2 str2) =
    Print (Set.union set1 set2) (fn str1 str2)


andThen : (String -> Print) -> Print -> Print
andThen fn (Print set str) =
    let
        (Print newSet newStr) =
            fn str
    in
    Print (Set.union set newSet) newStr


toStringHelp : Context a -> Type -> Print
toStringHelp context t =
    case t of
        TypeVar name ->
            pure name

        Function { from, to } ->
            map2 (\from_ to_ -> from_ ++ " -> " ++ to_)
                (wrappedFrom context from)
                (toStringHelp context to)

        Int ->
            pure "Int"

        Float ->
            pure "Float"

        Char ->
            pure "Char"

        String ->
            pure "String"

        Bool ->
            pure "Bool"

        List inner ->
            wrapped context inner
                |> map (\str -> "List " ++ str)

        Unit ->
            pure "()"

        Tuple2 a b ->
            map2
                (\a_ b_ -> "( " ++ a_ ++ ", " ++ b_ ++ " )")
                (toStringHelp context a)
                (toStringHelp context b)

        Tuple3 a b c ->
            map2
                (\a_ b_ -> "( " ++ a_ ++ ", " ++ b_ ++ " )")
                (toStringHelp context a)
                (map2 (\b_ c_ -> b_ ++ ", " ++ c_)
                    (toStringHelp context b)
                    (toStringHelp context c)
                )

        Record { fields } ->
            if Dict.isEmpty fields then
                pure "{}"

            else
                Dict.foldl
                    (\name fieldType acc ->
                        map2
                            (\acc_ typeStr -> acc_ ++ ", " ++ name ++ " : " ++ typeStr)
                            acc
                            (toStringHelp context fieldType)
                    )
                    (pure "")
                    fields
                    |> map (\s -> "{" ++ String.dropLeft 1 s ++ " }")

        ExtensibleRecord { fields, extensionTypevar } ->
            Dict.foldl
                (\name fieldType acc ->
                    map2
                        (\acc_ typeStr -> acc_ ++ ", " ++ name ++ " : " ++ typeStr)
                        acc
                        (toStringHelp context fieldType)
                )
                (pure "")
                fields
                |> map (\s -> "{ " ++ extensionTypevar ++ " |" ++ String.dropLeft 1 s ++ " }")

        Named { moduleName, name, arguments } ->
            let
                argStrings : Print
                argStrings =
                    List.foldl (wrapped context >> map2 (\a acc -> acc ++ " " ++ a)) (pure "") arguments

                dotted : String
                dotted =
                    String.join "." moduleName

                qualifiedName : Print
                qualifiedName =
                    if Set.member ( dotted, name ) context.availableTypes then
                        pure name

                    else
                        case Dict.get dotted context.moduleNameAliases of
                            Just "" ->
                                pure name

                            Just alias_ ->
                                pure (alias_ ++ "." ++ name)

                            Nothing ->
                                Print (Set.singleton dotted) (dotted ++ "." ++ name)
            in
            map2 (++) qualifiedName argStrings

        WebGLShader r ->
            map2
                (\a_ b_ -> "Shader " ++ a_ ++ " " ++ b_)
                (wrapped context r.attributes)
                (map2 (\b_ c_ -> b_ ++ " " ++ c_)
                    (wrapped context r.uniforms)
                    (wrapped context r.varyings)
                )


{-| Wraps a type in parentheses when it wouldn't parse back unambiguously
as an argument of a type constructor application.
-}
wrapped : Context a -> Type -> Print
wrapped context t =
    case t of
        Function _ ->
            paren (toStringHelp context t)

        List _ ->
            paren (toStringHelp context t)

        WebGLShader _ ->
            paren (toStringHelp context t)

        Named r ->
            if List.isEmpty r.arguments then
                toStringHelp context t

            else
                paren (toStringHelp context t)

        _ ->
            toStringHelp context t


{-| Wraps a type in parentheses when it wouldn't parse back unambiguously on the
left of `->`.

`->` is right-associative and type application binds tighter, so only a nested
`->` needs parens there: `List a -> b` already parses as `(List a) -> b`.

-}
wrappedFrom : Context a -> Type -> Print
wrappedFrom context t =
    case t of
        Function _ ->
            paren (toStringHelp context t)

        _ ->
            toStringHelp context t


paren : Print -> Print
paren =
    map (\str -> "(" ++ str ++ ")")
