module NoMissingTypeAnnotationInLetInTest exposing (all)

import Dependencies.ElmCore
import NoMissingTypeAnnotationInLetIn exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


details : List String
details =
    [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
    ]


all : Test
all =
    describe "NoMissingTypeAnnotationInLetIn"
        [ reportTests
        , fixTests
        ]


reportTests : Test
reportTests =
    describe "Reports"
        [ test "should not report anything for top-level declarations even if they have no type annotation" <|
            \_ ->
                """module A exposing (..)
hasTypeAnnotation : Int
hasTypeAnnotation = 1

hasNoTypeAnnotation = doSomething
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should not report anything when all let in declarations have a type annotation" <|
            \_ ->
                """module A exposing (..)
a = let
      b : number
      b = 1

      c : number
      c = 1
    in
    b + c
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when a let in declaration has no type annotation" <|
            \_ ->
                """module A exposing (..)
a = let
      hasNoTypeAnnotation_1 = foo
      hasNoTypeAnnotation_2 = foo
    in
    d
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing type annotation for `hasNoTypeAnnotation_1`"
                            , details = details
                            , under = "hasNoTypeAnnotation_1"
                            }
                        , Review.Test.error
                            { message = "Missing type annotation for `hasNoTypeAnnotation_2`"
                            , details = details
                            , under = "hasNoTypeAnnotation_2"
                            }
                        ]
        , test "should not report anything for let..in destructuring" <|
            \_ ->
                """module A exposing (..)
a = let
      (b, c) = foo
      {e, f} = foo
      (Thing thing) = foo
    in
    d
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        ]


fixTests : Test
fixTests =
    describe "Fixing"
        [ fixTest "when value is a literal string"
            { arguments = ""
            , value = "\"abc\""
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal char"
            { arguments = ""
            , value = "'c'"
            , expectedType = "Char"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal integer"
            { arguments = ""
            , value = "1"
            , expectedType = "number"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal hex"
            { arguments = ""
            , value = "0x12"
            , expectedType = "number"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal float"
            { arguments = ""
            , value = "1.0"
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a literal unit"
            { arguments = ""
            , value = "()"
            , expectedType = "()"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a negation of a number"
            { arguments = ""
            , value = "-1"
            , expectedType = "number"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a negation of a Float"
            { arguments = ""
            , value = "-1.0"
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
        , noFixTest "when value is a negation of something unknown"
            { arguments = ""
            , value = "-someThing"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is `True`"
            { arguments = ""
            , value = "True"
            , expectedType = "Bool"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is `False`"
            { arguments = ""
            , value = "False"
            , expectedType = "Bool"
            , topLevelDeclarations = ""
            }
        , fixTest "when value equals a top-level value: String"
            { arguments = ""
            , value = "someValue"
            , expectedType = "String"
            , topLevelDeclarations = """someValue : String
someValue = "abc\""""
            }
        , fixTest "when value equals a top-level value: Int"
            { arguments = ""
            , value = "someValue"
            , expectedType = "Int"
            , topLevelDeclarations = """someValue : Int
someValue = 1"""
            }
        , fixTest "when value equals a top-level value: Float"
            { arguments = ""
            , value = "someValue"
            , expectedType = "Float"
            , topLevelDeclarations = """someValue : Float
someValue = 1.0"""
            }
        , fixTest "when value equals a top-level value: unit"
            { arguments = ""
            , value = "someValue"
            , expectedType = "()"
            , topLevelDeclarations = """someValue : ()
someValue = ()"""
            }
        , fixTest "when value equals a top-level value: typeclass"
            { arguments = ""
            , value = "someValue"
            , expectedType = "number"
            , topLevelDeclarations = """someValue : number
someValue = 1"""
            }
        , fixTest "when value equals a top-level value: empty record"
            { arguments = ""
            , value = "someValue"
            , expectedType = "{}"
            , topLevelDeclarations = """someValue : { }
someValue = { a = 1 }"""
            }
        , fixTest "when value equals a top-level value: record"
            { arguments = ""
            , value = "someValue"
            , expectedType = "{ a : Int, b : String }"
            , topLevelDeclarations = """someValue : { a : Int, b:String }
someValue = someThing"""
            }
        , fixTest "when value equals a generic type"
            { arguments = ""
            , value = "someValue"
            , expectedType = "genericType"
            , topLevelDeclarations = """someValue : genericType
someValue = 1"""
            }
        , fixTest "when value equals a function"
            { arguments = ""
            , value = "someValue"
            , expectedType = "String -> Int"
            , topLevelDeclarations = """someValue : String -> Int
someValue = String.length"""
            }
        , fixTest "when value equals a function (multiple arguments)"
            { arguments = ""
            , value = "someValue"
            , expectedType = "Thing -> String -> Int"
            , topLevelDeclarations = """someValue : Thing -> String -> Int
someValue = something"""
            }
        , fixTest "when value is a function call to a known top-level function"
            { arguments = ""
            , value = "someValue thing"
            , expectedType = "String -> Int"
            , topLevelDeclarations = """someValue : Thing -> String -> Int
someValue = something"""
            }
        , fixTest "when value is a function call to a known top-level function (multiple arguments)"
            { arguments = ""
            , value = "someValue thing string"
            , expectedType = "Int"
            , topLevelDeclarations = """someValue : Thing -> String -> Int
someValue = something"""
            }
        , fixTest "when value is a call to known top-level function with a function as argument"
            { arguments = ""
            , value = "someValue thing string"
            , expectedType = "Int"
            , topLevelDeclarations = """someValue : (Thing -> Thing) -> String -> Int
someValue = something"""
            }
        , fixTest "when value is a function call to a known top-level function where parens are needed for the type variable"
            { arguments = ""
            , value = "someValue thing"
            , expectedType = "List (Attribute msg)"
            , topLevelDeclarations = """someValue : String -> List (Attribute msg)
someValue = something"""
            }
        , fixTest "when value is a function call to a known top-level function where parens are needed for the underlying function"
            { arguments = ""
            , value = "someValue string"
            , expectedType = "(Thing -> Thing) -> Int"
            , topLevelDeclarations = """someValue : String -> (Thing -> Thing) -> Int
someValue = something"""
            }
        , fixTest "when value is a function call to a known top-level function where one argument needs parens inside it"
            { arguments = ""
            , value = "someValue"
            , expectedType = "List (Attribute msg) -> List (Html msg) -> Html msg"
            , topLevelDeclarations = """someValue : List (Attribute msg) -> List (Html msg) -> Html msg
someValue = something"""
            }
        , fixTest "when value is a tuple"
            { arguments = ""
            , value = """( "abc", 1.0 )"""
            , expectedType = "( String, Float )"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a 3-tuple"
            { arguments = ""
            , value = """( "abc", 1.0, 1 )"""
            , expectedType = "( String, Float, number )"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is an empty list"
            -- TODO Create another case where this generic type is already in scope
            { arguments = ""
            , value = """[]"""
            , expectedType = "List nothing"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a list with known values"
            { arguments = ""
            , value = """[ "abc" ]"""
            , expectedType = "List String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a list with typeclasses"
            { arguments = ""
            , value = """[ 1 ]"""
            , expectedType = "List number"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a list where the first and some elements are unknown"
            { arguments = ""
            , value = """[ thing, "abc", otherThing ]"""
            , expectedType = "List String"
            , topLevelDeclarations = ""
            }
        , noFixTest "should not provide a fix when value is a list of only unknown values"
            { arguments = ""
            , value = "[ thing, someThing abc, otherThing ]"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a list with typeclasses where one element is very precise"
            { arguments = ""
            , value = "[ 1, 1.1 ]"
            , expectedType = "List Float"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is an empty list and Basics has been aliased"
            { arguments = ""
            , value = """[]"""
            , expectedType = "List nothing"
            , topLevelDeclarations = "import Basics as Thing"
            }
        , fixTest "when value is a non-empty list and Basics has been aliased"
            { arguments = ""
            , value = """[ 1.0 ]"""
            , expectedType = "List Thing.Float"
            , topLevelDeclarations = "import Basics as Thing"
            }
        , fixTest "when value is a list with typeclasses where one element is very precise (in a different order)"
            { arguments = ""
            , value = "[ 1, a, 1.1, 1 ]"
            , expectedType = "List Float"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a list with typed elements"
            { arguments = ""
            , value = "[ [1], [1.1] ]"
            , expectedType = "List (List Float)"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is an empty record"
            { arguments = ""
            , value = "{}"
            , expectedType = "{}"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a record with fields"
            { arguments = ""
            , value = """{ a = "abc", b = 1.0 }"""
            , expectedType = "{ a : String, b : Float }"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a record with a type variable"
            { arguments = ""
            , value = """{ a = Nothing }"""
            , expectedType = "{ a : Maybe a }"
            , topLevelDeclarations = ""
            }
        , Test.skip <|
            fixTest "when value is a record with multiple probably disjoint type variables"
                { arguments = ""
                , value = """{ a = Nothing, b = Nothing }"""
                , expectedType = "{ a : Maybe a, b : Maybe b }"
                , topLevelDeclarations = ""
                }
        , noFixTest "should not provide a fix when value is a record where some fields are unknown"
            { arguments = ""
            , value = "{ a = someThing, b = 1.0 }"
            , topLevelDeclarations = ""
            }
        , noFixTest "should not provide a fix when value is a function with extensible records as both input and output"
            -- TODO This should be supported when we infer type variables better
            { arguments = ""
            , value = """func { name = "abc" }"""
            , topLevelDeclarations = """
func : { a | name : String } -> { a | name : String }
func value = value
"""
            }
        , fixTest "when value is a parenthesized expression"
            { arguments = ""
            , value = """("abc")"""
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a record access on a record"
            { arguments = ""
            , value = """{ a = "abc", b = 1.0 }.a"""
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a record access on a complex value"
            { arguments = ""
            , value = "(someValue thing).a"
            , expectedType = "String"
            , topLevelDeclarations = """someValue : thing -> { a : String, b : Int }
someValue = something"""
            }
        , fixTest "when value is a record access function"
            -- TODO Create another case where this generic type is already in scope
            { arguments = ""
            , value = ".field"
            , expectedType = "{ b | field : a } -> a"
            , topLevelDeclarations = ""
            }
        , noFixTest "should not provide a fix (for now) when value is a record access function used on an expression"
            -- TODO This should be supported when we infer type variables better
            { arguments = ""
            , value = ".field { field = 1.0 } "
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a custom type variant without arguments"
            { arguments = ""
            , value = "A"
            , expectedType = "CustomType"
            , topLevelDeclarations = "type CustomType = A | B"
            }
        , fixTest "when value is part of a custom type that will use generics"
            { arguments = ""
            , value = "A"
            , expectedType = "a -> CustomType a"
            , topLevelDeclarations = "type CustomType a = A a | B"
            }
        , noFixTest "should not provide a fix (for now) when a custom type's variant uses generics"
            { arguments = ""
            , value = "A 1"
            , topLevelDeclarations = "type CustomType a = A a | B"
            }
        , fixTest "should provide a fix when a custom type's variant does not use the type's generics"
            { arguments = ""
            , value = "B"
            , expectedType = "CustomType a"
            , topLevelDeclarations = "type CustomType a = A a | B"
            }
        , fixTest "should provide a fix when value is a record type alias"
            { arguments = ""
            , value = "TypeAlias"
            , expectedType = "Int -> String -> TypeAlias"
            , topLevelDeclarations = "type alias TypeAlias = { a : Int, b : String }"
            }
        , fixTest "should provide a fix when value is a record type alias with generics"
            { arguments = ""
            , value = "TypeAlias"
            , expectedType = "b -> String -> TypeAlias a b"
            , topLevelDeclarations = "type alias TypeAlias a b = { a | c : b, d : String }"
            }
        , noFixTest "should not provide a fix when value is an alias to something that is not a record"
            { arguments = ""
            , value = "TypeAlias"
            , topLevelDeclarations = "type alias TypeAlias = Something"
            }
        , fixTest "when value is reference to a ingoing port"
            { arguments = ""
            , value = "input"
            , expectedType = "(String -> msg) -> Sub msg"
            , topLevelDeclarations = "port input : (String -> msg) -> Sub msg"
            }
        , fixTest "when value is reference to a outgoing port"
            { arguments = ""
            , value = "output"
            , expectedType = "String -> Cmd msg"
            , topLevelDeclarations = "port output : String -> Cmd msg"
            }
        , fixTest "when value is an if condition"
            { arguments = ""
            , value = """if condition then "abc" else "bcd" """
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is an if condition and true block is not known"
            { arguments = ""
            , value = """if condition then someThing else "bcd" """
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is an if condition and first condition could be more precise"
            { arguments = ""
            , value = "if condition then 1 else 1.0"
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
        , noFixTest "when value is an if condition and first condition could be more precise but second is unknown"
            { arguments = ""
            , value = "if condition then 1 else someThing"
            , topLevelDeclarations = ""
            }
        , noFixTest "when value is an if condition and first condition is unknown but second could be more precise"
            { arguments = ""
            , value = "if condition then someThing else 1"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is an if condition and both conditions use the same type variables"
            { arguments = ""
            , value = "if condition then 1 else 1"
            , expectedType = "number"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a record update expression"
            { arguments = ""
            , value = "{ person | age = 10 }"
            , expectedType = "{ name : String, age : Int }"
            , topLevelDeclarations = """person : { name : String, age : Int }
person = someThing"""
            }
        , fixTest "when value is a record update expression of a record with generics but doesn't change that field"
            { arguments = ""
            , value = "{ person | age = 10 }"
            , expectedType = "{ name : String, age : Int, generic : List a }"
            , topLevelDeclarations = """person : { name : String, age : Int, generic : List a }
person = someThing"""
            }
        , fixTest "when value is a record update expression of a record where it changes a field with generic"
            { arguments = ""
            , value = "{ person | generic = 10 }"
            , expectedType = "Person"
            , topLevelDeclarations = """person : Person
person = someThing"""
            }
        , fixTest "when value is a let expression"
            { arguments = ""
            , value = "let _ = 1 in 1.0"
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
        , fixTestWithAdditionalErrors "when value is a let expression that uses something it declared but we don't need it for inferring"
            { arguments = ""
            , value = "let list = someThing       in someValue list"
            , expectedType = "Float"
            , topLevelDeclarations = """someValue : a -> Float
someValue = something"""
            }
            [ Review.Test.error
                { message = "Missing type annotation for `list`"
                , details = details
                , under = "list"
                }
                |> Review.Test.atExactly { start = { row = 5, column = 34 }, end = { row = 5, column = 38 } }
            ]
        , fixTest "when value is a let expression that uses something it declared whose type which we can re-use"
            { arguments = ""
            , value = """
                    let
                        list : Float
                        list = 1.0
                    in list"""
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
        , fixTestWithAdditionalErrors "when value is a let expression that uses something it declared whose type we need to infer"
            { arguments = ""
            , value = "let list = 1.0       in list"
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
            [ Review.Test.error
                { message = "Missing type annotation for `list`"
                , details = details
                , under = "list"
                }
                |> Review.Test.atExactly { start = { row = 4, column = 34 }, end = { row = 4, column = 38 } }
                |> Review.Test.whenFixed """module A exposing (..)

a = let
      hasNoTypeAnnotation  = let list : Float
                                 list = 1.0       in list
    in
    d
"""
            ]
        , Test.skip <|
            test "when value is a reference to a sibling's value" <|
                \_ ->
                    """module A exposing (..)
a = let
      thing : Int
      thing = 1
      hasNoTypeAnnotation = thing
    in
    d
"""
                        |> Review.Test.runWithProjectData project rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
a = let
      thing : Int
      thing = 1
      hasNoTypeAnnotation : Int
      hasNoTypeAnnotation = thing
    in
    d
"""
                            ]
        , Test.skip <|
            fixTestWithAdditionalErrors "when value is a let expression where the let expressions need to be inferred for the whole expression to be inferred too"
                { arguments = ""
                , value = """
                                    let
                                        list = thing
                                        thing = 1.0
                                    in list"""
                , expectedType = "Float"
                , topLevelDeclarations = ""
                }
                [ Review.Test.error
                    { message = "Missing type annotation for `list`"
                    , details = details
                    , under = "list"
                    }
                , Review.Test.error
                    { message = "Missing type annotation for `thing`"
                    , details = details
                    , under = "thing"
                    }
                ]
        , fixTest "when value is a case expression with a single case"
            { arguments = ""
            , value = """
                    case foo of
                      _ -> "abc"
                    """
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a case expression with multiple cases"
            { arguments = ""
            , value = """
                    case foo of
                      Foo -> "abc"
                      Bar -> "def"
                    """
            , expectedType = "String"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a case expression with multiple cases, where we can refine"
            { arguments = ""
            , value = """
                    case foo of
                      Foo -> 1
                      Bar -> 1.0
                    """
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
        , noFixTest "when value is a case expression with multiple cases, where one can be refined further and another is unknown"
            { arguments = ""
            , value = """
                     case foo of
                       Foo -> 1
                       Bar -> someThing
                     """
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a case expression with multiple cases, where one can be refined further and another is unknown, and a final is very precise"
            { arguments = ""
            , value = """
                      case foo of
                        Foo -> 1
                        Bar -> someThing
                        Baz -> 1.0
                      """
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is contained inside the custom type constructor (tuple)"
            { arguments = ""
            , value = """
                      case foo of
                        A value -> value
                      """
            , expectedType = "()"
            , topLevelDeclarations = "type Thing = A ()"
            }
        , fixTest "when value is contained inside the custom type constructor (string)"
            { arguments = ""
            , value = """
                      case foo of
                        A value -> value
                      """
            , expectedType = "String"
            , topLevelDeclarations = "type Thing = A String"
            }
        , fixTest "when value is contained inside the custom type constructor (tuple literal destructuring)"
            { arguments = ""
            , value = """
                      case foo of
                        A (str, float) -> (str, float)
                      """
            , expectedType = "( String, Float )"
            , topLevelDeclarations = "type Thing = A ( String, Float )"
            }
        , fixTest "when value is contained inside the custom type constructor (record literal destructuring)"
            { arguments = ""
            , value = """
                      case foo of
                        A { a, b } -> { thing = a, otherThing = b }
                      """
            , expectedType = "{ thing : A, otherThing : B }"
            , topLevelDeclarations = "type Thing = A { a : A, b : B }"
            }
        , fixTest "when value is destructuring a literal tuple inside a case expression"
            { arguments = ""
            , value = """
                      case ("abc", 1.0) of
                        (str, float) -> (float, str)
                      """
            , expectedType = "( Float, String )"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is destructuring a tuple inside a case expression"
            { arguments = ""
            , value = """
                      case foo of
                        (str, float) -> (float, str)
                      """
            , expectedType = "( Float, String )"
            , topLevelDeclarations = """
foo : ( String, Float )
foo = ( "abc", "float" )
"""
            }
        , fixTest "when value is returning the type itself which we have already inferred (unit)"
            { arguments = ""
            , value = """
                      case foo of
                        () -> foo
                      """
            , expectedType = "()"
            , topLevelDeclarations = ""
            }
        , Test.skip <|
            fixTest "when value is returning the type itself which we have already inferred through a different pattern"
                { arguments = ""
                , value = """
                      case foo of
                        () -> someThing
                        _ -> foo
                      """
                , expectedType = "()"
                , topLevelDeclarations = ""
                }
        , Test.skip <|
            fixTest "when value is returning the type itself which we have already inferred (string)"
                { arguments = ""
                , value = """
                      case foo of
                        "" -> foo
                        _ -> someThing
                      """
                , expectedType = "String"
                , topLevelDeclarations = ""
                }
        , Test.skip <|
            fixTest "when value is destructuring an alias inside a case expression"
                { arguments = ""
                , value = """
                      case foo of
                        (str, float) -> (str, float)
                      """
                , expectedType = "( String, Float )"
                , topLevelDeclarations = """
type alias Thing = ( String, Float )
foo : Thing
foo = ( "abc", "float" )
"""
                }
        , Test.skip <|
            fixTest "when value is contained inside the custom type constructor (tuple destructuring of reference)"
                { arguments = ""
                , value = """
                      case foo of
                        A (str, float) -> (str, float)
                      """
                , expectedType = "( String, Float )"
                , topLevelDeclarations = """
type alias Thing = ( String, Float )
type Thing = A TupleAlias
"""
                }
        , noFixTest "when value is contained inside the custom type constructor (generic: number)"
            { arguments = ""
            , value = """
                       case foo of
                         A value -> value
                       """
            , topLevelDeclarations = "type Thing = A number"
            }
        , noFixTest "when value is contained inside the custom type constructor (generic: comparable)"
            { arguments = ""
            , value = """
                       case foo of
                         A value -> value
                       """
            , topLevelDeclarations = "type Thing = A comparable"
            }
        , noFixTest "when value is contained inside the custom type constructor (generic: appendable)"
            { arguments = ""
            , value = """
                       case foo of
                         A value -> value
                       """
            , topLevelDeclarations = "type Thing = A appendable"
            }
        , noFixTest "when value is contained inside the custom type constructor (generic: compappendable)"
            { arguments = ""
            , value = """
                       case foo of
                         A value -> value
                       """
            , topLevelDeclarations = "type Thing = A compappendable"
            }
        , noFixTest "when value is contained inside the custom type constructor but is a generic contained in the type itself"
            { arguments = ""
            , value = """
                       case foo of
                         A value -> value
                       """
            , topLevelDeclarations = "type Thing a = A a"
            }
        , fixTest "when value is an operator function"
            { arguments = ""
            , value = "(+)"
            , expectedType = "number -> number -> number"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a partially applied operator function"
            { arguments = ""
            , value = "(/) 1"
            , expectedType = "Float -> Float"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a binary operation"
            { arguments = ""
            , value = "14 / 4"
            , expectedType = "Float"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a function application from Basics (not)"
            { arguments = ""
            , value = "not True"
            , expectedType = "Bool"
            , topLevelDeclarations = ""
            }
        , fixTest "when value is a function application from Basics (round)"
            { arguments = ""
            , value = "round 1.0"
            , expectedType = "Int"
            , topLevelDeclarations = ""
            }
        , test "when value is a function from a different local module" <|
            \_ ->
                [ """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation = B.someThing
    in
    d
""", """module B exposing (..)
someThing : Int
someThing = 1
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation : Int
      hasNoTypeAnnotation = B.someThing
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value is a type from a different local module" <|
            \_ ->
                [ """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation = B.someThing
    in
    d
""", """module B exposing (..)
type B_Type = B_Type
someThing : B_Type
someThing = B_Type
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation : B.B_Type
      hasNoTypeAnnotation = B.someThing
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value is a custom type constructor from a different local module" <|
            \_ ->
                [ """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation = B.B_Type
    in
    d
""", """module B exposing (..)
type B_Type = B_Type
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation : B.B_Type
      hasNoTypeAnnotation = B.B_Type
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value is a type alias constructor from a different local module" <|
            \_ ->
                [ """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation = B.B_Type
    in
    d
""", """module B exposing (..)
type alias B_Type = {}
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation : B.B_Type
      hasNoTypeAnnotation = B.B_Type
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value is a port from a different local module" <|
            \_ ->
                [ """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation = B.output
    in
    d
""", """module B exposing (..)
type Msg = Msg
port output : String -> Cmd Msg
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation : String -> Cmd B.Msg
      hasNoTypeAnnotation = B.output
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value is a type from a different local module, when module is aliased" <|
            \_ ->
                [ """module A exposing (..)
import B as Not_B
a = let
      hasNoTypeAnnotation = Not_B.someThing
    in
    d
""", """module B exposing (..)
type B_Type = B_Type
someThing : B_Type
someThing = B_Type
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B as Not_B
a = let
      hasNoTypeAnnotation : Not_B.B_Type
      hasNoTypeAnnotation = Not_B.someThing
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value is a type from a different local module, when type is exposed explicitly" <|
            \_ ->
                [ """module A exposing (..)
import B exposing (B_Type)
a = let
      hasNoTypeAnnotation = B.someThing
    in
    d
""", """module B exposing (..)
type B_Type = B_Type
someThing : B_Type
someThing = B_Type
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B exposing (B_Type)
a = let
      hasNoTypeAnnotation : B_Type
      hasNoTypeAnnotation = B.someThing
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value is a type from a different local module, when type is exposed explicitly but a local type (alias) with the same name exists" <|
            \_ ->
                [ """module A exposing (..)
import B exposing (B_Type)
type alias B_Type = {}
a = let
      hasNoTypeAnnotation = B.someThing
    in
    d
""", """module B exposing (..)
type B_Type = B_Type
someThing : B_Type
someThing = B_Type
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B exposing (B_Type)
type alias B_Type = {}
a = let
      hasNoTypeAnnotation : B.B_Type
      hasNoTypeAnnotation = B.someThing
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value is a type from a different local module, when type is exposed implicitly but a local (custom) type with the same name exists" <|
            \_ ->
                [ """module A exposing (..)
import B exposing (..)
type B_Type = A
a = let
      hasNoTypeAnnotation = B.someThing
    in
    d
""", """module B exposing (..)
type B_Type = B_Type
someThing : B_Type
someThing = B_Type
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B exposing (..)
type B_Type = A
a = let
      hasNoTypeAnnotation : B.B_Type
      hasNoTypeAnnotation = B.someThing
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value references another module's function which references a type imported through the prelude (List)" <|
            \_ ->
                [ """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation = B.thing
    in
    d
""", """module B exposing (..)
thing : List Int
thing =
    []
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation : List Int
      hasNoTypeAnnotation = B.thing
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value references another module's function which references a type imported through the prelude (Maybe)" <|
            \_ ->
                [ """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation = B.thing
    in
    d
""", """module B exposing (..)
thing : Maybe ()
thing =
    Nothing
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation : Maybe ()
      hasNoTypeAnnotation = B.thing
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value references another module's function which references another module's type" <|
            \_ ->
                [ """module A exposing (..)
import B
import Set
a = let
      hasNoTypeAnnotation = B.thing
    in
    d
""", """module B exposing (..)
import Set
thing : Set.Set String
thing =
    Set.empty
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B
import Set
a = let
      hasNoTypeAnnotation : Set.Set String
      hasNoTypeAnnotation = B.thing
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value references another module's function which references another module's type that isn't imported" <|
            \_ ->
                [ """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation = B.thing
    in
    d
""", """module B exposing (..)
import Set
thing : Set.Set String
thing =
    Set.empty
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B
import Set
a = let
      hasNoTypeAnnotation : Set.Set String
      hasNoTypeAnnotation = B.thing
    in
    d
"""
                            ]
                          )
                        ]
        , test "when value references another module's function which references a local type" <|
            \_ ->
                [ """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation = B.thing
    in
    d
""", """module B exposing (..)
type Thing = Thing
thing : Thing
thing = Thing
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B
a = let
      hasNoTypeAnnotation : B.Thing
      hasNoTypeAnnotation = B.thing
    in
    d
"""
                            ]
                          )
                        ]
        , noFixTest "should not provide a fix (for now) when type variables are found both in the input parameters and output parameters"
            { arguments = ""
            , value = "someValue string"
            , topLevelDeclarations = """someValue : a -> a
someValue = something"""
            }
        , noFixTest "should not provide a fix (for now) when function has arguments"
            { arguments = "thing"
            , value = "someValue thing"
            , topLevelDeclarations = """someValue : String -> Int
someValue = something"""
            }
        ]


fixTest : String -> { arguments : String, value : String, expectedType : String, topLevelDeclarations : String } -> Test
fixTest title { arguments, value, expectedType, topLevelDeclarations } =
    test title <|
        \_ ->
            ("""module A exposing (..)
""" ++ topLevelDeclarations ++ """
a = let
      hasNoTypeAnnotation """ ++ arguments ++ """ = """ ++ value ++ """
    in
    d
""")
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Missing type annotation for `hasNoTypeAnnotation`"
                        , details = details
                        , under = "hasNoTypeAnnotation"
                        }
                        |> Review.Test.whenFixed ("""module A exposing (..)
""" ++ topLevelDeclarations ++ """
a = let
      hasNoTypeAnnotation : """ ++ expectedType ++ """
      hasNoTypeAnnotation """ ++ arguments ++ """ = """ ++ value ++ """
    in
    d
""")
                    ]


fixTestWithAdditionalErrors : String -> { arguments : String, value : String, expectedType : String, topLevelDeclarations : String } -> List Review.Test.ExpectedError -> Test
fixTestWithAdditionalErrors title { arguments, value, expectedType, topLevelDeclarations } additionalErrors =
    test title <|
        \_ ->
            ("""module A exposing (..)
""" ++ topLevelDeclarations ++ """
a = let
      hasNoTypeAnnotation """ ++ arguments ++ """ = """ ++ value ++ """
    in
    d
""")
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expectErrors
                    ((Review.Test.error
                        { message = "Missing type annotation for `hasNoTypeAnnotation`"
                        , details = details
                        , under = "hasNoTypeAnnotation"
                        }
                        |> Review.Test.whenFixed ("""module A exposing (..)
""" ++ topLevelDeclarations ++ """
a = let
      hasNoTypeAnnotation : """ ++ expectedType ++ """
      hasNoTypeAnnotation """ ++ arguments ++ """ = """ ++ value ++ """
    in
    d
""")
                     )
                        :: additionalErrors
                    )


noFixTest : String -> { arguments : String, value : String, topLevelDeclarations : String } -> Test
noFixTest title { arguments, value, topLevelDeclarations } =
    test title <|
        \_ ->
            ("""module A exposing (..)
""" ++ topLevelDeclarations ++ """
a = let
      hasNoTypeAnnotation """ ++ arguments ++ """ = """ ++ value ++ """
    in
    d
""")
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Missing type annotation for `hasNoTypeAnnotation`"
                        , details = details
                        , under = "hasNoTypeAnnotation"
                        }
                    ]


project : Project
project =
    Project.new
        |> Project.addDependency Dependencies.ElmCore.dependency
