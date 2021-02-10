module NoUnnecessaryTrailingUnderscoreTest exposing (all)

import NoUnnecessaryTrailingUnderscore exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


details : List String
details =
    [ "It looks like this was used to avoid a shadowing issue, but the variable it would have clashed with is not present in the scope of where this variable was declared anymore. You should rename the variable and remove the underscore."
    , "Note that this may not be a safe change, in that renaming may clash with a value declared deeper in the expression, but I think it's less confusing to have the nested variable have a trailing underscore rather than the variable declared higher-up."
    ]


messageForTopLevel : String
messageForTopLevel =
    "Top-level declaration names should not end with an underscore"


detailsForTopLevel : List String
detailsForTopLevel =
    [ "A trailing underscore \"_\" is often used to prevent shadowing issues, but top-level declarations should not resolve these issues in that manner."
    ]


all : Test
all =
    describe "NoUnnecessaryTrailingUnderscore"
        [ test "should report an error when argument has unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a value_ = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value_ should not end with an underscore"
                            , details = details
                            , under = "value_"
                            }
                        ]
        , test "should not report an error when argument does not have a trailing _" <|
            \() ->
                """module A exposing (..)
a value va_lue = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when another argument with the same name without the _ exists" <|
            \() ->
                """module A exposing (..)
a value_ value = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the name without _ is a reserved keyword" <|
            \() ->
                """module A exposing (..)
a if_ then_ else_ case_ of_ let_ in_ type_ module_ where_ import_ exposing_ as_ port_ = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when a top-level variable has a trailing _" <|
            \() ->
                """module A exposing (..)
a_ = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = messageForTopLevel
                            , details = detailsForTopLevel
                            , under = "a_"
                            }
                        ]
        , test "should report an error when a top-level variable has a trailing _, even if it would clash" <|
            \() ->
                """module A exposing (..)
a = 1
a_ = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = messageForTopLevel
                            , details = detailsForTopLevel
                            , under = "a_"
                            }
                        ]
        , test "should not report an error when a top-level variable has a trailing _ whose name is like a reserved keyword" <|
            \() ->
                """module A exposing (..)
if_ = 1
then_ = 1
else_ = 1
case_ = 1
of_ = 1
let_ = 1
in_ = 1
type_ = 1
module_ = 1
where_ = 1
import_ = 1
exposing_ = 1
as_ = 1
port_ = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when argument in parens has unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a (value_) = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value_ should not end with an underscore"
                            , details = details
                            , under = "value_"
                            }
                        ]
        , test "should report an error when arguments in alias has unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a (value1_ as value2_) = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value1_ should not end with an underscore"
                            , details = details
                            , under = "value1_"
                            }
                        , Review.Test.error
                            { message = "value2_ should not end with an underscore"
                            , details = details
                            , under = "value2_"
                            }
                        ]
        , test "should report an error when arguments in tuple has unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a (value1_, value2_) = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value1_ should not end with an underscore"
                            , details = details
                            , under = "value1_"
                            }
                        , Review.Test.error
                            { message = "value2_ should not end with an underscore"
                            , details = details
                            , under = "value2_"
                            }
                        ]
        , test "should report an error when arguments in uncons pattern has unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a (Foo value_) = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value_ should not end with an underscore"
                            , details = details
                            , under = "value_"
                            }
                        ]
        , test "should report an error when variables from case expression patterns have unnecessary trailing _" <|
            \() ->
                """module A exposing (..)
a =
  case b of
    value_ -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value_ should not end with an underscore"
                            , details = details
                            , under = "value_"
                            }
                        ]
        , test "should report an error for uncons patterns" <|
            \() ->
                """module A exposing (..)
a =
  case b of
    value1_ :: value2_ -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value1_ should not end with an underscore"
                            , details = details
                            , under = "value1_"
                            }
                        , Review.Test.error
                            { message = "value2_ should not end with an underscore"
                            , details = details
                            , under = "value2_"
                            }
                        ]
        , test "should report an error for list patterns" <|
            \() ->
                """module A exposing (..)
a =
  case b of
    [value1_ , value2_] -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value1_ should not end with an underscore"
                            , details = details
                            , under = "value1_"
                            }
                        , Review.Test.error
                            { message = "value2_ should not end with an underscore"
                            , details = details
                            , under = "value2_"
                            }
                        ]
        , test "should not report an error if a top-level function is already named without the _" <|
            \() ->
                """module A exposing (..)
a value_ = 1

value = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error if a argument is already named without the _" <|
            \() ->
                """module A exposing (..)
a value =
    case b of
      value_ -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error if a argument is already named without the _ in a case declaration" <|
            \() ->
                """module A exposing (..)
a =
    case b of
      value -> 1
        case b of
          value_ -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not consider a name from a different case as in the scope of a case" <|
            \() ->
                """module A exposing (..)
a =
    case b of
      value -> 1
      () ->
        case b of
          value_ -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value_ should not end with an underscore"
                            , details = details
                            , under = "value_"
                            }
                        ]
        , test "should record names from record in scope" <|
            \() ->
                """module A exposing (..)
a =
    case b of
      {value} ->
        case b of
          value_ -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report record fields in patterns in case pattern" <|
            \() ->
                """module A exposing (..)
a =
    case b of
      {value_} -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report when another name without the _ is defined" <|
            \() ->
                """module A exposing (..)
a =
    case b of
      (value, value_) -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report record fields in patterns in function argument" <|
            \() ->
                """module A exposing (..)
a {value_} = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not consider arguments from other functions as in scope" <|
            \() ->
                """module A exposing (..)
a value = 1
b value_ = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value_ should not end with an underscore"
                            , details = details
                            , under = "value_"
                            }
                        ]
        , test "should report names from anonymous functions" <|
            \() ->
                """module A exposing (..)
a = \\value_ -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value_ should not end with an underscore"
                            , details = details
                            , under = "value_"
                            }
                        ]
        , test "should report names from let declaration functions" <|
            \() ->
                """module A exposing (..)
a = let value_ = 1
    in 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value_ should not end with an underscore"
                            , details = details
                            , under = "value_"
                            }
                        ]
        , test "should report names from let declaration patterns" <|
            \() ->
                """module A exposing (..)
a = let (Value value_) = 1
    in 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value_ should not end with an underscore"
                            , details = details
                            , under = "value_"
                            }
                        ]
        , test "should not report names from let declaration patterns when the pattern is a record pattern" <|
            \() ->
                """module A exposing (..)
a = let {value_} = 1
    in 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report arguments from let functions" <|
            \() ->
                """module A exposing (..)
a = let fn value_ = 1
    in 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value_ should not end with an underscore"
                            , details = details
                            , under = "value_"
                            }
                        ]
        , test "should not report function names from let expressions that would clash with others" <|
            \() ->
                """module A exposing (..)
a value =
    let value_ = 1
    in value_
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report function names from let expressions even if it would clash with a name on the same-level" <|
            \() ->
                """module A exposing (..)
a =
    let value = 1
        value_ = 1
    in 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value_ should not end with an underscore"
                            , details =
                                [ "It seems that it has been used to prevent shadowing issues with a variable on the same level, but this is confusing. When should \"value_\" be used and when should \"value\" be used?"
                                , "Please rename this variable in a way that makes it more explicit when or how each should be used."
                                ]
                            , under = "value_"
                            }
                        ]
        , test "should not report names in a let body when name would clash with a let declaration value" <|
            \() ->
                """module A exposing (..)
a =
    let value = 1
    in
      let value_ = 1
      in 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report names in a let declaration body when name would clash with let declaration value" <|
            \() ->
                """module A exposing (..)
a =
    let
        value = 1
        value2 =
          let value_ = 1
          in 1
    in
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
