module Review exposing (review)

{-| Module to configure your review configuration and run it on a source file.


# Reviewing

@docs review

-}

import Elm.Parser as Parser
import Elm.Processing exposing (init, process)
import Elm.Syntax.File exposing (File)
import Review.File exposing (ParsedFile, RawFile)
import Review.Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)



-- REVIEWING


{-| Review a list of files and gives back the errors raised by the given rules.

    import Review
    import Review.File exposing (ParsedFile)
    import Review.Project as Project exposing (Project)

    config : List Rule
    config =
        [ Some.Rule.rule
        , Some.Other.Rule.rule
        ]

    project : Project
    project =
        Project.new
            |> Project.withModule { path = "src/A.elm", source = "module A exposing (a)\na = 1" }
            |> Project.withModule { path = "src/B.elm", source = "module B exposing (b)\nb = 1" }

    doReview =
        let
            ( errors, rulesWithCachedValues ) =
                Review.review rules project
        in
        doSomethingWithTheseValues

-}
review : List Rule -> Project -> ( List Error, List Rule )
review rules project =
    let
        ( ruleErrors, rulesWithCache ) =
            Rule.runRules rules project
    in
    ( List.concat
        [ ruleErrors
        , project
            |> Review.Project.filesThatFailedToParse
            |> List.map Rule.parsingError
        ]
    , rulesWithCache
    )
