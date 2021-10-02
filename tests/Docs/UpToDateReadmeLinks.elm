module Docs.UpToDateReadmeLinks exposing (rule)

{-|

@docs rule

-}

import Docs.Utils.Link as Link exposing (Link)
import Elm.Package
import Elm.Project
import Elm.Syntax.Node exposing (Node(..))
import Elm.Version
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports links in the `README.md` that point to this project's package documentation on <https://package.elm-lang.org/>,
where the version is set to `latest` or a different version than the current version of the package.

🔧 Running with `--fix` will automatically remove all the reported errors.

    config =
        [ Docs.UpToDateReadmeLinks.rule
        ]

The problem with linking to `latest` is that if you release a new version later,
the users who read the README for the older version will be directed to a version
where the module/function/section you pointed to may not exist anymore.

This rule ensures that you always use the correct version in all of your releases,
and that you do not forget to update the links.

This rule provides automatic fixes, so you won't to do the tedious job of updating
the links yourself.

**NOTE**: Just make sure to run tests between bumping the version of the package
and publishing the package. Otherwise the link for a given version could link to a previous one.

**NOTE**: A similar rule would be useful for links inside the modules. I'll be working on that too!


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-documentation/example --rules Docs.UpToDateReadmeLinks
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "Docs.UpToDateReadmeLinks" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withReadmeProjectVisitor readmeVisitor
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    Maybe
        { projectName : String
        , version : String
        }


initialProjectContext : ProjectContext
initialProjectContext =
    Nothing



-- elm.json VISITOR


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject _ =
    case maybeProject |> Maybe.map .project of
        Just (Elm.Project.Package pkg) ->
            ( []
            , Just
                { projectName = Elm.Package.toString pkg.name
                , version = Elm.Version.toString pkg.version
                }
            )

        _ ->
            ( [], Nothing )



-- README VISITOR


readmeVisitor : Maybe { readmeKey : Rule.ReadmeKey, content : String } -> ProjectContext -> ( List (Error scope), ProjectContext )
readmeVisitor maybeReadme maybeContext =
    case ( maybeReadme, maybeContext ) of
        ( Just { readmeKey, content }, Just context ) ->
            ( reportErrorsForReadme context readmeKey content, maybeContext )

        _ ->
            ( [], maybeContext )


reportErrorsForReadme : { projectName : String, version : String } -> Rule.ReadmeKey -> String -> List (Error scope)
reportErrorsForReadme context readmeKey content =
    content
        |> Link.findLinks 0 []
        |> List.concatMap (reportError context readmeKey)


reportError : { projectName : String, version : String } -> Rule.ReadmeKey -> Node Link -> List (Error scope)
reportError context readmeKey (Node range link) =
    case link.file of
        Link.ModuleTarget moduleName ->
            [ Rule.errorForReadmeWithFix readmeKey
                { message = "Found relative link to a module in README"
                , details =
                    [ "Relative links to other modules from the README don't work when looking at the docs from GitHub or the likes."
                    , "I suggest to run elm-review --fix to change the link to an absolute link."
                    ]
                }
                range
                [ Fix.replaceRangeBy range <| "https://package.elm-lang.org/packages/" ++ context.projectName ++ "/" ++ context.version ++ "/" ++ String.join "-" moduleName ++ formatSlug link.slug ]
            ]

        Link.ReadmeTarget ->
            if link.startsWithDotSlash then
                [ Rule.errorForReadmeWithFix readmeKey
                    { message = "Found relative link from and to README"
                    , details =
                        [ "Links from and to README that start with \"./\" will not work on all places on GitHub or the likes."
                        , "I suggest to remove the leading \"./\"."
                        ]
                    }
                    range
                    (case link.slug of
                        Just slug ->
                            [ Fix.replaceRangeBy range ("#" ++ slug) ]

                        Nothing ->
                            []
                    )
                ]

            else
                []

        Link.PackagesTarget { name, version, subTarget } ->
            if context.projectName == name && context.version /= version then
                [ Rule.errorForReadmeWithFix readmeKey
                    { message = "Link does not point to the current version of the package"
                    , details = [ "I suggest to run elm-review --fix to get the correct link." ]
                    }
                    range
                    [ Fix.replaceRangeBy range <| "https://package.elm-lang.org/packages/" ++ name ++ "/" ++ context.version ++ "/" ++ formatSubTarget subTarget ++ formatSlug link.slug ]
                ]

            else
                []

        Link.External _ ->
            []


formatSubTarget : Link.SubTarget -> String
formatSubTarget subTarget =
    case subTarget of
        Link.ModuleSubTarget moduleName ->
            String.join "-" moduleName ++ "/"

        Link.ReadmeSubTarget ->
            ""


formatSlug : Maybe String -> String
formatSlug maybeSlug =
    case maybeSlug of
        Just slug ->
            "#" ++ slug

        Nothing ->
            ""
