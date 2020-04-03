module MiscRules.ReadmeStartsWithProjectTitle exposing (rule)

import Elm.Package
import Elm.Project
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newProjectRuleSchema "ReadmeStartsWithProjectTitle" initialProjectContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withReadmeProjectVisitor readmeVisitor
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { projectTitle : Maybe String
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { projectTitle = Nothing
    }



-- elm.json VISITOR


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject projectContext =
    case maybeProject |> Maybe.map .project of
        Just (Elm.Project.Package pkg) ->
            ( [], { projectTitle = Just <| Elm.Package.toString pkg.name } )

        _ ->
            ( [], projectContext )



-- README VISITOR


readmeVisitor : Maybe { readmeKey : Rule.ReadmeKey, content : String } -> ProjectContext -> ( List (Error scope), ProjectContext )
readmeVisitor maybeReadme context =
    case ( maybeReadme, context.projectTitle ) of
        ( Just { readmeKey, content }, Just projectName ) ->
            if not <| String.startsWith ("# " ++ projectName) content then
                let
                    range : Range
                    range =
                        { start = { row = 1, column = 1 }
                        , end =
                            { row = 1
                            , column =
                                String.lines content
                                    |> List.head
                                    |> Maybe.withDefault ""
                                    |> String.length
                                    |> (+) 1
                            }
                        }
                in
                ( [ Rule.errorForReadme readmeKey
                        { message = "TODO"
                        , details = [ "TODO" ]
                        }
                        range
                  ]
                , context
                )

            else
                ( [], context )

        _ ->
            ( [], context )
