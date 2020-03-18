module ReadmeStartsWithProjectTitle exposing (rule)

import Elm.Package
import Elm.Project
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newProjectRuleSchema "ReadmeStartsWithProjectTitle"
        { moduleVisitor = moduleVisitor
        , initProjectContext = initProjectContext
        , fromProjectToModule = fromProjectToModule
        , fromModuleToProject = fromModuleToProject
        , foldProjectContexts = foldProjectContexts
        }
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withReadmeProjectVisitor readmeVisitor
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor (\_ context -> ( [], context ))


type alias ProjectContext =
    { projectTitle : Maybe String
    }


initProjectContext : ProjectContext
initProjectContext =
    { projectTitle = Nothing
    }


type alias ModuleContext =
    ProjectContext


fromProjectToModule : Rule.ModuleKey -> a -> ProjectContext -> ModuleContext
fromProjectToModule _ _ projectContext =
    projectContext


fromModuleToProject : Rule.ModuleKey -> a -> ModuleContext -> ProjectContext
fromModuleToProject _ _ moduleContext =
    moduleContext


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts _ previousContext =
    previousContext



-- elm.json VISITOR


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ProjectContext
elmJsonVisitor maybeProject projectContext =
    case maybeProject |> Maybe.map .project of
        Just (Elm.Project.Package pkg) ->
            { projectTitle = Just <| Elm.Package.toString pkg.name }

        _ ->
            projectContext



-- README VISITOR


readmeVisitor : Maybe { readmeKey : Rule.ReadmeKey, content : String } -> ProjectContext -> ( List Error, ProjectContext )
readmeVisitor maybeReadme context =
    case ( maybeReadme, context.projectTitle ) of
        ( Just { readmeKey, content }, Just projectName ) ->
            if String.startsWith ("# " ++ projectName) content then
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
