module Review.Rule3 exposing (ProjectContextCreator, ProjectRuleSchema, fromProjectSchema, newProjectSchema)

import Dict exposing (Dict)
import Elm.Project
import Review.Project.Dependency
import Review.Rule exposing (ElmJsonKey, Error, ReadmeKey)


type ProjectContextCreator projectContext
    = ProjectContextCreator projectContext


type Rule
    = Rule


type ProjectRuleSchema schemaState projectContext moduleContext
    = ProjectRuleSchema
        { name : String
        , projectContextCreator : ProjectContextCreator projectContext

        --, moduleVisitor : ModuleVisitorState projectContext moduleContext
        , elmJsonVisitors : List (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
        , readmeVisitors : List (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
        , dependenciesVisitors : List (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
        , finalEvaluationFns : List (projectContext -> List (Error {}))

        --, traversalType : TraversalType
        }


newProjectSchema : String -> ProjectContextCreator projectContext -> ProjectRuleSchema schemaState projectContext moduleContext
newProjectSchema name projectContextCreator =
    ProjectRuleSchema
        { name = name
        , projectContextCreator = projectContextCreator

        --, moduleVisitor : ModuleVisitorState projectContext moduleContext
        , elmJsonVisitors = []
        , readmeVisitors = []
        , dependenciesVisitors = []
        , finalEvaluationFns = []

        --, traversalType : TraversalType
        }


fromProjectSchema : ProjectRuleSchema schemaState projectContext moduleContext -> Rule
fromProjectSchema (ProjectRuleSchema projectRuleSchema) =
    Rule
