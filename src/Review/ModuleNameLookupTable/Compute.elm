module Review.ModuleNameLookupTable.Compute exposing (compute)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import NonEmpty exposing (NonEmpty)
import Review.ModuleNameLookupTable.Internal as ModuleNameLookupTableInternal exposing (ModuleNameLookupTable)
import Review.Project.Internal exposing (Project)
import Set exposing (Set)


compute : ModuleName -> Project -> ( ModuleNameLookupTable, Project )
compute moduleName project =
    let
        lookupTable : ModuleNameLookupTable
        lookupTable =
            ModuleNameLookupTableInternal.empty moduleName
    in
    ( lookupTable, project )


type alias ScopeProjectContext =
    { dependenciesModules : Dict String Elm.Docs.Module
    , modules : Dict ModuleName Elm.Docs.Module
    }


type alias ScopeModuleContext =
    { scopes : NonEmpty Scope
    , localTypes : Set String
    , importAliases : Dict String (List ModuleName)
    , importedFunctions : Dict String ModuleName
    , importedTypes : Dict String ModuleName
    , dependenciesModules : Dict String Elm.Docs.Module
    , modules : Dict ModuleName Elm.Docs.Module
    , exposesEverything : Bool
    , exposedNames : Dict String Range
    , exposedUnions : List Elm.Docs.Union
    , exposedAliases : List Elm.Docs.Alias
    , exposedValues : List Elm.Docs.Value
    , exposedBinops : List Elm.Docs.Binop
    , lookupTable : ModuleNameLookupTable
    }


type alias Scope =
    { names : Dict String VariableInfo
    , cases : List ( Node Expression, Dict String VariableInfo )
    , caseToExit : Node Expression
    }


type alias VariableInfo =
    { variableType : VariableType
    , node : Node String
    }


type VariableType
    = TopLevelVariable
    | CustomTypeConstructor
    | FunctionParameter
    | LetVariable
    | PatternVariable
    | Port


emptyScope : Scope
emptyScope =
    { names = Dict.empty
    , cases = []
    , caseToExit = Node Range.emptyRange (Expression.Literal "root")
    }
