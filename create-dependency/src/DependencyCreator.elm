port module DependencyCreator exposing (main)

import Elm.Constraint
import Elm.Docs
import Elm.License
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type
import Elm.Version
import Json.Decode as Decode
import Review.Project exposing (elmJson)
import Review.Project.Dependency as Dependency exposing (Dependency)


type alias Flags =
    { elmJson : String
    , docsJson : String
    }


main : Program Flags () ()
main =
    Platform.worker
        { init = \flags -> ( (), sendToJs (parseThings flags) )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


parseThings : Flags -> String
parseThings flags =
    let
        elmJson : Result String Elm.Project.PackageInfo
        elmJson =
            Decode.decodeString Elm.Project.decoder flags.elmJson
                |> Result.mapError (\err -> "Problem parsing elm.json: " ++ Debug.toString err)
                |> Result.andThen
                    (\elmJson_ ->
                        case elmJson_ of
                            Elm.Project.Application _ ->
                                Err "elm.json is for an application, not a project."

                            Elm.Project.Package package ->
                                Ok package
                    )

        docsJson : Result String (List Elm.Docs.Module)
        docsJson =
            Decode.decodeString (Decode.list Elm.Docs.decoder) flags.docsJson
                |> Result.mapError (\err -> "Problem parsing docs.json: " ++ Debug.toString err)
    in
    case Result.map2 formatFile elmJson docsJson of
        Ok str ->
            str

        Err error ->
            error


formatAlias alias_ =
    "{ args = " ++ listOfThings stringify alias_.args ++ """
    , comment = """ ++ stringify alias_.comment ++ """
    , name = """ ++ stringify alias_.name ++ """
    , tipe = Elm.Type.Record [ ( "row", Elm.Type.Type "Basics.Int" [] ), ( "col", Elm.Type.Type "Basics.Int" [] ), ( "problem", Elm.Type.Type "Parser.Problem" [] ) ] Nothing
    }"""


stringify : String -> String
stringify s =
    "\"\"\"" ++ String.replace "\"\"\"" """\\"\\"\\""" s ++ "\"\"\""


listOfThings : (a -> String) -> List a -> String
listOfThings mapper list =
    "[ " ++ String.join "\n    , " (List.map mapper list) ++ " ]"


formatModule mod =
    "{ name = " ++ stringify mod.name ++ """
    , comment = """ ++ stringify mod.comment ++ """
    , aliases = """ ++ listOfThings formatAlias mod.aliases ++ """
    , unions = []
    , binops = []
    , values = []
    }"""


formatDep ( name, constraint ) =
    "( unsafePackageName " ++ stringify (Elm.Package.toString name) ++ ", unsafeConstraint " ++ stringify (Elm.Constraint.toString constraint) ++ ")"


formatFile : Elm.Project.PackageInfo -> List Elm.Docs.Module -> String
formatFile elmJson docsJson =
    let
        listOfModuleNames list =
            list
                |> List.map (\name -> "unsafeModuleName " ++ stringify (Elm.Module.toString name))
                |> String.join ", "

        exposed =
            case elmJson.exposed of
                Elm.Project.ExposedList list ->
                    "Elm.Project.ExposedList [ " ++ listOfModuleNames list ++ " ]"

                Elm.Project.ExposedDict dict ->
                    "Elm.Project.ExposedDict [ " ++ String.join ", " (List.map (\( section, list ) -> "( \"" ++ section ++ "\", " ++ listOfModuleNames list ++ " ) ") dict) ++ " ]"

        moduleName =
            "Hello"

        dependencyModules =
            listOfThings formatModule docsJson
    in
    "module " ++ moduleName ++ """ exposing (dependency)

import Elm.Constraint
import Elm.Docs
import Elm.License
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type
import Elm.Version
import Review.Project.Dependency as Dependency exposing (Dependency)

dependency : Dependency
dependency =
    Dependency.create """ ++ stringify (Elm.Package.toString elmJson.name) ++ """
        elmJson
        dependencyModules


elmJson : Elm.Project.Project
elmJson =
    Elm.Project.Package
        { elm = unsafeConstraint """ ++ stringify (Elm.Constraint.toString elmJson.elm) ++ """
        , exposed = """ ++ exposed ++ """
        , license = Elm.License.fromString """ ++ stringify (Elm.License.toString elmJson.license) ++ """ |> Maybe.withDefault Elm.License.bsd3
        , name = unsafePackageName """ ++ stringify (Elm.Package.toString elmJson.name) ++ """
        , summary = """ ++ stringify elmJson.summary ++ """
        , deps = """ ++ listOfThings formatDep elmJson.deps ++ """
        , testDeps = """ ++ listOfThings formatDep elmJson.testDeps ++ """
        , version = Elm.Version.fromString """ ++ stringify (Elm.Version.toString elmJson.version) ++ """ |> Maybe.withDefault Elm.Version.one
        }

dependencyModules : List Elm.Docs.Module
dependencyModules =
    """ ++ dependencyModules ++ """

unsafePackageName : String -> Elm.Package.Name
unsafePackageName packageName =
    case Elm.Package.fromString packageName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafePackageName packageName
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity


unsafeModuleName : String -> Elm.Module.Name
unsafeModuleName moduleName =
    case Elm.Module.fromString moduleName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafeModuleName moduleName
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity


unsafeConstraint : String -> Elm.Constraint.Constraint
unsafeConstraint constraint =
    case Elm.Constraint.fromString constraint of
        Just constr ->
            constr

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafeConstraint constraint
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity
"""


formatPackageName : Elm.Package.Name -> String
formatPackageName packageName =
    "Elm.Package.fromString " ++ stringify (Elm.Package.toString packageName) ++ " |> Maybe.withDefault Elm.Package.one"


port sendToJs : String -> Cmd msg
