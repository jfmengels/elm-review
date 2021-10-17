module Review.Test.Dependencies.Unsafe exposing
    ( constraint
    , moduleName
    , packageName
    )

import Elm.Constraint
import Elm.Module
import Elm.Package


packageName : String -> Elm.Package.Name
packageName rawName =
    -- IGNORE TCO
    case Elm.Package.fromString rawName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            packageName rawName
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity


moduleName : String -> Elm.Module.Name
moduleName rawModuleName =
    -- IGNORE TCO
    case Elm.Module.fromString rawModuleName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            moduleName rawModuleName
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity


constraint : String -> Elm.Constraint.Constraint
constraint rawConstraint =
    -- IGNORE TCO
    case Elm.Constraint.fromString rawConstraint of
        Just constr ->
            constr

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            constraint rawConstraint
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity
