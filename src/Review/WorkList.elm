module Review.WorkList exposing
    ( WorkList
    , fromSortedModules
    , touchedElmJson
    , touchedExtraFiles
    , touchedModule
    , touchedReadme
    , visitedDependencies
    , visitedElmJson
    , visitedExtraFiles
    , visitedNextModule
    , visitedReadme
    )

import Review.FilePath exposing (FilePath)
import Set exposing (Set)


type alias WorkList =
    { elmJson : Bool
    , readme : Bool
    , extraFiles : Bool
    , dependencies : Bool
    , touchedModules : Set FilePath
    , modules : List FilePath
    }


fromSortedModules : List FilePath -> WorkList
fromSortedModules sortedModules =
    { elmJson = True
    , readme = True
    , extraFiles = True
    , dependencies = True
    , touchedModules = Set.fromList sortedModules
    , modules = sortedModules
    }


visitedElmJson : WorkList -> WorkList
visitedElmJson workList =
    { elmJson = False
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    }


visitedReadme : WorkList -> WorkList
visitedReadme workList =
    { elmJson = workList.elmJson
    , readme = False
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    }


visitedExtraFiles : WorkList -> WorkList
visitedExtraFiles workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = False
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    }


visitedNextModule : WorkList -> WorkList
visitedNextModule workList =
    case workList.modules of
        [] ->
            workList

        filePath :: restOfModules ->
            { elmJson = workList.elmJson
            , readme = workList.readme
            , extraFiles = workList.extraFiles
            , dependencies = workList.dependencies
            , touchedModules = Set.remove filePath workList.touchedModules
            , modules = restOfModules
            }


visitedDependencies : WorkList -> WorkList
visitedDependencies workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = False
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    }


touchedElmJson : WorkList -> WorkList
touchedElmJson workList =
    { elmJson = True
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    }


touchedReadme : WorkList -> WorkList
touchedReadme workList =
    { elmJson = workList.elmJson
    , readme = True
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    }


touchedExtraFiles : WorkList -> WorkList
touchedExtraFiles workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = True
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    }


touchedModule : FilePath -> WorkList -> WorkList
touchedModule filePath workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , touchedModules = Set.insert filePath workList.touchedModules
    , modules = workList.modules
    }
