module Review.WorkList exposing
    ( WorkList
    , fromSortedModules
    , touchedElmJson
    , touchedExtraFiles
    , touchedReadme
    , visitedDependencies
    , visitedElmJson
    , visitedExtraFiles
    , visitedReadme
    )

import Review.FilePath exposing (FilePath)


type alias WorkList =
    { elmJson : Bool
    , readme : Bool
    , extraFiles : Bool
    , dependencies : Bool
    , modules : List FilePath
    }


fromSortedModules : List FilePath -> WorkList
fromSortedModules sortedModules =
    { elmJson = True
    , readme = True
    , extraFiles = True
    , dependencies = True
    , modules = sortedModules
    }


visitedElmJson : WorkList -> WorkList
visitedElmJson workList =
    { elmJson = False
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , modules = workList.modules
    }


visitedReadme : WorkList -> WorkList
visitedReadme workList =
    { elmJson = workList.elmJson
    , readme = False
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , modules = workList.modules
    }


visitedExtraFiles : WorkList -> WorkList
visitedExtraFiles workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = False
    , dependencies = workList.dependencies
    , modules = workList.modules
    }


visitedDependencies : WorkList -> WorkList
visitedDependencies workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = False
    , modules = workList.modules
    }


touchedElmJson : WorkList -> WorkList
touchedElmJson workList =
    { elmJson = True
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , modules = workList.modules
    }


touchedReadme : WorkList -> WorkList
touchedReadme workList =
    { elmJson = workList.elmJson
    , readme = True
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , modules = workList.modules
    }


touchedExtraFiles : WorkList -> WorkList
touchedExtraFiles workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = True
    , dependencies = workList.dependencies
    , modules = workList.modules
    }
