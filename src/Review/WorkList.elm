module Review.WorkList exposing
    ( Step(..)
    , WorkList
    , computedFinalEvaluationDependencies
    , fromSortedModules
    , recomputeModules
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
import Vendor.Graph as Graph exposing (Graph)
import Vendor.IntSet as IntSet
import Vendor.Zipper exposing (Zipper)


type alias WorkList =
    { elmJson : Bool
    , readme : Bool
    , extraFiles : Bool
    , dependencies : Bool
    , touchedModules : Set FilePath
    , modules : List FilePath
    , finalEvaluation : Bool
    }


fromSortedModules : List FilePath -> WorkList
fromSortedModules sortedModules =
    { elmJson = True
    , readme = True
    , extraFiles = True
    , dependencies = True
    , touchedModules = Set.fromList sortedModules
    , modules = sortedModules
    , finalEvaluation = True
    }


recomputeModules : Graph FilePath -> List (Graph.NodeContext FilePath) -> WorkList -> WorkList
recomputeModules graph sortedModules workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = recomputeModulesHelp graph sortedModules workList.touchedModules []
    , finalEvaluation = workList.finalEvaluation
    }


recomputeModulesHelp : Graph FilePath -> List (Graph.NodeContext FilePath) -> Set FilePath -> List FilePath -> List FilePath
recomputeModulesHelp graph modules touchedModules acc =
    case modules of
        [] ->
            List.reverse acc

        mod :: rest ->
            recomputeModulesHelp
                graph
                rest
                touchedModules
                (if
                    Set.member mod.node.label touchedModules
                        || IntSet.foldl
                            (\key isMember ->
                                isMember
                                    || (case Graph.get key graph of
                                            Just importedModule ->
                                                Set.member importedModule.node.label touchedModules

                                            Nothing ->
                                                False
                                       )
                            )
                            False
                            mod.incoming
                 then
                    mod.node.label :: acc

                 else
                    acc
                )


visitedElmJson : WorkList -> WorkList
visitedElmJson workList =
    { elmJson = False
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    , finalEvaluation = workList.finalEvaluation
    }


visitedReadme : WorkList -> WorkList
visitedReadme workList =
    { elmJson = workList.elmJson
    , readme = False
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    , finalEvaluation = workList.finalEvaluation
    }


visitedExtraFiles : WorkList -> WorkList
visitedExtraFiles workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = False
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    , finalEvaluation = workList.finalEvaluation
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
            , finalEvaluation = workList.finalEvaluation
            }


visitedDependencies : WorkList -> WorkList
visitedDependencies workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = False
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    , finalEvaluation = workList.finalEvaluation
    }


computedFinalEvaluationDependencies : WorkList -> WorkList
computedFinalEvaluationDependencies workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    , finalEvaluation = False
    }


touchedElmJson : WorkList -> WorkList
touchedElmJson workList =
    { elmJson = True
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    , finalEvaluation = True
    }


touchedReadme : WorkList -> WorkList
touchedReadme workList =
    { elmJson = workList.elmJson
    , readme = True
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    , finalEvaluation = True
    }


touchedExtraFiles : WorkList -> WorkList
touchedExtraFiles workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = True
    , dependencies = workList.dependencies
    , touchedModules = workList.touchedModules
    , modules = workList.modules
    , finalEvaluation = True
    }


touchedModule : FilePath -> WorkList -> WorkList
touchedModule filePath workList =
    { elmJson = workList.elmJson
    , readme = workList.readme
    , extraFiles = workList.extraFiles
    , dependencies = workList.dependencies
    , touchedModules = Set.insert filePath workList.touchedModules
    , modules = workList.modules
    , finalEvaluation = True
    }
