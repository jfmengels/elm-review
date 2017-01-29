module Lint.Runner.Node exposing (..)

{-| Run the tests and render the results as a Web page.
-}


run : AppOptions msg model -> Test -> Program Value (Model msg model) (Msg msg)
run appOpts test =
    let
        init args =
            let
                cmd =
                    Task.perform Init Time.now

                initArgs : ( Maybe Int, Reporter.Report )
                initArgs =
                    case ( decodeInitArgs args, seed ) of
                        -- ( decodeValue (nullable intFromString) maybeInitialSeed, seed ) of
                        -- The --seed argument didn't decode
                        ( Err str, _ ) ->
                            Debug.crash ("Invalid --seed argument: " ++ str)

                        -- The user provided both a --seed flag and a seed from Elm
                        ( Ok ( Just fromCli, report ), Just fromElm ) ->
                            if fromCli == fromElm then
                                -- If they were the same, then that's no problem.
                                ( seed, report )
                            else
                                -- If they were different, crash. We don't know which to use.
                                Debug.crash ("Received both a --seed flag (" ++ toString fromCli ++ ") and a runner option seed (" ++ toString fromElm ++ "). Which initial seed did you mean to use?")

                        -- User passed --seed but not an Elm arg
                        ( Ok ( Just fromCli, report ), Nothing ) ->
                            ( Just fromCli, report )

                        -- User passed an Elm arg but not --seed
                        ( Ok ( Nothing, report ), Just fromElm ) ->
                            ( seed, report )

                        -- User passed neither --seed nor an Elm arg
                        ( Ok ( Nothing, report ), Nothing ) ->
                            ( Nothing, report )
            in
                ( Uninitialized appOpts.update
                    { maybeInitialSeed = Tuple.first initArgs
                    , report = Tuple.second initArgs
                    , runs = runs
                    , test = test
                    , init = appOpts.init
                    }
                , cmd
                )
    in
        Platform.programWithFlags
            { init = init
            , update = initOrUpdate
            , subscriptions = subscriptions appOpts.subscriptions
            }


{-| Run the linting and report the results.
-}
runLint : Emitter Msg -> Test -> TestProgram
runLint emit =
    run
        { init = init emit
        , update = update emit
        , subscriptions = \_ -> Sub.none
        }
