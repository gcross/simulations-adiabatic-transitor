-- @+leo-ver=4-thin
-- @+node:gcross.20091205211300.2400:@thin condor-adiabatic-random-angles-sweep.hs
-- @@language haskell

import Control.Monad

import System.Process

simulation_parameters =
    [(2,20)
    ]

header =
    unlines
        ["Executable = simulate-adiabatic-random-angles"
        ,"Universe = vanilla"
        ,"Error  = logs/err.$(cluster)"
        ,"Output = logs/out.$(cluster)"
        ,"Log    = logs/log.$(cluster)"
        ,""
        ,"Environment = \"LD_LIBRARY_PATH=/phys/users/gcross/local/lib\""
        ,""
        ]

main = do
    readProcess "condor_submit" []
        .
        (header ++)
        .
        unlines
        .
        concatMap (\(number_of_runs,number_of_sites) ->
            ["Arguments = " ++ show number_of_runs ++ " " ++ show number_of_sites
            ,"Queue"
            ,""
            ]
        )
        $
        simulation_parameters
-- @-node:gcross.20091205211300.2400:@thin condor-adiabatic-random-angles-sweep.hs
-- @-leo
