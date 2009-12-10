-- @+leo-ver=4-thin
-- @+node:gcross.20091209165001.1330:@thin condor-adiabatic-restricted-random-angles-sweep.hs
-- @@language haskell

import Control.Monad

import System.Process

import Text.Printf

simulation_parameters :: [(Int,Int)]
simulation_parameters = concat
    [replicate  10 (10, 10)
    ,replicate  20 ( 5, 20)
    ,replicate  50 ( 2, 40)
    ,replicate 100 ( 1, 80)
    ,replicate  40 ( 1,160)
    ,replicate  20 ( 1,240)
    ]


header =
    unlines
        ["Executable = programs/simulate-adiabatic-restricted-random-angles"
        ,"Universe = vanilla"
        ,""
        ,"Environment = \"LD_LIBRARY_PATH=/phys/users/gcross/local/lib:/phys/users/gcross/local/lib64\""
        ,""
        ]

main = do
    readProcess "condor_submit" []
        .
        (header ++)
        .
        unlines
        .
        concat
        .
        zipWith (\job_number (number_of_runs,number_of_sites) ->
            ["Arguments = " ++ show number_of_runs ++ " " ++ show number_of_sites
            ,printf "Error = logs/err/adiabatic-restricted-random-%i.$(cluster).%03i" number_of_sites job_number
            ,printf "Output = logs/out/adiabatic-restricted-random-%i.$(cluster).%03i" number_of_sites job_number
            ,printf "Log = logs/log/adiabatic-restricted-random-%i.$(cluster).%03i" number_of_sites job_number
            ,"Queue"
            ,""
            ]
        ) [0 :: Int ..]
        $
        simulation_parameters
-- @-node:gcross.20091209165001.1330:@thin condor-adiabatic-restricted-random-angles-sweep.hs
-- @-leo
