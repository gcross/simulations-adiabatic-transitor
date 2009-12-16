-- @+leo-ver=4-thin
-- @+node:gcross.20091215183613.1294:@thin sweep-condor-adiabatic-interleave-random-angles.hs
-- @@language haskell

import Control.Monad

import System.Process

import Text.Printf

simulation_parameters :: [(Int,Int)]
simulation_parameters = concat
    [replicate  10 (10, 10)
    ,replicate  10 (10, 15)
    ,replicate  20 ( 5, 20)
    ,replicate  20 ( 5, 30)
    ,replicate  20 ( 5, 40)
    ,replicate 100 ( 1, 60)
    ,replicate 100 ( 1, 80)
    ,replicate 100 ( 1,100)
    ,replicate 100 ( 1,150)
    ,replicate 100 ( 1,200)
    ]


header =
    unlines
        ["Executable = programs/simulate-adiabatic-random-angles"
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
            ,"Arguments = " ++ show number_of_runs ++ " " ++ show number_of_sites
            ,printf "Error = logs/err/adiabatic-random-%i.$(cluster).%03i" number_of_sites job_number
            ,printf "Output = logs/out/adiabatic-random-%i.$(cluster).%03i" number_of_sites job_number
            ,printf "Log = logs/log/adiabatic-random-%i.$(cluster).%03i" number_of_sites job_number
            ,"Queue"
            ,""
            ]
        ) [0 :: Int ..]
        $
        simulation_parameters
-- @-node:gcross.20091215183613.1294:@thin sweep-condor-adiabatic-interleave-random-angles.hs
-- @-leo
