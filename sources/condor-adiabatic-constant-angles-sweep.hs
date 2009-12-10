-- @+leo-ver=4-thin
-- @+node:gcross.20091205211300.2400:@thin condor-adiabatic-random-angles-sweep.hs
-- @@language haskell

import Control.Monad

import System.Process

import Text.Printf

simulation_parameters :: [(Double,Int)]
simulation_parameters =
    [(0.0,240)
    ,(0.5,240)
    ,(1.0,240)
    ,(0.0,320)
    ,(0.5,320)
    ,(1.0,320)
    ,(0.0,640)
    ,(0.5,640)
    ,(1.0,640)
    ]

header =
    unlines
        ["Executable = programs/simulate-adiabatic-constant-angle"
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
        concatMap (\(angle,number_of_sites) ->
            [printf "Arguments = %f %i" angle number_of_sites
            ,printf "Error = logs/err/adiabatic-constant-%f-%i.$(cluster)" angle number_of_sites
            ,printf "Output = logs/out/adiabatic-constant-%f-%i.$(cluster)" angle number_of_sites
            ,printf "Log = logs/log/adiabatic-constant-%f-%i.$(cluster)" angle number_of_sites
            ,"Queue"
            ,""
            ]
        )
        $
        simulation_parameters
-- @-node:gcross.20091205211300.2400:@thin condor-adiabatic-random-angles-sweep.hs
-- @-leo
