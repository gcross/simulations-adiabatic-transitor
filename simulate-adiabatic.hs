-- @+leo-ver=4-thin
-- @+node:gcross.20091120111528.1233:@thin simulate-adiabatic.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091120111528.1238:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20091120111528.1238:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091120111528.1235:<< Import needed modules >>
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.ConfigFile
import Data.IORef
import Data.UUID

import Database.Enumerator
import Database.PostgreSQL.Enumerator

import System
import System.Posix.Clock

import Text.Printf

import VMPS.Algorithms
import VMPS.EnergyMinimizationChain
import VMPS.OperatorConstruction
import VMPS.Pauli
import VMPS.States
import VMPS.Tensors

import Database
-- @-node:gcross.20091120111528.1235:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091120111528.1234:Operator tensors
-- @+at
--  rightmost is ZX
--  
--  X on first, none on last
--  on all others, must be symmetric
--  then s = 1/2
--  
-- @-at
-- @@c

makeModelOperatorSiteTensors :: Double -> Int -> [OperatorSiteTensor]
makeModelOperatorSiteTensors s =
    makeModelWithSpecialEndpointsOperatorSiteTensors
        4
        [(1 --> 1) 1.0 I
        ,(1 --> 2) 1.0 Z
        ,(1 --> 4) (-s) X
        ]
        [(1 --> 1) 1.0 I
        ,(1 --> 2) 1.0 Z
        ,(2 --> 3) (-(1-s)) X
        ,(3 --> 4) 1.0 Z
        ,(1 --> 4) (-s) X
        ,(4 --> 4) 1.0 I
        ]
        [(2 --> 1) (-(1-s)) X
        ,(3 --> 1) 1.0 Z
        ,(4 --> 1) 1.0 I
        ]
-- @-node:gcross.20091120111528.1234:Operator tensors
-- @+node:gcross.20091120111528.1237:analyzeTrialEnergies
data TrialAnalysis = TrialDidBetter | TrialDidWorse | TrialDidTheSame

analyzeTrialEnergies tolerance best_energy trial_energy
    | best_energy - trial_energy > tolerance = TrialDidBetter
    | trial_energy - best_energy > tolerance = TrialDidWorse
    | otherwise = TrialDidTheSame
-- @-node:gcross.20091120111528.1237:analyzeTrialEnergies
-- @+node:gcross.20091120111528.1236:main
main = do
    args <- getArgs

    let number_of_sites = read $ args !! 0
        s = 0.5
        operator_site_tensors = makeModelOperatorSiteTensors s number_of_sites
        number_of_trials_needed = 3
        bandwidth_increment = 5
        initial_bandwidth = 2
        number_of_levels = 3
        bandwidth_increase_energy_change_convergence_criterion = 1e-4
        multisweep_energy_change_convergence_criterion = 1e-5

    connection <- makeConnection "vmps"

    next_bandwidth_ref <- newIORef initial_bandwidth
    level_number_ref <- newIORef 1

    let getHeading = liftM (printf "LEVEL %i: ") (readIORef level_number_ref :: IO Int)
        callback_to_decide_whether_to_declare_victory_with_trial chain = do
            heading <- getHeading
            putStrLn $ heading ++ " energy = " ++ (show . chainEnergy $ chain)
            level_number <- readIORef level_number_ref
            let new_level_number = level_number + 1
            putStrLn $ printf "Now starting on level %i... (bandwidth=2 sweeps will not be displayed)" new_level_number
            writeIORef level_number_ref new_level_number
            alwaysDeclareVictory chain
        callback_to_increase_bandwidth chain = do
            next_bandwidth <- readIORef next_bandwidth_ref
            writeIORef next_bandwidth_ref (next_bandwidth+bandwidth_increment)
            increaseChainBandwidth 2 next_bandwidth chain
        callback_after_each_sweep victory_flag latest_chain = do
            heading <- getHeading
            next_bandwidth <- readIORef next_bandwidth_ref
            let current_bandwidth = next_bandwidth-bandwidth_increment
            unless (current_bandwidth <= 2) $
                putStrLn $ heading ++ (printf "bandwidth = %i, sweep energy = %f" current_bandwidth (chainEnergy latest_chain)        )

    (energies :: [Double],states :: [CanonicalStateRepresentation],_) <- fmap unzip3 $
        solveForMultipleLevelsWithCallbacks
            callback_to_decide_whether_to_declare_victory_with_trial
            (newChainCreator
                (writeIORef next_bandwidth_ref (initial_bandwidth+bandwidth_increment))
                operator_site_tensors
                2 initial_bandwidth
            )
            callback_to_increase_bandwidth
            callback_after_each_sweep
            ignoreSiteCallback
            bandwidth_increase_energy_change_convergence_criterion
            multisweep_energy_change_convergence_criterion
            0
            1000
            number_of_levels
            []
    putStrLn ""
    putStrLn "The energy levels are:"
    forM_ energies $ \energy -> do
        putStr "\t"
        putStrLn . show $ energy

    solution_ids <-
        withSession connection $
            withTransaction ReadCommitted $
                storeSolution (zip energies states)
    putStrLn solution_ids

    getTime ProcessCPUTime >>= putStrLn . show
-- @-node:gcross.20091120111528.1236:main
-- @-others
-- @-node:gcross.20091120111528.1233:@thin simulate-adiabatic.hs
-- @-leo
