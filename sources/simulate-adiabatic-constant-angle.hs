-- @+leo-ver=4-thin
-- @+node:gcross.20091120111528.1233:@thin simulate-adiabatic-constant-angle.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091120111528.1238:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20091120111528.1238:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091120111528.1235:<< Import needed modules >>
import Acme.Dont

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.Complex
import Data.ConfigFile
import Data.IORef
import Data.UUID

import Database.Enumerator
import Database.PostgreSQL.Enumerator

import System.Environment
import System.Posix.Clock
import System.Exit

import Text.Printf

import VMPS.Algorithms
import VMPS.Database
import VMPS.EnergyMinimizationChain
import VMPS.Models
import VMPS.Operators
import VMPS.Paulis
import VMPS.States
import VMPS.Tensors

import Debug.Trace
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
makeModelOperatorSiteTensors angle =
    let s = 0.5
        op = ((cos (pi / 2 * angle) :+ 0) *: pX) + ((sin (pi / 2 * angle) :+ 0) *: pY)
    in makeModelWithSpecialEndpointsOperatorSiteTensors
        4
        [(1 --> 1) pI
        ,(1 --> 2) pZ
        ,(1 --> 4) pX
        ]
        [(1 --> 1) pI
        ,(1 --> 2) pZ
        ,(2 --> 3) ((-(1-s)) *: pX)
        ,(3 --> 4) pZ
        ,(1 --> 4) ((-s)*: op)
        ,(4 --> 4) pI
        ]
        [(2 --> 1) ((-(1-s))*: pX)
        ,(3 --> 1) pZ
        ,(4 --> 1) pI
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

    let angle = read $ args !! 0
        number_of_sites = read $ args !! 1
        operator_site_tensors = makeModelOperatorSiteTensors angle number_of_sites
        bandwidth_increment = 5
        initial_bandwidth = 2
        bandwidth_increase_energy_change_convergence_criterion = 1e-4
        multisweep_energy_change_convergence_criterion = 1e-4
        level_similarity_tolerance = 1e-3

    connection <- makeConnection "vmps"
    (result,connection) <-
        withContinuedSession connection $
            doQuery
                (sqlbind "select 1 from adiabatic_constant_angle_simulations where angle=? and number_of_sites=?;"
                         [bindP angle,bindP number_of_sites]
                )
                get1
                (Nothing :: Maybe Int)

    unless (result == Nothing) $ do
        putStrLn "This data point has already been sampled."
        exitFailure

    -- @    << Define callbacks >>
    -- @+node:gcross.20091205211300.1710:<< Define callbacks >>
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
                putStrLn $ heading ++ (printf "bandwidth = %i, sweep energy = %f" current_bandwidth (chainEnergy latest_chain) )
    -- @-node:gcross.20091205211300.1710:<< Define callbacks >>
    -- @nl

    -- @    << Run simulation >>
    -- @+node:gcross.20091202133456.1303:<< Run simulation >>
    let findFirstTwoLevels attempt_number =
            (fmap unzip3 $ 
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
                    2
                    []
            ) >>= \result@([ground_energy_1, ground_energy_2],states,overlap_tensor_trios) ->
                    if abs (ground_energy_1 - ground_energy_2) < level_similarity_tolerance
                        then return result
                        else putStrLn "The first two levels do not agree!"
                             >>
                             if attempt_number < 3
                                then do
                                    putStrLn "Restarting simulation..."
                                    writeIORef level_number_ref 1
                                    findFirstTwoLevels (attempt_number + 1)
                                else do
                                    putStrLn "Three attempts to find the ground levels have failed.  Giving up!"
                                    exitFailure

    ( [ground_energy_1, ground_energy_2]
     ,[ground_state_1 , ground_state_2]
     ,ground_states_overlap_tensor_trios
     ) <- findFirstTwoLevels 1

    ([excited_energy],[excited_state],_) <-
        fmap unzip3 $ solveForMultipleLevelsWithCallbacks
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
            1
            ground_states_overlap_tensor_trios

    let energies = [ground_energy_1,ground_energy_2,excited_energy]
    -- @-node:gcross.20091202133456.1303:<< Run simulation >>
    -- @nl

    putStrLn ""
    putStrLn "The energy levels are:"
    forM_ energies $ \energy -> do
        putStr "\t"
        putStrLn . show $ energy

    let energy_gap = excited_energy - (ground_energy_1 `max` ground_energy_2)

    putStrLn ""
    putStrLn $ "The gap is " ++ show energy_gap


    TimeSpec time_in_seconds _ <- getTime ProcessCPUTime

    putStrLn $ "The elapsed CPU time for this run was " ++ show time_in_seconds ++ " seconds."

    -- @    << Store in database >>
    -- @+node:gcross.20091202133456.1304:<< Store in database >>
    withSession connection $
        withTransaction ReadCommitted $ do
            solution_id <-
                storeSolution
                    [(ground_energy_1,ground_state_1)
                    ,(ground_energy_2,ground_state_2)
                    ,(excited_energy ,excited_state )
                    ]
            number_of_rows_inserted <- execDML
                (cmdbind "insert into adiabatic_constant_angle_simulations (angle, number_of_sites, energy_gap, energy_gap_uncertainty, solution_id, convergence_criterion, simulation_running_time) values (?,?,?,?,?::uuid,?,?::interval);"
                     [bindP angle
                     ,bindP number_of_sites
                     ,bindP energy_gap
                     ,bindP $ abs (ground_energy_1 - ground_energy_2)
                     ,bindP solution_id
                     ,bindP (bandwidth_increase_energy_change_convergence_criterion `max` multisweep_energy_change_convergence_criterion)
                     ,bindP (show time_in_seconds ++ " seconds")
                     ]
                )
            if (number_of_rows_inserted == 1)
                then liftIO $ do
                    putStrLn ""
                    putStrLn $ "The id of the stored solution is " ++ solution_id
                    putStrLn ""
                else do
                    liftIO . putStrLn $
                        "Error adding the solution to the database. ("
                        ++ show number_of_rows_inserted ++
                        " rows inserted.)"
                    rollback
    -- @-node:gcross.20091202133456.1304:<< Store in database >>
    -- @nl
-- @-node:gcross.20091120111528.1236:main
-- @-others
-- @-node:gcross.20091120111528.1233:@thin simulate-adiabatic-constant-angle.hs
-- @-leo
