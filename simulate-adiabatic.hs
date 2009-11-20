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

import Data.IORef

import Text.Printf

import VMPS.Algorithms
import VMPS.EnergyMinimizationChain
import VMPS.OperatorConstruction
import VMPS.Pauli
import VMPS.Tensors
-- @-node:gcross.20091120111528.1235:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091120111528.1234:Operator tensors
makeModelOperatorSiteTensors :: Double -> Int -> [OperatorSiteTensor]
makeModelOperatorSiteTensors s =
    makeSimpleModelOperatorSiteTensors 4
        [(1 --> 1) 1.0 I
        ,(1 --> 2) 1.0 Z
        ,(2 --> 3) (-(1-s)) X
        ,(3 --> 4) 1.0 Z
        ,(1 --> 4) (-s) X
        ,(4 --> 4) 1.0 I
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
    let operator_site_tensors = makeModelOperatorSiteTensors 0.1 10
        number_of_trials_needed = 3
        bandwidth_increment = 5
        initial_bandwidth = 2

    next_bandwidth_ref <- newIORef undefined
    previous_trial_result_ref <- newIORef Nothing
    number_of_trials_remaining_ref <- newIORef undefined
    level_number_ref <- newIORef 1
    trial_number_ref <- newIORef 1

    let getTrialHeading = liftM2 (printf "LEVEL %i, trial %i: ") (readIORef level_number_ref :: IO Int) (readIORef trial_number_ref :: IO Int)
        callback_to_increase_bandwidth chain = do
            next_bandwidth <- readIORef next_bandwidth_ref
            writeIORef next_bandwidth_ref (next_bandwidth+bandwidth_increment)
            increaseChainBandwidth 2 next_bandwidth chain
        callback_to_create_new_chain =
            return . makeConfiguration operator_site_tensors
                >=>
            generateRandomizedChainWithOverlaps 2 initial_bandwidth
                >=>
            \new_chain -> (writeIORef next_bandwidth_ref (initial_bandwidth+bandwidth_increment) >> return new_chain)
        callback_to_decide_whether_to_declare_victory_with_trial chain = do
            let trial_energy = chainEnergy chain
                trial_result = (trial_energy,getCanonicalStateRepresentation chain)
            previous_trial_result <- readIORef previous_trial_result_ref
            current_level <- readIORef level_number_ref
            case previous_trial_result of
                Nothing -> do
                    writeIORef previous_trial_result_ref (Just trial_result)
                    modifyIORef trial_number_ref (+1)
                    return Nothing
                Just (best_result@(best_energy,_)) -> do
                    trial_heading <- getTrialHeading
                    case analyzeTrialEnergies 1e-5 best_energy trial_energy of
                        TrialDidBetter -> do
                            putStrLn $ printf (trial_heading ++ "Improvement!  (%f < %f)") trial_energy best_energy
                            writeIORef previous_trial_result_ref (Just trial_result)
                            writeIORef trial_number_ref 1
                            return Nothing
                        TrialDidTheSame -> do
                            putStrLn $ printf (trial_heading ++ "Repeat of best result. (%f ~= %f)") trial_energy best_energy
                            trial_number <- readIORef trial_number_ref
                            if trial_number == number_of_trials_needed
                                then do level_number <- readIORef level_number_ref
                                        putStrLn $ printf "LEVEL %i:  Final energy = %f" level_number best_energy
                                        modifyIORef level_number_ref (+1)
                                        writeIORef previous_trial_result_ref Nothing
                                        writeIORef trial_number_ref 1
                                        return (Just best_result)
                                else do modifyIORef trial_number_ref (+1)
                                        return Nothing
                        TrialDidWorse -> do
                            putStrLn $ printf (trial_heading ++ "Worse result.  (%f > %f)")  trial_energy best_energy
                            return Nothing
        callback_after_each_sweep victory_flag latest_chain = do
            trial_heading <- getTrialHeading
            next_bandwidth <- readIORef next_bandwidth_ref
            let current_bandwidth = next_bandwidth-bandwidth_increment
            if current_bandwidth <= 2
                then putStr "."
                else putStrLn $ printf (trial_heading ++ "bandwidth = %i, sweep energy = %f") current_bandwidth (chainEnergy latest_chain)        

    (energies :: [Double],_,_) <- fmap unzip3 $
        solveForMultipleLevelsWithCallbacks
            callback_to_decide_whether_to_declare_victory_with_trial
            callback_to_create_new_chain
            callback_to_increase_bandwidth
            callback_after_each_sweep
            ignoreSiteCallback
            1e-7
            1e-7
            0
            1000
            4
            []
    putStrLn ""
    putStrLn "The energy levels are:"
    forM_ energies $ \energy -> do
        putStr "\t"
        putStrLn . show $ energy
-- @-node:gcross.20091120111528.1236:main
-- @-others
-- @-node:gcross.20091120111528.1233:@thin simulate-adiabatic.hs
-- @-leo
