-- @+leo-ver=4-thin
-- @+node:gcross.20100126114250.1298:@thin fetch-adiabatic-random-angle-data.hs
-- @@language Haskell

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Trans
import Database.Enumerator
import Database.PostgreSQL.Enumerator
import System.IO

import VMPS.Database

printRow :: (Functor m, MonadIO m) => Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IterAct m ()
printRow a b c d e f g h () = fmap Right $ (liftIO . putStrLn) (show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e ++ " " ++ show f ++ " " ++ show g ++ " " ++ show h)

main =
    makeConnection "reader"
    >>=
    flip withSession (
        let sql_statement = "select number_of_sites, avg(energy_gap), stddev(energy_gap), min(energy_gap), max(energy_gap), ln(number_of_sites), ln(avg(energy_gap)), ln(avg(energy_gap)+stddev(energy_gap))-ln(avg(energy_gap)-stddev(energy_gap)) from adiabatic_random_angle_simulations group by number_of_sites order by number_of_sites asc;"
        in do
            liftIO . hPutStrLn stderr $ "> " ++ sql_statement
            doQuery (sql $ sql_statement) printRow ()
    )
-- @-node:gcross.20100126114250.1298:@thin fetch-adiabatic-random-angle-data.hs
-- @-leo
