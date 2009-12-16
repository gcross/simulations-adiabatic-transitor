-- @+leo-ver=4-thin
-- @+node:gcross.20091215183613.1296:@thin plot-adiabatic-restricted-random-angle-gap.hs
-- @@language Haskell

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Data.Char
import Graphics.Gnuplot.Simple
import System
import System.IO
import System.Console.GetOpt
import Text.Printf

import VMPS.Database

main =
    makeConnection "reader"
    >>=
    flip withSession (
        let sql_statement_1 = "select number_of_sites, energy_gap from adiabatic_restricted_random_angle_simulations order by number_of_sites asc;"
            sql_statement_2 = "select number_of_sites, min(energy_gap) from adiabatic_restricted_random_angle_simulations group by number_of_sites order by number_of_sites asc;"
        in do
            liftIO . hPutStrLn stderr $ "> " ++ sql_statement_1
            raw_points :: [(Float,Float)] <- doQuery (sql $ sql_statement_1) fetch2 []
            liftIO . hPutStrLn stderr $ "> " ++ sql_statement_2
            minimum_points :: [(Float,Float)]  <- doQuery (sql $ sql_statement_2) fetch2 []
            return (raw_points,minimum_points)
    )
    >>= return . (
        \(raw_points,minimum_points) ->
            [ (PlotStyle Points (CustomStyle [LineTitle ""]),raw_points)
            , (PlotStyle Lines (CustomStyle [LineTitle ""]),minimum_points)
            ]
    )
    >>=
    plotPathsStyle
        [Custom "logscale" ["xy"]
        ,Key (Just ["left","top"])
        ,Title "Energy Gap vs. Number of Sites"
        ,XLabel "Number of Sites"
        ,YLabel "Energy Gap"
        ]
-- @-node:gcross.20091215183613.1296:@thin plot-adiabatic-restricted-random-angle-gap.hs
-- @-leo
