-- @+leo-ver=4-thin
-- @+node:gcross.20091201234821.1630:@thin plot-adiabatic-constant-angle-gap.hs
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

import Database

main =
    makeConnection "reader"
    >>=
    flip withSession (do
        angles_list :: [Float] <- fmap (map read) $ liftIO getArgs
        forM angles_list $ \angle ->
            let sql_statement = "select number_of_sites, energy_gap from adiabatic_constant_angle_simulations where angle = " ++ show angle ++ " order by number_of_sites asc;"
            in do
                liftIO . hPutStrLn stderr $ "> " ++ sql_statement
                (data_points :: [(Float,Float)]) <- doQuery (sql $ sql_statement) fetch2 [] --"Error fetching completed graphs from the database:\n"
                return (angle,data_points)
    )
    >>=
    return . concatMap (\(color,(angle,data_points)) ->
        [ (PlotStyle LinesPoints (CustomStyle [LineTitle ("angle = " ++ show angle ++ " * pi/2"), LineType color, PointType 2]),data_points)
        ]
    ) . zip [1..]
    >>=
    plotPathsStyle
        [Custom "logscale" ["xy"]
        ,Key (Just ["left","top"])
        ,Title "Energy Gap vs. Number of Sites"
        ,XLabel "Number of Sites"
        ,YLabel "Energy Gap"
        ]
-- @-node:gcross.20091201234821.1630:@thin plot-adiabatic-constant-angle-gap.hs
-- @-leo
