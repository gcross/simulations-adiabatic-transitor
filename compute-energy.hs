-- @+leo-ver=4-thin
-- @+node:gcross.20091125100559.1245:@thin compute-energy.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091125100559.1253:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20091125100559.1253:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091125100559.1247:<< Import needed modules >>
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.ConfigFile
import Data.IORef

import Database.Enumerator
import Database.PostgreSQL.Enumerator

import System.Environment
import System.Posix.Clock

import Text.Printf

import VMPS.Algorithms
import VMPS.EnergyMinimizationChain
import VMPS.OperatorConstruction
import VMPS.Pauli
import VMPS.States
import VMPS.Tensors

import Database

import Debug.Trace
-- @-node:gcross.20091125100559.1247:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091130222822.1616:Operator tensors
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
-- @-node:gcross.20091130222822.1616:Operator tensors
-- @+node:gcross.20091125100559.1251:main
main = do
    state_id <- fmap head getArgs

    connection <- makeConnection "reader"

    Just state <- withSession connection $ fetchState state_id

    let s = 0.5
        operator_site_tensors = makeModelOperatorSiteTensors s (canonicalStateNumberOfSites state)
        energy = expectationOf operator_site_tensors state
    putStrLn . show $ energy
-- @-node:gcross.20091125100559.1251:main
-- @-others
-- @-node:gcross.20091125100559.1245:@thin compute-energy.hs
-- @-leo
