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

import Debug.Trace
-- @-node:gcross.20091125100559.1247:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091125100559.1251:main
main = do
    let state_id = "75641026-6fb6-442e-8a92-9f7fe451c6b7"
        perturbation_coefficient = 1

    connection <- makeConnection "reader"

    Just state <- withSession connection $ fetchState state_id

    let operator_site_tensors =
            makeModelWithSpecialEndpointsOperatorSiteTensors
                6
                [(1 --> 1) (-perturbation_coefficient/2) X
                ,(1 --> 3) 1 I
                ,(1 --> 5) 1 I
                ]
                [(1 --> 1) 1 I
                ,(1 --> 2) 1 X
                ,(2 --> 2) 1 I
                ,(3 --> 3) 1 I
                ,(3 --> 4) 1 X
                ,(4 --> 4) 1 I
                ,(5 --> 5) 1 I
                ,(5 --> 6) 1 Z
                ,(6 --> 6) 1 I
                ]
                [(2 --> 1) 1 I
                ,(4 --> 1) (-perturbation_coefficient/2) X
                ,(6 --> 1) 1 I
                ]
                (canonicalStateNumberOfSites state)
        energy = expectationOf operator_site_tensors state
    putStrLn . show $ energy
-- @-node:gcross.20091125100559.1251:main
-- @-others
-- @-node:gcross.20091125100559.1245:@thin compute-energy.hs
-- @-leo
