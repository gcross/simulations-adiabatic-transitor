-- @+leo-ver=4-thin
-- @+node:gcross.20091201234821.1626:@thin sweep-bitslayer-adiabatic-constant-angle.hs
-- @@language Haskell

import Control.Monad
import Data.Function
import System.IO
import System.Process
import Text.Printf

system_parameters = [
        (angle,number_of_sites)
        |   angle <- [0]
        ,   number_of_sites <- [10,15,20,30,40,60,80,100,150,200]
    ]

parametersToScript :: (Double,Int) -> String
parametersToScript (angle,number_of_sites) =
    let job_name = printf "adiabatic-constant-%f-%i" angle number_of_sites
    in unlines
        ["#PBS -d /home/gcross/Projects/QC/Simulations/Adiabatic"
        ,"#PBS -N " ++ job_name
        ,"#PBS -e logs/err/" ++ job_name
        ,"#PBS -o logs/out/" ++ job_name
        ,"#PBS -v LD_LIBRARY_PATH=/usr/local/pgsql/lib"
        ,""
        ,printf "programs/simulate-adiabatic-constant-angle %f %i" angle number_of_sites
        ]

main = forM_ system_parameters $ \parameters -> do
    (Just stdin,_,_,_) <- createProcess $ (shell "qsub") { std_in = CreatePipe }
    hPutStrLn stdin . parametersToScript $ parameters
    hFlush stdin
    return ()
-- @-node:gcross.20091201234821.1626:@thin sweep-bitslayer-adiabatic-constant-angle.hs
-- @-leo
