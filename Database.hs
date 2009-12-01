-- @+leo-ver=4-thin
-- @+node:gcross.20091130153357.1251:@thin Database.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091130182553.1675:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
-- @-node:gcross.20091130182553.1675:<< Language extensions >>
-- @nl

module Database where

-- @<< Import needed modules >>
-- @+node:gcross.20091130153357.1584:<< Import needed modules >>
import Control.Applicative.Infix
import Control.DeepSeq
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.ByteString (ByteString,unpack)
import Data.ByteString.Internal
import Data.Complex
import Data.ConfigFile
import Data.Word

import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Database.PostgreSQL.PGFunctions

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import System.Exit
import System.IO.Unsafe

import VMPS.States
import VMPS.Tensors
-- @-node:gcross.20091130153357.1584:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091130171453.1820:Values
-- @+node:gcross.20091130171453.1822:digits
zero :: Word8
zero = 48
one = zero+1
two = one+1
three = two+1
four = three+1
five = four+1
six = five+1
seven = six+1
eight = seven+1
nine = eight+1
slash = 92
-- @-node:gcross.20091130171453.1822:digits
-- @-node:gcross.20091130171453.1820:Values
-- @+node:gcross.20091130171453.1821:Functions
-- @+node:gcross.20091130153357.1583:makeConnection
makeConnection heading = do
    either_conn <- runErrorT $ do
        cp <- join $ liftIO $ readfile emptyCP "connection.cfg"
        host <- get cp "data source" "host"
        database <- get cp "data source" "database"
        user <- get cp heading "user"
        password <- get cp heading "password"
        return $ connect
            [   CAhost host
            ,   CAdbname database
            ,   CAuser user
            ,   CApassword password
            ]
    case either_conn of
        Left err -> do
            print err
            exitFailure
        Right conn -> return conn
-- @-node:gcross.20091130153357.1583:makeConnection
-- @+node:gcross.20091130171453.1831:toEncodedString
toEncodedString :: (Pinnable a, StateSiteTensorClass a) => a -> String
toEncodedString tensor = unsafePerformIO $
    withStateSiteTensorAsByteString tensor $ \bytestring ->
        let string = map w2c . encode . unpack $ bytestring
        in string `deepseq` return string
-- @-node:gcross.20091130171453.1831:toEncodedString
-- @+node:gcross.20091130182553.1270:fromEncodedString
fromEncodedString :: (Creatable a (Int,Int,Int)) => Int -> Int -> Int -> String -> a
fromEncodedString physical_dimension left_bandwidth_dimension right_bandwidth_dimension string =
    snd . unsafePerformIO . withNewPinnedTensor (physical_dimension,left_bandwidth_dimension,right_bandwidth_dimension) $ \ptr ->
        let decoded_string = decode . map c2w $ string
            decoded_string_length = length decoded_string
            expected_length =
                physical_dimension *
                left_bandwidth_dimension *
                right_bandwidth_dimension *
                sizeOf (undefined :: Complex Double)
        in if decoded_string_length == expected_length
            then go (castPtr ptr) decoded_string
            else error $
                    "The string has the wrong length! ("
                    ++ show expected_length ++
                    " /= "
                    ++ show decoded_string_length ++
                    ")"
  where
    go _ [] = return ()
    go ptr (word:rest) = poke ptr word >> go (ptr `plusPtr` sizeOf (undefined :: Word8)) rest
-- @-node:gcross.20091130182553.1270:fromEncodedString
-- @+node:gcross.20091130153357.1585:withStateSiteTensorAsByteString
withStateSiteTensorAsByteString :: (Pinnable a, StateSiteTensorClass a) => a -> (ByteString -> IO b) -> IO b
withStateSiteTensorAsByteString tensor thunk =
    let size_in_elements = (physicalDimensionOfState <^(*)^> leftBandwidthOfState <^(*)^> rightBandwidthOfState) tensor
        size_in_bytes = size_in_elements * sizeOf (undefined :: Complex Double)
    in withPinnedTensor tensor $
        return . castPtr
        >=>
        newForeignPtr_
        >=>
        (\foreign_ptr -> return $ fromForeignPtr foreign_ptr 0 size_in_bytes)
        >=>
        thunk
-- @-node:gcross.20091130153357.1585:withStateSiteTensorAsByteString
-- @+node:gcross.20091130171453.1819:decode
decode :: [Word8] -> [Word8]
decode [] = []
decode (92:92:rest) = 92:decode rest
decode (92:a:b:c:rest) = ((a-zero)*64+(b-zero)*8+(c-zero)):decode rest
decode (x:rest) = x:decode rest
-- @-node:gcross.20091130171453.1819:decode
-- @+node:gcross.20091130171453.1823:encode
encode :: [Word8] -> [Word8]
encode [] = []
encode (0:rest) = slash:slash:zero:zero:zero:encode rest
encode (39:rest) = slash:slash:zero:four:seven:encode rest
encode (92:rest) = slash:slash:one:three:four:encode rest
encode (other:rest)
    | (0 <= other && other <= 31) || (127 <= other && other <= 255)
      = let (a,remainder) = other `divMod` 64
            (b,c) = remainder `divMod` 8
        in slash:slash:(a+zero):(b+zero):(c+zero):encode rest
    | otherwise
      = other:encode rest
-- @-node:gcross.20091130171453.1823:encode
-- @+node:gcross.20091130171453.1824:deslash
deslash :: [Word8] -> [Word8]
deslash [] = []
deslash (92:92:rest) = 92:deslash rest
deslash (other:rest) = other:deslash rest
-- @-node:gcross.20091130171453.1824:deslash
-- @+node:gcross.20091130182553.1676:stateIteratee
stateIteratee :: (MonadIO m) =>
    Int -> Int -> Int -> -- dimensions
    String -> -- data
    Int -> -- site number
    IterAct m (Either [RightAbsorptionNormalizedStateSiteTensor] CanonicalStateRepresentation)
stateIteratee
    physical_dimension left_bandwidth_dimension right_bandwidth_dimension
    site_data
    0
    (Left right_sites)
    = return
        .
        Left
        .
        Right
        .
        flip (CanonicalStateRepresentation (length right_sites + 1)) right_sites
        $!
        fromEncodedString physical_dimension left_bandwidth_dimension right_bandwidth_dimension site_data
stateIteratee
    physical_dimension left_bandwidth_dimension right_bandwidth_dimension
    site_data
    _
    (Left right_sites)
    = return
        .
        Right
        .
        Left
        .
        (:right_sites)
        $!
        fromEncodedString physical_dimension left_bandwidth_dimension right_bandwidth_dimension site_data
-- @-node:gcross.20091130182553.1676:stateIteratee
-- @+node:gcross.20091130182553.2003:fetchState
fetchState state_id =
    doQuery
        (sql $ "select physical_dimension, left_bandwidth_dimension, right_bandwidth_dimension, data, site_number from state_site_tensors where state_id = '" ++ state_id ++ "' order by site_number desc;")
        stateIteratee
        (Left [])
    >>= \result ->
        case result of
            Left [] -> return Nothing
            Right state -> return (Just state)
-- @-node:gcross.20091130182553.2003:fetchState
-- @-node:gcross.20091130171453.1821:Functions
-- @-others
-- @-node:gcross.20091130153357.1251:@thin Database.hs
-- @-leo
