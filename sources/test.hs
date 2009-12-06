-- @+leo-ver=4-thin
-- @+node:gcross.20091130171453.1739:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091130171453.1740:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
-- @-node:gcross.20091130171453.1740:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091130171453.1741:<< Import needed modules >>
import Control.Applicative.Infix

import Data.Word

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Database
-- @-node:gcross.20091130171453.1741:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091130171453.1827:Generators
-- @+node:gcross.20091130171453.1828:Word8
instance Arbitrary Word8 where
    arbitrary = fmap fromIntegral (choose (0,255) :: Gen Integer)
-- @-node:gcross.20091130171453.1828:Word8
-- @-node:gcross.20091130171453.1827:Generators
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20091130171453.1825:<< Tests >>
    -- @+others
    -- @+node:gcross.20091130171453.1826:recode
    [testProperty "encode followed by decode" $ id <^(==)^> (decode . deslash . encode)
    -- @-node:gcross.20091130171453.1826:recode
    -- @-others
    -- @-node:gcross.20091130171453.1825:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091130171453.1739:@thin test.hs
-- @-leo
