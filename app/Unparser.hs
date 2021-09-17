{-# LANGUAGE OverloadedStrings #-}
module Unparser (unparse) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Word
import Data.Bits
import qualified Data.Set as Set

import Lib

intToWord8At :: Int -> Int -> Word8
intToWord8At n i = fromIntegral ((n .&. (255 `shiftL` i)) `shiftR` i)
{-# INLINABLE intToWord8At #-}

gdInt :: Int -> ByteString
gdInt n = BS.pack $ map (n `intToWord8At`) [0, 8, 16, 24]
{-# INLINABLE gdInt #-}

gdString :: ByteString -> ByteString
gdString bs = gdInt (BS.length bs) <> bs

gdBool :: Bool -> ByteString
gdBool b = gdInt $ if b then 1 else 0

gdNamedInt :: ByteString -> Int -> ByteString
gdNamedInt name value = gdString name <> gdInt value

gdEntry :: Entry -> ByteString
gdEntry (Entry name isRead) 
    =  gdString "itemName"
    <> gdString name
    <> gdString "formulaRead"
    <> gdBool isRead

gdBeginBlock :: Formulas -> ByteString
gdBeginBlock formulas = gdNamedInt "begin_block" $ beginBlock formulas

gdVersion :: Formulas -> ByteString
gdVersion formulas = gdNamedInt "formulasVersion" $ version formulas

gdNumEntries :: Formulas -> ByteString
gdNumEntries formulas = gdNamedInt "numEntries" $ numEntries formulas

gdExpansionStatus :: Formulas -> ByteString
gdExpansionStatus formulas = gdString "expansionStatus" <> es
  where
    es = BS.singleton (expansionStatus formulas)

gdEntries :: Formulas -> ByteString
gdEntries formulas = BS.concat . map gdEntry . Set.toList $ entries formulas

gdEndBlock :: Formulas -> ByteString
gdEndBlock formulas = gdNamedInt "end_block" $ endBlock formulas

unparse :: Formulas -> ByteString
unparse formulas = BS.concat $ map (\f -> f formulas) fs
  where 
    fs = [ gdBeginBlock
         , gdVersion
         , gdNumEntries
         , gdExpansionStatus
         , gdEntries
         , gdEndBlock
         ]
