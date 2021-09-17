{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser (parse) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Word
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import qualified Data.Set as Set
import Text.Printf

import Lib

data Error 
  = UnexpectedEof 
  | ExpectedEof
  | UnexpectedString { expected :: ByteString, found :: ByteString }
  | UnsupportedVersion Int
  deriving Show

type Parser a = ExceptT Error (State ByteString) a

errorToStr :: Error -> String
errorToStr UnexpectedEof = "Unexpected EOF"
errorToStr ExpectedEof = "Expected EOF"
errorToStr (UnexpectedString expected found) = printf "Expected string `%s` but found `%s`" (Char8.unpack expected) (Char8.unpack found)
errorToStr (UnsupportedVersion n) = printf "Only files with version `3` are supported, found version: `%d`" n

item :: Parser Word8
item = do
  bs <- lift get
  case BS.uncons bs of
    Nothing -> throwE UnexpectedEof
    Just (x, xs) -> do
      lift $ put xs
      return x

eof :: Parser ()
eof = do
  bs <- lift get 
  case BS.uncons bs of
    Nothing -> return ()
    Just _ -> throwE ExpectedEof

count :: Int -> Parser a -> Parser [a]
count n p
  | n <= 0    = return []
  | otherwise = replicateM n p

gdInt :: Parser Int
gdInt = bsToInt <$> count 4 item
  where
    bsToInt = foldr (\n acc -> fromIntegral n + 256 * acc) 0

gdBool :: Parser Bool
gdBool = (==1) <$> gdInt

gdAnyString :: Parser ByteString
gdAnyString = gdInt >>= \n -> BS.pack <$> count n item

gdString :: ByteString -> Parser ByteString
gdString xs = gdAnyString >>= \ys -> 
  if xs == ys
    then return xs
    else throwE UnexpectedString { expected = xs, found = ys }

gdBeginBlock :: Parser Int
gdBeginBlock = gdString "begin_block" *> gdInt

gdFormulasVersion :: Parser Int
gdFormulasVersion = gdString "formulasVersion" *> gdInt >>= \version -> 
  if version /= 3
    then throwE (UnsupportedVersion version)
    else return version

gdNumEntries :: Parser Int
gdNumEntries = gdString "numEntries" *> gdInt

gdExpansionStatus :: Parser Word8
gdExpansionStatus = gdString "expansionStatus" *> item

gdEntry :: Parser Entry
gdEntry = Entry 
  <$  gdString "itemName" 
  <*> gdAnyString 
  <*  gdString "formulaRead" 
  <*> gdBool

gdEntries :: Int -> Parser [Entry]
gdEntries n = count n gdEntry

gdEndBlock :: Parser Int
gdEndBlock = gdString "end_block" *> gdInt

gdFormulas :: Parser Formulas
gdFormulas = do
  beginBlock <- gdBeginBlock 
  version <- gdFormulasVersion
  numEntries <- gdNumEntries
  expansionStatus <- gdExpansionStatus
  entries <- gdEntries numEntries
  endBlock <- gdEndBlock
  void eof
  return $ Formulas 
    { beginBlock = beginBlock 
    , version = version
    , numEntries = numEntries
    , expansionStatus = expansionStatus
    , entries = Set.fromList entries
    , endBlock = endBlock
    }

parse :: String -> ByteString -> Either String FormulasFile
parse p bs = case result of
    Left err -> Left $ printf "[%s] %s. Consumed %d bytes out of %d" p (errorToStr err) inputSize consumedSize
    Right fs -> Right (FormulasFile p fs)
  where
    (result, unconsumed) = runState (runExceptT gdFormulas) bs
    inputSize = BS.length bs
    consumedSize = inputSize - BS.length unconsumed
