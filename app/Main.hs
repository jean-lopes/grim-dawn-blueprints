{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS

import Lib
import Parser
import Unparser
import Data.Either (partitionEithers)
import Data.Foldable (forM_)
import Control.Monad (when, unless)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Set as Set

printDiff :: Diff -> IO ()
printDiff (Diff ff missing) = do
  printf "%s (%d):\n" (filePath ff) (Set.size missing)
  mapM_ (printf "  %s\n" . Char8.unpack . name) missing

printDiffs :: [Diff] -> IO ()
printDiffs ds = putStrLn "showing missing entries by file" >> forM_ ds printDiff

main :: IO ()
main = do
  let filePaths = ["formulas.gsh", "formulas.gst"]
  
  files <- mapM (\p -> (,) p <$> BS.readFile p) filePaths -- catch exceptions
  
  let parseResults = map (uncurry parse) files
  
  let (parseErrors, formulaFiles) = partitionEithers parseResults

  let diffs = diff formulaFiles

  unless (null parseErrors) $ putStrLn $ unlines parseErrors
  printDiffs diffs

  let synched = map addMissingEntries diffs

  let raws = map (\(FormulasFile p f) -> (p, unparse f) ) synched

  printf "saving %d synchronized files\n" (length raws)

  mapM_ (uncurry BS.writeFile) raws

  printf "done\n"
