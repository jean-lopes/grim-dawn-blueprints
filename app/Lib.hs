module Lib where

import Data.Word
import Data.ByteString (ByteString)
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set

data Entry = Entry
  { name :: ByteString
  , isRead :: Bool
  } deriving Show

data Formulas = Formulas 
  { beginBlock :: Int
  , version :: Int
  , numEntries :: Int
  , expansionStatus :: Word8
  , entries :: Set Entry
  , endBlock :: Int
  } deriving Show

data FormulasFile = FormulasFile
  { filePath :: String
  , content :: Formulas
  } deriving Show

data Diff = Diff 
  { formulasFile :: FormulasFile
  , missingEntries :: Set Entry
  } deriving Show

instance Eq Entry where
  (==) = (==) `on` name

instance Ord Entry where
  compare = compare `on` name

instance Eq Formulas where
  (==) = (==) `on` entries

diff :: [FormulasFile] -> [Diff]
diff fs = map diff' fs
  where
    allEntries = Set.unions $ map (entries . content) fs
    diff' f = Diff f $ Set.difference allEntries $ (entries . content) f

addMissingEntries :: Diff -> FormulasFile
addMissingEntries (Diff f missing) = f { content = formulas { numEntries = Set.size xs, entries = xs } }
  where
    formulas = content f
    xs = Set.union missing $ entries formulas