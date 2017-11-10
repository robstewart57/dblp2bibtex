{-# LANGUAGE OverloadedStrings #-}

module BibtexProcessing (
    filterUniqueEntries
  ) where

import Data.List
import Text.BibTeX.Entry
import Text.BibTeX.Format
import Text.BibTeX.Parse hiding (entry)
import Text.Parsec.Prim
import Text.Parsec.String

filterUniqueEntries :: String -> IO String
filterUniqueEntries bibtexData = do
  -- content <- readFile bibtexFile
  let newContent = filter (/= '\\') bibtexData
  -- writeFile bibtexFile newContent
  -- result <- parseFromFile file bibtexFile
  let result = runP file () "" newContent
  case result of
    Left e -> error (show e)
    Right entries ->
      return $
        concatMap
        entry
        (nub
          (filter
           (\entry -> entryType entry /= "proceedings")
           entries)
        )

instance Eq T where
  (==) (Cons s1 s2 ss3) (Cons s4 s5 ss6) =
    s1 == s4 &&
    s2 == s5 &&
    ss3 == ss6
