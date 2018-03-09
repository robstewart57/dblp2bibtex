{-# LANGUAGE OverloadedStrings #-}

module BibtexProcessing (
    filterUniqueEntries
  ) where

import Data.List
import qualified Data.Text as T
import Text.BibTeX.Entry
import Text.BibTeX.Format
import Text.BibTeX.Parse hiding (entry)
import Text.Parsec.Prim
import Text.Parsec.String

filterUniqueEntries :: T.Text -> IO T.Text
filterUniqueEntries bibtexData = do
  -- content <- readFile bibtexFile
  let newContent = T.filter (/= '\\') bibtexData
  -- writeFile bibtexFile newContent
  -- result <- parseFromFile file bibtexFile
  let result = runP file () "" (T.unpack newContent)
  case result of
    Left e -> error (show e)
    Right entries ->
      return $
        T.pack $
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
