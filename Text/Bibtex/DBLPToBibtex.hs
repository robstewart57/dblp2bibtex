module Text.Bibtex.DBLPToBibtex (
    dblpToBibtex
  ) where

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import Data.RDF hiding (triple)
import qualified Data.Text as T
import Data.Maybe (mapMaybe,fromJust,isJust)
import Network.HTTP
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Combinators
import Text.XML.HaXml
import Text.BibTeX.Format
import Text.BibTeX.Entry

-- |'Bool' dictates whether cross referenced entries
--  should be added. 'String' is the DBLP key for a
--  given entry.
dblpToBibtex :: Bool -> String -> IO [String]
dblpToBibtex includeXRef authorURI = do
  keys <- getDBLPKeys authorURI
  mapM (getEntry includeXRef . T.unpack) keys

selectPublications :: String -> Query SelectQuery
selectPublications authorURI = do
  foaf <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
  let author  = iriRef authorURI
  publication <- var
  triple publication (foaf .:. "maker") author
  distinct
  return SelectQuery { queryVars = [publication] }

getDBLPKeys :: String -> IO [T.Text]
getDBLPKeys authorURI = do
  let dblpPrefix = T.pack "http://dblp.l3s.de/d2r/resource/publications/"
  (Just results) <- selectQuery "http://sparql.sindice.com/sparql" $ selectPublications authorURI
  let uris = map (\[Bound (UNode uriT)] -> uriT) results
  let dblpKeys = map (fromJust . T.stripPrefix dblpPrefix) uris
  return dblpKeys

getEntry ::  Bool -> String -> IO String
getEntry includeXRef key = do
  xml <- downloadXML key
  let (bibEntry, maybeXref) = entryFromXML key xml
  if includeXRef && isJust maybeXref then
    (do xrefEntry <- getEntry False (fromJust maybeXref)
        return $ xrefEntry ++ "\n" ++ bibEntry)
    else return bibEntry


downloadXML :: String -> IO String
downloadXML key = do
  let url = "http://dblp.uni-trier.de/rec/bibtex/" ++ key ++ ".xml"
      request  = replaceHeader HdrUserAgent "dblp2bib-client" (getRequest url)
  simpleHTTP request >>= getResponseBody

entryFromXML :: String -> String -> (String, Maybe String)
entryFromXML dblpKey xml = 
  let (Document _ _ root _) = xmlParse "error.log" xml
      rootElem = (CElem root noPos)
      dblpEntry = (tag "dblp" /> elm)

      authorsQ = (dblpEntry /> tag "author" /> txt)
      crossRefQ = (dblpEntry /> tag "crossref" /> txt)

      entryT = entryType' rootElem

      authorTuple = 
       let xs = extractTxt authorsQ rootElem
       in if null xs
          then []
          else [("author",authorList xs)]

      y = extractTxt crossRefQ rootElem
      crossRef = [("crossref", "DBLP:" ++ head y) | not (null y)]
      tuples = entryTuples rootElem dblpEntry
      tuples' = ("bibsource","DBLP, http://dblp.uni-trier.de")
                :
                authorTuple ++ crossRef ++ tuples

      bibtexEntry = Cons {
                       entryType = entryT
                     , identifier = "DBLP:"++dblpKey
                     , fields = tuples'
                    }

      maybeCrossRef = if not (null y)
                  then Just $ head y
                  else Nothing

   in (entry bibtexEntry,maybeCrossRef)


confirmEntryType :: Content i -> String -> Bool
confirmEntryType rootElem typeStr = 
  length ((tag "dblp" /> tag typeStr)  rootElem) > 0
  
entryType' :: Content i -> String
entryType' rootElem = 
  let xs = ["proceedings","inproceedings","article","misc","book","phdthesis","incollection"]
      xs'  = filter (confirmEntryType rootElem) xs
  in if null xs'
   then error "Unexpected entry type"
   else head xs'


entryTuples :: Content a -> CFilter a -> [(String, String)]
entryTuples rootElem dblpEntry =
  let keys = ["title","pages","year","booktitle","volume","journal","ee","number","pages"]
  in (mapMaybe entryTuple keys)

  where 
   entryTuple :: String -> Maybe (String, String)
   entryTuple key =
    let xmlFilter = (dblpEntry /> tag key /> txt)
        xs = extractTxt xmlFilter rootElem
    in if null xs
     then Nothing
     else Just (key, head xs)


extractTxt :: CFilter a -> Content a -> [String]
extractTxt xmlFilter event =
  let xs = xmlFilter event
  in (concatMap validityCheck xs)

  where
   validityCheck t = 
    case t of
     (CString _ y _) -> [y]
     _ -> []
