module Text.Bibtex.DBLPToBibtex (
    dblpToBibtex -- :: String -> IO String
  ) where

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import Data.Text.Lazy (pack,unpack,stripPrefix)
import Data.Text.Lazy.Internal (Text)
import Data.Maybe (mapMaybe,fromJust)
import Network.HTTP
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Combinators
import Text.XML.HaXml
import Text.BibTeX.Format
import Text.BibTeX.Entry

dblpToBibtex :: String -> IO [String]
dblpToBibtex authorURI = do
  keys <- getDBLPKeys authorURI
  mapM (getEntry . unpack) keys

selectPublications :: String -> Query SelectQuery
selectPublications authorURI = do
  foaf <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
  let author  = iriRef authorURI
  publication <- var
  triple publication (foaf .:. "maker") author
  return SelectQuery { queryVars = [publication] }

getDBLPKeys :: String -> IO [Text]
getDBLPKeys authorURI = do
  let dblpPrefix = pack "http://dblp.l3s.de/d2r/resource/publications/"
  (Just results) <- selectQuery "http://sparql.sindice.com/sparql" $ selectPublications authorURI
  let uris = map (\[URI uri] -> pack uri) results
  let dblpKeys = map (fromJust . stripPrefix dblpPrefix) uris
  return dblpKeys

getEntry :: String -> IO String
getEntry key = do
  xml <- downloadXML key
  return (entryFromXML key xml)

downloadXML :: String -> IO String
downloadXML key = do
  let url = "http://dblp.uni-trier.de/rec/bibtex/" ++ key ++ ".xml"
      request  = replaceHeader HdrUserAgent "dblp2bib-client" (getRequest url)
  simpleHTTP request >>= getResponseBody

entryFromXML :: String -> String -> String
entryFromXML dblpKey xml = 
  let (Document _ _ root _) = xmlParse "error.log" xml
      rootElem = (CElem root noPos)
      dblpEntry = (tag "dblp" /> elm)

      authorsQ = (dblpEntry /> tag "author" /> txt)
      crossRefQ = (dblpEntry /> tag "crossref" /> txt)

      entryT = entryType' rootElem

      authors = authorList $ extractTxt authorsQ rootElem

      maybeXref = extractTxt crossRefQ rootElem
      crossRef = [("crossref", "DBLP:" ++ head maybeXref) | not (null maybeXref)]        
      tuples = entryTuples rootElem dblpEntry
      tuples' = ("bibsource","DBLP, http://dblp.uni-trier.de")
                :
                ("author",authors)
                :
                crossRef ++ tuples

      bibtexEntry = Cons {
                       entryType = entryT
                     , identifier = "DBLP:"++dblpKey
                     , fields = tuples'
                    }

   in (entry bibtexEntry)


confirmEntryType :: Content i -> String -> Bool
confirmEntryType rootElem typeStr = 
  length ((tag "dblp" /> tag typeStr)  rootElem) > 0
  
entryType' :: Content i -> String
entryType' rootElem = 
  let xs = ["inproceedings","article","misc","book","phdthesis","incollection"]
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
