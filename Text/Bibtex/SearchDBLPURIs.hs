
module Text.Bibtex.SearchDBLPURIs (
    findURIByName
 ) where

import Network.HTTP hiding (Done)
import Data.RDF
import Data.List.Split
import System.IO (stdout, stderr, hSetBuffering, BufferMode(..))
import qualified Data.Text as T

queryURI :: String -> String
queryURI name = "http://api.sindice.com/v3/search?fq=class:foaf:Agent&format=rdfxml&fq=domain:dblp.l3s.de&field=link&q=" ++ urlEncode name

findURIByName :: String -> IO [String]
findURIByName name = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  let url = queryURI name
      request  = replaceHeader HdrUserAgent "dblp2bib-client" (getRequest url)
  xml <- simpleHTTP request >>= getResponseBody
  let doc = elimXmlHeader xml -- hack
  let (Right (rdf::TriplesGraph)) = parseString (XmlParser Nothing Nothing) (T.pack doc)
  return $ getURIs rdf


getURIs :: TriplesGraph -> [String]
getURIs rdf =
  let triples = query rdf Nothing (Just (unode "link")) Nothing
      linksURIs = map objectOf triples
      links = map (\(UNode s) -> T.unpack s) linksURIs
  in links

-- This is a hack. This should be resolved by:
-- https://github.com/UweSchmidt/hxt/issues/4
elimXmlHeader :: String -> String
elimXmlHeader xml =
  let xs = splitOn "\n" xml
  in unlines $ tail xs
