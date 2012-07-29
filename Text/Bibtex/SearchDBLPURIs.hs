{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Bibtex.SearchDBLPURIs (
    findURIByName -- :: String -> IO [String]
 ) where

import Network.HTTP hiding (Done)
import Data.RDF
import Data.RDF.TriplesGraph
import Text.RDF.RDF4H.XmlParser
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Split
import System.IO (stdout, stderr, hSetBuffering, BufferMode(..))

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
  let (Right (rdf::TriplesGraph)) = parseXmlRDF Nothing Nothing (B.pack doc)
  return $ getURIs rdf


getURIs :: TriplesGraph -> [String]
getURIs rdf = 
  let triples = query rdf Nothing (Just (unode "link")) Nothing
      linksURIs = map objectOf triples
      links = map (\(UNode s) -> (reverse . b2s . value) s) linksURIs
  in links

-- This is a hack. This should be resolved by:
-- https://github.com/UweSchmidt/hxt/issues/4
elimXmlHeader :: String -> String
elimXmlHeader xml = 
  let xs = splitOn "\n" xml
  in unlines $ tail xs