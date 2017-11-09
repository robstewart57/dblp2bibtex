{-# LANGUAGE OverloadedStrings #-}

module DBLPToBibtex (
    dblpToBibtexAuthor,
    dblpToBibtexAuthorList
  ) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.Either
import Data.RDF hiding (triple)
import qualified Data.Text as T
import Network.HTTP
import Network.Browser
import System.IO

import BibtexProcessing

-- example userId: Stewart:Robert_J
-- "http://dblp.uni-trier.de/pers/tr/s/" ++ userId ++ "=.nt"
--
-- example authorOf object: StewartMBGW16
-- "http://dblp.org/rec/bib2/conf/ica3pp/" ++ authorOfURI ++ ".bib"

dblpToBibtexAuthorList :: [String] -> IO String
dblpToBibtexAuthorList authors =
  concat <$> mapM dblpToBibtexAuthor authors

dblpToBibtexAuthor :: String -> IO String
dblpToBibtexAuthor userId = do
  putStr ("searching for author " ++ userId ++ "\n")
  let firstInitial = toLower (head userId)
  let uri = "http://dblp.uni-trier.de/pers/tr/" ++ [firstInitial] ++ "/" ++ userId ++ ".nt"
  result <- parseURL NTriplesParser uri :: IO (Either ParseFailure (RDF TList))
  bibtexData <- case result of
    Left err -> do
      hPutStr stderr ("cannot find author " ++ userId ++ "\n")
      return [Left ""]
    Right rdfGraph -> do
      let triples =
            query
            rdfGraph
            Nothing
            (Just (unode "http://dblp.uni-trier.de/rdf/schema-2017-04-18#authorOf"))
            Nothing
          publicationUris =
            map (\triple ->
                   let (UNode pubUri) = objectOf triple
                   in T.unpack pubUri ++ ".bib") triples
      mapM downloadUri publicationUris
  return (unlines (rights bibtexData))

downloadUri :: String -> IO (Either String String)
downloadUri url = do
  (_, rsp) <- Network.Browser.browse $ do
    setAllowRedirects True -- handle HTTP redirects
    setOutHandler (const (return ()))
    setErrHandler (const (return ()))
    request $ getRequest url
  case rspCode rsp of
   (3,0,3) -> do
     let err = "cannot download: " ++ show url
     hPutStr stderr err
     return (Left err)
   (4,0,4) -> do
     let err = "cannot be found: " ++ show url
     hPutStr stderr err
     return (Left err)
   (2,0,0) ->
     return (Right (rspBody rsp))
   (4,2,9) ->
     return (Right "we're making too many requests")
   code -> error (show code)
