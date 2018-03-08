{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DBLPToBibtex (
    dblpToBibtexAuthor,
    dblpToBibtexAuthorList
  ) where

import Control.Concurrent
import Control.Exception
import Control.Exception.Lifted
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Either
import Data.RDF hiding (triple)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Network.HTTP
import Network.HTTP.Conduit
import Network.HTTP.Headers
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
  -- putStrLn ("downloading " ++ uri)
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
      mapM (downloadUri 0) publicationUris
  return (unlines (rights bibtexData))

downloadUri :: Int -> String -> IO (Either String String)
downloadUri retryCount url =
  -- arbitrarily chosen number
  if retryCount > 3
  then do
    let err = "we are repeatedly making too many DBLPrequests, giving up."
    hPutStr stderr err
    return (Left err)
  else
    if (T.pack "http:") `T.isPrefixOf` (T.pack url)
    then downloadHttp retryCount url
    else if (T.pack "https:") `T.isPrefixOf` (T.pack url)
    then downloadHttps retryCount url
    else error ("unknown prefix for URL: " ++ url)

downloadHttps :: Int -> String -> IO (Either String String)
downloadHttps retryCount url = do
  result <- Control.Exception.Lifted.try $ simpleHttp url
  case result of
    Left (ex::HttpException) -> error (show ex)
    Right bs -> do
      let s = T.unpack (E.decodeUtf8 (BS.toStrict bs))
      putStrLn ("got: " ++ s)
      return (Right s)

downloadHttp :: Int -> String -> IO (Either String String)
downloadHttp retryCount url = do    
    (_, rsp) <- Network.Browser.browse $ do
      setAllowRedirects True -- handle HTTP redirects
      setOutHandler (const (return ()))
      -- setErrHandler (const (return ()))
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
      -- to many requests
      (4,2,9) -> do
        let headers = rspHeaders rsp
            retryAfter :: Int
            retryAfter = findRetry headers
            findRetry [] = error "cannot find Retry-After header"
            findRetry (Header HdrRetryAfter i : hdrs) = read i
            findRetry (_ : hdrs) = findRetry hdrs
            -- add another second to be sure
        putStrLn $
          "too many DBLP requests, retrying in " ++ show (retryAfter + 1) ++ " seconds."
        threadDelay ((retryAfter + 1) * 1000000)
        downloadUri (retryCount + 1) url
      (2,0,0) ->
        return (Right (rspBody rsp))
      code -> error (show code)
