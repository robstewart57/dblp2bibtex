{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBLPToBibtex
  ( dblpToBibtexAuthor,
    dblpToBibtexAuthorList,
  )
where

import BibtexProcessing
import Control.Concurrent
import Control.Exception
import Control.Exception.Lifted
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Either
import Data.List (nub)
import Data.Maybe
import Data.RDF hiding (triple)
import qualified Data.Text as T
import Data.Text.Encoding as E
import qualified Data.Text.Encoding as T
import Network.Browser
import Network.HTTP
import Network.HTTP.Conduit
import Network.HTTP.Headers
import System.IO

dblpToBibtexAuthor :: String -> IO T.Text
dblpToBibtexAuthor authorPid = dblpToBibtexAuthorList [authorPid]

dblpToBibtexAuthorList :: [String] -> IO T.Text
dblpToBibtexAuthorList authorPids = do
  allPaperURIs <- concat <$> mapM dblpPidToPaperURIs authorPids
  let allPaperURIsNoDuplicates = nub allPaperURIs
      howManyDuplicates = length allPaperURIs - length allPaperURIsNoDuplicates
  when (howManyDuplicates > 0) $ putStrLn ("removing " <> show howManyDuplicates <> " duplicate papers for co-authored papers")
  T.concat . rights <$> mapM paperUriToBib allPaperURIsNoDuplicates

paperUriToBib :: T.Text -> IO (Either String T.Text)
paperUriToBib paperUri = do
  downloadUri 0 (paperUri <> ".bib")

dblpPidToPaperURIs :: String -> IO [T.Text]
dblpPidToPaperURIs dblpPid = do
  putStrLn ("Searching for author " ++ dblpPid)
  let uri = "https://dblp.org/pid/" ++ dblpPid ++ ".nt"
  -- putStrLn ("downloading " ++ uri)
  result <- parseURL NTriplesParser uri :: IO (Either ParseFailure (RDF TList))
  paperUris <- case result of
    Left _err -> do
      hPutStr stderr ("cannot find author with DBLP pid " ++ dblpPid ++ "\n")
      return Nothing
    Right rdfGraph -> do
      let triples =
            query
              rdfGraph
              Nothing
              (Just (unode "https://dblp.org/rdf/schema#authorOf"))
              Nothing
          publicationUris =
            map
              ( \triple ->
                  let (UNode pubUri) = objectOf triple
                   in pubUri
              )
              triples
      return (Just publicationUris)
  return (fromMaybe [] paperUris)

downloadUri :: Int -> T.Text -> IO (Either String T.Text)
downloadUri retryCount url =
  -- arbitrarily chosen number
  if retryCount > 3
    then do
      let dblpErr = "we are repeatedly making too many DBLPrequests, giving up."
      hPutStr stderr dblpErr
      return (Left dblpErr)
    else do
      putStr ("downloading " ++ T.unpack url)
      if T.pack "http:" `T.isPrefixOf` url
        then downloadHttp retryCount url
        else
          if T.pack "https:" `T.isPrefixOf` url
            then downloadHttps retryCount url
            else error ("unknown prefix for URL: " ++ T.unpack url)

downloadHttps :: Int -> T.Text -> IO (Either String T.Text)
downloadHttps retryCount url = do
  result <- Control.Exception.Lifted.try $ simpleHttp (T.unpack url)
  case result of
    Left (theException :: HttpException) ->
      case theException of
        (InvalidUrlException invalidURL reason) -> do
          hPutStr stderr ("URL " <> invalidURL <> " is invalid because " <> reason)
          return (Left reason)
        (HttpExceptionRequest _ content) ->
          case content of
            ConnectionTimeout -> do
              let retryAfter = 60
              putStrLn $
                "\ntoo many DBLP requests, retrying in " ++ show retryAfter ++ " seconds."
              threadDelay ((retryAfter + 1) * 1000000)
              downloadUri (retryCount + 1) url
            StatusCodeException resp _ -> do
              let retryAfter =
                    let (_, retryTime) = head $ filter (\(headerName, _) -> headerName == "Retry-After") (responseHeaders resp)
                     in read (T.unpack (T.decodeASCII retryTime))
              putStrLn $
                "\ntoo many DBLP requests, retrying in " ++ show retryAfter ++ " seconds."
              threadDelay ((retryAfter + 1) * 1000000)
              downloadUri (retryCount + 1) url
            _ -> error ("HttpExceptionRequest content: " ++ show content)
    Right bs -> do
      putStr " ... downloaded.\n"
      let s = E.decodeUtf8 (BS.toStrict bs)
      return (Right s)

downloadHttp :: Int -> T.Text -> IO (Either String T.Text)
downloadHttp retryCount url = do
  (_, rsp) <- Network.Browser.browse $ do
    setAllowRedirects True -- handle HTTP redirects
    setOutHandler (const (return ()))
    -- setErrHandler (const (return ()))
    request $ getRequest (T.unpack url)
  case rspCode rsp of
    (3, 0, 3) -> do
      let dblpErr = "cannot download: " ++ show url
      hPutStr stderr dblpErr
      return (Left dblpErr)
    (4, 0, 4) -> do
      let dblpErr = "cannot be found: " ++ show url
      hPutStr stderr dblpErr
      return (Left dblpErr)
    -- to many requests
    (4, 2, 9) -> do
      let headers = rspHeaders rsp
          retryAfter :: Int
          retryAfter = findRetry headers
          findRetry [] = error "cannot find Retry-After header"
          findRetry (Header HdrRetryAfter i : _hdrs) = read i
          findRetry (_ : hdrs) = findRetry hdrs
      -- add another second to be sure
      putStrLn $
        "\ntoo many DBLP requests, retrying in " ++ show (retryAfter + 1) ++ " seconds."
      threadDelay ((retryAfter + 1) * 1000000)
      downloadUri (retryCount + 1) url
    (2, 0, 0) -> do
      putStr " ... downloaded.\n"
      return (Right (T.pack (rspBody rsp)))
    code -> error (show code)
