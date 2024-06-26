{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import BibtexProcessing
import Control.Exception
import DBLPToBibtex
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.CmdArgs
import System.IO

data MyOptions = MyOptions
  { listAuthorFile :: String,
    person :: String,
    outfile :: String
  }
  deriving (Data, Show, Eq)

_PROGRAM_NAME :: String
_PROGRAM_NAME = "dblp2bibtex"

_PROGRAM_VERSION :: String
_PROGRAM_VERSION = "1.0"

_PROGRAM_INFO :: String
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION

_PROGRAM_ABOUT :: String
_PROGRAM_ABOUT = "This program generates bibtex files for an author identified in DBLP"

_COPYRIGHT :: String
_COPYRIGHT = "(C) Rob Stewart 2024"

myProgOpts :: MyOptions
myProgOpts =
  MyOptions
    { listAuthorFile = def &= typ "Author ID list" &= help "Get bibtex file for all author IDs in a file",
      person = def &= typ "Author ID" &= help "Get bibtex file for an author ID (e.g. \"Stewart:Robert_J=\")",
      outfile = def &= typ "Bibtex filename" &= help "(default 'export.bib')"
    }

run :: Mode (CmdArgs MyOptions)
run =
  cmdArgsMode $
    myProgOpts
      &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
      &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
      &= help _PROGRAM_ABOUT
      &= helpArg [explicit, name "help", name "h"]
      &= program _PROGRAM_NAME

defaultFilename :: String
defaultFilename = "export.bib"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  opts <- cmdArgsRun run
  let authors = listAuthorFile opts
  let author = person opts
  bibtexContent <- case (authors, author) of
    ([], []) ->
      error "--listauthorfile or --person must be used"
    ([], personId@(_ : _)) ->
      dblpToBibtexAuthor personId
    (filenameAuthorsList@(_ : _), []) -> do
      fileLines <- lines <$> readBibtexAuthorsList filenameAuthorsList
      let authorPids =
            filter
              ( \line -> case line of
                  [] -> False
                  ['#'] -> False
                  ('#' : _) -> False
                  _ -> True
              )
              fileLines
      dblpToBibtexAuthorList authorPids
    (_, _) ->
      error "cannot both be used"

  bibtexProcessed <- filterUniqueEntries bibtexContent
  -- putStrLn bibtexProcessed
  let fname =
        case outfile opts of
          [] -> defaultFilename
          outFname -> outFname
  writeBibtex bibtexProcessed fname

readBibtexAuthorsList :: String -> IO String
readBibtexAuthorsList fname =
  catch
    (readFile fname)
    ( \e -> do
        let err = show (e :: IOException)
        error ("Warning: Couldn't open " ++ fname ++ ": " ++ err ++ "\n")
    )

writeBibtex :: T.Text -> String -> IO ()
writeBibtex bibtexContents fname =
  catch
    (TIO.writeFile fname bibtexContents)
    ( \e -> do
        let err = show (e :: IOException)
        hPutStr stderr ("Warning: Couldn't write to " ++ fname ++ ": " ++ err ++ "\n")
    )
