{-# LANGUAGE DeriveDataTypeable #-}

import Text.Bibtex.DBLPToBibtex (dblpToBibtex)
import Text.Bibtex.SearchDBLPURIs (findURIByName)
import Text.Bibtex.ListPapers (publicationTitlesForAuthor)
import System.Console.CmdArgs
import Control.Monad (when)


data MyOptions = MyOptions
    { generatebibtex :: String,
      search :: String,
      listpapers :: String,
      outfile :: String,
      xref :: Bool
    } deriving (Data, Typeable, Show, Eq)

_PROGRAM_NAME :: String
_PROGRAM_NAME = "dblp2bibtex"

_PROGRAM_VERSION :: String
_PROGRAM_VERSION = "0.0.2"

_PROGRAM_INFO :: String
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION

_PROGRAM_ABOUT :: String
_PROGRAM_ABOUT = "A Haskell utility to generate bibtex files for an author identified with a DBLP URI"

_COPYRIGHT :: String
_COPYRIGHT = "(C) Rob Stewart 2012"

myProgOpts :: MyOptions
myProgOpts = MyOptions
    { generatebibtex = def &= typ "Author URI" &= help "Get bibtex file for given URI",
      search = def  &= typ "Author name" &= help "Search for URI by name (e.g. \"Joe Bloggs\")",
      listpapers = def  &= typ "Author URI" &= help "List papers for an author URI",
      outfile = def &= typ "Bibtex filename" &= help "(default 'export.bib')",
      xref = def &= help "Include cross reference entries"
    }

run :: Mode (CmdArgs MyOptions)
run = cmdArgsMode $ myProgOpts
       &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
       &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
       &= help _PROGRAM_ABOUT
       &= helpArg [explicit, name "help", name "h"]
       &= program _PROGRAM_NAME

defaultFilename :: String
defaultFilename = "export.bib"

optUsed :: String -> Bool
optUsed x =  length x > 0

main :: IO ()
main = do
  opts <- cmdArgsRun run
  let searchURI = search opts
  let genURI = generatebibtex opts
  let lstURI = listpapers opts
  let includeXRef = xref opts
      x = filter (==True) (map optUsed [searchURI,genURI,lstURI])
  when (length x /= 1) $
   error "Exactly one option must be used. Try --help option"
  
  if not (null searchURI)
  then do
    -- Search Sindice for URIs
    uris <- findURIByName searchURI
    mapM_ putStrLn uris
  else
   if not (null genURI)
   then do
    -- Generate bibtex from DBLP URI
    bibtex <- dblpToBibtex includeXRef genURI
    let fname = if not (null (outfile opts))
                 then outfile opts
                 else defaultFilename
    writeFile fname (unlines bibtex)
   else  do
    -- List papers for a URI
    titles <- publicationTitlesForAuthor lstURI
    mapM_ putStrLn titles
