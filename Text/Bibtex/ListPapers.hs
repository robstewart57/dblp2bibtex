module Text.Bibtex.ListPapers (
    publicationTitlesForAuthor -- :: String -> IO String
  ) where

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

publicationTitlesForAuthor :: String -> IO [String]
publicationTitlesForAuthor authorURI = do
  (Just results) <- selectQuery "http://sparql.sindice.com/sparql" $ titlesQuery authorURI
  let titles = map (\[TypedLiteral uri _] -> uri) results
  return titles


titlesQuery :: String -> Query SelectQuery
titlesQuery authorURI = do
  foaf <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
  dc <- prefix "dc" (iriRef "http://purl.org/dc/elements/1.1/")
  let author  = iriRef authorURI
  publication <- var
  title <- var
  triple publication (foaf .:. "maker") author
  triple publication (dc .:. "title") title
  distinct
  return SelectQuery { queryVars = [title] }
