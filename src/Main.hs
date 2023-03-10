module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = Test.SimpleTest.Mock.writeFile "snippets.ben" (DB.serialize DB.empty)


-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  DB.load >>= myFunction
  where
    myFunction x =
      case x of
        Success ok -> case DB.findFirst predicate ok of
          Just o -> putStrLn (entrySnippet o)
          Nothing -> putStrLn "nothing"
        Error er -> putStrLn "Failed to load DB"
    predicate pr = entryId pr == id
    id = getOptId getOpts



-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = 
  do
    DB.load >>= myFunction
    where
      myFunction x=
        let
          showFEntry :: [Entry] -> String
          showFEntry [] = ""
          showFEntry (x:xs) = show (FmtEntry x) ++ "\n" ++ showFEntry xs
        in
          case x of
            Success ok -> case DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) ok of
                             [] -> putStrLn "No entries found"
                             l -> putStrLn (showFEntry l)
            Error er -> putStrLn "Failed to load DB"




-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts =
  do
    contents <- readFile (addOptFilename addOpts)
    let makeEntry2 idd = makeEntry idd contents addOpts
    let functionInsert myDB = DB.insertWith makeEntry2 myDB
    let predicate pr = entrySnippet pr == contents
    x <- DB.load
    case x of
      Success ok -> case DB.findFirst predicate ok of
                      Just q -> putStrLn ("Entry with this content already exists: " ++ "\n" ++ show (FmtEntry q))
                      Nothing -> DB.modify functionInsert >>= myFunction
      Error er -> putStrLn "Failed to load DB"
  where
    myFunction x=
      case x of
        Success ok -> putStrLn "ok"
        Error er -> putStrLn "Failed to load DB"

    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
