
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
  Re‑implementation of Josh Cox’s shell script in Haskell.
  original conversion by gpt-oss:120b

  The behaviour matches the original script as closely as possible:

  * Environment variables:
        SEPARATOR   – default "-"
        counto      – default terminal height (rows)
        NOUN_FOLDER – default "$PWD/nouns"
        ADJ_FOLDER  – default "$PWD/adjectives"
        NOUN_FILE   – a random regular file under NOUN_FOLDER
        ADJ_FILE    – a random regular file under ADJ_FOLDER
        DEBUG       – when set to "true" prints debugging info

  * For each line (up to @counto@):
        pick a random line from @NOUN_FILE@ (lower‑cased)
        pick a random line from @ADJ_FILE@
        print   adjective <> SEPARATOR <> noun

  * The script exits with an error message if any of the required directories
    or files cannot be found.
-}
module Main where

import           Control.Exception          (throwIO)
import           Control.Monad              (when, forM_, unless, filterM)
-- import           Data.Char                  (toLower)
import           Data.Char                  ()
-- import           Data.List                  (isSuffixOf)
import           Data.List                  ()
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           System.Directory
import           System.Environment
import           System.FilePath            ((</>))
import           System.IO()
import           System.Random              (randomRIO)
import           System.Console.Terminal.Size (Window (..), size)

--------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------

-- | Look up an environment variable, falling back to a default value.
envOr :: String -> String -> IO String
envOr var def = do
  m <- lookupEnv var
  pure $ maybe def id m

-- | Return the number of rows of the current terminal, or a sensible fallback.
terminalRows :: IO Int
terminalRows = do
  mSize <- size
  case mSize of
    Just (Window _ rows) -> pure rows
    Nothing              -> pure 24    -- a common default when we cannot query

-- | Recursively collect **regular files** under a directory.
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive top = do
  entries <- listDirectory top
  let fullPaths = map (top </>) entries
  files   <- filterM doesFileExist fullPaths
  dirs    <- filterM doesDirectoryExist fullPaths
  subFiles <- concat <$> mapM listFilesRecursive dirs
  pure (files ++ subFiles)

-- | Pick a random element from a non‑empty list.
randomChoice :: [a] -> IO a
randomChoice xs = do
  idx <- randomRIO (0, length xs - 1)
  pure (xs !! idx)

-- | Pick a random line from a file (the whole file is read into memory).
randomLine :: FilePath -> IO T.Text
randomLine fp = do
  contents <- TIO.readFile fp
  let ls = T.lines contents
  unless (not (null ls)) $
    throwIO $ userError $ "File " ++ fp ++ " contains no lines."
  randomChoice ls

--------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------
main :: IO ()
main = do
  ------------------------------------------------------------------
  -- 1. Gather configuration from the environment
  ------------------------------------------------------------------
  cwd          <- getCurrentDirectory

  separator    <- envOr "SEPARATOR" "-"
  countoStr    <- envOr "counto" ""          -- may be empty → use terminal rows
  nounFolder   <- envOr "NOUN_FOLDER" (cwd </> "nouns")
  adjFolder    <- envOr "ADJ_FOLDER"  (cwd </> "adjectives")
  debugFlag    <- envOr "DEBUG" "false"

  -- Resolve the number of lines to emit
  counto :: Int <- if null countoStr
                    then terminalRows
                    else maybe (error "counto must be an integer") pure (readMaybe countoStr)

  ------------------------------------------------------------------
  -- 2. Pick random noun / adjective files (once, like the shell script)
  ------------------------------------------------------------------
  nounFiles <- listFilesRecursive nounFolder
  adjFiles  <- listFilesRecursive adjFolder

  when (null nounFiles) $ error $ "No files found under " ++ nounFolder
  when (null adjFiles)  $ error $ "No files found under " ++ adjFolder

  nounFile  <- randomChoice nounFiles >>= canonicalizePath
  adjFile   <- randomChoice adjFiles  >>= canonicalizePath

  ------------------------------------------------------------------
  -- 3. Load the entire files once (makes the loop cheap)
  ------------------------------------------------------------------
  nounLines <- T.lines <$> TIO.readFile nounFile
  adjLines  <- T.lines <$> TIO.readFile adjFile

  when (null nounLines) $ error $ "Noun file " ++ nounFile ++ " is empty."
  when (null adjLines)  $ error $ "Adjective file " ++ adjFile ++ " is empty."

  ------------------------------------------------------------------
  -- 4. Main loop – emit one line per iteration
  ------------------------------------------------------------------
  let debug = debugFlag == "true"

  forM_ [1 .. counto] $ \_ -> do
    -- pick a random line from each list
    nounRaw :: T.Text <- randomChoice nounLines
    adjRaw  :: T.Text <- randomChoice adjLines

    let noun      = T.toLower nounRaw
        adjective = adjRaw

    -- optional debugging output (mirrors the original `debugger` function)
    when debug $ do
      TIO.putStrLn $ "DEBUG adjective: " <> adjective
      TIO.putStrLn $ "DEBUG noun:      " <> noun
      TIO.putStrLn $ "ADJ_FILE: " <> T.pack adjFile
      TIO.putStrLn $ "ADJ_FOLDER: " <> T.pack adjFolder
      TIO.putStrLn $ "NOUN_FILE: " <> T.pack nounFile
      TIO.putStrLn $ "NOUN_FOLDER: " <> T.pack nounFolder
      TIO.putStrLn $ T.pack (show counto) <> " > " <> T.pack (show counto)

    -- print the final line
    TIO.putStrLn $ adjective <> T.pack separator <> noun

--------------------------------------------------------------------
-- Utility: safe read of an Int (returns Nothing on failure)
--------------------------------------------------------------------
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                [(x,"")] -> Just x
                _        -> Nothing
