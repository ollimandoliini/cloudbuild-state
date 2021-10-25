module Main where

import Control.Monad (forM_, when)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as B hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as B (putStrLn)
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FileLock

dbPath = "db.json"

type Database = Map.Map String String

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs ["get", key] = get key
handleArgs ["set", key, val] = set key val
handleArgs ["list"] = list
handleArgs ["list", "--json"] = listJson
handleArgs args = putStr $ "Unknown command: '" ++ unwords args ++ "'\nCommands:\n - get [key]\n - set [key] [value]\n - list (--json)\n"

get :: String -> IO ()
get key = do
  lockFile dbPath Exclusive
  B.readFile dbPath >>= \content -> case (decode content :: Maybe Database) >>= Map.lookup key of
    (Just val) -> putStrLn val
    Nothing -> error "Value not found"

set :: String -> String -> IO ()
set key val = do
  lockFile dbPath Exclusive
  B.readFile dbPath >>= \content -> case (decode content :: Maybe Database) of
    (Just db) -> B.writeFile dbPath $ encode $ Map.insert key val db
    Nothing -> B.writeFile dbPath (encode $ Map.fromList [(key, val)])

list :: IO ()
list = do
  lockFile dbPath Exclusive
  B.readFile dbPath >>= \content -> case (decode content :: Maybe Database) of
    Just db -> forM_ (Map.toList db) (\(key, val) -> putStrLn (key ++ "=" ++ val))
    Nothing -> return ()

listJson :: IO ()
listJson = do
  lockFile dbPath Exclusive
  B.readFile dbPath >>= B.putStrLn
