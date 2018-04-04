{-# LANGUAGE OverloadedStrings #-}

module DB ( writeToDB ) where

import Database.SQLite.Simple
import Data.Time (formatTime, defaultTimeLocale, getZonedTime)
import Scrape

-- A wrapper for the records with timestamp for the DB
data StationWithTS = SWT String Station deriving Show

instance ToRow StationWithTS where
  toRow = toRow . swtToTuple

-- Transform SWT into a tuple - needed for writing to DB
swtToTuple :: StationWithTS -> (String, String, Int, Int, Int)
swtToTuple (SWT ts s) =
  (,,,,) ts <$> stationId <*> bikesAvailable <*> spacesAvailable
            <*> boolToInt . allowDropoff $ s where
    boolToInt x = if x then 1 else 0

getTime :: IO String
getTime = formatTime defaultTimeLocale "%F %H:%M" <$> getZonedTime

appendTS :: [Station] -> IO [StationWithTS]
appendTS l = flip fmap l <$> SWT <$> getTime

-- drop newline from the end of the line
dbFile :: String -> IO String
dbFile = (filter (/= '\n') <$>) . readFile

writeToDB :: [Station] -> IO ()
writeToDB val = appendTS val >>= writeRowsToDB

writeRowsToDB :: [StationWithTS] -> IO ()
writeRowsToDB val = do
  conn <- dbFile "db_dir.txt" >>= open
  execute_ conn "CREATE TABLE IF NOT EXISTS citybike(date TEXT, id INT, bikes INT, spaces INT, allow INT)"
  executeMany conn "INSERT INTO citybike (date, id, bikes, spaces, allow) VALUES (?,?,?,?,?)" val
  close conn
