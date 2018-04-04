{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Scrape ( dlAndParseData
              , Station
              , stationId
              , bikesAvailable
              , spacesAvailable
              , allowDropoff
              ) where

import Network.HTTP.Simple
import GHC.Generics
import Data.Aeson.Types

-- The actual data
data Station = Station {
    stationId :: String
  , bikesAvailable :: Int
  , spacesAvailable :: Int
  , allowDropoff :: Bool
  } deriving (Generic, Show)

-- A wrapper for Aeson
data Stations = Stations {bikeRentalStations :: [Station]} deriving Generic

instance FromJSON Stations
instance FromJSON Station

handleParsingErr :: Result [a] -> IO [a]
handleParsingErr (Success a) = return a
handleParsingErr (Error s) = print s >> return []

parseData :: Response Value -> IO [Station]
parseData = handleParsingErr
          . fmap bikeRentalStations
          . parse (withObject "stations" (.: "data"))
          . getResponseBody

getData :: IO (Response Value)
getData = httpJSON . setHeaders . setBody $ req where
  setHeaders = setRequestHeader "Content-Type" ["application/graphql"]
  setBody = setRequestBody body
  body = "{bikeRentalStations{stationId,bikesAvailable,spacesAvailable,allowDropoff}}"
  req = "POST https://api.digitransit.fi/routing/v1/routers/hsl/index/graphql"

dlAndParseData :: IO [Station]
dlAndParseData = getData >>= parseData
