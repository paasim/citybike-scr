module Main where

import DB ( writeToDB )
import Scrape ( dlAndParseData )

main :: IO ()
main = dlAndParseData >>= writeToDB
