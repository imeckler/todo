{-# LANGUAGE RecordWildCards #-}

module Common where

import System.Locale
import Data.Time
import Control.Applicative
import System.Directory

data TodoItem = TodoItem 
    { dateAdded  :: ZonedTime
    , message :: String
    } deriving (Show, Read)

formatTime' :: (FormatTime t) => String -> t -> String
formatTime' = formatTime defaultTimeLocale

twoDays :: NominalDiffTime
twoDays = fromInteger 172800

isOld :: UTCTime -> TodoItem -> Bool
isOld now (TodoItem {..}) = diffUTCTime now (zonedTimeToUTC dateAdded) > twoDays

getTodoListPath :: String -> IO String
getTodoListPath name = (++ "/.todo/" ++ name) <$> getHomeDirectory

getTodoList :: String -> IO [TodoItem]
getTodoList name = do
    p      <- getTodoListPath name
    exists <- doesFileExist p
    if exists
        then read <$> readFile p
        else writeFile p "[]" >> return []

getNow :: IO UTCTime
getNow = zonedTimeToUTC <$> getZonedTime