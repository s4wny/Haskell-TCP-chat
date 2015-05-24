module SharedCode
( ChatProtocol (..)
, trim
, strToLower
, logger
) where

import Data.Char (isSpace, toLower)
import Data.Time
import System.Locale (defaultTimeLocale)

data ChatProtocol = ChatProtocol {
    username :: String,
    message  :: String
} deriving (Show, Read)


-- Like PHP trim
trim :: String -> String
trim = f . f
    where
        f = reverse . dropWhile isSpace


-- strToLower
strToLower :: String -> String
strToLower = map toLower


-- Simple logging function
logger :: String -> IO ()
logger str = do
	date <- _getDate
	putStrLn $ date ++ " - " ++ str


-- Returns current date in Y-n-d H:M:s format
_getDate :: IO String
_getDate = do
	time <- getZonedTime
	return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time