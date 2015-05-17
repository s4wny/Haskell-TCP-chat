module SharedCode
( trim
, ChatProtocol (..)
) where

import Data.Char (isSpace)


data ChatProtocol = ChatProtocol {
    username :: String,
    message  :: String
} deriving (Show, Read)


-- Like PHP trim
trim :: String -> String
trim = f . f
    where
        f = reverse . dropWhile isSpace

