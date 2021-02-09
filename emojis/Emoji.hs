module Emoji where

import Data.Char

enc :: String -> String
enc [] = []
enc x | isUpper (head x) = (chr ((ord (head x)) - 65 + 128512)):enc (tail x) 
-- (-65) done to ensure that the converted uppercase characters do not exceed the encoding for animal emojis
      | isLower (head x) = (chr ((ord (head x)) - 97 + 128000)):enc (tail x) 
-- (-97) done to ensure that the converted lowercase characters do not exceed the encoding for emojis
      | otherwise = (head x):(enc (tail x))

dec :: String -> String
dec [] = []
dec x | (y >= 128512 && y <= 128537) = (chr ((ord (head x)) + 65 - 128512)):dec (tail x)
      | (y >= 128000 && y <= 128025) = (chr ((ord (head x)) + 97 - 128000)):dec (tail x)
      | otherwise = (head x):(enc (tail x))
      where
         y = ord (head x)