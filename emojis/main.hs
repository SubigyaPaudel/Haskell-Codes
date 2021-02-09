{- /
	Module: main.hs
-}

import System.IO
import Emoji
import Data.Char

convert :: String -> String
convert xs | null $ filter (\c -> isLetter c && isAscii c) xs = dec xs
           | otherwise = enc xs

main = do
contents <- getContents
putStrLn $ convert contents

