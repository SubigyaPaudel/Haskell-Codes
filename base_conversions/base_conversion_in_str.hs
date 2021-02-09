show' :: Int -> String
show' n | (n == 10) = ['A']
        | (n == 11) = ['B']
        | (n == 12) = ['C']
        | (n == 13) = ['D']
        | (n == 14) = ['E']
        | (n == 15) = ['F']
        | otherwise = show n

showBase :: Int -> Int -> String
showBase b n | (n < b) = show n
showBase 16 n = showBase 16 (n `div` 16) ++ show' (n `mod` 16) 
showBase b n = showBase b (n `div` b) ++ show (n `mod` b) 

showBin :: Int -> String
showBin a = showBase 2 a

showOct :: Int -> String
showOct a = showBase 8 a

showHex :: Int -> String
showHex a = showBase 16 a 