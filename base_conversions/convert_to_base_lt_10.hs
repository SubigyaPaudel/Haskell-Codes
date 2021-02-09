toBase :: Int -> Int -> [Int]
toBase b n | (n < b) = [n]
		   |otherwise =(toBase b (n `div` b)) ++ [n `mod` b]
