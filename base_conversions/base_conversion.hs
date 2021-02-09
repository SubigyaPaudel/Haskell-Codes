fromBase :: Int -> [Int] -> Int
fromBase _ [] = 0
fromBase b n = (fromBase b (tail n)) + ((head n) * (b^((length n)-1)))
