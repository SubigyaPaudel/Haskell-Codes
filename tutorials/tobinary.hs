module Tobinary where
    convert_to_binary:: Int -> Int
    convert_to_binary 1 = 1
    convert_to_binary 0 = 0
    convert_to_binary n = convert_to_binary (n `div` 2) * 10 + convert_to_binary (n `mod ` 2)

    countones:: Int -> Int
    countones a | a == 0 = 0
                | a == 1 = 1
                | otherwise = countones (a `div` 10) + countones (a `mod` 10)
 
    isEvil:: Int -> Bool
    isEvil = even . countones . convert_to_binary

    isOdious:: Int -> Bool
    isOdious = not . isEvil

    evils :: [Int]
    evils = filter isEvil [0..]

    odious:: [Int]
    odious = filter 