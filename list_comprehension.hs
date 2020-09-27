oddnum:: Int -> Int -> [Int]
oddnum a b = [x | x <- [a..b], x `mod` 2 == 1]
-- gives the odd numbers from a to b

evennum:: Int -> Int -> [Int]
evennum a b = [x | x <- [a..b], x `mod` 2 == 0]
-- gives the even numbers from a to b

factors :: Int -> [Int]
factors a = [x | x <- [1..a], mod a x == 0]
--gives the factors of a number

isPrime :: Int -> Bool
isPrime a = (length [x | x <- [2..(a-1)], a `mod` x == 0]) == 0
--checks if the given number is prime

primefactors :: Int -> [Int]
primefactors a = [x | x <- [2..a-1], mod a x == 0, isPrime(x)]

intersection :: [Int] -> [Int] -> [Int]
intersection a b = [x | x <- [minimum (a++b) .. maximum (a++b)], (x `elem` a) && (x `elem` b)]
-- gives the common elements of the two lists

union :: [Int] -> [Int] -> [Int]
union a b = [x | x <- [minimum (a++b) .. maximum (a++b)], (x `elem` a) || (x `elem` b)]
-- gives the list of elements belonging to either the list a or b. Discards duplicates

plot :: (Int -> Int) -> Int -> Int -> [(Int, Int)]
plot f a b = [(x,y) | x <- [a..b], y <- (map f [a..b]), y == (f x)]
-- gives the graphical coordinates defining the function f 

pythagorean_traids :: Int -> [(Int, Int, Int)]
pythagorean_traids a = [ (x,y,z) | x <- [1..a], y <- [x..a], z <- [y..a], x ^ 2 + y ^ 2 == z ^ 2]
-- gives the pythagorean triads upto the given number
