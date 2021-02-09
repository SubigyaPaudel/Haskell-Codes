--Haskell codes showing the use of recursion, pattern matching and guards
module Recursion where

len:: [a] -> Int
len [] = 0
len (a:ax) = 1 + len ax
--gives the length of a list

power:: Float -> Int -> Float
power _ 0 = 1
power x a = x * power x (a-1)
--exponentiates a number with an integer index

multiply:: Float -> Int -> Float
multiply _ 0 = 0
multiply a b = a + multiply a (b-1)
--multiplies a non-integer number and an integer

factorial:: Int -> Int
factorial a | (a == 0) = 0
            | otherwise = a * factorial (a-1)
-- gives the factorial of a function

count:: Eq(t) => t -> [t] -> Int
count _ [] = 0
count a b | (a == (head b)) = 1 + count a (tail b) 
          | otherwise = count a (tail b)

fibonacci_number:: Int -> Int
fibonacci_number a | a == 1 = 0
                   | a == 2 = 1
                   | otherwise = fibonacci_number (a-1) + fibonacci_number (a-2)
-- gives the ath number in the fibonacci sequence

removething:: Eq(t) => t -> [t] -> [t]
removething _ [] = []
removething thing list| (head list) == thing = tail list
                      | otherwise = (head list):(removething thing (tail list))
-- removes a specified item from the list

ffmap:: Show(t) => (t -> t -> t) -> [t] -> [t] -> [t]
ffmap _ _ [] = []
ffmap _ [] _ = []
ffmap f a b = [f (head a) (head b)] ++ ffmap f (tail a) (tail b)
--similar to map but for functions that take 2 arguments instead of 1

-- counts the number of times an item appears in a list
find_string_helper:: [Char] -> [Char] -> Int -> Int -> [Char]
-- finds a pattern in a text, parameters are pattern. Arguments are:
-- 1. The pattern to be searched
-- 2. The text within which the pattern is searched for
-- 3. The number of characters that have matched
-- 4. The current position in the text
-- called via find_string for user friendliness
find_string_helper [] _ _ _ = "No pattern provided"
find_string_helper _ [] _ _ = "No text provided"
find_string_helper pattern text matched position | (matched == (len pattern)) = "String lies in between " ++ show(position - matched) ++ " and " ++ show(position - 1)
                                                 | (position > (len text - len pattern) && matched == 0) = "Pattern not found"
                                                 | (pattern !! matched) == (text !! position) = find_string_helper pattern text (matched + 1) (position + 1)
                                                 | otherwise = find_string_helper pattern text 0 (position + 1)

find_string:: [Char] -> [Char] -> [Char]
find_string a b = find_string_helper a b 0 0
-- just a user friendly version of find_string_helper
