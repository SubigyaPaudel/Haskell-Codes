map2 :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> [Integer]
map2 f x [] = []
map2 f x l = (f x (head l)) : (map2 f x (tail l))
--declaring and defining a map function made to map out the range of a function which takes two arguments
-- (continuation) since the standard map function is only defined for functions that takes one arguments. 
-- (continuation) The map2 function, keeps the first argument constant while changing the other argument.

isPrime:: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x = not (0 `elem` (map (mod x) [2..(x-1)]))
-- This function checks whether the list of remainders obtained on dividing x by every number from 2 to x-1 contains 
-- a 0 or not. If it is True, then x is not a prime number.
 
digits :: Integer -> Integer
digits x | (x < 10) = 1
digits x = 1 + digits (x `div` 10)
-- A function giving the number of digits of a given number. The number of digits is required to produce all the 
-- (continuation) rotations of a given number using the rotate function. The number should be rotated from 1 to the (digits x)
-- (continuation) times to get all the circular permutations of the number.

rotate :: Integer -> Integer -> Integer
rotate x n | (x < 10) = x
rotate x n | (x `mod` 10 == 0) = x
-- this rotate function was tailored for the isCircPrime numbers. So any number that has a zero as one of its
-- digits is never a circular prime number. Thus, for this program, such numbers dont need to be rotated.
rotate x 1 = ((x `mod` 10) * (10 ^ ((digits x) - 1))) + (x `div` 10)
-- x `mod` 10 gives the last digit of the the number(say a). a is then multiplied by an exponent of 10 
-- (continuation) which has the same number of digits as the given number. This gives a number b. 
-- (continuation) To obtain the all the digits of the given number except for the last digit, we do x `div` 10 and add it to b. 
-- (continuation) Hence the number has been rotated by a digit. 
rotate x n = rotate (rotate x (n-1)) 1

isCircPrime :: Integer -> Bool
isCircPrime x = not (False `elem` (map isPrime (map2 rotate x [1..(digits x)])))
-- The 'isCircPrime' function works by checking whether False is an element of a list containing Boolean values obtained 
-- (Continuation) by testing whether the rotation of the elements are prime numbers or not. 
-- (continuation) If any rotation of the given number is not a prime number, the function returns a False value, 
-- (continuation) indicating that the number is not a circular prime function. 














