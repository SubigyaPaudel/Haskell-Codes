module Currying where

import List_comprehension
import Recursion

upperbound_evennum :: Int -> [Int]
upperbound_evennum a = evennum 1 a
-- alternatively you could also use a lambda function
-- upperbound_evennum a = (\x -> evennum 1 x)

mapwithfixedargument :: Num(t) => (t -> t -> t) -> Integer -> t -> [t] -> Maybe [t]
mapwithfixedargument f position_of_fixed_arg fixed_arg arg_list | (position_of_fixed_arg == 1) = Just $ map (\x -> f fixed_arg x) arg_list
                                                                -- (Preferred, makes the mode more readable) 
                                                                --use a lambda function to make a new function with one or more fixed arguments
                                                                | (position_of_fixed_arg == 2) = Just $ map (`f` fixed_arg) arg_list
                                                                -- or fix the argument in the function itself, doing so just returns the modified function
                                                                | otherwise = Nothing
justPlot:: (Int -> Int) -> [(Int,Int)]
justPlot f = plot f 0 100