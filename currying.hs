module Currying where

import List_comprehension

upperbound_evennum :: Int -> [Int]
upperbound_evennum a = evennum 1 a
-- alternatively you could also use a lambda function
-- upperbound_evennum a = (\x -> evennum 1 x)

mapwithfixedargument :: (t -> t -> t) -> Integer -> t -> [t] -> [t]
mapwithfixedargument f position_of_fixed_arg fixed_arg arg_list | (position_of_fixed_arg == 1) = map (\x -> f fixed_arg x) arg_list
                                                                | (position_of_fixed_arg == 2) = map (\x -> f x fixed_arg) arg_list
                                                                | otherwise = []

