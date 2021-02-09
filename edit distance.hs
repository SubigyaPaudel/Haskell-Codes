module Distance where

distance1 :: Eq t => [t] -> [t] -> Int
distance1 a [] = length a
distance1 [] b = length b
distance1 a b | ((head a) == (head b)) = distance1 (tail a) (tail b)
			  | otherwise = 1 + distance1 ((head b):a) b

distance2 :: Eq t => [t] -> [t] -> Int
distance2 a [] = length a
distance2 [] b = length b			  
distance2 a b | ((head a) == (head b)) = distance2 (tail a) (tail b)
			  | otherwise = 1 + distance2 (tail a) b

distance3 :: Eq t => [t] -> [t] -> Int
distance3 a [] = length a
distance3 [] b = length b		  
distance3 a b | ((head a) == (head b)) = distance3 (tail a) (tail b)
			  | otherwise = 1 + distance3 ((head b):(tail a)) b

ed :: Eq t => [t] -> [t] -> Int 			  
ed a b = 1 + minimum [(distance1 a b), (distance2 a b), (distance3 a b)]
			  
			  

			 
