read' :: [Char] -> Int
read' ['A'] = 10
read' ['B'] = 11
read' ['C'] = 12
read' ['D'] = 13
read' ['E'] = 14
read' ['F'] = 15
read' x = read(x) :: Int
--a function defined in order to deal with hexadecimal values

readBase :: Int -> String -> Int
readBase b [] = 0
readBase 16 n = (read' (last n:[])) + (b * (readBase b (init n)))
-- a function definition designed specially for hexadecimal values
readBase b n = (read (last n:[])) + (b * (readBase b (init n)))
 