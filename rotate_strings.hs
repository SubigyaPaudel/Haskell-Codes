headset :: [char] -> [char]
headset [] = []
headset x = (head x):[]

len :: [char] -> Integer
len [] = 0
len z = 1 + len (tail z)

rotate :: [char]-> [Integer] -> [char]
rotate [] _ = []
rotate (a:[]) n = [a]
rotate x 0 = x
rotate x 1 = tail x ++ headset x 
rotate x n = rotate 1 (rotate (n-1) x)

circle :: [char] -> [[char]]
circle [] = []
circle x = map (rotate x) [1 .. (len x)]


