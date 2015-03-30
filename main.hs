data List = List
	{
		element :: Int
		, list :: List
	}
	| End
	deriving (Show, Eq)

sort2 :: Int -> Int -> [Int]
sort2 a b
	| a < b = [a, b]
	| otherwise = [b, a]

merge :: [Int] -> [Int] -> [Int]
merge (l:lhs) (r:rhs)
	| l < r = [l] ++ merge lhs (r:rhs) 
	| otherwise = [r] ++ merge (l:lhs) rhs 

-- Splits into x y pairs and creates 1 big sawtooth list
msSplit :: [Int] -> [Int]
msSplit (x:xs) = sort2 x (head xs) ++ msSplit (tail (tail xs))
msSplit [] = []

-- Attempting to merge sort
-- First we must split
sort :: [Int] -> [Int]
sort a = msSplit a

main = do
	let x = [6, 5, 3, 9]
	print (sort x)






















