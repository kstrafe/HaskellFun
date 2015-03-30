merge :: [Int] -> [Int] -> [Int]
merge (l:lhs) (r:rhs)
	| l < r = [l] ++ merge lhs (r:rhs) 
	| otherwise = [r] ++ merge (l:lhs) rhs
merge [] (r:rhs) = r:rhs
merge (l:lhs) [] = l:lhs

mergeHeavy :: [[Int]] -> [[Int]]
mergeHeavy (x:xs)
	| length (tail xs) > 0 = mergeHeavy ([merge x (head xs)] ++ mergeHeavy (tail xs))
	| otherwise = [merge x (head xs)]
mergeHeavy [] = []

sortsub2 :: Int -> Int -> [Int]
sortsub2 l r
	| l < r = [l, r]
	| otherwise = [r, l]

sort2 :: [[Int]] -> [[Int]]
sort2 (x:xs)
	| length (tail x) > 0 = [(sortsub2 first second)] ++ sort2 xs
	| otherwise = [[first]]
	where
		first = head x
		second = head (tail x)		
sort2 [] = [[]]

-- Split the arrays to be sorted into tiny arrays
split :: [Int] -> [[Int]]
split (x:xs)
	| length xs == 0 = [[x]]
	| otherwise = [[x, head xs]] ++ split (tail xs)
split [] = []

sort :: [Int] -> [[Int]]
sort array = mergeHeavy (sort2 (split array))

main = do
	let x = [2, 9, 10, 13]
	let y = [1, 3, 4, 5, 12]
	let z = [3, 9, 2, 1, 5, 3, 8, 2]
	print (merge x y)
	print (split z)
	print (sort2 (split z))
	print (sort z) 





















