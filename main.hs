

merge :: [Int] -> [Int] -> [Int]
merge (l:lhs) (r:rhs)
	| l < r = [l] ++ merge lhs (r:rhs) 
	| otherwise = [r] ++ merge (l:lhs) rhs
merge [] (r:rhs) = r:rhs
merge (l:lhs) [] = l:lhs

-- Split the arrays to be sorted into tiny arrays
split :: [Int] -> [[Int]]
split (x:xs)
	| length xs == 0 = [[x]]
	| otherwise = [[x, head xs]] ++ split (tail xs)
split [] = []

main = do
	let x = [2, 9, 10, 13]
	let y = [1, 3, 4, 5, 12]
	print (merge x y)

	print (split [3, 9, 2, 1, 5])






















