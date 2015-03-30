

merge :: [Int] -> [Int] -> [Int]
merge (l:lhs) (r:rhs)
	| l < r = [l] ++ merge lhs (r:rhs) 
	| otherwise = [r] ++ merge (l:lhs) rhs 


main = do
	let x = [2, 9]
	let y = [1, 3]
	print (merge x y)






















