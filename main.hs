merge :: [Int] -> [Int] -> [Int]
merge array1@(x:xs) array2@(y:ys)
	| x < y = [x] ++ merge xs array2
	| otherwise = [y] ++ merge array1 ys 
merge array1 [] = array1
merge [] array2 = array2

mergeSort :: [Int] -> [Int]
mergeSort array@(x:xs)
	| len > 2 = merge (mergeSort first_half) (mergeSort second_half)
	| len == 2 = if x < y then [x, y] else [y, x] 
	| len == 1 = [x]
	where
		y = head xs; len = length array
		half_length = div (length array) 2
		splitted = splitAt half_length array
		first_half = fst splitted
		second_half = snd splitted
mergeSort [] = []

main :: IO()
main = print (mergeSort [1,3,4,2,-1,2,-3,9,0,1,3,-2])











