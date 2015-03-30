data Les = Les 
	{
		num :: Int
		, rest :: Les
	}
	| End
	deriving (Show, Eq)

createNLes :: Int -> Les
createNLes number
	| number > 0 = Les number (createNLes (number - 1))
	| otherwise = End

sumLes :: Les -> Int
sumLes l 
	| rest l == End = num l
	| otherwise = num l + sumLes (rest l) 

deep :: [[Int]] -> [Int]
deep ((x:xs):(xss)) = [x] ++ xs ++ deep xss
deep [] = []

some :: IO()
some = do
	print "eey"
	input <- readLn
	putStrLn "input" 

main = do
	some
	print (deep [[3,4,1],[5,9]])
	let x = createNLes 100
	print (sumLes x)

	let z = [3, 9, 2, 1, 5, 3, 8, 2, 1, 3, -9]
	print (head (sort z)) 




















