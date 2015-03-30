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

splitLines' :: String -> String
splitLines' (x:xs)
	| x == '\n' = ""
	| length xs > 0 = [x] ++ splitLines' xs
	| otherwise = ""
splitLines' [] = ""

cutNewline :: String -> String
cutNewline (x:xs)
	| x == '\n' = xs
	| length xs > 0 = cutNewline xs
	| otherwise = xs
cutNewline [] = ""

splitLines :: String -> [String]
splitLines (x:xs)
	| length xs > 0 = [splitLines' (x:xs)] ++ splitLines (cutNewline (x:xs))
	| otherwise = [splitLines' (x:xs)]
splitLines [] = []

main = do
	print (splitLines' "Heey\nBae\n")
	print (deep [[3,4,1],[5,9]])
	let x = createNLes 100
	print (sumLes x)




















