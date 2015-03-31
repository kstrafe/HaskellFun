generateRandomNumber :: Int -> Int
generateRandomNumber seed = mod (a * seed + c) m
	where
		a = 1103515245
		c = 12345
		m = 2^31

listify' :: String -> (String, String)
listify' (x:xs)
	| x == ',' = ([], xs)
	| otherwise = 
		let 
			result = listify' xs
			string = fst result
			rest   = snd result
		in
			if x /= ' ' then
				([x] ++ string, rest)
			else 
				(string, rest)
listify' [] = ([], [])

listify :: String -> [Int]
listify string@(x:xs) =
	let
		result = listify' string 
	in
		[read (fst result)::Int] ++ listify (snd result)
listify [] = []

main :: IO()
main = do
	line <- getLine
	print $ listify line




















