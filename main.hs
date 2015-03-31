generateRandomNumber :: Int -> Int
generateRandomNumber seed = mod (a * seed + c) m
	where
		a = 1103515245
		c = 12345
		m = 2^31

-- Turn a list like "92,43,65" into [Int]
listify' :: String -> (String, String)
listify' (x:xs)
	| x == ',' = ([], xs)
	| otherwise = 
		let 
			result = listify' xs
			string = fst result
			rest   = snd result
		in
			([x] ++ string, rest)
listify' [] = ([], [])

main :: IO()
main = do




















