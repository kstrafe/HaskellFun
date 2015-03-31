generateRandomNumber :: Int -> Int
generateRandomNumber seed = mod (a * seed + c) m
	where
		a = 1103515245
		c = 12345
		m = 2^31

generateRandomNumbers :: Int -> Int -> [Int]
generateRandomNumbers seed amount
	| amount > 0 =
		let
			generated = generateRandomNumber seed
		in
			[generated] ++ generateRandomNumbers generated (amount - 1)
	| otherwise = []

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

accumulate' :: [Int] -> Int -> [Int]
accumulate' (x:xs) sum =
	let
		next = sum + x
	in
		[next] ++ accumulate' xs next
accumulate' [] _ = []

accumulate :: [Int] -> [Int]
accumulate array@(_:_) = accumulate' array 0
accumulate [] = []

data Node =
	Node 
	{
		identifier :: String
		, left_child :: Node
		, right_child :: Node
	} 
	| End
	deriving (Show)

class N a where
	printTree :: a -> IO()

instance N Node where
	printTree node = print node 

merge :: [Int] -> [Int] -> [Int]
merge array1@(x:xs) array2@(y:ys)
	| x < y = [x] ++ merge xs array2
	| otherwise = [y] ++ merge array1 ys 
merge array1 [] = array1
merge [] array2 = array2

mergeSort :: [Int] -> [Int]
mergeSort array@(x:xs)
	| length array > 2 = merge (mergeSort first_half) (mergeSort second_half)
	| length array == 2 = if x < y then [x, y] else [y, x] 
	| length array == 1 = [x]
	where
		y = head xs
		half_length = div (length array) 2
		splitted = splitAt half_length array
		first_half = fst splitted
		second_half = snd splitted
mergeSort [] = []

main :: IO()
main = do
	print (mergeSort [1,3,4,2,-1])
	x <- getLine
	let y = (read x)::Int
	print (Just 5 >>= (\x -> if (x == 0) then fail "zero" else Just (x + 1)))
	print ([1..12] >>= (\x -> [x * 2]))
	print $ True > False
	print $ Just $ Node "H" End End
	let node = Node "Hey" (Node "derp" End End) End
	let mega = print "DD"
	printTree node
	line <- getLine
	let listified = listify line 
	print listified
	let cumulative = accumulate listified
	print cumulative
	print $ Prelude.foldl (+) 0 listified 
	print $ sum listified
	print 'a' >> print 'c' >> print 'b'
	















