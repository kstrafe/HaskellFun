data BinTree = Node {
	bintreedescription :: String
	, left_child :: BinTree
	, right_child :: BinTree
	}
	| Empty
	deriving (Show, Eq)

-- Attempting to use DFS to find a specific string
find :: BinTree -> String -> Bool
find x y
	| x == Empty = False
	| (bintreedescription x) == y = True
	| otherwise = find (left_child x) y || find (right_child x) y 

fun :: Int -> Int
fun a = b a + 1
	where
		b 0 = 8
		b 1 = 10
		b n = 1

main = do
	let x = Node "Top!" (Node "left" Empty Empty) (Node "right" Empty Empty)
	
	let y = 
		if result > 300 then
			Just 300
		else
			Just result 
		where
			z = 33
			max = 3000
			result = 100 * z
	

	print (fun 0)

	print y
	print (find x "right")





















