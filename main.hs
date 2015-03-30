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

main = do
	let x = Node "Top!" (Node "left" Empty Empty) (Node "right" Empty Empty)
	let y = 
		if 100 * z > 3000 then
			Just 10
		else
			Nothing 
		where
		z = 33
	print y
	print (find x "right")





















