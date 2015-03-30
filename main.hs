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
		b n
			| n > 10 = 10
			| otherwise = 1

favorite :: String
favorite = "Daaayum son where'd you find this?!"

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

	let z = 
		let z = 100 * 32
		in z

	print z

	print (fun 100)
	print favorite

	print y
	print (find x "right")





















