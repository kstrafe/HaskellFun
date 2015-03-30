data List = List 
	{
		element :: Int
		, list :: List
	}
	| End

main = do
	let x = List 1 (List 4 (List (-3) End))
	print (element (list x))

























