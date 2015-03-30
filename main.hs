data List = List
	{
		element :: Int
		, list :: List
	}
	| Maybe List

main = do
	let x = List 1 (List 4 (List (-3) Nothing))
	print (element (list x))
























