

main :: IO()
main = do
	print "Hey world"
	temp <- readLn
	putStrLn temp