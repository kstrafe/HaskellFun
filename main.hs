
main :: IO()
main = do
	print "Hey world"
	temporary <- getLine
	print temporary
	if temporary == "derp" then
		print "HEEEEEEEEY AYYYYYY"
	else
		print "'no'"
		