

generateRandomNumber :: Int -> Int
generateRandomNumber seed = mod (a * seed + c) m
	where
		a = 1103515245
		c = 12345
		m = 2^31

checkAnswer :: Int -> Int -> IO()
checkAnswer correct guess
	| correct == guess = do
		print "Dayum son, you won!"
		try correct (-1)
	| otherwise = do
		print "That's incorrect, try again"

try :: Int -> Int -> IO()
try correct tries
	| tries == -1 = do
		print "You won!"
	| tries == 0 = do
		print "Game over man..."
	| otherwise = do
		print "Guess the number: "
		input <- getLine

		checkAnswer correct (read input)
		print tries
		try correct (tries - 1)
		print ""

goIterate' :: Int -> IO()
goIterate' seed = do
	let number = generateRandomNumber seed
	try number 4
	goIterate' number

goIterate :: IO()
goIterate = goIterate' 124988

data X = X 
	{
		head :: Int
		, tail :: X
	} | End

add :: Int -> Int -> Int
add x y = x + y

main :: IO()
main = do
	print (1 `add` 2)
