

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

add :: Int -> Int -> Int
add x y = x + y

tmpl :: [some_template] -> Int -> Int
tmpl (x:xs) y
	| null xs = 0
	| otherwise = tmpl xs (y + 1) + y 

-- Basic list ops: head, tail, tails, null, length, last, init
-- concat, ++, reverse, and, or, all, any, 
-- sublists: take, splitAt, takeWhile, dropWhile, span, break, filter
-- isPrefixOf, isInfixOf, isSuffixOf, zip, zipWith
-- String handling: lines, words, unwords
-- Data.Char : toUpper, toLower
-- map, foldl, foldr

lambda :: Integer -> Integer
lambda = (\x -> x * x)

addOne :: Num a => a -> a
addOne x = x + 1
subOne x = x - 1
square x = x * x

data Wot = 
	A
	| B
	| C

qwerty :: Wot -> Wot -> Bool
qwerty _ A = False
qwerty A _ = True

main :: IO()
main = do
	let x = A
	print (qwerty x x)
	print ((addOne . square) 3)















