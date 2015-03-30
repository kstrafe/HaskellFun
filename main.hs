import Data.Char

factorial	::	Integer -> Integer
factorial 		x
	| x > 0 = x * factorial (x - 1)
	| x == 0 = 1

computeHeron a b c = sqrt (s * (s - a) * (s - b) * (s - c))
	where
	q = a + b + c
	s = q / 2

que a = do
	putStrLn("This is cool")
	print a

manip :: [Integer] -> Integer
manip list
	| length list > 0 = head list + manip (tail list)
	| length list == 0 = 0 + 0

fib :: Integer => Integer
fib n
	| n == 1 = 1
	| n == 0 = 1
	| otherwise = do 1

searchBinary n array
	| otherwise = n

main = do
	putStrLn "Hello world!"
	fib 34
	name <- getLine
	main
	let r = 10000
	print (factorial r)
	print (computeHeron 2 3 3)
	print (7 - 2 == 5)
	que 100
	print (chr 45)
	print (ord '-')
	let nums = [1, 2, 3]
	print (manip nums)
	name <- getLine
	print name
	main