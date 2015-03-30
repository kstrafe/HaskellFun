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

searchBinary' :: Int -> [Int] -> Int -> Int -> Int
searchBinary' search_for array min max
	| max < min = -1
	| array!!middle < search_for = searchBinary' search_for array (middle + 1) max 
	| array!!middle > search_for = searchBinary' search_for array min (middle - 1)
	| otherwise = middle
	where
	middle = div (max + min) 2

searchBinary :: Int -> [Int] -> Int
searchBinary search_for array = searchBinary' search_for array 0 ((length array) - 1)

main = do
	let r = [1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 22]
	print "standby"
	print (searchBinary (-32) r)
	print (searchBinary 0 r)
	print (searchBinary 1 r)
	print (searchBinary 2 r)
	print (searchBinary 3 r)
	print (searchBinary 4 r)
	print (searchBinary 5 r)
	print (searchBinary 6 r)
	print (searchBinary 7 r)
	print (searchBinary 8 r)
	print (searchBinary 9 r)
	print (searchBinary 10 r)
	print (searchBinary 11 r)
	print (searchBinary 16 r)
	print (searchBinary 22 r)
	print (searchBinary 25 r)


















