import System.Environment (getArgs)

merge :: [Int] -> [Int] -> [Int]
merge array1@(x:xs) array2@(y:ys)
	| x < y = [x] ++ merge xs array2
	| otherwise = [y] ++ merge array1 ys 
merge array1 [] = array1
merge [] array2 = array2

mergeSort :: [Int] -> [Int]
mergeSort array@(x:xs)
	| len > 2 = merge (mergeSort first_half) (mergeSort second_half)
	| len == 2 = if x < y then [x, y] else [y, x] 
	| len == 1 = [x]
	where
		y = head xs; len = length array
		half_length = div len 2
		splitted = splitAt half_length array
		first_half = fst splitted
		second_half = snd splitted
mergeSort [] = []

data I = I 
	{
		num :: Int
	}

class What a where
	conquer :: a -> Int

instance What I where
	conquer input = (num input) * 2

data X a = Y Int String a
	deriving (Show, Eq)

gnam :: X n -> String
gnam (Y a b c) = b

data Kanskje a = Ingenting | Bare a
	deriving(Show)

instance Monad Kanskje where
	Bare a >>= f = f a
	Ingenting >>= _ = Ingenting
	return a = Bare a

fun :: (Show a) => (Eq a) => a -> Kanskje a
fun a = return a

printAll :: [String] -> IO()
printAll (x:xs) = do
	print x
	printAll xs
printAll [] = print ""

fun2 :: Int -> Int
fun2 a = 
	case a of
		1 -> 2

main :: IO()
main = do
	print $ (Bare 2) >>= fun
	a <- System.Environment.getArgs
	printAll a
	print (mergeSort [1,3,4,2,-1,2,-3,9,0,1,3,-2])
	print (conquer (I 32))










