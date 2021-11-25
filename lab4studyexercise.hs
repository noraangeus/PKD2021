{-	returnSameList n list
	Takes an Int and a list of Int and returns the very same list.
	EXAMPLES: returnSameList 0 [] == []
			  returnSameList 5 [1, 2] == [1, 2]
-}
returnSameList :: Int -> [Int] -> [Int]
returnSameList 0 list = list
returnSameList n list = returnSameList (n-1) list



{-	countOccur e xs
	Counts the number of occurences of a given element in a list.
	PRE: Eq n
	RETURNS: The number of occurences.
	EXAMPLES:	countOccur 3 [1,2,3,4,3,6,8,3] == 3
				countOccur 
-}
countOccur :: (Eq a) => a -> [a] -> Int
countOccur _ [] = 0
countOccur e (x:xs) 
 | e == x = 1 + (countOccur e xs)
 | otherwise = countOccur e xs


 {-	evenOrOdd xs
 	Returns a list of numbers the same length as a given lit of numbers,
 	but with zeroes in place of the even numbers and 1 in place of the odd.
 	EXAMPLES:	evenOrOdd [] == []
 				evenOrOdd [1,2,3,4,5,6,7,8] == [1,0,1,0,1,0,1,0]
 -}
evenOrOdd :: [Int] -> [Int]
evenOrOdd [] = []
evenOrOdd (x:xs) = [x `mod` 2] ++ evenOrOdd xs

{-	toBinary n
	Converts an Int to a string representing its 
	corresponding binary number.
	PRE:
	EXAMPLES:
-}
toBinary :: Int -> String
toBinary 


{-	toDecimal num
	Converts a string representing to a binary number 
	to its corresponding Int
	PRE:
	EXAMPLES:
-}
toDecimal :: String -> Int










convert 0 = "0"
convert 1 = "1"
convert 2 = "2"
convert 3 = "3"
convert 4 = "4"
convert 5 = "5"
convert 6 = "6"
convert 7 = "7"
convert 9 = "9"
--convert x > 10 = convert(x % 10)