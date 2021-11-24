{-	fromTo a b
	Returns a sorted list of integers
	from the smallest given integer to the largest.
	VARIANT: high - low + 1
	EXAMPLES: fromTo 7 9 == [7,8,9]
	fromTo 9 6 == []
	fromTo 6 6 == [6]
-}
 
 fromTo :: Integer -> Integer -> [Integer]
 fromTo a b =
   if b > a then (fromTo a (b-1) ++ [b])
   else if (b == a) then [a]
   else []



{-	isSubstring main sub
	Checks whether one string is an infix of another string.
	RETURNS: True if, and only if, the string is an infix.
	EXAMPLES:	isSubstring "" "tomte" == False
				isSubstring "tomte" "" == True
				isSubstring "jultomte" "jul" == True
				isSubstring "jultomte" "gran" == False
-}

 isSubstring :: String -> String -> Bool
 isSubstring _ [] = True
 isSubstring [] _ = False
 isSubstring (x:xs) (y:ys)
   | x == y = isSubstring xs ys
   |otherwise = isSubstring xs (y:ys)


 {-	stringOfInteger n
 	Translates an integer into a string.
 	EXAMPLES:	stringOfInteger 0 == "0"
 				stringOfInteger 405 == "405"
 				stringOfInteger (-42) == "-42"
 -}

 stringOfInteger :: Integer -> String
 stringOfInteger 0 = "0"
 stringOfInteger 1 = "1"
 stringOfInteger 2 = "2"
 stringOfInteger 3 = "3"
 stringOfInteger 4 = "4"
 stringOfInteger 5 = "5"
 stringOfInteger 6 = "6"
 stringOfInteger 7 = "7"
 stringOfInteger 9 = "9"
 stringOfInteger 10 = stringOfInteger (10 `div` 10) ++ stringOfInteger(10 `mod` 10)
 stringOfInteger n
   | n > 10 = stringOfInteger(n `div` 10) ++ stringOfInteger(n `mod` 10)
 stringOfInteger n
   | n < 0 = "-" ++ stringOfInteger (abs n)

--VARIANT: abs n - signum av n + 1