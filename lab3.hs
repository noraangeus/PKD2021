-- {- updatePersonName (String, String, String, Integer, String) Int String
     -- Updates a datatabase with name and adress information about people. Specifically, changes the first or last name.
     --PRE: Input must follow argument type specification.
     --RETURNS: A tuple containing updated name and adress info.
     --EXAMPLES: updatePersonName  ("Angeus", "Nora", "Radmansgatan", "75432", "Osthammar") 1 "Lexie" -> ("Angeus","Lexie","Radmansgatan","75432","Osthammar")
     -- updatePersonName  ("Angeus", "Nora", "Radmansgatan", "75432", "Osthammar") 3 "Lexie" -> ("Angeus","Nora","Radmansgatan","75432","Osthammar")
--  -}
  
updatePersonName :: (String, String, String, Integer, String) -> Int -> String -> (String, String, String, Integer, String)
updatePersonName (fn, _, sa, pn, pa) 1 z = (fn, z, sa, pn, pa)
updatePersonName (_, gn, sa, pn, pa) 2 z = (z, gn, sa, pn, pa)
updatePersonName tuple _ _ = tuple

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
     VARIANT: abs n - signum av n + 1
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


{-	myInit xs
	Returns everything but the last element of a (non-empty) list.
	VARIANT: length xs
	EXAMPLES:	myInit [] == []
				myInit [1] == []
				myInit [1,2,3,4] [1,2,3]
				myInit ["dog", "cat", "chicken"] == ["dog","cat"]
-}
myInit :: [a] -> [a]
myInit [] = []
myInit [x] = []
myInit (x:xs) = [x] ++ myInit(xs)

{-	searchString mainstring substring
	Returns the position of substring in mainstring.
	VARIANT: length substring
	RETURNS: An Int representing the position
	if substring exists as an infix in mainstring.
	Otherwise it returns -1.
	EXAMPLES: 	searchString "jultomte" "tomte" == 3
				searchString "abrakadabra" "bra" == 1
				searchString "abrakadabra" "gran" == (-1)
				searchString "jultomte" "jul"
0
-}

isSubstring :: Integer -> String -> String -> Integer
isSubstring acc _ [] = acc
isSubstring acc [] _ = (-1)
isSubstring acc (x:xs) (y:ys)
  | x == y = isSubstring acc xs ys
  |otherwise = isSubstring (acc+1) xs (y:ys) 

searchString :: String -> String -> Integer
searchString (x:xs) (y:ys) = isSubstring 0 (x:xs) (y:ys)


{-	fromDecimals xs
	Converts a list of decimals into the corresponding integer value.
	PRE: All integer in xs must be from 0 to 9
	VARIANT: length xs
	EXAMPLES:	fromDecimals [] == 0 
				fromDecimals [4,2] == 42
				 fromDecimals [1,3,3,7] == 1337
-}

fromDecimals :: [Integer] -> Integer
fromDecimals [] = 0
fromDecimals [0] = 0
fromDecimals [1] = 1
fromDecimals [2] = 2
fromDecimals [3] = 3
fromDecimals [4] = 4
fromDecimals [5] = 5
fromDecimals [6] = 6
fromDecimals [7] = 7
fromDecimals [8] = 8
fromDecimals [9] = 9
fromDecimals (x:xs) = (fromDecimals [x]) * power 10 (length (x:xs)-1) + fromDecimals xs

power :: Integer -> Int -> Integer
power acc 0 = 1
power acc n
  | odd n = acc * power acc (n-1)
  | otherwise = let x = power acc (n `div` 2) in x * x


{-	squareOfEven1 xs
	Returns the squares of all even numbers in a given list.
	VARIANT: length xs
	EXAMPLES:	squareOffEven1 [] == []
				squareOffEven1 [1,3,5] == []
				squareOffEven1 [1,2,3,4] == [4,16]
-}
squareOffEven1 :: [Integer] -> [Integer]
squareOffEven1 [] = []
squareOffEven1 (x:xs) = if even x then [x*x] ++ squareOffEven1 xs else squareOffEven1 xs


{-	sqareOffEven2 xs
	Returns the squares of all even numbers in a given list.
	VARIANT: length xs
	EXAMPLES:	squareOffEven2 [] == []
				squareOffEven2 [1,3,5] == []
				squareOffEven2 [1,2,3,4] == [4,16]
-}
squareOffEven2 :: [Integer] -> [Integer]
squareOffEven2 [] = []
squareOffEven2 (x:xs) = [x*x | x <- (x:xs), even x]




split :: [Integer] -> ([Integer], [Integer])
split xs =
  let threshold = length xs `div` 2
    in (take threshold xs, drop threshold xs)


merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | y < x = y : merge (x:xs) ys
  | otherwise = x : merge xs (y:ys)

mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let
    (xs1,xs2) = split xs
  in
    merge (mergeSort xs1) (mergeSort xs2)

insert k [] = [k]
insert k (x:xs) =
  if k < x then
    k : x : xs
  else
    x:(insert k xs)

insertionSortAux sorted [] = sorted
insertionSortAux sorted (x:xs) =
  insertionSortAux (insert x sorted) xs

insertionSort xs =
  insertionSortAux [] xs
