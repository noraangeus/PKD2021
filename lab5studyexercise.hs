{-	only_even 
-}

only_even_not :: [Int] -> [Int]
only_even_not (x:xs)
  | x `mod` 2 == 0 = only_even_not xs ++ [x]

only_even :: [Int] -> [Int]
only_even (x:xs) = [x | x <- (x:xs), x `mod` 2 == 0]

{- select_second
-}
select_secondv1 :: [a] -> a 
select_secondv1 [] = error "empty"
select_secondv1 [a] = error "not enough"
select_secondv1 (x:y:xs) = y

select_secondv2 :: [a] -> a
select_secondv2 [] = error "empty"
select_secondv2 [a] = error "not enough"
select_secondv2 [x,y] = y
select_secondv2 (x:xs) = head xs

{- fib
-}
fibslow 0 = 1
fibslow 1 = 1
fibslow x = fib (x-1) + fib (x-2)

fibslowish = (fibs!!)
  where fibs = map fib [0..]

fib :: Int -> Int
fib n = fib' 0 1 n
    where fib' acc1 acc2 n | n <= 1 = acc2
                           | otherwise = fib' acc2 (acc1+acc2) (n-1)

{-	isElementOf n list
	Checks whether one Int is an element of a list of Int.
	RETURNS: True if (and only if) the chosen Int is an element of the list.
	EXAMPLES: 	isElementOf 3 [] == False
				isElementOf 3 [1,2,3] == True
				isElementOf 3 [4,5,6] == False
-}

isElementOf :: Int -> [Int] -> Bool

isElementOf n [] = False
isElementOf n (x:xs) = n == x || isElementOf n xs

{-	countElement e xs
	Counts the number of occurences of a given element in a list.
	PRE: Eq n
	VARIANT: length xs
	RETURNS: The number of occurences.
	EXAMPLES:	countElement 3 [1,2,3,4,3,6,8,3] == 3
				countElement 
-}
countElement :: (Eq a) => a -> [a] -> Int
countElement _ [] = 0
countElement e (x:xs) 
 | e == x = 1 + (countElement e xs)
 | otherwise = countElement e xs

{-	getElementList n xs
	Returns a list of all occurences of your given Int in the list xs.
	VARIANT: length xs
	EXAMPLES:	getElementList 4 [] == []
				getElementList 0 [1,2,3,4] == []
				getElementList 4 [1,2,3,4,5,6,4,7,5,4] == [4,4,4]
-}
getElementList :: Int -> [Int] -> [Int]
getElementList _ [] = []
getElementList n (x:xs)
  | n == x = [n] ++ getElementList n xs
  | otherwise = getElementList n xs

{-	removeElement n xs
	Returns a list of all elements in a list xs of Ints except your given Int n.
	VARIANT: length xs
	EXAMPLES:	removeElement 2 [] == []
				removeElement 2 [1,2,3,4] == [1,3,4]
-}
--removeElement :: Int -> [Int] -> [Int]
removeElement _ [] = []
removeElement n (x:xs)
  | n == x = removeElement n xs
  | otherwise = [x] ++ removeElement n xs


{-	getLastN n xs
	Returns a string with the last n characters of xs.
	VARIANT: n
	EXAMPLES: 	getLastN 2 [] == ""
				getLastN 5 "jultomte" == "tomte"
-}

getLastN :: Int -> String -> String
getLastN _ [] = []
getLastN n (x:xs) | length (x:xs) <= n = (x:xs)
                  | otherwise = getLastN n xs


-- quicksort
-- VARIANT: everything in low < p, everything in high > p
partition :: Integer -> [Integer] -> ([Integer], [Integer])
partition _ [] = ([], [])
partition p  (x:xs) =
  let
    (low, high) = partition p xs
  in
    if x < p then (x:low, high) else (low, x:high)

quicksort :: [Integer] -> [Integer]
quicksort [] = []
-- no more base case needed
quicksort (x:xs) =
  let 
    (low, high) = partition x xs
  in 
    (quicksort low) ++ x:(quicksort high)
