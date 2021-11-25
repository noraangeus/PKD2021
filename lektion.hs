{-  abc k str1 str2
     Checks if the prefix  in one string is equal to the suffix of a second string.
   RETURNS: True if the prefix (with length k) and suffix (with length k) match, and False if they do not 
   EXAMPLES: abc 2 "kaka" "baka == True
   			 abc 3 "hej" "tjolahopp" == False
   			 abc 15 "kaka" "baka" == False
   			 abc 15 "kaka" "kaka" == True
   			 abc (-1) "hej" "hejpådej" == True
   			 abc 1 "" "" == True
 -}

abc :: Int -> String -> String -> Bool

abc k s1 s2 = take k s1 == drop (length s2 - k) s2

{- ghi number1 number2 divider
	Checks for a common divider between two numbers.
	PRE: divider cannot be 0
	RETURNS: True if both number1 and number2 is divisible by divider, otherwise False
	EXAMPLES: ghi 6 4 2 == True
			  ghi 7 4 2 == False
			  ghi 0 (-4) 2 == True
-}
ghi :: Int -> Int -> Int -> Bool
ghi a b x = a `mod` x == 0 && b `mod` x == 0 