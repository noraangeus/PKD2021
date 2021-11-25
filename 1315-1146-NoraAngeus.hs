{-  matchNthChar n str str
     Checks if two strings agree in their n:th character
   PRE: 0 <= n <= length str 
   RETURNS: True if the characters match, and False if they do not 
   EXAMPLES: matchNthChar 3 "kaka" "baka" == True
             matchNthChar 8 "scientific" "halloween" == False
             matchNthChar 8 "scientific" "cable" == Exception: Prelude.!!: index too large

 -}
matchNthChar :: Int -> String -> String -> Bool
--matchNthChar n str1 str2 = str1!!(n-1) == str2!!(n-1)
matchNthChar n x y | length x >= n && length y >= n && length x > 0 && length y > n = x!!(n-1) == y!!(n-1)
