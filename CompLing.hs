-- written by Edvard Axelman and Nora Angéus
-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS

sortBook :: Document -> Sentence
sortBook lst = mergeSort (concat lst)

{- wordCount []  
Counts how many elements there are in a list
PRE: A list that contains lists that contains elements.  
RETURNS: A list that contains tuples that contains a String and an Integer. 
EXAMPLES: [["a", "rose", "is", "a", "rose"],["but", "so", "is", "a", "rose"]] -> [("rose", 3), ("a", 3), ("is", 2), ("but", 1), ("so", 1)]
          [] -> "Error the list is empty"
VARIANT: The functions ends when the list is empty. 
 -} 
wordCount :: Document -> WordTally
wordCount lst = wordCountAux (sortBook(lst))

wordCountAux :: Sentence -> WordTally
--wordCountAux [] = [()]
--wordCountAux [x] = (x,1)
wordCountAux (x:xs) = (x,length (filter (== x) xs)) : wordCountAux (filter (/= x) xs)


adjacentPairs :: Document -> Pairs
adjacentPairs [] = []
adjacentPairs (sentence:rest) = adjacentPairsAux sentence ++ adjacentPairs rest

adjacentPairsAux :: Sentence -> Pairs
adjacentPairsAux [] = []
adjacentPairsAux [x] = []
adjacentPairsAux (x:y:xs) = (x,y) : adjacentPairsAux (y:xs)


initialPairs :: Document -> Pairs
initialPairs = [] = []
initialPairs ((a:b:xs):ys) = (a,b) : initialPairs ys

finalPairs :: Document -> Pairs
finalPairs [] = []
finalPairs (y:ys) = 
   let 
     (a:b:xs) = ( reverse y)
   in
     (b,a) : finalPairs ys
     

pairsCount :: Pairs -> PairsTally
pairsCount [] =  []
pairsCount [x] = pairsCountAux 1 x []
pairsCount (x:xs) = pairsCountAux 0 x (x:xs) ++ pairsCount (removePairs x xs)


pairsCountAux :: Int -> (String, String) -> Pairs -> PairsTally 
pairsCountAux acc (a,b) []  = [((a,b), acc)]
pairsCountAux acc (a,b) (x:xs) = 
  if (a,b) == x || (b,a) == x 
    then pairsCountAux (acc + 1) (a,b) xs
    else pairsCountAux (acc) (a,b) xs

removePairs :: (String, String) -> Pairs -> Pairs
removePairs _ [] = []
removePairs (a,b) (x:xs) =
  if (a,b) == x || (b,a) == x
    then removePairs (a,b) xs
    else x : removePairs (a,b) xs

neighbours :: PairsTally -> String -> WordTally
neighbours = undefined  -- remove "undefined" and write your function here

mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour = undefined  -- remove "undefined" and write your function here



-- Test Cases
-- feel free to add other test cases here. an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])
test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a",2) (wordCount [["a","b"],["a"]]))

-- adjacentPairs, initialPairs, finalPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"],["bar"]]) 

test3a = TestCase $ assertEqual "initialPairs" [("a","b")] (initialPairs [["a","b","a"],["c"]])
                      
test3b = TestCase $ assertEqual "finalPairs" [("b","a")] (finalPairs [["a","b","a"],["c"]])
                      

-- pairsCount
test4 = TestCase $ assertBool "pairsCount simple" 
            (elem (("a","b"), 2) (pairsCount [("a","b"),("c","d"),("a","b")]))
test5 = TestCase $ assertBool "pairsCount tricky" 
             (let x = pairsCount (adjacentPairs [["a","b","a"],["c"]]) in 
                      elem (("a","b"), 2) x || elem (("b","a"), 2) x)

-- neighbours
test6 = TestCase $ assertEqual "neighbours left" [("b",2)] 
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "a") 

test7 = TestCase $ assertEqual "neighbours left" [("a",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "b") 

-- mostCommonNeighbour
test8 = TestCase $ assertEqual "mostCommonNeighbour text \"the\"" (Just "fun") 
                                                                  (mostCommonNeighbour input "the") 
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

test9 = TestCase $ assertEqual "mostCommonNeighbour text \"spam\"" 
                      Nothing (mostCommonNeighbour input "spam")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

-- testing the PandP.austin text
test10 = TestCase $ assertEqual "mostCommonNeighbour of \"bennet\"" 
            (Just "mr") (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet") 

-- for running all the tests (type "runtests" within ghci --- without the quotes)
runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7,test8,test9,test10]




