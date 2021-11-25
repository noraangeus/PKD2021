--här har vi lite kod som jag håller på och fuckar runt med och inte vill bli av med
--du behöver inte bry dig om den här filen alls egentligen

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

sortBook :: Document -> Sentence
sortBook lst = concat lst

wordCount :: Document -> WordTally
wordCount lst = wordCountAux (sortBook(lst))

wordCountAux :: Sentence -> WordTally
wordCountAux [] = []
wordCountAux (x:xs) = (x,(length (filter (== x) xs)+1)) : wordCountAux (filter (/= x) xs)


adjacentPairs :: Document -> Pairs
adjacentPairs [] = []
adjacentPairs (sentence:rest) = adjacentPairsAux sentence ++ adjacentPairs rest


adjacentPairsAux :: Sentence -> Pairs
adjacentPairsAux [] = []
adjacentPairsAux [x] = []
adjacentPairsAux (x:y:xs) = (x,y) : adjacentPairsAux (y:xs)


initialPairs :: Document -> Pairs
initialPairs [] = []
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

--neighbours :: PairsTally -> String -> WordTally
--neighbours [] _ = []
--neighbours (x:xs) keyword = neighboursAux 0 x keyword ++ neighbours xs

--neighboursAux acc [] _ = []
--neighboursAux acc (a,b) keyword
--  | keyword == a = (b, acc+1)
--  |- keyword == b = (a, acc+1)
--  | otherwise =
--    then pairsCountAux (acc + 1) (a,b) xs
--    else pairsCountAux (acc) (a,b) xs