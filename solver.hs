import Data.List
-- import Data.Char
-- import qualified Data.Set as Set

-- usun znaki interpunkcyjne (nie powinny wystapic)
removePunc :: String -> String
removePunc xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\'") && x /= ' ']

-- aplikacja removePunc dla zbioru slow
ignoreIrrelevantSigns :: [String] -> [String]
ignoreIrrelevantSigns [] = []
ignoreIrrelevantSigns (x:xs) = removePunc x : ignoreIrrelevantSigns xs

addNumberToLetterHelper :: Int -> String -> [(Char, Int)]
addNumberToLetterHelper _ [] = []
addNumberToLetterHelper num (x:xs) = (x, num) : addNumberToLetterHelper (num + 1) xs

addNumberToCrosswordHelper :: Int -> [String] -> [[(Char, Int)]]
addNumberToCrosswordHelper _ [] = []
addNumberToCrosswordHelper row ((x):xs) = addNumberToLetterHelper (length x * row) x : addNumberToCrosswordHelper (row + 1) xs

-- ponumeruj litery z planszy
addNumberToCrossword :: [String] -> [[(Char, Int)]]
addNumberToCrossword = addNumberToCrosswordHelper 0

-- wczytaj plik z opisem planszy z literami
readCrossword :: FilePath -> IO [[(Char, Int)]]
readCrossword path = do
  crossword <- readFile path
  let processedCrossword = addNumberToCrossword (lines crossword)
  return processedCrossword

-- wczytaj plik z listą słów do wykreślenia
readWords :: FilePath -> IO [String]
readWords path = do
  words <- readFile path
  let processedWords = ignoreIrrelevantSigns (lines words)
  return processedWords

-- transponuj plansze z literami
transposeCrossword :: [[(Char, Int)]] -> [[(Char, Int)]]
transposeCrossword ([]:_) = []
transposeCrossword x = map head x : transposeCrossword (map tail x)

-- usun pierwszy wiersz planszy
removeFirstRow :: [[(Char, Int)]] -> [[(Char, Int)]]
removeFirstRow [[]] = []
removeFirstRow ((x):xs) = xs

removeLastCol :: [[(Char, Int)]] -> [[(Char, Int)]]
removeLastCol [[]] = []
removeLastCol x = map init x

diagonal :: [[(Char, Int)]] -> [(Char, Int)]
diagonal [] = []
diagonal ([]:_) = []
diagonal ((x):xs) = head x : diagonal (map tail xs)

antidiagonal :: [[(Char, Int)]]
              -> [(Char, Int)]
antidiagonal [] = []
antidiagonal ([]:_) = []
antidiagonal ((x):xs) = last x : antidiagonal (map init xs)

-- -- diagonale rozpoczynające się od kolejnych kolumn
-- allDiagonals :: [[(Char, Int)]] -> [[(Char, Int)]]
-- allDiagonals [] = []
-- allDiagonals x = [diagonal x] ++ [reverse (antidiagonal x)] ++ allDiagonals (removeFirstRow x)

-- getAllCombinations :: [[(Char, Int)]] -> [[(Char, Int)]]
-- getAllCombinations [[]] = []
-- getAllCombinations crossword = crossword ++ transposeCrossword crossword ++ allDiagonals crossword ++ allDiagonals (transposeCrossword crossword)

-- printElements :: [(Char, Int)] -> Int -> Int -> IO()
-- printElements [] _ _  = putStrLn ""
-- printElements ((a,b):c) 0 crwrdLength = do
--     putStrLn([a])
--     printElements c crwrdLength crwrdLength

-- printElements ((a,b):c) num crwrdLength = do
--   putStr([a])
--   printElements c (num - 1) crwrdLength

-- printList :: String ->  IO()
-- printList [] = do putStrLn ""
-- printList (x:xs) = do putStr [x]
--                       printList xs

-- printSolution :: [String] -> IO()
-- printSolution [] = do putStr ""
-- printSolution (x:xs) = do printList x
--                           printSolution xs

lol = [[('A',1),('B',2),('C',3)],[('D',4),('E',5),('F',6)],[('G',7),('H',8),('I',9)]]
chuj = reverse ( antidiagonal lol )
chujstwo = removeLastCol lol

antiDiagonals :: [[(Char, Int)]] -> [[(Char, Int)]]
antiDiagonals [] = []
antiDiagonals x = [antidiagonal x] ++ [reverse (antidiagonal x)] ++ antiDiagonals (removeLastCol x)

-- diagonale rozpoczynające się od kolejnych kolumn
allDiagonals :: [[(Char, Int)]] -> [[(Char, Int)]]
allDiagonals [] = []
-- allDiagonals x = [reverse (antidiagonal x)] ++ allDiagonals (removeFirstRow x)
allDiagonals x = [diagonal x] ++ [reverse ( diagonal x )] ++ [antidiagonal x] ++ [reverse (antidiagonal x)] ++ allDiagonals (removeFirstRow x)

getAllCombinations :: [[(Char, Int)]] -> [[(Char, Int)]]
getAllCombinations [[]] = []
getAllCombinations crossword = crossword ++ transposeCrossword crossword ++ allDiagonals crossword ++ allDiagonals (transposeCrossword crossword) ++ antiDiagonals crossword ++ antiDiagonals (transpose crossword)

findString' search str = findIndex (isPrefixOf search) (tails str)

test = findString "CDE" [('A', 1),('B', 2),('C' ,3),('C',4),('E',5),('F', 6)]

--
findString :: String           -- ^ string to search for
            -> [(Char, Int)]   -- ^ row to search in
            -> Maybe [Int]     -- ^ list of indexes
findString search str = do
  idx <- findString' search (map fst str)
  return (map snd . take (length search) . drop idx $ str)

-- Find word in crossword
findWord :: String              -- ^ word
          -> [[(Char, Int)]]    -- ^ all combinations of crossword
          -> Maybe [Int]        -- ^ list of indexes
findWord _ [] = Nothing
-- findWord [] _ = Nothing
-- findWord word [x] = findString word x
-- findWord word (x:xs) = if (findString word x) == Nothing 
findWord word (x:xs) = if (findString word x) == Nothing
                        then findWord word xs
                      else findString word x

removeLetter' :: [(Char, Int)] -> Int -> [(Char, Int)]
removeLetter' [] _ = []
removeLetter' ((a,b):c) idx | b == idx = [('.',b)] ++ c
                            | otherwise = [(a,b)] ++ (removeLetter' c idx)

removeLetter :: [[(Char, Int)]]  -- ^ crossword
              -> Int             -- ^ index of letter to remove
              -> Int             -- ^ number of cols in crossword
              -> Int
              -> [[(Char, Int)]] -- ^ updated crossword
removeLetter [] _ _ _= []
removeLetter [x] idx cols _ = [removeLetter' x idx]
removeLetter (x:xs) idx cols colsConst | idx < 0 = (x:xs)
                                       | idx <= cols = ((removeLetter' x idx): xs)
                                       | otherwise = (x: (removeLetter xs idx (cols+colsConst+1) colsConst) )


collectIndexes :: [Int]         -- ^ indexes of word to remove
               -> [Int]       -- ^ set to update
               -> [Int]       -- ^ updated set
collectIndexes xs set = set ++ xs

removeWord :: [[(Char, Int)]]    -- ^ crossword
              -> [Int]           -- ^ indexes of letter to remove
              -> Int             -- ^ number of cols in crossword
              -> [[(Char, Int)]] -- ^ updated crossword
removeWord cross [] cols = cross
removeWord cross (x:xs) cols = removeWord (removeLetter cross x cols cols) xs cols

solve :: [[(Char, Int)]]  -- ^ crossword
      -> [String]         -- ^ list of words to find
      -> [Int]            -- ^ list of indexes to remove
      -> [Int]            -- ^ updated list of indexes to remove
solve cross [] idx = idx
solve cross (w:ws) idx = do
  case findWord w (getAllCombinations cross) of
    Just n -> idx ++ n --removeWord cross n 11
    Nothing -> error w--idx
  ++ solve cross ws idx

removeIndexes :: [[(Char, Int)]]  -- ^ crossword
              -> [Int]            -- ^ indexes to remove
              -> [[(Char, Int)]]  -- ^ solved crossword
-- set: delete duplicates
removeIndexes cross idxs = removeWord cross (nub idxs) (length (head cross) - 1)


makeCrossString :: [[(Char, Int)]] -> String
makeCrossString [] = []
makeCrossString (x:xs) = makeCrossString' x ++ "\n" ++ makeCrossString xs

makeCrossString' :: [(Char, Int)] -> String
makeCrossString' [] = []
makeCrossString' ((a,b):xs) = a : makeCrossString' xs

main :: IO ()
main = do
  putStrLn "Welcome to puzzle Solver. Give me a data set [1,2,3]"
  setNumber <- getLine
  let crosswordPath | setNumber == "1" = "data1/crossword"
                    | setNumber == "2" = "data2/crossword"
                    | setNumber == "3" = "data3/crossword"
                    | otherwise = error "No such data set"
  let wordsPath     | setNumber == "1" = "data1/words"
                    | setNumber == "2" = "data2/words"
                    | setNumber == "3" = "data3/words"
                    | otherwise = error "No such data set"
  cross <- readCrossword crosswordPath
  words <- readWords wordsPath
  --print words
  putStrLn (makeCrossString (removeIndexes cross (solve cross words [])))
