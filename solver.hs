import Data.List

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

diagonal :: [[(Char, Int)]] -> [(Char, Int)]
diagonal [] = []
diagonal ([]:_) = []
diagonal ((x):xs) = head x : diagonal (map tail xs)

-- diagonale rozpoczynające się od kolejnych kolumn
allDiagonals :: [[(Char, Int)]] -> [[(Char, Int)]]
allDiagonals [] = []
allDiagonals x = [diagonal x] ++ allDiagonals (removeFirstRow x)

getAllCombinations :: [[(Char, Int)]] -> [[(Char, Int)]]
getAllCombinations [[]] = []
getAllCombinations crossword = crossword ++ transposeCrossword crossword ++ allDiagonals crossword ++ allDiagonals (transposeCrossword crossword)

printElements :: [(Char, Int)] -> Int -> Int -> IO()
printElements [] _ _  = putStrLn ""
printElements ((a,b):c) 0 crwrdLength = do
    putStrLn([a])
    printElements c crwrdLength crwrdLength

printElements ((a,b):c) num crwrdLength = do
  putStr([a])
  printElements c (num - 1) crwrdLength

printList :: String ->  IO()
printList [] = do putStrLn ""
printList (x:xs) = do putStr [x]
                      printList xs

printSolution :: [String] -> IO()
printSolution [] = do putStr ""
printSolution (x:xs) = do printList x
                          printSolution xs


findString' :: String          -- ^ string to search for
            -> String          -- ^ string to search in
            -> Maybe Int       -- ^ starting index
findString' search str = findIndex (isPrefixOf search) (tails str)

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
findWord [] _ = Nothing
findWord word (x:xs) = if (findString word x) == Nothing
                        then findWord word xs
                      else findString word x

removeLetter' :: [(Char, Int)] -> Int -> [(Char, Int)]
removeLetter' [] _ = []
removeLetter' ((a,b):c) idx | b == idx = c
                            | otherwise = [(a,b)] ++ (removeLetter' c idx)

-- Remove letter with given index form crossword 
removeLetter :: [[(Char, Int)]]  -- ^ crossword
              -> Int             -- ^ index of letter to remove
              -> Int             -- ^ number of cols in crossword
              -> [[(Char, Int)]] -- ^ updated crossword
removeLetter xs idx cols | idx < 0 = xs
                         | idx < cols = ((removeLetter' (head xs) idx):(tail xs))
                         | otherwise = ((head xs):(removeLetter (tail xs) idx (cols+cols)))

-- -- Solve the crossword
-- solve :: [[(Char, Int)]]  -- ^ crossword
--       -> [String]         -- ^ list of words to find
--       -> String           -- ^ solution
-- solve cross words = do
--   all <- getAllCombinations cross

main :: IO ()
main = do
  cross <- readCrossword "data1/crossword"
  words <- readWords "data1/words"
  -- printElements (head cross) 11 11
  -- print (removeLetter cross 12 11)
  -- print (head (removeFirstRow cross))
  -- print (findString "ROMEO" (head (removeFirstRow cross)))
  -- print . getAllCombinations $ cross
  print(findWord "RYE" (getAllCombinations cross)) 

