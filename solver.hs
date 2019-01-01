readCrossword path = do
  crossword <- readFile path
  let processedCrossword = addNumberToCrossword (lines crossword)
  return processedCrossword

readWords path = do
  words <- readFile path
  let processedWords = ignoreIrrevelantSigns (lines words)
  return processedWords

removePunc :: String -> String
removePunc xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\'") && x /= ' ']

ignoreIrrevelantSigns :: [String] -> [String]
ignoreIrrevelantSigns [] = []
ignoreIrrevelantSigns (x:xs) = removePunc x : ignoreIrrevelantSigns xs

addNumberToLetterHelper :: Int -> String -> [(Char, Int)]
addNumberToLetterHelper _ [] = []
addNumberToLetterHelper num (x:xs) = (x, num) : addNumberToLetterHelper (num + 1) xs

addNumberToCrosswordHelper :: Int -> [String] -> [[(Char, Int)]]
addNumberToCrosswordHelper _ [] = []
addNumberToCrosswordHelper row ((x):xs) = addNumberToLetterHelper (length x * row) x : addNumberToCrosswordHelper (row + 1) xs

addNumberToCrossword :: [String] -> [[(Char, Int)]]
addNumberToCrossword = addNumberToCrosswordHelper 0

printElements ::  [(Char, Int)] -> Int -> Int -> IO()
printElements [] _ _  = putStrLn ""
printElements ((a,b):c) 0 crwrdLength = do
    putStrLn([a])
    printElements c crwrdLength crwrdLength

printElements ((a,b):c) num crwrdLength = do
  putStr([a])
  printElements c (num - 1) crwrdLength
