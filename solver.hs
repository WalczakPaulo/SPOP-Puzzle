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

transpose :: [[(Char, Int)]] -> [[(Char, Int)]]
transpose [[]] = []
transpose x = map head x : transpose (map tail x)

getAllCombinations :: [[(Char, Int)]] -> [[(Char, Int)]]
getAllCombinations [[]] = []
getAllCombinations crossword = crossword ++ transpose crossword

printElements ::  [(Char, Int)] -> Int -> Int -> IO()
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
