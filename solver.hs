import Data.List
import Data.Char
import qualified Data.Set as Set
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

--
antidiagonal :: [[(Char, Int)]]
              -> [(Char, Int)]
antidiagonal [] = []
antidiagonal ([]:_) = []
antidiagonal ((x):xs) = last x : antidiagonal (map init xs)

-- diagonale rozpoczynające się od kolejnych kolumn
allDiagonals :: [[(Char, Int)]] -> [[(Char, Int)]]
allDiagonals [] = []
-- allDiagonals x = [reverse (antidiagonal x)] ++ allDiagonals (removeFirstRow x)
allDiagonals x = [diagonal x] ++ [antidiagonal x] ++ [reverse (antidiagonal x)] ++ allDiagonals (removeFirstRow x)

getAllCombinations :: [[(Char, Int)]] -> [[(Char, Int)]]
getAllCombinations [[]] = []
getAllCombinations crossword = crossword ++ transposeCrossword crossword ++ allDiagonals crossword ++ allDiagonals (transposeCrossword crossword)

findString' :: String          -- ^ string to search for
            -> String          -- ^ string to search in
            -> Maybe Int       -- ^ starting index
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
findWord word (x:xs) = if (findString word x) == Nothing
                        then findWord word xs
                      else findString word x


removeLetter' :: [(Char, Int)] -> Int -> [(Char, Int)]
removeLetter' [] _ = []
removeLetter' ((a,b):c) idx | b == idx = [('*',b)] ++ c
                            | otherwise = [(a,b)] ++ (removeLetter' c idx)

-- Remove letter with given index form crossword
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

-- -- Remove letters with given list of numbers form crossword
-- removeWord :: [[(Char, Int)]]    -- ^ crossword
--               -> Maybe [Int]     -- ^ indexes of letter to remove
--               -> Int             -- ^ number of cols in crossword
--               -> [[(Char, Int)]] -- ^ updated crossword
-- removeWord cross (Just []) cols = cross
-- removeWord cross xs cols = case xs of
--   Just n -> removeWord (removeLetter cross (head n) cols) (Just (tail n)) cols
--   Nothing -> cross

-- Collect all found words (list of indexes)
collectIndexes :: [Int]         -- ^ indexes of word to remove
               -> [Int]       -- ^ set to update
               -> [Int]       -- ^ updated set
collectIndexes xs set = set ++ xs 
-- collectIndexes [] set = set 
-- collectIndexes [x] set = Set.insert x set 
-- collectIndexes (x:xs) set = map f xs where f = 
--   Set.insert x set
--   collectIndexes xs set

-- updateSet :: [Int] -> Set.Set Int -> Set.Set Int
-- updateSet idxs set = return (collectIndexes idxs set)
-- updateSet idxs set = do
--   let updatedSet = collectIndexes idxs set
--   return updatedSet



-- Remove letters with given list of numbers form crossword
removeWord :: [[(Char, Int)]]    -- ^ crossword
              -> [Int]           -- ^ indexes of letter to remove
              -> Int             -- ^ number of cols in crossword
              -> [[(Char, Int)]] -- ^ updated crossword
removeWord cross [] cols = cross
removeWord cross (x:xs) cols = removeWord (removeLetter cross x cols cols) xs cols

-- Solve the crossword
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

-- solve cross words =
--   case findWord "JULIET" (getAllCombinations cross) of
--     Just n -> removeWord cross n 11
--     Nothing -> error "world not found"

-- Remove all indexes from crossword
removeIndexes :: [[(Char, Int)]]  -- ^ crossword
              -> [Int]            -- ^ indexes to remove
              -> [[(Char, Int)]]  -- ^ solved crossword
-- set: delete duplicates
removeIndexes cross idxs = removeWord cross (nub idxs) 11


makeCrossString :: [[(Char, Int)]] -> String
makeCrossString [] = []
makeCrossString (x:xs) = makeCrossString' x ++ "\n" ++ makeCrossString xs

makeCrossString' :: [(Char, Int)] -> String
makeCrossString' [] = []
makeCrossString' ((a,b):xs) | a /= '*' = a : makeCrossString' xs
                            | otherwise = makeCrossString' xs

main :: IO ()
main = do
  cross <- readCrossword "data1/crossword"
  words <- readWords "data1/words"
  -- print words
  print (allDiagonals cross)
  print (makeCrossString (removeIndexes cross (solve cross words [])))
  -- print (solve cross ["JULIET", "CARESS","WEDDING", "ROMEO", "BOUQUETTE"] [])
  --print ( handle (solve cross ["JULIET", "CARESS"]) ([0..(16*12)]) (length(head cross)))
