readWords path = do
  words <- readFile path
  let processedWords = ignoreIrrevelantSigns (lines words)
  return processedWords

removePunc :: String -> String
removePunc xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\'") && x /= ' ']
ignoreIrrevelantSigns :: [String] -> [String]
ignoreIrrevelantSigns [] = []
ignoreIrrevelantSigns (x:xs) = removePunc x : ignoreIrrevelantSigns xs
