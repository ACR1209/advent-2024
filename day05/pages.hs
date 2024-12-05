import qualified Data.Map as M 

readPages :: IO (([String], [String]))
readPages = do
    contents <- readFile "input.txt"
    let fileLines = lines contents
    let (rules, updates) = (takeWhile (/= "") fileLines, tail $ dropWhile (/= "") fileLines)
    return (rules, updates)

getHashOfRules :: [String] -> M.Map String [Int]
getHashOfRules rules = foldr (\x acc -> let (k, v) = (takeWhile (/= '|') x, map read $ words $ tail $ dropWhile (/= '|') x) 
                                    in M.insertWith (++) k v acc) M.empty rules

-- Taken from: https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

getUpdatePages :: String -> [String]
getUpdatePages s = wordsWhen (==',') s

isUpdateValid :: [String] -> [String] -> M.Map String [Int] -> Bool
isUpdateValid [] _  _ = True
isUpdateValid (currentUpdatePage:otherPages) verified rules = if M.member currentUpdatePage rules then (if isInsertable then goToNext else False) else goToNext 
    where 
        goToNext =  isUpdateValid otherPages (currentUpdatePage : verified) rules
        lookupPage = M.lookup currentUpdatePage rules
        isInsertable = case lookupPage of
            (Just xs) -> if all (\x -> show x `notElem` verified) xs then True else False
            _ -> True

getMiddlePage :: [String] -> Int
getMiddlePage pages = map (\x -> read x :: Int) pages !! (length pages `div` 2)

main :: IO ()
main = do
    putStrLn "Day 4: First puzzle"
    putStrLn "-------------------"
    (rules, updates) <- readPages

    let rulesHash = getHashOfRules rules

    let updatePages = map getUpdatePages updates

    let correctPages = filter (\x -> isUpdateValid x [] rulesHash) updatePages

    let middlePages = map getMiddlePage correctPages

    putStr "The sum of the middle pages is: "
    putStrLn $ show $ sum middlePages
