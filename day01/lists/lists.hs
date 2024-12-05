import Data.List

{-
    Execute the program on the same directory as the input.txt file, as this uses relative pathing.
-}

-- Read the input file and parse it into a list of tuples 
readLists :: IO ([(Int, Int)])
readLists = do
    contents <- readFile "input.txt"

    let linesOfFiles = lines contents
        parseLine line = case words line of
            [left, right] -> ((read left) :: Int,  (read right) :: Int)
            _ -> error "Line does not contain exactly two words"
        parsedLists = map parseLine linesOfFiles
    return parsedLists

-- Separate the tuples into two lists
separateLists :: [(Int, Int)] -> ([Int], [Int])
separateLists lists = (map fst lists, map snd lists)

-- Sort the two lists
sortLists :: ([Int], [Int]) -> ([Int], [Int])
sortLists (list1, list2) = (sort list1, sort list2)

-- Count ocurrences of a number in a list   
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

main :: IO ()
main = do
    putStrLn "Day 1: First puzzle"
    putStrLn "-------------------"
    
    rawLists <- readLists
    let lists = sortLists $ separateLists rawLists
    let distances = zipWith (-) (fst lists) (snd lists)
    
    putStr "Difference score: "
    putStrLn $ show $ sum $ map abs distances

    putStrLn "\n\n\nDay 1: Second puzzle"
    putStrLn "--------------------"

    let howManyTimes = map (\x -> (x, count x (snd lists))) (fst lists)
    putStr "Similarity score: "
    putStrLn $ show $ sum $ map (\(x, y) -> x * y) howManyTimes
