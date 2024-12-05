import Data.List

{-
    Execute the program on the same directory as the input.txt file, as this uses relative pathing.
-}

-- Read the input file and parse it into a list of tuples 
readLists :: IO ([(Integer, Integer)])
readLists = do
    contents <- readFile "input.txt"

    let linesOfFiles = lines contents
        parseLine line = case words line of
            [left, right] -> ((read left) :: Integer,  (read right) :: Integer)
            _ -> error "Line does not contain exactly two words"
        parsedLists = map parseLine linesOfFiles
    return parsedLists

-- Separate the tuples into two lists
separateLists :: [(Integer, Integer)] -> ([Integer], [Integer])
separateLists lists = (map fst lists, map snd lists)

-- Sort the two lists
sortLists :: ([Integer], [Integer]) -> ([Integer], [Integer])
sortLists (list1, list2) = (sort list1, sort list2)

main :: IO ()
main = do
    rawLists <- readLists
    let lists = sortLists $ separateLists rawLists
    let distances = zipWith (-) (fst lists) (snd lists)
    
    putStrLn $ show $ sum $ map abs distances