readReports :: IO [[Int]]
readReports = do
    contents <- readFile "input.txt"

    let linesOfFiles = lines contents
    let reports = map parseLine linesOfFiles
    return reports

parseLine :: String -> [Int]
parseLine line = 
    let wordsList = words line
    in map read wordsList :: [Int]

isValidReport :: [Int] -> Bool
isValidReport report = allDecrease || allIncrease
    where
        allDecrease = all (\(x, y) -> x >= y && abs (x - y) <= 3 && abs (x - y) >= 1) (zip report (tail report))
        allIncrease = all (\(x, y) -> x <= y && abs (x - y) <= 3 && abs (x - y) >= 1) (zip report (tail report))

main :: IO ()
main = do
    putStrLn "Day 2: First puzzle"
    putStrLn "-------------------"

    reports <- readReports
    let validityOfReports = map isValidReport reports
    let safeReports = length $ filter (id) validityOfReports 

    putStr "Number of safe reports: "
    putStrLn $ show safeReports
