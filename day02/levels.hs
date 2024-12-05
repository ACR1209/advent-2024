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
        pairedLevels = zip report (tail report)
        allDecrease = all isDecreaseWithinRange pairedLevels
        allIncrease = all isIncreaseWithinRange pairedLevels

isDecreaseWithinRange :: (Int, Int) -> Bool
isDecreaseWithinRange (x, y) = x >= y && abs (x - y) <= 3 && abs (x - y) >= 1

isIncreaseWithinRange :: (Int, Int) -> Bool
isIncreaseWithinRange (x, y) = x <= y && abs (x - y) <= 3 && abs (x - y) >= 1

isValidReportWithProblemDampener :: [Int] -> Bool
isValidReportWithProblemDampener report = isValidReport report || ((length $ getInvalidLevels report) <= ((length report) - 1)) 

getInvalidLevels :: [Int] -> [Int]
getInvalidLevels report = [report !! i | i <- [0..(length report - 1)], not (isValidReport (removeAtIndex i report))]

removeAtIndex :: Int -> [a] -> [a]
removeAtIndex i xs = let (ys, zs) = splitAt i xs in ys ++ (tail zs)

numberOfSafeReports :: [Bool] -> Int
numberOfSafeReports reportsValidity = length $ filter (id) reportsValidity

main :: IO ()
main = do
    putStrLn "Day 2: First puzzle"
    putStrLn "-------------------"

    reports <- readReports
    let validityOfReports = map isValidReport reports
    let safeReports = numberOfSafeReports validityOfReports 

    putStr "Number of safe reports: "
    putStrLn $ show safeReports

    putStrLn "\n\n\nDay 2: Second puzzle"
    putStrLn "--------------------"

    let validityOfReportsWithDampener = map isValidReportWithProblemDampener reports
    let safeReportsDampener = numberOfSafeReports validityOfReportsWithDampener

    putStr "Number of safe reports with dampener: "
    putStrLn $ show safeReportsDampener 