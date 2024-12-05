import Data.List (isInfixOf, transpose, tails)

readWordSearch :: IO ([String])
readWordSearch = do
    contents <- readFile "input.txt"
    return (lines contents)

findWord :: String -> [String] -> [String]
findWord s p = filter (checkIn s) (rows p ++ cols p ++ dial p ++ diar p)
    where checkIn word line = word `isInfixOf` line || reverse word `isInfixOf` line
          rows puzzle = puzzle
          cols puzzle = transpose puzzle
          dial puzzle = diagonals puzzle
          diar puzzle = diagonals (map reverse puzzle)

diagonals :: [String] -> [String]
diagonals xs = [ [ xs !! (y - x) !! x 
                 | x <- [max 0 (y - length (head xs) + 1) .. min (length xs - 1) y] 
                 ] 
               | y <- [0 .. length xs + length (head xs) - 2] 
               ]

countTimesWordAppersOnString :: String -> String -> Int
countTimesWordAppersOnString word toCheck = countIn word toCheck
    where 
        countIn w t = length . filter (\x -> isInfixOf w x || isInfixOf (reverse w) x) . map (take (length w)) . tails $ t

main :: IO() 
main = do
    putStrLn "Day 4: First puzzle"
    putStrLn "-------------------"

    wordSearch <- readWordSearch

    let placesWhereWordPresent = findWord "XMAS" wordSearch
    let numWord = foldr (+) 0 (map (countTimesWordAppersOnString "XMAS") placesWhereWordPresent)

    putStr "Number of XMAS: "
    putStrLn $ show $ numWord