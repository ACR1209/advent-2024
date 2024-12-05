import Data.List (isInfixOf, transpose, tails, nub)

readWordSearch :: IO ([String])
readWordSearch = do
    contents <- readFile "input.txt"
    return (lines contents)

generatePermutations :: [String] -> [[String]]
generatePermutations pattern = 
    let rotations = take 4 $ iterate rotate pattern
        reflections = map reflect rotations
    in nub (rotations ++ reflections)

rotate :: [String] -> [String]
rotate = reverse . transpose

reflect :: [String] -> [String]
reflect = map reverse


matchesAt :: [String] -> [String] -> Int -> Int -> Bool
matchesAt grid pattern startRow startCol =
  all (\(r, row) -> 
         all (\(c, ch) -> 
                ch == '.' || (grid !! (startRow + r)) !! (startCol + c) == ch
             )
             (zip [0..] row)
      )
      (zip [0..] pattern)

countPattern :: [String] -> [String] -> Int
countPattern grid pattern =
  let rows = length grid
      cols = length (head grid)
      patRows = length pattern
      patCols = length (head pattern)
      permutations = generatePermutations pattern
  in sum [ length [() | r <- [0..rows - patRows], 
                        c <- [0..cols - patCols], 
                        matchesAt grid perm r c]
         | perm <- permutations ]

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

    putStrLn "\n\n\nDay 4: Second puzzle"
    putStrLn "--------------------"
    let pattern = [ "M.S", ".A.", "M.S" ]
    let patternCount = countPattern wordSearch pattern

    putStr "Number X-MAS: "
    putStrLn $ show patternCount