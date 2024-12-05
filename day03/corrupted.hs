import Data.List (isPrefixOf)

data Expr = 
    Mul Int Int
    deriving (Show)

eval :: Expr -> Int
eval (Mul x y) = x * y

readMemory :: IO (String)
readMemory = do
    readFile "input.txt"

readMulOperations :: String -> [Expr]
readMulOperations s = getAllValidMulExprOfString s

removeDontStrings :: String -> String
removeDontStrings s = removeDontStrings' s False
    where
        removeDontStrings' [] _ = []
        removeDontStrings' str@(x:xs) inDont
                | "don't()" `isPrefixOf` str = removeDontStrings' (drop 6 str) True
                | "do()" `isPrefixOf` str = removeDontStrings' (drop 4 str) False
                | inDont = removeDontStrings' xs True
                | otherwise = x : removeDontStrings' xs False

parseMulExpr :: String -> (String, Maybe Expr)
parseMulExpr s
    | "mul(" `isPrefixOf` s && ')' `elem` s =
        let inner = takeWhile (/= ')') . drop 4 $ s
            (xStr, yStr) = break (== ',') inner
            x = case reads xStr of
                [(n, "")] -> Just n
                _ -> Nothing
            y = if null yStr
                then Nothing
                else case reads (tail yStr) of
                    [(n, "")] -> Just n
                    _ -> Nothing
            rest = drop (length xStr + length yStr + 6) s
        in case (x, y) of
            (Just x', Just y') -> (rest, Just $ Mul x' y')
            _ -> (s, Nothing)
    | otherwise = (s, Nothing)

getAllValidMulExprOfString :: String -> [Expr]
getAllValidMulExprOfString [] = []
getAllValidMulExprOfString s = 
    case parseMulExpr s of
        (_, Just expr) -> expr : getAllValidMulExprOfString (tail s)
        _ -> getAllValidMulExprOfString (tail s)


evalExpressionTree :: [Expr] -> Int
evalExpressionTree exprs = sum $ map eval exprs 

main :: IO()
main = do
    putStrLn "Day 3: First puzzle"
    putStrLn "-------------------"

    memoryData <- readMemory

    let mulOps = readMulOperations memoryData

    putStr "Result of operations: "
    putStrLn $ show $ evalExpressionTree mulOps

    putStrLn "\n\n\nDay 3: Second puzzle"
    putStrLn "--------------------"


    let mulOpsWithDos = readMulOperations $ removeDontStrings memoryData

    putStr "Result of operations: "
    putStrLn $ show $ evalExpressionTree mulOpsWithDos
