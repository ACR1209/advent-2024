import Data.List (isPrefixOf)

data Expr = 
    Mul Int Int
    deriving (Show)

eval :: Expr -> Int
eval (Mul x y) = x * y

readMulOperations :: IO [Expr]
readMulOperations = do
    contents <- readFile "input.txt"
    let validMuls = getAllValidMulExprOfString contents

    return validMuls

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


main :: IO()
main = do
    putStrLn "Day 3: First puzzle"
    putStrLn "-------------------"

    mulOps <- readMulOperations

    putStr "Result of operations: "
    putStrLn $ show $ sum $ map eval mulOps