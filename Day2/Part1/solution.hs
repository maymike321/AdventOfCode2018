exactlyX :: Int -> String -> String -> Bool
exactlyX num orig (x:xs) = (length . filter (\char -> char == x)) orig == num || exactlyX num orig xs
exactlyX _ _ [] = False

exactlyTwo :: String -> String -> Bool
exactlyTwo = exactlyX 2

exactlyThree :: String -> String -> Bool
exactlyThree = exactlyX 3

fRecurse :: [String] -> Int -> Int -> Int
fRecurse (x:xs) a b
    | exactlyTwo x x && exactlyThree x x = fRecurse xs (a + 1) (b + 1)
    | exactlyTwo x x = fRecurse xs (a + 1) b
    | exactlyThree x x = fRecurse xs a (b + 1)
    | otherwise = fRecurse xs a b
fRecurse [] a b = a * b

f :: [String] -> Int
f x = fRecurse x 0 0

main :: IO ()
main = do
        contents <- readFile "input.txt"
        print . f . words $ contents