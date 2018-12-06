fRecurse :: [String] -> [String] -> [Int] -> Int
fRecurse (x:xs) origList n
        | num `elem` n = num
        | otherwise = fRecurse xs origList (n++[num])
        where   num
                        | head x == '+' = (read $ tail x) + prevNum
                        | otherwise = (negate . read $ tail x) + prevNum
                prevNum
                        | length n > 0 = last n
                        | otherwise = 0
fRecurse [] origList n = fRecurse origList origList n

f :: [String] -> Int
f x = fRecurse x x []

main :: IO ()
main = do
        contents <- readFile "input.txt"
        print . f . words $ contents
