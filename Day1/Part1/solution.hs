f :: [String] -> Int
f [] = 0
f (x:xs)
    | head x == '+' = read num + f xs
    | otherwise = f xs - read num
    where num = tail x
    
main :: IO ()
main = do
        contents <- readFile "input.txt"
        print . f . words $ contents
