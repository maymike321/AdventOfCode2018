import System.IO
import Data.List.Split

f :: [String] -> Int
f [] = 0
f (x:xs)
    | head x == '+' = read num + f xs
    | otherwise = f xs - read num
    where num = tail x
main = do
        contents <- readFile "input.txt"
        print . f . words $ contents
