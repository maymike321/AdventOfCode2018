compareBoxes :: String -> String -> String
compareBoxes (x:xs) (y:ys)
        | x == y = [x] ++ compareBoxes xs ys
        | otherwise = compareBoxes xs ys
compareBoxes [] _ = ""
compareBoxes _ [] = ""

f :: [String] -> String
f (x:xs)
        | length matchingBoxes > 0 = head matchingBoxes
        | otherwise = f xs
        where   matchingBoxes = filter (\box -> length box == boxLength - 1) comparedBoxes
                comparedBoxes = map (compareBoxes x) xs
                boxLength = (length x)
f [] = ""

main :: IO ()
main = do
        contents <- readFile "input.txt"
        print . f . words $ contents