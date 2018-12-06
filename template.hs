main :: IO ()
main = do
        contents <- readFile "input.txt"
        print . f . words $ contents