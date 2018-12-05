import System.IO
import Data.List.Split

main = do
        contents <- readFile "input.txt"
        print f . words $ contents
