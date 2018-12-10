import Data.List
import Data.List.Split

f :: [String] -> Int
f squareStrings = getSharedSquares (sortBy sortSquares squares) (-1, -1) False 0
        where squares = concat (map createRectangle squareStrings)

getSharedSquares :: [(Int, Int)] -> (Int, Int) -> Bool -> Int -> Int
getSharedSquares (square:squares) lastSquare lastSquareAlreadyTakenTwice sharedSquares
        | not lastSquareAlreadyTakenTwice && square == lastSquare = getSharedSquares squares square True (sharedSquares + 1)
        | lastSquareAlreadyTakenTwice && square == lastSquare = getSharedSquares squares square True sharedSquares
        | otherwise = getSharedSquares squares square False sharedSquares
getSharedSquares [] _ _ x = x

sortSquares :: (Int, Int) -> (Int, Int) -> Ordering
sortSquares (x1, y1) (x2, y2)
        | x1 > x2 = GT
        | x1 < x2 = LT
        | x1 == x2 && y1 > y2 = GT
        | x1 == x2 && y1 < y2 = LT
        | otherwise = EQ

createRectangle :: String -> [(Int, Int)]
createRectangle string = cartProd [topLeftX..bottomRightX] [topLeftY..bottomRightY]
        where   topLeftX = read $ head cornerStringValues :: Int
                topLeftY = read . init $ last cornerStringValues :: Int
                bottomRightX = topLeftX + (read $ head sizeValues) - 1
                bottomRightY = topLeftY + (read $ last sizeValues) - 1
                cornerStringValues = splitOn "," $ ((words string)!!2)
                sizeValues = splitOn "x" . last $ words string

cartProd :: [a] -> [a] -> [(a,a)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

main :: IO ()
main = do
        contents <- readFile "input.txt"
        print . f . splitOn "\n" $ contents