import System.IO
import Data.List

main = do
    contents <- readFile "input.txt"
    let l = map (map (read :: String -> Int) . words) $ lines contents
    print (length $ filter isSafe l)

isSafe :: [Int] -> Bool
isSafe v = checkSafe v 0 4 || checkSafe v (-4) 0

checkSafe :: [Int] -> Int -> Int -> Bool
checkSafe [v1, v2] min max = v2 - v1 > min && v2 - v1 < max
checkSafe (v1:v2:vs) min max = checkSafe [v1, v2] min max && checkSafe (v2:vs) min max