import System.IO
import Data.List

main = do
    contents <- readFile "input.txt"
    let l = map (map (read :: String -> Int) . words) $ lines contents
    print (length $ filter checkSafeDamp l)

checkSafeDamp :: [Int] -> Bool
checkSafeDamp [v1, v2, v3, v4] = isSafe [v1, v2, v3, v4] || isSafe [v2, v3, v4] || isSafe [v1, v3, v4] || isSafe[v1, v2, v4] || isSafe[v1, v2, v3]
checkSafeDamp (v1:v2:v3:v4:vs)
    | isSafe [v1, v2, v3, v4] = checkSafeDamp (v2:v3:v4:vs)
    | isSafe [v1, v2, v3] = isSafe(v1:v2:v3:vs)
    | isSafe [v1, v2, v4] = isSafe(v1:v2:v4:vs)
    | isSafe [v1, v3, v4] = isSafe(v1:v3:v4:vs)
    | isSafe [v2, v3, v4] = isSafe(v2:v3:v4:vs)
checkSafeDamp v = False

isSafe :: [Int] -> Bool
isSafe v = checkSafe v 0 4 || checkSafe v (-4) 0

checkSafe :: [Int] -> Int -> Int -> Bool
checkSafe [v1, v2] min max = v2 - v1 > min && v2 - v1 < max
checkSafe (v1:v2:vs) min max = checkSafe [v1, v2] min max && checkSafe (v2:vs) min max