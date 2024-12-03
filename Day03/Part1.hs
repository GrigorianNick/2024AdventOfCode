import System.IO
import Text.Regex.Posix

main = do
    contents <- readFile "input.txt"
    print (sum . map (uncurry (*)) $ extractMulNums contents)

extractMulNums :: String -> [(Int, Int)]
extractMulNums s = map readInts (s =~ "mul\\(([[:digit:]]{1,3})\\,([[:digit:]]{1,3})\\)" :: [[String]])

readInts :: [String] -> (Int, Int)
readInts (_:v1:v2:_) = (read v1 :: Int, read v2 :: Int)