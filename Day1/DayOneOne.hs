import System.IO
import Data.List

main = do
    contents <- readFile "input.txt"
    let (l1, l2) = unzip . map readInts $ lines contents
    print ( sum $ zipWith diff l1 l2)
    
readInts :: String -> (Int, Int)
readInts s = (read (head (words s)), read (words s !! 1))

diff :: Int -> Int -> Int
diff i1 i2 = abs $ i1 - i2