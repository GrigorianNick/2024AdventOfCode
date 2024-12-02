import System.IO
import Data.List

main = do
    contents <- readFile "inputOne.txt"
    let l1 = sort . fst . unzip . map readInts $ lines contents
    let l2 = sort . snd . unzip . map readInts $ lines contents
    print ( sum $ map diff $ zip l1 l2)
    
readInts :: String -> (Int, Int)
readInts s = (read (words s !! 0), read (words s !! 1))

diff :: (Int, Int) -> Int
diff (i1, i2) = abs $ i1 - i2