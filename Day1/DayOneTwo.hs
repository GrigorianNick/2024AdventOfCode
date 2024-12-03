import System.IO
import Data.List
import qualified Data.Map as M

main = do
    contents <- readFile "input.txt"
    let l = map readInts $ lines contents
    let m = M.fromListWith (+) $ map swivel l
    print (sum $ map (score m . fst) l)
    
readInts :: String -> (Int, Int)
readInts s = (read (head (words s)), read (words s !! 1))

swivel :: (Int, Int) -> (Int, Int)
swivel (_, y) = (y, 1 :: Int)

score ::  M.Map Int Int -> Int -> Int
score m n = n * M.findWithDefault 0 n m