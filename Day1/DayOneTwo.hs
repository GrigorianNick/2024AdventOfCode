import System.IO
import Data.List
import qualified Data.Map as M

main = do
    contents <- readFile "input.txt"
    let l = map readInts $ lines contents
    let m = M.fromListWith (+) $ map swivel l
    let x = map (score m) $ fst $ unzip l
    print $ sum x
    
readInts :: String -> (Int, Int)
readInts s = (read (words s !! 0), read (words s !! 1))

swivel :: (Int, Int) -> (Int, Int)
swivel (x, y) = (y, 1 :: Int)

score ::  M.Map Int Int -> Int -> Int
score m n = n * (M.findWithDefault 0 n m)