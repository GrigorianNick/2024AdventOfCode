import System.IO
import Data.List
import Control.Applicative (Applicative(liftA2))

main = do
    contents <- readFile "input.txt"
    let field = lines contents
    let dirs = liftA2 (,) ([-1, 0, 1] :: [Int]) ([-1, 0, 1] :: [Int])
    let starts = liftA2 (,) (getSeeds field 'X' 0) dirs
    print (length . filter id $ map (uncurry (searchXmas "XMAS" field)) starts)


searchXmas :: [Char] -> [String] -> (Int, Int) -> (Int, Int) -> Bool
searchXmas [] field (x, y) (dx, dy) = True
searchXmas word field (x, y) (dx, dy)
    | isInBounds field x y dx dy (length word - 1) = checkCharUnsafe field (head word) (x, y) && searchXmas (drop 1 word) field (x + dx, y + dy) (dx, dy)
    | otherwise = False

checkCharUnsafe :: [String] -> Char -> (Int, Int) -> Bool
checkCharUnsafe field c (x, y) = field !! y !! x == c

isInBounds :: [String] -> Int -> Int -> Int -> Int -> Int -> Bool
isInBounds field x y dx dy l = (x + (l * dx) >= 0) && (x + (l * dx) < length (head field))  && (y + (l * dy) >= 0) && (y + (l * dy) < length field)

getSeeds :: [String] -> Char -> Int -> [(Int, Int)]
getSeeds [] _ _ = []
getSeeds field c y = map (, y) (elemIndices c (head field)) ++ getSeeds (drop 1 field) c (y+1)