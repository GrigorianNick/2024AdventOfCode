import System.IO
import Data.List

main = do
    contents <- readFile "input.txt"
    let field = lines contents
    let begins = filter (\a -> snd a > 0 && snd a < length field - 1) $ filter (\a -> fst a > 0 && fst a < (length . head) field - 1) (getSeeds field 'A' 0)
    print ( length .filter id  $ map (checkCrossMas field ) begins )

checkMasUnsafe :: [String] -> (Int, Int) -> (Int, Int) -> Bool
checkMasUnsafe field (x, y) (ox, oy) = (checkCharUnsafe field 'M' (x + ox, y + oy) && checkCharUnsafe field 'S' (x-ox, y-oy)) || (checkCharUnsafe field 'S' (x + ox, y + oy) && checkCharUnsafe field 'M' (x-ox, y-oy))

checkCrossMas :: [String] -> (Int, Int) -> Bool
checkCrossMas field pos = checkMasUnsafe field pos (-1, 1) && checkMasUnsafe field pos (1, 1)

checkCharUnsafe :: [String] -> Char -> (Int, Int) -> Bool
checkCharUnsafe field c (x, y) = field !! y !! x == c

getSeeds :: [String] -> Char -> Int -> [(Int, Int)]
getSeeds [] _ _ = []
getSeeds field c y = map (, y) (elemIndices c (head field)) ++ getSeeds (drop 1 field) c (y+1)