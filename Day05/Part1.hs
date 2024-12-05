import System.IO
import Data.List
import Control.Applicative (Applicative(liftA2))

main = do
    content <- readFile "input.txt"
    let contents = lines content
    let rules = map extractRules $ takeWhile (/= "") contents
    let entries = map extractEntries $ drop 1 $ dropWhile (/= "") contents
    print (sum . map (\a -> a !! (length a `div` 2)) $ filter (validateRules rules []) entries)

extractRules :: String -> (Int, Int)
extractRules s = (read . takeWhile (/= '|') $ s, read . drop 1 . dropWhile (/= '|') $ s)

extractEntries :: String -> [Int]
extractEntries "" = []
extractEntries s = (read . takeWhile (/= ',') $ s):(extractEntries . drop 1 . dropWhile (/= ',') $ s)

validateRules :: [(Int, Int)] -> [Int] -> [Int] -> Bool
validateRules _ _ [] = True
validateRules rules bl (v:vs)
    | v `elem` bl = False
    | otherwise = do
        let p = map fst $ filter (\t -> snd t == v) rules
        validateRules rules (p ++ bl) vs