import System.IO
import Text.Regex.Posix
import Data.List.Split
import qualified Data.Text as Text

main = do
    contents <- readFile "input.txt"
    let s = splitOn "don't()" contents
    let l = unwords (head s : map extractDo (drop 1 s) :: [String])
    print (sum . map (uncurry (*)) $ extractMulNums l)

extractDo :: String -> String
extractDo s = Text.unpack (snd ( Text.breakOn (Text.pack "do()") (Text.pack s)))

extractMulNums :: String -> [(Int, Int)]
extractMulNums s = map readInts (s =~ "mul\\(([[:digit:]]{1,3})\\,([[:digit:]]{1,3})\\)" :: [[String]])

readInts :: [String] -> (Int, Int)
readInts (_:v1:v2:_) = (read v1 :: Int, read v2 :: Int)