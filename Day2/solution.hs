import Data.List.Split (splitOn)
import Data.List (sortBy)
import Data.Char (ord)

input :: FilePath -> IO [[String]]
input s = map (splitOn " ") . filter (/= "") . splitOn "\n" <$> readFile s

roundScore :: [String] -> Int
roundScore [[o], [s]]
  | (ord s - ord o) `mod` 3 == 0 = 6
  | ord o == ord s - 23 = 3
  | otherwise = 0
roundScore _ = error "Invalid round"

scoreMap :: String -> Int
scoreMap s
  | s == "X" = 1
  | s == "Y" = 2
  | s == "Z" = 3
  | otherwise = error "Invalid input string"

partOne :: [[String]] -> Int
partOne xs = sum $ zipWith (+) scores rounds
  where
    rounds = map roundScore xs
    scores = map (scoreMap . (!! 1)) xs


main :: IO ()
main = do
  f <- input "input.txt"
  print $ partOne f
