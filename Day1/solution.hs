import Data.List.Split (splitOn)
import Data.List (sortBy)

input :: FilePath -> IO [[Int]]
input s = map (map read . lines) . splitOn "\n\n" <$> readFile s

partOne :: [[Int]] -> Int
partOne = foldl max 0 . map sum

partTwo :: [[Int]] -> Int
partTwo = sum . take 3 . sortBy (flip compare) . map sum

main :: IO ()
main = do
  f <- input "input.txt"
  putStrLn $ "Part one: " ++ show (partOne f)
  putStrLn $ "Part two: " ++ show (partTwo f)
