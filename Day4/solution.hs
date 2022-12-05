import Data.List.Split (splitOn)
import Data.List (intersect)

toRange :: String -> [Int]
toRange s = [x..y]
  where [x, y] = map read $ splitOn "-" s

input :: FilePath -> IO [[[Int]]]
input f = map (map toRange . splitOn ",") . filter (/= "") . splitOn "\n" <$> readFile f

partOne :: [[[Int]]] -> Int
partOne = length . filter (== True) . map overlaps
  where
    overlaps [x, y] = let i = intersect x y in i == x || i == y
    overlaps _ = error "Invalid input"

partTwo :: [[[Int]]] -> Int
partTwo = length . filter (== True) . map overlaps
  where
    overlaps [x, y] = not $ null (x `intersect` y)
    overlaps _ = error "Invalid input"

main :: IO ()
main = do
  d <- input "input.txt"
  putStrLn $ "Part one: " ++ show (partOne d)
  putStrLn $ "Part two: " ++ show (partTwo d)
