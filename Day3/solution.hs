import Data.List.Split (splitOn, chunksOf)
import Data.List (intersectBy, intersect)
import Data.Char (ord, isAsciiUpper, isAsciiLower)

input :: FilePath -> IO [String]
input f = filter (/= "") . splitOn "\n" <$> readFile f

priority :: Char -> Int
priority c
  | isAsciiUpper c = ord c - ord 'A' + 27
  | isAsciiLower c = ord c - ord 'a' + 1
  | otherwise = error "Invalid range"

partOne :: [String] -> Int
partOne = sum . map (priority . head . uncurry intersect . half)
  where
    half c = splitAt (length c `div` 2) c

partTwo :: [String] -> Int
partTwo = sum . map (priority . head . foldl1 intersect) . chunksOf 3

main :: IO ()
main = do
  items <- input "input.txt"
  putStrLn $ "Part one: " ++ show (partOne items)
  putStrLn $ "Part two: " ++ show (partTwo items)
