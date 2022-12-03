import Data.List.Split (splitOn)

data Weapon = Rock | Paper | Scissor
  deriving Eq


instance Enum Weapon where
  toEnum n = case n of
    1 -> Rock
    2 -> Paper
    3 -> Scissor
    _ -> error "Invalid weapon"

  fromEnum Rock = 1
  fromEnum Paper = 2
  fromEnum Scissor = 3

  succ w = toEnum . max 1 $ (fromEnum w + 1) `mod` 4
  pred w = let x = fromEnum w - 1 `mod` 4 in if x == 0 then Scissor else toEnum x


toWeapon :: String -> Weapon
toWeapon s
  | s == "A" || s == "X" = Rock
  | s == "B" || s == "Y" = Paper
  | s == "C" || s == "Z" = Scissor
  | otherwise            = error "Invalid enum string"


input :: String -> IO [[Weapon]]
input name = map (map toWeapon . splitOn " ") . filter (/= "") . splitOn "\n" <$> readFile name


partOne :: [[Weapon]] -> Int
partOne = sum . map points
  where
    points :: [Weapon] -> Int
    points [o, s]
      | succ s == o = fromEnum s -- loss
      | s == o      = fromEnum s + 3 -- draw
      | otherwise   = fromEnum s + 6 -- win
    points _ = error "Invalid duel"


partTwo :: [[Weapon]] -> Int
partTwo = sum . map points
  where
    points :: [Weapon] -> Int
    points [o, s]
      | s == Rock = fromEnum $ pred o
      | s == Paper = 3 + fromEnum o
      | otherwise = 6 + fromEnum (succ o)
    points _ = error "Invalid duel"


main :: IO ()
main = do
  f <- input "input.txt"
  putStrLn $ "Part one: " ++ show (partOne f)
  putStrLn $ "Part two: " ++ show (partTwo f)
