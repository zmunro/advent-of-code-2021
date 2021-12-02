main :: IO ()
main = do
  xs <- map (sndToInt . words) . lines <$> readFile "puzzle-input/day02.txt"
  let ans = foldl navigate (0, 0) xs
  print ans
  print (uncurry (*) ans)


sndToInt :: [String] -> (String, Int)
sndToInt [str, n] = (str, read n :: Int)
sndToInt _ = ("", 0)


navigate :: Num a => (a, a) -> ([Char], a) -> (a, a)
navigate (depth, lat) ("forward", distance) = (depth, lat + distance)
navigate (depth, lat) ("down", distance) = (depth + distance, lat)
navigate (depth, lat) ("up", distance) = (depth - distance, lat)
navigate (depth, lat) (_, distance) = (0, 0)
