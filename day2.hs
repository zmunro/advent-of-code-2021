main :: IO ()
main = do
  xs <- map (sndToInt . words) . lines <$> readFile "puzzle-input/day02.txt"
  let ans = foldl part1 (0, 0) xs
  let ans2 = foldl part2 (0,0,0) xs
  print ans
  print (uncurry (*) ans)
  print ans2
  print ((\(a,b,_) -> a * b) ans2)


sndToInt :: [String] -> (String, Int)
sndToInt [str, n] = (str, read n :: Int)
sndToInt _ = ("", 0)


part1 :: Num a => (a, a) -> ([Char], a) -> (a, a)
part1 (depth, lat) ("forward", distance) = (depth, lat + distance)
part1 (depth, lat) ("down", distance) = (depth + distance, lat)
part1 (depth, lat) ("up", distance) = (depth - distance, lat)
part1 (depth, lat) (_, distance) = (0, 0)

part2 :: Num a => (a, a, a) -> ([Char], a) -> (a, a, a)
part2 (depth, lat, aim) ("forward", distance) = (depth + (aim * distance), lat + distance, aim)
part2 (depth, lat, aim) ("down", distance) = (depth, lat, aim + distance)
part2 (depth, lat, aim) ("up", distance) = (depth, lat, aim - distance)
part2 (depth, lat, aim) (_, distance) = (0, 0, 0)