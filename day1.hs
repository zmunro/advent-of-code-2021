import System.IO
import Control.Monad

main = do
  xs <- map (read :: String -> Int) . lines <$> readFile "puzzle-input/day01.txt"
  putStrLn $ "part 1: " <> show (part1 xs)
  putStrLn $ "part 2: " <> show (part2 xs)

part1 :: Ord a => [a] -> Int
part1 xs = snd $ foldl (\(prev, inc) x -> (x, (inc+) . fromEnum $ x > prev)) (head xs, 0) xs
part2 :: (Ord a, Num a) => [a] -> Int
part2 xs = snd $ foldl (\(prevs, inc) x -> (tail prevs ++ [x], (inc+) . fromEnum $ ((sum(tail prevs) + x) > sum prevs))) (take 3 xs, 0) (drop 3 xs)


-- -- code golfed part 2
f xs = snd $ foldl (\(p, i) x -> (tail p ++ [x], (i+) . fromEnum $ ((sum(tail p) + x) > sum p))) (take 3 xs, 0) (drop 3 xs)