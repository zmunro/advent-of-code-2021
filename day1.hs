import System.IO
import Control.Monad

main = do
  xs <- map read . lines <$> readFile "puzzle-input/day01.txt"
  putStrLn $ "part 1: " <> show (part1 xs)
  putStrLn $ "part 2: " <> show (part2 xs)

sumList :: [Int] -> Int
sumList [] = 0
sumlist (x:xs) = x + sumList xs

part1 list = snd $ foldl (\(prev, inc) x -> (x, (inc+) . fromEnum $ x > prev)) ((head list), 0) list
part2 list = snd $ foldl (\(prevs, inc) x -> ((tail prevs) ++ [x], (inc+) . fromEnum $ ((sum(tail prevs) + x) > (sum prevs)))) (take 3 list, 0) (drop 3 list)
