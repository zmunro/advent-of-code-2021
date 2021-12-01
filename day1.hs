import System.IO
import Control.Monad

main = do
        let list = []
        contents <- readFile "puzzle-input/day1-part1.txt"
        let singlewords = words contents
            list = f singlewords
            sum = foldl1 (\acc x -> acc + x)
            ans = snd $ foldl (\(prev, inc) x -> (x, (inc+) . fromEnum $ x > prev)) ((head list), 0) list
            ans2 = snd $ foldl (\(prevs, inc) x -> ((tail prevs) ++ [x], (inc+) . fromEnum $ ((sum(tail prevs) + x) > (sum prevs)))) (take 3 list, 0) (drop 3 list)
        print ans
        print ans2
f :: [String] -> [Int]
f = map read


